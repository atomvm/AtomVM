/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/* Erlang interface compatible with the esp32-ble-vhci branch. Controller
 * callbacks only copy packets into a bounded queue; a native task performs
 * controller lifecycle operations and delivers messages to the VM. */

#include "rtems_bt_vhci.h"

#include <errno.h>
#include <rtems.h>
#include <rtems/bspIo.h>
#include <rtems/rtems/intr.h>
#include <stdlib.h>
#include <string.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <mailbox.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

#define HCI_QUEUE_SIZE 24
#define HCI_WAKE RTEMS_EVENT_0
#define INVALID_PID INVALID_PROCESS_ID

#define HANDLE_ATOMSTR ATOM_STR("\x9", "$ble_vhci")

typedef enum
{
    SessionIdle,
    SessionStarting,
    SessionReady,
    SessionClosing,
    SessionFailed
} session_state_t;

typedef struct
{
    ErlNifPid owner;
    ErlNifMonitor monitor;
    bool monitored;
} ble_resource_t;

typedef struct
{
    uint16_t len;
    uint8_t *data;
} hci_packet_t;

static struct
{
    GlobalContext *global;
    ErlNifResourceType *resource_type;
    term handle_atom;
    term event_atom;
    term vhci_atom;
    term ready_atom;
    term overflow_atom;
    term closed_atom;
    term init_failed_atom;
    term busy_atom;
    term close_failed_atom;
    rtems_id task;
    rtems_id done;
    session_state_t state;
    /* Only compared for identity. The task never dereferences the resource:
     * its destructor may request asynchronous shutdown and then free it. */
    ble_resource_t *resource;
    int32_t owner;
    int32_t closing_pid;
    Message *init_reply;
    bool quit;
    bool ready;
    bool overflow;
    unsigned head;
    unsigned count;
    hci_packet_t *packets;
} ble;

RTEMS_INTERRUPT_LOCK_DEFINE(static, ble_lock, "VHCI")

static term pair(Heap *heap, term a, term b)
{
    term tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(tuple, 0, a);
    term_put_tuple_element(tuple, 1, b);
    return tuple;
}

static term error(Context *ctx, AtomString reason)
{
    if (memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return pair(&ctx->heap, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

static void wake_task(void)
{
    if (ble.task != RTEMS_ID_NONE) {
        (void) rtems_event_send(ble.task, HCI_WAKE);
    }
}

static void request_close(ble_resource_t *resource)
{
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    if (ble.resource == resource) {
        ble.owner = INVALID_PID;
        ble.state = SessionClosing;
    }
    rtems_interrupt_lock_release(&ble_lock, &lock);
    wake_task();
}

static void resource_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    request_close(obj);
}

static void resource_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *monitor)
{
    UNUSED(env);
    UNUSED(pid);
    UNUSED(monitor);
    ble_resource_t *resource = obj;
    resource->monitored = false;
    request_close(resource);
}

static const ErlNifResourceTypeInit resource_init = {
    .members = 3,
    .dtor = resource_dtor,
    .down = resource_down,
};

static int receive_packet(uint8_t *data, uint16_t len)
{
    if (data == NULL || len == 0 || len > RTEMS_BT_VHCI_PACKET_SIZE) {
        return -1;
    }
    /* VHCI callbacks run in the controller task. Allocate before taking the
     * queue lock; reserve memory for actual packet lengths, not 24 maximum
     * ACL frames competing with Wi-Fi's startup stacks. */
    uint8_t *copy = malloc(len);
    if (copy != NULL) {
        memcpy(copy, data, len);
    }
    int rc = -1;
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    if (ble.state == SessionStarting || ble.state == SessionReady) {
        if (copy != NULL && ble.count < HCI_QUEUE_SIZE) {
            hci_packet_t *packet = &ble.packets[(ble.head + ble.count) % HCI_QUEUE_SIZE];
            packet->len = len;
            packet->data = copy;
            copy = NULL;
            ble.count++;
            rc = 0;
        } else {
            /* Continuing after losing an HCI event corrupts host flow control. */
            ble.overflow = true;
            ble.state = SessionClosing;
        }
    }
    rtems_interrupt_lock_release(&ble_lock, &lock);
    free(copy);
    wake_task();
    return rc;
}

static void send_available(void)
{
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    if (ble.state == SessionReady) {
        ble.ready = true;
    }
    rtems_interrupt_lock_release(&ble_lock, &lock);
    wake_task();
}

static const esp_vhci_host_callback_t callbacks = {
    .notify_host_send_available = send_available,
    .notify_host_recv = receive_packet,
};

static void send_pair(int32_t pid, enum MessageType type, term a, term b)
{
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap);
    term message = pair(&heap, a, b);
    globalcontext_send_message_from_task(ble.global, pid, type, message);
    END_WITH_STACK_HEAP(heap, ble.global);
}

static bool send_packet(int32_t pid, const hci_packet_t *packet)
{
    /* Inline binaries just below the refc cutoff can need more heap words
     * than a maximum-size refc binary. Reserve space for both representations. */
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(REFC_BINARY_MIN - 1)
            + TERM_BINARY_HEAP_SIZE(RTEMS_BT_VHCI_PACKET_SIZE),
        heap);
    term binary = term_create_uninitialized_binary(packet->len, &heap, ble.global);
    bool success = !term_is_invalid_term(binary);
    if (success) {
        memcpy((void *) term_binary_data(binary), packet->data, packet->len);
        globalcontext_send_message_from_task(ble.global, pid, NormalMessage, pair(&heap, ble.event_atom, binary));
    }
    END_WITH_STACK_HEAP(heap, ble.global);
    return success;
}

static void hci_task(rtems_task_argument arg)
{
    UNUSED(arg);
    bool controller_owned = false;
    rtems_interrupt_lock_context lock;
    for (;;) {
        rtems_interrupt_lock_acquire(&ble_lock, &lock);
        session_state_t state = ble.state;
        Message *reply = ble.init_reply;
        int32_t owner = ble.owner;
        bool quit = ble.quit;
        rtems_interrupt_lock_release(&ble_lock, &lock);

        if (reply != NULL) {
            int rc = state == SessionStarting ? rtems_bt_vhci_open() : -ECANCELED;
            controller_owned = rc == 0;
            if (controller_owned && esp_vhci_host_register_callback(&callbacks) != 0) {
                rc = -EIO;
            }
            rtems_interrupt_lock_acquire(&ble_lock, &lock);
            bool started = rc == 0 && ble.state == SessionStarting;
            ble.state = started ? SessionReady : SessionClosing;
            ble.init_reply = NULL;
            rtems_interrupt_lock_release(&ble_lock, &lock);
            if (started) {
                globalcontext_send_message_from_task(ble.global, owner, TrapAnswerSignal, reply->message);
                send_pair(owner, NormalMessage, ble.vhci_atom, ble.ready_atom);
            } else {
                term reason = rc == -EBUSY ? ble.busy_atom : rc == -ECANCELED ? ble.closed_atom
                                                                              : ble.init_failed_atom;
                send_pair(owner, TrapAnswerSignal, ERROR_ATOM, reason);
            }
            mailbox_message_dispose_unsent(reply, ble.global, true);
            continue;
        }

        if (state == SessionClosing || quit) {
            if (controller_owned) {
                (void) esp_vhci_host_register_callback(NULL);
            }
            int rc = controller_owned ? rtems_bt_vhci_close() : 0;
            if (rc == 0) {
                controller_owned = false;
            }
            rtems_interrupt_lock_acquire(&ble_lock, &lock);
            int32_t closing_pid = ble.closing_pid;
            bool orphaned = ble.owner == INVALID_PID;
            bool overflow = ble.overflow;
            ble.closing_pid = INVALID_PID;
            ble.overflow = false;
            ble.ready = false;
            unsigned head = ble.head;
            unsigned count = ble.count;
            ble.head = ble.count = 0;
            hci_packet_t *packets = ble.packets;
            ble.packets = NULL;
            ble.state = rc == 0 ? SessionIdle : SessionFailed;
            if (rc == 0) {
                ble.resource = NULL;
            }
            rtems_interrupt_lock_release(&ble_lock, &lock);
            for (unsigned i = 0; i < count; i++) {
                free(packets[(head + i) % HCI_QUEUE_SIZE].data);
            }
            free(packets);
            if (overflow) {
                BEGIN_WITH_STACK_HEAP(2 * TUPLE_SIZE(2), heap);
                term message = pair(&heap, ble.vhci_atom, pair(&heap, ERROR_ATOM, ble.overflow_atom));
                globalcontext_send_message_from_task(ble.global, owner, NormalMessage, message);
                END_WITH_STACK_HEAP(heap, ble.global);
            }
            if (closing_pid != INVALID_PID && !orphaned) {
                if (rc == 0) {
                    globalcontext_send_message_from_task(ble.global, closing_pid, TrapAnswerSignal, OK_ATOM);
                } else {
                    send_pair(closing_pid, TrapAnswerSignal, ERROR_ATOM, ble.close_failed_atom);
                }
            }
            if (quit) {
                if (rc != 0) {
                    printk("ble_vhci: retrying controller shutdown (%d)\n", rc);
                    rtems_task_wake_after(1);
                    continue;
                }
                break;
            }
            if (rc != 0 && (orphaned || closing_pid == INVALID_PID)) {
                /* GC, owner exit and overflow have no caller left to retry.
                 * Keep ownership until the controller can actually stop. */
                rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(20));
                rtems_interrupt_lock_acquire(&ble_lock, &lock);
                if (ble.state == SessionFailed) {
                    ble.state = SessionClosing;
                }
                rtems_interrupt_lock_release(&ble_lock, &lock);
                continue;
            }
        } else if (state == SessionReady) {
            hci_packet_t packet;
            bool have_packet = false;
            rtems_interrupt_lock_acquire(&ble_lock, &lock);
            bool ready = ble.ready;
            ble.ready = false;
            if (ble.state == SessionReady && ble.count != 0) {
                packet = ble.packets[ble.head];
                ble.head = (ble.head + 1) % HCI_QUEUE_SIZE;
                ble.count--;
                have_packet = true;
            }
            rtems_interrupt_lock_release(&ble_lock, &lock);
            if (ready) {
                send_pair(owner, NormalMessage, ble.vhci_atom, ble.ready_atom);
            }
            if (have_packet) {
                if (!send_packet(owner, &packet)) {
                    rtems_interrupt_lock_acquire(&ble_lock, &lock);
                    ble.overflow = true;
                    ble.state = SessionClosing;
                    rtems_interrupt_lock_release(&ble_lock, &lock);
                }
                free(packet.data);
                continue;
            }
        }
        rtems_event_set events;
        (void) rtems_event_receive(HCI_WAKE, RTEMS_EVENT_ANY | RTEMS_WAIT, RTEMS_NO_TIMEOUT, &events);
    }
    (void) rtems_semaphore_release(ble.done);
    rtems_task_exit();
}

/* Created on first use and joined at VM shutdown. Keeping the worker alive
 * avoids deleting a task that may still be allocating a mailbox message. */
static bool ensure_task(void)
{
    if (ble.task != RTEMS_ID_NONE) {
        return true;
    }
    if (rtems_semaphore_create(rtems_build_name('V', 'H', 'D', 'N'), 0,
            RTEMS_SIMPLE_BINARY_SEMAPHORE | RTEMS_FIFO, 0, &ble.done)
        != RTEMS_SUCCESSFUL) {
        return false;
    }
    rtems_status_code sc = rtems_task_create(rtems_build_name('V', 'H', 'C', 'I'),
        10, 4096, RTEMS_DEFAULT_MODES, RTEMS_FLOATING_POINT | RTEMS_LOCAL, &ble.task);
    if (sc == RTEMS_SUCCESSFUL) {
        sc = rtems_task_start(ble.task, hci_task, 0);
        if (sc != RTEMS_SUCCESSFUL) {
            rtems_task_delete(ble.task);
        }
    }
    if (sc != RTEMS_SUCCESSFUL) {
        ble.task = RTEMS_ID_NONE;
        rtems_semaphore_delete(ble.done);
        ble.done = RTEMS_ID_NONE;
        return false;
    }
    return true;
}

static bool get_resource(Context *ctx, term handle, ble_resource_t **resource)
{
    if (!term_is_tuple(handle) || term_get_tuple_arity(handle) != 2
        || term_get_tuple_element(handle, 0) != ble.handle_atom) {
        return false;
    }
    void *obj;
    if (!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(handle, 1), ble.resource_type, &obj)) {
        return false;
    }
    *resource = obj;
    return (*resource)->owner == ctx->process_id;
}

static void cancel_init(void)
{
    free(ble.packets);
    ble.packets = NULL;
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    ble.state = SessionIdle;
    rtems_interrupt_lock_release(&ble_lock, &lock);
}

static term nif_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    if (ble.resource_type == NULL) {
        return error(ctx, ATOM_STR("\x6", "enomem"));
    }
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    bool busy = ble.state != SessionIdle;
    if (!busy) {
        ble.state = SessionStarting;
    }
    rtems_interrupt_lock_release(&ble_lock, &lock);
    if (busy) {
        return error(ctx, ATOM_STR("\xF", "already_started"));
    }
    if (!ensure_task()) {
        cancel_init();
        return error(ctx, ATOM_STR("\xB", "task_failed"));
    }
    if (memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE + 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK) {
        cancel_init();
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    ble.packets = malloc(HCI_QUEUE_SIZE * sizeof(hci_packet_t));
    if (ble.packets == NULL) {
        cancel_init();
        return error(ctx, ATOM_STR("\x6", "enomem"));
    }
    ble_resource_t *resource = enif_alloc_resource(ble.resource_type, sizeof(ble_resource_t));
    if (resource == NULL) {
        cancel_init();
        return error(ctx, ATOM_STR("\x6", "enomem"));
    }
    resource->owner = ctx->process_id;
    resource->monitored = false;
    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (enif_monitor_process(env, resource, &resource->owner, &resource->monitor) != 0) {
        enif_release_resource(resource);
        cancel_init();
        return error(ctx, ATOM_STR("\xE", "monitor_failed"));
    }
    resource->monitored = true;
    term handle = pair(&ctx->heap, ble.handle_atom, term_from_resource(resource, &ctx->heap));
    Message *reply = mailbox_message_create_normal_message_from_term(pair(&ctx->heap, OK_ATOM, handle));
    enif_release_resource(resource);
    if (reply == NULL) {
        enif_demonitor_process(env, resource, &resource->monitor);
        resource->monitored = false;
        cancel_init();
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    context_update_flags(ctx, ~NoFlags, Trap);
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    ble.resource = resource;
    ble.owner = ctx->process_id;
    ble.init_reply = reply;
    rtems_interrupt_lock_release(&ble_lock, &lock);
    wake_task();
    return term_invalid_term();
}

static term nif_send(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    ble_resource_t *resource;
    if (!get_resource(ctx, argv[0], &resource) || !term_is_local_pid(argv[1])
        || term_to_local_process_id(argv[1]) != resource->owner || !term_is_binary(argv[2])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t len = term_binary_size(argv[2]);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    if (!rtems_bt_vhci_valid_packet(data, len)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    bool active = ble.resource == resource && ble.state == SessionReady;
    rtems_interrupt_lock_release(&ble_lock, &lock);
    if (!active) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (!esp_vhci_host_check_send_available()) {
        return error(ctx, ATOM_STR("\x9", "not_ready"));
    }
    /* No lock across vendor calls: they can synchronously call back. */
    if (rtems_bt_vhci_send_packet(data, (uint16_t) len) != 0) {
        return error(ctx, ATOM_STR("\xB", "send_failed"));
    }
    return OK_ATOM;
}

static term nif_close(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    ble_resource_t *resource;
    if (!get_resource(ctx, argv[0], &resource)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    rtems_interrupt_lock_context lock;
    rtems_interrupt_lock_acquire(&ble_lock, &lock);
    bool active = ble.resource == resource;
    if (active) {
        context_update_flags(ctx, ~NoFlags, Trap);
        ble.closing_pid = ctx->process_id;
        ble.state = SessionClosing;
    }
    rtems_interrupt_lock_release(&ble_lock, &lock);
    if (!active) {
        if (resource->monitored) {
            resource->monitored = false;
            (void) enif_demonitor_process(erl_nif_env_from_context(ctx), resource, &resource->monitor);
        }
        return OK_ATOM;
    }
    /* Keep the monitor through asynchronous shutdown and failure. Otherwise
     * owner exit can strand a failed controller while a foreign process still
     * holds the handle. Closed resources drop the monitor on GC or owner exit. */
    wake_task();
    return term_invalid_term();
}

static const struct Nif init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_init };
static const struct Nif send_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_send };
static const struct Nif close_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_close };

static const struct Nif *get_nif(const char *name)
{
    if (strcmp(name, "ble_vhci:init/0") == 0 || strcmp(name, "Elixir.BleVhci:init/0") == 0) {
        return &init_nif;
    }
    if (strcmp(name, "ble_vhci:send/3") == 0 || strcmp(name, "Elixir.BleVhci:send/3") == 0) {
        return &send_nif;
    }
    if (strcmp(name, "ble_vhci:close/1") == 0 || strcmp(name, "Elixir.BleVhci:close/1") == 0) {
        return &close_nif;
    }
    return NULL;
}

static void collection_init(GlobalContext *global)
{
    memset(&ble, 0, sizeof(ble));
    ble.global = global;
    ble.owner = ble.closing_pid = INVALID_PID;
    ble.handle_atom = globalcontext_make_atom(global, HANDLE_ATOMSTR);
    ble.event_atom = globalcontext_make_atom(global, ATOM_STR("\x9", "hci_event"));
    ble.vhci_atom = globalcontext_make_atom(global, ATOM_STR("\x8", "ble_vhci"));
    ble.ready_atom = globalcontext_make_atom(global, ATOM_STR("\xD", "ready_to_send"));
    ble.overflow_atom = globalcontext_make_atom(global, ATOM_STR("\x8", "overflow"));
    ble.closed_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "closed"));
    ble.init_failed_atom = globalcontext_make_atom(global, ATOM_STR("\xB", "init_failed"));
    ble.busy_atom = globalcontext_make_atom(global, ATOM_STR("\xF", "controller_busy"));
    ble.close_failed_atom = globalcontext_make_atom(global, ATOM_STR("\xC", "close_failed"));
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    ble.resource_type = enif_init_resource_type(&env, "ble_vhci_resource", &resource_init, ERL_NIF_RT_CREATE, NULL);
}

static void collection_destroy(GlobalContext *global)
{
    if (ble.task != RTEMS_ID_NONE) {
        rtems_interrupt_lock_context lock;
        rtems_interrupt_lock_acquire(&ble_lock, &lock);
        ble.quit = true;
        rtems_interrupt_lock_release(&ble_lock, &lock);
        wake_task();
        (void) rtems_semaphore_obtain(ble.done, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
        ble.task = RTEMS_ID_NONE;
        rtems_semaphore_delete(ble.done);
        ble.done = RTEMS_ID_NONE;
    }
    globalcontext_process_task_driver_queues(global);
}

REGISTER_NIF_COLLECTION(ble_vhci, collection_init, collection_destroy, get_nif)
