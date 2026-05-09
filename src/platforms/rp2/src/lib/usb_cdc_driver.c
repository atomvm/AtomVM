/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#ifdef AVM_USB_CDC_PORT_DRIVER_ENABLED

#include <string.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <pico/time.h>
#include <pico/util/queue.h>
#include <tusb.h>
#pragma GCC diagnostic pop

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "port.h"
#include "scheduler.h"
#include "smp.h"
#include "term.h"
#include "utils.h"

#include "rp2_sys.h"

static NativeHandlerResult usb_cdc_driver_consume_mailbox(Context *ctx);

#define USB_CDC_BUF_SIZE 256
#define NO_REF 0
#define NO_READER term_invalid_term()

// Abort a write after this many ms of no progress (host stalled or unplugged).
#define USB_CDC_WRITE_TIMEOUT_MS 500

struct USBCDCData
{
    queue_t rxqueue;
    struct EventListener listener;
    GlobalContext *global;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
    uint8_t itf;
#ifndef AVM_NO_SMP
    Mutex *reader_lock;
#endif
};

enum usb_cdc_cmd
{
    USBCDCInvalidCmd = 0,
    USBCDCReadCmd = 1,
    USBCDCWriteCmd = 2,
    USBCDCCloseCmd = 3,
    USBCDCCancelCmd = 4
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "read"), USBCDCReadCmd },
    { ATOM_STR("\x5", "write"), USBCDCWriteCmd },
    { ATOM_STR("\x5", "close"), USBCDCCloseCmd },
    { ATOM_STR("\xB", "cancel_read"), USBCDCCancelCmd },
    SELECT_INT_DEFAULT(USBCDCInvalidCmd)
};

// Singleton driver data, protected by s_open_lock
static struct USBCDCData *s_cdc_data = NULL;

#ifndef AVM_NO_SMP
static Mutex *s_open_lock = NULL;
#endif

static void usb_cdc_driver_init(GlobalContext *global)
{
    UNUSED(global);
#ifndef AVM_NO_SMP
    s_open_lock = smp_mutex_create();
    if (IS_NULL_PTR(s_open_lock)) {
        fprintf(stderr, "usb_cdc: failed to create open lock\n");
        AVM_ABORT();
    }
#endif
}

// TinyUSB CDC receive callback -- invoked by tud_task() in USB core task context.
// The TinyUSB lock is already held by the caller of tud_task(); do not relock.
void tud_cdc_rx_cb(uint8_t itf)
{
    if (s_cdc_data == NULL || itf != s_cdc_data->itf) {
        return;
    }
    uint32_t available = tud_cdc_n_available(itf);
    if (available > 0) {
        sys_try_post_listener_event_from_isr(s_cdc_data->global, &s_cdc_data->rxqueue, &available);
    }
}

static EventListener *usb_cdc_listener_handler(GlobalContext *glb, EventListener *base_listener)
{
    struct USBCDCData *cdc_data = GET_LIST_ENTRY(base_listener, struct USBCDCData, listener);

    uint32_t event_val;
    if (!queue_try_remove(&cdc_data->rxqueue, &event_val)) {
        return base_listener;
    }

    SMP_MUTEX_LOCK(cdc_data->reader_lock);
    if (cdc_data->reader_process_pid == term_invalid_term()) {
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
        return base_listener;
    }

    uint8_t buf[USB_CDC_BUF_SIZE];
    sys_tinyusb_lock(glb);
    uint32_t rx_size = tud_cdc_n_read(cdc_data->itf, buf, sizeof(buf));
    sys_tinyusb_unlock(glb);
    if (rx_size == 0) {
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
        return base_listener;
    }

    term reader_pid = cdc_data->reader_process_pid;
    uint64_t reader_ref_ticks = cdc_data->reader_ref_ticks;
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
    SMP_MUTEX_UNLOCK(cdc_data->reader_lock);

    int bin_size = term_binary_heap_size(rx_size);

    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, bin_size + REF_SIZE + TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    term bin = term_create_uninitialized_binary(rx_size, &heap, glb);
    memcpy((void *) term_binary_data(bin), buf, rx_size);

    term ok_tuple = term_alloc_tuple(2, &heap);
    term_put_tuple_element(ok_tuple, 0, OK_ATOM);
    term_put_tuple_element(ok_tuple, 1, bin);

    term ref = term_from_ref_ticks(reader_ref_ticks, &heap);

    term result_tuple = term_alloc_tuple(2, &heap);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, ok_tuple);

    int local_pid = term_to_local_process_id(reader_pid);
    globalcontext_send_message(glb, local_pid, result_tuple);

    memory_destroy_heap(&heap, glb);

    return base_listener;
}

static Context *usb_cdc_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    SMP_MUTEX_LOCK(s_open_lock);
    if (s_cdc_data != NULL) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "usb_cdc: CDC interface already in use\n");
        return NULL;
    }

    Context *ctx = context_new(global);

    struct USBCDCData *cdc_data = malloc(sizeof(struct USBCDCData));
    if (IS_NULL_PTR(cdc_data)) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    queue_init(&cdc_data->rxqueue, sizeof(uint32_t), EVENT_QUEUE_LEN);
    cdc_data->listener.handler = usb_cdc_listener_handler;
    cdc_data->listener.queue = &cdc_data->rxqueue;
    cdc_data->global = global;
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
    cdc_data->itf = 0;

#ifndef AVM_NO_SMP
    cdc_data->reader_lock = smp_mutex_create();
#endif

    // TinyUSB should already be initialized by Pico SDK when
    // pico_enable_stdio_usb is set. For distribution use, stdio_usb
    // should be disabled and this driver takes over the CDC interface.
    // Ensure TinyUSB device stack is running
    sys_tinyusb_lock(global);
    if (!tud_inited()) {
        tusb_init();
    }
    sys_tinyusb_unlock(global);

    sys_register_listener(global, &cdc_data->listener);

    s_cdc_data = cdc_data;
    SMP_MUTEX_UNLOCK(s_open_lock);

    ctx->native_handler = usb_cdc_driver_consume_mailbox;
    ctx->platform_data = cdc_data;

    return ctx;
}

static void usb_cdc_driver_do_read(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct USBCDCData *cdc_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    int local_pid = term_to_local_process_id(pid);

    SMP_MUTEX_LOCK(cdc_data->reader_lock);
    if (cdc_data->reader_process_pid != term_invalid_term()) {
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            fprintf(stderr, "usb_cdc: Failed to allocate error tuple\n");
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        term ealready = globalcontext_make_atom(glb, ATOM_STR("\x8", "ealready"));
        term error_tuple = port_create_error_tuple(ctx, ealready);
        port_send_reply(ctx, pid, ref, error_tuple);
        return;
    }

    // Try immediate read
    uint8_t buf[USB_CDC_BUF_SIZE];
    sys_tinyusb_lock(glb);
    uint32_t rx_size = tud_cdc_n_read(cdc_data->itf, buf, sizeof(buf));
    sys_tinyusb_unlock(glb);
    if (rx_size > 0) {
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
        int bin_size = term_binary_heap_size(rx_size);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, bin_size + TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            fprintf(stderr, "usb_cdc: Failed to allocate return value\n");
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        term bin = term_create_uninitialized_binary(rx_size, &ctx->heap, glb);
        memcpy((void *) term_binary_data(bin), buf, rx_size);

        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, bin);

        port_send_reply(ctx, pid, ref, ok_tuple);
    } else {
        cdc_data->reader_process_pid = pid;
        cdc_data->reader_ref_ticks = ref_ticks;
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
    }
}

static void usb_cdc_driver_do_cancel_read(Context *ctx, GenMessage gen_message)
{
    struct USBCDCData *cdc_data = ctx->platform_data;
    term req = gen_message.req;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    // Two forms: bare `cancel_read` clears unconditionally (legacy), and
    // `{cancel_read, ReadRef}` only clears if ReadRef matches the stored
    // reader. The tuple form is what avoids the race where a cancel
    // wipes a freshly-installed reader belonging to a different request.
    bool clear_unconditionally = term_is_atom(req);
    uint64_t target_ref_ticks = 0;
    if (!clear_unconditionally) {
        target_ref_ticks = term_to_ref_ticks(term_get_tuple_element(req, 1));
    }

    SMP_MUTEX_LOCK(cdc_data->reader_lock);
    if (clear_unconditionally || cdc_data->reader_ref_ticks == target_ref_ticks) {
        cdc_data->reader_process_pid = term_invalid_term();
        cdc_data->reader_ref_ticks = 0;
    }
    SMP_MUTEX_UNLOCK(cdc_data->reader_lock);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        fprintf(stderr, "usb_cdc: Failed to allocate return value\n");
        globalcontext_send_message(ctx->global, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void usb_cdc_send_error_reply(Context *ctx, term pid, term ref, AtomString reason_atom_str)
{
    GlobalContext *glb = ctx->global;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    term reason = globalcontext_make_atom(glb, reason_atom_str);
    term error_tuple = port_create_error_tuple(ctx, reason);
    port_send_reply(ctx, pid, ref, error_tuple);
}

static void usb_cdc_driver_do_write(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct USBCDCData *cdc_data = ctx->platform_data;
    term msg = gen_message.req;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    term data = term_get_tuple_element(msg, 1);
    int local_pid = term_to_local_process_id(pid);

    size_t buffer_size;
    switch (interop_iolist_size(data, &buffer_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        case InteropBadArg:
            usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
            return;
    }
    void *buffer = malloc(buffer_size);
    if (IS_NULL_PTR(buffer)) {
        globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
        return;
    }
    switch (interop_write_iolist(data, buffer)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(buffer);
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        case InteropBadArg:
            free(buffer);
            usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
            return;
    }

    /*
     * If the host hasn't opened the CDC interface yet, silently drop
     * the data and report success. This matches typical CDC ACM
     * behavior on Linux/macOS (writes to a not-yet-opened tty are
     * absorbed) and is what serial_dist needs at boot: the link
     * manager blasts SYNC bytes hoping a peer is there, and a not-
     * yet-connected host should be a no-op rather than crash.
     */
    sys_tinyusb_lock(glb);
    bool initially_connected = tud_cdc_n_connected(cdc_data->itf);
    sys_tinyusb_unlock(glb);
    if (!initially_connected) {
        free(buffer);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        port_send_reply(ctx, pid, ref, OK_ATOM);
        return;
    }

    size_t written = 0;
    uint32_t deadline = to_ms_since_boot(get_absolute_time()) + USB_CDC_WRITE_TIMEOUT_MS;
    bool host_gone = false;
    bool timed_out = false;
    while (written < buffer_size) {
        sys_tinyusb_lock(glb);
        bool connected = tud_cdc_n_connected(cdc_data->itf);
        uint32_t chunk = 0;
        if (connected) {
            chunk = tud_cdc_n_write(cdc_data->itf,
                (const uint8_t *) buffer + written,
                buffer_size - written);
            tud_cdc_n_write_flush(cdc_data->itf);
            if (chunk == 0) {
                tud_task();
            }
        }
        sys_tinyusb_unlock(glb);

        if (!connected) {
            host_gone = true;
            break;
        }
        if (chunk > 0) {
            written += chunk;
            // Reset the deadline on any progress so a slow but moving
            // host can drain a buffer larger than the FIFO without tripping.
            deadline = to_ms_since_boot(get_absolute_time()) + USB_CDC_WRITE_TIMEOUT_MS;
        } else {
            if ((int32_t) (to_ms_since_boot(get_absolute_time()) - deadline) >= 0) {
                timed_out = true;
                break;
            }
            sleep_ms(1);
        }
    }

    free(buffer);

    if (host_gone) {
        usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "closed"));
        return;
    }
    if (timed_out) {
        usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x7", "timeout"));
        return;
    }

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void usb_cdc_driver_do_close(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct USBCDCData *cdc_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    SMP_MUTEX_LOCK(s_open_lock);
    sys_tinyusb_lock(glb);
    s_cdc_data = NULL;
    sys_tinyusb_unlock(glb);
    SMP_MUTEX_UNLOCK(s_open_lock);

    sys_unregister_listener(glb, &cdc_data->listener);
    queue_free(&cdc_data->rxqueue);

    SMP_MUTEX_LOCK(cdc_data->reader_lock);
    term pending_reader_pid = cdc_data->reader_process_pid;
    uint64_t pending_reader_ref_ticks = cdc_data->reader_ref_ticks;
    cdc_data->reader_process_pid = term_invalid_term();
    SMP_MUTEX_UNLOCK(cdc_data->reader_lock);

    if (pending_reader_pid != term_invalid_term()) {
        BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2) * 2, heap)
        term error_tuple = term_alloc_tuple(2, &heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(glb, ATOM_STR("\x6", "closed")));
        term reader_ref = term_from_ref_ticks(pending_reader_ref_ticks, &heap);
        term reply = term_alloc_tuple(2, &heap);
        term_put_tuple_element(reply, 0, reader_ref);
        term_put_tuple_element(reply, 1, error_tuple);
        globalcontext_send_message(glb, term_to_local_process_id(pending_reader_pid), reply);
        END_WITH_STACK_HEAP(heap, glb)
    }

#ifndef AVM_NO_SMP
    smp_mutex_destroy(cdc_data->reader_lock);
#endif

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
    } else {
        port_send_reply(ctx, pid, ref, OK_ATOM);
    }

    free(cdc_data);
    ctx->platform_data = NULL;
}

static NativeHandlerResult usb_cdc_driver_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    bool is_closed = false;
    while (mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        term msg = message->message;
        GenMessage gen_message;
        if (UNLIKELY(port_parse_gen_message(msg, &gen_message) != GenCallMessage)) {
            fprintf(stderr, "usb_cdc: Received invalid message.\n");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        uint64_t ref_ticks = term_to_ref_ticks(gen_message.ref);
        int local_pid = term_to_local_process_id(gen_message.pid);

        if (is_closed) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
                fprintf(stderr, "usb_cdc: Failed to allocate error tuple\n");
                globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            } else {
                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, NOPROC_ATOM);

                term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);

                term result_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(result_tuple, 0, ref);
                term_put_tuple_element(result_tuple, 1, error_tuple);

                globalcontext_send_message(glb, local_pid, result_tuple);
            }

            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        term req = gen_message.req;
        term cmd_term = term_is_atom(req) ? req : term_get_tuple_element(req, 0);

        enum usb_cdc_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {
            case USBCDCReadCmd:
                usb_cdc_driver_do_read(ctx, gen_message);
                break;
            case USBCDCWriteCmd:
                usb_cdc_driver_do_write(ctx, gen_message);
                break;
            case USBCDCCloseCmd:
                usb_cdc_driver_do_close(ctx, gen_message);
                is_closed = true;
                break;
            case USBCDCCancelCmd:
                usb_cdc_driver_do_cancel_read(ctx, gen_message);
                break;
            default:
                fprintf(stderr, "usb_cdc: unrecognized command\n");
                usb_cdc_send_error_reply(ctx, gen_message.pid, gen_message.ref, ATOM_STR("\xF", "unknown_command"));
                break;
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return is_closed ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(usb_cdc, usb_cdc_driver_init, NULL, usb_cdc_driver_create_port)

#endif
