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

#include <tusb.h>

#include "stm32_hal_platform.h"

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
#include "term.h"
#include "utils.h"

#include "stm_sys.h"
#include "usb_hw_init.h"

static NativeHandlerResult usb_cdc_driver_consume_mailbox(Context *ctx);

#define USB_CDC_BUF_SIZE 256

// Abort a write after this many ms of no progress (host stalled or unplugged).
#define USB_CDC_WRITE_TIMEOUT_MS 500

struct USBCDCData
{
    GlobalContext *global;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
    volatile bool rx_pending;
    uint8_t itf;
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

// Singleton driver data. Written by the main thread.
static struct USBCDCData *s_cdc_data = NULL;

void tud_cdc_rx_cb(uint8_t itf)
{
    if (s_cdc_data != NULL && itf == s_cdc_data->itf) {
        s_cdc_data->rx_pending = true;
    }
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

static void usb_cdc_check_rx(struct USBCDCData *cdc_data)
{
    if (!cdc_data->rx_pending) {
        return;
    }
    cdc_data->rx_pending = false;

    if (cdc_data->reader_process_pid == term_invalid_term()) {
        return;
    }

    uint8_t buf[USB_CDC_BUF_SIZE];
    uint32_t rx_size = tud_cdc_n_read(cdc_data->itf, buf, sizeof(buf));
    if (rx_size == 0) {
        return;
    }

    GlobalContext *glb = cdc_data->global;
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

    term ref = term_from_ref_ticks(cdc_data->reader_ref_ticks, &heap);

    term result_tuple = term_alloc_tuple(2, &heap);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, ok_tuple);

    int local_pid = term_to_local_process_id(cdc_data->reader_process_pid);
    globalcontext_send_message(glb, local_pid, result_tuple);

    memory_destroy_heap(&heap, glb);
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
}

static Context *usb_cdc_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    if (s_cdc_data != NULL) {
        fprintf(stderr, "usb_cdc: CDC interface already in use\n");
        return NULL;
    }

    // Initialize TinyUSB (clocks, GPIO, NVIC, then the device stack)
    // Done before allocating any AtomVM resources so a HW failure has no cleanup.
    if (!tud_inited()) {
        if (!usb_cdc_hw_init()) {
            fprintf(stderr, "usb_cdc: USB hardware init failed\n");
            return NULL;
        }
        tusb_init();
    }

    struct USBCDCData *cdc_data = malloc(sizeof(struct USBCDCData));
    if (IS_NULL_PTR(cdc_data)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        free(cdc_data);
        return NULL;
    }

    cdc_data->global = global;
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
    cdc_data->rx_pending = false;
    cdc_data->itf = 0;

    s_cdc_data = cdc_data;

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

    if (cdc_data->reader_process_pid != term_invalid_term()) {
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

    // Process USB tasks and try immediate read
    tud_task();
    uint8_t buf[USB_CDC_BUF_SIZE];
    uint32_t rx_size = tud_cdc_n_read(cdc_data->itf, buf, sizeof(buf));
    if (rx_size > 0) {
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

    if (clear_unconditionally || cdc_data->reader_ref_ticks == target_ref_ticks) {
        cdc_data->reader_process_pid = term_invalid_term();
        cdc_data->reader_ref_ticks = 0;
    }

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        fprintf(stderr, "usb_cdc: Failed to allocate return value\n");
        globalcontext_send_message(ctx->global, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
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

    // If the host hasn't opened the CDC interface yet, silently drop data
    if (!tud_cdc_n_connected(cdc_data->itf)) {
        free(buffer);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        port_send_reply(ctx, pid, ref, OK_ATOM);
        return;
    }

    size_t written = 0;
    uint32_t deadline = HAL_GetTick() + USB_CDC_WRITE_TIMEOUT_MS;
    while (written < buffer_size) {
        if (!tud_cdc_n_connected(cdc_data->itf)) {
            free(buffer);
            usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "closed"));
            return;
        }
        uint32_t chunk = tud_cdc_n_write(cdc_data->itf,
            (const uint8_t *) buffer + written,
            buffer_size - written);
        tud_cdc_n_write_flush(cdc_data->itf);
        if (chunk > 0) {
            written += chunk;
            // Reset the deadline on any progress so a slow but moving
            // host can drain a buffer larger than the FIFO without tripping.
            deadline = HAL_GetTick() + USB_CDC_WRITE_TIMEOUT_MS;
        } else {
            if ((int32_t) (HAL_GetTick() - deadline) >= 0) {
                free(buffer);
                usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x7", "timeout"));
                return;
            }
            tud_task();
        }
    }

    free(buffer);

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

    term pending_reader_pid = cdc_data->reader_process_pid;
    uint64_t pending_reader_ref_ticks = cdc_data->reader_ref_ticks;
    cdc_data->reader_process_pid = term_invalid_term();

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

    s_cdc_data = NULL;

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        free(cdc_data);
        ctx->platform_data = NULL;
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);

    free(cdc_data);
    ctx->platform_data = NULL;
}

/*
 * Public hook called from sys_poll_events: drain any RX delivered by
 * the OTG IRQ since the last poll. Without this, once the link manager
 * parks waiting on UartMod:read/2 the port stops receiving mailbox
 * messages, and even though tud_task processes the IRQ events, no one
 * delivers the resulting bytes to the parked reader.
 */
void usb_cdc_driver_poll(void)
{
    if (s_cdc_data == NULL) {
        return;
    }
    tud_task();
    usb_cdc_check_rx(s_cdc_data);
}

static NativeHandlerResult usb_cdc_driver_consume_mailbox(Context *ctx)
{
    struct USBCDCData *cdc_data = ctx->platform_data;
    bool is_closed = false;

    // Drain any pending USB RX before processing the mailbox so a fresh
    // {read, ...} request can return immediately if data is already here.
    if (cdc_data != NULL) {
        tud_task();
        usb_cdc_check_rx(cdc_data);
    }

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
            GlobalContext *glb = ctx->global;
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

REGISTER_PORT_DRIVER(usb_cdc, NULL, NULL, usb_cdc_driver_create_port)

#endif
