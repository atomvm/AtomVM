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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_USB_CDC_PORT_DRIVER

#include <assert.h>
#include <stdatomic.h>
#include <string.h>

#include <esp_log.h>
#include <freertos/FreeRTOS.h>
#include <freertos/queue.h>

#include <tinyusb.h>
#include <tusb_cdc_acm.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_defaultatoms.h"
#include "port.h"
#include "scheduler.h"
#include "smp.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"
#include "sys.h"

static NativeHandlerResult usb_cdc_driver_consume_mailbox(Context *ctx);

#define TAG "usb_cdc_driver"
#define USB_CDC_BUF_SIZE 512
#define NO_REF 0
#define NO_READER term_invalid_term()

// Abort a write after this many ms of no progress (host stalled or unplugged).
#define USB_CDC_WRITE_TIMEOUT_MS 500

struct USBCDCData
{
    QueueHandle_t rxqueue;
    EventListener listener;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
    tinyusb_cdcacm_itf_t itf;
    bool owns_itf;
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

// Singleton data for each CDC interface. Read by RX callback and
// written by create_port and do_close with s_open_lock
static _Atomic(struct USBCDCData *) s_cdc_data[CONFIG_TINYUSB_CDC_COUNT];

#ifndef AVM_NO_SMP
static Mutex *s_open_lock = NULL;
#endif

static void usb_cdc_driver_init(GlobalContext *global)
{
    UNUSED(global);
#ifndef AVM_NO_SMP
    s_open_lock = smp_mutex_create();
    if (IS_NULL_PTR(s_open_lock)) {
        ESP_LOGE(TAG, "Failed to create open lock");
        abort();
    }
#endif
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

static void update_reader_data(struct USBCDCData *cdc_data, term pid, uint64_t ref_ticks)
{
    cdc_data->reader_process_pid = pid;
    cdc_data->reader_ref_ticks = ref_ticks;
}

static void usb_cdc_rx_callback(int itf, cdcacm_event_t *event)
{
    if (itf < 0 || itf >= CONFIG_TINYUSB_CDC_COUNT) {
        return;
    }
    struct USBCDCData *cdc_data = atomic_load_explicit(&s_cdc_data[itf], memory_order_acquire);
    if (cdc_data == NULL) {
        return;
    }
    size_t rx_size = 0;
    esp_err_t ret = tinyusb_cdcacm_read(itf, NULL, 0, &rx_size);
    if (ret != ESP_OK || rx_size == 0) {
        // Peek failed, just signal that data may be available
        rx_size = 1;
    }
    // Post the number of available bytes to the queue
    uint32_t event_val = (uint32_t) rx_size;
    xQueueSendFromISR(cdc_data->rxqueue, &event_val, NULL);
}

EventListener *usb_cdc_interrupt_callback(GlobalContext *glb, EventListener *listener)
{
    struct USBCDCData *cdc_data = GET_LIST_ENTRY(listener, struct USBCDCData, listener);

    uint32_t event_val;
    if (xQueueReceive(cdc_data->rxqueue, (void *) &event_val, (TickType_t) 0)) {
        SMP_MUTEX_LOCK(cdc_data->reader_lock);
        term reader_pid = cdc_data->reader_process_pid;
        uint64_t reader_ref_ticks = cdc_data->reader_ref_ticks;
        if (reader_pid != term_invalid_term()) {
            update_reader_data(cdc_data, NO_READER, NO_REF);
        }
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);

        if (reader_pid != term_invalid_term()) {
            // Read actual available data
            uint8_t buf[USB_CDC_BUF_SIZE];
            size_t rx_size = 0;
            esp_err_t ret = tinyusb_cdcacm_read(cdc_data->itf, buf, sizeof(buf), &rx_size);
            if (ret != ESP_OK || rx_size == 0) {
                BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2) * 2, err_heap)
                term ref = term_from_ref_ticks(reader_ref_ticks, &err_heap);
                term error_tuple = term_alloc_tuple(2, &err_heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(glb, ATOM_STR("\x6", "eagain")));
                term result_tuple = term_alloc_tuple(2, &err_heap);
                term_put_tuple_element(result_tuple, 0, ref);
                term_put_tuple_element(result_tuple, 1, error_tuple);
                globalcontext_send_message(glb, term_to_local_process_id(reader_pid), result_tuple);
                END_WITH_STACK_HEAP(err_heap, glb)
                return listener;
            }

            int bin_size = term_binary_heap_size(rx_size);

            int local_pid = term_to_local_process_id(reader_pid);
            Heap heap;
            if (UNLIKELY(memory_init_heap(&heap, bin_size + REF_SIZE + TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
                return listener;
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

            globalcontext_send_message(glb, local_pid, result_tuple);

            memory_destroy_heap(&heap, glb);
        }
    }

    return listener;
}

static tinyusb_cdcacm_itf_t parse_cdc_interface(term opts, GlobalContext *global)
{
    term peripheral = interop_kv_get_value_default(opts, ATOM_STR("\xA", "peripheral"),
        UNDEFINED_ATOM, global);
    if (peripheral == UNDEFINED_ATOM) {
        return TINYUSB_CDC_ACM_0;
    }
    int ok;
    char *name = interop_term_to_string(peripheral, &ok);
    if (!name || !ok) {
        return TINYUSB_CDC_ACM_0;
    }
    tinyusb_cdcacm_itf_t itf = TINYUSB_CDC_ACM_0;
    if (strcmp(name, "CDC0") == 0 || strcmp(name, "USB0") == 0) {
        itf = TINYUSB_CDC_ACM_0;
    }
#if CONFIG_TINYUSB_CDC_COUNT > 1
    else if (strcmp(name, "CDC1") == 0 || strcmp(name, "USB1") == 0) {
        itf = 1;
    }
#endif
    free(name);
    return itf;
}

static Context *usb_cdc_driver_create_port(GlobalContext *global, term opts)
{
    tinyusb_cdcacm_itf_t itf = parse_cdc_interface(opts, global);

    if (itf >= CONFIG_TINYUSB_CDC_COUNT) {
        ESP_LOGE(TAG, "Invalid CDC interface number: %d", itf);
        return NULL;
    }

    SMP_MUTEX_LOCK(s_open_lock);
    if (atomic_load_explicit(&s_cdc_data[itf], memory_order_acquire) != NULL) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        ESP_LOGE(TAG, "CDC interface %d already in use", itf);
        return NULL;
    }

    struct USBCDCData *cdc_data = malloc(sizeof(struct USBCDCData));
    if (IS_NULL_PTR(cdc_data)) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

#ifndef AVM_NO_SMP
    cdc_data->reader_lock = smp_mutex_create();
    if (IS_NULL_PTR(cdc_data->reader_lock)) {
        free(cdc_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
#endif

    cdc_data->rxqueue = xQueueCreate(16, sizeof(uint32_t));
    if (IS_NULL_PTR(cdc_data->rxqueue)) {
        ESP_LOGE(TAG, "Failed to create RX queue");
#ifndef AVM_NO_SMP
        smp_mutex_destroy(cdc_data->reader_lock);
#endif
        free(cdc_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        return NULL;
    }
    cdc_data->listener.handler = usb_cdc_interrupt_callback;
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
    cdc_data->itf = itf;
    cdc_data->owns_itf = false;

    // Initialize TinyUSB if not already done
    // Note: TinyUSB init may already be done in main.c for console;
    // we only initialize our specific CDC interface for data.
    tinyusb_config_cdcacm_t acm_cfg = {
        .cdc_port = itf,
        .callback_rx = &usb_cdc_rx_callback,
        .callback_rx_wanted_char = NULL,
        .callback_line_state_changed = NULL,
        .callback_line_coding_changed = NULL,
    };
    esp_err_t err = tusb_cdc_acm_init(&acm_cfg);
    if (err == ESP_OK) {
        cdc_data->owns_itf = true;
    } else if (err == ESP_ERR_INVALID_STATE) {
        // The interface was already initialized (e.g. by the console).
        // tusb_cdc_acm_init ignored our acm_cfg, so register our RX
        // callback explicitly — otherwise we'd never see incoming bytes.
        esp_err_t rerr = tinyusb_cdcacm_register_callback(itf, CDC_EVENT_RX, &usb_cdc_rx_callback);
        if (rerr != ESP_OK) {
            ESP_LOGE(TAG, "Failed to register CDC RX callback for interface %d: %s",
                itf, esp_err_to_name(rerr));
            vQueueDelete(cdc_data->rxqueue);
#ifndef AVM_NO_SMP
            smp_mutex_destroy(cdc_data->reader_lock);
#endif
            free(cdc_data);
            SMP_MUTEX_UNLOCK(s_open_lock);
            return NULL;
        }
    } else {
        ESP_LOGE(TAG, "Failed to init CDC ACM interface %d: %s", itf, esp_err_to_name(err));
        vQueueDelete(cdc_data->rxqueue);
#ifndef AVM_NO_SMP
        smp_mutex_destroy(cdc_data->reader_lock);
#endif
        free(cdc_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        return NULL;
    }

    cdc_data->listener.sender = cdc_data->rxqueue;
    if (xQueueAddToSet(cdc_data->rxqueue, event_set) != pdPASS) {
        ESP_LOGE(TAG, "Failed to add USB CDC queue to event set.");
        if (cdc_data->owns_itf) {
            tusb_cdc_acm_deinit(itf);
        } else {
            tinyusb_cdcacm_unregister_callback(itf, CDC_EVENT_RX);
        }
        vQueueDelete(cdc_data->rxqueue);
#ifndef AVM_NO_SMP
        smp_mutex_destroy(cdc_data->reader_lock);
#endif
        free(cdc_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        return NULL;
    }

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        ESP_LOGE(TAG, "Failed to create context");
        xQueueRemoveFromSet(cdc_data->rxqueue, event_set);
        if (cdc_data->owns_itf) {
            tusb_cdc_acm_deinit(itf);
        } else {
            tinyusb_cdcacm_unregister_callback(itf, CDC_EVENT_RX);
        }
        vQueueDelete(cdc_data->rxqueue);
#ifndef AVM_NO_SMP
        smp_mutex_destroy(cdc_data->reader_lock);
#endif
        free(cdc_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        return NULL;
    }

    sys_register_listener(global, &cdc_data->listener);

    atomic_store_explicit(&s_cdc_data[itf], cdc_data, memory_order_release);
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
            ESP_LOGE(TAG, "Failed to allocate space for error tuple");
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
    size_t rx_size = 0;
    esp_err_t ret = tinyusb_cdcacm_read(cdc_data->itf, buf, sizeof(buf), &rx_size);
    if (ret == ESP_OK && rx_size > 0) {
        SMP_MUTEX_UNLOCK(cdc_data->reader_lock);
        int bin_size = term_binary_heap_size(rx_size);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, bin_size + TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Failed to allocate space for return value");
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
        update_reader_data(cdc_data, pid, ref_ticks);
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
        update_reader_data(cdc_data, NO_READER, NO_REF);
    }
    SMP_MUTEX_UNLOCK(cdc_data->reader_lock);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Failed to allocate space for return value");
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

    // If the host hasn't opened the CDC interface yet, silently drop the
    // data and report success
    if (!tud_cdc_n_connected(cdc_data->itf)) {
        free(buffer);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        port_send_reply(ctx, pid, ref, OK_ATOM);
        return;
    }

    // Write in chunks if needed (CDC has limited TX buffer)
    size_t written = 0;
    TickType_t deadline = xTaskGetTickCount() + pdMS_TO_TICKS(USB_CDC_WRITE_TIMEOUT_MS);
    while (written < buffer_size) {
        if (!tud_cdc_n_connected(cdc_data->itf)) {
            free(buffer);
            usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "closed"));
            return;
        }
        size_t to_write = buffer_size - written;
        size_t chunk = tinyusb_cdcacm_write_queue(cdc_data->itf,
            (const uint8_t *) buffer + written, to_write);
        tinyusb_cdcacm_write_flush(cdc_data->itf, pdMS_TO_TICKS(100));
        if (chunk > 0) {
            written += chunk;
            // Reset the deadline on any progress so a slow but moving
            // host can drain a buffer larger than the FIFO without tripping.
            deadline = xTaskGetTickCount() + pdMS_TO_TICKS(USB_CDC_WRITE_TIMEOUT_MS);
        } else {
            if ((int32_t) (xTaskGetTickCount() - deadline) >= 0) {
                free(buffer);
                usb_cdc_send_error_reply(ctx, pid, ref, ATOM_STR("\x7", "timeout"));
                return;
            }
            vTaskDelay(pdMS_TO_TICKS(1));
        }
    }

    free(buffer);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Failed to allocate space for return value");
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

    // Stop the RX path BEFORE deleting the queue: NULL-publish so any
    // future callback short-circuits, then unregister our callback so
    // esp_tinyusb won't dispatch new ones at all, then unhook from the
    // event set so xQueueDelete sees no consumers.
    SMP_MUTEX_LOCK(s_open_lock);
    atomic_store_explicit(&s_cdc_data[cdc_data->itf], NULL, memory_order_release);
    SMP_MUTEX_UNLOCK(s_open_lock);

    if (cdc_data->owns_itf) {
        tusb_cdc_acm_deinit(cdc_data->itf);
    } else {
        tinyusb_cdcacm_unregister_callback(cdc_data->itf, CDC_EVENT_RX);
    }
    sys_unregister_listener(glb, &cdc_data->listener);
    xQueueRemoveFromSet(cdc_data->rxqueue, event_set);
    vQueueDelete(cdc_data->rxqueue);

    SMP_MUTEX_LOCK(cdc_data->reader_lock);
    term pending_reader_pid = cdc_data->reader_process_pid;
    uint64_t pending_reader_ref_ticks = cdc_data->reader_ref_ticks;
    cdc_data->reader_process_pid = term_invalid_term();
    cdc_data->reader_ref_ticks = 0;
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
        ESP_LOGE(TAG, "Failed to allocate space for return value");
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);

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
            ESP_LOGW(TAG, "Received invalid message.");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        uint64_t ref_ticks = term_to_ref_ticks(gen_message.ref);
        int local_pid = term_to_local_process_id(gen_message.pid);

        if (is_closed) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
                ESP_LOGE(TAG, "Failed to allocate space for error tuple");
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
                ESP_LOGW(TAG, "Unrecognized command.");
                usb_cdc_send_error_reply(ctx, gen_message.pid, gen_message.ref,
                    ATOM_STR("\xF", "unknown_command"));
                break;
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return is_closed ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(usb_cdc, usb_cdc_driver_init, NULL, usb_cdc_driver_create_port)

#endif
