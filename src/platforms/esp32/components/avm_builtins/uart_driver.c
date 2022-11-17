/*
 * This file is part of AtomVM.
 *
 * Copyright 2020-2022 Davide Bettio <davide@uninstall.it>
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
#ifdef CONFIG_AVM_ENABLE_UART_PORT_DRIVER

#include <string.h>

#include <driver/uart.h>

#include <esp_log.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

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
#include "scheduler.h"
#include "term.h"
#include "utils.h"

#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

static Context *uart_driver_create_port(GlobalContext *global, term opts);

static const char *const ealready_atom = ATOM_STR("\x8", "ealready");
static const char *const no_proc_atom = ATOM_STR("\x7", "no_proc");
static void uart_driver_consume_mailbox(Context *ctx);

#define TAG "uart_driver"
#define UART_BUF_SIZE 256

struct UARTData
{
    xQueueHandle rxqueue;
    EventListener listener;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
    Context *ctx;
    uint8_t uart_num;
};

static const AtomStringIntPair parity_table[] = {
    { ATOM_STR("\x4", "none"), UART_PARITY_DISABLE },
    { ATOM_STR("\x4", "even"), UART_PARITY_EVEN },
    { ATOM_STR("\x3", "odd"), UART_PARITY_ODD },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair flow_control_table[] = {
    { ATOM_STR("\x4", "none"), UART_HW_FLOWCTRL_DISABLE },
    { ATOM_STR("\x4", "hardware"), UART_HW_FLOWCTRL_CTS_RTS },
    { ATOM_STR("\x3", "software"), -1 },
    SELECT_INT_DEFAULT(-1)
};

enum uart_cmd
{
    UARTInvalidCmd = 0,
    UARTReadCmd = 1,
    UARTWriteCmd = 2,
    UARTCloseCmd = 3
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "read"), UARTReadCmd },
    { ATOM_STR("\x5", "write"), UARTWriteCmd },
    { ATOM_STR("\x5", "close"), UARTCloseCmd },
    SELECT_INT_DEFAULT(UARTInvalidCmd)
};

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    if (LIKELY(target)) {
        mailbox_send(target, message);
    }
}

void uart_interrupt_callback(EventListener *listener)
{
    struct UARTData *uart_data = listener->data;

    uart_event_t event;
    if (xQueueReceive(uart_data->rxqueue, (void *) &event, (portTickType) portMAX_DELAY)) {
        switch (event.type) {
            case UART_DATA:
                if (uart_data->reader_process_pid != term_invalid_term()) {
                    int bin_size = term_binary_data_size_in_terms(event.size) + BINARY_HEADER_SIZE;
                    if (UNLIKELY(memory_ensure_free(uart_data->ctx, bin_size + REF_SIZE + TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
                        AVM_ABORT();
                    }

                    term bin = term_create_uninitialized_binary(event.size, uart_data->ctx);
                    uint8_t *bin_buf = (uint8_t *) term_binary_data(bin);
                    uart_read_bytes(uart_data->uart_num, bin_buf, event.size, portMAX_DELAY);

                    Context *ctx = uart_data->ctx;

                    term ok_tuple = term_alloc_tuple(2, ctx);
                    term_put_tuple_element(ok_tuple, 0, OK_ATOM);
                    term_put_tuple_element(ok_tuple, 1, bin);

                    term ref = term_from_ref_ticks(uart_data->reader_ref_ticks, ctx);

                    term result_tuple = term_alloc_tuple(2, ctx);
                    term_put_tuple_element(result_tuple, 0, ref);
                    term_put_tuple_element(result_tuple, 1, ok_tuple);

                    send_message(uart_data->reader_process_pid, result_tuple, ctx->global);

                    uart_data->reader_process_pid = term_invalid_term();
                    uart_data->reader_ref_ticks = 0;
                }
                break;
            case UART_FIFO_OVF:
                break;
            case UART_BUFFER_FULL:
                break;
            case UART_BREAK:
                break;
            case UART_PARITY_ERR:
                break;
            case UART_FRAME_ERR:
                break;
            case UART_PATTERN_DET:
                break;
            default:
                break;
        }
    }
}

static int get_uart_pin_opt(term opts, term pin_name)
{
    term value = interop_proplist_get_value_default(opts, pin_name, DEFAULT_ATOM);
    if (value == DEFAULT_ATOM) {
        return UART_PIN_NO_CHANGE;
    } else if (!term_is_integer(value)) {
        // TODO: let's return -2;
        fprintf(stderr, "abort() at %s:%i.\n", __FILE__, __LINE__);
        abort();
    } else {
        return term_to_int(value);
    }
}

Context *uart_driver_create_port(GlobalContext *global, term opts)
{
    Context *ctx = context_new(global);

    term uart_name_term = interop_proplist_get_value(opts, NAME_ATOM);
    term uart_speed_term = interop_proplist_get_value_default(opts, SPEED_ATOM, term_from_int(115200));

    term data_bits_term = interop_proplist_get_value_default(opts, DATA_BITS_ATOM, term_from_int(8));
    term stop_bits_term = interop_proplist_get_value_default(opts, STOP_BITS_ATOM, term_from_int(1));
    term flow_control_term = interop_proplist_get_value_default(opts, FLOW_CONTROL_ATOM, NONE_ATOM);
    term parity_term = interop_proplist_get_value_default(opts, PARITY_ATOM, NONE_ATOM);

    term tx_pin = get_uart_pin_opt(opts, TX_PIN_ATOM);
    term rx_pin = get_uart_pin_opt(opts, RX_PIN_ATOM);
    term rts_pin = get_uart_pin_opt(opts, RTS_PIN_ATOM);
    term cts_pin = get_uart_pin_opt(opts, CTS_PIN_ATOM);

    term event_queue_len_term = interop_proplist_get_value_default(opts, EVENT_QUEUE_LEN_ATOM, term_from_int(16));
    if (!term_is_integer(event_queue_len_term)) {
        fprintf(stderr, "abort() at %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    int event_queue_len = term_to_int(event_queue_len_term);

    int ok;
    char *uart_name = interop_term_to_string(uart_name_term, &ok);
    if (!uart_name || !ok) {
        AVM_ABORT();
    }

    uint8_t uart_num;
    if (!strcmp(uart_name, "UART0")) {
        uart_num = UART_NUM_0;
    } else if (!strcmp(uart_name, "UART1")) {
        uart_num = UART_NUM_1;
    }
    #if defined(CONFIG_IDF_TARGET_ESP32) || defined(CONFIG_IDF_TARGET_ESP32S3)
     else if (!strcmp(uart_name, "UART2")) {
        uart_num = UART_NUM_2;
    }
    #endif
    else {
        AVM_ABORT();
    }
    free(uart_name);

    avm_int_t uart_speed = term_to_int(uart_speed_term);

    int data_bits;
    switch (term_to_int(data_bits_term)) {
        case 8:
            data_bits = UART_DATA_8_BITS;
            break;
        case 7:
            data_bits = UART_DATA_7_BITS;
            break;
        case 6:
            data_bits = UART_DATA_6_BITS;
            break;
        case 5:
            data_bits = UART_DATA_5_BITS;
            break;
        default:
            AVM_ABORT();
    }

    int stop_bits;
    switch (term_to_int(stop_bits_term)) {
        case 1:
            stop_bits = UART_STOP_BITS_1;
            break;
        case 2:
            stop_bits = UART_STOP_BITS_2;
            break;
        default:
            AVM_ABORT();
    }

    uart_hw_flowcontrol_t flow_control = interop_atom_term_select_int(ctx->global, flow_control_table, flow_control_term);
    if (flow_control < 0) {
        AVM_ABORT();
    }

    uart_parity_t parity = interop_atom_term_select_int(ctx->global, parity_table, parity_term);
    if (parity < 0) {
        AVM_ABORT();
    }

    uart_config_t uart_config = {
        .baud_rate = uart_speed,
        .data_bits = data_bits,
        .parity = parity,
        .stop_bits = stop_bits,
        .flow_ctrl = flow_control
    };
    uart_param_config(uart_num, &uart_config);

    uart_set_pin(uart_num, tx_pin, rx_pin, rts_pin, cts_pin);

    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    struct UARTData *uart_data = malloc(sizeof(struct UARTData));
    if (IS_NULL_PTR(uart_data)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    uart_data->listener.data = uart_data;
    uart_data->listener.handler = uart_interrupt_callback;
    list_append(&platform->listeners, &uart_data->listener.listeners_list_head);
    uart_data->reader_process_pid = term_invalid_term();
    uart_data->reader_ref_ticks = 0;
    uart_data->ctx = ctx;
    uart_data->uart_num = uart_num;
    ctx->native_handler = uart_driver_consume_mailbox;
    ctx->platform_data = uart_data;

    if (uart_driver_install(uart_num, UART_BUF_SIZE, 0, event_queue_len, &uart_data->rxqueue, 0) != ESP_OK) {
        fprintf(stderr, "abort() at %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    uart_data->listener.sender = uart_data->rxqueue;
    if (xQueueAddToSet(uart_data->rxqueue, event_set) != pdPASS) {
        fprintf(stderr, "abort() at %s:%i.\n", __FILE__, __LINE__);
        abort();
    }

    return ctx;
}

static void uart_driver_do_read(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    if (uart_data->reader_process_pid != term_invalid_term()) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "[uart_driver_do_read] Failed to allocate space for error tuple");
            send_message(pid, MEMORY_ATOM, glb);
        }

        term ealready = context_make_atom(ctx, ealready_atom);

        term error_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, ealready);

        term result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
        term_put_tuple_element(result_tuple, 1, error_tuple);

        send_message(pid, result_tuple, glb);

        return;
    }

    size_t count;
    uart_get_buffered_data_len(uart_data->uart_num, &count);

    if (count > 0) {
        int bin_size = term_binary_data_size_in_terms(count) + BINARY_HEADER_SIZE;
        if (UNLIKELY(memory_ensure_free(uart_data->ctx, bin_size + TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "[uart_driver_do_read] Failed to allocate space for return value");
            send_message(pid, MEMORY_ATOM, glb);
        }

        term bin = term_create_uninitialized_binary(count, uart_data->ctx);
        uint8_t *bin_buf = (uint8_t *) term_binary_data(bin);
        uart_read_bytes(uart_data->uart_num, bin_buf, count, portMAX_DELAY);

        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, bin);

        term result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
        term_put_tuple_element(result_tuple, 1, ok_tuple);

        send_message(pid, result_tuple, uart_data->ctx->global);

    } else {
        uart_data->reader_process_pid = pid;
        uart_data->reader_ref_ticks = ref_ticks;
    }
}

static void uart_driver_do_write(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term cmd = term_get_tuple_element(msg, 2);

    term data = term_get_tuple_element(cmd, 1);

    int ok;
    int buffer_size = interop_iolist_size(data, &ok);
    void *buffer = malloc(buffer_size);
    interop_write_iolist(data, buffer);

    uart_write_bytes(uart_data->uart_num, buffer, buffer_size);

    free(buffer);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "[uart_driver_do_write] Failed to allocate space for return value");
        send_message(pid, MEMORY_ATOM, glb);
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
    term_put_tuple_element(result_tuple, 1, OK_ATOM);

    send_message(pid, result_tuple, glb);
}

static void uart_driver_do_close(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    list_remove(&uart_data->listener.listeners_list_head);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "[uart_driver_do_close] Failed to allocate space for return value");
        send_message(pid, MEMORY_ATOM, glb);
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
    term_put_tuple_element(result_tuple, 1, OK_ATOM);
    send_message(pid, result_tuple, glb);

    esp_err_t err = uart_driver_delete(uart_data->uart_num);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGW(TAG, "Failed to delete UART driver.  err=%i\n", err);
    }

    free(uart_data);
}

static void uart_driver_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    bool is_closed = false;
    while (!list_is_empty(&ctx->mailbox)) {
        Message *message = mailbox_dequeue(ctx);
        term msg = message->message;
        term pid = term_get_tuple_element(msg, 0);
        term ref = term_get_tuple_element(msg, 1);
        term req = term_get_tuple_element(msg, 2);
        uint64_t ref_ticks = term_to_ref_ticks(ref);

        if (is_closed) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
                ESP_LOGE(TAG, "[uart_driver_consume_mailbox] Failed to allocate space for error tuple");
                send_message(pid, MEMORY_ATOM, glb);
            }

            term no_proc = context_make_atom(ctx, no_proc_atom);
            term error_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, no_proc);

            term result_tuple = term_alloc_tuple(2, ctx);
            term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
            term_put_tuple_element(result_tuple, 1, error_tuple);

            send_message(pid, result_tuple, glb);

            mailbox_destroy_message(message);
            continue;
        }

        term cmd_term = term_is_atom(req) ? req : term_get_tuple_element(req, 0);

        enum uart_cmd cmd = interop_atom_term_select_int(ctx->global, cmd_table, cmd_term);
        switch (cmd) {
            case UARTReadCmd:
                TRACE("read\n");
                uart_driver_do_read(ctx, msg);
                break;

            case UARTWriteCmd:
                TRACE("write\n");
                uart_driver_do_write(ctx, msg);
                break;

            case UARTCloseCmd:
                TRACE("close\n");
                uart_driver_do_close(ctx, msg);
                is_closed = true;
                break;

            default:
                TRACE("uart: error: unrecognized command.\n");
        }

        mailbox_destroy_message(message);
    }
    if (is_closed) {
        scheduler_terminate(ctx);
    }
}

REGISTER_PORT_DRIVER(uart, NULL, uart_driver_create_port)

#endif
