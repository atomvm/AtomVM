/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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
#ifdef CONFIG_AVM_ENABLE_I2C_PORT_DRIVER

#include <stdlib.h>
#include <string.h>

#include <driver/i2c_master.h>
#include <driver/i2c_types.h>
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
#include "port.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"

//#define ENABLE_TRACE
#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

#include "include/i2c_bus_manager.h"
#include "include/i2c_driver.h"

#define TAG "i2c_driver"

// Matches the previous legacy-driver behavior, which always waited
// indefinitely for a bus transaction to complete.
#define I2C_PORT_DRIVER_TIMEOUT_MS I2C_BUS_MANAGER_TIMEOUT_INFINITE

static void i2c_driver_init(GlobalContext *global);
static Context *i2c_driver_create_port(GlobalContext *global, term opts);

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req);
static term i2cdriver_end_transmission(Context *ctx, term pid);
static term i2cdriver_write_byte(Context *ctx, term pid, term req);
static NativeHandlerResult i2cdriver_consume_mailbox(Context *ctx);

static const char *const einprogress_atom = ATOM_STR("\xB", "einprogress");
static const char *const i2c_driver_atom = ATOM_STR("\xA", "i2c_driver");
static term i2c_driver;

enum i2c_cmd
{
    I2CInvalidCmd = 0,
    I2CBeginTransmissionCmd,
    I2CEndTransmissionCmd,
    I2CWriteByteCmd,
    I2CReadBytesCmd,
    I2CWriteBytesCmd,
    I2CCloseCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x12", "begin_transmission"), I2CBeginTransmissionCmd },
    { ATOM_STR("\x10", "end_transmission"), I2CEndTransmissionCmd },
    { ATOM_STR("\xA", "write_byte"), I2CWriteByteCmd },
    { ATOM_STR("\xA", "read_bytes"), I2CReadBytesCmd },
    { ATOM_STR("\xB", "write_bytes"), I2CWriteBytesCmd },
    { ATOM_STR("\x5", "close"), I2CCloseCmd },
    SELECT_INT_DEFAULT(I2CInvalidCmd)
};

struct I2CData
{
    i2c_master_bus_handle_t bus_handle;
    struct I2CTxBuffer tx_buf;
    term transmitting_pid;
    int i2c_port;
    uint32_t clock_speed_hz;
    uint8_t tx_address;

    // no need to make it atomic, we use it only when the process table is locked
    int ref_count;
};

#define I2C_VALIDATE_NOT_INVALID(moniker)              \
    if (term_is_invalid_term(moniker##_term)) {        \
        ESP_LOGE(TAG, "Missing parameter: " #moniker); \
        goto free_and_exit;                            \
    }

void i2c_driver_init(GlobalContext *global)
{
    i2c_driver = globalcontext_make_atom(global, i2c_driver_atom);
}

Context *i2c_driver_create_port(GlobalContext *global, term opts)
{
    struct I2CData *i2c_data = calloc(1, sizeof(struct I2CData));
    i2c_data->ref_count = 1;
    i2c_data->transmitting_pid = term_invalid_term();
    i2c_tx_buffer_init(&i2c_data->tx_buf);

    term scl_io_num_term = interop_kv_get_value(opts, ATOM_STR("\x3", "scl"), global);
    I2C_VALIDATE_NOT_INVALID(scl_io_num);

    term sda_io_num_term = interop_kv_get_value(opts, ATOM_STR("\x3", "sda"), global);
    I2C_VALIDATE_NOT_INVALID(sda_io_num);

    term clock_hz_term = interop_kv_get_value(opts, ATOM_STR("\xE", "clock_speed_hz"), global);
    I2C_VALIDATE_NOT_INVALID(clock_hz);

    i2c_data->i2c_port = I2C_NUM_0;
    term i2c_num_term = interop_kv_get_value(opts, ATOM_STR("\xA", "peripheral"), global);
    if (!term_is_invalid_term(i2c_num_term)) {
        if (!term_is_integer(i2c_num_term)) {
            ESP_LOGE(TAG, "Invalid parameter: peripheral is not an integer");
            goto free_and_exit;
        }
        i2c_data->i2c_port = term_to_int32(i2c_num_term);
        if (i2c_data->i2c_port < 0 || i2c_data->i2c_port > I2C_NUM_MAX - 1) {
            ESP_LOGE(TAG, "Invalid parameter: i2c_num out of range");
            goto free_and_exit;
        }
    }

    i2c_data->clock_speed_hz = term_to_int32(clock_hz_term);

    esp_err_t err = i2c_bus_manager_open(i2c_data->i2c_port, term_to_int32(scl_io_num_term),
        term_to_int32(sda_io_num_term), &i2c_data->bus_handle);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to create I2C master bus.  err=%s", esp_err_to_name(err));
        goto free_and_exit;
    }

    ESP_LOGI(TAG, "I2C driver installed using I2C port %i", i2c_data->i2c_port);

    Context *ctx = context_new(global);
    ctx->native_handler = i2cdriver_consume_mailbox;
    ctx->platform_data = i2c_data;

    return ctx;

free_and_exit:
    free(i2c_data);
    return NULL;
}

static NativeHandlerResult i2c_driver_unref(Context *ctx)
{
    struct I2CData *i2c_data = ctx->platform_data;
    if (--i2c_data->ref_count != 0) {
        return NativeContinue;
    }

    ctx->platform_data = NULL;

    i2c_tx_buffer_reset(&i2c_data->tx_buf);

    esp_err_t err = i2c_bus_manager_close(i2c_data->bus_handle);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGW(TAG, "Failed to delete I2C master bus.  err=%s", esp_err_to_name(err));
    }

    free(i2c_data);

    return NativeTerminate;
}

static term make_einprogress_error(Context *ctx, term owner_pid)
{
    if (UNLIKELY(memory_ensure_free(ctx, 2 * TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return OUT_OF_MEMORY_ATOM;
    }

    term reason_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(reason_tuple, 0, globalcontext_make_atom(ctx->global, einprogress_atom));
    term_put_tuple_element(reason_tuple, 1, owner_pid);

    return port_create_error_tuple(ctx, reason_tuple);
}

static term make_esp_err_error(Context *ctx, esp_err_t err)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return OUT_OF_MEMORY_ATOM;
    }
    return port_create_error_tuple(ctx, esp_err_to_term(ctx->global, err));
}

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        // another process is already transmitting
        ESP_LOGE(TAG, "i2cdriver_begin_transmission: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    term address_term = term_get_tuple_element(req, 1);

    i2c_tx_buffer_reset(&i2c_data->tx_buf);
    i2c_data->tx_address = term_to_int32(address_term);
    i2c_data->transmitting_pid = pid;

    return OK_ATOM;
}

static term i2cdriver_end_transmission(Context *ctx, term pid)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        ESP_LOGE(TAG, "i2cdriver_end_transmission: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    esp_err_t result = i2c_bus_manager_transmit(i2c_data->bus_handle, i2c_data->tx_address,
        i2c_data->clock_speed_hz, i2c_data->tx_buf.data, i2c_data->tx_buf.len,
        I2C_PORT_DRIVER_TIMEOUT_MS);

    i2c_tx_buffer_reset(&i2c_data->tx_buf);
    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        ESP_LOGE(TAG, "i2cdriver_end_transmission transmit error: result was: %s", esp_err_to_name(result));
        return make_esp_err_error(ctx, result);
    }

    return OK_ATOM;
}

static term i2cdriver_write_byte(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        ESP_LOGE(TAG, "i2cdriver_write_byte: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    term data_term = term_get_tuple_element(req, 1);
    uint8_t data = term_to_int32(data_term);

    esp_err_t err = i2c_tx_buffer_append_byte(&i2c_data->tx_buf, data);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "i2cdriver_write_byte: failed to enqueue byte: err: %i.", err);
        return make_esp_err_error(ctx, err);
    }

    return OK_ATOM;
}

static term i2cdriver_qwrite_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        ESP_LOGE(TAG, "i2cdriver_qwrite_bytes: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    term data_term = term_get_tuple_element(req, 1);
    if (UNLIKELY(!term_is_binary(data_term))) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return OUT_OF_MEMORY_ATOM;
        }
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    esp_err_t err = i2c_tx_buffer_append(&i2c_data->tx_buf,
        (const uint8_t *) term_binary_data(data_term), term_binary_size(data_term));
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "i2cdriver_qwrite_bytes: failed to enqueue bytes: err: %i.", err);
        return make_esp_err_error(ctx, err);
    }

    return OK_ATOM;
}

static term i2cdriver_read_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        ESP_LOGE(TAG, "i2cdriver_read_bytes: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    int arity = term_get_tuple_arity(req);

    term address_term = term_get_tuple_element(req, 1);
    uint8_t address = term_to_int32(address_term);

    term read_bytes_term = term_get_tuple_element(req, 2);
    avm_int_t read_count = term_to_int32(read_bytes_term);

    term register_term = term_invalid_term();
    uint8_t register_address = 0;
    if (arity == 4) {
        register_term = term_get_tuple_element(req, 3);
        register_address = term_to_int32(register_term);
    }

    // {ok, Data :: binary()}
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(read_count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        ESP_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return OUT_OF_MEMORY_ATOM;
    }
    term data_term = term_create_uninitialized_binary(read_count, &ctx->heap, ctx->global);
    uint8_t *data = (uint8_t *) term_binary_data(data_term);

    esp_err_t result;
    if (!term_is_invalid_term(register_term)) {
        result = i2c_bus_manager_transmit_receive(i2c_data->bus_handle, address,
            i2c_data->clock_speed_hz, &register_address, 1, data, read_count,
            I2C_PORT_DRIVER_TIMEOUT_MS);
    } else {
        result = i2c_bus_manager_receive(i2c_data->bus_handle, address, i2c_data->clock_speed_hz,
            data, read_count, I2C_PORT_DRIVER_TIMEOUT_MS);
    }

    if (UNLIKELY(result != ESP_OK)) {
        ESP_LOGE(TAG, "i2cdriver_read_bytes: transaction error: result was: %s", esp_err_to_name(result));
        return make_esp_err_error(ctx, result);
    }

    term ok_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ok_tuple, 0, OK_ATOM);
    term_put_tuple_element(ok_tuple, 1, data_term);

    return ok_tuple;
}

static term i2cdriver_write_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        ESP_LOGE(TAG, "i2cdriver_write_bytes: Another process is already transmitting");
        return make_einprogress_error(ctx, i2c_data->transmitting_pid);
    }

    int arity = term_get_tuple_arity(req);

    term address_term = term_get_tuple_element(req, 1);
    uint8_t address = term_to_int32(address_term);

    term data_term = term_get_tuple_element(req, 2);
    uint8_t datum;
    uint8_t *data;
    size_t data_len;
    if (term_is_binary(data_term)) {
        data = (uint8_t *) term_binary_data(data_term);
        data_len = term_binary_size(data_term);
    } else {
        datum = term_to_int32(data_term);
        data = &datum;
        data_len = 1;
    }

    bool has_register = false;
    uint8_t register_address = 0;
    if (arity == 4) {
        term register_term = term_get_tuple_element(req, 3);
        register_address = term_to_int32(register_term);
        has_register = true;
    }

    // Only the register-prefixed case needs a scratch buffer to make the
    // register address and data contiguous; otherwise `data` can be handed
    // directly to the transmit call, avoiding an extra alloc + copy.
    esp_err_t result;
    if (has_register) {
        size_t write_size = 1 + data_len;
        uint8_t *write_buf = malloc(write_size);
        if (IS_NULL_PTR(write_buf)) {
            return make_esp_err_error(ctx, ESP_ERR_NO_MEM);
        }
        write_buf[0] = register_address;
        memcpy(write_buf + 1, data, data_len);

        result = i2c_bus_manager_transmit(i2c_data->bus_handle, address,
            i2c_data->clock_speed_hz, write_buf, write_size, I2C_PORT_DRIVER_TIMEOUT_MS);
        free(write_buf);
    } else {
        result = i2c_bus_manager_transmit(i2c_data->bus_handle, address,
            i2c_data->clock_speed_hz, data, data_len, I2C_PORT_DRIVER_TIMEOUT_MS);
    }

    if (UNLIKELY(result != ESP_OK)) {
        ESP_LOGE(TAG, "i2cdriver_write_bytes: transaction error: result was: %s", esp_err_to_name(result));
        return make_esp_err_error(ctx, result);
    }

    return OK_ATOM;
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static NativeHandlerResult i2cdriver_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    GenMessage gen_message;
    if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)) {
        ESP_LOGW(TAG, "Received invalid message.");
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }

#ifdef ENABLE_TRACE
    TRACE("message: ");
    term_display(stdout, msg, ctx);
    TRACE("\n");
#endif

    term cmd_term = term_get_tuple_element(gen_message.req, 0);

    int local_process_id = term_to_local_process_id(gen_message.pid);

    term ret;
    NativeHandlerResult handler_result = NativeContinue;

    enum i2c_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
    switch (cmd) {
        case I2CBeginTransmissionCmd:
            ret = i2cdriver_begin_transmission(ctx, gen_message.pid, gen_message.req);
            break;

        case I2CEndTransmissionCmd:
            ret = i2cdriver_end_transmission(ctx, gen_message.pid);
            break;

        case I2CWriteByteCmd:
            ret = i2cdriver_write_byte(ctx, gen_message.pid, gen_message.req);
            break;

        case I2CReadBytesCmd:
            ret = i2cdriver_read_bytes(ctx, gen_message.pid, gen_message.req);
            break;

        case I2CWriteBytesCmd:
            if (term_get_tuple_arity(gen_message.req) == 2) {
                ret = i2cdriver_qwrite_bytes(ctx, gen_message.pid, gen_message.req);
            } else {
                ret = i2cdriver_write_bytes(ctx, gen_message.pid, gen_message.req);
            }
            break;
        case I2CCloseCmd:
            // ugly hack: we lock before closing so _release and _acquire can assume
            // ctx->platform is not changed.
            globalcontext_get_process_lock(ctx->global, ctx->process_id);
            handler_result = i2c_driver_unref(ctx);
            globalcontext_get_process_unlock(ctx->global, ctx);
            ret = OK_ATOM;
            break;

        default:
            ESP_LOGE(TAG, "i2c: error: unrecognized command: %x", cmd);
            ret = ERROR_ATOM;
    }

    term ret_msg;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &ret, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret_msg = create_pair(ctx, gen_message.ref, ret);
    }

    globalcontext_send_message(ctx->global, local_process_id, ret_msg);
    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return handler_result;
}

I2CAcquireResult i2c_driver_acquire(term i2c_port, i2c_master_bus_handle_t *bus_handle, GlobalContext *global)
{
    if (UNLIKELY(!term_is_local_port(i2c_port))) {
        ESP_LOGW(TAG, "acquire: given term is not a port.");
        return I2CAcquireInvalidPeripheral;
    }

    int local_process_id = term_to_local_process_id(i2c_port);
    Context *ctx = globalcontext_get_process_lock(global, local_process_id);

    if ((ctx == NULL) || (ctx->native_handler != i2cdriver_consume_mailbox)
        || (ctx->platform_data == NULL)) {
        ESP_LOGW(TAG, "acquire: given term is not a I2C port driver.");
        globalcontext_get_process_unlock(global, ctx);
        return I2CAcquireInvalidPeripheral;
    }

    struct I2CData *i2c_data = ctx->platform_data;
    i2c_data->ref_count++;

    *bus_handle = i2c_data->bus_handle;

    globalcontext_get_process_unlock(global, ctx);

    return I2CAcquireOk;
}

void i2c_driver_release(term i2c_port, GlobalContext *global)
{
    if (UNLIKELY(!term_is_local_port(i2c_port))) {
        ESP_LOGW(TAG, "release: given term is not a port.");
        return;
    }

    int local_process_id = term_to_local_process_id(i2c_port);
    Context *ctx = globalcontext_get_process_lock(global, local_process_id);

    if ((ctx == NULL) || (ctx->native_handler != i2cdriver_consume_mailbox)
        || (ctx->platform_data == NULL)) {
        ESP_LOGW(TAG, "release: given term is not a I2C port driver.");
        globalcontext_get_process_unlock(global, ctx);
        return;
    }

    NativeHandlerResult close_result = i2c_driver_unref(ctx);
    if (close_result == NativeTerminate) {
        mailbox_send_term_signal(ctx, KillSignal, NORMAL_ATOM);
    }
    globalcontext_get_process_unlock(global, ctx);
}

//
// entrypoints
//

REGISTER_PORT_DRIVER(i2c, i2c_driver_init, NULL, i2c_driver_create_port)

#endif
