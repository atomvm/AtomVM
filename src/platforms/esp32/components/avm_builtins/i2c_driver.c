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

#include <string.h>

#include <driver/i2c.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_defaultatoms.h"
#include "term.h"
#include "utils.h"

//#define ENABLE_TRACE
#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

static void i2c_driver_init(GlobalContext *global);
static Context *i2c_driver_create_port(GlobalContext *global, term opts);

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req);
static term i2cdriver_end_transmission(Context *ctx, term pid);
static term i2cdriver_write_byte(Context *ctx, term pid, term req);
static void i2cdriver_consume_mailbox(Context *ctx);

static const char *const i2c_driver_atom = "\xA" "i2c_driver";
static term i2c_driver;

struct I2CData
{
    i2c_cmd_handle_t cmd;
    term transmitting_pid;
};

void i2c_driver_init(GlobalContext *global)
{
    int index = globalcontext_insert_atom(global, i2c_driver_atom);
    i2c_driver = term_from_atom_index(index);
}

Context *i2c_driver_create_port(GlobalContext *global, term opts)
{
    Context *ctx = context_new(global);

    struct I2CData *i2c_data = calloc(1, sizeof(struct I2CData));
    i2c_data->transmitting_pid = term_invalid_term();

    ctx->native_handler = i2cdriver_consume_mailbox;
    ctx->platform_data = i2c_data;

    term scl_io_num_term = interop_proplist_get_value(opts, SCL_IO_NUM_ATOM);
    term sda_io_num_term = interop_proplist_get_value(opts, SDA_IO_NUM_ATOM);
    term clock_hz_term = interop_proplist_get_value(opts, I2C_CLOCK_HZ_ATOM);

    i2c_config_t conf;
    memset(&conf, 0, sizeof(i2c_config_t));
    conf.mode = I2C_MODE_MASTER;
    conf.scl_io_num = term_to_int32(scl_io_num_term);
    conf.sda_io_num = term_to_int32(sda_io_num_term);
    conf.sda_pullup_en = GPIO_PULLUP_ENABLE;
    conf.scl_pullup_en = GPIO_PULLUP_ENABLE;
    conf.master.clk_speed = term_to_int32(clock_hz_term);
    esp_err_t ret = i2c_param_config(I2C_NUM_0, &conf);

    if (UNLIKELY(ret != ESP_OK)) {
        TRACE("i2cdriver_init: failed config, return value: %i\n", ret);
        //TODO: return error
        return NULL;
    }

    ret = i2c_driver_install(I2C_NUM_0, I2C_MODE_MASTER, 0, 0, 0);
    if (UNLIKELY(ret != ESP_OK)) {
        TRACE("i2cdriver_init: failed install, return vale: %i\n", ret);
        //TODO: return error
        return NULL;
    }
    return ctx;
}

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        // another process is already transmitting
        TRACE("i2cdriver_begin_transmission: Another process is already transmitting\n");
        return ERROR_ATOM;
    }

    term address_term = term_get_tuple_element(req, 1);
    uint8_t address = term_to_int32(address_term);

    i2c_data->cmd = i2c_cmd_link_create();
    i2c_master_start(i2c_data->cmd);
    i2c_master_write_byte(i2c_data->cmd, (address << 1) | I2C_MASTER_WRITE, true);

    i2c_data->transmitting_pid = pid;

    return OK_ATOM;
}

static term i2cdriver_end_transmission(Context *ctx, term pid)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        TRACE("i2cdriver_end_transmission: Another process is already transmitting\n");
        return ERROR_ATOM;
    }

    i2c_master_stop(i2c_data->cmd);
    esp_err_t result = i2c_master_cmd_begin(I2C_NUM_0, i2c_data->cmd, portMAX_DELAY);
    i2c_cmd_link_delete(i2c_data->cmd);

    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_end_transmission i2c_master_cmd_begin error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term i2cdriver_write_byte(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        TRACE("i2cdriver_write_byte: Another process is already transmitting\n");
        return ERROR_ATOM;
    }

    term data_term = term_get_tuple_element(req, 1);
    uint8_t data = term_to_int32(data_term);
    esp_err_t result = i2c_master_write_byte(i2c_data->cmd, (uint8_t) data, true);

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_write_byte: i2c_master_write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term i2cdriver_qwrite_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        TRACE("i2cdriver_qwrite_bytes: Another process is already transmitting\n");
        return ERROR_ATOM;
    }

    term data_term = term_get_tuple_element(req, 1);
    uint8_t data = term_to_int32(data_term);
    esp_err_t result = i2c_master_write(i2c_data->cmd, (unsigned char *) term_binary_data(data), term_binary_size(data), true);

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_qwrite_bytes: i2c_master_write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term i2cdriver_read_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        TRACE("i2cdriver_read_bytes: Another process is already transmitting\n");
        return ERROR_ATOM;
    }

    int arity = term_get_tuple_arity(req);

    term address_term = term_get_tuple_element(req, 1);
    uint8_t address = term_to_int32(address_term);

    term read_bytes_term = term_get_tuple_element(req, 2);
    avm_int_t read_count = term_to_int32(read_bytes_term);

    term register_term = term_invalid_term();
    uint8_t register_address;
    if (arity == 4) {
        register_term = term_get_tuple_element(req, 3);
        register_address = term_to_int32(register_term);
    }

    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        return ERROR_ATOM;
    }
    term data_term = term_create_uninitialized_binary(read_count, ctx);
    uint8_t *data = (uint8_t *) term_binary_data(data_term);

    i2c_data->cmd = i2c_cmd_link_create();
    i2c_master_start(i2c_data->cmd);

    if (!term_is_invalid_term(register_term)) {
        i2c_master_write_byte(i2c_data->cmd, (address << 1) | I2C_MASTER_WRITE, 0x1);
        i2c_master_write_byte(i2c_data->cmd, register_address, 0x1);
        i2c_master_start(i2c_data->cmd);
    }
    esp_err_t result = i2c_master_write_byte(i2c_data->cmd, (address << 1) | I2C_MASTER_READ, true);

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_read_bytes: i2c_master_write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    result = i2c_master_read(i2c_data->cmd, data, read_count, I2C_MASTER_LAST_NACK);
    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_read_bytes: i2c_master_read error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    i2c_master_stop(i2c_data->cmd);
    result = i2c_master_cmd_begin(I2C_NUM_0, i2c_data->cmd, portMAX_DELAY);
    i2c_cmd_link_delete(i2c_data->cmd);

    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_read_bytes: i2c_master_cmd_begin error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return data_term;
}

static term i2cdriver_write_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        TRACE("i2cdriver_write_bytes: Another process is already transmitting\n");
        return ERROR_ATOM;
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

    term register_term = term_invalid_term();
    uint8_t register_address;
    if (arity == 4) {
        register_term = term_get_tuple_element(req, 3);
        register_address = term_to_int32(register_term);
    }

    i2c_data->cmd = i2c_cmd_link_create();
    i2c_master_start(i2c_data->cmd);

    esp_err_t result = i2c_master_write_byte(i2c_data->cmd, (address << 1) | I2C_MASTER_WRITE, 0x01);
    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_write_bytes i2c_master_write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    if (!term_is_invalid_term(register_term)) {
        i2c_master_write_byte(i2c_data->cmd, register_address, 0x1);
    }

    result = i2c_master_write(i2c_data->cmd, data, data_len, 0x01);
    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_write_bytes i2c_master_write error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    i2c_master_stop(i2c_data->cmd);
    result = i2c_master_cmd_begin(I2C_NUM_0, i2c_data->cmd, portMAX_DELAY);
    i2c_cmd_link_delete(i2c_data->cmd);

    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver_write_bytes: i2c_master_cmd_begin error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, ctx);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static void i2cdriver_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term req = term_get_tuple_element(msg, 2);

    term cmd = term_get_tuple_element(req, 0);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    term ret;

    switch (cmd) {
        case BEGIN_TRANSMISSION_ATOM:
            ret = i2cdriver_begin_transmission(ctx, pid, req);
            break;

        case END_TRANSMISSION_ATOM:
            ret = i2cdriver_end_transmission(ctx, pid);
            break;

        case WRITE_BYTE_ATOM:
            ret = i2cdriver_write_byte(ctx, pid, req);
            break;

        case READ_BYTES_ATOM:
            ret = i2cdriver_read_bytes(ctx, pid, req);
            break;

        case WRITE_BYTES_ATOM:
            if (term_get_tuple_arity(req) == 2) {
                ret = i2cdriver_qwrite_bytes(ctx, pid, req);
            } else {
                ret = i2cdriver_write_bytes(ctx, pid, req);
            }
            break;

        default:
            TRACE("i2c: error: unrecognized command: %x\n", cmd);
            ret = ERROR_ATOM;
    }

    dictionary_put(&ctx->dictionary, ctx, i2c_driver, ret);
    term ret_msg;
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        ret_msg = OUT_OF_MEMORY_ATOM;
    } else {
        ret = dictionary_get(&ctx->dictionary, ctx, i2c_driver);
        term ref = term_get_tuple_element(msg, 1);
        ret_msg = create_pair(ctx, ref, ret);
    }
    dictionary_erase(&ctx->dictionary, ctx, i2c_driver);

    mailbox_send(target, ret_msg);
    mailbox_destroy_message(message);
}

REGISTER_PORT_DRIVER(i2c, i2c_driver_init, i2c_driver_create_port)

#endif
