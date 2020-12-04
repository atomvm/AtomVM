/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "i2cdriver.h"

#include <string.h>

#include <driver/i2c.h>
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
#include "term.h"
#include "utils.h"

#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req);
static term i2cdriver_end_transmission(Context *ctx, term pid);
static term i2cdriver_write_byte(Context *ctx, term pid, term req);
static void i2cdriver_consume_mailbox(Context *ctx);

struct I2CData
{
    i2c_cmd_handle_t cmd;
    term transmitting_pid;
};

void i2cdriver_init(Context *ctx, term opts)
{
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
        TRACE("i2cdriver: failed config, return value: %i\n", ret);
        //TODO: return error
        return;
    }

    ret = i2c_driver_install(I2C_NUM_0, I2C_MODE_MASTER, 0, 0, 0);
    if (UNLIKELY(ret != ESP_OK)) {
        TRACE("i2cdriver: failed install, return vale: %i\n", ret);
        //TODO: return error
    }
}

static term i2cdriver_begin_transmission(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        // another process is already transmitting
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
        return ERROR_ATOM;
    }

    i2c_master_stop(i2c_data->cmd);
    esp_err_t result = i2c_master_cmd_begin(I2C_NUM_0, i2c_data->cmd, portMAX_DELAY);
    i2c_cmd_link_delete(i2c_data->cmd);

    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver end_transmission error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term i2cdriver_write_byte(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != pid)) {
        // transaction owned from a different pid
        return ERROR_ATOM;
    }

    term data_term = term_get_tuple_element(req, 1);
    uint8_t data = term_to_int32(data_term);
    esp_err_t result = i2c_master_write_byte(i2c_data->cmd, (uint8_t) data, true);

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term i2cdriver_read_bytes(Context *ctx, term pid, term req)
{
    struct I2CData *i2c_data = ctx->platform_data;

    if (UNLIKELY(i2c_data->transmitting_pid != term_invalid_term())) {
        // another process is already transmitting
        return ERROR_ATOM;
    }

    term address_term = term_get_tuple_element(req, 1);
    uint8_t address = term_to_int32(address_term);

    term read_bytes_term = term_get_tuple_element(req, 2);
    avm_int_t read_count = term_to_int32(read_bytes_term);

    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        return ERROR_ATOM;
    }
    term data_term = term_create_uninitialized_binary(read_count, ctx);
    uint8_t *data = (uint8_t *) term_binary_data(data_term);

    i2c_data->cmd = i2c_cmd_link_create();
    i2c_master_start(i2c_data->cmd);
    esp_err_t result = i2c_master_write_byte(i2c_data->cmd, (address << 1) | I2C_MASTER_READ, true);

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver read_bytes error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    result = i2c_master_read(i2c_data->cmd, data, read_count, I2C_MASTER_LAST_NACK);
    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver read_bytes error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    i2c_master_stop(i2c_data->cmd);
    result = i2c_master_cmd_begin(I2C_NUM_0, i2c_data->cmd, portMAX_DELAY);
    i2c_cmd_link_delete(i2c_data->cmd);

    i2c_data->transmitting_pid = term_invalid_term();

    if (UNLIKELY(result != ESP_OK)) {
        TRACE("i2cdriver write_byte error: result was: %i.\n", result);
        return ERROR_ATOM;
    }

    return data_term;
}

static void i2cdriver_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
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

        default:
            TRACE("i2c: error: unrecognized command: %lx\n", cmd);
            ret = ERROR_ATOM;
    }

    mailbox_destroy_message(message);

    UNUSED(ref);
    mailbox_send(target, ret);
}
