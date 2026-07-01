/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 The AtomVM Project
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

#include "include/i2c_bus_manager.h"

#include <stdlib.h>
#include <string.h>

#include <esp_log.h>

#include "utils.h"

#define TAG "i2c_bus_manager"

#define I2C_TX_BUFFER_INITIAL_CAP 16

esp_err_t i2c_bus_manager_open(
    int i2c_port, int scl_io_num, int sda_io_num, i2c_master_bus_handle_t *out_bus_handle)
{
    i2c_master_bus_config_t bus_config;
    memset(&bus_config, 0, sizeof(bus_config));
    bus_config.i2c_port = i2c_port;
    bus_config.scl_io_num = scl_io_num;
    bus_config.sda_io_num = sda_io_num;
    bus_config.clk_source = I2C_CLK_SRC_DEFAULT;
    // Matches the glitch filter length recommended by Espressif examples and
    // used by other in-tree/vendored I2C master bus users.
    bus_config.glitch_ignore_cnt = 7;
    bus_config.flags.enable_internal_pullup = true;

    return i2c_new_master_bus(&bus_config, out_bus_handle);
}

esp_err_t i2c_bus_manager_close(i2c_master_bus_handle_t bus_handle)
{
    return i2c_del_master_bus(bus_handle);
}

static esp_err_t add_device(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, i2c_master_dev_handle_t *out_dev_handle)
{
    i2c_device_config_t dev_config;
    memset(&dev_config, 0, sizeof(dev_config));
    dev_config.dev_addr_length = I2C_ADDR_BIT_LEN_7;
    dev_config.device_address = address;
    dev_config.scl_speed_hz = clock_speed_hz;

    return i2c_master_bus_add_device(bus_handle, &dev_config, out_dev_handle);
}

// The legacy `driver/i2c.h` API (via `i2c_master_cmd_begin()`) always reported
// a NACK'd transaction as `ESP_FAIL`. The new I2C master driver reports the
// very same condition as `ESP_ERR_INVALID_STATE` on IDF <= 5.5, and as
// `ESP_ERR_INVALID_RESPONSE` starting with IDF 6.0 (see the "I2C Master
// Driver Updates" section of the IDF 6.0 migration guide). To preserve a
// single, version-independent `{error, esp_fail}` contract for existing
// AtomVM applications (and the `i2c` NACK test suite) across all supported
// IDF versions, both codes are normalized to `ESP_FAIL` here.
//
// NOTE: because each transaction owns a freshly-added, then removed, device
// handle (see `add_device`/`remove_device`), a NACK is by far the most likely
// cause of either of these two codes in this driver's usage pattern.
static esp_err_t normalize_transaction_error(esp_err_t err)
{
    if (err == ESP_ERR_INVALID_STATE || err == ESP_ERR_INVALID_RESPONSE) {
        return ESP_FAIL;
    }
    return err;
}

static esp_err_t remove_device(i2c_master_dev_handle_t dev_handle, esp_err_t transaction_result)
{
    esp_err_t err = i2c_master_bus_rm_device(dev_handle);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGW(TAG, "Failed to remove transient I2C device handle: err=%i", err);
    }
    // Prefer surfacing the transaction error (if any) over a remove-device
    // error, since the former is more useful/actionable to the caller.
    return (transaction_result != ESP_OK) ? transaction_result : err;
}

esp_err_t i2c_bus_manager_transmit(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, const uint8_t *write_buffer, size_t write_size, int xfer_timeout_ms)
{
    i2c_master_dev_handle_t dev_handle;
    esp_err_t err = add_device(bus_handle, address, clock_speed_hz, &dev_handle);
    if (UNLIKELY(err != ESP_OK)) {
        return err;
    }

    err = normalize_transaction_error(
        i2c_master_transmit(dev_handle, write_buffer, write_size, xfer_timeout_ms));

    return remove_device(dev_handle, err);
}

esp_err_t i2c_bus_manager_receive(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, uint8_t *read_buffer, size_t read_size, int xfer_timeout_ms)
{
    i2c_master_dev_handle_t dev_handle;
    esp_err_t err = add_device(bus_handle, address, clock_speed_hz, &dev_handle);
    if (UNLIKELY(err != ESP_OK)) {
        return err;
    }

    err = normalize_transaction_error(
        i2c_master_receive(dev_handle, read_buffer, read_size, xfer_timeout_ms));

    return remove_device(dev_handle, err);
}

esp_err_t i2c_bus_manager_transmit_receive(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, const uint8_t *write_buffer, size_t write_size,
    uint8_t *read_buffer, size_t read_size, int xfer_timeout_ms)
{
    i2c_master_dev_handle_t dev_handle;
    esp_err_t err = add_device(bus_handle, address, clock_speed_hz, &dev_handle);
    if (UNLIKELY(err != ESP_OK)) {
        return err;
    }

    err = normalize_transaction_error(i2c_master_transmit_receive(
        dev_handle, write_buffer, write_size, read_buffer, read_size, xfer_timeout_ms));

    return remove_device(dev_handle, err);
}

void i2c_tx_buffer_init(struct I2CTxBuffer *buf)
{
    buf->data = NULL;
    buf->len = 0;
    buf->cap = 0;
}

void i2c_tx_buffer_reset(struct I2CTxBuffer *buf)
{
    free(buf->data);
    buf->data = NULL;
    buf->len = 0;
    buf->cap = 0;
}

esp_err_t i2c_tx_buffer_append(struct I2CTxBuffer *buf, const uint8_t *data, size_t len)
{
    if (len == 0) {
        return ESP_OK;
    }

    if (buf->len + len > buf->cap) {
        size_t new_cap = buf->cap == 0 ? I2C_TX_BUFFER_INITIAL_CAP : buf->cap * 2;
        while (new_cap < buf->len + len) {
            new_cap *= 2;
        }
        uint8_t *new_data = realloc(buf->data, new_cap);
        if (IS_NULL_PTR(new_data)) {
            return ESP_ERR_NO_MEM;
        }
        buf->data = new_data;
        buf->cap = new_cap;
    }

    memcpy(buf->data + buf->len, data, len);
    buf->len += len;

    return ESP_OK;
}

esp_err_t i2c_tx_buffer_append_byte(struct I2CTxBuffer *buf, uint8_t byte)
{
    return i2c_tx_buffer_append(buf, &byte, 1);
}
