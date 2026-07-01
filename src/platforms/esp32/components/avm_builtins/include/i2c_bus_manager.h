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

#ifndef _I2C_BUS_MANAGER_H_
#define _I2C_BUS_MANAGER_H_

#include <stddef.h>
#include <stdint.h>

#include <driver/i2c_master.h>
#include <driver/i2c_types.h>
#include <esp_err.h>

#ifdef __cplusplus
extern "C" {
#endif

// Sentinel value for `xfer_timeout_ms` meaning "wait forever".
#define I2C_BUS_MANAGER_TIMEOUT_INFINITE (-1)

/**
 * @brief Create a new I2C master bus.
 *
 * This is a thin wrapper around `i2c_new_master_bus()`.  Unlike the legacy
 * `driver/i2c.h` API, the new driver does not associate a clock speed with
 * the bus itself: clock speed is a property of each device (address) added
 * to the bus.  AtomVM's Erlang-level API only exposes a single clock speed
 * per opened `i2c()` handle, which callers should pass to each of the
 * transaction functions below.
 */
esp_err_t i2c_bus_manager_open(
    int i2c_port, int scl_io_num, int sda_io_num, i2c_master_bus_handle_t *out_bus_handle);

/**
 * @brief Delete a previously created I2C master bus.
 */
esp_err_t i2c_bus_manager_close(i2c_master_bus_handle_t bus_handle);

/**
 * @brief Perform a single write transaction to a given address.
 *
 * AtomVM's I2C API takes the target address on every call rather than at
 * open time (e.g., a bus scanner probes arbitrary addresses on one open
 * bus).  Since the new I2C driver requires a persistent per-address device
 * handle for any transaction, this function adds a transient device handle
 * for the given address, performs the transaction, and removes the device
 * handle again before returning.
 *
 * If `write_size` is 0, an address-only ACK/NACK probe is performed via
 * `i2c_master_probe()` instead (the underlying driver rejects zero-length
 * write buffers outright), matching the legacy driver's support for
 * address-only transactions (e.g. device-presence or busy-polling checks).
 */
esp_err_t i2c_bus_manager_transmit(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, const uint8_t *write_buffer, size_t write_size,
    int xfer_timeout_ms);

/**
 * @brief Perform a single read transaction from a given address.
 *
 * See `i2c_bus_manager_transmit()` for a description of the transient
 * device handle strategy used here.
 */
esp_err_t i2c_bus_manager_receive(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, uint8_t *read_buffer, size_t read_size, int xfer_timeout_ms);

/**
 * @brief Perform a write, followed by a (repeated-start) read, from a given
 * address (typically used to read from a specific device register).
 *
 * See `i2c_bus_manager_transmit()` for a description of the transient
 * device handle strategy used here.
 */
esp_err_t i2c_bus_manager_transmit_receive(i2c_master_bus_handle_t bus_handle, uint16_t address,
    uint32_t clock_speed_hz, const uint8_t *write_buffer, size_t write_size,
    uint8_t *read_buffer, size_t read_size, int xfer_timeout_ms);

/**
 * @brief A simple growable byte buffer, used by both the `i2c` port driver
 * and the `i2c` NIF-resource driver to accumulate bytes written across a
 * `begin_transmission/write_byte(s).../end_transmission` sequence.
 *
 * The legacy driver deferred the actual bus transaction until
 * `i2c_master_cmd_begin()` was called at the end of such a sequence (via a
 * command link); the new driver has no equivalent of a command link, so
 * the bytes are accumulated here instead, and sent in one
 * `i2c_bus_manager_transmit()` call from `end_transmission`.
 */
struct I2CTxBuffer
{
    uint8_t *data;
    size_t len;
    size_t cap;
};

void i2c_tx_buffer_init(struct I2CTxBuffer *buf);
void i2c_tx_buffer_reset(struct I2CTxBuffer *buf);
esp_err_t i2c_tx_buffer_append(struct I2CTxBuffer *buf, const uint8_t *data, size_t len);
esp_err_t i2c_tx_buffer_append_byte(struct I2CTxBuffer *buf, uint8_t byte);

#ifdef __cplusplus
}
#endif

#endif
