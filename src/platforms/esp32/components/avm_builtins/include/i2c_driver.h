/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Davide Bettio <davide@uninstall.it>
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

#ifndef _I2C_DRIVER_H_
#define _I2C_DRIVER_H_

#include <driver/i2c_master.h>

#include <globalcontext.h>
#include <term.h>

enum I2CAcquireOpts
{
    I2CAcquireNoOpts
};

enum I2CAcquireResult
{
    I2CAcquireOk,
    I2CAcquireInvalidPeripheral
};

typedef enum I2CAcquireResult I2CAcquireResult;

// These functions are meant for integrating other native drivers with the
// `i2c` port driver, allowing them to share its already-open
// `i2c_master_bus_handle_t` (e.g. to add their own devices to the bus)
// instead of opening a competing bus on the same I2C peripheral.
//
// NOTE: callers must not call `i2c_master_bus_rm_device()`-style APIs on
// devices they did not add themselves, and must not delete the bus handle;
// ownership of the bus remains with the `i2c` port that created it, and is
// only released back via `i2c_driver_release()`.
I2CAcquireResult i2c_driver_acquire(
    term i2c_port, i2c_master_bus_handle_t *bus_handle, GlobalContext *global);
void i2c_driver_release(term i2c_port, GlobalContext *global);

#endif
