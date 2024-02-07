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

#include <driver/i2c.h>

#include <globalcontext.h>
#include <term.h>

#define ATOMVM_ESP32_I2C_OLD_API 1

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

// These functions are meant for integrating native drivers with the I2C port driver
// defined as following only when ATOMVM_ESP32_I2C_OLD_API is set
// it will be changed in future.
I2CAcquireResult i2c_driver_acquire(term i2c_port, i2c_port_t *i2c_num, GlobalContext *global);
void i2c_driver_release(term i2c_port, GlobalContext *global);

#endif
