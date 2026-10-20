/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#ifndef RTEMS_BT_H
#define RTEMS_BT_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Brings up the ESP32-C3 BLE controller and starts a minimal HCI advertising
 * smoke test. Shared modem startup supports either Wi-Fi or Bluetooth first.
 * This is a non-connectable advertiser; a BLE host/GATT stack is not provided.
 *
 * Initialization and advertising setup run in a native task. Returns false
 * if that task cannot start or a previous initialization failed. Poll the
 * state to observe HCI command completion without blocking the VM scheduler.
 */
bool rtems_bt_start_async(void);

typedef enum
{
    RTEMS_BT_STOPPED,
    RTEMS_BT_STARTING,
    RTEMS_BT_STARTED,
    RTEMS_BT_FAILED
} rtems_bt_state_t;

rtems_bt_state_t rtems_bt_get_state(void);

/*
 * True when the controller has been started and advertising was submitted.
 */
bool rtems_bt_is_started(void);

#ifdef __cplusplus
}
#endif

#endif
