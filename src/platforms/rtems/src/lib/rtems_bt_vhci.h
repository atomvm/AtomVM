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

#ifndef RTEMS_BT_VHCI_H
#define RTEMS_BT_VHCI_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Minimal VHCI host interface for the ESP32-C5 BLE controller, compatible with
 * the ESP-IDF esp_vhci_host_* API.  Packets use HCI H4 framing: the first byte
 * is the packet indicator (0x01 command, 0x02 ACL data, 0x04 event).
 *
 * Return values are ESP_OK (0) or a negative error code; esp_err_t is an int.
 */

typedef struct esp_vhci_host_callback
{
    void (*notify_host_send_available)(void);
    int (*notify_host_recv)(uint8_t *data, uint16_t len);
} esp_vhci_host_callback_t;

/*
 * Brings up the controller without the advertising smoke test and leaves the
 * HCI transport to the VHCI host.  Mutually exclusive with
 * rtems_bt_start_async(); poll rtems_bt_is_started() before registering.
 */
bool rtems_bt_vhci_start_async(void);

int esp_vhci_host_register_callback(const esp_vhci_host_callback_t *callback);
void esp_vhci_host_send_packet(uint8_t *data, uint16_t len);
bool esp_vhci_host_check_send_available(void);

#ifdef __cplusplus
}
#endif

#endif
