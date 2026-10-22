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
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Minimal VHCI host interface for the ESP32-C5 BLE controller, compatible with
 * the ESP-IDF esp_vhci_host_* API.  Packets use HCI H4 framing: the first byte
 * is the packet indicator (0x01 command, 0x02 ACL data, 0x04 event).
 *
 * Successful operations return ESP_OK (0); failures return a nonzero error
 * code. esp_err_t is an int.
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

/* Synchronous lifecycle for native host tasks. Open returns -EBUSY when the
 * smoke test or another native host owns the controller. Close may be retried
 * after a controller-disable failure. Serialize native lifecycle calls and
 * wait for asynchronous startup before closing. Do not call these from a VM
 * scheduler. */
int rtems_bt_vhci_open(void);
int rtems_bt_vhci_close(void);

/* The C5 profile permits 517 ACL payload bytes, plus the H4/ACL headers. */
#define RTEMS_BT_VHCI_ACL_SIZE 517u
#define RTEMS_BT_VHCI_PACKET_SIZE (1u + 4u + RTEMS_BT_VHCI_ACL_SIZE)

static inline bool rtems_bt_vhci_valid_packet(const uint8_t *data, size_t len)
{
    if (data == NULL || len < 4) {
        return false;
    }
    if (data[0] == 0x01) {
        return len == 4u + data[3];
    }
    if (data[0] == 0x02 && len >= 5) {
        size_t payload = data[3] | ((size_t) data[4] << 8);
        return payload <= RTEMS_BT_VHCI_ACL_SIZE && len == 5u + payload;
    }
    return false;
}

/* Checked variant used by the NIF: malformed packets and exhausted controller
 * buffers must not be reported to the host as successful sends. */
int rtems_bt_vhci_send_packet(const uint8_t *data, uint16_t len);

int esp_vhci_host_register_callback(const esp_vhci_host_callback_t *callback);
void esp_vhci_host_send_packet(uint8_t *data, uint16_t len);
bool esp_vhci_host_check_send_available(void);

#ifdef __cplusplus
}
#endif

#endif
