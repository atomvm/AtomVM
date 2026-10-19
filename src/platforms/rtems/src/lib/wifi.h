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

#ifndef _RTEMS_WIFI_H_
#define _RTEMS_WIFI_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum
{
    RTEMS_WIFI_DISCONNECTED = 0,
    RTEMS_WIFI_SCANNING,
    RTEMS_WIFI_ASSOCIATING,
    RTEMS_WIFI_ASSOCIATED,
    RTEMS_WIFI_GOT_IP
} rtems_wifi_state_t;

struct rtems_wifi_ip_info
{
    uint32_t ip;
    uint32_t netmask;
    uint32_t gateway;
};

typedef void (*rtems_wifi_event_cb)(rtems_wifi_state_t state, const struct rtems_wifi_ip_info *ip_info, void *arg);

int rtems_wifi_create_wlandev(void);
bool rtems_wifi_is_available(void);
rtems_wifi_state_t rtems_wifi_get_state(void);
int rtems_wifi_get_ip_info(struct rtems_wifi_ip_info *info);
/* Returns zero on success, or an errno value on failure. RSSI is in dBm. */
int rtems_wifi_get_rssi(int *rssi);

int rtems_wifi_start(const char *ssid, const char *psk, bool connect);
int rtems_wifi_start_with_config_file(const char *conf_path);
int rtems_wifi_stop(void);

void rtems_wifi_set_event_callback(rtems_wifi_event_cb cb, void *arg);
void rtems_wifi_notify_dhcp_event(const char *interface, const char *reason,
    uint32_t ip, uint32_t netmask, uint32_t gateway);

bool rtems_wifi_check_sd_config_and_start(void);

#ifdef __cplusplus
}
#endif

#endif
