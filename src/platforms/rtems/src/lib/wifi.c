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

#include "wifi.h"

#include <errno.h>
#include <fcntl.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(RTEMS_HAS_LIBBSD) && defined(RTEMS_HAS_WIFI)
#include <machine/rtems-bsd-commands.h>
#include <rtems.h>
#include <rtems/bsd/bsd.h>

#define WPA_WATCHER_PRIO 120
#define WPA_WATCHER_STACK_SIZE (32 * 1024)
#define SD_WPA_CONF_PATH "/media/mmcsd-0-0/wpa_supplicant.conf"
#define TMP_WPA_CONF_PATH "/tmp/wpa_supplicant.conf"

static atomic_bool wifi_enabled = ATOMIC_VAR_INIT(false);
static atomic_int wifi_state = ATOMIC_VAR_INIT(RTEMS_WIFI_DISCONNECTED);
static rtems_id wpa_task_id = RTEMS_ID_NONE;
static char current_conf_path[256] = {0};

static rtems_wifi_event_cb event_cb = NULL;
static void *event_cb_arg = NULL;
static struct rtems_wifi_ip_info current_ip_info = {0};

static void set_state(rtems_wifi_state_t new_state)
{
    atomic_store_explicit(&wifi_state, new_state, memory_order_release);
    if (event_cb != NULL) {
        event_cb(new_state, &current_ip_info, event_cb_arg);
    }
}

static bool check_file_exists(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
}

int rtems_wifi_create_wlandev(void)
{
    char *ifcfg_check[] = {"ifconfig", "wlan0", NULL};
    if (rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_check), ifcfg_check) == EXIT_SUCCESS) {
        return 0;
    }

    /* Wait for USB host stack to enumerate the Realtek RTL8188EU device (rtwn0) */
    char *ifcfg_rtwn[] = {"ifconfig", "rtwn0", NULL};
    bool rtwn_found = false;
    for (int attempt = 0; attempt < 10; attempt++) {
        if (rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_rtwn), ifcfg_rtwn) == EXIT_SUCCESS) {
            rtwn_found = true;
            break;
        }
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(500));
    }

    if (!rtwn_found) {
        fprintf(stderr, "AtomVM wifi: rtwn0 hardware device not available\n");
        return -1;
    }

    char *ifcfg_create[] = {"ifconfig", "wlan0", "create", "wlandev", "rtwn0", "down", NULL};
    int rv = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_create), ifcfg_create);
    if (rv != EXIT_SUCCESS) {
        fprintf(stderr, "AtomVM wifi: failed to create wlan0 clone\n");
        return -1;
    }

    rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(500));

    char *ifcfg_up[] = {"ifconfig", "wlan0", "up", NULL};
    rv = rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_up), ifcfg_up);
    if (rv != EXIT_SUCCESS) {
        fprintf(stderr, "AtomVM wifi: failed to bring wlan0 up\n");
        return -1;
    }

    return 0;
}

static void wpa_supplicant_watcher_task(rtems_task_argument arg)
{
    (void) arg;
    time_t last_start = 0;
    const time_t sleep_min = 1;
    const time_t sleep_max = 30;
    time_t delay_in_seconds = sleep_min;

    while (atomic_load_explicit(&wifi_enabled, memory_order_acquire)) {
        if (rtems_wifi_create_wlandev() != 0) {
            rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(1000));
            continue;
        }

        set_state(RTEMS_WIFI_ASSOCIATING);

        char *wpa_argv[] = {
            "wpa_supplicant",
            "-Dbsd",
            "-iwlan0",
            "-c",
            current_conf_path,
            NULL
        };
        int argc = RTEMS_BSD_ARGC(wpa_argv);

        time_t now = rtems_clock_get_uptime_seconds();
        if ((now - last_start) > (2 * sleep_max)) {
            delay_in_seconds = sleep_min;
        } else {
            delay_in_seconds *= 2;
            if (delay_in_seconds > sleep_max) {
                delay_in_seconds = sleep_max;
            }
        }
        last_start = now;

        int exit_code = rtems_bsd_command_wpa_supplicant(argc, wpa_argv);
        (void) exit_code;

        if (!atomic_load_explicit(&wifi_enabled, memory_order_acquire)) {
            break;
        }

        set_state(RTEMS_WIFI_DISCONNECTED);
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(delay_in_seconds * 1000));
    }

    wpa_task_id = RTEMS_ID_NONE;
    rtems_task_exit();
}

int rtems_wifi_start_with_config_file(const char *conf_path)
{
    if (!check_file_exists(conf_path)) {
        fprintf(stderr, "AtomVM wifi: config file not found: %s\n", conf_path);
        return -1;
    }

    rtems_wifi_stop();

    strncpy(current_conf_path, conf_path, sizeof(current_conf_path) - 1);
    current_conf_path[sizeof(current_conf_path) - 1] = '\0';
    atomic_store_explicit(&wifi_enabled, true, memory_order_release);

    rtems_status_code sc = rtems_task_create(
        rtems_build_name('W', 'P', 'A', 'W'),
        WPA_WATCHER_PRIO,
        WPA_WATCHER_STACK_SIZE,
        RTEMS_DEFAULT_MODES,
        RTEMS_FLOATING_POINT,
        &wpa_task_id
    );
    if (sc != RTEMS_SUCCESSFUL) {
        fprintf(stderr, "AtomVM wifi: failed to create WPA task: %s\n", rtems_status_text(sc));
        atomic_store_explicit(&wifi_enabled, false, memory_order_release);
        return -1;
    }

    sc = rtems_task_start(wpa_task_id, wpa_supplicant_watcher_task, 0);
    if (sc != RTEMS_SUCCESSFUL) {
        fprintf(stderr, "AtomVM wifi: failed to start WPA task: %s\n", rtems_status_text(sc));
        rtems_task_delete(wpa_task_id);
        wpa_task_id = RTEMS_ID_NONE;
        atomic_store_explicit(&wifi_enabled, false, memory_order_release);
        return -1;
    }

    return 0;
}

int rtems_wifi_start(const char *ssid, const char *psk)
{
    if (ssid == NULL || strlen(ssid) == 0) {
        return -1;
    }

    mkdir("/var", 0755);
    mkdir("/var/run", 0755);
    mkdir("/var/run/wpa_supplicant", 0755);
    mkdir("/tmp", 0755);

    FILE *f = fopen(TMP_WPA_CONF_PATH, "w");
    if (f == NULL) {
        fprintf(stderr, "AtomVM wifi: cannot write %s\n", TMP_WPA_CONF_PATH);
        return -1;
    }

    fprintf(f, "ctrl_interface=/var/run/wpa_supplicant\n");
    fprintf(f, "network={\n");
    fprintf(f, "    ssid=\"%s\"\n", ssid);
    if (psk != NULL && strlen(psk) > 0) {
        fprintf(f, "    psk=\"%s\"\n", psk);
        fprintf(f, "    key_mgmt=WPA-PSK\n");
    } else {
        fprintf(f, "    key_mgmt=NONE\n");
    }
    fprintf(f, "}\n");
    fclose(f);

    return rtems_wifi_start_with_config_file(TMP_WPA_CONF_PATH);
}

int rtems_wifi_stop(void)
{
    if (!atomic_load_explicit(&wifi_enabled, memory_order_acquire)) {
        return 0;
    }

    atomic_store_explicit(&wifi_enabled, false, memory_order_release);

    if (wpa_task_id != RTEMS_ID_NONE) {
        rtems_task_delete(wpa_task_id);
        wpa_task_id = RTEMS_ID_NONE;
    }

    char *ifcfg_down[] = {"ifconfig", "wlan0", "down", NULL};
    rtems_bsd_command_ifconfig(RTEMS_BSD_ARGC(ifcfg_down), ifcfg_down);

    set_state(RTEMS_WIFI_DISCONNECTED);
    return 0;
}

bool rtems_wifi_is_available(void)
{
    return true;
}

rtems_wifi_state_t rtems_wifi_get_state(void)
{
    return (rtems_wifi_state_t) atomic_load_explicit(&wifi_state, memory_order_acquire);
}

int rtems_wifi_get_ip_info(struct rtems_wifi_ip_info *info)
{
    if (info == NULL) {
        return -1;
    }
    *info = current_ip_info;
    return 0;
}

void rtems_wifi_set_event_callback(rtems_wifi_event_cb cb, void *arg)
{
    event_cb = cb;
    event_cb_arg = arg;
}

void rtems_wifi_notify_dhcp_event(const char *interface, const char *reason,
                                 uint32_t ip, uint32_t netmask, uint32_t gateway)
{
    if (interface == NULL || strcmp(interface, "wlan0") != 0) {
        return;
    }

    if (reason != NULL && (strcmp(reason, "BOUND") == 0
            || strcmp(reason, "RENEW") == 0
            || strcmp(reason, "REBIND") == 0
            || strcmp(reason, "INFORM") == 0
            || strcmp(reason, "REBOOT") == 0)) {
        current_ip_info.ip = ip;
        current_ip_info.netmask = netmask;
        current_ip_info.gateway = gateway;
        set_state(RTEMS_WIFI_GOT_IP);
    } else if (reason != NULL && (strcmp(reason, "EXPIRE") == 0
            || strcmp(reason, "FAIL") == 0
            || strcmp(reason, "STOP") == 0
            || strcmp(reason, "NOCARRIER") == 0)) {
        current_ip_info.ip = 0;
        current_ip_info.netmask = 0;
        current_ip_info.gateway = 0;
        set_state(RTEMS_WIFI_DISCONNECTED);
    }
}

bool rtems_wifi_check_sd_config_and_start(void)
{
    if (check_file_exists(SD_WPA_CONF_PATH)) {
        printf("AtomVM wifi: Found %s on SD card, starting Wi-Fi...\n", SD_WPA_CONF_PATH);
        return rtems_wifi_start_with_config_file(SD_WPA_CONF_PATH) == 0;
    }
    return false;
}

#elif defined(RTEMS_WIFI_BACKEND_ESP32)

#include <esp_wifi.h>
#include <esp_event.h>
#include <rtems-esp/netif.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <lwip/dns.h>
#include <lwip/ip4_addr.h>
#include <netstart.h>
#include <stdatomic.h>
#include <string.h>
#include <stdio.h>
#include <rtems.h>

static atomic_bool wifi_enabled = false;
static atomic_int wifi_state = RTEMS_WIFI_DISCONNECTED;
static rtems_wifi_event_cb event_cb = NULL;
static void *event_cb_arg = NULL;
static struct rtems_wifi_ip_info current_ip_info = {0};
static bool wifi_initialized = false;
static rtems_id dhcp_poll_task_id = RTEMS_ID_NONE;

int os_get_random(unsigned char *buf, size_t len)
{
#if defined(RTEMS_BOARD_ESP32C6) || defined(RTEMS_BOARD_ESP32C5)
    volatile uint32_t *rng = (volatile uint32_t *) 0x600B2808;
#else
    volatile uint32_t *rng = (volatile uint32_t *) 0x600260b0;
#endif
    while (len > 0) {
        uint32_t r = *rng;
        size_t chunk = len < sizeof(uint32_t) ? len : sizeof(uint32_t);
        memcpy(buf, &r, chunk);
        buf += chunk;
        len -= chunk;
    }
    return 0;
}

void os_sleep(long sec, long usec)
{
    rtems_interval ticks = RTEMS_MILLISECONDS_TO_TICKS(sec * 1000 + usec / 1000);
    if (ticks == 0 && (sec > 0 || usec > 0)) {
        ticks = 1;
    }
    if (ticks > 0) {
        rtems_task_wake_after(ticks);
    }
}

static void set_state(rtems_wifi_state_t new_state)
{
    atomic_store_explicit(&wifi_state, new_state, memory_order_release);
    if (event_cb != NULL) {
        event_cb(new_state, &current_ip_info, event_cb_arg);
    }
}

static rtems_task dhcp_poll_task(rtems_task_argument arg)
{
    (void) arg;
    while (atomic_load_explicit(&wifi_enabled, memory_order_acquire)) {
        struct netif *netif = rtems_esp_netif_get();
        struct dhcp *d = netif ? netif_dhcp_data(netif) : NULL;
        if (netif != NULL && (dhcp_supplied_address(netif) || (d != NULL && ip4_addr_get_u32(&d->offered_ip_addr) != 0))) {
            if (!dhcp_supplied_address(netif)) {
                netif_set_addr(netif, &d->offered_ip_addr, &d->offered_sn_mask, &d->offered_gw_addr);
                netif_set_up(netif);
            }
            uint32_t ip = lwip_ntohl(ip4_addr_get_u32(netif_ip4_addr(netif)));
            uint32_t netmask = lwip_ntohl(ip4_addr_get_u32(netif_ip4_netmask(netif)));
            uint32_t gw = lwip_ntohl(ip4_addr_get_u32(netif_ip4_gw(netif)));

            if (d != NULL && ip4_addr_get_u32(&d->offered_gw_addr) != 0) {
                ip_addr_t dns_addr;
                ip_addr_copy_from_ip4(dns_addr, d->offered_gw_addr);
                dns_setserver(0, &dns_addr);
            }

            rtems_wifi_notify_dhcp_event("wlan0", "BOUND", ip, netmask, gw);
            printf("rtems-wifi: DHCP bound! IP=%u.%u.%u.%u, GW=%u.%u.%u.%u\n",
                (ip >> 24) & 0xff, (ip >> 16) & 0xff, (ip >> 8) & 0xff, ip & 0xff,
                (gw >> 24) & 0xff, (gw >> 16) & 0xff, (gw >> 8) & 0xff, gw & 0xff);
            break;
        }
        rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(500));
    }
    dhcp_poll_task_id = RTEMS_ID_NONE;
    rtems_task_exit();
}

static void on_wifi_event(void *arg, const char *base, int32_t id, void *data)
{
    (void) arg;
    (void) data;
    if (base != NULL && strcmp(base, WIFI_EVENT) == 0) {
        if (id == WIFI_EVENT_STA_START) {
            set_state(RTEMS_WIFI_ASSOCIATING);
        } else if (id == WIFI_EVENT_STA_CONNECTED) {
            set_state(RTEMS_WIFI_ASSOCIATED);
            rtems_esp_netif_reattach();
            struct netif *netif = rtems_esp_netif_get();
            if (netif != NULL) {
                netif_set_default(netif);
                if (dhcp_poll_task_id == RTEMS_ID_NONE) {
                    dhcp_start(netif);
                    rtems_status_code sc = rtems_task_create(
                        rtems_build_name('W', 'D', 'H', 'C'),
                        120,
                        4096,
                        RTEMS_DEFAULT_MODES,
                        RTEMS_DEFAULT_ATTRIBUTES,
                        &dhcp_poll_task_id
                    );
                    if (sc == RTEMS_SUCCESSFUL) {
                        rtems_task_start(dhcp_poll_task_id, dhcp_poll_task, 0);
                    }
                }
            }
        } else if (id == WIFI_EVENT_STA_DISCONNECTED) {
            current_ip_info.ip = 0;
            current_ip_info.netmask = 0;
            current_ip_info.gateway = 0;
            set_state(RTEMS_WIFI_DISCONNECTED);
        }
    }
}

int rtems_wifi_create_wlandev(void)
{
    return 0;
}

bool rtems_wifi_is_available(void)
{
    return true;
}

rtems_wifi_state_t rtems_wifi_get_state(void)
{
    return (rtems_wifi_state_t) atomic_load_explicit(&wifi_state, memory_order_acquire);
}

int rtems_wifi_get_ip_info(struct rtems_wifi_ip_info *info)
{
    if (info == NULL) {
        return -1;
    }
    memset(info, 0, sizeof(*info));
    if (atomic_load_explicit(&wifi_state, memory_order_acquire) != RTEMS_WIFI_GOT_IP) {
        return -1;
    }
    *info = current_ip_info;
    return 0;
}

int rtems_wifi_start(const char *ssid, const char *psk)
{
    if (ssid == NULL) {
        return -1;
    }

    if (!wifi_initialized) {
        wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
#if defined(RTEMS_BOARD_ESP32C3) || defined(RTEMS_BOARD_ESP32C5)
        cfg.static_rx_buf_num = 4;
        cfg.dynamic_rx_buf_num = 16;
        cfg.dynamic_tx_buf_num = 16;
        cfg.rx_mgmt_buf_num = 2;
#endif
        esp_err_t err = esp_wifi_init(&cfg);
        if (err != ESP_OK) {
            fprintf(stderr, "AtomVM wifi: esp_wifi_init failed: %d\n", err);
            return -1;
        }

        int net_rc = start_networking(NULL, NULL, NULL, NULL, NULL);
        if (net_rc != 0) {
            fprintf(stderr, "AtomVM wifi: start_networking returned %d\n", net_rc);
        }

        esp_event_handler_register(WIFI_EVENT, ESP_EVENT_ANY_ID, on_wifi_event, NULL);
        wifi_initialized = true;
    }

    esp_wifi_set_mode(WIFI_MODE_STA);

    wifi_config_t sta_cfg = {0};
    strncpy((char *) sta_cfg.sta.ssid, ssid, sizeof(sta_cfg.sta.ssid) - 1);
    if (psk != NULL && strlen(psk) > 0) {
        strncpy((char *) sta_cfg.sta.password, psk, sizeof(sta_cfg.sta.password) - 1);
    }
    esp_wifi_set_config(WIFI_IF_STA, &sta_cfg);

    esp_err_t err = esp_wifi_start();
    if (err != ESP_OK) {
        fprintf(stderr, "AtomVM wifi: esp_wifi_start failed: %d\n", err);
        return -1;
    }

    atomic_store_explicit(&wifi_enabled, true, memory_order_release);
    esp_wifi_connect();
    return 0;
}

int rtems_wifi_start_with_config_file(const char *conf_path)
{
    (void) conf_path;
    return -1;
}

int rtems_wifi_stop(void)
{
    if (atomic_load_explicit(&wifi_enabled, memory_order_acquire)) {
        esp_wifi_disconnect();
        esp_wifi_stop();
        atomic_store_explicit(&wifi_enabled, false, memory_order_release);
        set_state(RTEMS_WIFI_DISCONNECTED);
    }
    return 0;
}

void rtems_wifi_set_event_callback(rtems_wifi_event_cb cb, void *arg)
{
    event_cb = cb;
    event_cb_arg = arg;
}

void rtems_wifi_notify_dhcp_event(const char *interface, const char *reason,
                                 uint32_t ip, uint32_t netmask, uint32_t gateway)
{
    (void) interface;
    if (reason != NULL && strcmp(reason, "BOUND") == 0) {
        current_ip_info.ip = ip;
        current_ip_info.netmask = netmask;
        current_ip_info.gateway = gateway;
        set_state(RTEMS_WIFI_GOT_IP);
    } else if (reason != NULL && (strcmp(reason, "STOP") == 0 || strcmp(reason, "FAIL") == 0)) {
        current_ip_info.ip = 0;
        current_ip_info.netmask = 0;
        current_ip_info.gateway = 0;
        set_state(RTEMS_WIFI_DISCONNECTED);
    }
}

bool rtems_wifi_check_sd_config_and_start(void)
{
    return false;
}

#else /* !defined(RTEMS_HAS_LIBBSD) && !defined(RTEMS_WIFI_BACKEND_ESP32) */

int rtems_wifi_create_wlandev(void) { return -1; }
bool rtems_wifi_is_available(void) { return false; }
rtems_wifi_state_t rtems_wifi_get_state(void) { return RTEMS_WIFI_DISCONNECTED; }
int rtems_wifi_get_ip_info(struct rtems_wifi_ip_info *info) { (void) info; return -1; }
int rtems_wifi_start(const char *ssid, const char *psk) { (void) ssid; (void) psk; return -1; }
int rtems_wifi_start_with_config_file(const char *conf_path) { (void) conf_path; return -1; }
int rtems_wifi_stop(void) { return 0; }
void rtems_wifi_set_event_callback(rtems_wifi_event_cb cb, void *arg) { (void) cb; (void) arg; }
void rtems_wifi_notify_dhcp_event(const char *interface, const char *reason,
                                 uint32_t ip, uint32_t netmask, uint32_t gateway)
{
    (void) interface; (void) reason; (void) ip; (void) netmask; (void) gateway;
}
bool rtems_wifi_check_sd_config_and_start(void) { return false; }

#endif
