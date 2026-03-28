/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
 * Copyright 2020-2023 Fred Dushin <fred@dushin.net>
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

#include <sdkconfig.h>

#ifdef CONFIG_AVM_ENABLE_NETWORK_PORT_DRIVER

#include <atom.h>
#include <context.h>
#include <debug.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <memory.h>
#include <platform_defaultatoms.h>
#include <port.h>
#include <sdkconfig.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE 1
#include <trace.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <esp32_sys.h>
#include <esp_efuse.h>
#include <esp_efuse_table.h>
#include <esp_log.h>
#include <esp_mac.h>
#include <esp_netif.h>
#include <esp_sntp.h>
#include <esp_wifi.h>
#include <lwip/inet.h>
#pragma GCC diagnostic pop

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TCPIP_HOSTNAME_MAX_SIZE 255

// Reduce the maximum number of networks returned for devices with less ram to help mitigate OOM.
#if defined(CONFIG_IDF_TARGET_ESP32C2)
#define MAX_SCAN_RESULTS 10
#elif defined(CONFIG_IDF_TARGET_ESP32S2) || defined(CONFIG_IDF_TARGET_ESP32C61) || defined(CONFIG_IDF_TARGET_ESP32C5)
#define MAX_SCAN_RESULTS 14
#elif defined(CONFIG_IDF_TARGET_ESP32P4) || defined(CONFIG_SPIRAM_USE_MALLOC)
#define MAX_SCAN_RESULTS 64
#else
#define MAX_SCAN_RESULTS 20
#endif
#define DEFAULT_SCAN_RESULT_MAX 6
#define IDF_DEFAULT_ACTIVE_SCAN_TIME 120
#define IDF_DEFAULT_PASSIVE_SCAN_TIME 360
#define SSID_MAX_SIZE 33
#define BSSID_SIZE 6

#define TAG "network_driver"
#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)

static const char *const ap_atom = ATOM_STR("\x2", "ap");
static const char *const ap_channel_atom = ATOM_STR("\xA", "ap_channel");
static const char *const ap_sta_connected_atom = ATOM_STR("\x10", "ap_sta_connected");
static const char *const ap_sta_disconnected_atom = ATOM_STR("\x13", "ap_sta_disconnected");
static const char *const ap_sta_ip_assigned_atom = ATOM_STR("\x12", "ap_sta_ip_assigned");
static const char *const ap_started_atom = ATOM_STR("\xA", "ap_started");
static const char *const dhcp_hostname_atom = ATOM_STR("\xD", "dhcp_hostname");
static const char *const host_atom = ATOM_STR("\x4", "host");
static const char *const managed_atom = ATOM_STR("\x7", "managed");
static const char *const max_connections_atom = ATOM_STR("\xF", "max_connections");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const rssi_atom = ATOM_STR("\x4", "rssi");
static const char *const sntp_atom = ATOM_STR("\x4", "sntp");
static const char *const sntp_sync_atom = ATOM_STR("\x9", "sntp_sync");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const ssid_hidden_atom = ATOM_STR("\xB", "ssid_hidden");
static const char *const sta_atom = ATOM_STR("\x3", "sta");
static const char *const sta_connected_atom = ATOM_STR("\xD", "sta_connected");
static const char *const sta_beacon_timeout_atom = ATOM_STR("\x12", "sta_beacon_timeout");
static const char *const sta_disconnected_atom = ATOM_STR("\x10", "sta_disconnected");
static const char *const sta_got_ip_atom = ATOM_STR("\xA", "sta_got_ip");
static const char *const network_down_atom = ATOM_STR("\x0C", "network_down");

ESP_EVENT_DECLARE_BASE(sntp_event_base);
ESP_EVENT_DEFINE_BASE(sntp_event_base);

enum
{
    SNTP_EVENT_BASE_SYNC
};

enum network_cmd
{
    NetworkInvalidCmd = 0,
    NetworkStartCmd,
    NetworkRssiCmd,
    NetworkStopCmd,
    StaHaltCmd,
    StaConnectCmd,
    NetworkScanCmd,
    NetworkScanStopCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    { rssi_atom, NetworkRssiCmd },
    { ATOM_STR("\x4", "stop"), NetworkStopCmd },
    { ATOM_STR("\x8", "halt_sta"), StaHaltCmd },
    { ATOM_STR("\x7", "connect"), StaConnectCmd },
    { ATOM_STR("\x4", "scan"), NetworkScanCmd },
    { ATOM_STR("\xB", "cancel_scan"), NetworkScanStopCmd },
    SELECT_INT_DEFAULT(NetworkInvalidCmd)
};

struct ClientData
{
    GlobalContext *global;
    uint32_t port_process_id;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    bool managed;
};

struct ScanData
{
    uint64_t ref_ticks;
    uint32_t owner_process_id;
    uint16_t num_results;
    GlobalContext *global;
};

static void scan_done_handler(void *arg, esp_event_base_t event_base, int32_t event_id, void *event_data);

static inline term make_atom(GlobalContext *global, AtomString atom_str)
{
    return globalcontext_make_atom(global, atom_str);
}

static inline term authmode_to_atom_term(GlobalContext *global, wifi_auth_mode_t mode)
{
    term authmode = UNDEFINED_ATOM;
    switch (mode) {
        case WIFI_AUTH_OPEN:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x4", "open"));
            break;
        case WIFI_AUTH_WEP:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x3", "wep"));
            break;
        case WIFI_AUTH_WPA_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x7", "wpa_psk"));
            break;
        case WIFI_AUTH_WPA2_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x8", "wpa2_psk"));
            break;
        case WIFI_AUTH_WPA_WPA2_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xC", "wpa_wpa2_psk"));
            break;
        case WIFI_AUTH_ENTERPRISE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x3", "eap"));
            break;
        case WIFI_AUTH_WPA3_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x8", "wpa3_psk"));
            break;
        case WIFI_AUTH_WPA2_WPA3_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xD", "wpa2_wpa3_psk"));
            break;
        case WIFI_AUTH_WAPI_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x4", "wapi"));
            break;
        case WIFI_AUTH_WPA3_ENT_192:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x13", "wpa3_enterprise_192"));
            break;
        case WIFI_AUTH_OWE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x3", "owe"));
            break;
// NOTE: dummy_ atoms will not be observed in the wild and are not pre-populated to the atom table
#if (ESP_IDF_VERSION < ESP_IDF_VERSION_VAL(5, 4, 0))
        case WIFI_AUTH_DUMMY1:
            authmode = make_atom(global, ATOM_STR("\x6", "dummy1"));
            break;
        case WIFI_AUTH_DUMMY2:
            authmode = make_atom(global, ATOM_STR("\x6", "dummy2"));
            break;
#if (ESP_IDF_VERSION < ESP_IDF_VERSION_VAL(5, 3, 0))
        case WIFI_AUTH_DUMMY3:
            authmode = make_atom(global, ATOM_STR("\x6", "dummy3"));
            break;
#if (ESP_IDF_VERSION < ESP_IDF_VERSION_VAL(5, 2, 0))
        case WIFI_AUTH_DUMMY4:
            authmode = make_atom(global, ATOM_STR("\x6", "dummy4"));
            break;
        case WIFI_AUTH_DUMMY5:
            authmode = make_atom(global, ATOM_STR("\x6", "dummy5"));
            break;
#endif
#endif
#endif
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 2, 0))
        case WIFI_AUTH_WPA3_EXT_PSK:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xC", "wpa3_ext_psk"));
            break;
        case WIFI_AUTH_WPA3_EXT_PSK_MIXED_MODE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x12", "wpa3_ext_psk_mixed"));
            break;
#endif
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 3, 0))
        case WIFI_AUTH_DPP:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x3", "dpp"));
            break;
#endif
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 4, 0))
        case WIFI_AUTH_WPA3_ENTERPRISE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xf", "wpa3_enterprise"));
            break;
        case WIFI_AUTH_WPA2_WPA3_ENTERPRISE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x14", "wpa2_wpa3_enterprise"));
            break;
#endif
        case WIFI_AUTH_WPA_ENTERPRISE:
            authmode = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xE", "wpa_enterprise"));
            break;
        case WIFI_AUTH_MAX:
            authmode = ERROR_ATOM;
            break;
    }
    return authmode;
}

#define UNLIKELY_NOT_ESP_OK(E) UNLIKELY((E) != ESP_OK)

static term tuple_from_addr(Heap *heap, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int((addr >> 24) & 0xFF);
    terms[1] = term_from_int((addr >> 16) & 0xFF);
    terms[2] = term_from_int((addr >> 8) & 0xFF);
    terms[3] = term_from_int(addr & 0xFF);

    return port_heap_create_tuple_n(heap, 4, terms);
}

static bool ensure_scan_atoms_exist(GlobalContext *global)
{
    // Atoms needed in return message
    if (UNLIKELY(globalcontext_make_atom(global, ssid_atom) == term_invalid_term())) {
        return false;
    }
    if (UNLIKELY(globalcontext_make_atom(global, rssi_atom) == term_invalid_term())) {
        return false;
    }
    static const char *const authmode = ATOM_STR("\x8", "authmode");
    if (UNLIKELY(globalcontext_make_atom(global, authmode) == term_invalid_term())) {
        return false;
    }
    static const char *const bssid = ATOM_STR("\x5", "bssid");
    if (UNLIKELY(globalcontext_make_atom(global, bssid) == term_invalid_term())) {
        return false;
    }
    static const char *const channel = ATOM_STR("\x7", "channel");
    if (UNLIKELY(globalcontext_make_atom(global, channel) == term_invalid_term())) {
        return false;
    }
    static const char *const unregister_handler = ATOM_STR("\x12", "unregister_handler");
    if (UNLIKELY(globalcontext_make_atom(global, unregister_handler) == term_invalid_term())) {
        return false;
    }

    // Common auth atoms, not every auth atom is preloaded
    static const char *const open = ATOM_STR("\x4", "open");
    if (UNLIKELY(globalcontext_make_atom(global, open) == term_invalid_term())) {
        return false;
    }
    static const char *const wep = ATOM_STR("\x3", "wep");
    if (UNLIKELY(globalcontext_make_atom(global, wep) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa_psk = ATOM_STR("\x7", "wpa_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa2_psk = ATOM_STR("\x8", "wpa2_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa2_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa_wpa2_psk = ATOM_STR("\xC", "wpa_wpa2_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa_wpa2_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa3_psk = ATOM_STR("\x8", "wpa3_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa3_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const eap = ATOM_STR("\x3", "eap");
    if (UNLIKELY(globalcontext_make_atom(global, eap) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa2_wpa3_psk = ATOM_STR("\xD", "wpa2_wpa3_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa2_wpa3_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const wapi = ATOM_STR("\x4", "wapi");
    if (UNLIKELY(globalcontext_make_atom(global, wapi) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa3_enterprise_192 = ATOM_STR("\x13", "wpa3_enterprise_192");
    if (UNLIKELY(globalcontext_make_atom(global, wpa3_enterprise_192) == term_invalid_term())) {
        return false;
    }
    static const char *const owe = ATOM_STR("\x3", "owe");
    if (UNLIKELY(globalcontext_make_atom(global, owe) == term_invalid_term())) {
        return false;
    }
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 2, 0))
    static const char *const wpa3_ext_psk = ATOM_STR("\xC", "wpa3_ext_psk");
    if (UNLIKELY(globalcontext_make_atom(global, wpa3_ext_psk) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa3_ext_psk_mixed = ATOM_STR("\x12", "wpa3_ext_psk_mixed");
    if (UNLIKELY(globalcontext_make_atom(global, wpa3_ext_psk_mixed) == term_invalid_term())) {
        return false;
    }
#endif
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 3, 0))
    static const char *const dpp = ATOM_STR("\x3", "dpp");
    if (UNLIKELY(globalcontext_make_atom(global, dpp) == term_invalid_term())) {
        return false;
    }
#endif
#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 4, 0))
    static const char *const wpa3_enterprise = ATOM_STR("\xF", "wpa3_enterprise");
    if (UNLIKELY(globalcontext_make_atom(global, wpa3_enterprise) == term_invalid_term())) {
        return false;
    }
    static const char *const wpa2_wpa3_enterprise = ATOM_STR("\x14", "wpa2_wpa3_enterprise");
    if (UNLIKELY(globalcontext_make_atom(global, wpa2_wpa3_enterprise) == term_invalid_term())) {
        return false;
    }
#endif
    static const char *const wpa_enterprise = ATOM_STR("\xE", "wpa_enterprise");
    if (UNLIKELY(globalcontext_make_atom(global, wpa_enterprise) == term_invalid_term())) {
        return false;
    }
    return true;
}

static term wifi_ap_records_to_list_maybe_gc(GlobalContext *global, wifi_ap_record_t *ap_records, uint16_t num_results, Heap *heap)
{
    term ssid_atom_term = globalcontext_existing_term_from_atom_string(global, ssid_atom);
    term rssi_atom_term = globalcontext_existing_term_from_atom_string(global, rssi_atom);
    term authmode_atom_term = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x8", "authmode"));
    term bssid_atom_term = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x5", "bssid"));
    term channel_atom_term = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\x7", "channel"));

    term networks_data_list = term_nil();
    for (int i = num_results - 1; i >= 0; i--) {

        term ssid_term;
        size_t ssid_size = strlen((char *) ap_records[i].ssid);
        if (ssid_size > 0) {
            ssid_term = term_from_literal_binary(ap_records[i].ssid, ssid_size, heap, global);
        } else {
            static const char *hidden_ap = "(hidden)";
            ssid_term = term_from_const_binary((const uint8_t *) hidden_ap, strlen(hidden_ap), heap, global);
        }

        term rssi = term_from_int(ap_records[i].rssi);
        term authmode = authmode_to_atom_term(global, ap_records[i].authmode);
        term bssid_term = term_from_literal_binary(ap_records[i].bssid, BSSID_SIZE, heap, global);
        term channel = term_from_int((int32_t) ap_records[i].primary);

        term ap_data = term_alloc_map(5, heap);
        term_set_map_assoc(ap_data, 0, authmode_atom_term, authmode);
        term_set_map_assoc(ap_data, 1, bssid_atom_term, bssid_term);
        term_set_map_assoc(ap_data, 2, channel_atom_term, channel);
        term_set_map_assoc(ap_data, 3, rssi_atom_term, rssi);
        term_set_map_assoc(ap_data, 4, ssid_atom_term, ssid_term);

        networks_data_list = term_list_prepend(ap_data, networks_data_list, heap);
    }
    return networks_data_list;
}

static void send_term(Heap *heap, struct ClientData *data, term t)
{
    term ref = term_from_ref_ticks(data->ref_ticks, heap);
    term msg = term_alloc_tuple(2, heap);
    term_put_tuple_element(msg, 0, ref);
    term_put_tuple_element(msg, 1, t);

    // Pid ! {Ref, T}
    port_send_message_from_task(data->global, term_from_local_process_id(data->owner_process_id), msg);
}

static void send_got_ip(struct ClientData *data, esp_netif_ip_info_t *info)
{
    TRACE("Sending got_ip back to AtomVM\n");

    // {Ref, {sta_got_ip, {{192, 168, 1, 2}, {255, 255, 255, 0}, {192, 168, 1, 1}}}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
    {
        term ip = tuple_from_addr(&heap, ntohl(info->ip.addr));
        term netmask = tuple_from_addr(&heap, ntohl(info->netmask.addr));
        term gw = tuple_from_addr(&heap, ntohl(info->gw.addr));

        term ip_info = port_heap_create_tuple3(&heap, ip, netmask, gw);
        term reply = port_heap_create_tuple2(&heap, make_atom(data->global, sta_got_ip_atom), ip_info);
        send_term(&heap, data, reply);
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_sta_connected(struct ClientData *data)
{
    TRACE("Sending sta_connected back to AtomVM\n");

    // {Ref, sta_connected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, data, make_atom(data->global, sta_connected_atom));
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_sta_beacon_timeout(struct ClientData *data)
{
    TRACE("Sending sta_beacon_timeout back to AtomVM\n");

    // {Ref, sta_beacon_timeout}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, data, make_atom(data->global, sta_beacon_timeout_atom));
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_sta_disconnected(struct ClientData *data)
{
    TRACE("Sending sta_disconnected back to AtomVM\n");

    // {Ref, sta_disconnected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, data, make_atom(data->global, sta_disconnected_atom));
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_ap_started(struct ClientData *data)
{
    TRACE("Sending ap_start back to AtomVM\n");

    // {Ref, ap_started}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, data, make_atom(data->global, ap_started_atom));
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_atom_mac(struct ClientData *data, term atom, uint8_t *mac)
{
    // {Ref, {ap_connected | ap_disconnected, <<1,2,3,4,5,6>>}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(6), heap);
    {
        term mac_term = term_from_literal_binary(mac, 6, &heap, data->global);
        term reply = port_heap_create_tuple2(&heap, atom, mac_term);
        send_term(&heap, data, reply);
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_ap_sta_connected(struct ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_connected back to AtomVM\n");
    send_atom_mac(data, make_atom(data->global, ap_sta_connected_atom), mac);
}

static void send_ap_sta_disconnected(struct ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_disconnected back to AtomVM\n");
    send_atom_mac(data, make_atom(data->global, ap_sta_disconnected_atom), mac);
}

static void send_ap_sta_ip_assigned(struct ClientData *data, esp_ip4_addr_t *ip)
{
    TRACE("Sending ap_sta_ip_assigned back to AtomVM\n");

    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(4), heap);
    {
        term ip_term = tuple_from_addr(&heap, ntohl(ip->addr));
        term reply = port_heap_create_tuple2(&heap, make_atom(data->global, ap_sta_ip_assigned_atom), ip_term);
        send_term(&heap, data, reply);
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_sntp_sync(struct ClientData *data, struct timeval *tv)
{
    TRACE("Sending sntp_sync back to AtomVM\n");

    // {Ref, {sntp_sync, {TVSec, TVUsec}}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) * 2 + BOXED_INT64_SIZE * 2, heap);
    {
        term tv_tuple = port_heap_create_tuple2(&heap, term_make_maybe_boxed_int64(tv->tv_sec, &heap), term_make_maybe_boxed_int64(tv->tv_usec, &heap));
        term reply = port_heap_create_tuple2(&heap, make_atom(data->global, sntp_sync_atom), tv_tuple);
        send_term(&heap, data, reply);
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_scan_error_reason(Context *ctx, term pid, term ref, term reason)
{
    size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2);
    port_ensure_available(ctx, error_size);

    term scan_results_atom = globalcontext_existing_term_from_atom_string(ctx->global, ATOM_STR("\xC", "scan_results"));
    if (UNLIKELY(term_is_invalid_term(scan_results_atom))) {
        return;
    }
    term ret = port_create_tuple2(ctx, scan_results_atom, port_create_error_tuple(ctx, reason));
    port_send_reply(ctx, pid, ref, ret);
}

// heap must be allocated and free'd by the caller
static void send_scan_error_from_task(GlobalContext *global, uint32_t local_process_id, term reason, uint64_t ref_ticks, Heap heap)
{
    term ref = term_from_ref_ticks(ref_ticks, &heap);
    term pid = term_from_local_process_id(local_process_id);
    term error_tuple = port_heap_create_error_tuple(&heap, reason);
    term scan_results_atom = globalcontext_existing_term_from_atom_string(global, ATOM_STR("\xC", "scan_results"));
    term ret = port_heap_create_tuple2(&heap, scan_results_atom, error_tuple);
    term msg = port_heap_create_tuple2(&heap, ref, ret);
    port_send_message_from_task(global, pid, msg);
}

static void send_scan_results(struct ScanData *data)
{
    uint16_t discovered = 0;
    esp_err_t err = esp_wifi_scan_get_ap_num(&discovered);
    if (UNLIKELY(err != ESP_OK)) {
        // the ap_list must be cleared on failures to prevent a memory leak
        esp_wifi_clear_ap_list();
        ESP_LOGE(TAG, "Failed to obtain number of networks found, reason: %s", esp_err_to_name(err));
        const char *error = esp_err_to_name(err);
        size_t error_len = strlen(error);

        // Handler should always be unregistered befor sending a reply to avoid a race condition,
        // once the message is sent another scan can be initiated, removing the handler after
        // sending the reply could remove the new one.
        err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(error_len), heap);
        term reason = term_from_const_binary((const uint8_t *) error, (uint16_t) error_len, &heap, data->global);
        send_scan_error_from_task(data->global, data->owner_process_id, reason, data->ref_ticks, heap);
        END_WITH_STACK_HEAP(heap, data->global);

        if (UNLIKELY(err != ESP_OK)) {
            ESP_LOGE(TAG, "Failed to unregister event handler for reason %s, future scans may fail", esp_err_to_name(err));
        } else {
            // This cannot be free'd if unregister fails or the next scan_done event will try
            // to access the free'd data and cause a hard crash.
            free(data);
        }
        return;
    }
    ESP_LOGD(TAG, "Scan found %u networks.", discovered);

    uint16_t num_results = data->num_results;
    uint16_t return_results;
    if (discovered > num_results) {
        return_results = num_results;
    } else {
        return_results = discovered;
    }
    wifi_ap_record_t *ap_records = NULL;
    if (return_results > 0) {
        ap_records = (wifi_ap_record_t *) calloc((size_t) return_results, sizeof(wifi_ap_record_t));
        if (IS_NULL_PTR(ap_records)) {
            esp_wifi_clear_ap_list();
            free(ap_records);

            err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2), heap);
            send_scan_error_from_task(data->global, data->owner_process_id, OUT_OF_MEMORY_ATOM, data->ref_ticks, heap);
            END_WITH_STACK_HEAP(heap, data->global);

            if (UNLIKELY(err != ESP_OK)) {
                ESP_LOGE(TAG, "Failed to unregister event handler for reason %s, future scans may fail", esp_err_to_name(err));
            } else {
                free(data);
            }
            return;
        }
        err = esp_wifi_scan_get_ap_records(&return_results, ap_records);
    } else {
        esp_wifi_clear_ap_list();
        err = ESP_OK;
    }

    if (UNLIKELY(err != ESP_OK)) {
        esp_wifi_clear_ap_list();
        free(ap_records);
        ap_records = NULL;
        ESP_LOGE(TAG, "Failed to obtain scan results, reason: %s", esp_err_to_name(err));
        const char *error = esp_err_to_name(err);
        size_t error_len = strlen(error);

        err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(error_len), heap);
        term reason = term_from_const_binary((const uint8_t *) error, (uint16_t) error_len, &heap, data->global);
        send_scan_error_from_task(data->global, data->owner_process_id, reason, data->ref_ticks, heap);
        END_WITH_STACK_HEAP(heap, data->global);

        if (UNLIKELY(err != ESP_OK)) {
            ESP_LOGE(TAG, "Failed to unregister event handler for reason %s, future scans may fail", esp_err_to_name(err));
        } else {
            free(data);
        }
        return;
    }
    ESP_LOGD(TAG, "Scan found %i networks", num_results);

    // ap_data example: {scan_results, {NumberResults, [#{ssid => SSID, rssi => DBM, authmode => Mode, bssid => Bssid, chanel => ChNumber}]}}
    size_t ap_data_size = (TERM_MAP_SIZE(5) + TERM_BINARY_HEAP_SIZE(SSID_MAX_SIZE) + TERM_BINARY_HEAP_SIZE(BSSID_SIZE));
    size_t results_size = (PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + BOXED_INT_SIZE + LIST_SIZE(return_results, ap_data_size));
    ESP_LOGD(TAG, "Requesting size '%zu' on heap for scan results", results_size);

    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, results_size) != MEMORY_GC_OK)) {
        esp_wifi_clear_ap_list();
        free(ap_records);
        ap_records = NULL;
        err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2), err_heap);
        send_scan_error_from_task(data->global, data->owner_process_id, OUT_OF_MEMORY_ATOM, data->ref_ticks, err_heap);
        END_WITH_STACK_HEAP(err_heap, data->global);
        if (UNLIKELY(err != ESP_OK)) {
            ESP_LOGE(TAG, "Failed to unregister event handler for reason %s, future scans may fail", esp_err_to_name(err));
        } else {
            free(data);
        }
        return;
    }

    // Unregister the callback, but do not report any problems until after results have been sent.
    err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

    term networks_data_list = term_nil();
    if (return_results != 0) {
        networks_data_list = wifi_ap_records_to_list_maybe_gc(data->global, ap_records, return_results, &heap);
    }
    free(ap_records);
    ap_records = NULL;

    term scan_results = port_heap_create_tuple2(&heap, term_from_int(discovered), networks_data_list);

    term scan_results_atom = globalcontext_existing_term_from_atom_string(data->global, ATOM_STR("\xC", "scan_results"));
    term results_tuple = port_heap_create_tuple2(&heap, scan_results_atom, scan_results);

    term ref = term_from_ref_ticks(data->ref_ticks, &heap);
    term msg = port_heap_create_tuple2(&heap, ref, results_tuple);

    port_send_message_from_task(data->global, term_from_local_process_id(data->owner_process_id), msg);
    memory_destroy_heap_from_task(&heap, data->global);

    // Send this event unregister error after the good scan results. If this problem is observed, blocking scans
    // may need the error included along with the good results (since future scans may fail). This should be an
    // exremely unlikely scenario.
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to unregister event handler for reason %s, future scans may fail", esp_err_to_name(err));
        const char *error = esp_err_to_name(err);
        size_t error_len = strlen(error);
        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(error_len), err_heap);

        term reason = term_from_const_binary((const uint8_t *) error, (uint16_t) error_len, &err_heap, data->global);
        term unregister_handler_atom = globalcontext_existing_term_from_atom_string(data->global, ATOM_STR("\x12", "unregister_handler"));
        term reason_tuple = port_heap_create_tuple2(&err_heap, unregister_handler_atom, reason);
        term error_tuple = port_heap_create_error_tuple(&err_heap, reason_tuple);
        term payload = port_heap_create_tuple2(&err_heap, globalcontext_existing_term_from_atom_string(data->global, ATOM_STR("\xC", "scan_results")), error_tuple);

        term ref = term_from_ref_ticks(data->ref_ticks, &err_heap);
        term msg = port_heap_create_tuple2(&err_heap, ref, payload);
        term pid = term_from_local_process_id(data->owner_process_id);
        port_send_message_from_task(data->global, pid, msg);
        END_WITH_STACK_HEAP(err_heap, data->global);
    } else {
        free(data);
    }
}

//
// Event Handlers
//

static void event_handler(void *arg, esp_event_base_t event_base, int32_t event_id, void *event_data)
{
    // TODO: Change all of the logging in event_handler to debug level, or move it to the
    // called send_* functions. We should not do any io operations inside the callback handler,
    // but debug output seems a fair trade off.
    struct ClientData *data = (struct ClientData *) arg;
    if (event_base == WIFI_EVENT) {

        switch (event_id) {

            case WIFI_EVENT_STA_START: {
                // TODO: expose an erlang callback so applications can choose how to respond to this
                // event, i.e. start a periodic scan for known networks, or initiate a connection
                ESP_LOGI(TAG, "WIFI_EVENT_STA_START received.");
                if (!data->managed) {
                    esp_wifi_connect();
                }
                break;
            }

            case WIFI_EVENT_STA_CONNECTED: {
                ESP_LOGI(TAG, "WIFI_EVENT_STA_CONNECTED received.");
                send_sta_connected(data);
                break;
            }

            case WIFI_EVENT_STA_DISCONNECTED: {
                ESP_LOGI(TAG, "WIFI_EVENT_STA_DISCONNECTED received.");
                send_sta_disconnected(data);
                break;
            }

            case WIFI_EVENT_AP_STACONNECTED: {
                ESP_LOGI(TAG, "WIFI_EVENT_AP_STACONNECTED received.");
                wifi_event_ap_staconnected_t *event = (wifi_event_ap_staconnected_t *) event_data;
                send_ap_sta_connected(data, event->mac);
                break;
            }

            case WIFI_EVENT_AP_STADISCONNECTED: {
                ESP_LOGI(TAG, "WIFI_EVENT_AP_STADISCONNECTED received.");
                wifi_event_ap_stadisconnected_t *event = (wifi_event_ap_stadisconnected_t *) event_data;
                send_ap_sta_disconnected(data, event->mac);
                break;
            }

            case WIFI_EVENT_AP_START: {
                ESP_LOGI(TAG, "WIFI_EVENT_AP_START received.");
                send_ap_started(data);
                break;
            }

#if (ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 2, 0))
            case WIFI_EVENT_HOME_CHANNEL_CHANGE: {
                wifi_event_home_channel_change_t *chan_data = (wifi_event_home_channel_change_t *) event_data;
                ESP_LOGD(TAG, "WIFI_EVENT home channel changed from %u to %u.", chan_data->old_chan, chan_data->new_chan);
                break;
            }
#endif

            case WIFI_EVENT_STA_BEACON_TIMEOUT: {
                ESP_LOGI(TAG, "WIFI_EVENT_STA_BEACON_TIMEOUT received. Maybe poor signal, or network congestion?");
                send_sta_beacon_timeout(data);
                break;
            }

            case WIFI_EVENT_SCAN_DONE: {
                ESP_LOGD(TAG, "WiFI network scan complete.");
                // Assure this wont be optimized away (if logging is disabled or lower than debug), so that
                // the gen_server doesn't receive spurious "Unhandled wifi event: 1" messages. The real handler
                // (scan_done_handler) is registered and unregistered per request. We catch this here so that
                // we can subscribe to all wifi events in network_start, otherwise each event needs to be
                // subscribed and unsubscribed individually.
                asm("nop");
                break;
            }

            default:
                ESP_LOGI(TAG, "Unhandled wifi event: %" PRIi32 ".", event_id);
                break;
        }

    } else if (event_base == IP_EVENT) {

        switch (event_id) {

            case IP_EVENT_STA_GOT_IP: {
                ip_event_got_ip_t *event = (ip_event_got_ip_t *) event_data;
                ESP_LOGI(TAG, "IP_EVENT_STA_GOT_IP: %s", inet_ntoa(event->ip_info.ip));
                // we restart sntp here for faster time sync (especially evident on coldboots)
                // the sntp_restart is no-op if sntp isn't configured
                esp_sntp_restart();
                send_got_ip(data, (esp_netif_ip_info_t *) &event->ip_info.ip);
                break;
            }

            case IP_EVENT_AP_STAIPASSIGNED: {
                ip_event_ap_staipassigned_t *event = (ip_event_ap_staipassigned_t *) event_data;
                ESP_LOGI(TAG, "IP_EVENT_AP_STAIPASSIGNED: %s", inet_ntoa(event->ip));
                send_ap_sta_ip_assigned(data, (esp_ip4_addr_t *) &event->ip);
                break;
            }

            default:
                ESP_LOGI(TAG, "Unhandled ip event: %" PRIi32 ".", event_id);
                break;
        }
    } else if (event_base == sntp_event_base) {

        switch (event_id) {

            case SNTP_EVENT_BASE_SYNC: {
                ESP_LOGI(TAG, "SNTP_EVENT_BASE_SYNC received.");
                send_sntp_sync(data, (struct timeval *) event_data);
                break;
            }

            default:
                ESP_LOGI(TAG, "Unhandled sntp event: %" PRIi32 ".", event_id);
                break;
        }

    } else {
        ESP_LOGI(TAG, "Unhandled network event: %" PRIi32 ".", event_id);
    }
}

static void scan_done_handler(void *arg, esp_event_base_t event_base, int32_t event_id, void *event_data)
{
    UNUSED(event_data)
    struct ScanData *data = (struct ScanData *) arg;

    if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_SCAN_DONE) {
        ESP_LOGD(TAG, "Scan complete.");
        send_scan_results(data);
    }
}

//
// message processing
//

static wifi_config_t *get_sta_wifi_config(term sta_config, GlobalContext *global)
{
    if (term_is_invalid_term(sta_config)) {
        TRACE("No STA config\n");
        return NULL;
    }
    term ssid_term = interop_kv_get_value(sta_config, ssid_atom, global);
    term pass_term = interop_kv_get_value(sta_config, psk_atom, global);
    term managed_term = interop_kv_get_value_default(sta_config, managed_atom, FALSE_ATOM, global);

    bool roaming = false;
    if ((!term_is_invalid_term(managed_term)) && (managed_term != FALSE_ATOM)) {
        roaming = true;
    }

    //
    // Check parameters
    //
    if (term_is_invalid_term(ssid_term)) {
        if (roaming != true) {
            ESP_LOGE(TAG, "get_sta_wifi_config: Missing SSID");
        }
        return NULL;
    }
    int ok = 0;
    char *ssid = interop_term_to_string(ssid_term, &ok);
    if (!ok || IS_NULL_PTR(ssid)) {
        ESP_LOGE(TAG, "get_sta_wifi_config: Invalid SSID");
        return NULL;
    }

    char *psk = NULL;
    if (term_is_invalid_term(pass_term)) {
        ESP_LOGW(TAG, "Warning: Attempting to connect to open network");
    } else {
        psk = interop_term_to_string(pass_term, &ok);
        if (!ok) {
            free(ssid);
            ESP_LOGE(TAG, "get_sta_wifi_config: Invalid PSK");
            return NULL;
        }
    }

    //
    // Allocate wifi_config and check variable sizes
    //
    wifi_config_t *wifi_config = malloc(sizeof(wifi_config_t));
    if (IS_NULL_PTR(wifi_config)) {
        ESP_LOGE(TAG, "Failed to allocate wifi_config_t for sta");
        return NULL;
    }
    if (UNLIKELY(strlen(ssid) > sizeof(wifi_config->sta.ssid))) {
        ESP_LOGE(TAG, "ssid cannot be more than %d characters", sizeof(wifi_config->sta.ssid));
        free(ssid);
        free(psk);
        return NULL;
    }
    if (!IS_NULL_PTR(psk) && UNLIKELY(strlen(psk) > sizeof(wifi_config->sta.password))) {
        ESP_LOGE(TAG, "psk cannot be more than %d characters", sizeof(wifi_config->sta.password));
        free(ssid);
        free(psk);
        return NULL;
    }

    //
    // Initialize the wifi_config structure
    //
    memset(wifi_config, 0, sizeof(wifi_config_t));
    strcpy((char *) wifi_config->sta.ssid, ssid);
    if (!IS_NULL_PTR(psk)) {
        strcpy((char *) wifi_config->sta.password, psk);
        free(psk);
    }
    free(ssid);

    //
    // done
    //
    ESP_LOGI(TAG, "STA ssid: %s", wifi_config->sta.ssid);
    return wifi_config;
}

static char *get_default_device_name(esp_mac_type_t interface)
{
    uint8_t mac[6];
    esp_read_mac(mac, interface);

    size_t buf_size = strlen("atomvm-") + 12 + 1;
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        ESP_LOGE(TAG, "Failed to allocate device name buf");
        return NULL;
    }
    snprintf(buf, buf_size,
        "atomvm-%02x%02x%02x%02x%02x%02x", mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    return buf;
}

static wifi_config_t *get_ap_wifi_config(term ap_config, GlobalContext *global)
{
    if (term_is_invalid_term(ap_config)) {
        TRACE("No AP config\n");
        return NULL;
    }
    term ssid_term = interop_kv_get_value(ap_config, ssid_atom, global);
    term pass_term = interop_kv_get_value(ap_config, psk_atom, global);
    term channel_term = interop_kv_get_value(ap_config, ap_channel_atom, global);

    //
    // Check parameters
    //
    char *ssid = NULL;
    if (term_is_invalid_term(ssid_term)) {
        ssid = get_default_device_name(ESP_MAC_WIFI_SOFTAP);
    } else {
        int ok = 0;
        ssid = interop_term_to_string(ssid_term, &ok);
        if (!ok || IS_NULL_PTR(ssid)) {
            ESP_LOGE(TAG, "get_ap_wifi_config: Invalid SSID");
            return NULL;
        }
    }
    char *psk = NULL;
    if (term_is_invalid_term(pass_term)) {
        ESP_LOGW(TAG, "Warning: Empty password.  AP will be open network!");
    } else {
        int ok = 0;
        psk = interop_term_to_string(pass_term, &ok);
        if (strlen(psk) < 8) {
            free(ssid);
            ESP_LOGE(TAG, "get_ap_wifi_config: AP PSK must be length 8 or more");
            return NULL;
        }
        if (!ok || IS_NULL_PTR(psk)) {
            free(ssid);
            ESP_LOGE(TAG, "get_ap_wifi_config: Invalid PSK");
            return NULL;
        }
    }
    uint8_t channel = 0;
    if (!term_is_invalid_term(channel_term)) {
        channel = term_to_uint8(channel_term);
    }

    //
    // Allocate wifi_config and check variable sizes
    //
    wifi_config_t *wifi_config = malloc(sizeof(wifi_config_t));
    if (IS_NULL_PTR(wifi_config)) {
        ESP_LOGE(TAG, "Failed to allocate wifi_config_t for ap");
        return NULL;
    }
    if (UNLIKELY(strlen(ssid) > sizeof(wifi_config->ap.ssid))) {
        ESP_LOGE(TAG, "ssid cannot be more than %d characters", sizeof(wifi_config->ap.ssid));
        free(ssid);
        free(psk);
        return NULL;
    }
    if (!IS_NULL_PTR(psk) && UNLIKELY(strlen(psk) > sizeof(wifi_config->ap.password))) {
        ESP_LOGE(TAG, "psk cannot be more than %d characters", sizeof(wifi_config->ap.password));
        free(ssid);
        free(psk);
        return NULL;
    }

    //
    // Initialize the wifi_config structure
    //
    memset(wifi_config, 0, sizeof(wifi_config_t));
    strcpy((char *) wifi_config->ap.ssid, ssid);
    free(ssid);
    if (!IS_NULL_PTR(psk)) {
        strcpy((char *) wifi_config->ap.password, psk);
        free(psk);
    }
    if (channel != 0) {
        wifi_config->ap.channel = channel;
    }

    wifi_config->ap.authmode = IS_NULL_PTR(psk) ? WIFI_AUTH_OPEN : WIFI_AUTH_WPA_WPA2_PSK;
    term ssid_hidden_term = interop_kv_get_value(ap_config, ssid_hidden_atom, global);
    wifi_config->ap.ssid_hidden = term_is_invalid_term(ssid_hidden_term) ? 0 : ssid_hidden_term == TRUE_ATOM;
    term max_connections_term = interop_kv_get_value(ap_config, max_connections_atom, global);
    wifi_config->ap.max_connection = term_is_invalid_term(max_connections_term) ? 4 : term_to_int(max_connections_term);

    ESP_LOGI(TAG, "AP ssid: %s", wifi_config->ap.ssid);
    ESP_LOGI(TAG, "AP authmode: %d", wifi_config->ap.authmode);
    ESP_LOGI(TAG, "AP ssid_hidden: %d", wifi_config->ap.ssid_hidden);
    ESP_LOGI(TAG, "AP max_connection: %d", wifi_config->ap.max_connection);

    return wifi_config;
}

static void time_sync_notification_cb(struct timeval *tv)
{
    esp_err_t err = esp_event_post(sntp_event_base, SNTP_EVENT_BASE_SYNC, tv, sizeof(tv), portMAX_DELAY);
    if (err == ESP_OK) {
        ESP_LOGI(TAG, "Posting SNTP synchronization event");
    } else {
        ESP_LOGI(TAG, "Unable to post SNTP synchronization event, err=%d", err);
    }
}

static void maybe_set_sntp(term sntp_config, GlobalContext *global)
{
    if (!term_is_invalid_term(sntp_config) && !term_is_invalid_term(interop_kv_get_value(sntp_config, host_atom, global))) {
        int ok;
        char *host = interop_term_to_string(interop_kv_get_value(sntp_config, host_atom, global), &ok);
        if (LIKELY(ok)) {
            // do not free(sntp)
            if (esp_sntp_enabled()) {
                esp_sntp_stop();
            }
            esp_sntp_setoperatingmode(SNTP_OPMODE_POLL);
            esp_sntp_setservername(0, host);
            sntp_set_time_sync_notification_cb(time_sync_notification_cb);
            esp_sntp_init();
            ESP_LOGI(TAG, "SNTP initialized with host set to %s", host);
        } else {
            ESP_LOGE(TAG, "Unable to locate sntp host in configuration");
        }
    }
}

static void set_dhcp_hostname(esp_netif_t *interface, const char *interface_name, term dhcp_hostname_term)
{
    char dhcp_hostname[TCPIP_HOSTNAME_MAX_SIZE + 1];
    if (!term_is_invalid_term(dhcp_hostname_term)) {
        int ok = 0;
        char *tmp = interop_term_to_string(dhcp_hostname_term, &ok);
        if (!ok || IS_NULL_PTR(tmp)) {
            ESP_LOGE(TAG, "WARNING: dhcp_hostname is not a valid string value");
            return;
        } else {
            strncpy(dhcp_hostname, tmp, TCPIP_HOSTNAME_MAX_SIZE);
            dhcp_hostname[TCPIP_HOSTNAME_MAX_SIZE] = '\0';
            free(tmp);
        }
    } else {
        char *tmp = get_default_device_name(ESP_MAC_WIFI_STA);
        strncpy(dhcp_hostname, tmp, TCPIP_HOSTNAME_MAX_SIZE);
        free(tmp);
    }
    esp_err_t status = esp_netif_set_hostname(interface, dhcp_hostname);
    if (status == ESP_OK) {
        ESP_LOGI(TAG, "%s DHCP hostname set to %s", interface_name, dhcp_hostname);
    } else {
        ESP_LOGW(TAG, "Unable to set %s DHCP hostname to %s.  status=%d", interface_name, dhcp_hostname, status);
    }
}

static void start_network(Context *ctx, term pid, term ref, term config)
{
    TRACE("start_network\n");

    // {Ref, ok | {error, atom() | integer()}}
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate heap space for start_network; no message sent");
        return;
    }

    //
    // Get the STA and AP config, if set
    //
    term sta_config = interop_kv_get_value_default(config, sta_atom, term_invalid_term(), ctx->global);
    term ap_config = interop_kv_get_value_default(config, ap_atom, term_invalid_term(), ctx->global);
    if (UNLIKELY(term_is_invalid_term(sta_config) && term_is_invalid_term(ap_config))) {
        ESP_LOGE(TAG, "Expected STA or AP configuration but got neither");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    bool roaming = false;
    term managed = interop_kv_get_value_default(sta_config, managed_atom, FALSE_ATOM, ctx->global);
    if ((!term_is_invalid_term(managed)) && (managed != FALSE_ATOM)) {
        roaming = true;
    }

    wifi_config_t *sta_wifi_config = get_sta_wifi_config(sta_config, ctx->global);
    wifi_config_t *ap_wifi_config = get_ap_wifi_config(ap_config, ctx->global);
    if ((!roaming) && IS_NULL_PTR(sta_wifi_config) && IS_NULL_PTR(ap_wifi_config)) {
        ESP_LOGE(TAG, "Unable to get STA or AP configuration");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }

    struct ClientData *data = malloc(sizeof(struct ClientData));
    if (IS_NULL_PTR(data)) {
        ESP_LOGE(TAG, "Failed to allocate ClientData");
        term error = port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }
    data->global = ctx->global;
    data->port_process_id = ctx->process_id;
    data->owner_process_id = term_to_local_process_id(pid);
    data->ref_ticks = term_to_ref_ticks(ref);
    data->managed = roaming;

    esp_netif_t *sta_wifi_interface = NULL;
    if ((sta_wifi_config != NULL) || (roaming)) {
        sta_wifi_interface = esp_netif_create_default_wifi_sta();
        if (IS_NULL_PTR(sta_wifi_interface)) {
            ESP_LOGE(TAG, "Failed to create network STA interface");
            term error = port_create_error_tuple(ctx, ERROR_ATOM);
            port_send_reply(ctx, pid, ref, error);
            goto cleanup;
        }
    }
    esp_netif_t *ap_wifi_interface = NULL;
    if (ap_wifi_config != NULL) {
        ap_wifi_interface = esp_netif_create_default_wifi_ap();
        if (IS_NULL_PTR(ap_wifi_interface)) {
            ESP_LOGE(TAG, "Failed to create network AP interface");
            term error = port_create_error_tuple(ctx, ERROR_ATOM);
            port_send_reply(ctx, pid, ref, error);
            goto cleanup;
        }
    }

    esp_err_t err;
    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    if (UNLIKELY_NOT_ESP_OK(err = esp_wifi_init(&cfg))) {
        ESP_LOGE(TAG, "Failed to initialize ESP WiFi, reason: %s", esp_err_to_name(err));
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }
    if (UNLIKELY((err = esp_wifi_set_storage(WIFI_STORAGE_FLASH)) != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to set ESP WiFi storage, reason: %s", esp_err_to_name(err));
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }

    if ((err = esp_event_handler_register(WIFI_EVENT, ESP_EVENT_ANY_ID, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register wifi event handler, reason: %s", esp_err_to_name(err));
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }

    if ((err = esp_event_handler_register(IP_EVENT, IP_EVENT_STA_GOT_IP, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register got_ip event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }
    if ((err = esp_event_handler_register(IP_EVENT, IP_EVENT_AP_STAIPASSIGNED, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register staipassigned event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }
    if ((err = esp_event_handler_register(sntp_event_base, SNTP_EVENT_BASE_SYNC, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register sntp event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    }

    //
    // Set the wifi mode
    //
    wifi_mode_t wifi_mode = WIFI_MODE_NULL;
    if ((!IS_NULL_PTR(sta_wifi_config) || (roaming)) && !IS_NULL_PTR(ap_wifi_config)) {
        wifi_mode = WIFI_MODE_APSTA;
    } else if (!IS_NULL_PTR(ap_wifi_config)) {
        wifi_mode = WIFI_MODE_AP;
    } else {
        wifi_mode = WIFI_MODE_STA;
    }

    if ((err = esp_wifi_set_mode(wifi_mode)) != ESP_OK) {
        ESP_LOGE(TAG, "Error setting wifi mode %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    } else {
        ESP_LOGI(TAG, "WIFI mode set to %d", wifi_mode);
    }

    //
    // Set up STA mode, if configured
    //
    if (!IS_NULL_PTR(sta_wifi_config)) {
        if ((err = esp_wifi_set_config(WIFI_IF_STA, sta_wifi_config)) != ESP_OK) {
            ESP_LOGE(TAG, "Error setting STA mode config %d", err);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
            goto cleanup;
        } else {
            ESP_LOGI(TAG, "STA mode configured");
        }
    }

    //
    // Set up AP mode, if configured
    //
    if (!IS_NULL_PTR(ap_wifi_config)) {
        if ((err = esp_wifi_set_config(WIFI_IF_AP, ap_wifi_config)) != ESP_OK) {
            ESP_LOGE(TAG, "Error setting AP mode config %d", err);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
            goto cleanup;
        } else {
            ESP_LOGI(TAG, "AP mode configured");
        }
    }

    //
    // Start the configured interface(s)
    //
    if ((err = esp_wifi_start()) != ESP_OK) {
        ESP_LOGE(TAG, "Error in esp_wifi_start %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        goto cleanup;
    } else {
        ESP_LOGI(TAG, "WIFI started");
    }

    //
    // Set up simple NTP, if configured
    //
    maybe_set_sntp(interop_kv_get_value(config, sntp_atom, ctx->global), ctx->global);

    //
    // Set the DHCP hostname, if STA mode is enabled
    //
    if (!IS_NULL_PTR(sta_wifi_config)) {
        set_dhcp_hostname(sta_wifi_interface, "STA", interop_kv_get_value(sta_config, dhcp_hostname_atom, ctx->global));
    }
    if (!IS_NULL_PTR(ap_wifi_config)) {
        set_dhcp_hostname(ap_wifi_interface, "AP", interop_kv_get_value(ap_config, dhcp_hostname_atom, ctx->global));
    }

    //
    // Done -- send an ok so the FSM can proceed
    //
    port_send_reply(ctx, pid, ref, OK_ATOM);
    goto cleanup;

cleanup:
    free(sta_wifi_config);
    free(ap_wifi_config);
    return;
}

static void stop_network(Context *ctx)
{
    // Stop sntp (ignore OK, or not configured error)
    esp_sntp_stop();

    // Stop unregister event callbacks so they dont trigger during shutdown.
    esp_event_handler_unregister(WIFI_EVENT, ESP_EVENT_ANY_ID, &event_handler);
    esp_event_handler_unregister(IP_EVENT, IP_EVENT_STA_GOT_IP, &event_handler);
    esp_event_handler_unregister(IP_EVENT, IP_EVENT_AP_STAIPASSIGNED, &event_handler);
    esp_event_handler_unregister(sntp_event_base, SNTP_EVENT_BASE_SYNC, &event_handler);
    esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);

    esp_netif_t *sta_wifi_interface = esp_netif_get_handle_from_ifkey("WIFI_STA_DEF");
    esp_netif_t *ap_wifi_interface = esp_netif_get_handle_from_ifkey("WIFI_AP_DEF");

    // Disconnect STA if connected to access point
    if ((sta_wifi_interface != NULL) && (esp_netif_is_netif_up(sta_wifi_interface))) {
        esp_err_t err = esp_wifi_disconnect();
        if (UNLIKELY(err == ESP_FAIL)) {
            ESP_LOGE(TAG, "ESP FAIL error while disconnecting from AP, continuing network shutdown...");
        }
    }

    // Stop and deinit the WiFi driver, these only return OK, or not init error (fine to ignore).
    esp_wifi_stop();
    esp_wifi_deinit();

    // Destroy existing netif interfaces
    if (ap_wifi_interface != NULL) {
        esp_netif_destroy_default_wifi(ap_wifi_interface);
    }
    if (sta_wifi_interface != NULL) {
        esp_netif_destroy_default_wifi(sta_wifi_interface);
    }
}

static void get_sta_rssi(Context *ctx, term pid, term ref)
{
    size_t tuple_reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);

    int sta_rssi = 0;
    wifi_ap_record_t ap_info;
    esp_err_t status = esp_wifi_sta_get_ap_info(&ap_info);
    if (status == ESP_OK) {
        esp_err_t err = esp_wifi_sta_get_rssi(&sta_rssi);
        if (UNLIKELY(err != ESP_OK)) {
            term error_term = term_from_int(err);
            ESP_LOGE(TAG, "error obtaining RSSI: [%i] %u", err, error_term);
            // Reply: {Ref, {error, Reason}}
            port_ensure_available(ctx, tuple_reply_size);
            term error = port_create_error_tuple(ctx, error_term);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    } else {
        ESP_LOGE(TAG, "Device is not connected to any AP.");
        // Reply: {Ref, {error, network_down}}
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, make_atom(ctx->global, network_down_atom));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    term rssi = term_from_int(sta_rssi);
    // {Ref, {rssi, -25}}
    port_ensure_available(ctx, tuple_reply_size);
    term reply = port_create_tuple2(ctx, make_atom(ctx->global, rssi_atom), rssi);
    port_send_reply(ctx, pid, ref, reply);
}

static void sta_disconnect(Context *ctx, term pid, term ref)
{
    esp_err_t err = esp_wifi_disconnect();
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "Error while disconnecting from AP (%i)", err);
        port_ensure_available(ctx, PORT_REPLY_SIZE + TUPLE_SIZE(2));
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate heap space for sta_disconnect; no message sent");
        return;
    }

    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void sta_connect(Context *ctx, term pid, term ref, term config)
{
    size_t tuple_reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);

    //
    // Check wifi mode
    //
    wifi_mode_t mode;
    esp_err_t err = esp_wifi_get_mode(&mode);
    if ((err != ESP_OK) || ((mode != WIFI_MODE_STA) && (mode != WIFI_MODE_APSTA))) {
        ESP_LOGE(TAG, "sta_connect: WiFi mode must be started in either STA mode or APSTA mode to use this function");
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, ERROR_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    //
    // Get the STA config
    //
    term sta_config = interop_kv_get_value_default(config, sta_atom, term_invalid_term(), ctx->global);
    if (UNLIKELY(term_is_invalid_term(sta_config))) {
        // Also accept a proplist containing `ssid` and `psk` key/value tuples.
        if ((interop_kv_get_value_default(config, ssid_atom, term_invalid_term(), ctx->global)) != (term_invalid_term())) {
            sta_config = config;
        } else {
            ESP_LOGE(TAG, "Expected STA configuration but got none");
            port_ensure_available(ctx, tuple_reply_size);
            term error = port_create_error_tuple(ctx, BADARG_ATOM);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    }

    wifi_config_t *sta_wifi_config = get_sta_wifi_config(sta_config, ctx->global);
    if (IS_NULL_PTR(sta_wifi_config)) {
        ESP_LOGE(TAG, "Unable to get STA configuration");
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    //
    // Set up STA mode
    //
    if ((err = esp_wifi_set_config(ESP_IF_WIFI_STA, sta_wifi_config)) != ESP_OK) {
        ESP_LOGE(TAG, "Error setting STA mode config %d", err);
        free(sta_wifi_config);
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    } else {
        ESP_LOGD(TAG, "STA mode configured");
        free(sta_wifi_config);
    }

    //
    // Set the DHCP hostname
    //
    esp_netif_t *sta_interface = esp_netif_get_handle_from_ifkey("WIFI_STA_DEF");
    set_dhcp_hostname(sta_interface, "STA", interop_kv_get_value(sta_config, dhcp_hostname_atom, ctx->global));

    if ((err = esp_wifi_connect()) != ESP_OK) {
        ESP_LOGE(TAG, "Error while connecting: %d", err);
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    } else {
        ESP_LOGI(TAG, "WiFi connection started.");
    }

    //
    // Set up simple NTP, if configured
    //
    maybe_set_sntp(interop_kv_get_value(config, sntp_atom, ctx->global), ctx->global);

    if (UNLIKELY(memory_ensure_free(ctx, tuple_reply_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate heap space for sta_connect/1; no message sent");
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void sta_reconnect(Context *ctx, term pid, term ref)
{
    size_t tuple_reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);

    esp_err_t err = esp_wifi_connect();
    if (err != ESP_OK) {
        ESP_LOGE(TAG, "Error while connecting: %d", err);
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    } else {
        ESP_LOGI(TAG, "WiFi connection started.");
    }

    if (UNLIKELY(memory_ensure_free(ctx, tuple_reply_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate heap space for sta_connect/0; no message sent");
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void wifi_scan(Context *ctx, term pid, term ref, term config)
{
    static const char *const scan_results = ATOM_STR("\xC", "scan_results");
    if (UNLIKELY(globalcontext_make_atom(ctx->global, scan_results) == term_invalid_term())) {
        size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2);
        port_ensure_available(ctx, error_size);

        term ret = port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, ret);
        return;
    }

    wifi_mode_t mode;
    esp_err_t err = esp_wifi_get_mode(&mode);
    if ((err != ESP_OK) || ((mode != WIFI_MODE_STA) && (mode != WIFI_MODE_APSTA))) {
        ESP_LOGE(TAG, "WiFi must already be configured in STA or AP+STA mode to use network:wifi_scan/0,1");
        term reason = make_atom(ctx->global, ATOM_STR("\x10", "unsupported_mode"));
        send_scan_error_reason(ctx, pid, ref, reason);
        return;
    }

    term cfg_results = interop_kv_get_value_default(config, ATOM_STR("\x7", "results"), term_from_int(DEFAULT_SCAN_RESULT_MAX), ctx->global);
    if (UNLIKELY(!term_is_integer(cfg_results))) {
        ESP_LOGE(TAG, "results option must be an integer (i.e. {results, 6})");
        send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
        return;
    }

    int32_t requested_results = term_to_int(cfg_results);
    if (UNLIKELY((requested_results < 1) || (requested_results > MAX_SCAN_RESULTS))) {
        ESP_LOGE(TAG, "results option must be between 1 and %i on this platform.", MAX_SCAN_RESULTS);
        send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
        return;
    }
    uint16_t num_results = (uint16_t) requested_results;
    ESP_LOGD(TAG, "Scan will return a maximum of %u results", num_results);

    term term_passive = interop_kv_get_value_default(config, ATOM_STR("\x7", "passive"), term_invalid_term(), ctx->global);
    bool active_scan = true;
    if (!term_is_invalid_term(term_passive)) {
        if ((term_passive == TRUE_ATOM) || (term_passive == make_atom(ctx->global, ATOM_STR("\x7", "passive")))) {
            active_scan = false;
        } else if (term_passive != FALSE_ATOM) {
            send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
            return;
        }
    }

    term cfg_dwell = interop_kv_get_value_default(config, ATOM_STR("\x5", "dwell"), term_invalid_term(), ctx->global);
    uint32_t dwell_ms = 0;
    if (cfg_dwell == term_invalid_term()) {
        if (active_scan == true) {
            dwell_ms = IDF_DEFAULT_ACTIVE_SCAN_TIME;
        } else {
            dwell_ms = IDF_DEFAULT_PASSIVE_SCAN_TIME;
        }
    } else {
        if (UNLIKELY(!term_is_integer(cfg_dwell))) {
            ESP_LOGE(TAG, "Channel dwell time milliseconds must be an integer (i.e. {dwell, 250})");
            send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
            return;
        }
        dwell_ms = (uint32_t) term_to_int(cfg_dwell);
        if (UNLIKELY((dwell_ms < 1lu) || (dwell_ms > 1500lu))) {
            ESP_LOGE(TAG, "Per channel dwell time milliseconds must be {dwell, 1..1500}");
            send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
            return;
        } else {
            ESP_LOGD(TAG, "Scan will spend %lu ms per channel", dwell_ms);
        }
    }

    term term_hidden = interop_kv_get_value_default(config, ATOM_STR("\xB", "show_hidden"), term_invalid_term(), ctx->global);
    bool show_hidden = false;
    if (!term_is_invalid_term(term_hidden)) {
        if ((term_hidden == TRUE_ATOM) || (term_hidden == make_atom(ctx->global, ATOM_STR("\xB", "show_hidden")))) {
            show_hidden = true;
        } else if (term_hidden != FALSE_ATOM) {
            send_scan_error_reason(ctx, pid, ref, BADARG_ATOM);
            return;
        }
    }

    wifi_scan_type_t scan_type = WIFI_SCAN_TYPE_ACTIVE;
    switch (active_scan) {
        case false:
            scan_type = WIFI_SCAN_TYPE_PASSIVE;
            break;
        case true:
            break;
    }

    wifi_scan_config_t *scan_config = (wifi_scan_config_t *) calloc(1, sizeof(wifi_scan_config_t));
    if (IS_NULL_PTR(scan_config)) {
        ESP_LOGE(TAG, "Unable to allocate memory for configuration");
        send_scan_error_reason(ctx, pid, ref, OUT_OF_MEMORY_ATOM);
        return;
    }

    if (scan_type == WIFI_SCAN_TYPE_ACTIVE) {
        scan_config->scan_time.active.max = dwell_ms;
        // For fast scans use the same min time as max (like ESP-IDF default), but for longer
        // per-channel dwell times set the min scan time to 1/2 of the maximum, but never less
        // than the 120ms min used in the default scan.
        if (dwell_ms > IDF_DEFAULT_ACTIVE_SCAN_TIME * 2) {
            scan_config->scan_time.active.min = (dwell_ms / 2);
        } else {
            scan_config->scan_time.active.min = dwell_ms;
        }
    } else {
        scan_config->scan_time.passive = dwell_ms;
        if (dwell_ms > 1000) {
            // Increase home channel dwell between scanning consecutive channel from 30 to 60ms to prevent beacon timeouts
            scan_config->home_chan_dwell_time = 60;
        }
    }

    scan_config->show_hidden = show_hidden;
    scan_config->scan_type = scan_type;

    if (UNLIKELY(!ensure_scan_atoms_exist(ctx->global))) {
        ESP_LOGE(TAG, "Unable to allocate atoms for scan return, atom table exhausted or OOM!");
        send_scan_error_reason(ctx, pid, ref, OUT_OF_MEMORY_ATOM);
        free(scan_config);
        return;
    }

    struct ScanData *data = malloc(sizeof(struct ScanData));
    if (IS_NULL_PTR(data)) {
        ESP_LOGE(TAG, "Failed to allocate ClientData");
        send_scan_error_reason(ctx, pid, ref, OUT_OF_MEMORY_ATOM);
        free(scan_config);
        return;
    }
    data->global = ctx->global;
    data->owner_process_id = term_to_local_process_id(pid);
    data->ref_ticks = term_to_ref_ticks(ref);
    data->num_results = num_results;

    if ((err = esp_event_handler_register(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register wifi event handler");
        send_scan_error_reason(ctx, pid, ref, term_from_int(err));
        free(data);
        free(scan_config);
        return;
    }

    err = esp_wifi_scan_start(scan_config, false);
    free(scan_config);
    scan_config = NULL;
    if (UNLIKELY(err != ESP_OK)) {
        const char *err_str = esp_err_to_name(err);
        size_t error_len = strlen(err_str);
        size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(error_len);
        port_ensure_available(ctx, error_size);
        term reason = term_from_literal_binary((const uint8_t *) err_str, (uint16_t) error_len, &ctx->heap, ctx->global);
        term scan_results_atom = globalcontext_existing_term_from_atom_string(ctx->global, ATOM_STR("\xC", "scan_results"));
        term ret = port_create_tuple2(ctx, scan_results_atom, port_create_error_tuple(ctx, reason));
        err = esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, &scan_done_handler);
        port_send_reply(ctx, pid, ref, ret);
        if (UNLIKELY(err != ESP_OK)) {
            ESP_LOGE(TAG, "Failed to unregister event handler, future scans may fail");
        } else {
            free(data);
        }
    }
    return;
}

static void cancel_scan(Context *ctx, term pid, term ref, term reply_to)
{
    term scan_canceled_atom = make_atom(ctx->global, ATOM_STR("\xD", "scan_canceled"));
    if (UNLIKELY(scan_canceled_atom == term_invalid_term())) {
        size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2);
        port_ensure_available(ctx, error_size);

        term ret = port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, ret);
        return;
    }

    esp_err_t err = esp_wifi_scan_stop();
    if (UNLIKELY(err == ESP_ERR_WIFI_STATE)) {
        ESP_LOGE(TAG, "Unable to stop wifi scan, wifi is negotiating a connection to an access point");

        size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(3) + TUPLE_SIZE(2);
        port_ensure_available(ctx, error_size);

        term reason = make_atom(ctx->global, ATOM_STR("\xE", "sta_connecting"));
        term error_tuple = port_create_error_tuple(ctx, reason);
        term ret = port_create_tuple3(ctx, scan_canceled_atom, reply_to, error_tuple);
        port_send_reply(ctx, pid, ref, ret);
        return;

    } else if (UNLIKELY(err != ESP_OK)) {
        const char *err_str = esp_err_to_name(err);
        size_t error_len = strlen(err_str);
        size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(3) + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(error_len);
        port_ensure_available(ctx, error_size);

        term reason = term_from_const_binary((const uint8_t *) err_str, (uint16_t) error_len, &ctx->heap, ctx->global);
        term error_tuple = port_create_error_tuple(ctx, reason);
        term ret = port_create_tuple3(ctx, scan_canceled_atom, reply_to, error_tuple);
        port_send_reply(ctx, pid, ref, ret);
        return;
    }

    size_t reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(3);
    port_ensure_available(ctx, reply_size);
    term reply = port_create_tuple3(ctx, scan_canceled_atom, reply_to, OK_ATOM);
    port_send_reply(ctx, pid, ref, reply);
}

static NativeHandlerResult consume_mailbox(Context *ctx)
{
    bool cmd_terminate = false;
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

    if (UNLIKELY(!term_is_tuple(msg) || term_get_tuple_arity(msg) != 3)) {
        ESP_LOGW(TAG, "Invalid message.  Ignoring.");
        return NativeContinue;
    }

    // TODO: port this code to standard port (and gen_message)
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term cmd_term = term_invalid_term();
    term config = term_invalid_term();

    if ((term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) || term_is_atom(cmd)) {

        if (term_is_atom(cmd)) {
            cmd_term = cmd;
        } else {
            cmd_term = term_get_tuple_element(cmd, 0);
            config = term_get_tuple_element(cmd, 1);
        }

        enum network_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {
            case NetworkStartCmd:
                start_network(ctx, pid, ref, config);
                break;
            case NetworkRssiCmd:
                get_sta_rssi(ctx, pid, ref);
                break;
            case NetworkStopCmd:
                cmd_terminate = true;
                stop_network(ctx);
                break;
            case NetworkScanCmd:
                wifi_scan(ctx, pid, ref, config);
                break;
            case NetworkScanStopCmd:
                cancel_scan(ctx, pid, ref, config);
                break;
            case StaHaltCmd:
                sta_disconnect(ctx, pid, ref);
                break;
            case StaConnectCmd:
                if (term_is_invalid_term(config)) {
                    sta_reconnect(ctx, pid, ref);
                } else {
                    sta_connect(ctx, pid, ref, config);
                }
                break;

            default: {
                ESP_LOGE(TAG, "Unrecognized command: %x", cmd);
                // {Ref, {error, badarg}}
                size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
                if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                    ESP_LOGE(TAG, "Unable to allocate heap space for error; no message sent");
                    return NativeContinue;
                }
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
            }
        }
    } else {
        // {Ref, {error, badarg}}
        size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for error; no message sent");
            return NativeContinue;
        }
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
    }

    // TODO: handle close with new API

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return cmd_terminate ? NativeTerminate : NativeContinue;
}

//
// Entrypoints
//

void network_driver_init(GlobalContext *global)
{
    esp_err_t err;

    if ((err = esp_netif_init()) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to initialize network interface %d", err);
        AVM_ABORT();
    } else {
        ESP_LOGI(TAG, "Initialized network interface");
    }
    err = esp_event_loop_create_default();
    if (err != ESP_OK && err != ESP_ERR_INVALID_STATE) {
        ESP_LOGE(TAG, "Failed to create default event loop (err=%d)", err);
        AVM_ABORT();
    } else {
        ESP_LOGI(TAG, "Created default event loop");
    }
}

Context *network_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, NULL, network_driver_create_port)

#endif
