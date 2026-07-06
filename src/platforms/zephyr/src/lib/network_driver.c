/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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

#include <zephyr/kernel.h>

#if defined(CONFIG_WIFI)

#include <atom.h>
#include <context.h>
#include <debug.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <memory.h>
#include <port.h>
#include <term.h>
#include <utils.h>

#include <zephyr/net/net_if.h>
#include <zephyr/net/net_mgmt.h>
#include <zephyr/net/wifi_mgmt.h>
#include <zephyr/net/net_event.h>
#include <zephyr/net/dhcpv4.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "zephyros_sys.h"

#define TAG "network_driver"
#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)
#define ZEPHYR_WIFI_SCAN_MAX_RESULTS 10

static const char *const sta_atom = ATOM_STR("\x3", "sta");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const rssi_atom = ATOM_STR("\x4", "rssi");
static const char *const scan_results_atom = ATOM_STR("\xC", "scan_results");
static const char *const scan_canceled_atom = ATOM_STR("\xD", "scan_canceled");
static const char *const sta_connected_atom = ATOM_STR("\xD", "sta_connected");
static const char *const sta_disconnected_atom = ATOM_STR("\x10", "sta_disconnected");
static const char *const sta_got_ip_atom = ATOM_STR("\xA", "sta_got_ip");
static const char *const managed_atom = ATOM_STR("\x7", "managed");

enum network_cmd
{
    NetworkInvalidCmd = 0,
    NetworkStartCmd,
    NetworkRssiCmd,
    NetworkStopCmd,
    NetworkScanStopCmd,
    StaHaltCmd,
    StaConnectCmd,
    WifiScanCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    { rssi_atom, NetworkRssiCmd },
    { ATOM_STR("\x4", "stop"), NetworkStopCmd },
    { ATOM_STR("\xB", "cancel_scan"), NetworkScanStopCmd },
    { ATOM_STR("\x8", "halt_sta"), StaHaltCmd },
    { ATOM_STR("\x7", "connect"), StaConnectCmd },
    { ATOM_STR("\x4", "scan"), WifiScanCmd },
    SELECT_INT_DEFAULT(NetworkInvalidCmd)
};

struct ZephyrScanResult
{
    uint8_t ssid[WIFI_SSID_MAX_LEN + 1];
    uint8_t ssid_length;
    uint8_t channel;
    enum wifi_security_type security;
    int8_t rssi;
    uint8_t mac[WIFI_MAC_ADDR_LEN];
    uint8_t mac_length;
};

struct NetworkDriverData
{
    GlobalContext *global;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    struct net_mgmt_event_callback wifi_cb;
    struct net_mgmt_event_callback ipv4_cb;
    struct net_mgmt_event_callback scan_cb;
    struct net_if *iface;
    char *ssid;
    char *psk;
    bool connected;
    bool got_ip;
    bool managed;
    bool cb_registered;
    bool scan_cb_registered;
    bool scanning;
    uint16_t scan_requested_results;
    uint16_t scan_discovered;
    uint16_t scan_stored;
    uint32_t scan_owner_process_id;
    uint64_t scan_ref_ticks;
    struct ZephyrScanResult scan_results[ZEPHYR_WIFI_SCAN_MAX_RESULTS];
    term sta_connected_term;
    term sta_disconnected_term;
    term sta_got_ip_term;
};

static struct NetworkDriverData *driver_data = NULL;

static term wifi_security_to_atom_term(GlobalContext *global, enum wifi_security_type security)
{
    switch (security) {
        case WIFI_SECURITY_TYPE_NONE:
            return globalcontext_make_atom(global, ATOM_STR("\x4", "open"));
        case WIFI_SECURITY_TYPE_WEP:
        case WIFI_SECURITY_TYPE_WEP_OPEN:
        case WIFI_SECURITY_TYPE_WEP_SHARED:
            return globalcontext_make_atom(global, ATOM_STR("\x3", "wep"));
        case WIFI_SECURITY_TYPE_WPA_PSK:
        case WIFI_SECURITY_TYPE_FT_PSK:
            return globalcontext_make_atom(global, ATOM_STR("\x7", "wpa_psk"));
        case WIFI_SECURITY_TYPE_PSK:
        case WIFI_SECURITY_TYPE_PSK_SHA256:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "wpa2_psk"));
        case WIFI_SECURITY_TYPE_SAE:
        case WIFI_SECURITY_TYPE_SAE_H2E:
        case WIFI_SECURITY_TYPE_SAE_AUTO:
        case WIFI_SECURITY_TYPE_FT_SAE:
        case WIFI_SECURITY_TYPE_SAE_EXT_KEY:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "wpa3_psk"));
        case WIFI_SECURITY_TYPE_WPA_AUTO_PERSONAL:
            return globalcontext_make_atom(global, ATOM_STR("\xc", "wpa_wpa2_psk"));
        case WIFI_SECURITY_TYPE_EAP:
        case WIFI_SECURITY_TYPE_EAP_PEAP_MSCHAPV2:
        case WIFI_SECURITY_TYPE_EAP_PEAP_GTC:
        case WIFI_SECURITY_TYPE_EAP_TTLS_MSCHAPV2:
        case WIFI_SECURITY_TYPE_EAP_PEAP_TLS:
        case WIFI_SECURITY_TYPE_FT_EAP:
        case WIFI_SECURITY_TYPE_FT_EAP_SHA384:
            return globalcontext_make_atom(global, ATOM_STR("\x3", "eap"));
        case WIFI_SECURITY_TYPE_WAPI:
            return globalcontext_make_atom(global, ATOM_STR("\x4", "wapi"));
        case WIFI_SECURITY_TYPE_DPP:
            return globalcontext_make_atom(global, ATOM_STR("\x3", "dpp"));
        default:
            return globalcontext_make_atom(global, ATOM_STR("\x7", "unknown"));
    }
}

static void send_scan_error_from_task(struct NetworkDriverData *data, term reason)
{
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2), heap);
    {
        term ref = term_from_ref_ticks(data->scan_ref_ticks, &heap);
        term error_tuple = port_heap_create_error_tuple(&heap, reason);
        term payload = port_heap_create_tuple2(&heap, globalcontext_make_atom(data->global, scan_results_atom), error_tuple);
        term msg = port_heap_create_tuple2(&heap, ref, payload);
        globalcontext_send_message_from_task(data->global, data->scan_owner_process_id, NormalMessage, msg);
    }
    END_WITH_STACK_HEAP(heap, data->global);
}

static void send_scan_results_from_task(struct NetworkDriverData *data)
{
    size_t ap_data_size = TERM_MAP_SIZE(6) + TERM_BINARY_HEAP_SIZE(WIFI_SSID_MAX_LEN) + TERM_BINARY_HEAP_SIZE(WIFI_MAC_ADDR_LEN);
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + BOXED_INT_SIZE + LIST_SIZE(data->scan_stored, ap_data_size);

    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, heap_size) != MEMORY_GC_OK)) {
        send_scan_error_from_task(data, OUT_OF_MEMORY_ATOM);
        return;
    }

    term authmode_atom_term = globalcontext_make_atom(data->global, ATOM_STR("\x8", "authmode"));
    term bssid_atom_term = globalcontext_make_atom(data->global, ATOM_STR("\x5", "bssid"));
    term channel_atom_term = globalcontext_make_atom(data->global, ATOM_STR("\x7", "channel"));
    term hidden_atom_term = globalcontext_make_atom(data->global, ATOM_STR("\x6", "hidden"));
    term rssi_atom_term = globalcontext_make_atom(data->global, rssi_atom);
    term ssid_atom_term = globalcontext_make_atom(data->global, ssid_atom);

    term networks_data_list = term_nil();
    for (int i = data->scan_stored - 1; i >= 0; i--) {
        struct ZephyrScanResult *result = &data->scan_results[i];
        term ssid_term = term_from_literal_binary(result->ssid, result->ssid_length, &heap, data->global);
        term bssid_term = term_from_literal_binary(result->mac, WIFI_MAC_ADDR_LEN, &heap, data->global);

        term ap_data = term_alloc_map(6, &heap);
        term_set_map_assoc(ap_data, 0, authmode_atom_term, wifi_security_to_atom_term(data->global, result->security));
        term_set_map_assoc(ap_data, 1, bssid_atom_term, bssid_term);
        term_set_map_assoc(ap_data, 2, channel_atom_term, term_from_int(result->channel));
        term_set_map_assoc(ap_data, 3, hidden_atom_term, result->ssid_length == 0 ? TRUE_ATOM : FALSE_ATOM);
        term_set_map_assoc(ap_data, 4, rssi_atom_term, term_from_int(result->rssi));
        term_set_map_assoc(ap_data, 5, ssid_atom_term, ssid_term);

        networks_data_list = term_list_prepend(ap_data, networks_data_list, &heap);
    }

    term scan_results = port_heap_create_tuple2(&heap, term_from_int(data->scan_discovered), networks_data_list);
    term payload = port_heap_create_tuple2(&heap, globalcontext_make_atom(data->global, scan_results_atom), scan_results);
    term ref = term_from_ref_ticks(data->scan_ref_ticks, &heap);
    term msg = port_heap_create_tuple2(&heap, ref, payload);

    globalcontext_send_message_from_task(data->global, data->scan_owner_process_id, NormalMessage, msg);
    memory_destroy_heap_from_task(&heap, data->global);
}

static void wifi_scan_event_handler(struct net_mgmt_event_callback *cb, uint64_t mgmt_event, struct net_if *iface)
{
    UNUSED(cb);

    if (!driver_data || iface != driver_data->iface || !driver_data->scanning) {
        return;
    }

    if (mgmt_event == NET_EVENT_WIFI_SCAN_RESULT) {
        const struct wifi_scan_result *result = (const struct wifi_scan_result *) cb->info;
        driver_data->scan_discovered++;
        if (driver_data->scan_stored >= driver_data->scan_requested_results) {
            return;
        }
        struct ZephyrScanResult *stored = &driver_data->scan_results[driver_data->scan_stored++];
        memset(stored, 0, sizeof(*stored));
        stored->ssid_length = result->ssid_length > WIFI_SSID_MAX_LEN ? WIFI_SSID_MAX_LEN : result->ssid_length;
        memcpy(stored->ssid, result->ssid, stored->ssid_length);
        stored->channel = result->channel;
        stored->security = result->security;
        stored->rssi = result->rssi;
        stored->mac_length = result->mac_length;
        memcpy(stored->mac, result->mac, WIFI_MAC_ADDR_LEN);
    } else if (mgmt_event == NET_EVENT_WIFI_SCAN_DONE) {
        if (driver_data->scan_cb_registered) {
            net_mgmt_del_event_callback(&driver_data->scan_cb);
            driver_data->scan_cb_registered = false;
        }
        driver_data->scanning = false;
        send_scan_results_from_task(driver_data);
    }
}

static void wifi_mgmt_event_handler(struct net_mgmt_event_callback *cb,
                                    uint64_t mgmt_event,
                                    struct net_if *iface)
{
    UNUSED(cb);

    if (!driver_data || iface != driver_data->iface) {
        return;
    }

    if (mgmt_event == NET_EVENT_WIFI_CONNECT_RESULT) {
        const struct wifi_status *status = (const struct wifi_status *)cb->info;
        if (status->status == 0) {
            if (driver_data->connected || driver_data->got_ip) {
                driver_data->connected = true;
                return;
            }
            driver_data->connected = true;

            // Notify AtomVM process: sta_connected
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            {
                term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
                term msg = port_heap_create_tuple2(&heap, ref, driver_data->sta_connected_term);
                globalcontext_send_message_from_task(driver_data->global, driver_data->owner_process_id, NormalMessage, msg);
            }
            END_WITH_STACK_HEAP(heap, driver_data->global);

            // Check if we already have an IP address configured
            struct net_if_ipv4 *ipv4 = driver_data->iface->config.ip.ipv4;
            bool has_ip = false;
            struct in_addr ip = {0};
            struct in_addr netmask = {0};
            struct in_addr gw = {0};
            if (ipv4) {
                for (int i = 0; i < NET_IF_MAX_IPV4_ADDR; i++) {
                    if (ipv4->unicast[i].ipv4.is_used) {
                        ip = ipv4->unicast[i].ipv4.address.in_addr;
                        netmask = ipv4->unicast[i].netmask;
                        gw = ipv4->gw;
                        has_ip = true;
                        break;
                    }
                }
            }

            if (has_ip && !driver_data->got_ip) {
                driver_data->got_ip = true;

                // Notify AtomVM process: {sta_got_ip, {IP, Netmask, Gateway}}
                BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
                {
                    uint32_t ip_val = ntohl(ip.s_addr);
                    term ip_elements[4] = {
                        term_from_int((ip_val >> 24) & 0xFF),
                        term_from_int((ip_val >> 16) & 0xFF),
                        term_from_int((ip_val >> 8) & 0xFF),
                        term_from_int(ip_val & 0xFF)
                    };
                    term ip_tuple = port_heap_create_tuple_n(&heap, 4, ip_elements);

                    uint32_t mask_val = ntohl(netmask.s_addr);
                    term mask_elements[4] = {
                        term_from_int((mask_val >> 24) & 0xFF),
                        term_from_int((mask_val >> 16) & 0xFF),
                        term_from_int((mask_val >> 8) & 0xFF),
                        term_from_int(mask_val & 0xFF)
                    };
                    term mask_tuple = port_heap_create_tuple_n(&heap, 4, mask_elements);

                    uint32_t gw_val = ntohl(gw.s_addr);
                    term gw_elements[4] = {
                        term_from_int((gw_val >> 24) & 0xFF),
                        term_from_int((gw_val >> 16) & 0xFF),
                        term_from_int((gw_val >> 8) & 0xFF),
                        term_from_int(gw_val & 0xFF)
                    };
                    term gw_tuple = port_heap_create_tuple_n(&heap, 4, gw_elements);

                    term ip_info = port_heap_create_tuple3(&heap, ip_tuple, mask_tuple, gw_tuple);
                    term reply_val = port_heap_create_tuple2(&heap, driver_data->sta_got_ip_term, ip_info);

                    term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
                    term msg = port_heap_create_tuple2(&heap, ref, reply_val);
                    globalcontext_send_message_from_task(driver_data->global, driver_data->owner_process_id, NormalMessage, msg);
                }
                END_WITH_STACK_HEAP(heap, driver_data->global);
            }

            // Start DHCPv4 client on interface
#if defined(CONFIG_NET_DHCPV4)
            net_dhcpv4_start(driver_data->iface);
#endif
        } else {
            driver_data->connected = false;
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            {
                term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
                term msg = port_heap_create_tuple2(&heap, ref, driver_data->sta_disconnected_term);
                globalcontext_send_message_from_task(driver_data->global, driver_data->owner_process_id, NormalMessage, msg);
            }
            END_WITH_STACK_HEAP(heap, driver_data->global);
        }
    } else if (mgmt_event == NET_EVENT_WIFI_DISCONNECT_RESULT) {
        driver_data->connected = false;
        driver_data->got_ip = false;

        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
        {
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = port_heap_create_tuple2(&heap, ref, driver_data->sta_disconnected_term);
            globalcontext_send_message_from_task(driver_data->global, driver_data->owner_process_id, NormalMessage, msg);
        }
        END_WITH_STACK_HEAP(heap, driver_data->global);
    }
}

static void ipv4_mgmt_event_handler(struct net_mgmt_event_callback *cb,
                                    uint64_t mgmt_event,
                                    struct net_if *iface)
{
    UNUSED(cb);

    if (!driver_data || iface != driver_data->iface) {
        return;
    }

    if (mgmt_event == NET_EVENT_IPV4_ADDR_ADD) {
        struct net_if_ipv4 *ipv4 = iface->config.ip.ipv4;
        if (!ipv4) {
            return;
        }

        struct in_addr ip = {0};
        struct in_addr netmask = {0};
        struct in_addr gw = ipv4->gw;

        for (int i = 0; i < NET_IF_MAX_IPV4_ADDR; i++) {
            if (ipv4->unicast[i].ipv4.is_used) {
                ip = ipv4->unicast[i].ipv4.address.in_addr;
                netmask = ipv4->unicast[i].netmask;
                break;
            }
        }

        driver_data->got_ip = true;

        // Notify AtomVM process: {sta_got_ip, {IP, Netmask, Gateway}}
        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
        {
            uint32_t ip_val = ntohl(ip.s_addr);
            term ip_elements[4] = {
                term_from_int((ip_val >> 24) & 0xFF),
                term_from_int((ip_val >> 16) & 0xFF),
                term_from_int((ip_val >> 8) & 0xFF),
                term_from_int(ip_val & 0xFF)
            };
            term ip_tuple = port_heap_create_tuple_n(&heap, 4, ip_elements);

            uint32_t mask_val = ntohl(netmask.s_addr);
            term mask_elements[4] = {
                term_from_int((mask_val >> 24) & 0xFF),
                term_from_int((mask_val >> 16) & 0xFF),
                term_from_int((mask_val >> 8) & 0xFF),
                term_from_int(mask_val & 0xFF)
            };
            term mask_tuple = port_heap_create_tuple_n(&heap, 4, mask_elements);

            uint32_t gw_val = ntohl(gw.s_addr);
            term gw_elements[4] = {
                term_from_int((gw_val >> 24) & 0xFF),
                term_from_int((gw_val >> 16) & 0xFF),
                term_from_int((gw_val >> 8) & 0xFF),
                term_from_int(gw_val & 0xFF)
            };
            term gw_tuple = port_heap_create_tuple_n(&heap, 4, gw_elements);

            term ip_info = port_heap_create_tuple3(&heap, ip_tuple, mask_tuple, gw_tuple);
            term reply_val = port_heap_create_tuple2(&heap, driver_data->sta_got_ip_term, ip_info);

            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = port_heap_create_tuple2(&heap, ref, reply_val);
            globalcontext_send_message_from_task(driver_data->global, driver_data->owner_process_id, NormalMessage, msg);
        }
        END_WITH_STACK_HEAP(heap, driver_data->global);
    }
}

static term start_network(Context *ctx, term pid, term ref, term config)
{
    if (UNLIKELY(!driver_data || !driver_data->iface)) {
        return BADARG_ATOM;
    }

    if (term_is_invalid_term(config)) {
        return BADARG_ATOM;
    }

    term sta_config = interop_kv_get_value(config, sta_atom, ctx->global);
    if (term_is_invalid_term(sta_config)) {
        return BADARG_ATOM;
    }

    term ssid_term = interop_kv_get_value(sta_config, ssid_atom, ctx->global);
    term psk_term = interop_kv_get_value(sta_config, psk_atom, ctx->global);

    term managed_term = interop_kv_get_value(sta_config, managed_atom, ctx->global);
    bool managed = (managed_term == TRUE_ATOM);

    if (term_is_invalid_term(ssid_term) && !managed) {
        return BADARG_ATOM;
    }

    int ok = 0;
    char *ssid = NULL;
    if (!term_is_invalid_term(ssid_term)) {
        ssid = interop_term_to_string(ssid_term, &ok);
        if (!ok) {
            return BADARG_ATOM;
        }
    }

    char *psk = NULL;
    if (!term_is_invalid_term(psk_term)) {
        psk = interop_term_to_string(psk_term, &ok);
        if (!ok) {
            free(ssid);
            return BADARG_ATOM;
        }
    }

    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);
    driver_data->managed = managed;

    if (driver_data->ssid) {
        free(driver_data->ssid);
    }
    driver_data->ssid = ssid;

    if (driver_data->psk) {
        free(driver_data->psk);
    }
    driver_data->psk = psk;

    // Register callbacks if not registered yet
    if (!driver_data->cb_registered) {
        net_mgmt_init_event_callback(&driver_data->wifi_cb, wifi_mgmt_event_handler,
                                     NET_EVENT_WIFI_CONNECT_RESULT | NET_EVENT_WIFI_DISCONNECT_RESULT);
        net_mgmt_add_event_callback(&driver_data->wifi_cb);

        net_mgmt_init_event_callback(&driver_data->ipv4_cb, ipv4_mgmt_event_handler,
                                     NET_EVENT_IPV4_ADDR_ADD);
        net_mgmt_add_event_callback(&driver_data->ipv4_cb);

        driver_data->cb_registered = true;
    }

    if (!managed) {
        struct wifi_connect_req_params cnx_params = {0};
        cnx_params.ssid = (const uint8_t *)driver_data->ssid;
        cnx_params.ssid_length = strlen(driver_data->ssid);
        if (driver_data->psk && (strlen(driver_data->psk) > 0)) {
            cnx_params.psk = (const uint8_t *)driver_data->psk;
            cnx_params.psk_length = strlen(driver_data->psk);
            cnx_params.security = WIFI_SECURITY_TYPE_PSK;
        } else {
            cnx_params.security = WIFI_SECURITY_TYPE_NONE;
        }
        cnx_params.channel = WIFI_CHANNEL_ANY;
        cnx_params.mfp = WIFI_MFP_OPTIONAL;
        cnx_params.timeout = SYS_FOREVER_MS;

        int err = net_mgmt(NET_REQUEST_WIFI_CONNECT, driver_data->iface, &cnx_params, sizeof(cnx_params));
        if (err != 0) {
            return ERROR_ATOM;
        }
    }

    return OK_ATOM;
}

static void wifi_scan(Context *ctx, term pid, term ref, term config)
{
    size_t error_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2);

    if (UNLIKELY(!driver_data || !driver_data->iface)) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }
    if (driver_data->scanning) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy"))));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }

    term results_term = interop_kv_get_value_default(config, ATOM_STR("\x7", "results"), term_from_int(6), ctx->global);
    if (!term_is_integer(results_term)) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }
    avm_int_t requested_results = term_to_int(results_term);
    if (requested_results < 1 || requested_results > ZEPHYR_WIFI_SCAN_MAX_RESULTS) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }

    term passive_term = interop_kv_get_value_default(config, ATOM_STR("\x7", "passive"), FALSE_ATOM, ctx->global);
    if (passive_term != TRUE_ATOM && passive_term != FALSE_ATOM) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }
    bool passive = passive_term == TRUE_ATOM;

    term dwell_term = interop_kv_get_value_default(config, ATOM_STR("\x5", "dwell"), term_from_int(passive ? 360 : 120), ctx->global);
    if (!term_is_integer(dwell_term)) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }
    avm_int_t dwell_ms = term_to_int(dwell_term);
    if (dwell_ms < 1 || dwell_ms > 1500) {
        port_ensure_available(ctx, error_size);
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, BADARG_ATOM));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }

    term reply_term = interop_kv_get_value_default(config, ATOM_STR("\x5", "reply"), FALSE_ATOM, ctx->global);
    bool reply_immediately = reply_term == TRUE_ATOM;

    driver_data->scan_owner_process_id = term_to_local_process_id(pid);
    driver_data->scan_ref_ticks = term_to_ref_ticks(ref);
    driver_data->scan_requested_results = (uint16_t) requested_results;
    driver_data->scan_discovered = 0;
    driver_data->scan_stored = 0;
    memset(driver_data->scan_results, 0, sizeof(driver_data->scan_results));

    if (!driver_data->scan_cb_registered) {
        net_mgmt_init_event_callback(&driver_data->scan_cb, wifi_scan_event_handler, NET_EVENT_WIFI_SCAN_RESULT | NET_EVENT_WIFI_SCAN_DONE);
        net_mgmt_add_event_callback(&driver_data->scan_cb);
        driver_data->scan_cb_registered = true;
    }
    driver_data->scanning = true;

    struct wifi_scan_params params = {0};
    params.scan_type = passive ? WIFI_SCAN_TYPE_PASSIVE : WIFI_SCAN_TYPE_ACTIVE;
    params.dwell_time_active = (uint16_t) dwell_ms;
    params.dwell_time_passive = (uint16_t) dwell_ms;
    params.max_bss_cnt = (uint16_t) requested_results;

    int err = net_mgmt(NET_REQUEST_WIFI_SCAN, driver_data->iface, &params, sizeof(params));
    if (err != 0) {
        if (driver_data->scan_cb_registered) {
            net_mgmt_del_event_callback(&driver_data->scan_cb);
            driver_data->scan_cb_registered = false;
        }
        driver_data->scanning = false;
        port_ensure_available(ctx, error_size);
        term reason = (err == -EBUSY) ? globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")) : ERROR_ATOM;
        term payload = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, scan_results_atom), port_create_error_tuple(ctx, reason));
        port_send_reply(ctx, pid, ref, payload);
        return;
    }

    if (reply_immediately) {
        port_ensure_available(ctx, PORT_REPLY_SIZE);
        port_send_reply(ctx, pid, ref, OK_ATOM);
    }
}

static void cancel_scan(Context *ctx, term pid, term ref, term reply_config)
{
    if (driver_data && driver_data->scanning) {
        if (driver_data->scan_cb_registered) {
            net_mgmt_del_event_callback(&driver_data->scan_cb);
            driver_data->scan_cb_registered = false;
        }
        driver_data->scanning = false;
    }

    term scan_canceled = globalcontext_make_atom(ctx->global, scan_canceled_atom);
    size_t reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(3);
    port_ensure_available(ctx, reply_size);
    term reply = port_create_tuple3(ctx, scan_canceled, reply_config, OK_ATOM);
    port_send_reply(ctx, pid, ref, reply);
}

static term resolve_sta_config(term config, GlobalContext *global)
{
    term sta_config = interop_kv_get_value(config, sta_atom, global);
    if (!term_is_invalid_term(sta_config)) {
        return sta_config;
    }
    if (!term_is_invalid_term(interop_kv_get_value(config, ssid_atom, global))) {
        return config;
    }
    return term_invalid_term();
}

static term sta_connect_ap(Context *ctx, term pid, term ref, term config)
{
    if (UNLIKELY(!driver_data || !driver_data->iface)) {
        return BADARG_ATOM;
    }

    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);

    char *ssid = NULL;
    char *psk = NULL;
    bool new_creds = false;

    if (!term_is_invalid_term(config)) {
        term resolved = resolve_sta_config(config, ctx->global);
        if (!term_is_invalid_term(resolved)) {
            term ssid_term = interop_kv_get_value(resolved, ssid_atom, ctx->global);
            if (!term_is_invalid_term(ssid_term)) {
                int ok = 0;
                ssid = interop_term_to_string(ssid_term, &ok);
                if (!ok) {
                    return BADARG_ATOM;
                }
                term psk_term = interop_kv_get_value(resolved, psk_atom, ctx->global);
                if (!term_is_invalid_term(psk_term)) {
                    psk = interop_term_to_string(psk_term, &ok);
                    if (!ok) {
                        free(ssid);
                        return BADARG_ATOM;
                    }
                }
                new_creds = true;
            }
        }
    }

    if (!ssid) {
        if (!driver_data->ssid) {
            return BADARG_ATOM;
        }
        ssid = driver_data->ssid;
        psk = driver_data->psk;
    }

    if (new_creds) {
        if (driver_data->ssid) {
            free(driver_data->ssid);
        }
        driver_data->ssid = ssid;
        if (driver_data->psk) {
            free(driver_data->psk);
        }
        driver_data->psk = psk;
    }

    struct wifi_connect_req_params cnx_params = {0};
    cnx_params.ssid = (const uint8_t *)ssid;
    cnx_params.ssid_length = strlen(ssid);
    if (psk && (strlen(psk) > 0)) {
        cnx_params.psk = (const uint8_t *)psk;
        cnx_params.psk_length = strlen(psk);
        cnx_params.security = WIFI_SECURITY_TYPE_PSK;
    } else {
        cnx_params.security = WIFI_SECURITY_TYPE_NONE;
    }
    cnx_params.channel = WIFI_CHANNEL_ANY;
    cnx_params.mfp = WIFI_MFP_OPTIONAL;
    cnx_params.timeout = SYS_FOREVER_MS;

    int err = net_mgmt(NET_REQUEST_WIFI_CONNECT, driver_data->iface, &cnx_params, sizeof(cnx_params));
    if (err != 0) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term sta_disconnect_ap(Context *ctx, term pid, term ref)
{
    UNUSED(ctx);
    UNUSED(pid);
    UNUSED(ref);

    if (UNLIKELY(!driver_data)) {
        return BADARG_ATOM;
    }

    int err = net_mgmt(NET_REQUEST_WIFI_DISCONNECT, driver_data->iface, NULL, 0);
    if (err != 0) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static void send_cmd_reply(Context *ctx, term pid, term ref, term ret)
{
    if (ret == OK_ATOM) {
        port_ensure_available(ctx, PORT_REPLY_SIZE);
        port_send_reply(ctx, pid, ref, OK_ATOM);
    } else {
        port_ensure_available(ctx, PORT_REPLY_SIZE + TUPLE_SIZE(2));
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, ret));
    }
}

static void stop_network(void)
{
    if (!driver_data) {
        return;
    }

    if (driver_data->cb_registered) {
        net_mgmt_del_event_callback(&driver_data->wifi_cb);
        net_mgmt_del_event_callback(&driver_data->ipv4_cb);
        driver_data->cb_registered = false;
    }

    if (driver_data->iface) {
#if defined(CONFIG_NET_DHCPV4)
        net_dhcpv4_stop(driver_data->iface);
#endif
        net_mgmt(NET_REQUEST_WIFI_DISCONNECT, driver_data->iface, NULL, 0);
    }

    driver_data->connected = false;
    driver_data->got_ip = false;
}

static void get_sta_rssi(Context *ctx, term pid, term ref)
{
    size_t tuple_reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);

    if (UNLIKELY(!driver_data)) {
        port_ensure_available(ctx, tuple_reply_size);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        return;
    }

    struct wifi_iface_status status = {0};
    int err = net_mgmt(NET_REQUEST_WIFI_IFACE_STATUS, driver_data->iface, &status, sizeof(status));
    if (err != 0 || status.state < WIFI_STATE_ASSOCIATED) {
        port_ensure_available(ctx, tuple_reply_size);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, ERROR_ATOM));
        return;
    }

    term rssi = term_from_int11(status.rssi);
    port_ensure_available(ctx, tuple_reply_size);
    term reply = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, rssi_atom), rssi);
    port_send_reply(ctx, pid, ref, reply);
}

static NativeHandlerResult consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

    if (UNLIKELY(!term_is_tuple(msg) || term_get_tuple_arity(msg) != 3)) {
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }

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

        enum network_cmd command = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (command) {
            case NetworkStartCmd: {
                term ret = start_network(ctx, pid, ref, config);
                send_cmd_reply(ctx, pid, ref, ret);
                break;
            }
            case NetworkRssiCmd: {
                get_sta_rssi(ctx, pid, ref);
                break;
            }
            case NetworkStopCmd: {
                stop_network();
                mailbox_remove_message(&ctx->mailbox, &ctx->heap);
                return NativeTerminate;
            }
            case StaHaltCmd: {
                term ret = sta_disconnect_ap(ctx, pid, ref);
                send_cmd_reply(ctx, pid, ref, ret);
                break;
            }
            case StaConnectCmd: {
                term ret = sta_connect_ap(ctx, pid, ref, config);
                send_cmd_reply(ctx, pid, ref, ret);
                break;
            }
            case WifiScanCmd: {
                wifi_scan(ctx, pid, ref, config);
                break;
            }
            case NetworkScanStopCmd: {
                cancel_scan(ctx, pid, ref, config);
                break;
            }
            default: {
                send_cmd_reply(ctx, pid, ref, BADARG_ATOM);
            }
        }
    } else {
        send_cmd_reply(ctx, pid, ref, BADARG_ATOM);
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    return NativeContinue;
}

void network_driver_init(GlobalContext *global)
{
    if (driver_data) {
        return;
    }

    struct NetworkDriverData *data = calloc(1, sizeof(struct NetworkDriverData));
    if (!data) {
        return;
    }

    data->global = global;
    data->iface = net_if_get_first_wifi();
    if (!data->iface) {
        free(data);
        return;
    }
    data->sta_connected_term = globalcontext_make_atom(global, sta_connected_atom);
    data->sta_disconnected_term = globalcontext_make_atom(global, sta_disconnected_atom);
    data->sta_got_ip_term = globalcontext_make_atom(global, sta_got_ip_atom);
    driver_data = data;
}

void network_driver_destroy(GlobalContext *global)
{
    UNUSED(global);

    if (driver_data) {
        if (driver_data->cb_registered) {
            net_mgmt_del_event_callback(&driver_data->wifi_cb);
            net_mgmt_del_event_callback(&driver_data->ipv4_cb);
        }
        if (driver_data->scan_cb_registered) {
            net_mgmt_del_event_callback(&driver_data->scan_cb);
        }
        free(driver_data->ssid);
        free(driver_data->psk);
        free(driver_data);
        driver_data = NULL;
    }
}

Context *network_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);
    if (!driver_data) {
        return NULL;
    }

    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_destroy, network_driver_create_port)

#endif /* CONFIG_WIFI */
