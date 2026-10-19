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

#include "sntp.h"
#include "wifi.h"

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <memory.h>
#include <port.h>
#include <portnifloader.h>
#include <term.h>
#include <utils.h>

#include <errno.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <string.h>

#ifdef RTEMS_WIFI_BACKEND_ESP32
#include <esp_event.h>
#include <esp_wifi.h>
#endif

#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + TERM_BOXED_REFERENCE_SHORT_SIZE)

static const char *const sta_atom = ATOM_STR("\x3", "sta");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const sta_connected_atom = ATOM_STR("\xD", "sta_connected");
static const char *const sta_disconnected_atom = ATOM_STR("\x10", "sta_disconnected");
static const char *const sta_got_ip_atom = ATOM_STR("\xA", "sta_got_ip");
static const char *const rssi_atom = ATOM_STR("\x4", "rssi");
static const char *const sntp_atom = ATOM_STR("\x4", "sntp");
static const char *const host_atom = ATOM_STR("\x4", "host");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const sntp_sync_atom = ATOM_STR("\x9", "sntp_sync");

enum network_cmd
{
    NetworkInvalidCmd = 0,
    NetworkStartCmd,
    NetworkConnectCmd,
    NetworkHaltStaCmd,
    NetworkRssiCmd,
    NetworkScanCmd,
    NetworkCancelScanCmd,
    NetworkStopCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    { ATOM_STR("\x7", "connect"), NetworkConnectCmd },
    { ATOM_STR("\x8", "halt_sta"), NetworkHaltStaCmd },
    { ATOM_STR("\x4", "rssi"), NetworkRssiCmd },
    { ATOM_STR("\x4", "scan"), NetworkScanCmd },
    { ATOM_STR("\xB", "cancel_scan"), NetworkCancelScanCmd },
    { ATOM_STR("\x4", "stop"), NetworkStopCmd },
    SELECT_INT_DEFAULT(NetworkInvalidCmd)
};

struct NetworkDriverData
{
    GlobalContext *global;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    char *ssid;
    char *psk;
    char *sntp_host;
    int sntp_port;
    bool sntp_enabled;
#ifdef RTEMS_WIFI_BACKEND_ESP32
    atomic_uint scan_port_id;
    atomic_bool scan_done_received;
    bool scan_handler_registered;
#endif
};

static struct NetworkDriverData *driver_data = NULL;

static void send_callback_message(GlobalContext *global, uint32_t process_id, term message)
{
#ifdef AVM_TASK_DRIVER_ENABLED
    port_send_message_from_task(global, term_from_local_process_id(process_id), message);
#else
    globalcontext_send_message(global, process_id, message);
#endif
}

#ifdef RTEMS_WIFI_BACKEND_ESP32
struct ScanData
{
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    uint16_t limit;
    bool active;
    bool canceled;
    bool stopping;
};

/* The RTEMS event dispatcher holds its handler lock during callbacks. Notify
 * the port and collect results there; registering/unregistering here deadlocks. */
static void scan_done_callback(void *arg, esp_event_base_t base, int32_t id, void *event)
{
    UNUSED(base);
    UNUSED(id);
    struct NetworkDriverData *data = arg;
    uint32_t port_id = atomic_load(&data->scan_port_id);
    if (port_id == 0 || port_id == UINT32_MAX) {
        return;
    }
    const wifi_event_sta_scan_done_t *done = event;
    atomic_store(&data->scan_done_received, true);
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap);
    term msg = port_heap_create_tuple2(&heap,
        globalcontext_make_atom(data->global, ATOM_STR("\xE", "wifi_scan_done")),
        term_from_int(done != NULL ? done->status : 1));
    send_callback_message(data->global, port_id, msg);
    END_WITH_STACK_HEAP(heap, data->global);
}

static term scan_authmode(GlobalContext *global, wifi_auth_mode_t mode)
{
    static const AtomStringIntPair modes[] = {
        { ATOM_STR("\x4", "open"), WIFI_AUTH_OPEN },
        { ATOM_STR("\x3", "wep"), WIFI_AUTH_WEP },
        { ATOM_STR("\x7", "wpa_psk"), WIFI_AUTH_WPA_PSK },
        { ATOM_STR("\x8", "wpa2_psk"), WIFI_AUTH_WPA2_PSK },
        { ATOM_STR("\xC", "wpa_wpa2_psk"), WIFI_AUTH_WPA_WPA2_PSK },
        { ATOM_STR("\x3", "eap"), WIFI_AUTH_ENTERPRISE },
        { ATOM_STR("\x8", "wpa3_psk"), WIFI_AUTH_WPA3_PSK },
        { ATOM_STR("\xD", "wpa2_wpa3_psk"), WIFI_AUTH_WPA2_WPA3_PSK },
        { ATOM_STR("\x4", "wapi"), WIFI_AUTH_WAPI_PSK },
        { ATOM_STR("\x3", "owe"), WIFI_AUTH_OWE },
        { ATOM_STR("\x13", "wpa3_enterprise_192"), WIFI_AUTH_WPA3_ENT_192 },
        { ATOM_STR("\x3", "dpp"), WIFI_AUTH_DPP },
        { ATOM_STR("\xF", "wpa3_enterprise"), WIFI_AUTH_WPA3_ENTERPRISE },
        { ATOM_STR("\x14", "wpa2_wpa3_enterprise"), WIFI_AUTH_WPA2_WPA3_ENTERPRISE },
        { ATOM_STR("\xE", "wpa_enterprise"), WIFI_AUTH_WPA_ENTERPRISE },
        SELECT_INT_DEFAULT(-1)
    };
    term authmode = interop_atom_term_select_atom(modes, mode, global);
    return term_is_invalid_term(authmode) ? UNDEFINED_ATOM : authmode;
}

static NativeHandlerResult finish_scan(Context *ctx, int status)
{
    struct ScanData *scan = ctx->platform_data;
    if (!scan->active) {
        return NativeContinue;
    }
    uint16_t found = 0;
    uint16_t count = 0;
    wifi_ap_record_t *records = NULL;
    term reason = term_invalid_term();
    if (!scan->canceled) {
        if (status != 0 || esp_wifi_scan_get_ap_num(&found) != ESP_OK) {
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "eio"));
        } else {
            count = found < scan->limit ? found : scan->limit;
            if (count > 0) {
                records = calloc(count, sizeof(*records));
                if (records == NULL) {
                    reason = OUT_OF_MEMORY_ATOM;
                } else if (esp_wifi_scan_get_ap_records(&count, records) != ESP_OK) {
                    reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "eio"));
                }
            }
        }
    }
    /* Fetching AP records consumes the SDK list; clear it on other paths. */
    if (records == NULL || !term_is_invalid_term(reason)) {
        esp_wifi_clear_ap_list();
    }
    scan->active = false;
    atomic_store(&driver_data->scan_port_id, 0);
    if (!scan->canceled) {
        size_t reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) * 2;
        size_t needed = reply_size;
        if (term_is_invalid_term(reason)) {
            needed += LIST_SIZE(count, TERM_MAP_SIZE(6) + TERM_BINARY_HEAP_SIZE(32) + TERM_BINARY_HEAP_SIZE(6));
        }
        if (memory_ensure_free_opt(ctx, needed, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK) {
            free(records);
            records = NULL;
            reason = OUT_OF_MEMORY_ATOM;
            if (memory_ensure_free_opt(ctx, reply_size, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK) {
                return NativeContinue;
            }
        }
        term result;
        if (!term_is_invalid_term(reason)) {
            result = port_create_error_tuple(ctx, reason);
        } else {
            term list = term_nil();
            for (uint16_t i = count; i > 0; i--) {
                wifi_ap_record_t *ap = &records[i - 1];
                size_t ssid_len = strnlen((const char *) ap->ssid, 32);
                term ssid = term_from_literal_binary(ap->ssid, ssid_len, &ctx->heap, ctx->global);
                term bssid = term_from_literal_binary(ap->bssid, 6, &ctx->heap, ctx->global);
                term map = term_alloc_map(6, &ctx->heap);
                term_set_map_assoc(map, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "authmode")), scan_authmode(ctx->global, ap->authmode));
                term_set_map_assoc(map, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "bssid")), bssid);
                term_set_map_assoc(map, 2, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "channel")), term_from_int(ap->primary));
                term_set_map_assoc(map, 3, globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "hidden")), ssid_len == 0 ? TRUE_ATOM : FALSE_ATOM);
                term_set_map_assoc(map, 4, globalcontext_make_atom(ctx->global, rssi_atom), term_from_int(ap->rssi));
                term_set_map_assoc(map, 5, globalcontext_make_atom(ctx->global, ssid_atom), ssid);
                list = term_list_prepend(map, list, &ctx->heap);
            }
            result = port_create_tuple2(ctx, term_from_int(found), list);
        }
        term reply = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "scan_results")), result);
        port_send_reply(ctx, term_from_local_process_id(scan->owner_process_id), term_from_ref_ticks(scan->ref_ticks, &ctx->heap), reply);
    }
    free(records);
    if (scan->stopping) {
        rtems_wifi_stop();
        return NativeTerminate;
    }
    return NativeContinue;
}
#endif

static void send_scan_error(Context *ctx, term pid, term ref, term reason)
{
    term roots[] = { ref, reason };
    if (memory_ensure_free_with_roots(ctx, PORT_REPLY_SIZE + TUPLE_SIZE(2) * 2, 2, roots, MEMORY_NO_SHRINK) != MEMORY_GC_OK) {
        return;
    }
    term reply = port_create_tuple2(ctx,
        globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "scan_results")),
        port_create_error_tuple(ctx, roots[1]));
    port_send_reply(ctx, pid, roots[0], reply);
}

static void start_scan(Context *ctx, term pid, term ref, term options)
{
#ifdef RTEMS_WIFI_BACKEND_ESP32
    if (!term_is_list(options)) {
        send_scan_error(ctx, pid, ref, BADARG_ATOM);
        return;
    }
    term limit = interop_kv_get_value_default(options, ATOM_STR("\x7", "results"), term_from_int(6), ctx->global);
    term passive = interop_kv_get_value_default(options, ATOM_STR("\x7", "passive"), FALSE_ATOM, ctx->global);
    term hidden = interop_kv_get_value_default(options, ATOM_STR("\xB", "show_hidden"), FALSE_ATOM, ctx->global);
    term dwell = interop_kv_get_value_default(options, ATOM_STR("\x5", "dwell"), term_from_int(passive == FALSE_ATOM ? 120 : 360), ctx->global);
    bool is_passive = passive == TRUE_ATOM || globalcontext_is_term_equal_to_atom_string(ctx->global, passive, ATOM_STR("\x7", "passive"));
    bool show_hidden = hidden == TRUE_ATOM || globalcontext_is_term_equal_to_atom_string(ctx->global, hidden, ATOM_STR("\xB", "show_hidden"));
    if (!term_is_integer(limit) || term_to_int(limit) < 1 || term_to_int(limit) > 64
        || !term_is_integer(dwell) || term_to_int(dwell) < 1 || term_to_int(dwell) > 1500
        || (!is_passive && passive != FALSE_ATOM) || (!show_hidden && hidden != FALSE_ATOM)) {
        send_scan_error(ctx, pid, ref, BADARG_ATOM);
        return;
    }
    struct ScanData *scan = ctx->platform_data;
    unsigned previous = atomic_load(&driver_data->scan_port_id);
    if (previous != 0 && previous != UINT32_MAX && atomic_load(&driver_data->scan_done_received)
        && !globalcontext_process_exists(ctx->global, previous)
        && atomic_compare_exchange_strong(&driver_data->scan_port_id, &previous, UINT32_MAX)) {
        esp_wifi_clear_ap_list();
        atomic_store(&driver_data->scan_port_id, 0);
    }
    unsigned expected = 0;
    if (!atomic_compare_exchange_strong(&driver_data->scan_port_id, &expected, ctx->process_id)) {
        send_scan_error(ctx, pid, ref, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")));
        return;
    }
    atomic_store(&driver_data->scan_done_received, false);
    esp_err_t err = ESP_OK;
    if (!driver_data->scan_handler_registered) {
        err = esp_event_handler_register(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, scan_done_callback, driver_data);
        driver_data->scan_handler_registered = err == ESP_OK;
    }
    *scan = (struct ScanData) {
        .owner_process_id = term_to_local_process_id(pid),
        .ref_ticks = term_to_ref_ticks(ref),
        .limit = term_to_int(limit),
        .active = true
    };
    wifi_scan_config_t config = { 0 };
    config.show_hidden = show_hidden;
    config.scan_type = is_passive ? WIFI_SCAN_TYPE_PASSIVE : WIFI_SCAN_TYPE_ACTIVE;
    if (is_passive) {
        config.scan_time.passive = term_to_int(dwell);
    } else {
        config.scan_time.active.min = term_to_int(dwell) > 240 ? term_to_int(dwell) / 2 : term_to_int(dwell);
        config.scan_time.active.max = term_to_int(dwell);
    }
    /*
     * A zero channel bitmap means "all channels" to the older blobs, and to
     * this one in 2.4 GHz-only mode. In dual-band auto mode the same zero
     * means "no channels": cnx_sta_scan_cmd skips scan_bitmap_to_channel_list
     * and scan_start then fails, so the scan completes with no APs. Fill the
     * bitmap for each band the chip can use, leaving bit 0 clear so the band
     * is not bypassed. Country policy still drops channels that are not
     * allowed. A 2.4 GHz-only blob ignores a 5 GHz bitmap, but the immediates
     * would still be linked into every board, so omit them there.
     */
    config.channel_bitmap.ghz_2_channels = (uint16_t) (WIFI_CHANNEL_1 | WIFI_CHANNEL_2
        | WIFI_CHANNEL_3 | WIFI_CHANNEL_4 | WIFI_CHANNEL_5 | WIFI_CHANNEL_6
        | WIFI_CHANNEL_7 | WIFI_CHANNEL_8 | WIFI_CHANNEL_9 | WIFI_CHANNEL_10
        | WIFI_CHANNEL_11 | WIFI_CHANNEL_12 | WIFI_CHANNEL_13 | WIFI_CHANNEL_14);
#if defined(RTEMS_BOARD_ESP32C5)
    config.channel_bitmap.ghz_5_channels = WIFI_CHANNEL_36 | WIFI_CHANNEL_40
        | WIFI_CHANNEL_44 | WIFI_CHANNEL_48 | WIFI_CHANNEL_52 | WIFI_CHANNEL_56
        | WIFI_CHANNEL_60 | WIFI_CHANNEL_64 | WIFI_CHANNEL_100 | WIFI_CHANNEL_104
        | WIFI_CHANNEL_108 | WIFI_CHANNEL_112 | WIFI_CHANNEL_116 | WIFI_CHANNEL_120
        | WIFI_CHANNEL_124 | WIFI_CHANNEL_128 | WIFI_CHANNEL_132 | WIFI_CHANNEL_136
        | WIFI_CHANNEL_140 | WIFI_CHANNEL_144 | WIFI_CHANNEL_149 | WIFI_CHANNEL_153
        | WIFI_CHANNEL_157 | WIFI_CHANNEL_161 | WIFI_CHANNEL_165 | WIFI_CHANNEL_169
        | WIFI_CHANNEL_173 | WIFI_CHANNEL_177;
#endif
    if (err == ESP_OK) {
        err = esp_wifi_scan_start(&config, false);
    }
    if (err != ESP_OK) {
        scan->active = false;
        atomic_store(&driver_data->scan_port_id, 0);
        send_scan_error(ctx, pid, ref, term_from_int(err));
    } else if (interop_kv_get_value_default(options, ATOM_STR("\x5", "reply"), FALSE_ATOM, ctx->global) == TRUE_ATOM) {
        if (memory_ensure_free_with_roots(ctx, PORT_REPLY_SIZE, 1, &ref, MEMORY_NO_SHRINK) == MEMORY_GC_OK) {
            port_send_reply(ctx, pid, ref, OK_ATOM);
        }
    }
#else
    UNUSED(options);
    send_scan_error(ctx, pid, ref, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "enotsup")));
#endif
}

static void cancel_scan(Context *ctx, term pid, term ref, term config)
{
    term roots[] = { ref, config };
    if (memory_ensure_free_with_roots(ctx, PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3), 2, roots, MEMORY_NO_SHRINK) != MEMORY_GC_OK) {
        return;
    }
    term result = OK_ATOM;
#ifdef RTEMS_WIFI_BACKEND_ESP32
    struct ScanData *scan = ctx->platform_data;
    if (scan->active) {
        esp_err_t err = atomic_load(&driver_data->scan_done_received) ? ESP_OK : esp_wifi_scan_stop();
        if (err == ESP_OK || atomic_load(&driver_data->scan_done_received)) {
            scan->canceled = true;
        } else {
            result = port_create_error_tuple(ctx, term_from_int(err));
        }
    }
#endif
    term reply = port_create_tuple3(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "scan_canceled")), roots[1], result);
    port_send_reply(ctx, pid, roots[0], reply);
}

static term make_ip_tuple(Heap *heap, uint32_t addr)
{
    term ip = term_alloc_tuple(4, heap);
    term_put_tuple_element(ip, 0, term_from_int11((addr >> 24) & 0xFF));
    term_put_tuple_element(ip, 1, term_from_int11((addr >> 16) & 0xFF));
    term_put_tuple_element(ip, 2, term_from_int11((addr >> 8) & 0xFF));
    term_put_tuple_element(ip, 3, term_from_int11(addr & 0xFF));
    return ip;
}

static void sntp_sync_callback(const struct timeval *tv, void *arg)
{
    (void) arg;
    if (driver_data == NULL || driver_data->owner_process_id == 0) {
        return;
    }

    GlobalContext *glb = driver_data->global;
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) * 2 + BOXED_INT64_SIZE * 2, heap);
    term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
    term tv_tuple = port_heap_create_tuple2(&heap,
        term_make_maybe_boxed_int64(tv->tv_sec, &heap),
        term_make_maybe_boxed_int64(tv->tv_usec, &heap));
    term payload = port_heap_create_tuple2(&heap,
        globalcontext_make_atom(glb, sntp_sync_atom),
        tv_tuple);
    term msg = port_heap_create_tuple2(&heap, ref, payload);
    send_callback_message(glb, driver_data->owner_process_id, msg);
    END_WITH_STACK_HEAP(heap, glb);
}

static void wifi_event_callback(rtems_wifi_state_t state, const struct rtems_wifi_ip_info *ip_info, void *arg)
{
    (void) arg;
    if (driver_data == NULL || driver_data->owner_process_id == 0) {
        return;
    }

    GlobalContext *glb = driver_data->global;

    switch (state) {
        case RTEMS_WIFI_ASSOCIATED: {
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(msg, 0, ref);
            term_put_tuple_element(msg, 1, globalcontext_make_atom(glb, sta_connected_atom));
            send_callback_message(glb, driver_data->owner_process_id, msg);
            END_WITH_STACK_HEAP(heap, glb);
            break;
        }

        case RTEMS_WIFI_GOT_IP: {
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term ip = make_ip_tuple(&heap, ip_info->ip);
            term netmask = make_ip_tuple(&heap, ip_info->netmask);
            term gw = make_ip_tuple(&heap, ip_info->gateway);

            term ip_tuple = term_alloc_tuple(3, &heap);
            term_put_tuple_element(ip_tuple, 0, ip);
            term_put_tuple_element(ip_tuple, 1, netmask);
            term_put_tuple_element(ip_tuple, 2, gw);

            term payload = term_alloc_tuple(2, &heap);
            term_put_tuple_element(payload, 0, globalcontext_make_atom(glb, sta_got_ip_atom));
            term_put_tuple_element(payload, 1, ip_tuple);

            term msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(msg, 0, ref);
            term_put_tuple_element(msg, 1, payload);
            send_callback_message(glb, driver_data->owner_process_id, msg);
            END_WITH_STACK_HEAP(heap, glb);

            if (driver_data->sntp_enabled) {
                rtems_sntp_start(driver_data->sntp_host, driver_data->sntp_port, sntp_sync_callback, NULL);
            }
            break;
        }

        case RTEMS_WIFI_DISCONNECTED: {
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(msg, 0, ref);
            term_put_tuple_element(msg, 1, globalcontext_make_atom(glb, sta_disconnected_atom));
            send_callback_message(glb, driver_data->owner_process_id, msg);
            END_WITH_STACK_HEAP(heap, glb);
            break;
        }

        default:
            break;
    }
}

static bool extract_sta_config(term sta_config, GlobalContext *glb, char **ssid_out, char **psk_out, bool require_ssid)
{
    term ssid_term = interop_kv_get_value_default(sta_config, ssid_atom, term_invalid_term(), glb);
    if (term_is_invalid_term(ssid_term) && require_ssid) {
        return false;
    }

    int ok = 0;
    char *ssid;
    if (term_is_invalid_term(ssid_term)) {
        ssid = strdup("");
        ok = ssid != NULL;
    } else {
        ssid = interop_term_to_string(ssid_term, &ok);
    }
    if (!ok || ssid == NULL) {
        return false;
    }

    char *psk = NULL;
    term psk_term = interop_kv_get_value_default(sta_config, psk_atom, term_invalid_term(), glb);
    if (!term_is_invalid_term(psk_term)) {
        psk = interop_term_to_string(psk_term, &ok);
        if (!ok) {
            free(ssid);
            return false;
        }
    }

    *ssid_out = ssid;
    *psk_out = psk;
    return true;
}

static void start_network(Context *ctx, term pid, term ref, term config, bool connect)
{
    if (driver_data == NULL) {
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        return;
    }

    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);

    term sta_config = interop_kv_get_value_default(config, sta_atom, term_invalid_term(), ctx->global);
    if (!term_is_invalid_term(sta_config)) {
        bool associate = true;
#ifdef RTEMS_WIFI_BACKEND_ESP32
        term managed = interop_kv_get_value_default(sta_config, ATOM_STR("\x7", "managed"), FALSE_ATOM, ctx->global);
        associate = connect || !(managed == TRUE_ATOM || globalcontext_is_term_equal_to_atom_string(ctx->global, managed, ATOM_STR("\x7", "managed")));
#else
        UNUSED(connect);
#endif
        {
            char *ssid = NULL;
            char *psk = NULL;
            if (extract_sta_config(sta_config, ctx->global, &ssid, &psk, associate)) {
                free(driver_data->ssid);
                free(driver_data->psk);
                driver_data->ssid = ssid;
                driver_data->psk = psk;

                if (rtems_wifi_start(ssid, psk, associate) != 0) {
                    port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, ERROR_ATOM));
                    return;
                }
            } else {
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
                return;
            }
        }
    }

    term sntp_config = interop_kv_get_value_default(config, sntp_atom, term_invalid_term(), ctx->global);
    if (!term_is_invalid_term(sntp_config)) {
        driver_data->sntp_enabled = true;
        term host_term = interop_kv_get_value_default(sntp_config, host_atom, term_invalid_term(), ctx->global);
        if (!term_is_invalid_term(host_term)) {
            int ok = 0;
            char *host = interop_term_to_string(host_term, &ok);
            if (ok && host != NULL) {
                free(driver_data->sntp_host);
                driver_data->sntp_host = host;
            }
        }
        term port_term = interop_kv_get_value_default(sntp_config, port_atom, term_invalid_term(), ctx->global);
        if (!term_is_invalid_term(port_term)) {
            driver_data->sntp_port = term_to_int32(port_term);
        } else {
            driver_data->sntp_port = 123;
        }

        if (rtems_wifi_get_state() == RTEMS_WIFI_GOT_IP) {
            rtems_sntp_start(driver_data->sntp_host, driver_data->sntp_port, sntp_sync_callback, NULL);
        }
    }

    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void get_sta_rssi(Context *ctx, term pid, term ref)
{
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_size, 1, &ref, MEMORY_NO_SHRINK) != MEMORY_GC_OK)) {
        return;
    }

    int rssi;
    int error = rtems_wifi_get_rssi(&rssi);
    if (error != 0) {
        term reason;
        if (error == ENOTSUP) {
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "enotsup"));
        } else if (error == ENETDOWN) {
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "network_down"));
        } else {
            reason = term_from_int(error);
        }
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, reason));
        return;
    }
    term rssi_val = term_from_int(rssi);
    term reply = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, rssi_atom), rssi_val);
    port_send_reply(ctx, pid, ref, reply);
}

static NativeHandlerResult consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

#ifdef RTEMS_WIFI_BACKEND_ESP32
    if (term_is_tuple(msg) && term_get_tuple_arity(msg) == 2
        && globalcontext_is_term_equal_to_atom_string(ctx->global, term_get_tuple_element(msg, 0), ATOM_STR("\xE", "wifi_scan_done"))) {
        NativeHandlerResult result = finish_scan(ctx, term_to_int(term_get_tuple_element(msg, 1)));
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return result;
    }
#endif

    if (UNLIKELY(!term_is_tuple(msg) || term_get_tuple_arity(msg) != 3)) {
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
        return NativeContinue;
    }

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    term cmd_term = term_invalid_term();
    term config = term_invalid_term();

    if (term_is_atom(cmd)) {
        cmd_term = cmd;
    } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
        cmd_term = term_get_tuple_element(cmd, 0);
        config = term_get_tuple_element(cmd, 1);
    }

    enum network_cmd command = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
    switch (command) {
        case NetworkStartCmd:
            start_network(ctx, pid, ref, config, false);
            break;

        case NetworkConnectCmd:
            start_network(ctx, pid, ref, config, true);
            break;

        case NetworkHaltStaCmd:
            rtems_wifi_stop();
            port_send_reply(ctx, pid, ref, OK_ATOM);
            break;

        case NetworkRssiCmd:
            get_sta_rssi(ctx, pid, ref);
            break;

        case NetworkScanCmd:
            start_scan(ctx, pid, ref, config);
            break;

        case NetworkCancelScanCmd:
            cancel_scan(ctx, pid, ref, config);
            break;

        case NetworkStopCmd:
            rtems_sntp_stop();
            driver_data->owner_process_id = 0;
            driver_data->sntp_enabled = false;
#ifdef RTEMS_WIFI_BACKEND_ESP32
            if (((struct ScanData *) ctx->platform_data)->active) {
                ((struct ScanData *) ctx->platform_data)->stopping = true;
                if (!((struct ScanData *) ctx->platform_data)->canceled) {
                    esp_wifi_scan_stop();
                }
                break;
            }
#endif
            rtems_wifi_stop();
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            return NativeTerminate;

        default:
            port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
            break;
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    return NativeContinue;
}

void network_driver_init(GlobalContext *global)
{
    if (driver_data == NULL) {
        driver_data = calloc(1, sizeof(struct NetworkDriverData));
        driver_data->global = global;
#ifdef RTEMS_WIFI_BACKEND_ESP32
        atomic_init(&driver_data->scan_port_id, 0);
        atomic_init(&driver_data->scan_done_received, false);
#endif
        rtems_wifi_set_event_callback(wifi_event_callback, NULL);
    }
}

void network_driver_destroy(GlobalContext *global)
{
    (void) global;
    if (driver_data != NULL) {
#ifdef RTEMS_WIFI_BACKEND_ESP32
        if (driver_data->scan_handler_registered) {
            esp_event_handler_unregister(WIFI_EVENT, WIFI_EVENT_SCAN_DONE, scan_done_callback);
        }
#endif
        rtems_sntp_stop();
        rtems_wifi_stop();
        free(driver_data->ssid);
        free(driver_data->psk);
        free(driver_data->sntp_host);
        free(driver_data);
        driver_data = NULL;
    }
}

Context *network_driver_create_port(GlobalContext *global, term opts)
{
    (void) opts;
    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = NULL;
#ifdef RTEMS_WIFI_BACKEND_ESP32
    ctx->platform_data = calloc(1, sizeof(struct ScanData));
    if (ctx->platform_data == NULL) {
        context_destroy(ctx);
        return NULL;
    }
#endif
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_destroy, network_driver_create_port)
