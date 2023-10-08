/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#ifdef LIB_PICO_CYW43_ARCH

#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <port.h>

#include "rp2040_sys.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <cyw43.h>
#include <dhserver.h>
#include <hardware/rtc.h>
#include <lwip/apps/sntp.h>
#include <pico/cyw43_arch.h>

#pragma GCC diagnostic pop

#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)

static const char *const ap_atom = ATOM_STR("\x2", "ap");
static const char *const ap_sta_connected_atom = ATOM_STR("\x10", "ap_sta_connected");
static const char *const ap_sta_disconnected_atom = ATOM_STR("\x13", "ap_sta_disconnected");
static const char *const ap_started_atom = ATOM_STR("\xA", "ap_started");
static const char *const host_atom = ATOM_STR("\x4", "host");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const sntp_atom = ATOM_STR("\x4", "sntp");
static const char *const sntp_sync_atom = ATOM_STR("\x9", "sntp_sync");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const sta_atom = ATOM_STR("\x3", "sta");
static const char *const sta_connected_atom = ATOM_STR("\xD", "sta_connected");
static const char *const sta_disconnected_atom = ATOM_STR("\x10", "sta_disconnected");
static const char *const sta_got_ip_atom = ATOM_STR("\xA", "sta_got_ip");

enum network_cmd
{
    NetworkInvalidCmd = 0,
    // TODO add support for scan, ifconfig
    NetworkStartCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    SELECT_INT_DEFAULT(NetworkInvalidCmd)
};

struct NetworkDriverData
{
    GlobalContext *global;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    int link_status;
    char *sntp_hostname;
    int stas_count;
    uint8_t *stas_mac;
    struct dhcp_config *dhcp_config;
};

// Callbacks do not allow for user data
// netif->state is actually pointing to &cyw43_state
static struct NetworkDriverData *driver_data;

static void network_driver_netif_status_cb(struct netif *netif);
static void network_driver_cyw43_assoc_cb(bool assoc);

static term tuple_from_addr(Heap *heap, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_heap_create_tuple_n(heap, 4, terms);
}

static void send_term(Heap *heap, term t)
{
    term ref = term_from_ref_ticks(driver_data->ref_ticks, heap);
    term msg = term_alloc_tuple(2, heap);
    term_put_tuple_element(msg, 0, ref);
    term_put_tuple_element(msg, 1, t);

    // Pid ! {Ref, T}
    port_send_message(driver_data->global, term_from_local_process_id(driver_data->owner_process_id), msg);
}

static void send_sta_connected()
{
    // {Ref, sta_connected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(driver_data->global, sta_connected_atom));
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static void send_sta_disconnected()
{
    // {Ref, sta_disconnected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(driver_data->global, sta_disconnected_atom));
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static void send_got_ip(struct netif *netif)
{
    // {Ref, {sta_got_ip, {{192, 168, 1, 2}, {255, 255, 255, 0}, {192, 168, 1, 1}}}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
    {
        term ip = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_addr(netif))));
        term netmask = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_netmask(netif))));
        term gw = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_gw(netif))));

        term ip_info = port_heap_create_tuple3(&heap, ip, netmask, gw);
        term reply = port_heap_create_tuple2(&heap, globalcontext_make_atom(driver_data->global, sta_got_ip_atom), ip_info);
        send_term(&heap, reply);
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static void send_ap_started()
{
    // {Ref, ap_started}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(driver_data->global, ap_started_atom));
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static void send_atom_mac(term atom, uint8_t *mac)
{
    // {Ref, {ap_connected | ap_disconnected, <<1,2,3,4,5,6>>}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(6), heap);
    {
        term mac_term = term_from_literal_binary(mac, 6, &heap, driver_data->global);
        term reply = port_heap_create_tuple2(&heap, atom, mac_term);
        send_term(&heap, reply);
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static void send_ap_sta_connected(uint8_t *mac)
{
    send_atom_mac(globalcontext_make_atom(driver_data->global, ap_sta_connected_atom), mac);
}

static void send_ap_sta_disconnected(uint8_t *mac)
{
    send_atom_mac(globalcontext_make_atom(driver_data->global, ap_sta_disconnected_atom), mac);
}

static void send_sntp_sync(struct timeval *tv)
{
    // {Ref, {sntp_sync, {TVSec, TVUsec}}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) * 2 + BOXED_INT64_SIZE * 2, heap);
    {
        term tv_tuple = port_heap_create_tuple2(&heap, term_make_maybe_boxed_int64(tv->tv_sec, &heap), term_make_maybe_boxed_int64(tv->tv_usec, &heap));
        term reply = port_heap_create_tuple2(&heap, globalcontext_make_atom(driver_data->global, sntp_sync_atom), tv_tuple);
        send_term(&heap, reply);
    }
    END_WITH_STACK_HEAP(heap, driver_data->global);
}

static term start_sta(term sta_config, GlobalContext *global)
{
    term ssid_term = interop_kv_get_value(sta_config, ssid_atom, global);
    term pass_term = interop_kv_get_value(sta_config, psk_atom, global);

    //
    // Check parameters
    //
    if (term_is_invalid_term(ssid_term)) {
        return BADARG_ATOM;
    }
    int ok = 0;
    char *ssid = interop_term_to_string(ssid_term, &ok);
    if (!ok || IS_NULL_PTR(ssid)) {
        return BADARG_ATOM;
    }
    char *psk = NULL;
    if (!term_is_invalid_term(pass_term)) {
        psk = interop_term_to_string(pass_term, &ok);
        if (!ok) {
            free(ssid);
            return BADARG_ATOM;
        }
    }

    cyw43_arch_enable_sta_mode();
    uint32_t auth = (psk == NULL) ? CYW43_AUTH_OPEN : CYW43_AUTH_WPA2_MIXED_PSK;
    int result = cyw43_arch_wifi_connect_async(ssid, psk, auth);
    // We need to set the callback after calling connect async because it's
    // erased by cyw43_arch_wifi_connect_async.
    // There could be a race condition here.
    netif_set_status_callback(&cyw43_state.netif[CYW43_ITF_STA], network_driver_netif_status_cb);
    network_driver_netif_status_cb(&cyw43_state.netif[CYW43_ITF_STA]);
    free(ssid);
    free(psk);
    if (result != 0) {
        return BADARG_ATOM;
    }

    return OK_ATOM;
}

static char *get_default_device_name()
{
    uint8_t mac[6];
    // Device name is used for AP mode. It seems the interface parameter is
    // ignored and both interfaces have the same MAC address.
    int err = cyw43_wifi_get_mac(&cyw43_state, CYW43_ITF_AP, mac);
    if (err) {
        return NULL;
    }

    size_t buf_size = strlen("atomvm-") + 12 + 1;
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        return NULL;
    }
    snprintf(buf, buf_size,
        "atomvm-%02x%02x%02x%02x%02x%02x", mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    return buf;
}

static void network_driver_cyw43_assoc_cb(bool assoc)
{
    UNUSED(assoc);

    int max_stas;
    cyw43_wifi_ap_get_max_stas(&cyw43_state, &max_stas);
    uint8_t *new_macs = malloc(6 * max_stas);
    int nb_stas;
    cyw43_wifi_ap_get_stas(&cyw43_state, &nb_stas, new_macs);
    // Determine new macs.
    for (int i = 0; i < nb_stas; i++) {
        bool new_mac = true;
        for (int j = 0; j < driver_data->stas_count; j++) {
            if (memcmp(&driver_data->stas_mac[6 * j], &new_macs[6 * i], 6) == 0) {
                new_mac = false;
                break;
            }
        }
        if (new_mac) {
            send_ap_sta_connected(&new_macs[6 * i]);
        }
    }
    // Determine old macs
    for (int j = 0; j < driver_data->stas_count; j++) {
        bool old_mac = true;
        for (int i = 0; i < nb_stas; i++) {
            if (memcmp(&driver_data->stas_mac[6 * j], &new_macs[6 * i], 6) == 0) {
                old_mac = false;
                break;
            }
        }
        if (old_mac) {
            send_ap_sta_disconnected(&driver_data->stas_mac[6 * j]);
        }
    }
    free(driver_data->stas_mac);
    new_macs = realloc(new_macs, 6 * nb_stas);
    driver_data->stas_mac = new_macs;
    driver_data->stas_count = nb_stas;
}

static term setup_dhcp_server()
{
    int max_stas;
    // Supposedly, max_stas doesn't change.
    cyw43_wifi_ap_get_max_stas(&cyw43_state, &max_stas);
    // max_stas is 10, but let's work for up to 253.
    // we do networking on a /24 and we reserve 0, 255 (broadcast) and our own address.
    if (max_stas > 253) {
        max_stas = 253;
    }

    size_t dhcp_config_size = sizeof(struct dhcp_config) + max_stas * sizeof(struct dhcp_entry);
    driver_data->dhcp_config = malloc(dhcp_config_size);
    bzero(driver_data->dhcp_config, dhcp_config_size);
    driver_data->dhcp_config->num_entry = max_stas;
    driver_data->dhcp_config->entries = (dhcp_entry_t *) ((uint8_t *) driver_data->dhcp_config + sizeof(struct dhcp_config));
    uint32_t ip_addr4 = ntohl(ip4_addr_get_u32(netif_ip4_addr(&cyw43_state.netif[CYW43_ITF_AP])));
    driver_data->dhcp_config->addr[0] = ip_addr4 >> 24;
    driver_data->dhcp_config->addr[1] = (ip_addr4 >> 16) & 0xFF;
    driver_data->dhcp_config->addr[2] = (ip_addr4 >> 8) & 0xFF;
    driver_data->dhcp_config->addr[3] = ip_addr4 & 0xFF;

    int self_last_ip_byte = ip_addr4 & 0xFF;
    int dhcp_client_addr = 0;

    for (int i = 0; i < max_stas; i++) {
        driver_data->dhcp_config->entries[i].addr[0] = ip_addr4 >> 24;
        driver_data->dhcp_config->entries[i].addr[1] = (ip_addr4 >> 16) & 0xFF;
        driver_data->dhcp_config->entries[i].addr[2] = (ip_addr4 >> 8) & 0xFF;
        dhcp_client_addr++;
        if (dhcp_client_addr == self_last_ip_byte) {
            dhcp_client_addr++;
        }
        driver_data->dhcp_config->entries[i].addr[3] = dhcp_client_addr;
        driver_data->dhcp_config->entries[i].subnet[0] = 255;
        driver_data->dhcp_config->entries[i].subnet[1] = 255;
        driver_data->dhcp_config->entries[i].subnet[2] = 255;
        driver_data->dhcp_config->entries[i].subnet[3] = 0;
        driver_data->dhcp_config->entries[i].lease = 86400;
    }

    // We don't have a DNS server yet but we can't route anything either.
    driver_data->dhcp_config->dns[0] = ip_addr4 >> 24;
    driver_data->dhcp_config->dns[1] = (ip_addr4 >> 16) & 0xFF;
    driver_data->dhcp_config->dns[2] = (ip_addr4 >> 8) & 0xFF;
    driver_data->dhcp_config->dns[3] = ip_addr4 & 0xFF;
    driver_data->dhcp_config->port = 67;

    err_t err = dhserv_init(driver_data->dhcp_config);
    if (err) {
        free(driver_data->dhcp_config);
        driver_data->dhcp_config = NULL;
        return BADARG_ATOM;
    }

    return OK_ATOM;
}

static term start_ap(term ap_config, GlobalContext *global)
{
    term ssid_term = interop_kv_get_value(ap_config, ssid_atom, global);
    term pass_term = interop_kv_get_value(ap_config, psk_atom, global);

    //
    // Check parameters
    //
    char *ssid = NULL;
    if (term_is_invalid_term(ssid_term)) {
        ssid = get_default_device_name();
    } else {
        int ok = 0;
        ssid = interop_term_to_string(ssid_term, &ok);
        if (!ok || IS_NULL_PTR(ssid)) {
            return BADARG_ATOM;
        }
    }
    char *psk = NULL;
    if (!term_is_invalid_term(pass_term)) {
        int ok = 0;
        psk = interop_term_to_string(pass_term, &ok);
        if (strlen(psk) < 8) {
            free(ssid);
            return BADARG_ATOM;
        }
        if (!ok) {
            free(ssid);
            return BADARG_ATOM;
        }
    }

    uint32_t auth = (psk == NULL) ? CYW43_AUTH_OPEN : CYW43_AUTH_WPA2_AES_PSK;
    cyw43_state.assoc_cb = network_driver_cyw43_assoc_cb;
    cyw43_arch_enable_ap_mode(ssid, psk, auth);
    send_ap_started();
    free(ssid);
    free(psk);

    // We need to start dhcp server after tcp/ip is setup on AP.
    // There can be a race condition here, but clients will retry resending DHCP Requests
    return setup_dhcp_server();
}

void sntp_set_system_time_us(unsigned long sec, unsigned long usec)
{
    struct timeval tv;
    tv.tv_sec = sec;
    tv.tv_usec = usec;
    settimeofday(&tv, NULL);

    send_sntp_sync(&tv);

    // We also set RTC time.
    if (UNLIKELY(!rtc_running())) {
        rtc_init();
    }
    gettimeofday(&tv, NULL);
    struct tm utc;
    gmtime_r(&tv.tv_sec, &utc);
    datetime_t pico_datetime;
    pico_datetime.year = utc.tm_year + 1900;
    pico_datetime.month = utc.tm_mon + 1;
    pico_datetime.day = utc.tm_mday;
    pico_datetime.dotw = 0;
    pico_datetime.hour = utc.tm_hour;
    pico_datetime.min = utc.tm_min;
    pico_datetime.sec = utc.tm_sec;
    rtc_set_datetime(&pico_datetime);
}

static void setup_sntp(term sntp_config, GlobalContext *global)
{
    if (!term_is_invalid_term(interop_kv_get_value(sntp_config, host_atom, global))) {
        int ok;
        driver_data->sntp_hostname = interop_term_to_string(interop_kv_get_value(sntp_config, host_atom, global), &ok);
        if (LIKELY(ok)) {
            sntp_setoperatingmode(SNTP_OPMODE_POLL);
            sntp_setservername(0, driver_data->sntp_hostname);
            sntp_init();
        } else {
            free(driver_data->sntp_hostname);
        }
    } else {
        sntp_setoperatingmode(SNTP_OPMODE_POLL);
        sntp_servermode_dhcp(1);
        sntp_init();
    }
}

static void network_driver_netif_status_cb(struct netif *netif)
{
    // We don't really need to lock to call cyw43_tcpip_link_status
    // However, we take advantage of this lock to protect driver_data->link_status.
    cyw43_arch_lwip_begin();
    int link_status = cyw43_tcpip_link_status(&cyw43_state, CYW43_ITF_STA);
    int previous_link_status = driver_data->link_status;
    driver_data->link_status = link_status;
    cyw43_arch_lwip_end();
    if (link_status != previous_link_status) {
        if (link_status == CYW43_LINK_DOWN) {
            send_sta_disconnected();
        } else if (link_status == CYW43_LINK_JOIN) {
            send_sta_connected();
        } else if (link_status == CYW43_LINK_UP) {
            send_got_ip(netif);
        }
    }
}

static void start_network(Context *ctx, term pid, term ref, term config)
{
    // {Ref, ok | {error, atom() | integer()}}
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        return;
    }

    if (driver_data == NULL) {
        driver_data = malloc(sizeof(struct NetworkDriverData));
        driver_data->sntp_hostname = NULL;
        driver_data->stas_mac = NULL;
        driver_data->dhcp_config = NULL;
    }
    driver_data->global = ctx->global;
    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);
    driver_data->link_status = CYW43_LINK_DOWN;
    free(driver_data->sntp_hostname);
    driver_data->sntp_hostname = NULL;
    free(driver_data->stas_mac);
    free(driver_data->dhcp_config);
    driver_data->stas_count = 0;
    driver_data->stas_mac = NULL;

    //
    // Get the STA and AP config, if set
    //
    term sta_config = interop_kv_get_value_default(config, sta_atom, term_invalid_term(), ctx->global);
    term ap_config = interop_kv_get_value_default(config, ap_atom, term_invalid_term(), ctx->global);
    if (UNLIKELY(term_is_invalid_term(sta_config) && term_is_invalid_term(ap_config))) {
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    if (!term_is_invalid_term(sta_config)) {
        term result_atom = start_sta(sta_config, ctx->global);
        if (result_atom != OK_ATOM) {
            term error = port_create_error_tuple(ctx, result_atom);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    } else {
        // Always enable sta mode so the bus is initialized and we get a MAC
        // address.
        cyw43_arch_enable_sta_mode();
    }

    if (!term_is_invalid_term(ap_config)) {
        term result_atom = start_ap(ap_config, ctx->global);
        if (result_atom != OK_ATOM) {
            term error = port_create_error_tuple(ctx, result_atom);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
        if (!sta_config) {
            // We can disable sta mode now.
            cyw43_arch_disable_sta_mode();
        }
    } else {
        cyw43_arch_disable_ap_mode();
    }

    term sntp_config = interop_kv_get_value_default(config, sntp_atom, term_invalid_term(), ctx->global);
    if (!term_is_invalid_term(sntp_config)) {
        setup_sntp(sntp_config, ctx->global);
    }

    //
    // Done -- send an ok so the FSM can proceed
    //
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static NativeHandlerResult consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

    if (UNLIKELY(!term_is_tuple(msg) || term_get_tuple_arity(msg) != 3)) {
        return NativeContinue;
    }

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {

        term cmd_term = term_get_tuple_element(cmd, 0);
        term config = term_get_tuple_element(cmd, 1);

        enum network_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {
            case NetworkStartCmd:
                start_network(ctx, pid, ref, config);
                break;

            default: {
                // {Ref, {error, badarg}}
                size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
                if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                    return NativeContinue;
                }
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
            }
        }
    } else {
        // {Ref, {error, badarg}}
        size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            return NativeContinue;
        }
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return NativeContinue;
}

//
// Entrypoints
//

void network_driver_init(GlobalContext *global)
{
    UNUSED(global);

    driver_data = NULL;
}

void network_driver_destroy(GlobalContext *global)
{
    UNUSED(global);

    if (driver_data) {
        free(driver_data->sntp_hostname);
        free(driver_data->stas_mac);
        if (driver_data->dhcp_config) {
            dhserv_free();
        }
        free(driver_data->dhcp_config);
    }
    free(driver_data);
    driver_data = NULL;
}

Context *network_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_destroy, network_driver_create_port)

#endif
