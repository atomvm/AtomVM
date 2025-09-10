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

#include "rp2_sys.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <cyw43.h>
#include <dhserver.h>
#if PICO_RP2040
#include <hardware/rtc.h>
#endif
#include <lwip/apps/sntp.h>
#include <pico/cyw43_arch.h>
#include <string.h>

#pragma GCC diagnostic pop

#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)
#define DEFAULT_HOSTNAME_FMT "atomvm-%02x%02x%02x%02x%02x%02x"
#define DEFAULT_HOSTNAME_SIZE (strlen("atomvm-") + 12 + 1)

static const char *const ap_atom = ATOM_STR("\x2", "ap");
static const char *const ap_channel_atom = ATOM_STR("\xA", "ap_channel");
static const char *const ap_sta_connected_atom = ATOM_STR("\x10", "ap_sta_connected");
static const char *const ap_sta_disconnected_atom = ATOM_STR("\x13", "ap_sta_disconnected");
static const char *const ap_started_atom = ATOM_STR("\xA", "ap_started");
static const char *const dhcp_hostname_atom = ATOM_STR("\xD", "dhcp_hostname");
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
    NetworkStartCmd,
    NetworkRssiCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    { ATOM_STR("\x4", "rssi"), NetworkRssiCmd },
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
    char *hostname;
    char *ap_hostname;
    queue_t queue;
};

enum NetworkDriverEventType
{
    NetworkDriverEventTypeCyw43Assoc,
    NetworkDriverEventTypeSTADisconnected,
    NetworkDriverEventTypeSTAConnected,
    NetworkDriverEventTypeGotIP,
};

struct NetworkDriverEvent
{
    enum NetworkDriverEventType type;
    // Union of parameters
    union
    {
        struct netif *netif;
    };
};

enum DriverErrorCodeType
{
    DriverOK,
    DriverBADARG,
    DriverMACError,
    DriverOOM
};

// Callbacks do not allow for user data
// netif->state is actually pointing to &cyw43_state
static struct NetworkDriverData *driver_data;

static void network_driver_netif_status_cb(struct netif *netif);
static void network_driver_cyw43_assoc_cb(bool assoc);

static void network_driver_do_cyw43_assoc(GlobalContext *glb);

static term tuple_from_addr(Heap *heap, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_heap_create_tuple_n(heap, 4, terms);
}

static term error_code_to_term(int error, GlobalContext *global)
{
    switch (error) {
        case DriverOK:
            return OK_ATOM;
            break;
        case DriverBADARG:
            return BADARG_ATOM;
            break;
        case DriverMACError:
            return globalcontext_make_atom(global, ATOM_STR("\x10", "device_mac_error"));
            break;
        case DriverOOM:
            return OUT_OF_MEMORY_ATOM;
            break;
        default:
            return BADARG_ATOM;
    }
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

static void send_sta_connected(GlobalContext *glb)
{
    // {Ref, sta_connected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(glb, sta_connected_atom));
    }
    END_WITH_STACK_HEAP(heap, glb);
}

static void send_sta_disconnected(GlobalContext *glb)
{
    // {Ref, sta_disconnected}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(glb, sta_disconnected_atom));
    }
    END_WITH_STACK_HEAP(heap, glb);
}

static void send_got_ip(struct netif *netif, GlobalContext *glb)
{
    // {Ref, {sta_got_ip, {{192, 168, 1, 2}, {255, 255, 255, 0}, {192, 168, 1, 1}}}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3, heap);
    {
        term ip = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_addr(netif))));
        term netmask = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_netmask(netif))));
        term gw = tuple_from_addr(&heap, ntohl(ip4_addr_get_u32(netif_ip4_gw(netif))));

        term ip_info = port_heap_create_tuple3(&heap, ip, netmask, gw);
        term reply = port_heap_create_tuple2(&heap, globalcontext_make_atom(glb, sta_got_ip_atom), ip_info);
        send_term(&heap, reply);
    }
    END_WITH_STACK_HEAP(heap, glb);
}

static void send_ap_started(GlobalContext *glb)
{
    // {Ref, ap_started}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
    {
        send_term(&heap, globalcontext_make_atom(glb, ap_started_atom));
    }
    END_WITH_STACK_HEAP(heap, glb);
}

static void send_atom_mac(term atom, uint8_t *mac, GlobalContext *glb)
{
    // {Ref, {ap_connected | ap_disconnected, <<1,2,3,4,5,6>>}}
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + TERM_BINARY_HEAP_SIZE(6), heap);
    {
        term mac_term = term_from_literal_binary(mac, 6, &heap, driver_data->global);
        term reply = port_heap_create_tuple2(&heap, atom, mac_term);
        send_term(&heap, reply);
    }
    END_WITH_STACK_HEAP(heap, glb);
}

static void send_ap_sta_connected(uint8_t *mac, GlobalContext *glb)
{
    send_atom_mac(globalcontext_make_atom(glb, ap_sta_connected_atom), mac, glb);
}

static void send_ap_sta_disconnected(uint8_t *mac, GlobalContext *glb)
{
    send_atom_mac(globalcontext_make_atom(glb, ap_sta_disconnected_atom), mac, glb);
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

static enum DriverErrorCodeType write_default_device_name(size_t size, char **out)
{
    uint8_t mac[6];
    // Device name is used for AP mode ssid (if undefined), and for the
    // default dhcp_hostname on both interfaces.  It seems the interface
    // parameter is ignored and both interfaces have the same MAC address.
    int err = cyw43_wifi_get_mac(&cyw43_state, CYW43_ITF_STA, mac);
    if (UNLIKELY(err)) {
        return DriverMACError;
    }
    *out = malloc(size);
    if (IS_NULL_PTR(out)) {
        return DriverOOM;
    }
    snprintf(*out, size,
        DEFAULT_HOSTNAME_FMT, mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    return DriverOK;
}

static enum DriverErrorCodeType set_interface_dhcp_name(term dhcp_name, char **out)
{
    if (term_is_invalid_term(dhcp_name)) {
        enum DriverErrorCodeType ok_ret = write_default_device_name(DEFAULT_HOSTNAME_SIZE, out);
        if (UNLIKELY(ok_ret != DriverOK)) {
            free(out);
            cyw43_arch_disable_sta_mode();
            return ok_ret;
        }
    } else {
        int ok = 0;
        *out = interop_term_to_string(dhcp_name, &ok);
        if (!ok || IS_NULL_PTR(out)) {
            if (out != NULL) {
                free(out);
                cyw43_arch_disable_sta_mode();
                return DriverBADARG;
            }
            cyw43_arch_disable_sta_mode();
            return DriverOOM;
        }
    }
    return DriverOK;
}

static term start_sta(term sta_config, GlobalContext *global)
{
    term ssid_term = interop_kv_get_value(sta_config, ssid_atom, global);
    term pass_term = interop_kv_get_value(sta_config, psk_atom, global);
    term hostname_term = interop_kv_get_value(sta_config, dhcp_hostname_atom, global);

    //
    // Check parameters
    //
    if (UNLIKELY(term_is_invalid_term(ssid_term))) {
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
        if (UNLIKELY(!ok)) {
            free(ssid);
            return BADARG_ATOM;
        }
    }

    enum DriverErrorCodeType ret = set_interface_dhcp_name(hostname_term, &driver_data->hostname);
    if (UNLIKELY(ret != DriverOK)) {
        free(ssid);
        free(psk);
        return error_code_to_term(ret, global);
    }

    netif_set_hostname(&cyw43_state.netif[CYW43_ITF_STA], driver_data->hostname);

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
        free(driver_data->hostname);
        cyw43_arch_disable_sta_mode();
        return BADARG_ATOM;
    }

    return OK_ATOM;
}

static void network_driver_do_cyw43_assoc(GlobalContext *glb)
{
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
            send_ap_sta_connected(&new_macs[6 * i], glb);
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
            send_ap_sta_disconnected(&driver_data->stas_mac[6 * j], glb);
        }
    }
    free(driver_data->stas_mac);
    new_macs = realloc(new_macs, 6 * nb_stas);
    driver_data->stas_mac = new_macs;
    driver_data->stas_count = nb_stas;
}

static void network_driver_cyw43_assoc_cb(bool assoc)
{
    UNUSED(assoc);

    struct NetworkDriverEvent event;
    event.type = NetworkDriverEventTypeCyw43Assoc;
    sys_try_post_listener_event_from_isr(driver_data->global, &driver_data->queue, &event);
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
    term channel_term = interop_kv_get_value(ap_config, ap_channel_atom, global);
    term hostname_term = interop_kv_get_value(ap_config, dhcp_hostname_atom, global);

    //
    // Check parameters
    //
    char *ssid = NULL;
    if (term_is_invalid_term(ssid_term)) {
        enum DriverErrorCodeType ret = write_default_device_name(DEFAULT_HOSTNAME_SIZE, &ssid);
        if (UNLIKELY(ret != DriverOK)) {
            free(ssid);
            return error_code_to_term(ret, global);
        }
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
        if (UNLIKELY(strlen(psk) < 8)) {
            free(ssid);
            return BADARG_ATOM;
        }
        if (UNLIKELY(!ok)) {
            free(ssid);
            return BADARG_ATOM;
        }
    }
    uint32_t channel = 0;
    if (!term_is_invalid_term(channel_term)) {
        channel = term_to_int32(channel_term);
        if (channel != 0) {
            cyw43_wifi_ap_set_channel(&cyw43_state, channel);
        }
    }

    enum DriverErrorCodeType ret = set_interface_dhcp_name(hostname_term, &driver_data->ap_hostname);
    if (UNLIKELY(ret != DriverOK)) {
        free(ssid);
        free(psk);
        return error_code_to_term(ret, global);
    }

    uint32_t auth = (psk == NULL) ? CYW43_AUTH_OPEN : CYW43_AUTH_WPA2_AES_PSK;
    cyw43_state.assoc_cb = network_driver_cyw43_assoc_cb;
    cyw43_arch_enable_ap_mode(ssid, psk, auth);
    // Set hostname after enabling AP mode otherwise hostname will revert to "PicoW"
    netif_set_hostname(&cyw43_state.netif[CYW43_ITF_AP], driver_data->ap_hostname);
    send_ap_started(global);
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
#if PICO_RP2040
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
#endif
    // On Pico 2W, this is not required as mtime is set with settimeofday
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
            struct NetworkDriverEvent event;
            event.type = NetworkDriverEventTypeSTADisconnected;
            sys_try_post_listener_event_from_isr(driver_data->global, &driver_data->queue, &event);
        } else if (link_status == CYW43_LINK_JOIN) {
            struct NetworkDriverEvent event;
            event.type = NetworkDriverEventTypeSTAConnected;
            sys_try_post_listener_event_from_isr(driver_data->global, &driver_data->queue, &event);
        } else if (link_status == CYW43_LINK_UP) {
            struct NetworkDriverEvent event;
            event.type = NetworkDriverEventTypeGotIP;
            event.netif = netif;
            sys_try_post_listener_event_from_isr(driver_data->global, &driver_data->queue, &event);
        }
    }
}

static EventListener *network_events_handler(GlobalContext *glb, EventListener *listener)
{
    struct NetworkDriverEvent event;
    while (queue_try_remove(listener->queue, &event)) {
        switch (event.type) {
            case NetworkDriverEventTypeCyw43Assoc:
                network_driver_do_cyw43_assoc(glb);
                break;
            case NetworkDriverEventTypeSTADisconnected:
                send_sta_disconnected(glb);
                break;
            case NetworkDriverEventTypeSTAConnected:
                send_sta_connected(glb);
                break;
            case NetworkDriverEventTypeGotIP:
                send_got_ip(event.netif, glb);
                break;
        }
    }
    return listener;
}

static void init_driver_data(GlobalContext *glb)
{
    driver_data = malloc(sizeof(struct NetworkDriverData));
    driver_data->sntp_hostname = NULL;
    driver_data->stas_mac = NULL;
    driver_data->dhcp_config = NULL;
    driver_data->global = glb;
    queue_init(&driver_data->queue, sizeof(struct NetworkDriverEvent), EVENT_QUEUE_LEN);

    EventListener *network_listener = malloc(sizeof(EventListener));

    network_listener->handler = network_events_handler;
    network_listener->queue = &driver_data->queue;
    sys_register_listener(glb, network_listener);
}

static void start_network(Context *ctx, term pid, term ref, term config)
{
    // {Ref, ok | {error, atom() | integer()}}
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        return;
    }

    if (driver_data == NULL) {
        init_driver_data(ctx->global);
    }
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

    // Always enable sta mode so the bus is initialized and we get a MAC
    // address. This is done before configuring the interface because the
    // MAC is added to the default hostname, and default ssid in ap mode.
    // (i.e. atomvm-0123456789ab)
    cyw43_arch_enable_sta_mode();
    if (!term_is_invalid_term(sta_config)) {
        term result_atom = start_sta(sta_config, ctx->global);
        if (result_atom != OK_ATOM) {
            term error = port_create_error_tuple(ctx, result_atom);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
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

static void get_sta_rssi(Context *ctx, term pid, term ref)
{
    size_t tuple_reply_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);

    int32_t sta_rssi = 0;
    int err = cyw43_wifi_get_rssi(&cyw43_state, &sta_rssi);
    if (UNLIKELY(err != 0)) {
        // Reply: {Ref, {error, Reason}}
        port_ensure_available(ctx, tuple_reply_size);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    term rssi = term_from_int32(sta_rssi);
    // Reply: {Ref, {rssi, Value}}
    port_ensure_available(ctx, tuple_reply_size);
    term reply = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "rssi")), rssi);
    port_send_reply(ctx, pid, ref, reply);
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
                start_network(ctx, pid, ref, config);
                break;
            }
            case NetworkRssiCmd: {
                get_sta_rssi(ctx, pid, ref);
                break;
            }

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
    if (driver_data) {
        free(driver_data->hostname);
        if (driver_data->ap_hostname) {
            free(driver_data->ap_hostname);
        }
        free(driver_data->sntp_hostname);
        free(driver_data->stas_mac);
        if (driver_data->dhcp_config) {
            dhserv_free();
        }
        free(driver_data->dhcp_config);
        sys_unregister_listener_from_event(global, &driver_data->queue);
        queue_free(&driver_data->queue);
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
