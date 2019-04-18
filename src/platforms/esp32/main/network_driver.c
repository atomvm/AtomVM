/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
 *   Copyright 2018 by Fred Dushin <fred@dushin.net>                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "network_driver.h"
#include "port.h"

#include <string.h>

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "utils.h"
#include "socket.h"
#include "term.h"

#include <esp_log.h>
#include <esp_event_loop.h>
#include <esp_log.h>
#include <esp_wifi.h>

#include <freertos/event_groups.h>

#include <lwip/inet.h>

#include <apps/sntp/sntp.h>

//#define ENABLE_TRACE 1

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

#define CONNECTED_BIT BIT0

static esp_err_t wifi_event_handler(void *ctx, system_event_t *event);

static const char *const sta_a = "\x3" "sta";
static const char *const ssid_a = "\x4" "ssid";
static const char *const psk_a = "\x3" "psk";
static const char *const sntp_a = "\x4" "sntp";
static const char *const sta_got_ip_a = "\xA" "sta_got_ip";
static const char *const sta_connected_a = "\xD" "sta_connected";
static const char *const sta_disconnected_a = "\x10" "sta_disconnected";

static EventGroupHandle_t wifi_event_group;

typedef struct ClientData {
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
} ClientData;

void network_driver_start(Context *ctx, term_ref pid, term_ref ref, term config)
{
    TRACE("network_driver_start");

    term sta_config = interop_proplist_get_value(config, context_make_atom(ctx, sta_a));
    if (!term_is_nil(sta_config)) {
        term ssid_value = interop_proplist_get_value(sta_config, context_make_atom(ctx, ssid_a));
        term pass_value = interop_proplist_get_value(sta_config, context_make_atom(ctx, psk_a));
        term sntp_value = interop_proplist_get_value(sta_config, context_make_atom(ctx, sntp_a));

        char *ssid = interop_list_to_string(ssid_value);
        char *psk = interop_list_to_string(pass_value);

        if (UNLIKELY(!ssid || !psk)) {
            if (ssid != NULL) {
                free(ssid);
            }
            if (psk != NULL) {
                free(psk);
            }
            term reply = port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
            port_send_reply(ctx, pid, ref, reply);
            return;
        }
        TRACE("ssid: %s psk: xxxxxxxx\n", ssid);

        ClientData *data = (ClientData *) malloc(sizeof(ClientData));
        if (UNLIKELY(data == NULL)) {
            fprintf(stderr, "malloc %s:%d", __FILE__, __LINE__);
            abort();
        }
        data->ctx = ctx;
        data->pid = pid;
        data->ref_ticks = term_to_ref_ticks(ref);

        wifi_event_group = xEventGroupCreate();
        ESP_ERROR_CHECK(esp_event_loop_init(wifi_event_handler, data));
        TRACE("Initialized event loop.\n");
        wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
        ESP_ERROR_CHECK(esp_wifi_init(&cfg));
        ESP_ERROR_CHECK(esp_wifi_set_storage(WIFI_STORAGE_RAM));

        wifi_config_t wifi_config;
        if (UNLIKELY((strlen(ssid) > sizeof(wifi_config.sta.ssid)) || (strlen(psk) > sizeof(wifi_config.sta.password)))) {
            TRACE("ssid or psk is too long\n");
            free(ssid);
            free(psk);
            term reply = port_create_error_tuple(ctx, BADARG_ATOM);
            port_send_reply(ctx, pid, ref, reply);
            return;
        }

        memset(&wifi_config, 0, sizeof(wifi_config_t));
        strcpy((char *) wifi_config.sta.ssid, ssid);
        strcpy((char *) wifi_config.sta.password, psk);

        free(ssid);
        free(psk);

        ESP_ERROR_CHECK(esp_wifi_set_mode(WIFI_MODE_STA));
        ESP_ERROR_CHECK(esp_wifi_set_config(ESP_IF_WIFI_STA, &wifi_config));
        ESP_LOGI("NETWORK", "starting wifi: SSID: [%s], password: [XXXXXXXX].", wifi_config.sta.ssid);
        ESP_ERROR_CHECK(esp_wifi_start());

        if (sntp_value != term_nil()) {
            char *sntp = interop_list_to_string(sntp_value);
            if (sntp) {
                sntp_setoperatingmode(SNTP_OPMODE_POLL);
                sntp_setservername(0, sntp);
                sntp_init();
            }
        }
        port_send_reply(ctx, pid, ref, OK_ATOM);
    } else {
        term reply = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, reply);
    }
}

term network_driver_ifconfig(Context *ctx)
{
    return port_create_error_tuple(ctx, UNDEFINED_ATOM);
}

static u32_t get_ipv4_addr(ip4_addr_t *addr)
{
    return addr->addr;
}

static void send_got_ip(ClientData *data, tcpip_adapter_ip_info_t *info)
{
    TRACE("Sending got_ip back to AtomVM\n");
    Context *ctx = data->ctx;

    port_ensure_available(ctx, ((4 + 1) * 3 + (2 + 1) + (2 + 1))*2);

    term pid = data->pid;
    term ref = term_from_ref_ticks(data->ref_ticks, ctx);

    term ip = socket_tuple_from_addr(ctx, ntohl(get_ipv4_addr(&info->ip)));
    term netmask = socket_tuple_from_addr(ctx, ntohl(get_ipv4_addr(&info->netmask)));
    term gw = socket_tuple_from_addr(ctx, ntohl(get_ipv4_addr(&info->gw)));

    term ip_info = port_create_tuple3(ctx, ip, netmask, gw);
    term reply = port_create_tuple2(ctx, context_make_atom(ctx, sta_got_ip_a), ip_info);
    port_send_reply(ctx, pid, ref, reply);
}

static void send_atom(ClientData *data, AtomString atom)
{
    Context *ctx = data->ctx;
    port_ensure_available(ctx, 6);

    term pid = data->pid;
    term ref = term_from_ref_ticks(data->ref_ticks, ctx);
    // Pid ! {Ref, Atom}
    port_send_reply(ctx, pid, ref, context_make_atom(ctx, atom));
}

static void send_sta_connected(ClientData *data)
{
    TRACE("Sending sta_connected back to AtomVM\n");
    send_atom(data, sta_connected_a);
}

static void send_sta_disconnected(ClientData *data)
{
    TRACE("Sending sta_disconnected back to AtomVM\n");
    send_atom(data, sta_disconnected_a);
}

static esp_err_t wifi_event_handler(void *ctx, system_event_t *event)
{
    ClientData *data = (ClientData *) ctx;
    switch (event->event_id) {
        case SYSTEM_EVENT_STA_START:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_START received.");
            esp_wifi_connect();
            break;

        case SYSTEM_EVENT_STA_GOT_IP:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_GOT_IP: %s", inet_ntoa(event->event_info.got_ip.ip_info.ip));
            xEventGroupSetBits(wifi_event_group, CONNECTED_BIT);
            send_got_ip(data, &event->event_info.got_ip.ip_info);
            break;

        case SYSTEM_EVENT_STA_CONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_CONNECTED received.");
            send_sta_connected(data);
            break;

        case SYSTEM_EVENT_STA_DISCONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_DISCONNECTED received.");
            esp_wifi_connect();
            xEventGroupClearBits(wifi_event_group, CONNECTED_BIT);
            send_sta_disconnected(data);
            break;

        default:
            ESP_LOGI("NETWORK", "Unhandled wifi event: %i.", event->event_id);
            break;
    }

    return ESP_OK;
}
