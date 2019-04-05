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
#include "term.h"

#include <esp_log.h>
#include <esp_event_loop.h>
#include <esp_log.h>
#include <esp_wifi.h>

#include <freertos/event_groups.h>

#include <lwip/inet.h>

#include <apps/sntp/sntp.h>

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

#define CONNECTED_BIT BIT0

static esp_err_t wifi_event_handler(void *ctx, system_event_t *event);

static const char *const sta_a  = "\x3" "sta";
static const char *const ssid_a = "\x4" "ssid";
static const char *const psk_a  = "\x3" "psk";
static const char *const sntp_a = "\x4" "sntp";

static EventGroupHandle_t wifi_event_group;


void network_driver_setup(Context *ctx, term_ref pid, term_ref ref, term config)
{
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

        wifi_event_group = xEventGroupCreate();
        ESP_ERROR_CHECK(esp_event_loop_init(wifi_event_handler, NULL));
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
    return port_create_error_tuple(ctx, "unimplemented");
}

static esp_err_t wifi_event_handler(void *ctx, system_event_t *event)
{
    switch (event->event_id) {
        case SYSTEM_EVENT_STA_START:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_START received.");
            esp_wifi_connect();
            break;

        case SYSTEM_EVENT_STA_GOT_IP:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_GOT_IP received");
            ESP_LOGI("NETWORK", "IP: %s", inet_ntoa(event->event_info.got_ip.ip_info.ip));
            xEventGroupSetBits(wifi_event_group, CONNECTED_BIT);
            break;

        case SYSTEM_EVENT_STA_CONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_CONNECTED received.");
            break;

        case SYSTEM_EVENT_STA_DISCONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_STA_DISCONNECTED received.");
            esp_wifi_connect();
            xEventGroupClearBits(wifi_event_group, CONNECTED_BIT);
            break;

        default:
            ESP_LOGI("NETWORK", "Unhandled wifi event: %i.", event->event_id);
            break;
    }

    return ESP_OK;
}
