/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
 * Copyright 2020 Fred Dushin <fred@dushin.net>
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

#include "port.h"

#include <string.h>

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "port.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"

#include "platform_defaultatoms.h"

#include <esp_event_loop.h>
#include <esp_log.h>
#include <esp_wifi.h>
#include <tcpip_adapter.h>

#include <freertos/event_groups.h>

#include <lwip/inet.h>

#if ESP_IDF_VERSION_MAJOR < 4
#include <apps/sntp/sntp.h>
#endif

#if ESP_IDF_VERSION_MAJOR >= 4
#define TCPIP_HOSTNAME_MAX_SIZE 255
#endif

//#define ENABLE_TRACE 1

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
    #else
        #define TRACE(...)
    #endif
#endif

#define CONNECTED_BIT BIT0

static void network_driver_init(GlobalContext *global);

static void network_driver_start(Context *ctx, term pid, term ref, term config);
static term network_driver_ifconfig(Context *ctx);

static const char *const start_a = "\x5" "start";
static const char *const ifconfig_a = "\x8" "ifconfig";

static NativeHandlerResult network_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

    if (port_is_standard_port_command(msg)) {
        port_ensure_available(ctx, 32);

        term pid = term_get_tuple_element(msg, 0);
        term ref = term_get_tuple_element(msg, 1);
        term cmd = term_get_tuple_element(msg, 2);

        if (term_is_atom(cmd) && cmd == globalcontext_make_atom(ctx->global, ifconfig_a)) {
            term reply = network_driver_ifconfig(ctx);
            port_send_reply(ctx, pid, ref, reply);
        } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
            term cmd_name = term_get_tuple_element(cmd, 0);
            term config = term_get_tuple_element(cmd, 1);
            if (cmd_name == globalcontext_make_atom(ctx->global, start_a)) {
                network_driver_start(ctx, pid, ref, config);
            } else {
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
            }
        } else {
            port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        }
    } else {
        fprintf(stderr, "WARNING: Invalid port command.  Unable to send reply");
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return NativeContinue;
}

// TODO Move to new event handler APIs when we move to IDF SDK 4.x or later
static esp_err_t wifi_event_handler(void *ctx, system_event_t *event);

static EventGroupHandle_t wifi_event_group;

typedef struct ClientData
{
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
} ClientData;

static wifi_config_t *get_sta_wifi_config(term sta_config)
{
    if (term_is_invalid_term(sta_config)) {
        TRACE("No STA config\n");
        return NULL;
    }
    term ssid_term = interop_proplist_get_value(sta_config, SSID_ATOM);
    term pass_term = interop_proplist_get_value(sta_config, PSK_ATOM);
    //
    // Check parameters
    //
    if (term_is_nil(ssid_term)) {
        fprintf(stderr, "get_sta_wifi_config: Missing SSID\n");
        return NULL;
    }
    int ok = 0;
    char *ssid = interop_term_to_string(ssid_term, &ok);
    if (!ok || IS_NULL_PTR(ssid)) {
        fprintf(stderr, "get_sta_wifi_config: Invalid SSID\n");
        return NULL;
    }
    char *psk = NULL;
    if (term_is_nil(pass_term)) {
        fprintf(stderr, "Warning: Attempting to connect to open network\n");
    } else {
        psk = interop_term_to_string(pass_term, &ok);
        if (!ok || IS_NULL_PTR(psk)) {
            free(ssid);
            fprintf(stderr, "get_sta_wifi_config: Invalid PSK\n");
            return NULL;
        }
    }
    //
    // Allocate wifi_config and check variable sizes
    //
    wifi_config_t *wifi_config = malloc(sizeof(wifi_config_t));
    if (IS_NULL_PTR(wifi_config)) {
        fprintf(stderr, "Failed to allocate wifi_config_t %s:%d\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    if (UNLIKELY(strlen(ssid) > sizeof(wifi_config->sta.ssid))) {
        fprintf(stderr, "ssid cannot be more than %d characters\n", sizeof(wifi_config->sta.ssid));
        free(ssid);
        free(psk);
        return NULL;
    }
    if (UNLIKELY(strlen(psk) > sizeof(wifi_config->sta.password))) {
        fprintf(stderr, "psk cannot be more than %d characters\n", sizeof(wifi_config->sta.password));
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
    ESP_LOGI("NETWORK", "STA ssid: %s", wifi_config->sta.ssid);
    return wifi_config;
}

static char *get_default_device_name()
{
    uint8_t mac[6];
    esp_efuse_mac_get_default(mac);

    size_t buf_size = strlen("atomvm-") + 12 + 1;
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate buf %s:%d\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    snprintf(buf, buf_size,
        "atomvm-%02x%02x%02x%02x%02x%02x", mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    return buf;
}

static wifi_config_t *get_ap_wifi_config(term ap_config)
{
    if (term_is_invalid_term(ap_config)) {
        TRACE("No AP config\n");
        return NULL;
    }
    term ssid_term = interop_proplist_get_value(ap_config, SSID_ATOM);
    term pass_term = interop_proplist_get_value(ap_config, PSK_ATOM);
    //
    // Check parameters
    //
    char *ssid = NULL;
    if (term_is_nil(ssid_term)) {
        ssid = get_default_device_name();
    } else {
        int ok = 0;
        ssid = interop_term_to_string(ssid_term, &ok);
        if (!ok || IS_NULL_PTR(ssid)) {
            fprintf(stderr, "get_ap_wifi_config: Invalid SSID\n");
            return NULL;
        }
    }
    char *psk = NULL;
    if (term_is_nil(pass_term)) {
        ESP_LOGW("NETWORK", "Warning: Empty password.  AP will be open network!");
    } else {
        int ok = 0;
        psk = interop_term_to_string(pass_term, &ok);
        if (strlen(psk) < 8) {
            fprintf(stderr, "get_ap_wifi_config: AP PSK must be length 8 or more\n");
            return NULL;
        }
        if (!ok || IS_NULL_PTR(psk)) {
            free(ssid);
            fprintf(stderr, "get_ap_wifi_config: Invalid PSK\n");
            return NULL;
        }
    }
    //
    // Allocate wifi_config and check variable sizes
    //
    wifi_config_t *wifi_config = malloc(sizeof(wifi_config_t));
    if (IS_NULL_PTR(wifi_config)) {
        fprintf(stderr, "Failed to allocate wifi_config_t %s:%d\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    if (UNLIKELY(strlen(ssid) > sizeof(wifi_config->ap.ssid))) {
        fprintf(stderr, "ssid cannot be more than %d characters\n", sizeof(wifi_config->ap.ssid));
        free(ssid);
        free(psk);
        return NULL;
    }
    if (!IS_NULL_PTR(psk) && UNLIKELY(strlen(psk) > sizeof(wifi_config->ap.password))) {
        fprintf(stderr, "psk cannot be more than %d characters\n", sizeof(wifi_config->ap.password));
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
    wifi_config->ap.authmode = IS_NULL_PTR(psk) ? WIFI_AUTH_OPEN : WIFI_AUTH_WPA_WPA2_PSK;
    term ssid_hidden_term = interop_proplist_get_value(ap_config, SSID_HIDDEN_ATOM);
    wifi_config->ap.ssid_hidden = term_is_nil(ssid_hidden_term) ? 0 : ssid_hidden_term == TRUE_ATOM;
    term max_connections_term = interop_proplist_get_value(ap_config, MAX_CONNECTIONS_ATOM);
    wifi_config->ap.max_connection = term_is_nil(max_connections_term) ? 4 : term_to_int(max_connections_term);
    //
    // done
    //
    ESP_LOGI("NETWORK", "AP ssid: %s", wifi_config->ap.ssid);
    ESP_LOGI("NETWORK", "AP authmode: %d", wifi_config->ap.authmode);
    ESP_LOGI("NETWORK", "AP ssid_hidden: %d", wifi_config->ap.ssid_hidden);
    ESP_LOGI("NETWORK", "AP max_connection: %d", wifi_config->ap.max_connection);
    return wifi_config;
}

static void maybe_set_sntp(term sntp_config)
{
#if ESP_IDF_VERSION_MAJOR < 4
    if (!term_is_nil(sntp_config) && !term_is_nil(interop_proplist_get_value(sntp_config, HOST_ATOM))) {
        int ok;
        char *host = interop_term_to_string(interop_proplist_get_value(sntp_config, HOST_ATOM), &ok);
        if (LIKELY(ok)) {
            // do not free(sntp)
            sntp_setoperatingmode(SNTP_OPMODE_POLL);
            sntp_setservername(0, host);
            sntp_init();
            ESP_LOGI("NETWORK", "SNTP initialized with host set to %s", host);
        } else {
            fprintf(stderr, "Unable to locate sntp host in configuration\n");
        }
    }
#else
    fprintf(stderr, "SNTP not yet supported on esp-idf 4.x\n");
#endif
}

static void set_dhcp_hostname(term dhcp_hostname_term)
{
    char dhcp_hostname[TCPIP_HOSTNAME_MAX_SIZE + 1];
    if (!term_is_nil(dhcp_hostname_term)) {
        int ok = 0;
        char *tmp = interop_term_to_string(dhcp_hostname_term, &ok);
        if (!ok || IS_NULL_PTR(tmp)) {
            fprintf(stderr, "WARNING: dhcp_hostname is not a valid string value\n");
            return;
        } else {
            strncpy(dhcp_hostname, tmp, TCPIP_HOSTNAME_MAX_SIZE);
            dhcp_hostname[TCPIP_HOSTNAME_MAX_SIZE] = '\0';
            free(tmp);
        }
    } else {
        char *tmp = get_default_device_name();
        strncpy(dhcp_hostname, tmp, TCPIP_HOSTNAME_MAX_SIZE);
        free(tmp);
    }
    esp_err_t status = tcpip_adapter_set_hostname(WIFI_IF_STA, dhcp_hostname);
    if (status == ESP_OK) {
        ESP_LOGI("NETWORK", "DHCP hostname set to %s", dhcp_hostname);
    } else {
        ESP_LOGW("NETWORK", "Unable to set DHCP hostname to %s.  status=%d", dhcp_hostname, status);
    }
}

static void network_driver_start(Context *ctx, term pid, term ref, term config)
{
    TRACE("network_driver_start");
    //
    // Get the STA and AP config, if set
    //
    term sta_config = interop_proplist_get_value_default(config, STA_ATOM, term_invalid_term());
    term ap_config = interop_proplist_get_value_default(config, AP_ATOM, term_invalid_term());
    if (term_is_invalid_term(sta_config) && term_is_invalid_term(ap_config)) {
        fprintf(stderr, "Expected STA or AP configuration but got neither.\n");
        term reply = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, reply);
        return;
    }
    wifi_config_t *sta_wifi_config = get_sta_wifi_config(sta_config);
    wifi_config_t *ap_wifi_config = get_ap_wifi_config(ap_config);
    if (IS_NULL_PTR(sta_wifi_config) && IS_NULL_PTR(ap_wifi_config)) {
        fprintf(stderr, "Unable to get STA or AP configuration\n");
        term reply = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, reply);
        return;
    }
    //
    // Initialize event loop with client information, so that any
    // asynchronous responses can be delivered back to client mailbox
    //
    ClientData *data = (ClientData *) malloc(sizeof(ClientData));
    if (IS_NULL_PTR(data)) {
        fprintf(stderr, "Failed to allocate ClientData %s:%d\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    data->ctx = ctx;
    data->pid = pid;
    data->ref_ticks = term_to_ref_ticks(ref);
    // NB. wifi_event_group is a (static) global variable
    wifi_event_group = xEventGroupCreate();
    ESP_ERROR_CHECK(esp_event_loop_init(wifi_event_handler, data));
    TRACE("Initialized event loop.\n");
    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    ESP_ERROR_CHECK(esp_wifi_init(&cfg));
    ESP_ERROR_CHECK(esp_wifi_set_storage(WIFI_STORAGE_RAM));
    //
    // Set the wifi mode
    //
    wifi_mode_t wifi_mode = WIFI_MODE_NULL;
    if (!IS_NULL_PTR(sta_wifi_config) && !IS_NULL_PTR(ap_wifi_config)) {
        wifi_mode = WIFI_MODE_APSTA;
    } else if (!IS_NULL_PTR(sta_wifi_config)) {
        wifi_mode = WIFI_MODE_STA;
    } else {
        wifi_mode = WIFI_MODE_AP;
    }
    esp_err_t status;
    status = esp_wifi_set_mode(wifi_mode);
    if (status != ESP_OK) {
        fprintf(stderr, "Error setting wifi mode %d\n", status);
        term reply = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, reply);
        return;
    } else {
        ESP_LOGI("NETWORK", "WIFI mode set to %d", wifi_mode);
    }
    //
    // Set up STA mode, if configured
    //
    if (!IS_NULL_PTR(sta_wifi_config)) {
        status = esp_wifi_set_config(ESP_IF_WIFI_STA, sta_wifi_config);
        if (status != ESP_OK) {
            fprintf(stderr, "Error setting STA mode config %d\n", status);
            free(sta_wifi_config);
            if (!IS_NULL_PTR(ap_wifi_config)) {
                free(ap_wifi_config);
            }
            term reply = port_create_error_tuple(ctx, BADARG_ATOM);
            port_send_reply(ctx, pid, ref, reply);
            return;
        } else {
            ESP_LOGI("NETWORK", "STA mode configured");
            free(sta_wifi_config);
        }
    }
    //
    // Set up AP mode, if configured
    //
    if (!IS_NULL_PTR(ap_wifi_config)) {
        status = esp_wifi_set_config(ESP_IF_WIFI_AP, ap_wifi_config);
        if (status != ESP_OK) {
            fprintf(stderr, "Error setting AP mode config %d\n", status);
            free(ap_wifi_config);
            if (!IS_NULL_PTR(sta_wifi_config)) {
                free(sta_wifi_config);
            }
            term reply = port_create_error_tuple(ctx, BADARG_ATOM);
            port_send_reply(ctx, pid, ref, reply);
            return;
        } else {
            ESP_LOGI("NETWORK", "AP mode configured");
            free(ap_wifi_config);
        }
    }
    ESP_ERROR_CHECK(esp_wifi_start());
    //
    // Set up simple NTP, if configured
    //
    maybe_set_sntp(interop_proplist_get_value(config, SNTP_ATOM));
    //
    // Set the DHCP hostname, if STA mode is enabled
    //
    if (!IS_NULL_PTR(sta_wifi_config)) {
        set_dhcp_hostname(interop_proplist_get_value(sta_config, STA_DHCP_HOSTNAME_ATOM));
    }
    //
    // Done -- send an ok so the FSM can proceed
    //
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static term network_driver_ifconfig(Context *ctx)
{
    return port_create_error_tuple(ctx, UNDEFINED_ATOM);
}

static term tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

//
// Event handlers
//

static void send_term(ClientData *data, term t)
{
    Context *ctx = data->ctx;

    term pid = data->pid;
    term ref = term_from_ref_ticks(data->ref_ticks, &ctx->heap);
    // Pid ! {Ref, T}
    port_send_reply(ctx, pid, ref, t);
}

static void send_got_ip(ClientData *data, tcpip_adapter_ip_info_t *info)
{
    TRACE("Sending got_ip back to AtomVM\n");
    Context *ctx = data->ctx;

    port_ensure_available(ctx, ((4 + 1) * 3 + (2 + 1) + (2 + 1)) * 2);
    term ip = tuple_from_addr(ctx, ntohl(info->ip.addr));
    term netmask = tuple_from_addr(ctx, ntohl(info->netmask.addr));
    term gw = tuple_from_addr(ctx, ntohl(info->gw.addr));

    term ip_info = port_create_tuple3(ctx, ip, netmask, gw);
    term reply = port_create_tuple2(ctx, STA_GOT_IP_ATOM, ip_info);
    send_term(data, reply);
}

static void send_sta_connected(ClientData *data)
{
    TRACE("Sending sta_connected back to AtomVM\n");
    port_ensure_available(data->ctx, 6);
    send_term(data, STA_CONNECTED_ATOM);
}

static void send_sta_disconnected(ClientData *data)
{
    TRACE("Sending sta_disconnected back to AtomVM\n");
    port_ensure_available(data->ctx, 6);
    send_term(data, STA_DISCONNECTED_ATOM);
}

static void send_ap_started(ClientData *data)
{
    TRACE("Sending ap_start back to AtomVM\n");
    port_ensure_available(data->ctx, 6);
    send_term(data, AP_STARTED_ATOM);
}

static void send_atom_mac(ClientData *data, term atom, uint8_t *mac)
{
    port_ensure_available(data->ctx, term_binary_data_size_in_terms(6) + 12);
    term mac_term = term_from_literal_binary(mac, 6, &data->ctx->heap, data->ctx->global);
    term reply = port_create_tuple2(data->ctx, atom, mac_term);
    send_term(data, reply);
}

static void send_ap_sta_connected(ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_connected back to AtomVM\n");
    send_atom_mac(data, AP_STA_CONNECTED_ATOM, mac);
}

static void send_ap_sta_disconnected(ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_disconnected back to AtomVM\n");
    send_atom_mac(data, AP_STA_DISCONNECTED_ATOM, mac);
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
            send_got_ip(data, (tcpip_adapter_ip_info_t *) &event->event_info.got_ip.ip_info);
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

        case SYSTEM_EVENT_AP_START:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_AP_START received.");
            send_ap_started(data);
            break;

        case SYSTEM_EVENT_AP_STACONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_AP_STACONNECTED received.");
            send_ap_sta_connected(data, event->event_info.sta_connected.mac);
            break;

        case SYSTEM_EVENT_AP_STADISCONNECTED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_AP_STADISCONNECTED received.");
            send_ap_sta_disconnected(data, event->event_info.sta_connected.mac);
            break;

        case SYSTEM_EVENT_AP_STAIPASSIGNED:
            ESP_LOGI("NETWORK", "SYSTEM_EVENT_AP_STAIPASSIGNED received.");
            // TODO Enable when we move to IDF-SDK v3.3 or later
            // send_ap_sta_ip_acquired(data, &(event->event_info.ap_staipassigned.ip));
            break;

        default:
            ESP_LOGI("NETWORK", "Unhandled wifi event: %i.", event->event_id);
            break;
    }

    return ESP_OK;
}

void network_driver_init(GlobalContext *global)
{
    tcpip_adapter_init();
}

Context *network_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = network_consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_create_port)

#endif
