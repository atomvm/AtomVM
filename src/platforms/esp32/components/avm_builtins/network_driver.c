/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
 * Copyright 2020-2022 Fred Dushin <fred@dushin.net>
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

#include <atom.h>
#include <context.h>
#include <debug.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <platform_defaultatoms.h>
#include <port.h>
#include <sdkconfig.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE 1
#include <trace.h>

#include <esp32_sys.h>
#include <esp_log.h>
#include <esp_wifi.h>
#include <lwip/inet.h>

#include <string.h>

#if ESP_IDF_VERSION_MAJOR < 4
#include <apps/sntp/sntp.h>
#else
#include <lwip/apps/sntp.h>
#endif

#if ESP_IDF_VERSION_MAJOR >= 4
#define TCPIP_HOSTNAME_MAX_SIZE 255
#endif

#define TAG "network_driver"
#define PORT_REPLY_SIZE TUPLE_SIZE(2) + REF_SIZE

static const char *const ap_atom = ATOM_STR("\x2", "ap");
static const char *const ap_sta_connected_atom = ATOM_STR("\x10", "ap_sta_connected");
static const char *const ap_sta_disconnected_atom = ATOM_STR("\x13", "ap_sta_disconnected");
static const char *const ap_sta_ip_assigned_atom = ATOM_STR("\x12", "ap_sta_ip_assigned");
static const char *const ap_started_atom = ATOM_STR("\xA", "ap_started");
static const char *const dhcp_hostname_atom = ATOM_STR("\xD", "dhcp_hostname");
static const char *const host_atom = ATOM_STR("\x4", "host");
static const char *const max_connections_atom = ATOM_STR("\xF", "max_connections");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const sntp_atom = ATOM_STR("\x4", "sntp");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const ssid_hidden_atom = ATOM_STR("\xB", "ssid_hidden");
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

struct ClientData
{
    GlobalContext *global;
    uint32_t port_process_id;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
};

static inline term make_atom(GlobalContext *global, AtomString atom_str)
{
    return globalcontext_make_atom(global, atom_str);
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

// WARNING: this function assumes space has been allocated for the reference
// and that the context has been safely acquired under a lock
static void send_term(Context *ctx, struct ClientData *data, term t)
{
    term ref = term_from_ref_ticks(data->ref_ticks, ctx);
    term msg = term_alloc_tuple(2, ctx);
    term_put_tuple_element(msg, 0, ref);
    term_put_tuple_element(msg, 1, t);

    // Pid ! {Ref, T}
    port_send_message_nolock(ctx->global, term_from_local_process_id(data->owner_process_id), msg);
}

static void send_got_ip(struct ClientData *data, tcpip_adapter_ip_info_t *info)
{
    TRACE("Sending got_ip back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        // {Ref, {sta_got_ip, {{192, 168, 1, 2}, {255, 255, 255, 0}, {192, 168, 1, 1}}}}
        size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) * 3;
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for sta_got_ip; no reply sent");
        } else {
            term ip = tuple_from_addr(ctx, ntohl(info->ip.addr));
            term netmask = tuple_from_addr(ctx, ntohl(info->netmask.addr));
            term gw = tuple_from_addr(ctx, ntohl(info->gw.addr));

            term ip_info = port_create_tuple3(ctx, ip, netmask, gw);
            term reply = port_create_tuple2(ctx, make_atom(ctx->global, sta_got_ip_atom), ip_info);
            send_term(ctx, data, reply);
        }
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_sta_connected(struct ClientData *data)
{
    TRACE("Sending sta_connected back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        // {Ref, sta_connected}
        size_t heap_size = PORT_REPLY_SIZE;
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for sta_connected; no message sent");
        } else {
            send_term(ctx, data, make_atom(ctx->global, sta_connected_atom));
        }
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_sta_disconnected(struct ClientData *data)
{
    TRACE("Sending sta_disconnected back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        // {Ref, sta_disconnected}
        size_t heap_size = PORT_REPLY_SIZE;
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for sta_disconnected; no message sent");
        } else {
            send_term(ctx, data, make_atom(ctx->global, sta_disconnected_atom));
        }
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_ap_started(struct ClientData *data)
{
    TRACE("Sending ap_start back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        // {Ref, ap_started}
        size_t heap_size = PORT_REPLY_SIZE;
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for ap_started; no message sent");
        } else {
            send_term(ctx, data, make_atom(ctx->global, ap_started_atom));
        }
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_atom_mac(Context *ctx, struct ClientData *data, term atom, uint8_t *mac)
{
    // {Ref, {ap_connected | ap_disconnected, <<1,2,3,4,5,6>>}}
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + term_binary_heap_size(6);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate heap space for ap_connected or ap_disconnected; no message sent");
    } else {
        term mac_term = term_from_literal_binary(mac, 6, ctx);
        term reply = port_create_tuple2(ctx, atom, mac_term);
        send_term(ctx, data, reply);
    }
}

static void send_ap_sta_connected(struct ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_connected back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        send_atom_mac(ctx, data, make_atom(ctx->global, ap_sta_connected_atom), mac);
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_ap_sta_disconnected(struct ClientData *data, uint8_t *mac)
{
    TRACE("Sending ap_sta_disconnected back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        send_atom_mac(ctx, data, make_atom(ctx->global, ap_sta_disconnected_atom), mac);
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

static void send_ap_sta_ip_assigned(struct ClientData *data, esp_ip4_addr_t *ip)
{
    TRACE("Sending ap_sta_ip_assigned back to AtomVM\n");

    Context *ctx = globalcontext_get_process_lock(data->global, data->port_process_id);
    {
        // {Ref, {ap_sta_ip_assigned, {192, 168, 4, 2}}}
        size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(4);
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate heap space for ap_sta_ip_assigned; no message sent");
        } else {
            term ip_term = tuple_from_addr(ctx, ntohl(ip->addr));
            term reply = port_create_tuple2(ctx, make_atom(ctx->global, ap_sta_ip_assigned_atom), ip_term);
            send_term(ctx, data, reply);
        }
    }
    globalcontext_get_process_unlock(data->global, ctx);
}

#define UNLIKELY_NOT_ESP_OK(E) UNLIKELY((E) != ESP_OK)

//
// Event Handler
//

static void event_handler(void *arg, esp_event_base_t event_base, int32_t event_id, void *event_data)
{
    struct ClientData *data = (struct ClientData *) arg;

    if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_STA_START) {

        ESP_LOGI(TAG, "WIFI_EVENT_STA_START received.");
        esp_wifi_connect();

    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_STA_CONNECTED) {

        ESP_LOGI(TAG, "WIFI_EVENT_STA_CONNECTED received.");
        send_sta_connected(data);

    } else if (event_base == IP_EVENT && event_id == IP_EVENT_STA_GOT_IP) {

        ip_event_got_ip_t *event = (ip_event_got_ip_t *) event_data;
        ESP_LOGI(TAG, "IP_EVENT_STA_GOT_IP: %s", inet_ntoa(event->ip_info.ip));
        send_got_ip(data, (tcpip_adapter_ip_info_t *) &event->ip_info.ip);

    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_STA_DISCONNECTED) {

        ESP_LOGI(TAG, "WIFI_EVENT_STA_DISCONNECTED received.");
        esp_wifi_connect();
        send_sta_disconnected(data);

    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_AP_START) {

        ESP_LOGI(TAG, "WIFI_EVENT_AP_START received.");
        send_ap_started(data);

    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_AP_STACONNECTED) {

        ESP_LOGI(TAG, "WIFI_EVENT_AP_STACONNECTED received.");
        wifi_event_ap_staconnected_t *event = (wifi_event_ap_staconnected_t *) event_base;
        send_ap_sta_connected(data, event->mac);

    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_AP_STADISCONNECTED) {

        ESP_LOGI(TAG, "WIFI_EVENT_AP_STADISCONNECTED received.");
        wifi_event_ap_stadisconnected_t *event = (wifi_event_ap_stadisconnected_t *) event_base;
        send_ap_sta_disconnected(data, event->mac);

    } else if (event_base == IP_EVENT && event_id == IP_EVENT_AP_STAIPASSIGNED) {

        ip_event_ap_staipassigned_t *event = (ip_event_ap_staipassigned_t *) event_data;
        ESP_LOGI(TAG, "IP_EVENT_AP_STAIPASSIGNED: %s", inet_ntoa(event->ip));
        send_ap_sta_ip_assigned(data, (esp_ip4_addr_t *) &event->ip);

    } else {
        ESP_LOGI(TAG, "Unhandled wifi event: %i.", event_id);
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

    //
    // Check parameters
    //
    if (term_is_invalid_term(ssid_term)) {
        ESP_LOGE(TAG, "get_sta_wifi_config: Missing SSID");
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
    if (UNLIKELY(strlen(psk) > sizeof(wifi_config->sta.password))) {
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

static char *get_default_device_name()
{
    uint8_t mac[6];
    esp_efuse_mac_get_default(mac);

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
            ESP_LOGE(TAG, "get_ap_wifi_config: AP PSK must be length 8 or more");
            return NULL;
        }
        if (!ok || IS_NULL_PTR(psk)) {
            free(ssid);
            ESP_LOGE(TAG, "get_ap_wifi_config: Invalid PSK");
            return NULL;
        }
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

static void maybe_set_sntp(term sntp_config, GlobalContext *global)
{
    if (!term_is_invalid_term(sntp_config) && !term_is_invalid_term(interop_kv_get_value(sntp_config, host_atom, global))) {
        int ok;
        char *host = interop_term_to_string(interop_kv_get_value(sntp_config, host_atom, global), &ok);
        if (LIKELY(ok)) {
            // do not free(sntp)
            sntp_setoperatingmode(SNTP_OPMODE_POLL);
            sntp_setservername(0, host);
            sntp_init();
            ESP_LOGI(TAG, "SNTP initialized with host set to %s", host);
        } else {
            ESP_LOGE(TAG, "Unable to locate sntp host in configuration");
        }
    }
}

static void set_dhcp_hostname(term dhcp_hostname_term)
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
        char *tmp = get_default_device_name();
        strncpy(dhcp_hostname, tmp, TCPIP_HOSTNAME_MAX_SIZE);
        free(tmp);
    }
    esp_err_t status = tcpip_adapter_set_hostname(WIFI_IF_STA, dhcp_hostname);
    if (status == ESP_OK) {
        ESP_LOGI(TAG, "DHCP hostname set to %s", dhcp_hostname);
    } else {
        ESP_LOGW(TAG, "Unable to set DHCP hostname to %s.  status=%d", dhcp_hostname, status);
    }
}

static void start_network(Context *ctx, term pid, term ref, term config)
{
    TRACE("start_network");

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

    wifi_config_t *sta_wifi_config = get_sta_wifi_config(sta_config, ctx->global);
    wifi_config_t *ap_wifi_config = get_ap_wifi_config(ap_config, ctx->global);
    if (UNLIKELY(IS_NULL_PTR(sta_wifi_config) && IS_NULL_PTR(ap_wifi_config))) {
        ESP_LOGE(TAG, "Unable to get STA or AP configuration");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    struct ClientData *data = malloc(sizeof(struct ClientData));
    if (IS_NULL_PTR(data)) {
        ESP_LOGE(TAG, "Failed to allocate ClientData");
        term error = port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    data->global = ctx->global;
    data->port_process_id = ctx->process_id;
    data->owner_process_id = term_to_local_process_id(pid);
    data->ref_ticks = term_to_ref_ticks(ref);

    esp_err_t err;

    if (sta_wifi_config != NULL) {
        esp_netif_create_default_wifi_sta();
    }
    if (ap_wifi_config != NULL) {
        esp_netif_create_default_wifi_ap();
    }

    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    if (UNLIKELY_NOT_ESP_OK(err = esp_wifi_init(&cfg))) {
        ESP_LOGE(TAG, "Failed to initialize ESP WiFi");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    if (UNLIKELY((err = esp_wifi_set_storage(WIFI_STORAGE_RAM)) != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to set ESP WiFi storage");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    if ((err = esp_event_handler_register(WIFI_EVENT, ESP_EVENT_ANY_ID, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register wifi event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    if ((err = esp_event_handler_register(IP_EVENT, IP_EVENT_STA_GOT_IP, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register got_ip event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    if ((err = esp_event_handler_register(IP_EVENT, IP_EVENT_AP_STAIPASSIGNED, &event_handler, data)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to register staipassigned event handler");
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

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
    if ((err = esp_wifi_set_mode(wifi_mode)) != ESP_OK) {
        ESP_LOGE(TAG, "Error setting wifi mode %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    } else {
        ESP_LOGI(TAG, "WIFI mode set to %d", wifi_mode);
    }

    //
    // Set up STA mode, if configured
    //
    if (!IS_NULL_PTR(sta_wifi_config)) {
        if ((err = esp_wifi_set_config(ESP_IF_WIFI_STA, sta_wifi_config)) != ESP_OK) {
            ESP_LOGE(TAG, "Error setting STA mode config %d", err);
            free(sta_wifi_config);
            if (!IS_NULL_PTR(ap_wifi_config)) {
                free(ap_wifi_config);
            }
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
            return;
        } else {
            ESP_LOGI(TAG, "STA mode configured");
            free(sta_wifi_config);
        }
    }

    //
    // Set up AP mode, if configured
    //
    if (!IS_NULL_PTR(ap_wifi_config)) {
        if ((err = esp_wifi_set_config(ESP_IF_WIFI_AP, ap_wifi_config)) != ESP_OK) {
            ESP_LOGE(TAG, "Error setting AP mode config %d", err);
            free(ap_wifi_config);
            free(sta_wifi_config);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
            return;
        } else {
            ESP_LOGI(TAG, "AP mode configured");
            free(ap_wifi_config);
        }
    }
    if ((err = esp_wifi_start()) != ESP_OK) {
        ESP_LOGE(TAG, "Error in esp_wifi_start %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        free(ap_wifi_config);
        free(sta_wifi_config);
        return;
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
        set_dhcp_hostname(interop_kv_get_value(sta_config, dhcp_hostname_atom, ctx->global));
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
        ESP_LOGW(TAG, "Invalid message.  Ignoring.");
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

    mailbox_remove(&ctx->mailbox);

    return NativeContinue;
}

//
// Entrypoints
//

void network_driver_init(GlobalContext *global)
{
    esp_err_t err;

    if ((err = esp_netif_init()) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to initialize network interface");
        AVM_ABORT();
    } else {
        ESP_LOGI(TAG, "Initialized network interface");
    }
    if ((err = esp_event_loop_create_default()) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to create default event loop");
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
    ctx->platform_data = NULL;
    return ctx;
}

#ifdef CONFIG_AVM_ENABLE_NETWORK_PORT_DRIVER

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_create_port)

#endif
