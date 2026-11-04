/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
 *
 * The Erlang API follows harmon25/atomvm_espnow (Apache-2.0).
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
#ifdef CONFIG_AVM_ENABLE_ESPNOW

#include <string.h>

#include <esp_idf_version.h>
#include <esp_log.h>
#include <esp_now.h>
#include <esp_wifi.h>
#include <freertos/FreeRTOS.h>
#include <freertos/queue.h>

#include "atom.h"
#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "port.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"
#include "sys.h"

#include <network_wifi.h>

#define TAG "espnow_driver"

// Events wait here between the Wi-Fi task and the scheduler. A fixed queue of
// fixed-size items, so a burst costs no allocation: when it is full the packet
// is dropped and counted (`stats'), never queued in more memory.
#define ESPNOW_QUEUE_LEN 8
#define MAC_LEN 6
// {Ref, Reply}
#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + TERM_BOXED_REFERENCE_SHORT_SIZE)

enum EspNowEventKind
{
    EspNowReceived,
    EspNowSent
};

struct EspNowEvent
{
    uint8_t kind;
    uint8_t mac[MAC_LEN];
    int8_t rssi;
    uint8_t ok;
    uint8_t len;
    uint8_t data[ESP_NOW_MAX_DATA_LEN];
};

struct EspNowData
{
    QueueHandle_t queue;
    EventListener listener;
    GlobalContext *global;
    int32_t receiver;
    uint8_t channel;
    volatile uint32_t dropped;
};

// ESP-NOW is one per chip: at most one open port.
static struct EspNowData *espnow_data;

static NativeHandlerResult espnow_consume_mailbox(Context *ctx);

static const uint8_t broadcast_mac[MAC_LEN] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };

enum espnow_cmd
{
    EspNowInvalidCmd = 0,
    EspNowCloseCmd,
    EspNowAddPeerCmd,
    EspNowDelPeerCmd,
    EspNowSendCmd,
    EspNowSetChannelCmd,
    EspNowGetChannelCmd,
    EspNowStatsCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "close"), EspNowCloseCmd },
    { ATOM_STR("\x8", "add_peer"), EspNowAddPeerCmd },
    { ATOM_STR("\x8", "del_peer"), EspNowDelPeerCmd },
    { ATOM_STR("\x4", "send"), EspNowSendCmd },
    { ATOM_STR("\xB", "set_channel"), EspNowSetChannelCmd },
    { ATOM_STR("\xB", "get_channel"), EspNowGetChannelCmd },
    { ATOM_STR("\x5", "stats"), EspNowStatsCmd },
    SELECT_INT_DEFAULT(EspNowInvalidCmd)
};

static void queue_event(const struct EspNowEvent *event)
{
    struct EspNowData *data = espnow_data;
    if (UNLIKELY(data == NULL)) {
        return;
    }
    if (xQueueSend(data->queue, event, 0) != pdTRUE) {
        data->dropped++;
    }
}

// Wi-Fi task.
static void espnow_recv_cb(const esp_now_recv_info_t *info, const uint8_t *payload, int len)
{
    struct EspNowEvent event = { .kind = EspNowReceived };
    memcpy(event.mac, info->src_addr, MAC_LEN);
    event.rssi = info->rx_ctrl->rssi;
    event.len = len < ESP_NOW_MAX_DATA_LEN ? len : ESP_NOW_MAX_DATA_LEN;
    memcpy(event.data, payload, event.len);
    queue_event(&event);
}

// Wi-Fi task. ESP-IDF 5.5 hands the send result with the frame's info, older
// versions with the destination address.
#if ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 5, 0)
static void espnow_send_cb(const esp_now_send_info_t *tx_info, esp_now_send_status_t status)
{
    const uint8_t *mac = tx_info->des_addr;
#else
static void espnow_send_cb(const uint8_t *mac, esp_now_send_status_t status)
{
#endif
    struct EspNowEvent event = { .kind = EspNowSent, .ok = status == ESP_NOW_SEND_SUCCESS };
    if (mac != NULL) {
        memcpy(event.mac, mac, MAC_LEN);
    }
    queue_event(&event);
}

// The network driver brought the radio back as a bare STA (network:stop/0):
// the channel is ours again.
static void espnow_wifi_restarted(void)
{
    struct EspNowData *data = espnow_data;
    if (data != NULL && data->channel != 0) {
        esp_wifi_set_channel(data->channel, WIFI_SECOND_CHAN_NONE);
    }
}

// Scheduler: one event to the receiver, as
// {espnow, rx, Mac, Data, #{rssi => Rssi}} or {espnow, sent, Mac, ok | fail}.
static EventListener *espnow_event_callback(GlobalContext *glb, EventListener *listener)
{
    struct EspNowData *data = GET_LIST_ENTRY(listener, struct EspNowData, listener);
    struct EspNowEvent event;
    if (xQueueReceive(data->queue, &event, 0) != pdTRUE) {
        return listener;
    }

    bool received = event.kind == EspNowReceived;
    size_t size = term_binary_heap_size(MAC_LEN)
        + (received ? TUPLE_SIZE(5) + term_binary_heap_size(event.len) + term_map_size_in_terms(1) : TUPLE_SIZE(4));
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "No memory for an event, dropped");
        data->dropped++;
        return listener;
    }

    term mac = term_from_literal_binary(event.mac, MAC_LEN, &heap, glb);
    term espnow = globalcontext_make_atom(glb, ATOM_STR("\x6", "espnow"));
    term message;
    if (received) {
        term map = term_alloc_map(1, &heap);
        term_set_map_assoc(map, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "rssi")), term_from_int(event.rssi));
        message = term_alloc_tuple(5, &heap);
        term_put_tuple_element(message, 0, espnow);
        term_put_tuple_element(message, 1, globalcontext_make_atom(glb, ATOM_STR("\x2", "rx")));
        term_put_tuple_element(message, 2, mac);
        term_put_tuple_element(message, 3, term_from_literal_binary(event.data, event.len, &heap, glb));
        term_put_tuple_element(message, 4, map);
    } else {
        message = term_alloc_tuple(4, &heap);
        term_put_tuple_element(message, 0, espnow);
        term_put_tuple_element(message, 1, globalcontext_make_atom(glb, ATOM_STR("\x4", "sent")));
        term_put_tuple_element(message, 2, mac);
        term_put_tuple_element(message, 3, event.ok ? OK_ATOM : globalcontext_make_atom(glb, ATOM_STR("\x4", "fail")));
    }
    globalcontext_send_message(glb, data->receiver, message);
    memory_destroy_heap(&heap, glb);

    return listener;
}

// A 6-byte MAC binary, or `broadcast'.
static bool get_mac(term t, GlobalContext *glb, uint8_t mac[MAC_LEN])
{
    if (t == globalcontext_make_atom(glb, ATOM_STR("\x9", "broadcast"))) {
        memcpy(mac, broadcast_mac, MAC_LEN);
        return true;
    }
    if (!term_is_binary(t) || term_binary_size(t) != MAC_LEN) {
        return false;
    }
    memcpy(mac, term_binary_data(t), MAC_LEN);
    return true;
}

// 0 (follow the current channel) or 1..14; -1 if neither.
static int get_channel(term t)
{
    if (!term_is_integer(t)) {
        return -1;
    }
    avm_int_t channel = term_to_int(t);
    return channel >= 0 && channel <= 14 ? channel : -1;
}

static void teardown(struct EspNowData *data)
{
    esp_now_unregister_recv_cb();
    esp_now_unregister_send_cb();
    esp_now_deinit();
    espnow_data = NULL;
    sys_unregister_listener(data->global, &data->listener);
    xQueueRemoveFromSet(data->queue, event_set);
    vQueueDelete(data->queue);
    network_wifi_release();
    free(data);
}

static esp_err_t add_peer(const uint8_t mac[MAC_LEN], uint8_t channel, const uint8_t *lmk)
{
    esp_now_peer_info_t peer = { .channel = channel, .ifidx = WIFI_IF_STA, .encrypt = lmk != NULL };
    memcpy(peer.peer_addr, mac, MAC_LEN);
    if (lmk != NULL) {
        memcpy(peer.lmk, lmk, ESP_NOW_KEY_LEN);
    }
    return esp_now_add_peer(&peer);
}

static Context *espnow_create_port(GlobalContext *global, term opts)
{
    if (espnow_data != NULL) {
        ESP_LOGE(TAG, "ESP-NOW is already open");
        return NULL;
    }

    term receiver = interop_kv_get_value_default(opts, ATOM_STR("\x8", "receiver"), term_invalid_term(), global);
    term channel_term = interop_kv_get_value_default(opts, ATOM_STR("\x7", "channel"), term_from_int(0), global);
    term pmk = interop_kv_get_value_default(opts, ATOM_STR("\x3", "pmk"), term_invalid_term(), global);
    int channel = get_channel(channel_term);
    if (!term_is_local_pid(receiver) || channel < 0
        || (!term_is_invalid_term(pmk) && (!term_is_binary(pmk) || term_binary_size(pmk) != ESP_NOW_KEY_LEN))) {
        ESP_LOGE(TAG, "Invalid options");
        return NULL;
    }

    struct EspNowData *data = malloc(sizeof(struct EspNowData));
    if (IS_NULL_PTR(data)) {
        return NULL;
    }
    data->global = global;
    data->receiver = term_to_local_process_id(receiver);
    data->channel = channel;
    data->dropped = 0;
    data->queue = xQueueCreate(ESPNOW_QUEUE_LEN, sizeof(struct EspNowEvent));
    if (IS_NULL_PTR(data->queue)) {
        free(data);
        return NULL;
    }

    esp_err_t err = network_wifi_hold(espnow_wifi_restarted);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "No Wi-Fi radio for ESP-NOW: %s", esp_err_to_name(err));
        vQueueDelete(data->queue);
        free(data);
        return NULL;
    }
    // On a connected STA the channel is the access point's: set_channel fails
    // and ESP-NOW follows it.
    if (channel != 0) {
        esp_wifi_set_channel(channel, WIFI_SECOND_CHAN_NONE);
    }

    espnow_data = data;
    data->listener.handler = espnow_event_callback;
    data->listener.sender = data->queue;
    sys_register_listener(global, &data->listener);
    if (xQueueAddToSet(data->queue, event_set) != pdPASS) {
        ESP_LOGE(TAG, "Failed to add the ESP-NOW queue to the event set");
        sys_unregister_listener(global, &data->listener);
        espnow_data = NULL;
        vQueueDelete(data->queue);
        network_wifi_release();
        free(data);
        return NULL;
    }

    if ((err = esp_now_init()) != ESP_OK
        || (!term_is_invalid_term(pmk) && (err = esp_now_set_pmk((const uint8_t *) term_binary_data(pmk))) != ESP_OK)
        || (err = esp_now_register_recv_cb(espnow_recv_cb)) != ESP_OK
        || (err = esp_now_register_send_cb(espnow_send_cb)) != ESP_OK
        || (err = add_peer(broadcast_mac, 0, NULL)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to start ESP-NOW: %s", esp_err_to_name(err));
        teardown(data);
        return NULL;
    }

    Context *ctx = context_new(global);
    ctx->native_handler = espnow_consume_mailbox;
    ctx->platform_data = data;
    return ctx;
}

static term error_tuple(Context *ctx, esp_err_t err)
{
    return port_create_error_tuple(ctx, term_from_int(err));
}

// {add_peer, Mac, Options}: `channel' (0, the current one) and `lmk', a 16-byte
// key that encrypts the traffic with this peer (the PMK must be set at open).
static term do_add_peer(Context *ctx, term req)
{
    GlobalContext *glb = ctx->global;
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 3 || !get_mac(term_get_tuple_element(req, 1), glb, mac)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    term peer_opts = term_get_tuple_element(req, 2);
    int channel = get_channel(interop_kv_get_value_default(peer_opts, ATOM_STR("\x7", "channel"), term_from_int(0), glb));
    term lmk = interop_kv_get_value_default(peer_opts, ATOM_STR("\x3", "lmk"), term_invalid_term(), glb);
    if (channel < 0 || (!term_is_invalid_term(lmk) && (!term_is_binary(lmk) || term_binary_size(lmk) != ESP_NOW_KEY_LEN))) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    esp_err_t err = add_peer(mac, channel, term_is_invalid_term(lmk) ? NULL : (const uint8_t *) term_binary_data(lmk));
    return err == ESP_OK ? OK_ATOM : error_tuple(ctx, err);
}

static term do_del_peer(Context *ctx, term req)
{
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 2 || !get_mac(term_get_tuple_element(req, 1), ctx->global, mac)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    esp_err_t err = esp_now_del_peer(mac);
    return err == ESP_OK ? OK_ATOM : error_tuple(ctx, err);
}

// {send, Mac | broadcast, Data}: queued for the radio; the result comes as
// {espnow, sent, Mac, ok | fail}.
static term do_send(Context *ctx, term req)
{
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 3 || !get_mac(term_get_tuple_element(req, 1), ctx->global, mac)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    term payload = term_get_tuple_element(req, 2);
    if (!term_is_binary(payload) || term_binary_size(payload) == 0 || term_binary_size(payload) > ESP_NOW_MAX_DATA_LEN) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    esp_err_t err = esp_now_send(mac, (const uint8_t *) term_binary_data(payload), term_binary_size(payload));
    return err == ESP_OK ? OK_ATOM : error_tuple(ctx, err);
}

static term do_set_channel(Context *ctx, struct EspNowData *data, term req)
{
    int channel = term_is_tuple(req) && term_get_tuple_arity(req) == 2 ? get_channel(term_get_tuple_element(req, 1)) : -1;
    if (channel <= 0) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    esp_err_t err = esp_wifi_set_channel(channel, WIFI_SECOND_CHAN_NONE);
    if (err != ESP_OK) {
        return error_tuple(ctx, err);
    }
    data->channel = channel;
    return OK_ATOM;
}

static term do_get_channel(Context *ctx)
{
    uint8_t primary;
    wifi_second_chan_t second;
    esp_err_t err = esp_wifi_get_channel(&primary, &second);
    if (err != ESP_OK) {
        return error_tuple(ctx, err);
    }
    term ok = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ok, 0, OK_ATOM);
    term_put_tuple_element(ok, 1, term_from_int(primary));
    return ok;
}

// {ok, #{dropped => N}}: events lost because the queue was full.
static term do_stats(Context *ctx, struct EspNowData *data)
{
    term map = term_alloc_map(1, &ctx->heap);
    term_set_map_assoc(map, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "dropped")), term_from_int(data->dropped));
    term ok = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ok, 0, OK_ATOM);
    term_put_tuple_element(ok, 1, map);
    return ok;
}

static NativeHandlerResult espnow_consume_mailbox(Context *ctx)
{
    struct EspNowData *data = ctx->platform_data;
    bool closed = false;
    while (!closed && mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        GenMessage gen_message;
        if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)) {
            ESP_LOGW(TAG, "Received invalid message.");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }
        term req = gen_message.req;
        term cmd_term = term_is_atom(req) ? req : (term_is_tuple(req) && term_get_tuple_arity(req) > 0 ? term_get_tuple_element(req, 0) : req);
        enum espnow_cmd cmd = term_is_atom(cmd_term) ? interop_atom_term_select_int(cmd_table, cmd_term, ctx->global) : EspNowInvalidCmd;

        // The largest reply: {Ref, {ok, #{dropped => N}}}, or an error tuple.
        if (UNLIKELY(memory_ensure_free(ctx, PORT_REPLY_SIZE + TUPLE_SIZE(2) + term_map_size_in_terms(1)) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "No memory for a reply");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        term reply;
        switch (cmd) {
            case EspNowAddPeerCmd:
                reply = do_add_peer(ctx, req);
                break;
            case EspNowDelPeerCmd:
                reply = do_del_peer(ctx, req);
                break;
            case EspNowSendCmd:
                reply = do_send(ctx, req);
                break;
            case EspNowSetChannelCmd:
                reply = do_set_channel(ctx, data, req);
                break;
            case EspNowGetChannelCmd:
                reply = do_get_channel(ctx);
                break;
            case EspNowStatsCmd:
                reply = do_stats(ctx, data);
                break;
            case EspNowCloseCmd:
                teardown(data);
                ctx->platform_data = NULL;
                closed = true;
                reply = OK_ATOM;
                break;
            default:
                reply = port_create_error_tuple(ctx, BADARG_ATOM);
                break;
        }
        port_send_reply(ctx, gen_message.pid, gen_message.ref, reply);
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return closed ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(espnow, NULL, NULL, espnow_create_port)

#endif
