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

#include <stdatomic.h>
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
#include "refc_binary.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"
#include "sys.h"

#include <network_wifi.h>

#define TAG "espnow_driver"

// The longest frame this ESP-IDF sends and receives: 1470 bytes with ESP-NOW
// v2 (ESP-IDF 5.4 or newer), 250 before.
#ifdef ESP_NOW_MAX_DATA_LEN_V2
#define ESPNOW_MAX_FRAME ESP_NOW_MAX_DATA_LEN_V2
#else
#define ESPNOW_MAX_FRAME ESP_NOW_MAX_DATA_LEN
#endif
// Events wait here between the Wi-Fi task and the scheduler: small items, a
// received frame being a pointer to a binary the Wi-Fi task already filled.
// The bytes those binaries hold are bounded by CONFIG_AVM_ESPNOW_RX_BUDGET.
#define ESPNOW_QUEUE_LEN 16
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
    // EspNowReceived only: created and filled in the Wi-Fi task, neither
    // listed nor counted until the listener turns it into a term.
    struct RefcBinary *frame;
};

// A send/3 waiting for the radio: ESP-NOW is given one frame at a time, the
// next once the send callback of the previous one has come, and its caller is
// answered from that callback.
struct EspNowSend
{
    struct EspNowSend *next;
    int32_t pid;
    uint64_t ref;
    uint8_t mac[MAC_LEN];
    uint16_t len;
    uint8_t data[];
};

struct EspNowData
{
    QueueHandle_t queue;
    EventListener listener;
    GlobalContext *global;
    int32_t receiver;
    // The port that opened it. Its context does not point here: a port
    // killed (exit/2, or a linked owner that crashed) is destroyed without
    // its handler running, and must leave nothing freed that the Wi-Fi task
    // or a listener still reaches. The next open finds the port gone and
    // tears this down.
    int32_t port;
    // Bytes of received frames between the Wi-Fi task and their delivery.
    _Atomic uint32_t rx_bytes;
    _Atomic uint32_t dropped;
    // The sends, the first one with the radio when `sending'. Only the
    // port's handler (and teardown) touches them.
    struct EspNowSend *sends;
    struct EspNowSend *last_send;
    bool sending;
};

// ESP-NOW is one per chip: at most one open port. Stored before the
// callbacks are registered, cleared before teardown frees it.
static _Atomic(struct EspNowData *) espnow_data;
// Callbacks running now, so that teardown waits for them.
static _Atomic uint32_t espnow_in_callback;
// The channel ESP-NOW was given (0: follow the current one), read when the
// network driver brings the radio back as a bare STA.
static _Atomic uint8_t espnow_channel;

// The keys, installed again each time the radio restarts (a network starting
// or stopping), which clears them: the PMK given at open, and the encrypted
// peers' LMKs. Written by the port, read by the network driver's callback,
// both under `espnow_keys_lock'.
struct EspNowKey
{
    uint8_t mac[MAC_LEN];
    uint8_t lmk[ESP_NOW_KEY_LEN];
};
static portMUX_TYPE espnow_keys_lock = portMUX_INITIALIZER_UNLOCKED;
static bool espnow_has_pmk;
static uint8_t espnow_pmk[ESP_NOW_KEY_LEN];
static struct EspNowKey espnow_lmks[ESP_NOW_MAX_ENCRYPT_PEER_NUM];
static size_t espnow_lmk_count;

static NativeHandlerResult espnow_consume_mailbox(Context *ctx);
static bool sta_associated(void);

static const uint8_t broadcast_mac[MAC_LEN] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };

enum espnow_cmd
{
    EspNowInvalidCmd = 0,
    EspNowOpenCmd,
    EspNowCloseCmd,
    EspNowAddPeerCmd,
    EspNowDelPeerCmd,
    EspNowSendCmd,
    EspNowSetChannelCmd,
    EspNowGetChannelCmd,
    EspNowStatsCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "open"), EspNowOpenCmd },
    { ATOM_STR("\x5", "close"), EspNowCloseCmd },
    { ATOM_STR("\x8", "add_peer"), EspNowAddPeerCmd },
    { ATOM_STR("\x8", "del_peer"), EspNowDelPeerCmd },
    { ATOM_STR("\x4", "send"), EspNowSendCmd },
    { ATOM_STR("\xB", "set_channel"), EspNowSetChannelCmd },
    { ATOM_STR("\xB", "get_channel"), EspNowGetChannelCmd },
    { ATOM_STR("\x5", "stats"), EspNowStatsCmd },
    SELECT_INT_DEFAULT(EspNowInvalidCmd)
};

// The callbacks run in the Wi-Fi task and take no VM lock: malloc, memcpy,
// atomics and a queue send that does not wait. NULL means the driver is
// gone: do nothing.
static struct EspNowData *callback_enter(void)
{
    atomic_fetch_add(&espnow_in_callback, 1); // ordered before the load
    struct EspNowData *data = atomic_load(&espnow_data);
    if (data == NULL) {
        atomic_fetch_sub(&espnow_in_callback, 1);
    }
    return data;
}

static void callback_leave(void)
{
    atomic_fetch_sub(&espnow_in_callback, 1);
}

// Wi-Fi task: the payload is copied once, into the binary the receiver will
// get.
static void espnow_recv_cb(const esp_now_recv_info_t *info, const uint8_t *payload, int len)
{
    struct EspNowData *data = callback_enter();
    if (data == NULL) {
        return;
    }
    if (len <= 0 || len > ESPNOW_MAX_FRAME) {
        atomic_fetch_add(&data->dropped, 1);
        goto leave;
    }
    // The last slot is the send result's: with one frame out at a time
    // there is at most one, and a send/3 waits for it. Both callbacks run in
    // the Wi-Fi task, one after the other.
    if (atomic_fetch_add(&data->rx_bytes, len) + len > CONFIG_AVM_ESPNOW_RX_BUDGET
        || uxQueueSpacesAvailable(data->queue) <= 1) {
        goto drop;
    }
    struct RefcBinary *frame = refc_binary_create_refc(len);
    if (UNLIKELY(frame == NULL)) {
        goto drop;
    }
    memcpy(frame->data, payload, len);
    struct EspNowEvent event = { .kind = EspNowReceived, .rssi = info->rx_ctrl->rssi, .frame = frame };
    memcpy(event.mac, info->src_addr, MAC_LEN);
    if (LIKELY(xQueueSend(data->queue, &event, 0) == pdTRUE)) {
        goto leave;
    }
    free(frame); // neither listed nor counted
drop:
    atomic_fetch_sub(&data->rx_bytes, len);
    atomic_fetch_add(&data->dropped, 1);
leave:
    callback_leave();
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
    struct EspNowData *data = callback_enter();
    if (data == NULL) {
        return;
    }
    struct EspNowEvent event = { .kind = EspNowSent, .ok = status == ESP_NOW_SEND_SUCCESS };
    if (mac != NULL) {
        memcpy(event.mac, mac, MAC_LEN);
    }
    // A slot is kept for it (espnow_recv_cb).
    xQueueSend(data->queue, &event, 0);
    callback_leave();
}

static void forget_lmk(const uint8_t mac[MAC_LEN])
{
    taskENTER_CRITICAL(&espnow_keys_lock);
    for (size_t i = 0; i < espnow_lmk_count; i++) {
        if (memcmp(espnow_lmks[i].mac, mac, MAC_LEN) == 0) {
            espnow_lmks[i] = espnow_lmks[--espnow_lmk_count];
            break;
        }
    }
    taskEXIT_CRITICAL(&espnow_keys_lock);
}

static void remember_lmk(const uint8_t mac[MAC_LEN], const uint8_t lmk[ESP_NOW_KEY_LEN])
{
    forget_lmk(mac);
    taskENTER_CRITICAL(&espnow_keys_lock);
    if (espnow_lmk_count < ESP_NOW_MAX_ENCRYPT_PEER_NUM) {
        memcpy(espnow_lmks[espnow_lmk_count].mac, mac, MAC_LEN);
        memcpy(espnow_lmks[espnow_lmk_count].lmk, lmk, ESP_NOW_KEY_LEN);
        espnow_lmk_count++;
    }
    taskEXIT_CRITICAL(&espnow_keys_lock);
}

static void forget_keys(void)
{
    taskENTER_CRITICAL(&espnow_keys_lock);
    espnow_has_pmk = false;
    espnow_lmk_count = 0;
    taskEXIT_CRITICAL(&espnow_keys_lock);
}

// The network driver stopped and started the radio, which cleared the keys
// ESP-NOW installed in it: encrypted frames went out with no key and were
// dropped by the peer. The keys go back; on a bare STA (network:stop/0) the
// channel is ours again too. Reads nothing that teardown frees.
static void espnow_wifi_restarted(bool bare_sta)
{
    taskENTER_CRITICAL(&espnow_keys_lock);
    bool has_pmk = espnow_has_pmk;
    uint8_t pmk[ESP_NOW_KEY_LEN];
    memcpy(pmk, espnow_pmk, ESP_NOW_KEY_LEN);
    size_t count = espnow_lmk_count;
    struct EspNowKey lmks[ESP_NOW_MAX_ENCRYPT_PEER_NUM];
    memcpy(lmks, espnow_lmks, sizeof(lmks));
    taskEXIT_CRITICAL(&espnow_keys_lock);

    if (has_pmk) {
        esp_now_set_pmk(pmk);
    }
    for (size_t i = 0; i < count; i++) {
        esp_now_peer_info_t peer;
        if (esp_now_get_peer(lmks[i].mac, &peer) == ESP_OK) {
            peer.encrypt = true;
            memcpy(peer.lmk, lmks[i].lmk, ESP_NOW_KEY_LEN);
            esp_now_mod_peer(&peer);
        }
    }
    uint8_t channel = atomic_load(&espnow_channel);
    if (bare_sta && channel != 0) {
        esp_wifi_set_channel(channel, WIFI_SECOND_CHAN_NONE);
    }
}

// Scheduler. A received frame goes to the receiver as
// {espnow, rx, Mac, Data, #{rssi => Rssi}}, its binary the one the Wi-Fi task
// filled; a send result goes to the port, as {espnow_sent, ok | fail}, which
// answers the waiting send/3 and gives the radio the next frame.
static EventListener *espnow_event_callback(GlobalContext *glb, EventListener *listener)
{
    struct EspNowData *data = GET_LIST_ENTRY(listener, struct EspNowData, listener);
    struct EspNowEvent event;
    if (xQueueReceive(data->queue, &event, 0) != pdTRUE) {
        return listener;
    }

    if (event.kind == EspNowSent) {
        // On the stack: a send/3 waits for it, it cannot be lost.
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap);
        {
            term message = term_alloc_tuple(2, &heap);
            term_put_tuple_element(message, 0, globalcontext_make_atom(glb, ATOM_STR("\xB", "espnow_sent")));
            term_put_tuple_element(message, 1, event.ok ? OK_ATOM : globalcontext_make_atom(glb, ATOM_STR("\x4", "fail")));
            globalcontext_send_message(glb, data->port, message);
        }
        END_WITH_STACK_HEAP(heap, glb);
        return listener;
    }

    size_t staged = event.frame->size; // read now: the frame may be freed below
    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, TUPLE_SIZE(5) + term_binary_heap_size(MAC_LEN) + TERM_BOXED_REFC_BINARY_SIZE + term_map_size_in_terms(1)) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "No memory for a frame, dropped");
        free(event.frame); // still neither listed nor counted
        atomic_fetch_sub(&data->rx_bytes, staged);
        atomic_fetch_add(&data->dropped, 1);
        return listener;
    }

    {
        term map = term_alloc_map(1, &heap);
        term_set_map_assoc(map, 0, globalcontext_make_atom(glb, ATOM_STR("\x4", "rssi")), term_from_int(event.rssi));
        term message = term_alloc_tuple(5, &heap);
        term_put_tuple_element(message, 0, globalcontext_make_atom(glb, ATOM_STR("\x6", "espnow")));
        term_put_tuple_element(message, 1, globalcontext_make_atom(glb, ATOM_STR("\x2", "rx")));
        term_put_tuple_element(message, 2, term_from_literal_binary(event.mac, MAC_LEN, &heap, glb));
        // Listed with a count of 1 here; the receiver's copy of the message
        // takes it to 2, and destroying this heap back to 1, the receiver's.
        // A receiver that is gone copies nothing, and the binary is freed.
        term_put_tuple_element(message, 3, term_from_refc_binary(event.frame, &heap, glb));
        term_put_tuple_element(message, 4, map);
        globalcontext_send_message(glb, data->receiver, message);
    }
    memory_destroy_heap(&heap, glb);
    atomic_fetch_sub(&data->rx_bytes, staged);
    return listener;
}

// A 6-byte MAC binary, or `broadcast' where `broadcast' is allowed: send/3.
// The broadcast peer is the driver's, added at open; adding it again can
// only fail, and deleting it would break every later broadcast.
static bool get_mac(term t, GlobalContext *glb, bool broadcast, uint8_t mac[MAC_LEN])
{
    if (broadcast && t == globalcontext_make_atom(glb, ATOM_STR("\x9", "broadcast"))) {
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

// Answers a send/3 that is no longer waiting in the port's handler.
static void reply_send(GlobalContext *glb, const struct EspNowSend *send, term result_atom, bool ok)
{
    BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2), heap);
    {
        term result = ok ? OK_ATOM : port_heap_create_error_tuple(&heap, result_atom);
        term reply = port_heap_create_reply(&heap, term_from_ref_ticks(send->ref, &heap), result);
        port_send_message(glb, term_from_local_process_id(send->pid), reply);
    }
    END_WITH_STACK_HEAP(heap, glb);
}

// The callbacks first: once espnow_data is NULL no new callback touches
// `data', and one already running finishes before it goes (a callback never
// blocks, and on a single core the scheduler cannot preempt one). Then the
// listener, whose dispatch unregistering waits for, and the queue, emptied
// of the frames still staged: xQueueRemoveFromSet fails on a non-empty
// queue. The sends still waiting are answered {error, closed}.
static void teardown(struct EspNowData *data)
{
    GlobalContext *glb = data->global;
    esp_now_unregister_recv_cb();
    esp_now_unregister_send_cb();
    esp_now_deinit();
    atomic_store(&espnow_data, NULL);
    while (atomic_load(&espnow_in_callback) != 0) {
        vTaskDelay(1);
    }
    atomic_store(&espnow_channel, 0);
    forget_keys();
    sys_unregister_listener(glb, &data->listener);
    struct EspNowEvent event;
    while (xQueueReceive(data->queue, &event, 0) == pdTRUE) {
        if (event.kind == EspNowReceived) {
            free(event.frame);
        }
    }
    if (UNLIKELY(xQueueRemoveFromSet(data->queue, event_set) != pdPASS)) {
        ESP_LOGE(TAG, "Failed to remove the ESP-NOW queue from the event set");
    }
    vQueueDelete(data->queue);
    network_wifi_release();
    term closed = globalcontext_make_atom(glb, ATOM_STR("\x6", "closed"));
    while (data->sends != NULL) {
        struct EspNowSend *send = data->sends;
        data->sends = send->next;
        reply_send(glb, send, closed, false);
        free(send);
    }
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

static term error_atom(Heap *heap, GlobalContext *glb, AtomString reason)
{
    return port_heap_create_error_tuple(heap, globalcontext_make_atom(glb, reason));
}

// The ESP-NOW errors a caller can act on, as atoms; anything else as
// esp_err_to_term/2 gives it.
static term error_reason(GlobalContext *glb, esp_err_t err)
{
    switch (err) {
        case ESP_ERR_ESPNOW_NOT_FOUND:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "not_found"));
        case ESP_ERR_ESPNOW_EXIST:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "peer_exists"));
        case ESP_ERR_ESPNOW_FULL:
            return globalcontext_make_atom(glb, ATOM_STR("\xE", "peer_list_full"));
        case ESP_ERR_ESPNOW_IF:
            return globalcontext_make_atom(glb, ATOM_STR("\x12", "interface_mismatch"));
        case ESP_ERR_ESPNOW_CHAN:
            return globalcontext_make_atom(glb, ATOM_STR("\x10", "channel_mismatch"));
        case ESP_ERR_ESPNOW_NO_MEM:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "no_mem"));
        default:
            return esp_err_to_term(glb, err);
    }
}

static term error_tuple(Heap *heap, GlobalContext *glb, esp_err_t err)
{
    return port_heap_create_error_tuple(heap, error_reason(glb, err));
}

static term badarg(Heap *heap)
{
    return port_heap_create_error_tuple(heap, BADARG_ATOM);
}

// {open, Options}, the port's first call: `receiver', `channel' and `pmk'.
// `{error, already_open}' while another port has ESP-NOW, `{error, ap_only}'
// while an AP-only network runs the radio, `badarg' for bad options, the
// ESP-IDF error otherwise. A port that fails to open terminates.
static term do_open(Heap *heap, Context *ctx, term req)
{
    GlobalContext *glb = ctx->global;
    struct EspNowData *old = atomic_load(&espnow_data);
    if (old != NULL) {
        if (globalcontext_process_exists(glb, old->port)) {
            return error_atom(heap, glb, ATOM_STR("\xC", "already_open"));
        }
        teardown(old);
    }

    term opts = term_is_tuple(req) && term_get_tuple_arity(req) == 2 ? term_get_tuple_element(req, 1) : term_invalid_term();
    if (!term_is_list(opts)) {
        return badarg(heap);
    }
    term receiver = interop_kv_get_value_default(opts, ATOM_STR("\x8", "receiver"), term_invalid_term(), glb);
    term channel_term = interop_kv_get_value_default(opts, ATOM_STR("\x7", "channel"), term_from_int(0), glb);
    term pmk = interop_kv_get_value_default(opts, ATOM_STR("\x3", "pmk"), term_invalid_term(), glb);
    int channel = get_channel(channel_term);
    if (!term_is_local_pid(receiver) || channel < 0
        || (!term_is_invalid_term(pmk) && (!term_is_binary(pmk) || term_binary_size(pmk) != ESP_NOW_KEY_LEN))) {
        return badarg(heap);
    }

    struct EspNowData *data = calloc(1, sizeof(struct EspNowData));
    if (IS_NULL_PTR(data)) {
        return error_atom(heap, glb, ATOM_STR("\x6", "no_mem"));
    }
    data->global = glb;
    data->receiver = term_to_local_process_id(receiver);
    data->port = ctx->process_id;
    data->queue = xQueueCreate(ESPNOW_QUEUE_LEN, sizeof(struct EspNowEvent));
    if (IS_NULL_PTR(data->queue)) {
        free(data);
        return error_atom(heap, glb, ATOM_STR("\x6", "no_mem"));
    }

    esp_err_t err = network_wifi_hold(espnow_wifi_restarted);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "No Wi-Fi radio for ESP-NOW: %s", esp_err_to_name(err));
        vQueueDelete(data->queue);
        free(data);
        return err == ESP_ERR_NOT_SUPPORTED ? error_atom(heap, glb, ATOM_STR("\x7", "ap_only")) : error_tuple(heap, glb, err);
    }
    // On an associated STA the channel is the access point's, and ESP-NOW
    // follows it.
    atomic_store(&espnow_channel, channel);
    if (channel != 0 && !sta_associated()) {
        esp_wifi_set_channel(channel, WIFI_SECOND_CHAN_NONE);
    }

    data->listener.handler = espnow_event_callback;
    data->listener.sender = data->queue;
    sys_register_listener(glb, &data->listener);
    if (xQueueAddToSet(data->queue, event_set) != pdPASS) {
        ESP_LOGE(TAG, "Failed to add the ESP-NOW queue to the event set");
        sys_unregister_listener(glb, &data->listener);
        atomic_store(&espnow_channel, 0);
        vQueueDelete(data->queue);
        network_wifi_release();
        free(data);
        return error_atom(heap, glb, ATOM_STR("\x6", "no_mem"));
    }
    // Before the callbacks are registered: they find the state from here.
    atomic_store(&espnow_data, data);

    if (!term_is_invalid_term(pmk)) {
        taskENTER_CRITICAL(&espnow_keys_lock);
        memcpy(espnow_pmk, term_binary_data(pmk), ESP_NOW_KEY_LEN);
        espnow_has_pmk = true;
        taskEXIT_CRITICAL(&espnow_keys_lock);
    }
    if ((err = esp_now_init()) != ESP_OK
        || (!term_is_invalid_term(pmk) && (err = esp_now_set_pmk((const uint8_t *) term_binary_data(pmk))) != ESP_OK)
        || (err = esp_now_register_recv_cb(espnow_recv_cb)) != ESP_OK
        || (err = esp_now_register_send_cb(espnow_send_cb)) != ESP_OK
        || (err = add_peer(broadcast_mac, 0, NULL)) != ESP_OK) {
        ESP_LOGE(TAG, "Failed to start ESP-NOW: %s", esp_err_to_name(err));
        teardown(data);
        return error_tuple(heap, glb, err);
    }
    return OK_ATOM;
}

// Cheap: the radio and ESP-NOW start in the first call, {open, Options}
// (espnow:open/1), whose reply can say why they did not.
static Context *espnow_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);
    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        return NULL;
    }
    ctx->native_handler = espnow_consume_mailbox;
    return ctx;
}

// {add_peer, Mac, Options}: `channel' (0, the current one) and `lmk', a 16-byte
// key that encrypts the traffic with this peer (the PMK must be set at open).
static term do_add_peer(Heap *heap, GlobalContext *glb, term req)
{
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 3 || !get_mac(term_get_tuple_element(req, 1), glb, false, mac)) {
        return badarg(heap);
    }
    term peer_opts = term_get_tuple_element(req, 2);
    int channel = get_channel(interop_kv_get_value_default(peer_opts, ATOM_STR("\x7", "channel"), term_from_int(0), glb));
    term lmk = interop_kv_get_value_default(peer_opts, ATOM_STR("\x3", "lmk"), term_invalid_term(), glb);
    if (channel < 0 || (!term_is_invalid_term(lmk) && (!term_is_binary(lmk) || term_binary_size(lmk) != ESP_NOW_KEY_LEN))) {
        return badarg(heap);
    }
    const uint8_t *key = term_is_invalid_term(lmk) ? NULL : (const uint8_t *) term_binary_data(lmk);
    esp_err_t err = add_peer(mac, channel, key);
    if (err != ESP_OK) {
        return error_tuple(heap, glb, err);
    }
    if (key != NULL) {
        remember_lmk(mac, key);
    }
    return OK_ATOM;
}

static term do_del_peer(Heap *heap, GlobalContext *glb, term req)
{
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 2 || !get_mac(term_get_tuple_element(req, 1), glb, false, mac)) {
        return badarg(heap);
    }
    esp_err_t err = esp_now_del_peer(mac);
    if (err != ESP_OK) {
        return error_tuple(heap, glb, err);
    }
    forget_lmk(mac);
    return OK_ATOM;
}

// Gives the radio the first waiting frame, unless one is out. A frame the
// radio refuses is answered at once with the error, and the next one tried.
static void next_send(GlobalContext *glb, struct EspNowData *data)
{
    while (!data->sending && data->sends != NULL) {
        struct EspNowSend *send = data->sends;
        esp_err_t err = esp_now_send(send->mac, send->data, send->len);
        if (err == ESP_OK) {
            data->sending = true;
            return;
        }
        data->sends = send->next;
        reply_send(glb, send, error_reason(glb, err), false);
        free(send);
    }
}

// {espnow_sent, ok | fail}, from the listener: the frame out has its result.
static void sent(GlobalContext *glb, struct EspNowData *data, term result)
{
    struct EspNowSend *send = data->sends;
    if (!data->sending || send == NULL) {
        return;
    }
    data->sending = false;
    data->sends = send->next;
    reply_send(glb, send, result, result == OK_ATOM);
    free(send);
    next_send(glb, data);
}

// {send, Mac | broadcast, Data}: waits, with the frame, until the radio has
// sent it and the send callback has come; the reply is then `ok' (for a
// unicast frame, the peer acknowledged it) or `{error, fail}'. Frames go to
// the radio one at a time, as the ESP-NOW guide recommends: nothing in the
// callback says which frame it is about. term_invalid_term() means no reply
// now.
static term do_send(Heap *heap, GlobalContext *glb, struct EspNowData *data, term req, term pid, term ref)
{
    uint8_t mac[MAC_LEN];
    if (!term_is_tuple(req) || term_get_tuple_arity(req) != 3 || !get_mac(term_get_tuple_element(req, 1), glb, true, mac)
        || !term_is_local_reference(ref) || !term_is_local_pid(pid)) {
        return badarg(heap);
    }
    term payload = term_get_tuple_element(req, 2);
    if (!term_is_binary(payload) || term_binary_size(payload) == 0 || term_binary_size(payload) > ESPNOW_MAX_FRAME) {
        return badarg(heap);
    }
    size_t len = term_binary_size(payload);
    struct EspNowSend *send = malloc(sizeof(struct EspNowSend) + len);
    if (IS_NULL_PTR(send)) {
        return error_atom(heap, glb, ATOM_STR("\x6", "no_mem"));
    }
    send->next = NULL;
    send->pid = term_to_local_process_id(pid);
    send->ref = term_to_ref_ticks(ref);
    memcpy(send->mac, mac, MAC_LEN);
    send->len = len;
    memcpy(send->data, term_binary_data(payload), len);
    if (data->sends == NULL) {
        data->sends = send;
    } else {
        data->last_send->next = send;
    }
    data->last_send = send;
    next_send(glb, data);
    return term_invalid_term();
}

// While the STA is associated the channel is the access point's: moving the
// radio would take the STA off it, so it is refused.
static bool sta_associated(void)
{
    wifi_ap_record_t ap;
    return esp_wifi_sta_get_ap_info(&ap) == ESP_OK;
}

static term do_set_channel(Heap *heap, GlobalContext *glb, term req)
{
    int channel = term_is_tuple(req) && term_get_tuple_arity(req) == 2 ? get_channel(term_get_tuple_element(req, 1)) : -1;
    if (channel <= 0) {
        return badarg(heap);
    }
    if (sta_associated()) {
        return error_atom(heap, glb, ATOM_STR("\xD", "sta_connected"));
    }
    esp_err_t err = esp_wifi_set_channel(channel, WIFI_SECOND_CHAN_NONE);
    if (err != ESP_OK) {
        return error_tuple(heap, glb, err);
    }
    atomic_store(&espnow_channel, channel);
    return OK_ATOM;
}

static term do_get_channel(Heap *heap, GlobalContext *glb)
{
    uint8_t primary;
    wifi_second_chan_t second;
    esp_err_t err = esp_wifi_get_channel(&primary, &second);
    if (err != ESP_OK) {
        return error_tuple(heap, glb, err);
    }
    return port_heap_create_ok_tuple(heap, term_from_int(primary));
}

// {ok, #{dropped => N}}: frames lost before reaching the receiver.
static term do_stats(Heap *heap, GlobalContext *glb, struct EspNowData *data)
{
    term map = term_alloc_map(1, heap);
    term_set_map_assoc(map, 0, globalcontext_make_atom(glb, ATOM_STR("\x7", "dropped")), term_from_int(atomic_load(&data->dropped)));
    return port_heap_create_ok_tuple(heap, map);
}

// {espnow_sent, Result}, which the listener sends to the port.
static bool is_sent(GlobalContext *glb, term message, term *result)
{
    if (!term_is_tuple(message) || term_get_tuple_arity(message) != 2
        || term_get_tuple_element(message, 0) != globalcontext_make_atom(glb, ATOM_STR("\xB", "espnow_sent"))) {
        return false;
    }
    *result = term_get_tuple_element(message, 1);
    return true;
}

static NativeHandlerResult espnow_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    bool closed = false;
    while (!closed && mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        // Only the port that opened ESP-NOW drives it.
        struct EspNowData *data = atomic_load(&espnow_data);
        bool owner = data != NULL && data->port == ctx->process_id;

        term result;
        if (is_sent(glb, message->message, &result)) {
            if (owner) {
                sent(glb, data, result);
            }
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }
        GenMessage gen_message;
        if (UNLIKELY(port_parse_gen_message(message->message, &gen_message) != GenCallMessage)) {
            ESP_LOGW(TAG, "Received invalid message.");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }
        term req = gen_message.req;
        term cmd_term = term_is_atom(req) ? req : (term_is_tuple(req) && term_get_tuple_arity(req) > 0 ? term_get_tuple_element(req, 0) : req);
        enum espnow_cmd cmd = term_is_atom(cmd_term) ? interop_atom_term_select_int(cmd_table, cmd_term, glb) : EspNowInvalidCmd;
        if (!owner && cmd != EspNowOpenCmd && cmd != EspNowCloseCmd) {
            cmd = EspNowInvalidCmd;
        }

        // The reply is built on the stack, so it can always be sent: a caller
        // of port:call/2 waits for it without a timeout. The largest is
        // {Ref, {ok, #{dropped => N}}}.
        BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE + TUPLE_SIZE(2) + term_map_size_in_terms(1), heap);
        {
            term reply;
            switch (cmd) {
                case EspNowOpenCmd:
                    reply = owner ? error_atom(&heap, glb, ATOM_STR("\xC", "already_open")) : do_open(&heap, ctx, req);
                    closed = reply != OK_ATOM && !owner;
                    break;
                case EspNowAddPeerCmd:
                    reply = do_add_peer(&heap, glb, req);
                    break;
                case EspNowDelPeerCmd:
                    reply = do_del_peer(&heap, glb, req);
                    break;
                case EspNowSendCmd:
                    reply = do_send(&heap, glb, data, req, gen_message.pid, gen_message.ref);
                    break;
                case EspNowSetChannelCmd:
                    reply = do_set_channel(&heap, glb, req);
                    break;
                case EspNowGetChannelCmd:
                    reply = do_get_channel(&heap, glb);
                    break;
                case EspNowStatsCmd:
                    reply = do_stats(&heap, glb, data);
                    break;
                case EspNowCloseCmd:
                    if (owner) {
                        teardown(data);
                    }
                    closed = true;
                    reply = OK_ATOM;
                    break;
                default:
                    reply = owner ? badarg(&heap) : error_atom(&heap, glb, ATOM_STR("\x8", "not_open"));
                    break;
            }
            if (!term_is_invalid_term(reply)) {
                port_send_message(glb, gen_message.pid, port_heap_create_reply(&heap, gen_message.ref, reply));
            }
        }
        END_WITH_STACK_HEAP(heap, glb);
        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return closed ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(espnow, NULL, NULL, espnow_create_port)

#endif
