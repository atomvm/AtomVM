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

#include <stdlib.h>
#include <string.h>

#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + TERM_BOXED_REFERENCE_SHORT_SIZE)

static const char *const sta_atom = ATOM_STR("\x3", "sta");
static const char *const ssid_atom = ATOM_STR("\x4", "ssid");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
static const char *const sta_connected_atom = ATOM_STR("\xD", "sta_connected");
static const char *const sta_disconnected_atom = ATOM_STR("\x10", "sta_disconnected");
static const char *const sta_got_ip_atom = ATOM_STR("\xA", "sta_got_ip");
static const char *const rssi_atom = ATOM_STR("\x4", "rssi");

enum network_cmd {
    NetworkInvalidCmd = 0,
    NetworkStartCmd,
    NetworkConnectCmd,
    NetworkHaltStaCmd,
    NetworkRssiCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x5", "start"), NetworkStartCmd },
    { ATOM_STR("\x7", "connect"), NetworkConnectCmd },
    { ATOM_STR("\x8", "halt_sta"), NetworkHaltStaCmd },
    { ATOM_STR("\x4", "rssi"), NetworkRssiCmd },
    SELECT_INT_DEFAULT(NetworkInvalidCmd)
};

struct NetworkDriverData {
    GlobalContext *global;
    uint32_t owner_process_id;
    uint64_t ref_ticks;
    char *ssid;
    char *psk;
};

static struct NetworkDriverData *driver_data = NULL;

static term make_ip_tuple(Heap *heap, uint32_t addr)
{
    term ip = term_alloc_tuple(4, heap);
    term_put_tuple_element(ip, 0, term_from_int11((addr >> 24) & 0xFF));
    term_put_tuple_element(ip, 1, term_from_int11((addr >> 16) & 0xFF));
    term_put_tuple_element(ip, 2, term_from_int11((addr >> 8) & 0xFF));
    term_put_tuple_element(ip, 3, term_from_int11(addr & 0xFF));
    return ip;
}

static void wifi_event_callback(rtems_wifi_state_t state, const struct rtems_wifi_ip_info *ip_info, void *arg)
{
    (void) arg;
    if (driver_data == NULL || driver_data->owner_process_id == 0) {
        return;
    }

    GlobalContext *glb = driver_data->global;

    switch (state) {
        case RTEMS_WIFI_ASSOCIATED:
        case RTEMS_WIFI_ASSOCIATING: {
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(msg, 0, ref);
            term_put_tuple_element(msg, 1, globalcontext_make_atom(glb, sta_connected_atom));
            globalcontext_send_message(glb, driver_data->owner_process_id, msg);
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
            globalcontext_send_message(glb, driver_data->owner_process_id, msg);
            END_WITH_STACK_HEAP(heap, glb);
            break;
        }

        case RTEMS_WIFI_DISCONNECTED: {
            BEGIN_WITH_STACK_HEAP(PORT_REPLY_SIZE, heap);
            term ref = term_from_ref_ticks(driver_data->ref_ticks, &heap);
            term msg = term_alloc_tuple(2, &heap);
            term_put_tuple_element(msg, 0, ref);
            term_put_tuple_element(msg, 1, globalcontext_make_atom(glb, sta_disconnected_atom));
            globalcontext_send_message(glb, driver_data->owner_process_id, msg);
            END_WITH_STACK_HEAP(heap, glb);
            break;
        }

        default:
            break;
    }
}

static bool extract_sta_config(term sta_config, GlobalContext *glb, char **ssid_out, char **psk_out)
{
    term ssid_term = interop_kv_get_value_default(sta_config, ssid_atom, term_invalid_term(), glb);
    if (term_is_invalid_term(ssid_term)) {
        return false;
    }

    int ok = 0;
    char *ssid = interop_term_to_string(ssid_term, &ok);
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

static void start_network(Context *ctx, term pid, term ref, term config)
{
    if (driver_data == NULL) {
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        return;
    }

    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);

    term sta_config = interop_kv_get_value_default(config, sta_atom, term_invalid_term(), ctx->global);
    if (!term_is_invalid_term(sta_config)) {
        char *ssid = NULL;
        char *psk = NULL;
        if (extract_sta_config(sta_config, ctx->global, &ssid, &psk)) {
            free(driver_data->ssid);
            free(driver_data->psk);
            driver_data->ssid = ssid;
            driver_data->psk = psk;

            if (rtems_wifi_start(ssid, psk) != 0) {
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, ERROR_ATOM));
                return;
            }
        }
    }

    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void get_sta_rssi(Context *ctx, term pid, term ref)
{
    size_t heap_size = PORT_REPLY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        return;
    }

    term rssi_val = term_from_int11(-50);
    term reply = port_create_tuple2(ctx, globalcontext_make_atom(ctx->global, rssi_atom), rssi_val);
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

    if (term_is_atom(cmd)) {
        cmd_term = cmd;
    } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
        cmd_term = term_get_tuple_element(cmd, 0);
        config = term_get_tuple_element(cmd, 1);
    }

    enum network_cmd command = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
    switch (command) {
        case NetworkStartCmd:
            start_network(ctx, pid, ref, config);
            break;

        case NetworkConnectCmd:
            start_network(ctx, pid, ref, config);
            break;

        case NetworkHaltStaCmd:
            rtems_wifi_stop();
            port_send_reply(ctx, pid, ref, OK_ATOM);
            break;

        case NetworkRssiCmd:
            get_sta_rssi(ctx, pid, ref);
            break;

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
        rtems_wifi_set_event_callback(wifi_event_callback, NULL);
    }
}

void network_driver_destroy(GlobalContext *global)
{
    (void) global;
    if (driver_data != NULL) {
        rtems_wifi_stop();
        free(driver_data->ssid);
        free(driver_data->psk);
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
    return ctx;
}

REGISTER_PORT_DRIVER(network, network_driver_init, network_driver_destroy, network_driver_create_port)
