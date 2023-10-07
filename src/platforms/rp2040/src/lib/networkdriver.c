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

#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <port.h>

#include "rp2040_sys.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <cyw43.h>
#include <pico/cyw43_arch.h>

#pragma GCC diagnostic pop

#ifdef LIB_PICO_CYW43_ARCH

#define PORT_REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)

static const char *const ap_atom = ATOM_STR("\x2", "ap");
static const char *const psk_atom = ATOM_STR("\x3", "psk");
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
};

// Callbacks do not allow for user data
// netif->state is actually pointing to &cyw43_state
static struct NetworkDriverData *driver_data;

static void network_driver_netif_status_cb(struct netif *netif);

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

static term start_ap(term ap_config, GlobalContext *glb)
{
    UNUSED(ap_config);
    UNUSED(glb);
    return BADARG_ATOM;
}

static void network_driver_netif_status_cb(struct netif *netif)
{
    if (netif == &cyw43_state.netif[CYW43_ITF_STA]) {
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
    }
    driver_data->global = ctx->global;
    driver_data->owner_process_id = term_to_local_process_id(pid);
    driver_data->ref_ticks = term_to_ref_ticks(ref);
    driver_data->link_status = CYW43_LINK_DOWN;

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

    if (sta_config) {
        term result_atom = start_sta(sta_config, ctx->global);
        if (result_atom != OK_ATOM) {
            term error = port_create_error_tuple(ctx, result_atom);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    } else {
        cyw43_arch_disable_sta_mode();
    }

    if (ap_config) {
        term result_atom = start_ap(ap_config, ctx->global);
        if (result_atom != OK_ATOM) {
            term error = port_create_error_tuple(ctx, result_atom);
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    } else {
        cyw43_arch_disable_ap_mode();
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
