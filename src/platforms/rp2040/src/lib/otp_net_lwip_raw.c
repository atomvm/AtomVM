/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Paul Guyot <pguyot@kallisys.net>
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

#include <defaultatoms.h>
#include <inet.h>
#include <nifs.h>
#include <otp_net_lwip_raw.h>
#include <otp_socket.h>
#include <rp2040_sys.h>

#include <lwip/dns.h>

// #define ENABLE_TRACE
#include <trace.h>

//
// net:getaddrinfo/1
//

// addr map is shared by the two items

#define GETADDRINFO_RESULT_SIZE \
    (TUPLE_SIZE(2) + (CONS_SIZE * 2) + TERM_MAP_SIZE(3) + TERM_MAP_SIZE(5) + TERM_MAP_SHARED_SIZE(5) + INET_ADDR4_TUPLE_SIZE)

static term make_getaddrinfo_result(uint32_t addr, GlobalContext *global, Heap *heap)
{
    term addr_term = inet_make_addr4(addr, heap);
    term addr_map = term_alloc_map(3, heap);

    term family_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "family"));
    term inet_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "inet"));
    term_set_map_assoc(addr_map, 0, family_atom, inet_atom);

    term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
    term_set_map_assoc(addr_map, 1, addr_atom, addr_term);

    term port_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "port"));
    term_set_map_assoc(addr_map, 2, port_atom, term_from_int(0));

    term udp_info_map = term_alloc_map(5, heap);
    term_set_map_assoc(udp_info_map, 0, family_atom, inet_atom);

    // Compatibility with OTP
    term_set_map_assoc(udp_info_map, 1, addr_atom, addr_map);

    term address_atom = globalcontext_make_atom(global, ATOM_STR("\x7", "address"));
    term_set_map_assoc(udp_info_map, 2, address_atom, addr_map);

    term type_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "type"));
    term dgram_atom = globalcontext_make_atom(global, ATOM_STR("\x5", "dgram"));
    term stream_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "stream"));
    term_set_map_assoc(udp_info_map, 3, type_atom, dgram_atom);

    term protocol_atom = globalcontext_make_atom(global, ATOM_STR("\x8", "protocol"));
    term udp_atom = globalcontext_make_atom(global, ATOM_STR("\x3", "udp"));
    term tcp_atom = globalcontext_make_atom(global, ATOM_STR("\x3", "tcp"));
    term_set_map_assoc(udp_info_map, 4, protocol_atom, udp_atom);

    term tcp_info_map = term_alloc_map_maybe_shared(5, term_get_map_keys(udp_info_map), heap);
    term_set_map_assoc(tcp_info_map, 0, family_atom, inet_atom);
    term_set_map_assoc(tcp_info_map, 1, addr_atom, addr_map);
    term_set_map_assoc(tcp_info_map, 2, address_atom, addr_map);
    term_set_map_assoc(tcp_info_map, 3, type_atom, stream_atom);
    term_set_map_assoc(tcp_info_map, 4, protocol_atom, tcp_atom);

    term info_list = term_nil();
    info_list = term_list_prepend(tcp_info_map, info_list, heap);
    info_list = term_list_prepend(udp_info_map, info_list, heap);

    term result_tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, info_list);

    return result_tuple;
}

static term make_error_enoname_tuple(GlobalContext *global, Heap *heap)
{
    term error_tuple = term_alloc_tuple(2, heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(global, ATOM_STR("\x7", "enoname")));
    return error_tuple;
}

struct DNSCallbackArg
{
    GlobalContext *global;
    int32_t process_id;
};

static void dns_found_callback_handler(struct LWIPEvent *event)
{
    TRACE("dns_found_callback_handler\n");

    struct DNSCallbackArg *callback_arg = event->dns_gethostbyname.callback_arg;
    if (event->dns_gethostbyname.success) {
        BEGIN_WITH_STACK_HEAP(2 * GETADDRINFO_RESULT_SIZE, heap)
        term result = make_getaddrinfo_result(event->dns_gethostbyname.addr, callback_arg->global, &heap);
        Context *target_ctx = globalcontext_get_process_lock(callback_arg->global, callback_arg->process_id);
        if (target_ctx) {
            mailbox_send_term_signal(target_ctx, TrapAnswerSignal, result);
            globalcontext_get_process_unlock(callback_arg->global, target_ctx);
        }
        END_WITH_STACK_HEAP(heap, callback_arg->global)
    } else {
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap)
        term result = make_error_enoname_tuple(callback_arg->global, &heap);
        Context *target_ctx = globalcontext_get_process_lock(callback_arg->global, callback_arg->process_id);
        if (target_ctx) {
            mailbox_send_term_signal(target_ctx, TrapAnswerSignal, result);
            globalcontext_get_process_unlock(callback_arg->global, target_ctx);
        }
        END_WITH_STACK_HEAP(heap, callback_arg->global)
    }
    free(callback_arg);
}

static void dns_found_callback_cb(const char *name, const ip_addr_t *ipaddr, void *callback_arg)
{
    TRACE("dns_found_callback_cb\n");

    UNUSED(name);
    struct LWIPEvent event;
    event.handler = dns_found_callback_handler;
    event.dns_gethostbyname.callback_arg = callback_arg;
    if (ipaddr) {
        event.dns_gethostbyname.success = true;
        event.dns_gethostbyname.addr = ntohl(ip_addr_get_ip4_u32(ipaddr));
    } else {
        event.dns_gethostbyname.success = false;
    }
    otp_socket_lwip_enqueue(&event);
}

static term nif_net_getaddrinfo(Context *ctx, int argc, term argv[])
{
    TRACE("nif_net_getaddrinfo\n");
    UNUSED(argc);

    term host = argv[0];

    if (host == UNDEFINED_ATOM) {
        TRACE("Host param may not be undefined");
        RAISE_ERROR(BADARG_ATOM);
    }

    int ok;
    char *host_str = interop_term_to_string(host, &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ip_addr_t cached_result;

    // We need to allocate a callback struct here because we need Global
    // in the handler.
    struct DNSCallbackArg *callback_arg = malloc(sizeof(struct DNSCallbackArg));
    callback_arg->global = ctx->global;
    callback_arg->process_id = ctx->process_id;
    err_t err = dns_gethostbyname(host_str, &cached_result, dns_found_callback_cb, callback_arg);
    if (err == ERR_OK) {
        free(callback_arg);
        if (UNLIKELY(memory_ensure_free(ctx, GETADDRINFO_RESULT_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_getaddrinfo_result(ntohl(ip_addr_get_ip4_u32(&cached_result)), ctx->global, &ctx->heap);
    }

    if (err == ERR_ARG) {
        free(callback_arg);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_enoname_tuple(ctx->global, &ctx->heap);
    }

    // Trap caller
    context_update_flags(ctx, ~NoFlags, Trap);
    return term_invalid_term();
}

//
// Nifs
//

static const struct Nif net_getaddrinfo_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_net_getaddrinfo
};

//
// Entrypoints
//

const struct Nif *otp_net_nif_get_nif(const char *nifname)
{
    if (strncmp("net:", nifname, 4) == 0) {
        const char *rest = nifname + 4;
        if (strcmp("getaddrinfo_nif/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &net_getaddrinfo_nif;
        }
    }
    return NULL;
}

static void otp_net_nif_init(GlobalContext *global)
{
    UNUSED(global);
    dns_init();
}

REGISTER_NIF_COLLECTION(otp_net, otp_net_nif_init, NULL, otp_net_nif_get_nif)

#endif
