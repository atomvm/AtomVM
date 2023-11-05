/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Fred Dushin <fred@dushin.net>
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

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <inet.h>
#include <interop.h>
#include <nifs.h>
#include <otp_net.h>
#include <port.h>
#include <term.h>

#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

// #define ENABLE_TRACE
#include <trace.h>

#define UNKNOWN_TABLE_VALUE -1

static const AtomStringIntPair protocol_table[] = {
    { ATOM_STR("\x3", "tcp"), IPPROTO_TCP },
    { ATOM_STR("\x3", "udp"), IPPROTO_UDP },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

static const AtomStringIntPair type_table[] = {
    { ATOM_STR("\x5", "dgram"), SOCK_DGRAM },
    { ATOM_STR("\x6", "stream"), SOCK_STREAM },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

//
// utilities
//

static inline term make_error_tuple(term reason, Context *ctx)
{
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason);
    return error_tuple;
}

static term eai_errno_to_term(int err, GlobalContext *glb)
{
    switch (err) {
        case EAI_AGAIN:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "eaiagain"));
        case EAI_BADFLAGS:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaibadflags"));
        case EAI_FAIL:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "eaifail"));
        case EAI_FAMILY:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaifamily"));
        case EAI_MEMORY:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaimemory"));
        case EAI_NONAME:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eainoname"));
        case EAI_SERVICE:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "eaiservice"));
        case EAI_SOCKTYPE:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaisocktype"));
#ifdef HAVE_EXTENDED_EAI_ERRNO
        case EAI_BADHINTS:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaibadhints"));
        case EAI_OVERFLOW:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaioverflow"));
        case EAI_PROTOCOL:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaiprotocol"));
        case EAI_SYSTEM:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaisystem"));
#endif
    }
    return term_from_int(err);
}

/**
 * @brief Make a getaddrino result item as part of the iteration
 * @param keys pointer to a term to store the keys of the map. If it's
 *             invalid_term, a non-shared map will be created and the keys term
 *             will be updated. Otherwise, it's used to create a shared map
 * @param ai_protocol protocol field of the addrinfo
 * @param ai_socktype socktype field of the addrinfo
 * @param inner_addr IP address  that will be stored in both address and addr
 *             entries of the map
 * @param global the global context
 * @returrn the getaddrinfo result item term
 * @param heap the heap to create terms in, should have sufficient free space
 * @details This function is called in a loop to create optimized maps that
 * share keys.
 * @end
 */
static term make_getaddrinfo_result(term *keys, int ai_protocol, int ai_socktype, term inner_addr, GlobalContext *global, Heap *heap)
{
    term result_map;
    if (term_is_invalid_term(*keys)) {
        result_map = term_alloc_map(5, heap);
    } else {
        result_map = term_alloc_map_maybe_shared(5, *keys, heap);
    }

    // in the current implementation, this will always be `inet`
    term family_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "family"));
    term family = globalcontext_make_atom(global, ATOM_STR("\x4", "inet"));
    term_set_map_assoc(result_map, 0, family_atom, family);

    term protocol_atom = globalcontext_make_atom(global, ATOM_STR("\x8", "protocol"));
    term protocol = interop_atom_term_select_atom(protocol_table, ai_protocol, global);
    term_set_map_assoc(result_map, 1, protocol_atom, term_is_invalid_term(protocol) ? UNDEFINED_ATOM : protocol);

    term type_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "type"));
    term type = interop_atom_term_select_atom(type_table, ai_socktype, global);
    term_set_map_assoc(result_map, 2, type_atom, term_is_invalid_term(type) ? UNDEFINED_ATOM : type);

    // embed the inner_addr, but reference it from both address and addr
    // for compatibility with OTP
    term address_atom = globalcontext_make_atom(global, ATOM_STR("\x7", "address"));
    term_set_map_assoc(result_map, 3, address_atom, inner_addr);

    term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
    term_set_map_assoc(result_map, 4, addr_atom, inner_addr);

    if (term_is_invalid_term(*keys)) {
        *keys = term_get_map_keys(result_map);
    }

    return result_map;
}

//
// net:getaddrinfo/1
//

static term nif_net_getaddrinfo(Context *ctx, int argc, term argv[])
{
    TRACE("nif_net_getaddrinfo\n");
    UNUSED(argc);

    GlobalContext *global = ctx->global;

    term host = argv[0];
    term service = argv[1];

    if (host == UNDEFINED_ATOM && service == UNDEFINED_ATOM) {
        TRACE("Host and Service params may not both be undefined");
        RAISE_ERROR(BADARG_ATOM);
    }

    char *host_str = NULL;
    if (host != UNDEFINED_ATOM) {
        int ok;
        host_str = interop_term_to_string(host, &ok);
        if (!ok) {
            RAISE_ERROR(BADARG_ATOM);
        }
        TRACE("Host: %s\n", host_str);
    }

    char *service_str = NULL;
    if (service != UNDEFINED_ATOM) {
        int ok;
        service_str = interop_term_to_string(service, &ok);
        if (!ok) {
            free(host_str);
            RAISE_ERROR(BADARG_ATOM);
        }
        TRACE("Service: %s\n", service_str);
    }

    avm_uint_t port = 0;
#ifdef HAVE_SERVBYNAME
    if (!IS_NULL_PTR(service_str)) {
        struct servent *svt = getservbyname(service_str, NULL);
        if (!IS_NULL_PTR(svt)) {
            port = ntohs(svt->s_port);
        }
    }
#endif
    TRACE("port: %zu\n", port);

    // for now, we are only supporting IPv4 addresses
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_INET;

    struct addrinfo *host_info;
    int err = getaddrinfo(host_str, service_str, &hints, &host_info);

    // some implementations do not support service filters
    if (err == EAI_SERVICE) {
        fprintf(stderr, "WARNING: EAI_SERVICE unsupported on this platform.\n");
        err = getaddrinfo(host_str, NULL, &hints, &host_info);
    }

    free(host_str);
    free(service_str);

    if (err != 0 && err != EAI_SERVICE) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(eai_errno_to_term(err, global), ctx);
    }
    TRACE("getaddrinfo succeeded\n");

    size_t num_addrinfos = 0;
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        num_addrinfos++;
    }
    TRACE("num_addrinfos: %zu\n", num_addrinfos);

    if (num_addrinfos == 0) {
        return term_nil();
    }

    // {ok, [#{
    //      family => Family :: atom()
    //      protocol => Protocol :: atom()
    //      type -> Type :: atom()
    //      address, addr =>
    //          #{
    //              addr => Address :: {0..255, 0..255, 0..255, 0..255},
    //              port => 0..65535,
    //              family => inet
    //          }
    // }]}
    // Note.  We might over-allocate for some more esoteric calls

    // Determine the number of entries, if we have ai_protocol or ai_socktype as unspec, return two
    size_t nb_results = 0;
    size_t requested_size = TUPLE_SIZE(2); // {ok, _}
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        // Each list item is:
        // 1 CONS
        // 1 IPv4 address
        // 1 map with 5 items (family, protocol, type, address, addr)
        // 1 map with 3 items (addr, port, family)
        requested_size += CONS_SIZE + INET_ADDR4_TUPLE_SIZE;
        // First result: regular maps
        // Subsequent results: shared maps
        if (nb_results) {
            requested_size += TERM_MAP_SHARED_SIZE(5) + TERM_MAP_SHARED_SIZE(3);
        } else {
            requested_size += TERM_MAP_SIZE(5) + TERM_MAP_SIZE(3);
        }
        nb_results++;
        // If protocol or socktype are unspecified (what esp-idf returns), add
        // another entry so we'll have tcp and udp
        if (p->ai_protocol == 0 || p->ai_socktype == 0) {
            nb_results++;
            // We only need cons and shared maps here as the IP address will be shared
            requested_size += CONS_SIZE + TERM_MAP_SHARED_SIZE(5) + TERM_MAP_SHARED_SIZE(3);
        }
    }
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term infos = term_nil();
    term result_keys = term_invalid_term();
    term addrinfo_keys = term_invalid_term();
    term family_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "family"));
    term inet_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "inet"));
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        term addrinfo_map;
        if (term_is_invalid_term(addrinfo_keys)) {
            addrinfo_map = term_alloc_map(3, &ctx->heap);
        } else {
            addrinfo_map = term_alloc_map_maybe_shared(3, addrinfo_keys, &ctx->heap);
        }
        // The inner addr contains a family, port, and addr
        term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
        term_set_map_assoc(addrinfo_map, 0, family_atom, inet_atom);
        term_set_map_assoc(addrinfo_map, 1, PORT_ATOM, term_from_int(port));
        term address = inet_make_addr4(ntohl(((struct sockaddr_in *) p->ai_addr)->sin_addr.s_addr), &ctx->heap);
        term_set_map_assoc(addrinfo_map, 2, addr_atom, address);

        if (term_is_invalid_term(addrinfo_keys)) {
            addrinfo_keys = term_get_map_keys(addrinfo_map);
        }

        if (p->ai_protocol != 0 && p->ai_socktype != 0) {
            term result_map = make_getaddrinfo_result(&result_keys, p->ai_protocol, p->ai_socktype, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(result_map, infos, &ctx->heap);
        } else {
            term tcp_map = make_getaddrinfo_result(&result_keys, IPPROTO_TCP, SOCK_STREAM, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(tcp_map, infos, &ctx->heap);
            term udp_map = make_getaddrinfo_result(&result_keys, IPPROTO_UDP, SOCK_DGRAM, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(udp_map, infos, &ctx->heap);
        }
    }
    freeaddrinfo(host_info);

    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, OK_ATOM);
    term_put_tuple_element(ret, 1, infos);

#ifdef ENABLE_TRACE
    fprintf(stdout, "host info: ");
    term_display(stdout, ret, ctx);
    fprintf(stdout, "\n");
#endif

    return ret;
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

void otp_net_init(GlobalContext *global)
{
    UNUSED(global);

    // noop
}
