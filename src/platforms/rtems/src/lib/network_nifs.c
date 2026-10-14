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

#include "network.h"

#include <string.h>

#ifdef RTEMS_HAS_LIBBSD
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>
#include <utils.h>

#include "wifi.h"
#include <interop.h>

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term error_atom(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

static term error_term(Context *ctx, term reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return create_pair(ctx, ERROR_ATOM, reason);
}

static term nif_rtems_ifaddrs(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

#ifdef RTEMS_HAS_LIBBSD
    struct ifaddrs *addrs = NULL;
    if (getifaddrs(&addrs) != 0) {
        return error_atom(ctx, ATOM_STR("\x7", "eagain"));
    }

    size_t count = 0;
    for (struct ifaddrs *ifa = addrs; ifa != NULL; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr != NULL && ifa->ifa_addr->sa_family == AF_INET) {
            count++;
        }
    }

    size_t needed = TUPLE_SIZE(2)
        + LIST_SIZE(count, TUPLE_SIZE(3) + TUPLE_SIZE(4) + IFNAMSIZ * CONS_SIZE);
    if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
        freeifaddrs(addrs);
        return OUT_OF_MEMORY_ATOM;
    }

    term list = term_nil();
    for (struct ifaddrs *ifa = addrs; ifa != NULL; ifa = ifa->ifa_next) {
        if (ifa->ifa_addr == NULL || ifa->ifa_addr->sa_family != AF_INET || ifa->ifa_name == NULL) {
            continue;
        }
        size_t name_len = strlen(ifa->ifa_name);
        if (name_len == 0 || name_len > IFNAMSIZ) {
            continue;
        }
        struct sockaddr_in *sin = (struct sockaddr_in *) ifa->ifa_addr;
        uint32_t ip = ntohl(sin->sin_addr.s_addr);
        term addr = term_alloc_tuple(4, &ctx->heap);
        term_put_tuple_element(addr, 0, term_from_int((ip >> 24) & 0xFF));
        term_put_tuple_element(addr, 1, term_from_int((ip >> 16) & 0xFF));
        term_put_tuple_element(addr, 2, term_from_int((ip >> 8) & 0xFF));
        term_put_tuple_element(addr, 3, term_from_int(ip & 0xFF));

        term name = term_from_string((const uint8_t *) ifa->ifa_name, (uint16_t) name_len, &ctx->heap);
        term flags = term_from_int(ifa->ifa_flags);
        term entry = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(entry, 0, name);
        term_put_tuple_element(entry, 1, addr);
        term_put_tuple_element(entry, 2, flags);
        list = term_list_prepend(entry, list, &ctx->heap);
    }
    freeifaddrs(addrs);
    return create_pair(ctx, OK_ATOM, list);
#else
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_ifaddrs_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_ifaddrs
};

static term nif_rtems_resolver_ready(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

#ifdef RTEMS_HAS_LIBBSD
    return rtems_atomvm_resolver_ready() ? TRUE_ATOM : FALSE_ATOM;
#elif defined(RTEMS_HAS_LWIP)
    return TRUE_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_resolver_ready_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_resolver_ready
};

static term nif_rtems_wifi_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
#if (defined(RTEMS_HAS_LIBBSD) && defined(RTEMS_HAS_WIFI)) || defined(RTEMS_WIFI_BACKEND_ESP32)
    int ok = 0;
    char *ssid = interop_term_to_string(argv[0], &ok);
    if (!ok || ssid == NULL) {
        return error_term(ctx, BADARG_ATOM);
    }
    char *psk = NULL;
    if (argv[1] != NONE_ATOM && argv[1] != UNDEFINED_ATOM) {
        psk = interop_term_to_string(argv[1], &ok);
        if (!ok) {
            free(ssid);
            return error_term(ctx, BADARG_ATOM);
        }
    }
    int rv = rtems_wifi_start(ssid, psk);
    free(ssid);
    free(psk);
    return rv == 0 ? OK_ATOM : error_atom(ctx, ATOM_STR("\x5", "eio"));
#else
    (void) argv;
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_wifi_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_wifi_start
};

static term nif_rtems_wifi_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);
#if (defined(RTEMS_HAS_LIBBSD) && defined(RTEMS_HAS_WIFI)) || defined(RTEMS_WIFI_BACKEND_ESP32)
    rtems_wifi_stop();
    return OK_ATOM;
#else
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_wifi_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_wifi_stop
};

static term nif_rtems_wifi_status(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
#if (defined(RTEMS_HAS_LIBBSD) && defined(RTEMS_HAS_WIFI)) || defined(RTEMS_WIFI_BACKEND_ESP32)
    rtems_wifi_state_t state = rtems_wifi_get_state();
    struct rtems_wifi_ip_info ip_info = {0};
    rtems_wifi_get_ip_info(&ip_info);

    size_t needed = TUPLE_SIZE(2) + TUPLE_SIZE(4) * 3 + TUPLE_SIZE(2) * 4 + CONS_SIZE * 4;
    if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }

    term state_atom;
    switch (state) {
        case RTEMS_WIFI_SCANNING:
            state_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "scanning"));
            break;
        case RTEMS_WIFI_ASSOCIATING:
            state_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "associating"));
            break;
        case RTEMS_WIFI_ASSOCIATED:
            state_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "associated"));
            break;
        case RTEMS_WIFI_GOT_IP:
            state_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "got_ip"));
            break;
        default:
            state_atom = globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "disconnected"));
            break;
    }

    term ip = term_alloc_tuple(4, &ctx->heap);
    term_put_tuple_element(ip, 0, term_from_int((ip_info.ip >> 24) & 0xFF));
    term_put_tuple_element(ip, 1, term_from_int((ip_info.ip >> 16) & 0xFF));
    term_put_tuple_element(ip, 2, term_from_int((ip_info.ip >> 8) & 0xFF));
    term_put_tuple_element(ip, 3, term_from_int(ip_info.ip & 0xFF));

    term netmask = term_alloc_tuple(4, &ctx->heap);
    term_put_tuple_element(netmask, 0, term_from_int((ip_info.netmask >> 24) & 0xFF));
    term_put_tuple_element(netmask, 1, term_from_int((ip_info.netmask >> 16) & 0xFF));
    term_put_tuple_element(netmask, 2, term_from_int((ip_info.netmask >> 8) & 0xFF));
    term_put_tuple_element(netmask, 3, term_from_int(ip_info.netmask & 0xFF));

    term gw = term_alloc_tuple(4, &ctx->heap);
    term_put_tuple_element(gw, 0, term_from_int((ip_info.gateway >> 24) & 0xFF));
    term_put_tuple_element(gw, 1, term_from_int((ip_info.gateway >> 16) & 0xFF));
    term_put_tuple_element(gw, 2, term_from_int((ip_info.gateway >> 8) & 0xFF));
    term_put_tuple_element(gw, 3, term_from_int(ip_info.gateway & 0xFF));

    term list = term_nil();
    list = term_list_prepend(create_pair(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "gateway")), gw), list, &ctx->heap);
    list = term_list_prepend(create_pair(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "netmask")), netmask), list, &ctx->heap);
    list = term_list_prepend(create_pair(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "ip")), ip), list, &ctx->heap);
    list = term_list_prepend(create_pair(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "state")), state_atom), list, &ctx->heap);

    return create_pair(ctx, OK_ATOM, list);
#else
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_wifi_status_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_wifi_status
};

static const struct Nif *network_nif_get_nif(const char *nifname)
{
    if (strcmp("atomvm_rtems:ifaddrs/0", nifname) == 0) {
        return &rtems_ifaddrs_nif;
    }
    if (strcmp("atomvm_rtems:resolver_ready/0", nifname) == 0) {
        return &rtems_resolver_ready_nif;
    }
    if (strcmp("atomvm_rtems:wifi_start/2", nifname) == 0) {
        return &rtems_wifi_start_nif;
    }
    if (strcmp("atomvm_rtems:wifi_stop/0", nifname) == 0) {
        return &rtems_wifi_stop_nif;
    }
    if (strcmp("atomvm_rtems:wifi_status/0", nifname) == 0) {
        return &rtems_wifi_status_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtems_network, NULL, NULL, network_nif_get_nif)
