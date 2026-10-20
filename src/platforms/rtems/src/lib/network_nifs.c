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
#include "sntp.h"

#include <string.h>
#include <time.h>

#ifdef RTEMS_HAS_LIBBSD
#include <arpa/inet.h>
#include <ifaddrs.h>
#include <net/if.h>
#include <netinet/in.h>
#include <sys/socket.h>
#elif defined(RTEMS_HAS_LWIP)
#include <lwip/inet.h>
#include <lwip/ip4_addr.h>
#include <lwip/netif.h>
#include <stdio.h>
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
#ifdef RTEMS_HAS_BT
#include "rtems_bt.h"
#endif
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
#elif defined(RTEMS_HAS_LWIP)
    size_t count = 0;
    for (struct netif *netif = netif_list; netif != NULL; netif = netif->next) {
        count++;
    }

    size_t needed = TUPLE_SIZE(2)
        + LIST_SIZE(count, TUPLE_SIZE(3) + TUPLE_SIZE(4) + 8 * CONS_SIZE);
    if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }

    term list = term_nil();
    for (struct netif *netif = netif_list; netif != NULL; netif = netif->next) {
        char name_buf[8];
        if (netif->name[0] == 'w' && netif->name[1] == 'l') {
            /* Expose the station name used by wait_wifi(), independent of lwIP index. */
            strcpy(name_buf, "wlan0");
        } else {
            snprintf(name_buf, sizeof(name_buf), "%c%c%u", netif->name[0], netif->name[1], netif->num);
        }
        uint32_t ip = lwip_ntohl(ip4_addr_get_u32(netif_ip4_addr(netif)));
        term addr = term_alloc_tuple(4, &ctx->heap);
        term_put_tuple_element(addr, 0, term_from_int((ip >> 24) & 0xFF));
        term_put_tuple_element(addr, 1, term_from_int((ip >> 16) & 0xFF));
        term_put_tuple_element(addr, 2, term_from_int((ip >> 8) & 0xFF));
        term_put_tuple_element(addr, 3, term_from_int(ip & 0xFF));

        term name = term_from_string((const uint8_t *) name_buf, (uint16_t) strlen(name_buf), &ctx->heap);
        term flags = term_from_int(netif->flags);
        term entry = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(entry, 0, name);
        term_put_tuple_element(entry, 1, addr);
        term_put_tuple_element(entry, 2, flags);
        list = term_list_prepend(entry, list, &ctx->heap);
    }
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

static term rtems_wifi_start_impl(Context *ctx, term ssid_term, term psk_term, bool connect)
{
#if (defined(RTEMS_HAS_LIBBSD) && defined(RTEMS_HAS_WIFI)) || defined(RTEMS_WIFI_BACKEND_ESP32)
    int ok = 0;
    char *ssid = interop_term_to_string(ssid_term, &ok);
    if (!ok || ssid == NULL) {
        return error_term(ctx, BADARG_ATOM);
    }
    char *psk = NULL;
    if (psk_term != NONE_ATOM && psk_term != UNDEFINED_ATOM) {
        psk = interop_term_to_string(psk_term, &ok);
        if (!ok) {
            free(ssid);
            return error_term(ctx, BADARG_ATOM);
        }
    }
    int rv = rtems_wifi_start(ssid, psk, connect);
    free(ssid);
    free(psk);
    return rv == 0 ? OK_ATOM : error_atom(ctx, ATOM_STR("\x5", "eio"));
#else
    (void) ssid_term;
    (void) psk_term;
    (void) connect;
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static term nif_rtems_wifi_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return rtems_wifi_start_impl(ctx, argv[0], argv[1], true);
}

static const struct Nif rtems_wifi_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_wifi_start
};

static term nif_rtems_wifi_start3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (argv[2] != TRUE_ATOM && argv[2] != FALSE_ATOM) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return rtems_wifi_start_impl(ctx, argv[0], argv[1], argv[2] == TRUE_ATOM);
}

static const struct Nif rtems_wifi_start3_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_wifi_start3
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
    struct rtems_wifi_ip_info ip_info = { 0 };
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

static term nif_rtems_bt_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
#ifdef RTEMS_HAS_BT
    return rtems_bt_start_async() ? OK_ATOM : error_atom(ctx, ATOM_STR("\x3", "eio"));
#else
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_bt_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_bt_start
};

static term nif_rtems_bt_status(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
#ifdef RTEMS_HAS_BT
    switch (rtems_bt_get_state()) {
        case RTEMS_BT_STOPPED:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "stopped"));
        case RTEMS_BT_STARTING:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "starting"));
        case RTEMS_BT_STARTED:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "started"));
        case RTEMS_BT_FAILED:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "failed"));
    }
#endif
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
}

static const struct Nif rtems_bt_status_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_bt_status
};

static term nif_rtems_sntp_sync(Context *ctx, int argc, term argv[])
{
#if defined(RTEMS_HAS_LIBBSD) || defined(RTEMS_HAS_LWIP)
    char *host = NULL;
    int port = 123;

    if (argc >= 1) {
        int ok = 0;
        host = interop_term_to_string(argv[0], &ok);
        if (!ok || host == NULL) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }
    if (argc == 2) {
        if (!term_is_integer(argv[1])) {
            free(host);
            RAISE_ERROR(BADARG_ATOM);
        }
        port = term_to_int(argv[1]);
    }

    struct timeval tv;
    int res = rtems_sntp_sync_now(host, port, 4000, &tv);
    free(host);

    if (res != 0) {
        return error_atom(ctx, ATOM_STR("\x7", "timeout"));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + BOXED_INT64_SIZE * 2) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }

    term tv_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(tv_tuple, 0, term_make_maybe_boxed_int64(tv.tv_sec, &ctx->heap));
    term_put_tuple_element(tv_tuple, 1, term_make_maybe_boxed_int64(tv.tv_usec, &ctx->heap));

    return create_pair(ctx, OK_ATOM, tv_tuple);
#else
    UNUSED(argc);
    UNUSED(argv);
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
#endif
}

static const struct Nif rtems_sntp_sync_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_sntp_sync
};

static term nif_rtems_set_clock(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    if (!term_is_any_integer(argv[0]) || !term_is_any_integer(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t sec = term_maybe_unbox_int64(argv[0]);
    int64_t usec = term_maybe_unbox_int64(argv[1]);

    struct timespec ts;
    ts.tv_sec = (time_t) sec;
    ts.tv_nsec = (long) usec * 1000L;
    if (clock_settime(CLOCK_REALTIME, &ts) != 0) {
        return error_atom(ctx, ATOM_STR("\x5", "error"));
    }
    return OK_ATOM;
}

static const struct Nif rtems_set_clock_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_rtems_set_clock
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
    if (strcmp("atomvm_rtems:wifi_start/3", nifname) == 0) {
        return &rtems_wifi_start3_nif;
    }
    if (strcmp("atomvm_rtems:wifi_stop/0", nifname) == 0) {
        return &rtems_wifi_stop_nif;
    }
    if (strcmp("atomvm_rtems:wifi_status/0", nifname) == 0) {
        return &rtems_wifi_status_nif;
    }
    if (strcmp("atomvm_rtems:bt_start_async/0", nifname) == 0) {
        return &rtems_bt_start_nif;
    }
    if (strcmp("atomvm_rtems:bt_status/0", nifname) == 0) {
        return &rtems_bt_status_nif;
    }
    if (strcmp("atomvm_rtems:sntp_sync/1", nifname) == 0) {
        return &rtems_sntp_sync_nif;
    }
    if (strcmp("atomvm_rtems:sntp_sync/2", nifname) == 0) {
        return &rtems_sntp_sync_nif;
    }
    if (strcmp("atomvm_rtems:set_clock/2", nifname) == 0) {
        return &rtems_set_clock_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtems_network, NULL, NULL, network_nif_get_nif)
