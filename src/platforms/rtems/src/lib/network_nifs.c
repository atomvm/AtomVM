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

static const struct Nif *network_nif_get_nif(const char *nifname)
{
    if (strcmp("atomvm_rtems:ifaddrs/0", nifname) == 0) {
        return &rtems_ifaddrs_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(rtems_network, NULL, NULL, network_nif_get_nif)
