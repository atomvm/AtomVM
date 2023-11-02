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

#include "inet.h"
#include "term.h"

#include <port.h>

static const AtomStringIntPair inet_domain_table[] = {
    { ATOM_STR("\x4", "inet"), InetDomain },
    SELECT_INT_DEFAULT(InetInvalidDomain)
};

enum inet_domain inet_atom_to_domain(term domain, GlobalContext *global)
{
    return interop_atom_term_select_int(inet_domain_table, domain, global);
}

static const AtomStringIntPair inet_type_table[] = {
    { ATOM_STR("\x6", "stream"), InetStreamType },
    { ATOM_STR("\x5", "dgram"), InetDgramType },
    SELECT_INT_DEFAULT(InetInvalidType)
};

enum inet_type inet_atom_to_type(term type, GlobalContext *global)
{
    return interop_atom_term_select_int(inet_type_table, type, global);
}

static const AtomStringIntPair inet_protocol_table[] = {
    { ATOM_STR("\x2", "ip"), InetIpProtocol },
    { ATOM_STR("\x3", "tcp"), InetTcpProtocol },
    { ATOM_STR("\x3", "udp"), InetUdpProtocol },
    SELECT_INT_DEFAULT(InetInvalidProtocol)
};

enum inet_protocol inet_atom_to_protocol(term protocol, GlobalContext *global)
{
    return interop_atom_term_select_int(inet_protocol_table, protocol, global);
}

uint32_t inet_addr4_to_uint32(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

term inet_make_addr4(uint32_t addr, Heap *heap)
{
    term result = term_alloc_tuple(4, heap);
    term_put_tuple_element(result, 0, term_from_int32((addr >> 24) & 0xFF));
    term_put_tuple_element(result, 1, term_from_int32((addr >> 16) & 0xFF));
    term_put_tuple_element(result, 2, term_from_int32((addr >> 8) & 0xFF));
    term_put_tuple_element(result, 3, term_from_int32(addr & 0xFF));
    return result;
}
