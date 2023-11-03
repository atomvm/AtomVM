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

#ifndef _INET_COMMON_H_
#define _INET_COMMON_H_

#include <interop.h>
#include <memory.h>

#ifdef __cplusplus
extern "C" {
#endif

#define INET_ADDR4_TUPLE_SIZE TUPLE_SIZE(4)

enum inet_domain
{
    InetInvalidDomain = 0,
    InetDomain
};

/**
 * @brief Parse an inet domain atom
 * @param domain the inet domain atom
 * @param global the global context
 * @returns InetDomain or InetInvalidDomain
 */
enum inet_domain inet_atom_to_domain(term domain, GlobalContext *global);

enum inet_type
{
    InetInvalidType = 0,
    InetStreamType,
    InetDgramType
};

/**
 * @brief Parse an inet type
 * @param type the inet type atom
 * @param global the global context
 * @returns the parsed type
 */
enum inet_type inet_atom_to_type(term type, GlobalContext *global);

enum inet_protocol
{
    InetInvalidProtocol = 0,
    InetIpProtocol,
    InetTcpProtocol,
    InetUdpProtocol
};

/**
 * @brief Parse an inet protocol
 * @param type the inet protocol atom
 * @param global the global context
 * @returns the parsed protocol
 */
enum inet_protocol inet_atom_to_protocol(term protocol, GlobalContext *global);

/**
 * @brief Parse an inet IPv4 address tuple
 * @param addr_tuple    the tuple to parse
 * @returns the address as a uint32_t, in host order
 */
uint32_t inet_addr4_to_uint32(term addr_tuple);

/**
 * @brief Make an inet IPv4 address tuple
 * @param addr  the address to make as a tuple, as a uint32_t in host order
 * @param heap  the heap to build the tuple in
 * @returns the tuple
 * @details this function requires that at least INET_ADDR4_TUPLE_SIZE terms
 * are free on the heap.
 */
term inet_make_addr4(uint32_t addr, Heap *heap);

#ifdef __cplusplus
}
#endif

#endif
