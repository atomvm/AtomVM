/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

#define PROTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 0)
#define UDP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 1)
#define TCP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 2)
#define SOCKET_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 3)
#define FCNTL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 4)
#define BIND_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 5)
#define GETPEERNAME_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 6)
#define GETSOCKNAME_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 7)
#define RECVFROM_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 8)
#define RECV_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 9)
#define SENDTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 10)
#define SEND_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 11)

#define STA_GOT_IP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 12)
#define STA_CONNECTED_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 13)

#define ADDRESS_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 14)
#define CONTROLLING_PROCESS_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 15)
#define ACTIVE_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 16)
#define BUFFER_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 17)

#define GETADDRINFO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 18)
#define NO_SUCH_HOST_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 19)
#define CONNECT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 20)
#define TCP_CLOSED_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 21)

#define LISTEN_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 22)
#define BACKLOG_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 23)
#define ACCEPT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 24)
#define FD_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 25)
#define GENERIC_UNIX_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 26)

#define PROTO_ATOM term_from_atom_index(PROTO_ATOM_INDEX)
#define UDP_ATOM term_from_atom_index(UDP_ATOM_INDEX)
#define TCP_ATOM term_from_atom_index(TCP_ATOM_INDEX)
#define SOCKET_ATOM term_from_atom_index(SOCKET_ATOM_INDEX)
#define FCNTL_ATOM term_from_atom_index(FCNTL_ATOM_INDEX)
#define BIND_ATOM term_from_atom_index(BIND_ATOM_INDEX)
#define GETPEERNAME_ATOM term_from_atom_index(GETPEERNAME_ATOM_INDEX)
#define GETSOCKNAME_ATOM term_from_atom_index(GETSOCKNAME_ATOM_INDEX)
#define RECVFROM_ATOM term_from_atom_index(RECVFROM_ATOM_INDEX)
#define RECV_ATOM term_from_atom_index(RECV_ATOM_INDEX)
#define SENDTO_ATOM term_from_atom_index(SENDTO_ATOM_INDEX)
#define SEND_ATOM term_from_atom_index(SEND_ATOM_INDEX)

#define STA_GOT_IP_ATOM term_from_atom_index(STA_GOT_IP_ATOM_INDEX)
#define STA_CONNECTED_ATOM term_from_atom_index(STA_CONNECTED_ATOM_INDEX)

#define ADDRESS_ATOM term_from_atom_index(ADDRESS_ATOM_INDEX)
#define CONTROLLING_PROCESS_ATOM term_from_atom_index(CONTROLLING_PROCESS_ATOM_INDEX)
#define ACTIVE_ATOM term_from_atom_index(ACTIVE_ATOM_INDEX)
#define BUFFER_ATOM term_from_atom_index(BUFFER_ATOM_INDEX)

#define GETADDRINFO_ATOM term_from_atom_index(GETADDRINFO_ATOM_INDEX)
#define NO_SUCH_HOST_ATOM term_from_atom_index(NO_SUCH_HOST_ATOM_INDEX)
#define CONNECT_ATOM term_from_atom_index(CONNECT_ATOM_INDEX)
#define TCP_CLOSED_ATOM term_from_atom_index(TCP_CLOSED_ATOM_INDEX)

#define LISTEN_ATOM term_from_atom_index(LISTEN_ATOM_INDEX)
#define BACKLOG_ATOM term_from_atom_index(BACKLOG_ATOM_INDEX)
#define ACCEPT_ATOM term_from_atom_index(ACCEPT_ATOM_INDEX)
#define FD_ATOM term_from_atom_index(FD_ATOM_INDEX)
#define GENERIC_UNIX_ATOM TERM_FROM_ATOM_INDEX(GENERIC_UNIX_ATOM_INDEX)

#endif
