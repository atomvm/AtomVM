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

#include "platform_defaultatoms.h"

static const char *const proto_atom = "\x5" "proto";
static const char *const udp_atom = "\x3" "udp";
static const char *const tcp_atom = "\x3" "tcp";
static const char *const socket_atom = "\x6" "socket";
static const char *const fcntl_atom = "\x5" "fcntl";
static const char *const bind_atom = "\x4" "bind";
static const char *const getpeername_atom = "\xB" "getpeername";
static const char *const getsockname_atom = "\xB" "getsockname";
static const char *const recvfrom_atom = "\x8" "recvfrom";
static const char *const recv_atom = "\x4" "recv";
static const char *const sendto_atom = "\x6" "sendto";
static const char *const send_atom = "\x4" "send";

static const char *const sta_got_ip_atom = "\xA" "sta_got_ip";
static const char *const sta_connected_atom = "\xD" "sta_connected";

static const char *const address_atom = "\x7" "address";
static const char *const controlling_process_atom = "\x13" "controlling_process";
static const char *const active_atom = "\x6" "active";
static const char *const buffer_atom = "\x6" "buffer";

static const char *const getaddrinfo_atom = "\xB" "getaddrinfo";
static const char *const no_such_host_atom = "\xC" "no_such_host";
static const char *const connect_atom = "\x7" "connect";
static const char *const tcp_closed_atom = "\xA" "tcp_closed";

static const char *const listen_atom = "\x6" "listen";
static const char *const backlog_atom = "\x7" "backlog";
static const char *const accept_atom = "\x6" "accept";
static const char *const fd_atom = "\x2" "fd";
static const char *const generic_unix_atom = "\xC" "generic_unix";

void platform_defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, proto_atom) == PROTO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, udp_atom) == UDP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, tcp_atom) == TCP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, socket_atom) == SOCKET_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fcntl_atom) == FCNTL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, bind_atom) == BIND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, getpeername_atom) == GETPEERNAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, getsockname_atom) == GETSOCKNAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, recvfrom_atom) == RECVFROM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, recv_atom) == RECV_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sendto_atom) == SENDTO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, send_atom) == SEND_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, sta_got_ip_atom) == STA_GOT_IP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_connected_atom) == STA_CONNECTED_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, address_atom) == ADDRESS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, controlling_process_atom) == CONTROLLING_PROCESS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, active_atom) == ACTIVE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, buffer_atom) == BUFFER_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, getaddrinfo_atom) == GETADDRINFO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, no_such_host_atom) == NO_SUCH_HOST_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, connect_atom) == CONNECT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, tcp_closed_atom) == TCP_CLOSED_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, listen_atom) == LISTEN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, backlog_atom) == BACKLOG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, accept_atom) == ACCEPT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fd_atom) == FD_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, generic_unix_atom) == GENERIC_UNIX_ATOM_INDEX;

    if (!ok) {
        AVM_ABORT();
    }
}
