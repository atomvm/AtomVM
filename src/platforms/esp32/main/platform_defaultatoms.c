/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "platform_defaultatoms.h"

static const char *const set_level_atom = "\x9" "set_level";
static const char *const read_atom = "\x4" "read";
static const char *const input_atom = "\x5" "input";
static const char *const output_atom = "\x6" "output";
static const char *const set_direction_atom ="\xD" "set_direction";
static const char *const set_int_atom = "\x7" "set_int";
static const char *const gpio_interrupt_atom = "\xE" "gpio_interrupt";

static const char *const proto_atom = "\x5" "proto";
static const char *const udp_atom = "\x3" "udp";
static const char *const tcp_atom = "\x3" "tcp";
static const char *const socket_atom = "\x6" "socket";
static const char *const fcntl_atom = "\x5" "fcntl";
static const char *const bind_atom = "\x4" "bind";
static const char *const getsockname_atom = "\xB" "getsockname";
static const char *const recvfrom_atom = "\x8" "recvfrom";
static const char *const sendto_atom = "\x6" "sendto";

static const char *const sta_atom = "\x3" "sta";
static const char *const ssid_atom = "\x4" "ssid";
static const char *const psk_atom = "\x3" "psk";
static const char *const sntp_atom = "\x4" "sntp";
static const char *const sta_got_ip_atom = "\xA" "sta_got_ip";
static const char *const sta_connected_atom = "\xD" "sta_connected";
static const char *const sta_disconnected_atom = "\x10" "sta_disconnected";

void platform_defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, set_level_atom) == SET_LEVEL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, read_atom) == READ_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, input_atom) == INPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, output_atom) == OUTPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_direction_atom) == SET_DIRECTION_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_int_atom) == SET_INT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, gpio_interrupt_atom) == GPIO_INTERRUPT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, proto_atom) == PROTO_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, udp_atom) == UDP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, tcp_atom) == TCP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, socket_atom) == SOCKET_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fcntl_atom) == FCNTL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, bind_atom) == BIND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, getsockname_atom) == GETSOCKNAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, recvfrom_atom) == RECVFROM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sendto_atom) == SENDTO_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, sta_atom) == STA_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ssid_atom) == SSID_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, psk_atom) == PSK_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sntp_atom) == SNTP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_got_ip_atom) == STA_GOT_IP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_connected_atom) == STA_CONNECTED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, sta_disconnected_atom) == STA_DISCONNECTED_ATOM_INDEX;

    if (!ok) {
        abort();
    }
}
