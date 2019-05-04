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

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

#define SET_LEVEL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 0)
#define INPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 1)
#define OUTPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 2)
#define SET_DIRECTION_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 3)
#define SET_INT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 4)
#define GPIO_INTERRUPT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 5)

#define PROTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 6)
#define UDP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 7)
#define TCP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 8)
#define SOCKET_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 9)
#define FCNTL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 10)
#define BIND_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 11)
#define GETSOCKNAME_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 12)
#define RECVFROM_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 13)
#define SENDTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 14)

#define STA_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 15)
#define SSID_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 16)
#define PSK_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 17)
#define SNTP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 18)
#define STA_GOT_IP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 19)
#define STA_CONNECTED_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 20)
#define STA_DISCONNECTED_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 21)

#define SET_LEVEL_ATOM term_from_atom_index(SET_LEVEL_ATOM_INDEX)
#define INPUT_ATOM term_from_atom_index(INPUT_ATOM_INDEX)
#define OUTPUT_ATOM term_from_atom_index(OUTPUT_ATOM_INDEX)
#define SET_DIRECTION_ATOM term_from_atom_index(SET_DIRECTION_ATOM_INDEX)
#define SET_INT_ATOM term_from_atom_index(SET_INT_ATOM_INDEX)
#define GPIO_INTERRUPT_ATOM term_from_atom_index(GPIO_INTERRUPT_ATOM_INDEX)

#define PROTO_ATOM term_from_atom_index(PROTO_ATOM_INDEX)
#define UDP_ATOM term_from_atom_index(UDP_ATOM_INDEX)
#define TCP_ATOM term_from_atom_index(TCP_ATOM_INDEX)
#define SOCKET_ATOM term_from_atom_index(SOCKET_ATOM_INDEX)
#define FCNTL_ATOM term_from_atom_index(FCNTL_ATOM_INDEX)
#define BIND_ATOM term_from_atom_index(BIND_ATOM_INDEX)
#define GETSOCKNAME_ATOM term_from_atom_index(GETSOCKNAME_ATOM_INDEX)
#define RECVFROM_ATOM term_from_atom_index(RECVFROM_ATOM_INDEX)
#define SENDTO_ATOM term_from_atom_index(SENDTO_ATOM_INDEX)

#define STA_ATOM term_from_atom_index(STA_ATOM_INDEX)
#define SSID_ATOM term_from_atom_index(SSID_ATOM_INDEX)
#define PSK_ATOM term_from_atom_index(PSK_ATOM_INDEX)
#define SNTP_ATOM term_from_atom_index(SNTP_ATOM_INDEX)
#define STA_GOT_IP_ATOM term_from_atom_index(STA_GOT_IP_ATOM_INDEX)
#define STA_CONNECTED_ATOM term_from_atom_index(STA_CONNECTED_ATOM_INDEX)
#define STA_DISCONNECTED_ATOM term_from_atom_index(STA_DISCONNECTED_ATOM_INDEX)

#endif
