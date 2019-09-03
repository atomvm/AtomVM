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

#define PROTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 0)
#define UDP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 1)
#define TCP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 2)
#define SOCKET_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 3)
#define FCNTL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 4)
#define BIND_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 5)
#define GETSOCKNAME_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 6)
#define RECVFROM_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 7)
#define SENDTO_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 8)

#define STA_GOT_IP_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 9)
#define STA_CONNECTED_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 10)

#define ADDRESS_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 11)
#define PORT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 12)
#define CONTROLLING_PROCESS_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 13)
#define BINARY_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 14)
#define ACTIVE_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 15)
#define BUFFER_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 16)

#define PROTO_ATOM term_from_atom_index(PROTO_ATOM_INDEX)
#define UDP_ATOM term_from_atom_index(UDP_ATOM_INDEX)
#define TCP_ATOM term_from_atom_index(TCP_ATOM_INDEX)
#define SOCKET_ATOM term_from_atom_index(SOCKET_ATOM_INDEX)
#define FCNTL_ATOM term_from_atom_index(FCNTL_ATOM_INDEX)
#define BIND_ATOM term_from_atom_index(BIND_ATOM_INDEX)
#define GETSOCKNAME_ATOM term_from_atom_index(GETSOCKNAME_ATOM_INDEX)
#define RECVFROM_ATOM term_from_atom_index(RECVFROM_ATOM_INDEX)
#define SENDTO_ATOM term_from_atom_index(SENDTO_ATOM_INDEX)

#define STA_GOT_IP_ATOM term_from_atom_index(STA_GOT_IP_ATOM_INDEX)
#define STA_CONNECTED_ATOM term_from_atom_index(STA_CONNECTED_ATOM_INDEX)

#define ADDRESS_ATOM term_from_atom_index(ADDRESS_ATOM_INDEX)
#define PORT_ATOM term_from_atom_index(PORT_ATOM_INDEX)
#define CONTROLLING_PROCESS_ATOM term_from_atom_index(CONTROLLING_PROCESS_ATOM_INDEX)
#define BINARY_ATOM term_from_atom_index(BINARY_ATOM_INDEX)
#define ACTIVE_ATOM term_from_atom_index(ACTIVE_ATOM_INDEX)
#define BUFFER_ATOM term_from_atom_index(BUFFER_ATOM_INDEX)

#endif
