/***************************************************************************
 *   Copyright 2018 by Fred Dushin <fred@dushin.net>                       *
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

#ifndef _SOCKET_DRIVER_H_
#define _SOCKET_DRIVER_H_

#include "context.h"
#include "term.h"

void *socket_driver_create_data();
void socket_driver_delete_data(void *data);

term socket_driver_do_init(Context *ctx, term params);
term socket_driver_do_bind(Context *ctx, term address, term port);
term socket_driver_do_send(Context *ctx, term dest_address, term dest_port, term buffer);
void socket_driver_do_recvfrom(Context *ctx, term pid, term ref);

#endif
