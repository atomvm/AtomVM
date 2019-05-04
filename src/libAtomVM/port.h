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

#ifndef _PORT_H_
#define _PORT_H_

#include "globalcontext.h"
#include "context.h"
#include "term.h"
#include "defaultatoms.h"

term port_create_tuple2(Context *ctx, term a, term b);
term port_create_tuple3(Context *ctx, term a, term b, term c);
term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms);
term port_create_error_tuple(Context *ctx, term reason);
term port_create_sys_error_tuple(Context *ctx, term syscall, int errno);
term port_create_ok_tuple(Context *ctx, term t);
void port_send_reply(Context *ctx, term pid, term ref, term reply);
void port_ensure_available(Context *ctx, size_t size);
int port_is_standard_port_command(term msg);

#endif
