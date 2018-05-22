/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

/**
 * @file nifs.h
 * @brief Private NIFs
 */

#ifndef _NIFS_H_
#define _NIFS_H_

#include "atom.h"
#include "context.h"
#include "exportedfunction.h"

const struct Nif *nifs_get(AtomString module, AtomString function, int arity);
term nif_erlang_open_port_2(Context *ctx, int argc, term argv[]);
term nif_erlang_register_2(Context *ctx, int argc, term argv[]);
term nif_erlang_whereis_1(Context *ctx, int argc, term argv[]);

native_handler platform_open_port(const char *driver_name);

#endif
