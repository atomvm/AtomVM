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

#ifndef _BIF_H_
#define _BIF_H_

#include "atom.h"
#include "Context.h"
#include "Module.h"

#define MAX_BIF_NAME_LEN 32

extern BifImpl bif_registry_get_handler(AtomString module, AtomString function, int arity);
extern int bif_registry_is_bif(AtomString module_atom, AtomString function_atom, uint32_t arity);
extern void bif_erlang_add_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg);
extern void bif_erlang_sub_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg);
extern void bif_erlang_mul_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg);

#endif
