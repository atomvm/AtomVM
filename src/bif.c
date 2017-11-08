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

#include "bif.h"

#include <stdlib.h>

#include "bifs_hash.h"


typedef const void * AtomString;

BifImpl bif_registry_get_handler(AtomString module, AtomString function, int arity)
{
    char bifname[MAX_BIF_NAME_LEN];

    int module_name_len = atom_string_len(module);
    memcpy(bifname, atom_string_data(module), module_name_len);

    bifname[module_name_len] = ':';

    int function_name_len = atom_string_len(function);
    memcpy(bifname + module_name_len + 1, atom_string_data(function), function_name_len);

    bifname[module_name_len + function_name_len + 1] = '\\';
    bifname[module_name_len + function_name_len + 2] = '0' + arity;
    bifname[module_name_len + function_name_len + 3] = 0;

    BifNameAndPtr *nameAndPtr = in_word_set(bifname, strlen(bifname));
    if (!nameAndPtr) {
        fprintf(stderr, "bif not found\n");
        return 0;
    }

    return nameAndPtr->function;
}

int bif_registry_is_bif(AtomString module_atom, AtomString function_atom, uint32_t arity)
{
    return 1;
}

void bif_erlang_add_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg)
{
    if (term_is_integer(arg1) && term_is_integer(arg2)) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        ctx->x[reg] = term_from_int32(term_to_int32(arg1) + term_to_int32(arg2));

    } else {
        printf("add: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

void bif_erlang_sub_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg)
{
    if (term_is_integer(arg1) && term_is_integer(arg2)) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        ctx->x[reg] = term_from_int32(term_to_int32(arg1) - term_to_int32(arg2));

    } else {
        printf("sub: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

void bif_erlang_mul_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2, int reg)
{
    if (term_is_integer(arg1) && term_is_integer(arg2)) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        ctx->x[reg] = term_from_int32(term_to_int32(arg1) * term_to_int32(arg2));

    } else {
        printf("mul: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

