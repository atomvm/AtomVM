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
 * @file bif.h
 * @brief BIF private functions.
 */

#ifndef _BIF_H_
#define _BIF_H_

#include "atom.h"
#include "context.h"
#include "exportedfunction.h"
#include "module.h"

#define MAX_BIF_NAME_LEN 32

BifImpl bif_registry_get_handler(AtomString module, AtomString function, int arity);
term bif_erlang_self_0(Context *ctx);
term bif_erlang_byte_size_1(Context *ctx, uint32_t failure_label, int live, term arg1);
term bif_erlang_length_1(Context *ctx, uint32_t failure_label, int live, term arg1);

term bif_erlang_is_atom_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_binary_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_integer_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_list_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_number_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_pid_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_reference_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_is_tuple_1(Context *ctx, uint32_t failure_label, term arg1);

term bif_erlang_hd_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_tuple_size_1(Context *ctx, uint32_t failure_label, term arg1);

term bif_erlang_add_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_sub_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_mul_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_div_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_rem_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_neg_1(Context *ctx, uint32_t failure_label, int live, term arg1);

term bif_erlang_bor_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_band_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_bxor_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_bsl_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_bsr_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2);
term bif_erlang_bnot_1(Context *ctx, uint32_t failure_label, int live, term arg1);

term bif_erlang_not_1(Context *ctx, uint32_t failure_label, term arg1);
term bif_erlang_and_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_or_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_xor_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);

term bif_erlang_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_not_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);

term bif_erlang_exactly_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_exactly_not_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);

term bif_erlang_greater_than_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_less_than_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_less_than_or_equal_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);
term bif_erlang_greater_than_or_equal_2(Context *ctx, uint32_t failure_label, term arg1, term arg2);

#endif
