/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

/**
 * @file bif.h
 * @brief BIF private functions.
 */

#ifndef _BIF_H_
#define _BIF_H_

#include <stdbool.h>

#include "atom.h"
#include "context.h"
#include "exportedfunction.h"
#include "module.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_BIF_NAME_LEN 260

const struct ExportedFunction *bif_registry_get_handler(AtomString module, AtomString function, int arity);

term bif_erlang_self_0(Context *ctx);
term bif_erlang_node_0(Context *ctx);
term bif_erlang_byte_size_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_bit_size_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_binary_part_3(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, term arg3);
term bif_erlang_length_1(Context *ctx, uint32_t fail_label, int live, term arg1);

term bif_erlang_is_atom_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_binary_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_boolean_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_float_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_function_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_function_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_is_integer_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_list_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_number_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_pid_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_port_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_reference_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_tuple_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_record_2(Context *ctx, uint32_t fail_label, term arg1, term record_tag);
term bif_erlang_is_map_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_is_map_key_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_hd_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_tl_1(Context *ctx, uint32_t fail_label, term arg1);

term bif_erlang_element_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_tuple_size_1(Context *ctx, uint32_t fail_label, term arg1);

term bif_erlang_map_size_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_map_get_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_unique_integer_0(Context *ctx);
term bif_erlang_unique_integer_1(Context *ctx, uint32_t fail_label, term arg1);

term bif_erlang_add_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_plus_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_sub_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_mul_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_fdiv_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_div_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_rem_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_neg_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_abs_1(Context *ctx, uint32_t fail_label, int live, term arg1);

term bif_erlang_ceil_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_floor_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_round_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_trunc_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_float_1(Context *ctx, uint32_t fail_label, int live, term arg1);

term bif_erlang_bor_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_band_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_bxor_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_bsl_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_bsr_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_bnot_1(Context *ctx, uint32_t fail_label, int live, term arg1);

term bif_erlang_not_1(Context *ctx, uint32_t fail_label, term arg1);
term bif_erlang_and_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_or_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_xor_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_not_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_exactly_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_exactly_not_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_greater_than_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_less_than_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_less_than_or_equal_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_greater_than_or_equal_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_get_1(Context *ctx, uint32_t fail_label, term arg1);

term bif_erlang_min_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);
term bif_erlang_max_2(Context *ctx, uint32_t fail_label, term arg1, term arg2);

term bif_erlang_size_1(Context *ctx, uint32_t fail_label, int live, term arg1);

term bif_erlang_list_to_atom_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_list_to_existing_atom_1(Context *ctx, uint32_t fail_label, int live, term arg1);
term bif_erlang_binary_to_atom_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);
term bif_erlang_binary_to_existing_atom_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2);

// helpers:
term binary_to_atom(Context *ctx, term a_binary, term encoding, bool create_new, term *error_reason);

#ifdef __cplusplus
}
#endif

#endif
