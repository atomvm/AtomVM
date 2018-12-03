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

#include "atom.h"
#include "trace.h"
#include "utils.h"

//Ignore warning caused by gperf generated code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "bifs_hash.h"
#pragma GCC diagnostic pop

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = term_from_atom_string(ctx->global, error_atom); \
    ctx->x[1] = term_from_atom_string(ctx->global, (error_type_atom)); \
    return term_invalid_term();

#define VERIFY_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        RAISE_ERROR(badarg_atom); \
    }

static const char *const true_atom = "\x04" "true";
static const char *const false_atom = "\x05" "false";
static const char *const error_atom = "\x05" "error";
static const char *const badarith_atom = "\x08" "badarith";
static const char *const badarg_atom = "\x6" "badarg";

static inline term term_from_atom_string(GlobalContext *glb, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(glb, string);
    return term_from_atom_index(global_atom_index);
}

BifImpl bif_registry_get_handler(AtomString module, AtomString function, int arity)
{
    char bifname[MAX_BIF_NAME_LEN];

    int module_name_len = atom_string_len(module);
    memcpy(bifname, atom_string_data(module), module_name_len);

    bifname[module_name_len] = ':';

    int function_name_len = atom_string_len(function);
    if (UNLIKELY((arity > 9) || (module_name_len + function_name_len + 4 > MAX_BIF_NAME_LEN))) {
        abort();
    }
    memcpy(bifname + module_name_len + 1, atom_string_data(function), function_name_len);

    //TODO: handle BIFs with more than 9 parameters
    bifname[module_name_len + function_name_len + 1] = '/';
    bifname[module_name_len + function_name_len + 2] = '0' + arity;
    bifname[module_name_len + function_name_len + 3] = 0;

    const BifNameAndPtr *nameAndPtr = in_word_set(bifname, strlen(bifname));
    if (!nameAndPtr) {
        return NULL;
    }

    return nameAndPtr->function;
}


term bif_erlang_self_0(Context *ctx)
{
    return term_from_local_process_id(ctx->process_id);
}

term bif_erlang_byte_size_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VERIFY_VALUE(arg1, term_is_binary);

    return term_from_int32(term_binary_size(arg1));
}

term bif_erlang_is_atom_1(Context *ctx, term arg1)
{
    if (term_is_atom(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_binary_1(Context *ctx, term arg1)
{
    if (term_is_binary(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_integer_1(Context *ctx, term arg1)
{
    if (term_is_integer(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_list_1(Context *ctx, term arg1)
{
    if (term_is_list(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_number_1(Context *ctx, term arg1)
{
    //TODO: change to term_is_number
    if (term_is_integer(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_pid_1(Context *ctx, term arg1)
{
    if (term_is_pid(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_reference_1(Context *ctx, term arg1)
{
    if (term_is_reference(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_is_tuple_1(Context *ctx, term arg1)
{
    if (term_is_tuple(arg1)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_length_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VERIFY_VALUE(arg1, term_is_list);

    return term_from_int32(term_list_length(arg1));
}

term bif_erlang_hd_1(Context *ctx, term arg1)
{
    VERIFY_VALUE(arg1, term_is_nonempty_list);

    return term_get_list_head(arg1);
}

term bif_erlang_tl_1(Context *ctx, term arg1)
{
    VERIFY_VALUE(arg1, term_is_nonempty_list);

    return term_get_list_tail(arg1);
}

term bif_erlang_element_2(Context *ctx, term arg1, term arg2)
{
    VERIFY_VALUE(arg1, term_is_integer);
    VERIFY_VALUE(arg2, term_is_tuple);

    // indexes are 1 based
    int elem_index = term_to_int32(arg1) - 1;
    if (LIKELY((elem_index >= 0) && (elem_index < term_get_tuple_arity(arg2)))) {
        return term_get_tuple_element(arg2, elem_index);

    } else {
        RAISE_ERROR(badarg_atom);
    }
}

term bif_erlang_tuple_size_1(Context *ctx, term arg1)
{
    VERIFY_VALUE(arg1, term_is_tuple);

    return term_from_int32(term_get_tuple_arity(arg1));
}

term bif_erlang_add_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) + term_to_int32(arg2));

    } else {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_sub_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) - term_to_int32(arg2));

    } else {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_mul_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) * term_to_int32(arg2));

    } else {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_div_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        int32_t operand_b = term_to_int32(arg2);
        if (operand_b != 0) {
            return term_from_int32(term_to_int32(arg1) / operand_b);

        } else {
            RAISE_ERROR(badarith_atom);
        }

    } else {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_neg_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        return term_from_int32(-term_to_int32(arg1));

    } else {
        TRACE("error: arg1: %lx\n", arg1);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_abs_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        int32_t int_val = term_to_int32(arg1);
        if (int_val < 0) {
            return term_from_int32(-int_val);
        } else {
            return arg1;
        }

    } else {
        TRACE("error: arg1: %lx\n", arg1);
        RAISE_ERROR(badarg_atom);
    }
}

term bif_erlang_rem_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        int32_t operand_b = term_to_int32(arg2);
        if (LIKELY(operand_b != 0)) {
            return term_from_int32(term_to_int32(arg1) % operand_b);

        } else {
            RAISE_ERROR(badarith_atom);
        }

    } else {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(badarith_atom);
    }
}

term bif_erlang_bor_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) | term_to_int32(arg2));

    } else {
        printf("bor: operand is not an integer: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_band_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) & term_to_int32(arg2));

    } else {
        printf("band: operand is not an integer: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_bxor_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) ^ term_to_int32(arg2));

    } else {
        printf("bxor: operand is not an integer: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_bsl_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) << term_to_int32(arg2));

    } else {
        printf("bsl: operand is not an integer: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_bsr_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) >> term_to_int32(arg2));

    } else {
        printf("bsr: operand is not an integer: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_bnot_1(Context *ctx, int live, term arg1)
{
    UNUSED(ctx);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        return term_from_int32(~term_to_int32(arg1));

    } else {
        printf("bnot: operand is not an integer: arg1=%lx\n", arg1);
        abort();
    }
}

term bif_erlang_not_1(Context *ctx, term arg1)
{
    term true_term = term_from_atom_string(ctx->global, true_atom);
    term false_term = term_from_atom_string(ctx->global, false_atom);

    if (arg1 == true_term) {
        return false_term;

    } else if (arg1 == false_term) {
        return true_term;

    } else {
        fprintf(stderr, "invalid argument\n");
        abort();
    }
}

term bif_erlang_and_2(Context *ctx, term arg1, term arg2)
{
    term true_term = term_from_atom_string(ctx->global, true_atom);
    term false_term = term_from_atom_string(ctx->global, false_atom);

    if ((arg1 == false_term) && (arg2 == false_term)) {
        return false_term;

    } else if ((arg1 == false_term) && (arg2 == true_term)) {
        return false_term;

    } else if ((arg1 == true_term) && (arg2 == false_term)) {
        return false_term;

    } else if ((arg1 == true_term) && (arg2 == true_term)) {
        return true_term;

    } else {
        fprintf(stderr, "invalid argument\n");
        abort();
    }
}

term bif_erlang_or_2(Context *ctx, term arg1, term arg2)
{
    term true_term = term_from_atom_string(ctx->global, true_atom);
    term false_term = term_from_atom_string(ctx->global, false_atom);

    if ((arg1 == false_term) && (arg2 == false_term)) {
        return false_term;

    } else if ((arg1 == false_term) && (arg2 == true_term)) {
        return true_term;

    } else if ((arg1 == true_term) && (arg2 == false_term)) {
        return true_term;

    } else if ((arg1 == true_term) && (arg2 == true_term)) {
        return true_term;

    } else {
        fprintf(stderr, "invalid argument\n");
        abort();
    }
}

term bif_erlang_xor_2(Context *ctx, term arg1, term arg2)
{
    term true_term = term_from_atom_string(ctx->global, true_atom);
    term false_term = term_from_atom_string(ctx->global, false_atom);

    if ((arg1 == false_term) && (arg2 == false_term)) {
        return false_term;

    } else if ((arg1 == false_term) && (arg2 == true_term)) {
        return true_term;

    } else if ((arg1 == true_term) && (arg2 == false_term)) {
        return true_term;

    } else if ((arg1 == true_term) && (arg2 == true_term)) {
        return false_term;

    } else {
        fprintf(stderr, "invalid argument\n");
        abort();
    }
}

term bif_erlang_equal_to_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 == 5
    if (arg1 == arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 != 5 is false
    if (arg1 != arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_exactly_equal_to_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (arg1 == arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_exactly_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (arg1 != arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}


term bif_erlang_greater_than_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) > term_to_int32(arg2)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_less_than_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) < term_to_int32(arg2)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_less_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) <= term_to_int32(arg2)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_greater_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) >= term_to_int32(arg2)) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}
