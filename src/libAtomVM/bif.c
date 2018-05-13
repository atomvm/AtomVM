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
#include "utils.h"

//Ignore warning caused by gperf generated code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "bifs_hash.h"
#pragma GCC diagnostic pop

static const char *const true_atom = "\x04" "true";
static const char *const false_atom = "\x05" "false";

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
    memcpy(bifname + module_name_len + 1, atom_string_data(function), function_name_len);

    bifname[module_name_len + function_name_len + 1] = '\\';
    bifname[module_name_len + function_name_len + 2] = '0' + arity;
    bifname[module_name_len + function_name_len + 3] = 0;

    BifNameAndPtr *nameAndPtr = in_word_set(bifname, strlen(bifname));
    if (!nameAndPtr) {
        return NULL;
    }

    return nameAndPtr->function;
}


term bif_erlang_self_0(Context *ctx)
{
    return term_from_local_process_id(ctx->process_id);
}

term bif_erlang_byte_size_1(Context *ctx, uint32_t failure_label, int live, term arg1)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    return term_from_int32(term_binary_size(arg1));
}

term bif_erlang_length_1(Context *ctx, uint32_t failure_label, int live, term arg1)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    return term_from_int32(term_list_length(arg1));
}

//TODO: fail if not a list
term bif_erlang_hd_1(Context *ctx, uint32_t failure_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(failure_label);

    return term_get_list_head(arg1);
}

term bif_erlang_add_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) + term_to_int32(arg2));

    } else {
        printf("add: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_sub_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) - term_to_int32(arg2));

    } else {
        printf("sub: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_mul_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //NEED CHECK OVERFLOW AND BIG INTEGER
        return term_from_int32(term_to_int32(arg1) * term_to_int32(arg2));

    } else {
        printf("mul: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_div_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        int32_t operand_b = term_to_int32(arg2);
        if (operand_b != 0) {
            return term_from_int32(term_to_int32(arg1) / operand_b);

        } else {
            fprintf(stderr, "error: division by 0");
            abort();
        }

    } else {
        printf("div: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }
}

term bif_erlang_rem_2(Context *ctx, uint32_t failure_label, int live, term arg1, term arg2)
{
    UNUSED(ctx);
    UNUSED(failure_label);
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        int32_t operand_b = term_to_int32(arg2);
        if (LIKELY(operand_b != 0)) {
            return term_from_int32(term_to_int32(arg1) % operand_b);

        } else {
            fprintf(stderr, "error: division by 0");
            abort();
        }

    } else {
        printf("rem: operands are not integers: arg1=%lx, arg2=%lx\n", arg1, arg2);
        abort();
    }

}

term bif_erlang_not_1(Context *ctx, uint32_t failure_label, term arg1)
{
    UNUSED(failure_label);

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

term bif_erlang_and_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

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

term bif_erlang_or_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

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

term bif_erlang_xor_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

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

term bif_erlang_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 == 5
    if (arg1 == arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_not_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 != 5 is false
    if (arg1 != arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_exactly_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

    //TODO: fix this implementation, it needs to cover more types
    if (arg1 == arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}

term bif_erlang_exactly_not_equal_to_2(Context *ctx, uint32_t failure_label, term arg1, term arg2)
{
    UNUSED(failure_label);

    //TODO: fix this implementation, it needs to cover more types
    if (arg1 != arg2) {
        return term_from_atom_string(ctx->global, true_atom);
    } else {
        return term_from_atom_string(ctx->global, false_atom);
    }
}
