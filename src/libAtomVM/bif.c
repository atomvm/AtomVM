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

#include "bif.h"

#include <stdlib.h>
#include <math.h>

#include "atom.h"
#include "bitstring.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "interop.h"
#include "intn.h"
#include "overflow_helpers.h"
#include "smp.h"
#include "term.h"
#include "term_typedef.h"
#include "trace.h"
#include "unicode.h"
#include "utils.h"

//Ignore warning caused by gperf generated code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "bifs_hash.h"
#pragma GCC diagnostic pop

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM;          \
    ctx->x[1] = (error_type_atom);   \
    return term_invalid_term();

#define RAISE_ERROR_BIF(fail_label, error_type_atom) \
    if (fail_label == 0) {                           \
        ctx->x[0] = ERROR_ATOM;                      \
        ctx->x[1] = (error_type_atom);               \
    }                                                \
    return term_invalid_term();

#define VALIDATE_VALUE_BIF(fail_label, value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) {                 \
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);              \
    }

#define MAX(a, b) ((a) > (b) ? (a) : (b))

/*
 * they are the max/min values, that can be converted to int64, such as:
 * avm_float_t fvalue;
 * int64_t ivalue = fvalue;
 * // ivalue is guarnteed to be valid (>= INT64_MIN and <= INT64_MAX)
 *
 * They have been found with few test C programs (and while playing with bits)
 * do not use `(avm_float_t) INT64_MIN` or `(avm_float_t) INT64_MAX`.
 */
#ifdef AVM_USE_SINGLE_PRECISION
    #define INT64_MIN_AS_AVM_FLOAT -9223372586610590720.0 // 0xDF000000 = -2^63
    #define INT64_MAX_AS_AVM_FLOAT 9223371761976868863.0 // 0x5F000000 = 2^63
#else
    #define INT64_MIN_AS_AVM_FLOAT -9223372036854776832.0 // 0xC3E0000000000000 = -2^63
    #define INT64_MAX_AS_AVM_FLOAT 9223372036854775295.0 // 0x43DFFFFFFFFFFFFF = 2^62 * 1.1...1b
#endif

static term make_bigint(Context *ctx, uint32_t fail_label, uint32_t live,
    const intn_digit_t bigres[], size_t bigres_len, intn_integer_sign_t sign);

static void conv_term_to_bigint(term arg1, intn_digit_t *tmp_buf1, const intn_digit_t **b1,
    size_t *b1_len, intn_integer_sign_t *b1_sign);

const struct ExportedFunction *bif_registry_get_handler(const char *mfa)
{
    const BifNameAndPtr *nameAndPtr = in_word_set(mfa, strlen(mfa));
    if (!nameAndPtr) {
        return NULL;
    }

    return &nameAndPtr->bif.base;
}

term bif_erlang_self_0(Context *ctx)
{
    return term_from_local_process_id(ctx->process_id);
}

term bif_erlang_node_0(Context *ctx)
{
    return ctx->global->node_name;
}

term bif_erlang_byte_size_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    size_t len;

    if (term_is_match_state(arg1)) {
        avm_int_t offset = term_get_match_state_offset(arg1);
        term src_bin = term_get_match_state_binary(arg1);
        len = term_binary_size(src_bin) - offset / 8;
    } else {
        VALIDATE_VALUE_BIF(fail_label, arg1, term_is_binary);
        len = term_binary_size(arg1);
    }

    return term_from_int32(len);
}

term bif_erlang_bit_size_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    size_t len;

    if (term_is_match_state(arg1)) {
        avm_int_t offset = term_get_match_state_offset(arg1);
        term src_bin = term_get_match_state_binary(arg1);
        len = term_binary_size(src_bin) * 8 - offset;
    } else {
        VALIDATE_VALUE_BIF(fail_label, arg1, term_is_binary);
        len = term_binary_size(arg1) * 8;
    }

    return term_from_int32(len);
}

term bif_erlang_binary_part_3(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, term arg3)
{
    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_binary);
    VALIDATE_VALUE_BIF(fail_label, arg2, term_is_integer);
    VALIDATE_VALUE_BIF(fail_label, arg3, term_is_integer);

    avm_int_t pos = term_to_int(arg2);
    avm_int_t len = term_to_int(arg3);
    BinaryPosLen slice;
    if (UNLIKELY(!term_normalize_binary_pos_len(arg1, pos, len, &slice))) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }

    TERM_DEBUG_ASSERT((sizeof(ctx->x) / sizeof(ctx->x[0])) >= MAX_REG + 1);
    ctx->x[live] = arg1;
    size_t heap_size = term_sub_binary_heap_size(arg1, len);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, heap_size, live + 1, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    arg1 = ctx->x[live];

    return term_maybe_create_sub_binary(arg1, slice.pos, slice.len, &ctx->heap, ctx->global);
}

term bif_erlang_is_atom_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_atom(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_binary_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_binary(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_boolean_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return (arg1 == TRUE_ATOM || arg1 == FALSE_ATOM) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_float_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_float(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_function_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_function(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_function_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    VALIDATE_VALUE_BIF(fail_label, arg2, term_is_any_non_neg_integer);

    if (!term_is_integer(arg2)) {
        // function takes any positive integer, including big integers
        // but internally we use only small integers
        return FALSE_ATOM;
    }
    avm_int_t arity = term_to_int(arg2);

    if (!term_is_function(arg1)) {
        return FALSE_ATOM;
    }

    // following part has been taken from opcodesswitch.h
    // TODO: factor this out
    const term *boxed_value = term_to_const_term_ptr(arg1);

    Module *fun_module = (Module *) boxed_value[1];
    term index_or_module = boxed_value[2];

    uint32_t fun_arity;

    if (term_is_atom(index_or_module)) {
        fun_arity = term_to_int(boxed_value[3]);

    } else {
        uint32_t fun_index = term_to_int32(index_or_module);

        uint32_t fun_label;
        uint32_t fun_arity_and_freeze;
        uint32_t fun_n_freeze;

        module_get_fun(fun_module, fun_index, &fun_label, &fun_arity_and_freeze, &fun_n_freeze);
        fun_arity = fun_arity_and_freeze - fun_n_freeze;
    }

    return (arity == ((avm_int_t) fun_arity)) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_integer_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_any_integer(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_list_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_list(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_number_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_number(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_pid_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_pid(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

// Generated by OTP-21 compiler
term bif_erlang_is_port_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_port(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_reference_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_reference(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_tuple_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_tuple(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_record_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    UNUSED(ctx);
    VALIDATE_VALUE_BIF(fail_label, arg2, term_is_atom);
    if (!term_is_tuple(arg1) || term_get_tuple_arity(arg1) == 0) {
        return FALSE_ATOM;
    }

    term tag = term_get_tuple_element(arg1, 0);
    return tag == arg2 ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_map_1(Context *ctx, uint32_t fail_label, term arg1)
{
    UNUSED(ctx);
    UNUSED(fail_label);

    return term_is_map(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_map_key_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    if (UNLIKELY(!term_is_map(arg2))) {
        if (fail_label) {
            return term_invalid_term();
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADMAP_ATOM);
        term_put_tuple_element(err, 1, arg2);

        RAISE_ERROR(err);
    }

    switch (term_find_map_pos(arg2, arg1, ctx->global)) {
        case TERM_MAP_NOT_FOUND:
            return FALSE_ATOM;
        case TERM_MAP_MEMORY_ALLOC_FAIL:
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        default:
            return TRUE_ATOM;
    }
}

term bif_erlang_length_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_list);

    int proper;
    avm_int_t len = term_list_length(arg1, &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }

    return term_from_int(len);
}

term bif_erlang_hd_1(Context *ctx, uint32_t fail_label, term arg1)
{
    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_nonempty_list);

    return term_get_list_head(arg1);
}

term bif_erlang_tl_1(Context *ctx, uint32_t fail_label, term arg1)
{
    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_nonempty_list);

    return term_get_list_tail(arg1);
}

term bif_erlang_element_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_integer);
    VALIDATE_VALUE_BIF(fail_label, arg2, term_is_tuple);

    // indexes are 1 based
    int elem_index = term_to_int32(arg1) - 1;
    if (LIKELY((elem_index >= 0) && (elem_index < term_get_tuple_arity(arg2)))) {
        return term_get_tuple_element(arg2, elem_index);
    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_tuple_size_1(Context *ctx, uint32_t fail_label, term arg1)
{
    VALIDATE_VALUE_BIF(fail_label, arg1, term_is_tuple);

    return term_from_int32(term_get_tuple_arity(arg1));
}

term bif_erlang_map_size_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (!UNLIKELY(term_is_map(arg1))) {
        if (fail_label) {
            return term_invalid_term();
        }
        // We don't need to preserve registers as we're raising
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADMAP_ATOM);
        term_put_tuple_element(err, 1, arg1);

        RAISE_ERROR(err);
    }

    return term_from_int32(term_get_map_size(arg1));
}

term bif_erlang_map_get_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    if (!UNLIKELY(term_is_map(arg2))) {
        if (fail_label) {
            return term_invalid_term();
        }
        // We don't need to preserve registers as we're raising
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADMAP_ATOM);
        term_put_tuple_element(err, 1, arg2);

        RAISE_ERROR(err);
    }

    int pos = term_find_map_pos(arg2, arg1, ctx->global);
    if (pos == TERM_MAP_NOT_FOUND) {
        if (fail_label) {
            return term_invalid_term();
        }
        // We don't need to preserve registers as we're raising
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADKEY_ATOM);
        term_put_tuple_element(err, 1, arg1);

        RAISE_ERROR(err);
    } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    return term_get_map_value(arg2, pos);
}

term bif_erlang_unique_integer_0(Context *ctx)
{
    int64_t value = globalcontext_get_ref_ticks(ctx->global);
    return term_make_maybe_boxed_int64(value, &ctx->heap);
}

term bif_erlang_unique_integer_1(Context *ctx, uint32_t fail_label, term arg1)
{
    int proper = 0;
    if (UNLIKELY(!term_is_list(arg1))) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
    size_t _len = term_list_length(arg1, &proper);
    UNUSED(_len);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }

    // List is checked only for correctness if in the future
    // we would like to handle monotonic and positive integers separately.
    //
    // Right now the implementation is backed by increasing counter
    // that always covers both options
    int64_t value = globalcontext_get_ref_ticks(ctx->global);
    return term_make_maybe_boxed_int64(value, &ctx->heap);
}

static inline term make_boxed_int(Context *ctx, uint32_t fail_label, uint32_t live, avm_int_t value)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, BOXED_INT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int(value, &ctx->heap);
}

#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
static inline term make_boxed_int64(Context *ctx, uint32_t fail_label, uint32_t live, avm_int64_t value)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, BOXED_INT64_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int64(value, &ctx->heap);
}
#endif

static inline term make_maybe_boxed_int(Context *ctx, uint32_t fail_label, uint32_t live, avm_int_t value)
{
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return make_boxed_int(ctx, fail_label, live, value);

    } else {
        return term_from_int(value);
    }
}

#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
static inline term make_maybe_boxed_int64(Context *ctx, uint32_t fail_label, uint32_t live, avm_int64_t value)
{
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        return make_boxed_int64(ctx, fail_label, live, value);

    } else if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return make_boxed_int(ctx, fail_label, live, value);

    } else {
        return term_from_int(value);
    }
}
#endif

static term add_int64_to_bigint(
    Context *ctx, uint32_t fail_label, uint32_t live, int64_t val1, int64_t val2)
{
    size_t out_buf_len = INTN_ADD_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN);
    intn_digit_t add_out[out_buf_len];
    intn_integer_sign_t out_sign;
    size_t out_len = intn_add_int64(val1, val2, add_out, &out_sign);

    return make_bigint(ctx, fail_label, live, add_out, out_len, out_sign);
}

static term add_maybe_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    intn_digit_t tmp_buf1[INTN_INT64_LEN];
    intn_digit_t tmp_buf2[INTN_INT64_LEN];

    const intn_digit_t *bn1;
    size_t bn1_len;
    intn_integer_sign_t bn1_sign;
    conv_term_to_bigint(arg1, tmp_buf1, &bn1, &bn1_len, &bn1_sign);
    const intn_digit_t *bn2;
    size_t bn2_len;
    intn_integer_sign_t bn2_sign;
    conv_term_to_bigint(arg2, tmp_buf2, &bn2, &bn2_len, &bn2_sign);

    size_t bigres_len = INTN_ADD_OUT_LEN(bn1_len, bn2_len);
    if (bigres_len > INTN_MAX_RES_LEN) {
        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
    }

    intn_digit_t bigres[INTN_MAX_RES_LEN];
    intn_integer_sign_t res_sign;
    bigres_len = intn_add(bn1, bn1_len, bn1_sign, bn2, bn2_len, bn2_sign, bigres, &res_sign);

    return make_bigint(ctx, fail_label, live, bigres, bigres_len, res_sign);
}

static term add_overflow_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, fail_label, live, val1 + val2);
}

static term add_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    if (UNLIKELY(!term_is_number(arg1) || !term_is_number(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_any_integer(arg1) && term_is_any_integer(arg2)) {

        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        size_t arg2_size = term_is_integer(arg2) ? 0 : term_boxed_size(arg2);
        switch (MAX(arg1_size, arg2_size)) {
            case 0:
                UNREACHABLE();
            case 1: {
                avm_int_t val1 = term_maybe_unbox_int(arg1);
                avm_int_t val2 = term_maybe_unbox_int(arg2);
                avm_int_t res;

                if (BUILTIN_ADD_OVERFLOW_INT(val1, val2, &res)) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        avm_int64_t res64 = (avm_int64_t) val1 + (avm_int64_t) val2;
                        return make_boxed_int64(ctx, fail_label, live, res64);

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        return add_int64_to_bigint(ctx, fail_label, live, val1, val2);
                    #else
                        #error "Unsupported configuration."
                    #endif
                }

                return make_maybe_boxed_int(ctx, fail_label, live, res);
            }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val1 = term_maybe_unbox_int64(arg1);
                avm_int64_t val2 = term_maybe_unbox_int64(arg2);
                avm_int64_t res;

                if (BUILTIN_ADD_OVERFLOW_INT64(val1, val2, &res)) {
                    return add_int64_to_bigint(ctx, fail_label, live, val1, val2);
                }

                return make_maybe_boxed_int64(ctx, fail_label, live, res);
            }
        #endif

            default:
                return add_maybe_bigint(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 + farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK)
                != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }
}

term bif_erlang_add_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //TODO: use long integer instead, and term_to_longint
        avm_int_t res;
        if (!BUILTIN_ADD_OVERFLOW((avm_int_t) (arg1 & ~TERM_INTEGER_TAG), (avm_int_t) (arg2 & ~TERM_INTEGER_TAG), &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return add_overflow_helper(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        return add_boxed_helper(ctx, fail_label, live, arg1, arg2);
    }
}

term bif_erlang_plus_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_number(arg1))) {
        return arg1;
    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

static term sub_int64_to_bigint(
    Context *ctx, uint32_t fail_label, uint32_t live, int64_t val1, int64_t val2)
{
    size_t out_buf_len = INTN_SUB_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN);
    intn_digit_t sub_out[out_buf_len];
    intn_integer_sign_t out_sign;
    size_t out_len = intn_sub_int64(val1, val2, sub_out, &out_sign);

    return make_bigint(ctx, fail_label, live, sub_out, out_len, out_sign);
}

static term sub_maybe_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    intn_digit_t tmp_buf1[INTN_INT64_LEN];
    intn_digit_t tmp_buf2[INTN_INT64_LEN];

    const intn_digit_t *bn1;
    size_t bn1_len;
    intn_integer_sign_t bn1_sign;
    conv_term_to_bigint(arg1, tmp_buf1, &bn1, &bn1_len, &bn1_sign);
    const intn_digit_t *bn2;
    size_t bn2_len;
    intn_integer_sign_t bn2_sign;
    conv_term_to_bigint(arg2, tmp_buf2, &bn2, &bn2_len, &bn2_sign);

    size_t bigres_len = INTN_SUB_OUT_LEN(bn1_len, bn2_len);
    if (bigres_len > INTN_MAX_RES_LEN) {
        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
    }

    intn_digit_t bigres[INTN_MAX_RES_LEN];
    intn_integer_sign_t res_sign;
    bigres_len = intn_sub(bn1, bn1_len, bn1_sign, bn2, bn2_len, bn2_sign, bigres, &res_sign);

    return make_bigint(ctx, fail_label, live, bigres, bigres_len, res_sign);
}

static term sub_overflow_helper(
    Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, fail_label, live, val1 - val2);
}

static term sub_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    if (UNLIKELY(!term_is_number(arg1) || !term_is_number(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_any_integer(arg1) && term_is_any_integer(arg2)) {

        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        size_t arg2_size = term_is_integer(arg2) ? 0 : term_boxed_size(arg2);
        switch (MAX(arg1_size, arg2_size)) {
            case 0:
                UNREACHABLE();
            case 1: {
                avm_int_t val1 = term_maybe_unbox_int(arg1);
                avm_int_t val2 = term_maybe_unbox_int(arg2);
                avm_int_t res;

                if (BUILTIN_SUB_OVERFLOW_INT(val1, val2, &res)) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        avm_int64_t res64 = (avm_int64_t) val1 - (avm_int64_t) val2;
                        return make_boxed_int64(ctx, fail_label, live, res64);

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        return sub_int64_to_bigint(ctx, fail_label, live, val1, val2);
                    #else
                        #error "Unsupported configuration."
                    #endif
                }

                return make_maybe_boxed_int(ctx, fail_label, live, res);
            }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val1 = term_maybe_unbox_int64(arg1);
                avm_int64_t val2 = term_maybe_unbox_int64(arg2);
                avm_int64_t res;

                if (BUILTIN_SUB_OVERFLOW_INT64(val1, val2, &res)) {
                    return sub_int64_to_bigint(ctx, fail_label, live, val1, val2);
                }

                return make_maybe_boxed_int64(ctx, fail_label, live, res);
            }
        #endif

            default:
                return sub_maybe_bigint(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 - farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK)
                != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }
}

term bif_erlang_sub_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //TODO: use long integer instead, and term_to_longint
        avm_int_t res;
        if (!BUILTIN_SUB_OVERFLOW((avm_int_t) (arg1 & ~TERM_INTEGER_TAG), (avm_int_t) (arg2 & ~TERM_INTEGER_TAG), &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return sub_overflow_helper(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        return sub_boxed_helper(ctx, fail_label, live, arg1, arg2);
    }
}

// this function assumes that bigres_len is always <= bigres buffer capacity
static term make_bigint(Context *ctx, uint32_t fail_label, uint32_t live,
    const intn_digit_t bigres[], size_t bigres_len, intn_integer_sign_t sign)
{
    size_t count = intn_count_digits(bigres, bigres_len);

    if (UNLIKELY(count > INTN_MAX_IN_LEN)) {
        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
    }

    if (!intn_fits_int64(bigres, count, sign)) {
        size_t intn_data_size;
        size_t rounded_res_len;
        term_intn_to_term_size(count, &intn_data_size, &rounded_res_len);

        if (UNLIKELY(memory_ensure_free_with_roots(
                         ctx, BOXED_INTN_SIZE(intn_data_size), live, ctx->x, MEMORY_CAN_SHRINK)
                != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }

        term bigres_term = term_create_uninitialized_intn(
            intn_data_size, (term_integer_sign_t) sign, &ctx->heap);
        term_initialize_bigint(bigres_term, bigres, count, rounded_res_len);

        return bigres_term;
    } else {
        int64_t res64 = intn_to_int64(bigres, count, sign);
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
        return make_maybe_boxed_int64(ctx, fail_label, live, res64);
#else
        return make_maybe_boxed_int(ctx, fail_label, live, res64);
#endif
    }
}

static void conv_term_to_bigint(term arg1, intn_digit_t *tmp_buf1, const intn_digit_t **b1,
    size_t *b1_len, intn_integer_sign_t *b1_sign)
{
    if (term_is_bigint(arg1)) {
        term_to_bigint(arg1, b1, b1_len, b1_sign);

    } else {
        avm_int64_t i64 = term_maybe_unbox_int64(arg1);
        intn_from_int64(i64, tmp_buf1, b1_sign);
        *b1 = tmp_buf1;
        *b1_len = INTN_INT64_LEN;
    }
}

static term mul_int64_to_bigint(
    Context *ctx, uint32_t fail_label, uint32_t live, int64_t val1, int64_t val2)
{
    size_t mul_out_len = INTN_MUL_OUT_LEN(INTN_INT64_LEN, INTN_INT64_LEN);
    intn_digit_t mul_out[mul_out_len];
    intn_integer_sign_t out_sign;
    intn_mul_int64(val1, val2, mul_out, &out_sign);

    return make_bigint(ctx, fail_label, live, mul_out, mul_out_len, out_sign);
}

static term mul_maybe_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    intn_digit_t tmp_buf1[INTN_INT64_LEN];
    intn_digit_t tmp_buf2[INTN_INT64_LEN];

    const intn_digit_t *bn1;
    size_t bn1_len;
    intn_integer_sign_t bn1_sign;
    conv_term_to_bigint(arg1, tmp_buf1, &bn1, &bn1_len, &bn1_sign);
    const intn_digit_t *bn2;
    size_t bn2_len;
    intn_integer_sign_t bn2_sign;
    conv_term_to_bigint(arg2, tmp_buf2, &bn2, &bn2_len, &bn2_sign);

    size_t bigres_len = INTN_MUL_OUT_LEN(bn1_len, bn2_len);
    if (bigres_len > INTN_MAX_RES_LEN) {
        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
    }

    intn_digit_t bigres[INTN_MAX_RES_LEN];
    intn_integer_sign_t res_sign;
    intn_mul(bn1, bn1_len, bn1_sign, bn2, bn2_len, bn2_sign, bigres, &res_sign);

    return make_bigint(ctx, fail_label, live, bigres, bigres_len, res_sign);
}

static term mul_overflow_helper(
    Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    avm_int_t res;
#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
    avm_int64_t res64;
#endif

    if (!BUILTIN_MUL_OVERFLOW_INT(val1, val2, &res)) {
        return make_boxed_int(ctx, fail_label, live, res);

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
    } else if (!BUILTIN_MUL_OVERFLOW_INT64((avm_int64_t) val1, (avm_int64_t) val2, &res64)) {
        return make_boxed_int64(ctx, fail_label, live, res64);
#endif

    } else {
        return mul_int64_to_bigint(ctx, fail_label, live, val1, val2);
    }
}

static term mul_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    if (UNLIKELY(!term_is_number(arg1) || !term_is_number(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_any_integer(arg1) && term_is_any_integer(arg2)) {

        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        size_t arg2_size = term_is_integer(arg2) ? 0 : term_boxed_size(arg2);
        switch (MAX(arg1_size, arg2_size)) {
            case 0:
                UNREACHABLE();
            case 1: {
                avm_int_t val1 = term_maybe_unbox_int(arg1);
                avm_int_t val2 = term_maybe_unbox_int(arg2);
                avm_int_t res;

                if (BUILTIN_MUL_OVERFLOW_INT(val1, val2, &res)) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        avm_int64_t res64 = (avm_int64_t) val1 * (avm_int64_t) val2;
                        return make_boxed_int64(ctx, fail_label, live, res64);

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        return mul_int64_to_bigint(ctx, fail_label, live, val1, val2);
                    #else
                        #error "Unsupported configuration."
                    #endif
                }

                return make_maybe_boxed_int(ctx, fail_label, live, res);
            }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val1 = term_maybe_unbox_int64(arg1);
                avm_int64_t val2 = term_maybe_unbox_int64(arg2);
                avm_int64_t res;

                if (BUILTIN_MUL_OVERFLOW_INT64(val1, val2, &res)) {
                    return mul_int64_to_bigint(ctx, fail_label, live, val1, val2);
                }

                return make_maybe_boxed_int64(ctx, fail_label, live, res);
            }
        #endif

            default:
                return mul_maybe_bigint(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 * farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK)
                != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }
}

term bif_erlang_mul_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t res;
        avm_int_t a = ((avm_int_t) (arg1 & ~TERM_INTEGER_TAG)) >> 2;
        avm_int_t b = ((avm_int_t) (arg2 & ~TERM_INTEGER_TAG)) >> 2;
        if (!BUILTIN_MUL_OVERFLOW(a, b, &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return mul_overflow_helper(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        return mul_boxed_helper(ctx, fail_label, live, arg1, arg2);
    }
}

term bif_erlang_fdiv_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);
    if (UNLIKELY(!term_is_number(arg1))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
    if (UNLIKELY(!term_is_number(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
    avm_float_t farg1 = term_conv_to_float(arg1);
    avm_float_t farg2 = term_conv_to_float(arg2);

    if (UNLIKELY(farg2 == 0)) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
    avm_float_t fresult = farg1 / farg2;

    if (UNLIKELY(!isfinite(fresult))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    return term_from_float(fresult, &ctx->heap);
}

static term div_maybe_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    // 0 is always normalized to `term_from_int(0)`, so we can do this
    if (UNLIKELY(arg2 == term_from_int(0))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    intn_digit_t tmp_buf1[INTN_INT64_LEN];
    intn_digit_t tmp_buf2[INTN_INT64_LEN];

    const intn_digit_t *bn1;
    size_t bn1_len;
    intn_integer_sign_t bn1_sign;
    conv_term_to_bigint(arg1, tmp_buf1, &bn1, &bn1_len, &bn1_sign);
    const intn_digit_t *bn2;
    size_t bn2_len;
    intn_integer_sign_t bn2_sign;
    conv_term_to_bigint(arg2, tmp_buf2, &bn2, &bn2_len, &bn2_sign);

    int cmp_result = intn_cmp(bn1, bn1_len, bn2, bn2_len);
    if (cmp_result < 0) {
        // a / b when a < b -> always 0
        return term_from_int(0);
    } else if (cmp_result == 0) {
        // a / b when a == b -> always +-1
        return (bn1_sign == bn2_sign) ? term_from_int(1) : term_from_int(-1);
    }

    intn_digit_t bigres[INTN_MAX_RES_LEN];
    intn_integer_sign_t res_sign;
    size_t bigres_len = intn_div(bn1, bn1_len, bn1_sign, bn2, bn2_len, bn2_sign, bigres, &res_sign, NULL, NULL);

    return make_bigint(ctx, fail_label, live, bigres, bigres_len, res_sign);
}

static term int64_max_plus_one(Context *ctx, uint32_t fail_label, uint32_t live)
{
    intn_digit_t int_buf[INTN_UINT64_LEN];
    intn_from_uint64(((uint64_t) INT64_MAX) + 1, int_buf);
    return make_bigint(ctx, fail_label, live, int_buf, INTN_UINT64_LEN, IntNPositiveInteger);
}

static term div_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    if (LIKELY(term_is_any_integer(arg1) && term_is_any_integer(arg2))) {

        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        size_t arg2_size = term_is_integer(arg2) ? 0 : term_boxed_size(arg2);
        switch (MAX(arg1_size, arg2_size)) {
            case 0:
                UNREACHABLE();
            case 1: {
                avm_int_t val1 = term_maybe_unbox_int(arg1);
                avm_int_t val2 = term_maybe_unbox_int(arg2);

                if (UNLIKELY(val2 == 0)) {
                    RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);

                } else if (UNLIKELY((val2 == -1) && (val1 == AVM_INT_MIN))) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        return make_boxed_int64(ctx, fail_label, live, -((avm_int64_t) AVM_INT_MIN));

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        return int64_max_plus_one(ctx, fail_label, live);
                    #endif
                }

                return make_maybe_boxed_int(ctx, fail_label, live, val1 / val2);
            }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val1 = term_maybe_unbox_int64(arg1);
                avm_int64_t val2 = term_maybe_unbox_int64(arg2);

                if (UNLIKELY(val2 == 0)) {
                    RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);

                } else if (UNLIKELY((val2 == -1) && (val1 == INT64_MIN))) {
                    return int64_max_plus_one(ctx, fail_label, live);
                }

                return make_maybe_boxed_int64(ctx, fail_label, live, val1 / val2);
            }
        #endif

            default:
                return div_maybe_bigint(ctx, fail_label, live, arg1, arg2);
        }
    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

term bif_erlang_div_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t operand_b = term_to_int(arg2);
        if (operand_b != 0) {
            avm_int_t res = term_to_int(arg1) / operand_b;
            if (UNLIKELY(res == -MIN_NOT_BOXED_INT)) {
                return make_boxed_int(ctx, fail_label, live, -MIN_NOT_BOXED_INT);

            } else {
                return term_from_int(res);
            }
        } else {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }

    } else {
        return div_boxed_helper(ctx, fail_label, live, arg1, arg2);
    }
}

// TODO: implement an optimized version
// that just copies the given term but changes the sign
static term neg_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1)
{
    const intn_digit_t *m;
    size_t m_len;
    intn_integer_sign_t m_sign;
    term_to_bigint(arg1, &m, &m_len, &m_sign);

    intn_digit_t tmp_copy[INTN_MAX_RES_LEN];
    memcpy(tmp_copy, m, m_len * sizeof(intn_digit_t));

    return make_bigint(ctx, fail_label, live, tmp_copy, m_len, intn_negate_sign(m_sign));
}

static term neg_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1)
{
    if (term_is_float(arg1)) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t fresult = -farg1;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }

    if (term_is_boxed_integer(arg1)) {
        switch (term_boxed_size(arg1)) {
            case 0:
                //BUG
                AVM_ABORT();

            case 1: {
                avm_int_t val = term_unbox_int(arg1);
                switch (val) {
                    case (MAX_NOT_BOXED_INT + 1):
                        return term_from_int(MIN_NOT_BOXED_INT);

                    case AVM_INT_MIN:
                        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                            return make_boxed_int64(ctx, fail_label, live, -((avm_int64_t) val));

                        #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                            return int64_max_plus_one(ctx, fail_label, live);

                        #else
                            #error "Unsupported configuration."
                        #endif

                    default:
                        return make_boxed_int(ctx, fail_label, live, -val);
                }
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);

                if (val == INT64_MIN) {
                    return int64_max_plus_one(ctx, fail_label, live);

                } else {
                    // maybe boxed int64 since we need to handle -(AVM_INT_MAX + 1) that is
                    // AVM_INT_MIN that fits into a 32 bit boxed value
                    return make_maybe_boxed_int64(ctx, fail_label, live, -val);
                }
            }
            #endif
            default:
                return neg_bigint(ctx, fail_label, live, arg1);
        }
    } else {
        TRACE("error: arg1: 0x%lx\n", arg1);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

term bif_erlang_neg_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        avm_int_t int_val = term_to_int(arg1);
        if (UNLIKELY(int_val == MIN_NOT_BOXED_INT)) {
            return make_boxed_int(ctx, fail_label, live, -MIN_NOT_BOXED_INT);
        } else {
            return term_from_int(-int_val);
        }
    } else {
        return neg_boxed_helper(ctx, fail_label, live, arg1);
    }
}

// TODO: implement an optimized version
// that just copies the given term but changes the sign
static term abs_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1)
{
    const intn_digit_t *m;
    size_t m_len;
    intn_integer_sign_t discarded_sign;
    term_to_bigint(arg1, &m, &m_len, &discarded_sign);

    intn_digit_t tmp_copy[INTN_MAX_RES_LEN];
    memcpy(tmp_copy, m, m_len * sizeof(intn_digit_t));

    return make_bigint(ctx, fail_label, live, tmp_copy, m_len, IntNPositiveInteger);
}

static term abs_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1)
{
    if (term_is_float(arg1)) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t fresult;
        #if AVM_USE_SINGLE_PRECISION
            fresult = fabsf(farg1);
        #else
            fresult = fabs(farg1);
        #endif

        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }

    if (term_is_boxed_integer(arg1)) {
        switch (term_boxed_size(arg1)) {
            case 0:
                //BUG
                AVM_ABORT();

            case 1: {
                avm_int_t val = term_unbox_int(arg1);
                if (val >= 0) {
                    return arg1;
                }

                if (val == AVM_INT_MIN) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        return make_boxed_int64(ctx, fail_label, live, -((avm_int64_t) val));

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        return int64_max_plus_one(ctx, fail_label, live);

                    #else
                        #error "Unsupported configuration."
                    #endif

                } else {
                    return make_boxed_int(ctx, fail_label, live, -val);
                }
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);
                if (val >= 0) {
                    return arg1;
                }

                if (val == INT64_MIN) {
                    return int64_max_plus_one(ctx, fail_label, live);

                } else {
                    return make_boxed_int64(ctx, fail_label, live, -val);
                }
            }
            #endif
            default:
                return abs_bigint(ctx, fail_label, live, arg1);
        }
    } else {
        TRACE("error: arg1: 0x%lx\n", arg1);
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_abs_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        avm_int_t int_val = term_to_int(arg1);

        if (int_val < 0) {
            if (UNLIKELY(int_val == MIN_NOT_BOXED_INT)) {
                return make_boxed_int(ctx, fail_label, live, -MIN_NOT_BOXED_INT);
            } else {
                return term_from_int(-int_val);
            }
        } else {
            return arg1;
        }

    } else {
        return abs_boxed_helper(ctx, fail_label, live, arg1);
    }
}

static term rem_maybe_bigint(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    // 0 is always normalized to `term_from_int(0)`, so we can do this
    if (UNLIKELY(arg2 == term_from_int(0))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    intn_digit_t tmp_buf1[INTN_INT64_LEN];
    intn_digit_t tmp_buf2[INTN_INT64_LEN];

    const intn_digit_t *bn1;
    size_t bn1_len;
    intn_integer_sign_t bn1_sign;
    conv_term_to_bigint(arg1, tmp_buf1, &bn1, &bn1_len, &bn1_sign);
    const intn_digit_t *bn2;
    size_t bn2_len;
    intn_integer_sign_t bn2_sign;
    conv_term_to_bigint(arg2, tmp_buf2, &bn2, &bn2_len, &bn2_sign);

    int cmp_result = intn_cmp(bn1, bn1_len, bn2, bn2_len);
    if (cmp_result < 0) {
        // a rem b when |a| < |b| -> always a
        return arg1;
    } else if (cmp_result == 0) {
        // a rem b when |a| == |b| -> always 0
        return term_from_int(0);
    }

    intn_digit_t q[INTN_MAX_RES_LEN];
    intn_digit_t bigres[INTN_MAX_RES_LEN];
    size_t bigres_len;
    intn_divu(bn1, bn1_len, bn2, bn2_len, q, bigres, &bigres_len);

    return make_bigint(ctx, fail_label, live, bigres, bigres_len, bn1_sign);
}

static term rem_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (UNLIKELY(!term_is_integer(arg1))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (UNLIKELY(!term_is_integer(arg2))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            AVM_ABORT();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
            }

            return make_maybe_boxed_int(ctx, fail_label, live, val1 % val2);
        }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
            }

            return make_maybe_boxed_int64(ctx, fail_label, live, val1 % val2);
        }
        #endif

        default:
            return rem_maybe_bigint(ctx, fail_label, live, arg1, arg2);
    }
}

term bif_erlang_rem_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t operand_b = term_to_int(arg2);
        if (LIKELY(operand_b != 0)) {
            return term_from_int(term_to_int(arg1) % operand_b);

        } else {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }

    } else {
        return rem_boxed_helper(ctx, fail_label, live, arg1, arg2);
    }
}

static term float_to_integer_helper(
    avm_float_t fresult, Context *ctx, uint32_t fail_label, int live)
{
    if (LIKELY(isfinite(fresult))) {
        if ((fresult >= INT64_MIN_AS_AVM_FLOAT) && (fresult <= INT64_MAX_AS_AVM_FLOAT)) {
#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, fresult);
#else
            return make_maybe_boxed_int(ctx, fail_label, live, fresult);
#endif
        } else {
            intn_digit_t res[INTN_MAX_RES_LEN];
            intn_integer_sign_t sign;
            size_t len = intn_from_double(fresult, res, &sign);
            if (LIKELY(len > 0)) {
                return make_bigint(ctx, fail_label, live, res, len, sign);
            }
        }
    }

    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
}

term bif_erlang_ceil_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);

        avm_float_t fresult;
#if AVM_USE_SINGLE_PRECISION
        fresult = ceilf(fvalue);
#else
        fresult = ceil(fvalue);
#endif

        return float_to_integer_helper(fresult, ctx, fail_label, live);
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_floor_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);

        avm_float_t fresult;
#if AVM_USE_SINGLE_PRECISION
        fresult = floorf(fvalue);
#else
        fresult = floor(fvalue);
#endif

        return float_to_integer_helper(fresult, ctx, fail_label, live);
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_round_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);

        avm_float_t fresult;
#if AVM_USE_SINGLE_PRECISION
        fresult = roundf(fvalue);
#else
        fresult = round(fvalue);
#endif

        return float_to_integer_helper(fresult, ctx, fail_label, live);
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_trunc_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);

        avm_float_t fresult;
#if AVM_USE_SINGLE_PRECISION
        fresult = truncf(fvalue);
#else
        fresult = trunc(fvalue);
#endif

        return float_to_integer_helper(fresult, ctx, fail_label, live);
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_float_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    if (term_is_float(arg1)) {
        return arg1;
    }

    if (!term_is_any_integer(arg1)) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }

    avm_float_t fresult = term_conv_to_float(arg1);
    if (UNLIKELY(!isfinite(fresult))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    return term_from_float(fresult, &ctx->heap);
}

typedef int64_t (*bitwise_op)(int64_t a, int64_t b);
typedef size_t (*bitwise_big_op)(
        const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
        const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign,
        intn_digit_t out[], intn_integer_sign_t *out_sign);

static inline term bitwise_helper(
    Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, bitwise_op op, bitwise_big_op big_op)
{
    if (LIKELY(term_is_any_integer(arg1) && term_is_any_integer(arg2))) {
        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        size_t arg2_size = term_is_integer(arg2) ? 0 : term_boxed_size(arg2);
        if (MAX(arg1_size, arg2_size) <= BOXED_TERMS_REQUIRED_FOR_INT64) {
            int64_t a = term_maybe_unbox_int64(arg1);
            int64_t b = term_maybe_unbox_int64(arg2);
            int64_t result = op(a, b);

#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, result);
#else
            return make_maybe_boxed_int(ctx, fail_label, live, result);
#endif
        } else {
            intn_digit_t tmp_buf1[INTN_INT64_LEN];
            intn_digit_t tmp_buf2[INTN_INT64_LEN];
            const intn_digit_t *m;
            size_t m_len;
            intn_integer_sign_t m_sign;
            conv_term_to_bigint(arg1, tmp_buf1, &m, &m_len, &m_sign);
            const intn_digit_t *n;
            size_t n_len;
            intn_integer_sign_t n_sign;
            conv_term_to_bigint(arg2, tmp_buf2, &n, &n_len, &n_sign);

            intn_digit_t bigres[INTN_MAX_RES_LEN];
            intn_integer_sign_t bigres_sign;
            size_t bigres_len = big_op(m, m_len, m_sign, n, n_len, n_sign, bigres, &bigres_sign);

            return make_bigint(ctx, fail_label, live, bigres, bigres_len, bigres_sign);
        }
    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

static inline int64_t bor(int64_t a, int64_t b)
{
    return a | b;
}

term bif_erlang_bor_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 | arg2;
    } else {
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, bor, intn_bor);
    }
}

static inline int64_t band(int64_t a, int64_t b)
{
    return a & b;
}

term bif_erlang_band_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 & arg2;
    } else {
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, band, intn_band);
    }
}

static inline int64_t bxor(int64_t a, int64_t b)
{
    return a ^ b;
}

term bif_erlang_bxor_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return (arg1 ^ arg2) | TERM_INTEGER_TAG;
    } else {
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, bxor, intn_bxor);
    }
}

term bif_erlang_bsl_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_any_integer(arg1) && term_is_non_neg_int(arg2))) {
        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);
        avm_int_t b = term_to_int(arg2);
        if (arg1_size <= BOXED_TERMS_REQUIRED_FOR_INT64) {
            int64_t a = term_maybe_unbox_int64(arg1);
            int64_t result;
            if (!int64_bsl_overflow(a, b, &result)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
                    return make_maybe_boxed_int64(ctx, fail_label, live, result);
                #else
                    return make_maybe_boxed_int(ctx, fail_label, live, result);
                #endif
            }
        }

        intn_digit_t tmp_buf1[INTN_INT64_LEN];
        const intn_digit_t *m;
        size_t m_len;
        intn_integer_sign_t m_sign;
        conv_term_to_bigint(arg1, tmp_buf1, &m, &m_len, &m_sign);

        intn_digit_t bigres[INTN_MAX_RES_LEN];
        size_t bigres_len = intn_bsl(m, m_len, b, bigres);
        // this check is required in order to avoid out-of-bounds read in make_bigint
        if (UNLIKELY(bigres_len > INTN_MAX_RES_LEN)) {
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
        }

        return make_bigint(ctx, fail_label, live, bigres, bigres_len, m_sign);

    } else if (term_is_neg_int(arg2)) {
        term abs_arg2 = term_from_int(-term_to_int(arg2));
        return bif_erlang_bsr_2(ctx, fail_label, live, arg1, abs_arg2);

    } else if (UNLIKELY(term_is_any_integer(arg1) && term_is_any_integer(arg2))) {
        if (term_is_any_non_neg_integer(arg2)) {
            // This basically means we are shifting with a quantity bigger than 2^28
            // that is always overflow except when arg1 is 0:
            // 0 bsl HugeNumber is always allowed
            if (term_is_integer(arg1) && term_to_int(arg1) == 0) {
                return term_from_int(0);
            } else {
                // The BEAM raises system_limit error here, however
                // in AtomVM we have overflow that is more specific
                // so we are going for internal consistency over perfect BEAM compliance
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
            }
            // negative arg2 means bsr
        } else {
            return term_is_any_neg_integer(arg1) ? term_from_int(-1) : term_from_int(0);
        }

    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

term bif_erlang_bsr_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_any_integer(arg1) && term_is_non_neg_int(arg2))) {
        size_t arg1_size = term_is_integer(arg1) ? 0 : term_boxed_size(arg1);

        avm_int_t b = term_to_int(arg2);

        if (arg1_size <= BOXED_TERMS_REQUIRED_FOR_INT64) {
            int64_t a = term_maybe_unbox_int64(arg1);
            int64_t result = int64_bsr_safe(a, b);

            #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
                return make_maybe_boxed_int64(ctx, fail_label, live, result);
            #else
                return make_maybe_boxed_int(ctx, fail_label, live, result);
            #endif
        }

        intn_digit_t tmp_buf1[INTN_INT64_LEN];
        const intn_digit_t *m;
        size_t m_len;
        intn_integer_sign_t m_sign;
        conv_term_to_bigint(arg1, tmp_buf1, &m, &m_len, &m_sign);

        intn_digit_t bigres[INTN_MAX_RES_LEN];
        size_t bigres_len = intn_bsr(m, m_len, m_sign, b, bigres);

        return make_bigint(ctx, fail_label, live, bigres, bigres_len, m_sign);

    } else if (term_is_neg_int(arg2)) {
        term abs_arg2 = term_from_int(-term_to_int(arg2));
        return bif_erlang_bsl_2(ctx, fail_label, live, arg1, abs_arg2);

    } else if (UNLIKELY(term_is_any_integer(arg1) && term_is_any_integer(arg2))) {
        if (term_is_any_non_neg_integer(arg2)) {
            // This basically means we are shifting with a quantity bigger than 2^28
            // that is always 0
            return term_is_any_neg_integer(arg1) ? term_from_int(-1) : term_from_int(0);

            // negative arg2 means bsl
        } else {
            // This basically means we are shifting with a quantity bigger than 2^28
            // that is always overflow except when arg1 is 0:
            // 0 bsl HugeNumber is always allowed
            if (term_is_integer(arg1) && term_to_int(arg1) == 0) {
                return term_from_int(0);
            } else {
                // The BEAM raises system_limit error here, however
                // in AtomVM we have overflow that is more specific
                // so we are going for internal consistency over perfect BEAM compliance
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
            }
        }

    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

static term bnot_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1)
{
    if (term_is_boxed_integer(arg1)) {
        switch (term_boxed_size(arg1)) {
            case 0:
                //BUG
                AVM_ABORT();

            case 1: {
                avm_int_t val = term_unbox_int(arg1);
                return make_boxed_int(ctx, fail_label, live, ~val);
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);
                return make_boxed_int64(ctx, fail_label, live, ~val);
            }
            #endif
            default: {
                intn_digit_t tmp_buf1[INTN_INT64_LEN];
                const intn_digit_t *m;
                size_t m_len;
                intn_integer_sign_t m_sign;
                conv_term_to_bigint(arg1, tmp_buf1, &m, &m_len, &m_sign);

                intn_digit_t bigres[INTN_MAX_RES_LEN];
                intn_integer_sign_t bigres_sign;

                size_t bigres_len = intn_bnot(m, m_len, m_sign, bigres, &bigres_sign);

                return make_bigint(ctx, fail_label, live, bigres, bigres_len, bigres_sign);
            }
        }
    } else {
        TRACE("error: arg1: 0x%lx\n", arg1);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }
}

term bif_erlang_bnot_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        return ~arg1 | TERM_INTEGER_TAG;

    } else {
        return bnot_boxed_helper(ctx, fail_label, live, arg1);
    }
}

term bif_erlang_not_1(Context *ctx, uint32_t fail_label, term arg1)
{
    if (arg1 == TRUE_ATOM) {
        return FALSE_ATOM;

    } else if (arg1 == FALSE_ATOM) {
        return TRUE_ATOM;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_and_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    if ((arg1 == FALSE_ATOM) && (arg2 == FALSE_ATOM)) {
        return FALSE_ATOM;

    } else if ((arg1 == FALSE_ATOM) && (arg2 == TRUE_ATOM)) {
        return FALSE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == FALSE_ATOM)) {
        return FALSE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == TRUE_ATOM)) {
        return TRUE_ATOM;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_or_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    if ((arg1 == FALSE_ATOM) && (arg2 == FALSE_ATOM)) {
        return FALSE_ATOM;

    } else if ((arg1 == FALSE_ATOM) && (arg2 == TRUE_ATOM)) {
        return TRUE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == FALSE_ATOM)) {
        return TRUE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == TRUE_ATOM)) {
        return TRUE_ATOM;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_xor_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    if ((arg1 == FALSE_ATOM) && (arg2 == FALSE_ATOM)) {
        return FALSE_ATOM;

    } else if ((arg1 == FALSE_ATOM) && (arg2 == TRUE_ATOM)) {
        return TRUE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == FALSE_ATOM)) {
        return TRUE_ATOM;

    } else if ((arg1 == TRUE_ATOM) && (arg2 == TRUE_ATOM)) {
        return FALSE_ATOM;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }
}

term bif_erlang_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermEquals) {
        return TRUE_ATOM;
    } else if (result & (TermLessThan | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_not_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermLessThan | TermGreaterThan)) {
        return TRUE_ATOM;
    } else if (result == TermEquals) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_exactly_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    //TODO: 5.0 != 5
    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
    if (result == TermEquals) {
        return TRUE_ATOM;
    } else if (result & (TermLessThan | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_exactly_not_equal_to_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
    if (result & (TermLessThan | TermGreaterThan)) {
        return TRUE_ATOM;
    } else if (result == TermEquals) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_greater_than_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermGreaterThan) {
        return TRUE_ATOM;
    } else if (result & (TermEquals | TermLessThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_less_than_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermLessThan) {
        return TRUE_ATOM;
    } else if (result & (TermEquals | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_less_than_or_equal_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermLessThan | TermEquals)) {
        return TRUE_ATOM;
    } else if (result == TermGreaterThan) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_greater_than_or_equal_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermGreaterThan | TermEquals)) {
        return TRUE_ATOM;
    } else if (result & TermLessThan) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_get_1(Context *ctx, uint32_t fail_label, term arg1)
{
    term value;
    DictionaryFunctionResult result = dictionary_get(&ctx->dictionary, arg1, &value, ctx->global);
    if (UNLIKELY(result != DictionaryOk)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }

    return value;
}

term bif_erlang_min_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult r = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    return r == TermLessThan ? arg1 : arg2;
}

term bif_erlang_max_2(Context *ctx, uint32_t fail_label, term arg1, term arg2)
{
    TermCompareResult r = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
        RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
    }
    return r == TermGreaterThan ? arg1 : arg2;
}

term bif_erlang_size_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    if (term_is_binary(arg1)) {
        // For bitstrings, number of bytes is rounded down
        return bif_erlang_byte_size_1(ctx, fail_label, live, arg1);
    } else if (term_is_tuple(arg1)) {
        return bif_erlang_tuple_size_1(ctx, fail_label, arg1);
    }

    RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
}

static term list_to_atom(Context *ctx, term a_list, bool create_new, term *error_reason);

term bif_erlang_list_to_atom_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    term error_reason;
    term result = list_to_atom(ctx, arg1, true, &error_reason);
    if (UNLIKELY(term_is_invalid_term(result))) {
        RAISE_ERROR_BIF(fail_label, error_reason);
    }
    return result;
}

term bif_erlang_list_to_existing_atom_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    term error_reason;
    term result = list_to_atom(ctx, arg1, false, &error_reason);
    if (UNLIKELY(term_is_invalid_term(result))) {
        RAISE_ERROR_BIF(fail_label, error_reason);
    }
    return result;
}

static term list_to_atom(Context *ctx, term a_list, bool create_new, term *error_reason)
{
    if (UNLIKELY(!term_is_list(a_list))) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    }

    int ok;
    avm_int_t len = term_list_length(a_list, &ok);
    if (UNLIKELY(!ok)) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    }
    if (len > 255) {
        *error_reason = SYSTEM_LIMIT_ATOM;
        return term_invalid_term();
    }

    char *atom_string = interop_list_to_utf8_string(a_list, &ok);
    if (UNLIKELY(!ok)) {
        *error_reason = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    size_t atom_string_len = strlen(atom_string);

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    atom_index_t global_atom_index;
    enum AtomTableEnsureAtomResult ensure_result = atom_table_ensure_atom(ctx->global->atom_table, (const uint8_t *) atom_string, atom_string_len, atom_opts, &global_atom_index);
    switch (ensure_result) {
        case AtomTableEnsureAtomNotFound:
        case AtomTableEnsureAtomInvalidLen: {
            *error_reason = BADARG_ATOM;
            return term_invalid_term();
        }
        case AtomTableEnsureAtomAllocFail: {
            *error_reason = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        case AtomTableEnsureAtomOk: {
            return term_from_atom_index(global_atom_index);
        }
        default:
            UNREACHABLE();
    }
}

term bif_erlang_binary_to_atom_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    term error_reason;
    term result = binary_to_atom(ctx, arg1, arg2, true, &error_reason);
    if (UNLIKELY(term_is_invalid_term(result))) {
        RAISE_ERROR_BIF(fail_label, error_reason);
    }
    return result;
}

term bif_erlang_binary_to_existing_atom_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    UNUSED(live);

    term error_reason;
    term result = binary_to_atom(ctx, arg1, arg2, false, &error_reason);
    if (UNLIKELY(term_is_invalid_term(result))) {
        RAISE_ERROR_BIF(fail_label, error_reason);
    }
    return result;
}

term binary_to_atom(Context *ctx, term a_binary, term encoding, bool create_new, term *error_reason)
{
    if (UNLIKELY(!term_is_binary(a_binary))) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    }

    const char *atom_string = term_binary_data(a_binary);
    size_t atom_string_len = term_binary_size(a_binary);

    bool encode_latin1_to_utf8 = false;
    if (UNLIKELY((encoding == LATIN1_ATOM)
            && !unicode_buf_is_ascii((const uint8_t *) atom_string, atom_string_len))) {
        encode_latin1_to_utf8 = true;
    } else if (UNLIKELY((encoding != LATIN1_ATOM) && (encoding != UNICODE_ATOM)
                   && (encoding != UTF8_ATOM))) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    }

    const uint8_t *atom_data;
    size_t atom_data_len;
    uint8_t *buf = NULL;
    if (LIKELY(!encode_latin1_to_utf8)) {
        if (UNLIKELY(!unicode_is_valid_utf8_buf((const uint8_t *) atom_string, atom_string_len))) {
            *error_reason = BADARG_ATOM;
            return term_invalid_term();
        }
        size_t len = unicode_buf_utf8_len((const uint8_t *) atom_string, atom_string_len);
        if (UNLIKELY(len > 255)) {
            *error_reason = SYSTEM_LIMIT_ATOM;
            return term_invalid_term();
        }

        atom_data = (const uint8_t *) atom_string;
        atom_data_len = atom_string_len;
    } else {
        if (UNLIKELY(atom_string_len > 255)) {
            *error_reason = SYSTEM_LIMIT_ATOM;
            return term_invalid_term();
        }

        // * 2 is the worst case size
        size_t buf_len = atom_string_len * 2;
        buf = malloc(buf_len);
        if (IS_NULL_PTR(buf)) {
            *error_reason = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        atom_data = buf;
        atom_data_len = 0;
        for (size_t i = 0; i < atom_string_len; i++) {
            size_t out_size;
            bitstring_utf8_encode(((uint8_t) atom_string[i]), &buf[atom_data_len], &out_size);
            atom_data_len += out_size;
        }
    }

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    atom_index_t global_atom_index;
    enum AtomTableEnsureAtomResult ensure_result = atom_table_ensure_atom(ctx->global->atom_table, atom_data, atom_data_len, atom_opts, &global_atom_index);
    switch (ensure_result) {
        case AtomTableEnsureAtomNotFound:
        case AtomTableEnsureAtomInvalidLen: {
            *error_reason = BADARG_ATOM;
            return term_invalid_term();
        }
        case AtomTableEnsureAtomAllocFail: {
            *error_reason = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        case AtomTableEnsureAtomOk: {
            return term_from_atom_index(global_atom_index);
        }
        default:
            UNREACHABLE();
    }
}
