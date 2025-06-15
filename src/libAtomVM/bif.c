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
#include "overflow_helpers.h"
#include "smp.h"
#include "term.h"
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

const struct ExportedFunction *bif_registry_get_handler(AtomString module, AtomString function, int arity)
{
    char bifname[MAX_BIF_NAME_LEN];

    atom_write_mfa(bifname, MAX_BIF_NAME_LEN, module, function, arity);
    const BifNameAndPtr *nameAndPtr = in_word_set(bifname, strlen(bifname));
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
    VALIDATE_VALUE_BIF(fail_label, arg2, term_is_any_integer);

    if (!term_is_integer(arg2)) {
        // function takes any positive integer, including big integers
        // but internally we use only small integers
        return FALSE_ATOM;
    }
    avm_int_t arity = term_to_int(arg2);
    if (arity < 0) {
        RAISE_ERROR_BIF(fail_label, BADARG_ATOM);
    }

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

static term add_overflow_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, fail_label, live, val1 + val2);
}

static term add_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 + farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }

        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }

    switch (size) {
        case 0: {
            //BUG
            AVM_ABORT();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_ADD_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 + (avm_int64_t) val2;
                    return make_boxed_int64(ctx, fail_label, live, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, fail_label, live, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_ADD_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: val1: " AVM_INT64_FMT ", val2: " AVM_INT64_FMT "\n", arg1, arg2);
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, fail_label, live, res);
        }
    #endif

        default:
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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

static term sub_overflow_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, fail_label, live, val1 - val2);
}

static term sub_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 - farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }

    switch (size) {
        case 0: {
            //BUG
            AVM_ABORT();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_SUB_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 - (avm_int64_t) val2;
                    return make_boxed_int64(ctx, fail_label, live, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, fail_label, live, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_SUB_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: val1: " AVM_INT64_FMT ", val2: " AVM_INT64_FMT "\n", arg1, arg2);
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, fail_label, live, res);
        }
    #endif

        default:
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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

static term mul_overflow_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
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
        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
    }
}

static term mul_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 * farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, FLOAT_SIZE, live, ctx->x, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR_BIF(fail_label, OUT_OF_MEMORY_ATOM);
        }
        return term_from_float(fresult, &ctx->heap);
    }

    switch (size) {
        case 0: {
            //BUG
            AVM_ABORT();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_MUL_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 * (avm_int64_t) val2;
                    return make_boxed_int64(ctx, fail_label, live, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, fail_label, live, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_MUL_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, fail_label, live, res);
        }
    #endif

        default:
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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
    if (!term_is_integer(arg1) && !term_is_float(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    if (!term_is_integer(arg2) && !term_is_float(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
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

static term div_boxed_helper(Context *ctx, uint32_t fail_label, uint32_t live, term arg1, term arg2)
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

            } else if (UNLIKELY((val2 == -1) && (val1 == AVM_INT_MIN))) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    return make_boxed_int64(ctx, fail_label, live, -((avm_int64_t) AVM_INT_MIN));

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
                #endif

            } else {
                return make_maybe_boxed_int(ctx, fail_label, live, val1 / val2);
            }
        }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);

            } else if (UNLIKELY((val2 == -1) && (val1 == INT64_MIN))) {
                TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);

            } else {
                return make_maybe_boxed_int64(ctx, fail_label, live, val1 / val2);
            }
        }
        #endif

        default:
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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
                            TRACE("overflow: val: " AVM_INT_FMT "\n", val);
                            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);

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
                    TRACE("overflow: arg1: " AVM_INT64_FMT "\n", arg1);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);

                } else {
                    return make_boxed_int64(ctx, fail_label, live, -val);
                }
            }
            #endif
            default:
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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
                        TRACE("overflow: val: " AVM_INT_FMT "\n", val);
                        RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);

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
                    TRACE("overflow: val:" AVM_INT64_FMT "\n", val);
                    RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);

                } else {
                    return make_boxed_int64(ctx, fail_label, live, -val);
                }
            }
            #endif
            default:
                RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
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

term bif_erlang_ceil_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = ceilf(fvalue);
        #else
            result = ceil(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, result);
        #else
            return make_maybe_boxed_int(ctx, fail_label, live, result);
        #endif
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
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = floorf(fvalue);
        #else
            result = floor(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, result);
        #else
            return make_maybe_boxed_int(ctx, fail_label, live, result);
        #endif
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
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = llroundf(fvalue);
        #else
            result = llround(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, result);
        #else
            return make_maybe_boxed_int(ctx, fail_label, live, result);
        #endif
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
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR_BIF(fail_label, OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = truncf(fvalue);
        #else
            result = trunc(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, fail_label, live, result);
        #else
            return make_maybe_boxed_int(ctx, fail_label, live, result);
        #endif
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

static inline term bitwise_helper(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, bitwise_op op)
{
    UNUSED(live);

    if (UNLIKELY(!term_is_any_integer(arg1) || !term_is_any_integer(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    int64_t a = term_maybe_unbox_int64(arg1);
    int64_t b = term_maybe_unbox_int64(arg2);
    int64_t result = op(a, b);

    #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
        return make_maybe_boxed_int64(ctx, fail_label, live, result);
    #else
        return make_maybe_boxed_int(ctx, fail_label, live, result);
    #endif
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
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, bor);
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
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, band);
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
        return bitwise_helper(ctx, fail_label, live, arg1, arg2, bxor);
    }
}

typedef int64_t (*bitshift_op)(int64_t a, avm_int_t b);

static inline term bitshift_helper(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2, bitshift_op op)
{
    UNUSED(live);

    if (UNLIKELY(!term_is_any_integer(arg1) || !term_is_integer(arg2))) {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
    }

    int64_t a = term_maybe_unbox_int64(arg1);
    avm_int_t b = term_to_int(arg2);
    int64_t result = op(a, b);

    #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
        return make_maybe_boxed_int64(ctx, fail_label, live, result);
    #else
        return make_maybe_boxed_int(ctx, fail_label, live, result);
    #endif
}

static inline int64_t bsl(int64_t a, avm_int_t b)
{
    // TODO check for overflow
    return a << b;
}

term bif_erlang_bsl_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    return bitshift_helper(ctx, fail_label, live, arg1, arg2, bsl);
}

static inline int64_t bsr(int64_t a, avm_int_t b)
{
    // TODO check for underflow
    return a >> b;
}

term bif_erlang_bsr_2(Context *ctx, uint32_t fail_label, int live, term arg1, term arg2)
{
    return bitshift_helper(ctx, fail_label, live, arg1, arg2, bsr);
}

term bif_erlang_bnot_1(Context *ctx, uint32_t fail_label, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        return ~arg1 | TERM_INTEGER_TAG;

    } else {
        RAISE_ERROR_BIF(fail_label, BADARITH_ATOM);
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
    char *atom_string = interop_list_to_utf8_string(a_list, &ok);
    if (UNLIKELY(!ok)) {
        *error_reason = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        *error_reason = SYSTEM_LIMIT_ATOM;
        return term_invalid_term();
    }

    AtomString atom = malloc(atom_string_len + 1);
    if (IS_NULL_PTR(atom)) {
        free(atom_string);
        *error_reason = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    ((uint8_t *) atom)[0] = atom_string_len;
    memcpy(((char *) atom) + 1, atom_string, atom_string_len);
    free(atom_string);

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    long global_atom_index = atom_table_ensure_atom(ctx->global->atom_table, atom, atom_opts);
    free((void *) atom);
    if (UNLIKELY(global_atom_index == ATOM_TABLE_NOT_FOUND)) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    } else if (UNLIKELY(global_atom_index == ATOM_TABLE_ALLOC_FAIL)) {
        *error_reason = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    return term_from_atom_index(global_atom_index);
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
    if (UNLIKELY(atom_string_len > 255)) {
        *error_reason = SYSTEM_LIMIT_ATOM;
        return term_invalid_term();
    }

    bool encode_latin1_to_utf8 = false;
    if (UNLIKELY((encoding == LATIN1_ATOM)
            && !unicode_buf_is_ascii((const uint8_t *) atom_string, atom_string_len))) {
        encode_latin1_to_utf8 = true;
    } else if (UNLIKELY((encoding != LATIN1_ATOM) && (encoding != UNICODE_ATOM)
                   && (encoding != UTF8_ATOM))) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    }

    AtomString atom;
    if (LIKELY(!encode_latin1_to_utf8)) {
        if (UNLIKELY(!unicode_is_valid_utf8_buf((const uint8_t *) atom_string, atom_string_len))) {
            *error_reason = BADARG_ATOM;
            return term_invalid_term();
        }
        atom = malloc(atom_string_len + 1);
        if (IS_NULL_PTR(atom)) {
            *error_reason = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        ((uint8_t *) atom)[0] = atom_string_len;
        memcpy(((char *) atom) + 1, atom_string, atom_string_len);
    } else {
        // * 2 is the worst case size
        size_t buf_len = atom_string_len * 2;
        atom = malloc(buf_len + 1);
        if (IS_NULL_PTR(atom)) {
            *error_reason = OUT_OF_MEMORY_ATOM;
            return term_invalid_term();
        }
        uint8_t *atom_data = ((uint8_t *) atom) + 1;
        size_t out_pos = 0;
        for (size_t i = 0; i < atom_string_len; i++) {
            size_t out_size;
            bitstring_utf8_encode(((uint8_t) atom_string[i]), &atom_data[out_pos], &out_size);
            out_pos += out_size;
        }
        if (out_pos > 255) {
            free((void *) atom);
            *error_reason = SYSTEM_LIMIT_ATOM;
            return term_invalid_term();
        }
        ((uint8_t *) atom)[0] = out_pos;
    }

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    long global_atom_index = atom_table_ensure_atom(ctx->global->atom_table, atom, atom_opts);
    free((void *) atom);
    if (UNLIKELY(global_atom_index == ATOM_TABLE_NOT_FOUND)) {
        *error_reason = BADARG_ATOM;
        return term_invalid_term();
    } else if (UNLIKELY(global_atom_index == ATOM_TABLE_ALLOC_FAIL)) {
        *error_reason = OUT_OF_MEMORY_ATOM;
        return term_invalid_term();
    }
    return term_from_atom_index(global_atom_index);
}
