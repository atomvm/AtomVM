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
#include "defaultatoms.h"
#include "dictionary.h"
#include "overflow_helpers.h"
#include "trace.h"
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

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        RAISE_ERROR(BADARG_ATOM);              \
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

term bif_erlang_byte_size_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VALIDATE_VALUE(arg1, term_is_binary);

    return term_from_int32(term_binary_size(arg1));
}

term bif_erlang_bit_size_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VALIDATE_VALUE(arg1, term_is_binary);

    return term_from_int32(term_binary_size(arg1) * 8);
}

term bif_erlang_is_atom_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_atom(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_binary_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_binary(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_boolean_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return (arg1 == TRUE_ATOM || arg1 == FALSE_ATOM) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_float_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_float(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_function_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_function(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_integer_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_any_integer(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_list_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_list(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_number_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    //TODO: change to term_is_number
    return term_is_any_integer(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_pid_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_pid(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_reference_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_reference(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_tuple_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_tuple(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_map_1(Context *ctx, term arg1)
{
    UNUSED(ctx);

    return term_is_map(arg1) ? TRUE_ATOM : FALSE_ATOM;
}

term bif_erlang_is_map_key_2(Context *ctx, term arg1, term arg2)
{
    if (UNLIKELY(!term_is_map(arg2))) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        default:
            return TRUE_ATOM;
    }
}

term bif_erlang_length_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VALIDATE_VALUE(arg1, term_is_list);

    int proper;
    avm_int_t len = term_list_length(arg1, &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_int(len);
}

term bif_erlang_hd_1(Context *ctx, term arg1)
{
    VALIDATE_VALUE(arg1, term_is_nonempty_list);

    return term_get_list_head(arg1);
}

term bif_erlang_tl_1(Context *ctx, term arg1)
{
    VALIDATE_VALUE(arg1, term_is_nonempty_list);

    return term_get_list_tail(arg1);
}

term bif_erlang_element_2(Context *ctx, term arg1, term arg2)
{
    VALIDATE_VALUE(arg1, term_is_integer);
    VALIDATE_VALUE(arg2, term_is_tuple);

    // indexes are 1 based
    int elem_index = term_to_int32(arg1) - 1;
    if (LIKELY((elem_index >= 0) && (elem_index < term_get_tuple_arity(arg2)))) {
        return term_get_tuple_element(arg2, elem_index);

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_tuple_size_1(Context *ctx, term arg1)
{
    VALIDATE_VALUE(arg1, term_is_tuple);

    return term_from_int32(term_get_tuple_arity(arg1));
}

term bif_erlang_map_size_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (!UNLIKELY(term_is_map(arg1))) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADMAP_ATOM);
        term_put_tuple_element(err, 1, arg1);

        RAISE_ERROR(err);
    }

    return term_from_int32(term_get_map_size(arg1));
}

term bif_erlang_map_get_2(Context *ctx, term arg1, term arg2)
{
    if (!UNLIKELY(term_is_map(arg2))) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADMAP_ATOM);
        term_put_tuple_element(err, 1, arg2);

        RAISE_ERROR(err);
    }

    int pos = term_find_map_pos(arg2, arg1, ctx->global);
    if (pos == TERM_MAP_NOT_FOUND) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, 3, 1, &arg1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term err = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err, 0, BADKEY_ATOM);
        term_put_tuple_element(err, 1, arg1);

        RAISE_ERROR(err);
    } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_get_map_value(arg2, pos);
}

static inline term make_boxed_int(Context *ctx, avm_int_t value)
{
    if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int(value, &ctx->heap);
}

#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
static inline term make_boxed_int64(Context *ctx, avm_int64_t value)
{
    if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT64_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int64(value, &ctx->heap);
}
#endif

static inline term make_maybe_boxed_int(Context *ctx, avm_int_t value)
{
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return make_boxed_int(ctx, value);

    } else {
        return term_from_int(value);
    }
}

#if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
static inline term make_maybe_boxed_int64(Context *ctx, avm_int64_t value)
{
    if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
        return make_boxed_int64(ctx, value);

    } else if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        return make_boxed_int(ctx, value);

    } else {
        return term_from_int(value);
    }
}
#endif

static term add_overflow_helper(Context *ctx, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, val1 + val2);
}

static term add_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 + farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR(BADARITH_ATOM);
        }

        if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
                    return make_boxed_int64(ctx, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_ADD_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: val1: " AVM_INT64_FMT ", val2: " AVM_INT64_FMT "\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, res);
        }
    #endif

        default:
            RAISE_ERROR(OVERFLOW_ATOM);
    }
}

term bif_erlang_add_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //TODO: use long integer instead, and term_to_longint
        avm_int_t res;
        if (!BUILTIN_ADD_OVERFLOW((avm_int_t) (arg1 & ~TERM_INTEGER_TAG), (avm_int_t) (arg2 & ~TERM_INTEGER_TAG), &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return add_overflow_helper(ctx, arg1, arg2);
        }
    } else {
        return add_boxed_helper(ctx, arg1, arg2);
    }
}

static term sub_overflow_helper(Context *ctx, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    return make_boxed_int(ctx, val1 - val2);
}

static term sub_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 - farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR(BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
                    return make_boxed_int64(ctx, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_SUB_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: val1: " AVM_INT64_FMT ", val2: " AVM_INT64_FMT "\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, res);
        }
    #endif

        default:
            RAISE_ERROR(OVERFLOW_ATOM);
    }
}

term bif_erlang_sub_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        //TODO: use long integer instead, and term_to_longint
        avm_int_t res;
        if (!BUILTIN_SUB_OVERFLOW((avm_int_t) (arg1 & ~TERM_INTEGER_TAG), (avm_int_t) (arg2 & ~TERM_INTEGER_TAG), &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return sub_overflow_helper(ctx, arg1, arg2);
        }
    } else {
        return sub_boxed_helper(ctx, arg1, arg2);
    }
}

static term mul_overflow_helper(Context *ctx, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    avm_int_t res;
#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
    avm_int64_t res64;
#endif

    if (!BUILTIN_MUL_OVERFLOW_INT(val1, val2, &res)) {
        return make_boxed_int(ctx, res);

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
    } else if (!BUILTIN_MUL_OVERFLOW_INT64((avm_int64_t) val1, (avm_int64_t) val2, &res64)) {
        return make_boxed_int64(ctx, res64);
#endif

    } else {
        RAISE_ERROR(OVERFLOW_ATOM);
    }
}

static term mul_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int use_float = 0;
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (term_is_float(arg1)) {
        use_float = 1;
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (term_is_float(arg2)) {
        use_float = 1;
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    if (use_float) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t farg2 = term_conv_to_float(arg2);
        avm_float_t fresult = farg1 * farg2;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR(BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
                    return make_boxed_int64(ctx, res64);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return make_maybe_boxed_int(ctx, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_MUL_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);
            }

            return make_maybe_boxed_int64(ctx, res);
        }
    #endif

        default:
            RAISE_ERROR(OVERFLOW_ATOM);
    }
}

term bif_erlang_mul_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t res;
        avm_int_t a = ((avm_int_t) (arg1 & ~TERM_INTEGER_TAG)) >> 2;
        avm_int_t b = ((avm_int_t) (arg2 & ~TERM_INTEGER_TAG)) >> 2;
        if (!BUILTIN_MUL_OVERFLOW(a, b, &res)) {
            return res | TERM_INTEGER_TAG;
        } else {
            return mul_overflow_helper(ctx, arg1, arg2);
        }
    } else {
        return mul_boxed_helper(ctx, arg1, arg2);
    }
}

static term div_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (UNLIKELY(!term_is_integer(arg1))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (UNLIKELY(!term_is_integer(arg2))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
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
                RAISE_ERROR(BADARITH_ATOM);

            } else if (UNLIKELY((val2 == -1) && (val1 == AVM_INT_MIN))) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    return make_boxed_int64(ctx, -((avm_int64_t) AVM_INT_MIN));

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #endif

            } else {
                return make_maybe_boxed_int(ctx, val1 / val2);
            }
        }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR(BADARITH_ATOM);

            } else if (UNLIKELY((val2 == -1) && (val1 == INT64_MIN))) {
                TRACE("overflow: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);

            } else {
                return make_maybe_boxed_int64(ctx, val1 / val2);
            }
        }
        #endif

        default:
            RAISE_ERROR(OVERFLOW_ATOM);
    }
}

term bif_erlang_div_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t operand_b = term_to_int(arg2);
        if (operand_b != 0) {
            avm_int_t res = term_to_int(arg1) / operand_b;
            if (UNLIKELY(res == -MIN_NOT_BOXED_INT)) {
                return make_boxed_int(ctx, -MIN_NOT_BOXED_INT);

            } else {
                return term_from_int(res);
            }
        } else {
            RAISE_ERROR(BADARITH_ATOM);
        }

    } else {
        return div_boxed_helper(ctx, arg1, arg2);
    }
}

static term neg_boxed_helper(Context *ctx, term arg1)
{
    if (term_is_float(arg1)) {
        avm_float_t farg1 = term_conv_to_float(arg1);
        avm_float_t fresult = -farg1;
        if (UNLIKELY(!isfinite(fresult))) {
            RAISE_ERROR(BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
                            return make_boxed_int64(ctx, -((avm_int64_t) val));

                        #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                            TRACE("overflow: val: " AVM_INT_FMT "\n", val);
                            RAISE_ERROR(OVERFLOW_ATOM);

                        #else
                            #error "Unsupported configuration."
                        #endif

                    default:
                        return make_boxed_int(ctx, -val);
                }
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);

                if (val == INT64_MIN) {
                    TRACE("overflow: arg1: " AVM_INT64_FMT "\n", arg1);
                    RAISE_ERROR(OVERFLOW_ATOM);

                } else {
                    return make_boxed_int64(ctx, -val);
                }
            }
            #endif
            default:
                RAISE_ERROR(OVERFLOW_ATOM);
        }
    } else {
        TRACE("error: arg1: 0x%lx\n", arg1);
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_neg_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        avm_int_t int_val = term_to_int(arg1);
        if (UNLIKELY(int_val == MIN_NOT_BOXED_INT)) {
            return make_boxed_int(ctx, -MIN_NOT_BOXED_INT);
        } else {
            return term_from_int(-int_val);
        }
    } else {
        return neg_boxed_helper(ctx, arg1);
    }
}

static term abs_boxed_helper(Context *ctx, term arg1)
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
            RAISE_ERROR(BADARITH_ATOM);
        }
        if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
                        return make_boxed_int64(ctx, -((avm_int64_t) val));

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        TRACE("overflow: val: " AVM_INT_FMT "\n", val);
                        RAISE_ERROR(OVERFLOW_ATOM);

                    #else
                        #error "Unsupported configuration."
                    #endif

                } else {
                    return make_boxed_int(ctx, -val);
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
                    RAISE_ERROR(OVERFLOW_ATOM);

                } else {
                    return make_boxed_int64(ctx, -val);
                }
            }
            #endif
            default:
                RAISE_ERROR(OVERFLOW_ATOM);
        }
    } else {
        TRACE("error: arg1: 0x%lx\n", arg1);
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_abs_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        avm_int_t int_val = term_to_int(arg1);

        if (int_val < 0) {
            if (UNLIKELY(int_val == MIN_NOT_BOXED_INT)) {
                return make_boxed_int(ctx, -MIN_NOT_BOXED_INT);
            } else {
                return term_from_int(-int_val);
            }
        } else {
            return arg1;
        }

    } else {
        return abs_boxed_helper(ctx, arg1);
    }
}

static term rem_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (UNLIKELY(!term_is_integer(arg1))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (UNLIKELY(!term_is_integer(arg2))) {
        TRACE("error: arg1: 0x%lx, arg2: 0x%lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
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
                RAISE_ERROR(BADARITH_ATOM);
            }

            return make_maybe_boxed_int(ctx, val1 % val2);
        }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR(BADARITH_ATOM);
            }

            return make_maybe_boxed_int64(ctx, val1 % val2);
        }
        #endif

        default:
            RAISE_ERROR(OVERFLOW_ATOM);
    }
}

term bif_erlang_rem_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        avm_int_t operand_b = term_to_int(arg2);
        if (LIKELY(operand_b != 0)) {
            return term_from_int(term_to_int(arg1) % operand_b);

        } else {
            RAISE_ERROR(BADARITH_ATOM);
        }

    } else {
        return rem_boxed_helper(ctx, arg1, arg2);
    }
}

term bif_erlang_ceil_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR(OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = ceilf(fvalue);
        #else
            result = ceil(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, result);
        #else
            return make_maybe_boxed_int(ctx, result);
        #endif
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_floor_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR(OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = floorf(fvalue);
        #else
            result = floor(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, result);
        #else
            return make_maybe_boxed_int(ctx, result);
        #endif
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_round_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR(OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = llroundf(fvalue);
        #else
            result = llround(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, result);
        #else
            return make_maybe_boxed_int(ctx, result);
        #endif
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_trunc_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (term_is_float(arg1)) {
        avm_float_t fvalue = term_to_float(arg1);
        if ((fvalue <= INT64_MIN_AS_AVM_FLOAT) || (fvalue >= INT64_MAX_AS_AVM_FLOAT)) {
            RAISE_ERROR(OVERFLOW_ATOM);
        }

        avm_int64_t result;
        #if AVM_USE_SINGLE_PRECISION
            result = truncf(fvalue);
        #else
            result = trunc(fvalue);
        #endif

        #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
            return make_maybe_boxed_int64(ctx, result);
        #else
            return make_maybe_boxed_int(ctx, result);
        #endif
    }

    if (term_is_any_integer(arg1)) {
        return arg1;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

typedef int64_t (*bitwise_op)(int64_t a, int64_t b);

static inline term bitwise_helper(Context *ctx, int live, term arg1, term arg2, bitwise_op op)
{
    UNUSED(live);

    if (UNLIKELY(!term_is_any_integer(arg1) || !term_is_any_integer(arg2))) {
        RAISE_ERROR(BADARITH_ATOM);
    }

    int64_t a = term_maybe_unbox_int64(arg1);
    int64_t b = term_maybe_unbox_int64(arg2);
    int64_t result = op(a, b);

    #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
        return make_maybe_boxed_int64(ctx, result);
    #else
        return make_maybe_boxed_int(ctx, result);
    #endif
}

static inline int64_t bor(int64_t a, int64_t b)
{
    return a | b;
}

term bif_erlang_bor_2(Context *ctx, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 | arg2;
    } else {
        return bitwise_helper(ctx, live, arg1, arg2, bor);
    }
}

static inline int64_t band(int64_t a, int64_t b)
{
    return a & b;
}

term bif_erlang_band_2(Context *ctx, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 & arg2;
    } else {
        return bitwise_helper(ctx, live, arg1, arg2, band);
    }
}

static inline int64_t bxor(int64_t a, int64_t b)
{
    return a ^ b;
}

term bif_erlang_bxor_2(Context *ctx, int live, term arg1, term arg2)
{
    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return (arg1 ^ arg2) | TERM_INTEGER_TAG;
    } else {
        return bitwise_helper(ctx, live, arg1, arg2, bxor);
    }
}

typedef int64_t (*bitshift_op)(int64_t a, avm_int_t b);

static inline term bitshift_helper(Context *ctx, int live, term arg1, term arg2, bitshift_op op)
{
    UNUSED(live);

    if (UNLIKELY(!term_is_any_integer(arg1) || !term_is_integer(arg2))) {
        RAISE_ERROR(BADARITH_ATOM);
    }

    int64_t a = term_maybe_unbox_int64(arg1);
    avm_int_t b = term_to_int(arg2);
    int64_t result = op(a, b);

    #if BOXED_TERMS_REQUIRED_FOR_INT64 > 1
        return make_maybe_boxed_int64(ctx, result);
    #else
        return make_maybe_boxed_int(ctx, result);
    #endif
}

static inline int64_t bsl(int64_t a, avm_int_t b)
{
    // TODO check for overflow
    return a << b;
}

term bif_erlang_bsl_2(Context *ctx, int live, term arg1, term arg2)
{
    return bitshift_helper(ctx, live, arg1, arg2, bsl);
}

static inline int64_t bsr(int64_t a, avm_int_t b)
{
    // TODO check for underflow
    return a >> b;
}

term bif_erlang_bsr_2(Context *ctx, int live, term arg1, term arg2)
{
    return bitshift_helper(ctx, live, arg1, arg2, bsr);
}

term bif_erlang_bnot_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        return ~arg1 | TERM_INTEGER_TAG;

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_not_1(Context *ctx, term arg1)
{
    if (arg1 == TRUE_ATOM) {
        return FALSE_ATOM;

    } else if (arg1 == FALSE_ATOM) {
        return TRUE_ATOM;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_and_2(Context *ctx, term arg1, term arg2)
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
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_or_2(Context *ctx, term arg1, term arg2)
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
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_xor_2(Context *ctx, term arg1, term arg2)
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
        RAISE_ERROR(BADARG_ATOM);
    }
}

term bif_erlang_equal_to_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermEquals) {
        return TRUE_ATOM;
    } else if (result & (TermLessThan | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermLessThan | TermGreaterThan)) {
        return TRUE_ATOM;
    } else if (result == TermEquals) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_exactly_equal_to_2(Context *ctx, term arg1, term arg2)
{
    //TODO: 5.0 != 5
    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
    if (result == TermEquals) {
        return TRUE_ATOM;
    } else if (result & (TermLessThan | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_exactly_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareExact, ctx->global);
    if (result & (TermLessThan | TermGreaterThan)) {
        return TRUE_ATOM;
    } else if (result == TermEquals) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_greater_than_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermGreaterThan) {
        return TRUE_ATOM;
    } else if (result & (TermEquals | TermLessThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_less_than_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result == TermLessThan) {
        return TRUE_ATOM;
    } else if (result & (TermEquals | TermGreaterThan)) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_less_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermLessThan | TermEquals)) {
        return TRUE_ATOM;
    } else if (result == TermGreaterThan) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_greater_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult result = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (result & (TermGreaterThan | TermEquals)) {
        return TRUE_ATOM;
    } else if (result & TermLessThan) {
        return FALSE_ATOM;
    } else {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
}

term bif_erlang_get_1(Context *ctx, term arg1)
{
    term value;
    DictionaryFunctionResult result = dictionary_get(&ctx->dictionary, arg1, &value, ctx->global);
    if (UNLIKELY(result != DictionaryOk)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return value;
}

term bif_erlang_min_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult r = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);       
    }
    return r == TermLessThan ? arg1 : arg2;
}

term bif_erlang_max_2(Context *ctx, term arg1, term arg2)
{
    TermCompareResult r = term_compare(arg1, arg2, TermCompareNoOpts, ctx->global);
    if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);       
    }
    return r == TermGreaterThan ? arg1 : arg2;
}
