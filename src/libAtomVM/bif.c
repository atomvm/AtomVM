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
#include "defaultatoms.h"
#include "overflow_helpers.h"
#include "trace.h"
#include "utils.h"

//Ignore warning caused by gperf generated code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#include "bifs_hash.h"
#pragma GCC diagnostic pop

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM; \
    ctx->x[1] = (error_type_atom); \
    return term_invalid_term();

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        RAISE_ERROR(BADARG_ATOM); \
    }

BifImpl bif_registry_get_handler(AtomString module, AtomString function, int arity)
{
    char bifname[MAX_BIF_NAME_LEN];

    atom_write_mfa(bifname, MAX_BIF_NAME_LEN, module, function, arity);
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

    VALIDATE_VALUE(arg1, term_is_binary);

    return term_from_int32(term_binary_size(arg1));
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

term bif_erlang_length_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    VALIDATE_VALUE(arg1, term_is_list);

    return term_from_int32(term_list_length(arg1));
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

static inline term term_make_maybe_boxed_int(Context *ctx, avm_int_t value)
{
    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_make_boxed_int(value, ctx);

    } else {
        return term_from_int(value);
    }
}

static inline term term_make_maybe_boxed_int64(Context *ctx, avm_int64_t value)
{
    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
            if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return term_make_boxed_int64(value, ctx);

        }
    #endif

    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_make_boxed_int(value, ctx);

    } else {
        return term_from_int(value);
    }
}

static term add_overflow_helper(Context *ctx, term arg1, term arg2)
{
    avm_int_t val1 = term_to_int(arg1);
    avm_int_t val2 = term_to_int(arg2);

    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int(val1 + val2, ctx);
}

static term add_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            abort();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_ADD_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 + (avm_int64_t) val2;

                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(res64, ctx);
                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return term_make_maybe_boxed_int(ctx, res);
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

            return term_make_maybe_boxed_int64(ctx, res);
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

    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int(val1 - val2, ctx);
}

static term sub_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            abort();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_SUB_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 + (avm_int64_t) val2;

                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(res64, ctx);
                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return term_make_maybe_boxed_int(ctx, res);
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

            return term_make_maybe_boxed_int64(ctx, res);
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
        if (UNLIKELY(memory_ensure_free(ctx, BOXED_TERMS_REQUIRED_FOR_INT) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_make_boxed_int(res, ctx);

#if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
    } else if (!BUILTIN_MUL_OVERFLOW_INT64((avm_int64_t) val1, (avm_int64_t) val2, &res64)) {
        if (UNLIKELY(memory_ensure_free(ctx, BOXED_TERMS_REQUIRED_FOR_INT64) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_make_boxed_int64(res64, ctx);
#endif

    } else {
        RAISE_ERROR(OVERFLOW_ATOM);
    }
}

static term mul_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (!term_is_integer(arg1)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (!term_is_integer(arg2)) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            abort();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            avm_int_t res;

            if (BUILTIN_MUL_OVERFLOW_INT(val1, val2, &res)) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    avm_int64_t res64 = (avm_int64_t) val1 * (avm_int64_t) val2;

                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(res64, ctx);
                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: " AVM_INT64_FMT ", arg2: " AVM_INT64_FMT "\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #else
                    #error "Unsupported configuration."
                #endif
            }

            return term_make_maybe_boxed_int(ctx, res);
        }

    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            avm_int64_t res;

            if (BUILTIN_MUL_OVERFLOW_INT64(val1, val2, &res)) {
                TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);
            }

            return term_make_maybe_boxed_int64(ctx, res);
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

static term make_minus_min_not_boxed_int(Context *ctx)
{
    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_make_boxed_int(-MIN_NOT_BOXED_INT, ctx);
}

static term div_boxed_helper(Context *ctx, term arg1, term arg2)
{
    int size = 0;
    if (term_is_boxed_integer(arg1)) {
        size = term_boxed_size(arg1);
    } else if (UNLIKELY(!term_is_integer(arg1))) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (UNLIKELY(!term_is_integer(arg2))) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            abort();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR(BADARITH_ATOM);

            } else if (UNLIKELY((val2 == -1) && (val1 == AVM_INT_MIN))) {
                #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(-((avm_int64_t) AVM_INT_MIN), ctx);

                #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                    TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);
                #endif

            } else {
                return term_make_maybe_boxed_int(ctx, val1 / val2);
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
                TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                RAISE_ERROR(OVERFLOW_ATOM);

            } else {
                return term_make_maybe_boxed_int64(ctx, val1 / val2);
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
                return make_minus_min_not_boxed_int(ctx);

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
    if (term_is_boxed_integer(arg1)) {
        switch (term_boxed_size(arg1)) {
            case 0:
                //BUG
                abort();

            case 1: {
                avm_int_t val = term_unbox_int(arg1);
                switch (val) {
                    case (MAX_NOT_BOXED_INT + 1):
                        return term_from_int(MIN_NOT_BOXED_INT);

                    case AVM_INT_MIN:
                        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                            if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                            }

                            return term_make_boxed_int64(-((avm_int64_t) val), ctx);

                        #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                            TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                            RAISE_ERROR(OVERFLOW_ATOM);

                        #else
                            #error "Unsupported configuration."
                        #endif

                    default:
                        if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }

                        return term_make_boxed_int(-val, ctx);
                }
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);

                if (val == INT64_MIN) {
                    TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);

                } else {
                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(-val, ctx);
                }
            }
            #endif
            default:
                RAISE_ERROR(OVERFLOW_ATOM);
        }
    } else {
        TRACE("error: arg1: %lx\n", arg1);
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_neg_1(Context *ctx, int live, term arg1)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1))) {
        avm_int_t int_val = term_to_int(arg1);
        if (UNLIKELY(int_val == MIN_NOT_BOXED_INT)) {
            return make_minus_min_not_boxed_int(ctx);
        } else {
            return term_from_int(-int_val);
        }
    } else {
        return neg_boxed_helper(ctx, arg1);
    }
}

static term abs_boxed_helper(Context *ctx, term arg1)
{
    if (term_is_boxed_integer(arg1)) {
        switch (term_boxed_size(arg1)) {
            case 0:
                //BUG
                abort();

            case 1: {
                avm_int_t val = term_unbox_int(arg1);
                if (val >= 0) {
                    return arg1;
                }

                if (val == AVM_INT_MIN) {
                    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
                        if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                        }

                        return term_make_boxed_int64(-((avm_int64_t) val), ctx);

                    #elif BOXED_TERMS_REQUIRED_FOR_INT64 == 1
                        TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                        RAISE_ERROR(OVERFLOW_ATOM);

                    #else
                        #error "Unsupported configuration."
                    #endif

                } else {
                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int(-val, ctx);
                }
            }

            #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
            case 2: {
                avm_int64_t val = term_unbox_int64(arg1);
                if (val >= 0) {
                    return arg1;
                }

                if (val == INT64_MIN) {
                    TRACE("overflow: arg1: %lx, arg2: %lx\n", arg1, arg2);
                    RAISE_ERROR(OVERFLOW_ATOM);

                } else {
                    if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    return term_make_boxed_int64(-val, ctx);
                }
            }
            #endif
            default:
                RAISE_ERROR(OVERFLOW_ATOM);
        }
    } else {
        TRACE("error: arg1: %lx\n", arg1);
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
                return make_minus_min_not_boxed_int(ctx);
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
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }
    if (term_is_boxed_integer(arg2)) {
        size |= term_boxed_size(arg2);
    } else if (UNLIKELY(!term_is_integer(arg2))) {
        TRACE("error: arg1: %lx, arg2: %lx\n", arg1, arg2);
        RAISE_ERROR(BADARITH_ATOM);
    }

    switch (size) {
        case 0: {
            //BUG
            abort();
        }

        case 1: {
            avm_int_t val1 = term_maybe_unbox_int(arg1);
            avm_int_t val2 = term_maybe_unbox_int(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR(BADARITH_ATOM);
            }

            return term_make_maybe_boxed_int(ctx, val1 % val2);
        }

        #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        case 2:
        case 3: {
            avm_int64_t val1 = term_maybe_unbox_int64(arg1);
            avm_int64_t val2 = term_maybe_unbox_int64(arg2);
            if (UNLIKELY(val2 == 0)) {
                RAISE_ERROR(BADARITH_ATOM);
            }

            return term_make_maybe_boxed_int64(ctx, val1 % val2);
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

term bif_erlang_bor_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 | arg2;

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_band_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return arg1 & arg2;

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_bxor_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return (arg1 ^ arg2) | TERM_INTEGER_TAG;

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_bsl_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) << term_to_int32(arg2));

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
}

term bif_erlang_bsr_2(Context *ctx, int live, term arg1, term arg2)
{
    UNUSED(live);

    if (LIKELY(term_is_integer(arg1) && term_is_integer(arg2))) {
        return term_from_int32(term_to_int32(arg1) >> term_to_int32(arg2));

    } else {
        RAISE_ERROR(BADARITH_ATOM);
    }
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
    UNUSED(ctx);

    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 == 5
    if (arg1 == arg2) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation
    //it should compare any kind of type, and 5.0 != 5 is false
    if (arg1 != arg2) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_exactly_equal_to_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (arg1 == arg2) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_exactly_not_equal_to_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (arg1 != arg2) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}


term bif_erlang_greater_than_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (UNLIKELY(!(term_is_integer(arg1) && term_is_integer(arg2)))) {
        abort();
    }

    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) > term_to_int32(arg2)) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_less_than_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (UNLIKELY(!(term_is_integer(arg1) && term_is_integer(arg2)))) {
        abort();
    }

    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) < term_to_int32(arg2)) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_less_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (UNLIKELY(!(term_is_integer(arg1) && term_is_integer(arg2)))) {
        abort();
    }

    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) <= term_to_int32(arg2)) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}

term bif_erlang_greater_than_or_equal_2(Context *ctx, term arg1, term arg2)
{
    UNUSED(ctx);

    //TODO: fix this implementation, it needs to cover more types
    if (UNLIKELY(!(term_is_integer(arg1) && term_is_integer(arg2)))) {
        abort();
    }

    //TODO: fix this implementation, it needs to cover more types
    if (term_to_int32(arg1) >= term_to_int32(arg2)) {
        return TRUE_ATOM;
    } else {
        return FALSE_ATOM;
    }
}
