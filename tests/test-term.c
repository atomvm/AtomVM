/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Davide Bettio <davide@uninstall.it>
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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "context.h"
#include "globalcontext.h"
#include "intn.h"
#include "memory.h"
#include "term.h"
#include "utils.h"

// Enough room for all terms created by a single test function, so that no GC
// runs between term creation and the checks
#define TEST_HEAP_SIZE 256

enum
{
    IsIntegerOn32Bit = 1 << 0,
    IsIntegerOn64Bit = 1 << 1,
    IsPositive = 1 << 2,
    IsNegative = 1 << 3,
    IsUint8 = 1 << 4,
    IsInt32 = 1 << 5,
    IsUint32 = 1 << 6,
    IsInt64 = 1 << 7,
    IsUint64 = 1 << 8,
    IsBigint = 1 << 9
};

static bool flags_is_immediate(unsigned int flags)
{
#if TERM_BITS == 32
    return (flags & IsIntegerOn32Bit) != 0;
#elif TERM_BITS == 64
    return (flags & IsIntegerOn64Bit) != 0;
#else
#error "Unsupported TERM_BITS"
#endif
}

static void assert_integer_predicates(term t, unsigned int flags)
{
    bool is_immediate = flags_is_immediate(flags);
    bool is_positive = (flags & IsPositive) != 0;
    bool is_negative = (flags & IsNegative) != 0;

    assert(term_is_any_integer(t));
    assert(term_is_number(t));
    assert(term_is_int(t) == is_immediate);
    assert(term_is_boxed_integer(t) == !is_immediate);

    assert(term_is_non_neg_int(t) == (is_immediate && !is_negative));
    assert(term_is_pos_int(t) == (is_immediate && is_positive));
    assert(term_is_neg_int(t) == (is_immediate && is_negative));

    assert(term_is_pos_boxed_integer(t) == (!is_immediate && !is_negative));
    assert(term_is_neg_boxed_integer(t) == (!is_immediate && is_negative));
    if (!is_immediate) {
        assert(term_boxed_integer_sign(t)
            == (is_negative ? TermNegativeInteger : TermPositiveInteger));
    }

    assert(term_is_any_non_neg_integer(t) == !is_negative);
    assert(term_is_any_pos_integer(t) == is_positive);
    assert(term_is_any_neg_integer(t) == is_negative);

    assert(term_is_bigint(t) == ((flags & IsBigint) != 0));
}

static term make_bigint_term(
    Context *ctx, const intn_digit_t *digits, size_t len, term_integer_sign_t sign)
{
    size_t count = intn_count_digits(digits, len);
    size_t intn_data_size;
    size_t rounded_num_len;
    term_bigint_size_requirements(count, &intn_data_size, &rounded_num_len);

    term t = term_create_uninitialized_bigint(intn_data_size, sign, &ctx->heap);
    term_initialize_bigint(t, digits, count, rounded_num_len);
    return t;
}

static void test_int64_range_checks(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    enum MemoryGCResult res = memory_ensure_free(ctx, TEST_HEAP_SIZE);
    assert(res == MEMORY_GC_OK);

    static const struct
    {
        int64_t value;
        unsigned int flags;
    } cases[] = {
        { 0,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsUint8 | IsInt32 | IsUint32 | IsInt64
                | IsUint64 },
        { 1,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsPositive | IsUint8 | IsInt32 | IsUint32
                | IsInt64 | IsUint64 },
        { -1, IsIntegerOn32Bit | IsIntegerOn64Bit | IsNegative | IsInt32 | IsInt64 },
        { 42,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsPositive | IsUint8 | IsInt32 | IsUint32
                | IsInt64 | IsUint64 },
        { -42, IsIntegerOn32Bit | IsIntegerOn64Bit | IsNegative | IsInt32 | IsInt64 },
        { 255,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsPositive | IsUint8 | IsInt32 | IsUint32
                | IsInt64 | IsUint64 },
        { 256,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsPositive | IsInt32 | IsUint32 | IsInt64
                | IsUint64 },
        // 2^27 - 1 and -2^27: immediate boundary on 32-bit builds
        { 0x07FFFFFF,
            IsIntegerOn32Bit | IsIntegerOn64Bit | IsPositive | IsInt32 | IsUint32 | IsInt64
                | IsUint64 },
        { 0x08000000, IsIntegerOn64Bit | IsPositive | IsInt32 | IsUint32 | IsInt64 | IsUint64 },
        { -0x08000000, IsIntegerOn32Bit | IsIntegerOn64Bit | IsNegative | IsInt32 | IsInt64 },
        { -0x08000001, IsIntegerOn64Bit | IsNegative | IsInt32 | IsInt64 },
        { INT32_MAX, IsIntegerOn64Bit | IsPositive | IsInt32 | IsUint32 | IsInt64 | IsUint64 },
        { INT32_MIN, IsIntegerOn64Bit | IsNegative | IsInt32 | IsInt64 },
        { (int64_t) INT32_MAX + 1, IsIntegerOn64Bit | IsPositive | IsUint32 | IsInt64 | IsUint64 },
        { (int64_t) INT32_MIN - 1, IsIntegerOn64Bit | IsNegative | IsInt64 },
        { UINT32_MAX, IsIntegerOn64Bit | IsPositive | IsUint32 | IsInt64 | IsUint64 },
        { (int64_t) UINT32_MAX + 1, IsIntegerOn64Bit | IsPositive | IsInt64 | IsUint64 },
        // 2^59 - 1 and -2^59: immediate boundary on 64-bit builds
        { 0x07FFFFFFFFFFFFFF, IsIntegerOn64Bit | IsPositive | IsInt64 | IsUint64 },
        { 0x0800000000000000, IsPositive | IsInt64 | IsUint64 },
        { -0x0800000000000000, IsIntegerOn64Bit | IsNegative | IsInt64 },
        { -0x0800000000000001, IsNegative | IsInt64 },
        { INT64_MAX, IsPositive | IsInt64 | IsUint64 },
        { INT64_MIN, IsNegative | IsInt64 },
    };

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        int64_t value = cases[i].value;
        unsigned int flags = cases[i].flags;
        term t = term_make_maybe_boxed_int64(value, &ctx->heap);

        assert_integer_predicates(t, flags);

        if (flags_is_immediate(flags)) {
            assert(term_is_integer(t) == true);
            assert(term_to_int(t) == (avm_int_t) value);
        } else {
            assert(term_is_integer(t) == false);
        }
        if (flags & IsUint8) {
            assert(term_is_uint8(t) == true);
            assert(term_to_uint8(t) == (uint8_t) value);
        } else {
            assert(term_is_uint8(t) == false);
        }
        if (flags & IsInt32) {
            assert(term_is_int32(t) == true);
            assert(term_to_int32(t) == (int32_t) value);
        } else {
            assert(term_is_int32(t) == false);
        }
        if (flags & IsUint32) {
            assert(term_is_uint32(t) == true);
            assert(term_to_uint32(t) == (uint32_t) value);
        } else {
            assert(term_is_uint32(t) == false);
        }
        if (flags & IsInt64) {
            assert(term_is_int64(t) == true);
            assert(term_to_int64(t) == value);
        } else {
            assert(term_is_int64(t) == false);
        }
        if (flags & IsUint64) {
            assert(term_is_uint64(t) == true);
            assert(term_to_uint64(t) == (uint64_t) value);
        } else {
            assert(term_is_uint64(t) == false);
        }
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

static void test_bigint_range_checks(void)
{
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    enum MemoryGCResult res = memory_ensure_free(ctx, TEST_HEAP_SIZE);
    assert(res == MEMORY_GC_OK);

    static const struct
    {
        intn_digit_t digits[INTN_MAX_IN_LEN];
        size_t len;
        unsigned int flags;
        uint64_t uint64_value;
    } cases[] = {
        // 2^63
        { { 0x00000000, 0x80000000 }, 2, IsPositive | IsBigint | IsUint64, UINT64_C(1) << 63 },
        // 2^64 - 1
        { { 0xFFFFFFFF, 0xFFFFFFFF }, 2, IsPositive | IsBigint | IsUint64, UINT64_MAX },
        // 2^64: low 64 bits are 0
        { { 0x00000000, 0x00000000, 0x00000001 }, 3, IsPositive | IsBigint, 0 },
        // 2^64 + 7: low 64 bits are 7
        { { 0x00000007, 0x00000000, 0x00000001 }, 3, IsPositive | IsBigint, 0 },
        // 2^65 + 2^40
        { { 0x00000000, 0x00000100, 0x00000002 }, 3, IsPositive | IsBigint, 0 },
        // -(2^63 + 1)
        { { 0x00000001, 0x80000000 }, 2, IsNegative | IsBigint, 0 },
        // -(2^64)
        { { 0x00000000, 0x00000000, 0x00000001 }, 3, IsNegative | IsBigint, 0 },
        // 2^256 - 1: largest supported magnitude
        { { 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
              0xFFFFFFFF },
            8, IsPositive | IsBigint, 0 },
    };

    for (size_t i = 0; i < sizeof(cases) / sizeof(cases[0]); i++) {
        unsigned int flags = cases[i].flags;
        term_integer_sign_t sign = (flags & IsNegative) ? TermNegativeInteger : TermPositiveInteger;
        term t = make_bigint_term(ctx, cases[i].digits, cases[i].len, sign);

        assert_integer_predicates(t, flags);

        assert(term_is_uint8(t) == false);
        assert(term_is_int32(t) == false);
        assert(term_is_uint32(t) == false);
        assert(term_is_int64(t) == false);
        if (flags & IsUint64) {
            assert(term_is_uint64(t) == true);
            assert(term_to_uint64(t) == cases[i].uint64_value);
        } else {
            assert(term_is_uint64(t) == false);
        }
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_int64_range_checks();
    test_bigint_range_checks();

    return EXIT_SUCCESS;
}
