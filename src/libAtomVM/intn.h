/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Davide Bettio <davide@uninstall.it>
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

#ifndef _INTN_H_
#define _INTN_H_

#include <stdbool.h>
#include <string.h>

#include "utils.h"

// INTN_MAX_RES_LEN is bigger than INTN_MAX_IN_LEN, even the most trivial
// INTN_MUL_OUT_LEN(8, 1) = 9.
//
// Also since we may use INTN_INT64_LEN digits even for small values such as `1` (it will be padded
// with zeros, actually INTN_INT64_LEN + 1 digits, for some reason specific to how AtomVM handles
// boxed values).
//
// Example: { ... 8 digits ... } * { 0x1, 0x0, 0x0}, that will require INTN_MUL_OUT_LEN(8, 3) = 11
// digits.
//
// Also we need some room for any potential overflow, worst case is still INTN_MUL_OUT_LEN(8, 3).
#define INTN_INT64_LEN 2
#define INTN_MAX_IN_LEN 8 // 256 bit / 32 bit = 8 digits
#define INTN_MAX_RES_LEN (INTN_MAX_IN_LEN + INTN_INT64_LEN + 1)

#define INTN_NEG_OUT_LEN(m) ((m) + 1)
#define INTN_MUL_OUT_LEN(m, n) ((m) + (n))
#define INTN_DIV_OUT_LEN(m, n) ((m) - (n) + 1 + 1)
#define INTN_ABS_OUT_LEN(m) ((m) + 1)

typedef enum
{
    IntNPositiveInteger = 0,
    IntNNegativeInteger = 4
} intn_integer_sign_t;

typedef uint32_t intn_digit_t;

size_t intn_addmnu(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[]);

void intn_mulmnu(
    const intn_digit_t u[], size_t m, const intn_digit_t v[], size_t n, intn_digit_t w[]);
void intn_mul_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign);

void print_num(const uint32_t num[], int len);

size_t intn_count_digits(const intn_digit_t *num, size_t num_len);

char *intn_to_string(const intn_digit_t *num, size_t len, intn_integer_sign_t num_sign, int base,
    size_t *string_len);
int intn_parse(
    const char buf[], size_t buf_len, int base, intn_digit_t *out, intn_integer_sign_t *out_sign);

double intn_to_double(const intn_digit_t *num, size_t len, intn_integer_sign_t sign);
int intn_from_double(double dnum, intn_digit_t *out, intn_integer_sign_t *out_sign);

static inline void intn_copy(
    const intn_digit_t *num, size_t num_len, intn_digit_t *out, size_t extend_to)
{
    memcpy(out, num, num_len * sizeof(intn_digit_t));
    memset(out + num_len, 0, (extend_to - num_len) * sizeof(intn_digit_t));
}

static inline void intn_u64_to_digits(uint64_t absu64, uint32_t out[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(out, &absu64, sizeof(absu64));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    const uint32_t *i32 = (const uint32_t *) &absu64;
    out[0] = i32[1];
    out[1] = i32[0];
#else
#error "Unsupported endianess"
#endif
}

static inline void int64_to_intn_2(int64_t i64, uint32_t out[], intn_integer_sign_t *out_sign)
{
    bool is_negative;
    uint64_t absu64 = int64_safe_unsigned_abs_set_flag(i64, &is_negative);
    *out_sign = is_negative ? IntNNegativeInteger : IntNPositiveInteger;
    intn_u64_to_digits(absu64, out);
}

static inline uint64_t intn_digits_to_u64(const intn_digit_t num[])
{
    uint64_t utmp;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&utmp, num, sizeof(uint64_t));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    utmp = (((uint64_t) num[1] << 32) | (uint64_t) num[0]);
#else
#error "Unsupported endianess"
#endif

    return utmp;
}

static inline int64_t intn_2_digits_to_int64(
    const intn_digit_t num[], size_t len, intn_integer_sign_t sign)
{
    switch (len) {
        case 0:
            return 0;
        case 1:
            return int32_cond_neg_unsigned(sign == IntNNegativeInteger, num[0]);
        case 2: {
            uint64_t utmp = intn_digits_to_u64(num);
            return int64_cond_neg_unsigned(sign == IntNNegativeInteger, utmp);
        }
        default:
            UNREACHABLE();
    }
}

static inline bool intn_fits_int64(const intn_digit_t num[], size_t len, intn_integer_sign_t sign)
{
    if (len < INTN_INT64_LEN) {
        return true;
    } else if (len == INTN_INT64_LEN) {
        uint64_t u64 = intn_digits_to_u64(num);
        return !uint64_does_overflow_int64(u64, sign == IntNNegativeInteger);
    }
    return false;
}

static inline intn_integer_sign_t intn_muldiv_sign(intn_integer_sign_t s1, intn_integer_sign_t s2)
{
    return (intn_integer_sign_t) ((unsigned int) s1 ^ (unsigned int) s2) & IntNNegativeInteger;
}

#endif
