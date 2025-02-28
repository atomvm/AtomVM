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

#include <string.h>

#include "utils.h"

#define INTN_INT64_LEN 2
#define INTN_MAX_IN_LEN 8 // 256 bit / 32 bit = 8 digits
#define INTN_MAX_RES_LEN (INTN_MAX_IN_LEN + INTN_INT64_LEN) // 1 digit for overflow

#define INTN_NEG_OUT_LEN(m) ((m) + 1)
#define INTN_MUL_OUT_LEN(m, n) ((m) + (n))
#define INTN_DIV_OUT_LEN(m, n) ((m) - (n) + 1 + 1)
#define INTN_ABS_OUT_LEN(m) ((m) + 1)

typedef uint32_t intn_digit_t;

size_t intn_addmns(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[]);

void intn_mulmns(
    const intn_digit_t u[], size_t m, const intn_digit_t v[], size_t n, intn_digit_t w[]);
void intn_mul_int64(int64_t num1, int64_t num2, intn_digit_t *out);

void intn_abs(const intn_digit_t num[], size_t num_len, intn_digit_t out[], size_t *out_len);
void intn_neg(const intn_digit_t num[], size_t num_len, intn_digit_t out[], size_t *out_len);
void intn_sign_extend(const intn_digit_t *num, size_t num_len, size_t extend_to, intn_digit_t *out);

void print_num(const uint32_t num[], int len);

size_t intn_count_digits(const intn_digit_t *num, size_t num_len);

char *intn_to_string(const intn_digit_t *num, size_t len, int base, size_t *string_len);
int intn_parse(const char buf[], size_t buf_len, int base, intn_digit_t *out);

static inline void int64_to_intn_2(int64_t i64, uint32_t out[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(out, &i64, sizeof(i64));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    const uint32_t *i32 = (const uint32_t *) &i64;
    out[0] = i32[1];
    out[1] = i32[0];
#else
#error "Unsupported endianess"
#endif
}

static inline int64_t intn_2_digits_to_int64(const intn_digit_t num[], size_t len)
{
    switch (len) {
        case 0:
            return 0;
        case 1:
            return (int32_t) num[0];
        case 2: {
            int64_t ret;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
            memcpy(&ret, num, sizeof(int64_t));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
            ret = (((uint64_t) num[1] << 32) | (uint64_t) num[0]);
#else
#error "Unsupported endianess"
#endif
            return ret;
        }
        default:
            UNREACHABLE();
    }
}

#endif
