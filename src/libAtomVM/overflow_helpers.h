/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#ifndef _OVERFLOW_HELPERS_H_
#define _OVERFLOW_HELPERS_H_

#include "term.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __GNUC__
#if __GNUC__ >= 5
#define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
#define BUILTIN_SUB_OVERFLOW __builtin_sub_overflow
#define BUILTIN_MUL_OVERFLOW __builtin_mul_overflow

#define BUILTIN_ADD_OVERFLOW_INT __builtin_add_overflow
#define BUILTIN_SUB_OVERFLOW_INT __builtin_sub_overflow
#define BUILTIN_MUL_OVERFLOW_INT __builtin_mul_overflow

#define BUILTIN_ADD_OVERFLOW_INT64 __builtin_add_overflow
#define BUILTIN_SUB_OVERFLOW_INT64 __builtin_sub_overflow
#define BUILTIN_MUL_OVERFLOW_INT64 __builtin_mul_overflow
#endif
#endif

#ifdef __has_builtin
#if __has_builtin(__builtin_add_overflow)
#define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
#define BUILTIN_ADD_OVERFLOW_INT __builtin_add_overflow
#define BUILTIN_ADD_OVERFLOW_INT64 __builtin_add_overflow
#endif
#if __has_builtin(__builtin_sub_overflow)
#define BUILTIN_SUB_OVERFLOW __builtin_sub_overflow
#define BUILTIN_SUB_OVERFLOW_INT __builtin_sub_overflow
#define BUILTIN_SUB_OVERFLOW_INT64 __builtin_sub_overflow
#endif
#if __has_builtin(__builtin_mul_overflow)
#define BUILTIN_MUL_OVERFLOW __builtin_mul_overflow
#define BUILTIN_MUL_OVERFLOW_INT __builtin_mul_overflow
#define BUILTIN_MUL_OVERFLOW_INT64 __builtin_mul_overflow
#endif
#endif

#ifndef BUILTIN_ADD_OVERFLOW
#define BUILTIN_ADD_OVERFLOW atomvm_add_overflow
#define BUILTIN_ADD_OVERFLOW_INT atomvm_add_overflow_int
#define BUILTIN_ADD_OVERFLOW_INT64 atomvm_add_overflow_int64

static inline int atomvm_add_overflow(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    // a and b are shifted integers
    avm_int_t sum = (a >> 4) + (b >> 4);
    *res = sum << 4;
    return ((sum > MAX_NOT_BOXED_INT) || (sum < MIN_NOT_BOXED_INT));
}

static inline int atomvm_add_overflow_int(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    avm_int64_t sum = (avm_int64_t) a + (avm_int64_t) b;
    *res = sum;
    return ((sum < AVM_INT_MIN) || (sum > AVM_INT_MAX));
}

static inline int atomvm_add_overflow_int64(avm_int64_t a, avm_int64_t b, avm_int64_t *res)
{
    *res = a + b;
    return 0;
}
#endif

#ifndef BUILTIN_SUB_OVERFLOW
#define BUILTIN_SUB_OVERFLOW atomvm_sub_overflow
#define BUILTIN_SUB_OVERFLOW_INT atomvm_sub_overflow_int
#define BUILTIN_SUB_OVERFLOW_INT64 atomvm_sub_overflow_int64

#include <stdint.h>

static inline int atomvm_sub_overflow(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    // a and b are shifted integers
    avm_int_t diff = (a >> 4) - (b >> 4);
    *res = diff << 4;
    return ((diff > MAX_NOT_BOXED_INT) || (diff < MIN_NOT_BOXED_INT));
}

static inline int atomvm_sub_overflow_int(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    avm_int64_t diff = (avm_int64_t) a - (avm_int64_t) b;
    *res = diff;
    return ((diff > AVM_INT_MAX) || (diff < AVM_INT_MIN));
}

static inline int atomvm_sub_overflow_int64(avm_int64_t a, avm_int64_t b, avm_int64_t *res)
{
    avm_int64_t diff = a - b;
    *res = diff;
    return 0;
}
#endif

#ifndef BUILTIN_MUL_OVERFLOW
#define BUILTIN_MUL_OVERFLOW atomvm_mul_overflow
#define BUILTIN_MUL_OVERFLOW_INT atomvm_mul_overflow_int
#define BUILTIN_MUL_OVERFLOW_INT64 atomvm_mul_overflow_int64

static inline int atomvm_mul_overflow_int(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    avm_int64_t mul = (avm_int64_t) a * (avm_int64_t) b;
    *res = mul;
    return ((mul < AVM_INT_MIN) || (mul > AVM_INT_MAX));
}

static inline int atomvm_mul_overflow_int64(avm_int64_t a, avm_int64_t b, avm_int64_t *res)
{
    if ((a == 0) || (b == 0)) {
        *res = 0;
        return 0;

    } else {
        avm_int64_t mul_res = a * b;
        *res = mul_res;
        return a != mul_res / b;
    }
}

static inline int atomvm_mul_overflow(avm_int_t a, avm_int_t b, avm_int_t *res)
{
#if AVM_INT_MAX < INT64_MAX
    avm_int64_t mul = (avm_int64_t) (a >> 2) * (avm_int64_t) (b >> 2);
    *res = mul << 4;
    return ((mul > MAX_NOT_BOXED_INT) || (mul < MIN_NOT_BOXED_INT));
#elif AVM_INT_MAX == INT64_MAX
    int64_t mul;
    int ovf = atomvm_mul_overflow_int64(a >> 2, b >> 2, &mul);
    *res = mul << 4;
    return ovf || ((mul > MAX_NOT_BOXED_INT) || (mul < MIN_NOT_BOXED_INT));
#else
#error "Unsupported AVM_INT_MAX size"
#endif
}
#endif

#ifdef __cplusplus
}
#endif

#endif
