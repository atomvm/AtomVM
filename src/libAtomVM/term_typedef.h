/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

/**
 * @file term_typedef.h
 * @brief term type definition
 *
 * @details This header defines term time and few related macros.
 */

#ifndef _TERM_TYPEDEF_H_
#define _TERM_TYPEDEF_H_

#ifdef __cplusplus
#include <climits>
#else
#include <assert.h>
#include <limits.h>
#endif
#include <inttypes.h>
#include <stdint.h>

/**
 * A value of any data type, types bigger than a machine word will require some additional space on heap.
 */
typedef uintptr_t term;

#define TERM_U_FMT PRIuPTR
#define TERM_X_FMT PRIXPTR

#if ( (UINT32_MAX != 4294967295ULL) || (UINT64_MAX != 18446744073709551615ULL) \
    || (INT32_MAX != 2147483647LL) || (INT64_MAX != 9223372036854775807LL))
    #error "limits.h or preprocessor is not sane."
#endif

#if UINTPTR_MAX == UINT32_MAX
    #define TERM_BITS 32
    #define TERM_BYTES 4

#elif UINTPTR_MAX == UINT64_MAX
    #define TERM_BITS 64
    #define TERM_BYTES 8

#else
    #error "Term size must be either 32 bit or 64 bit."
#endif

typedef intptr_t avm_int_t;
typedef uintptr_t avm_uint_t;

typedef int64_t avm_int64_t;
typedef uint64_t avm_uint64_t;

#if UINTPTR_MAX == UINT32_MAX
    #define AVM_INT_MIN INT32_MIN
    #define AVM_INT_MAX INT32_MAX
    #define INT64_IS_ALWAYS_BOXED 1
    #define BOXED_TERMS_REQUIRED_FOR_INT 1
    #define BOXED_TERMS_REQUIRED_FOR_INT64 2

#elif UINTPTR_MAX == UINT64_MAX
    #define AVM_INT_MIN INT64_MIN
    #define AVM_INT_MAX INT64_MAX
    #define INT64_IS_ALWAYS_BOXED 0
    #define BOXED_TERMS_REQUIRED_FOR_INT 1
    #define BOXED_TERMS_REQUIRED_FOR_INT64 1

#else
    #error "term size must be either 32 bit or 64 bit."
#endif

#define UNICODE_CHAR_MAX 0x10FFFF

#define MIN_NOT_BOXED_INT (AVM_INT_MIN >> 4)
#define MAX_NOT_BOXED_INT (AVM_INT_MAX >> 4)

#if AVM_INT_MAX == INT_MAX
    #define AVM_INT_FMT "%i"
#elif AVM_INT_MAX == LONG_MAX
    #define AVM_INT_FMT "%li"
#elif AVM_INT_MAX == LLONG_INT_MAX
    #define AVM_INT_FMT "%lli"
#else
    #error "cannot define AVM_INT_MAX: invalid build env."
#endif

#if INT64_MAX == INT_MAX
    #define AVM_INT64_FMT "%i"
#elif INT64_MAX == LONG_MAX
    #if defined(__clang__) && defined(__APPLE__)
        #define AVM_INT64_FMT "%lli"
    #else
        #define AVM_INT64_FMT "%li"
    #endif
#elif INT64_MAX == LLONG_MAX
    #define AVM_INT64_FMT "%lli"
#else
    #error "cannot define AVM_INT64_FMT: invalid build env."
#endif

// %f and %lf are the same since C99 when using printf
// this is not true for scanf.
#ifdef AVM_USE_SINGLE_PRECISION
    typedef float avm_float_t;
    #define AVM_FLOAT_FMT "%f"

#ifndef __cplusplus
    _Static_assert(sizeof(avm_float_t) == 4, "avm_float_t must be a 32-bit float");
#endif

    #define INT64_MIN_AS_AVM_FLOAT -9223372036854775808.0
    #define INT64_MAX_AS_AVM_FLOAT 9223372036854775808.0
#else
    typedef double avm_float_t;
    #define AVM_FLOAT_FMT "%lf"

#ifndef __cplusplus
    _Static_assert(sizeof(avm_float_t) == 8, "avm_float_t must be a 64-bit float");
#endif

    #define INT64_MIN_AS_AVM_FLOAT -9223372036854775808.0
    #define INT64_MAX_AS_AVM_FLOAT 9223372036854775808.0
#endif

typedef union {
    term t;
    avm_float_t f;
} float_term_t;

#endif
