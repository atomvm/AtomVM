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

/**
 * @file utils.h
 * @brief Misc functions and macros.
 *
 * @details Miscellaneous functions and macros useful for different tasks, like endian byteswap, unaligned reads, marking unused vars, etc...
 */

#ifndef _UTILS_H_
#define _UTILS_H_

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    #ifdef __GNUC__
        #define READ_32_ALIGNED(ptr) \
            __builtin_bswap32(*((uint32_t *) (ptr)))
    #else
        #define READ_32_ALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )
    #endif

    #if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
        #define READ_64_UNALIGNED(ptr) \
            __builtin_bswap64(*((uint64_t *) (ptr)))

        #define WRITE_64_UNALIGNED(ptr, val) \
            *((uint64_t *) (ptr)) = __builtin_bswap64(val)

        #define READ_32_UNALIGNED(ptr) \
            __builtin_bswap32(*((uint32_t *) (ptr)))

        #define WRITE_32_UNALIGNED(ptr, val) \
            *((uint32_t *) (ptr)) = __builtin_bswap32(val)

        #define READ_16_UNALIGNED(ptr) \
            __builtin_bswap16(*((uint16_t *) (ptr)))

        #define WRITE_16_UNALIGNED(ptr, val) \
            *((uint16_t *) (ptr)) = __builtin_bswap16(val)

    #else
        #define READ_64_UNALIGNED(ptr) \
            ( (((uint64_t) ((uint8_t *)(ptr))[0]) << 56) | (((uint64_t) ((uint8_t *) (ptr))[1]) << 48) | \
              (((uint64_t) ((uint8_t *)(ptr))[2]) << 40) | (((uint64_t) ((uint8_t *) (ptr))[3]) << 32) | \
              (((uint64_t) ((uint8_t *)(ptr))[4]) << 24) | (((uint64_t) ((uint8_t *) (ptr))[5]) << 16) | \
              (((uint64_t) ((uint8_t *)(ptr))[6]) << 8) | (((uint64_t) ((uint8_t *) (ptr))[7])) )

        #define WRITE_64_UNALIGNED(ptr, val) \
            { \
                ((uint8_t *)(ptr))[0] = (((uint64_t) val) >> 56) & 0xff; \
                ((uint8_t *)(ptr))[1] = (((uint64_t) val) >> 48) & 0xff; \
                ((uint8_t *)(ptr))[2] = (((uint64_t) val) >> 40) & 0xff; \
                ((uint8_t *)(ptr))[3] = (((uint64_t) val) >> 32) & 0xff; \
                ((uint8_t *)(ptr))[4] = (((uint64_t) val) >> 24) & 0xff; \
                ((uint8_t *)(ptr))[5] = (((uint64_t) val) >> 16) & 0xff; \
                ((uint8_t *)(ptr))[6] = (((uint64_t) val) >> 8) & 0xff; \
                ((uint8_t *)(ptr))[7] = ((uint64_t) val) & 0xff; \
            }

        #define READ_32_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

        #define WRITE_32_UNALIGNED(ptr, val) \
            { \
                ((uint8_t *)(ptr))[0] = (((uint32_t) val) >> 24) & 0xff; \
                ((uint8_t *)(ptr))[1] = (((uint32_t) val) >> 16) & 0xff; \
                ((uint8_t *)(ptr))[2] = (((uint32_t) val) >> 8) & 0xff; \
                ((uint8_t *)(ptr))[3] = ((uint32_t) val) & 0xff; \
            }

        #define READ_16_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

        #define WRITE_16_UNALIGNED(ptr, val) \
            { \
                ((uint8_t *)(ptr))[0] = (((uint16_t) val) >> 8) & 0xff; \
                ((uint8_t *)(ptr))[1] = ((uint16_t) val) & 0xff; \
            }
    #endif

    #ifdef __GNUC__
        #define ENDIAN_SWAP_32(value) __builtin_bswap32(value)
    #else
        #define ENDIAN_SWAP_32(value) ((((value) & 0xFF) << 24) | (((value) & 0xFF00) << 8) | (((value) & 0xFF0000) >> 8) | (((value) & 0xFF000000) >> 24))
    #endif

#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__

    #define READ_64_UNALIGNED(ptr) \
        ( (((uint64_t) ((uint8_t *)(ptr))[0]) << 56) | (((uint64_t) ((uint8_t *) (ptr))[1]) << 48) | \
            (((uint64_t) ((uint8_t *)(ptr))[2]) << 40) | (((uint64_t) ((uint8_t *) (ptr))[3]) << 32) | \
            (((uint64_t) ((uint8_t *)(ptr))[4]) << 24) | (((uint64_t) ((uint8_t *) (ptr))[5]) << 16) | \
            (((uint64_t) ((uint8_t *)(ptr))[6]) << 8) | (((uint64_t) ((uint8_t *) (ptr))[7])) )

    #define WRITE_64_UNALIGNED(ptr, val) \
        { \
            ((uint8_t *)(ptr))[0] = (((uint64_t) val) >> 56) & 0xff; \
            ((uint8_t *)(ptr))[1] = (((uint64_t) val) >> 48) & 0xff; \
            ((uint8_t *)(ptr))[2] = (((uint64_t) val) >> 40) & 0xff; \
            ((uint8_t *)(ptr))[3] = (((uint64_t) val) >> 32) & 0xff; \
            ((uint8_t *)(ptr))[4] = (((uint64_t) val) >> 24) & 0xff; \
            ((uint8_t *)(ptr))[5] = (((uint64_t) val) >> 16) & 0xff; \
            ((uint8_t *)(ptr))[6] = (((uint64_t) val) >> 8) & 0xff; \
            ((uint8_t *)(ptr))[7] = ((uint64_t) val) & 0xff; \
        }

    #define READ_32_ALIGNED(ptr) \
        (*((uint32_t *) (ptr)))

    #define READ_32_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

    #define WRITE_32_UNALIGNED(ptr, val) \
        { \
            ((uint8_t *)(ptr))[0] = (((uint32_t) val) >> 24) & 0xff; \
            ((uint8_t *)(ptr))[1] = (((uint32_t) val) >> 16) & 0xff; \
            ((uint8_t *)(ptr))[2] = (((uint32_t) val) >> 8) & 0xff; \
            ((uint8_t *)(ptr))[3] = ((uint32_t) val) & 0xff; \
        }

    #define READ_16_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

    #define WRITE_16_UNALIGNED(ptr, val) \
        { \
            ((uint8_t *)(ptr))[0] = (((uint16_t) val) >> 8) & 0xff; \
            ((uint8_t *)(ptr))[1] = ((uint16_t) val) & 0xff; \
        }

    #define ENDIAN_SWAP_32(value) (value)

#else
    #error "Unsupported __BYTE_ORDER__ value."
#endif

#define UNUSED(x) (void) (x);


#ifdef __GNUC__
    #define IS_NULL_PTR(x) __builtin_expect((x) == NULL, 0)
    #define LIKELY(x) __builtin_expect(!!(x), 1)
    #define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
    #define IS_NULL_PTR(x) ((x) == NULL)
    #define LIKELY(x) (x)
    #define UNLIKELY(x) (x)
#endif

#ifdef __GNUC__
    #define HOT_FUNC __attribute__ ((hot))
    #define COLD_FUNC __attribute__ ((cold))
#else
    #define HOT_FUNC
    #define COLD_FUNC
#endif

#ifdef __GNUC__
    #define MALLOC_LIKE __attribute__((malloc))
#else
    #define MALLOC_LIKE
#endif

#ifdef __GNUC__
    #define MUST_CHECK __attribute__((warn_unused_result))
#else
    #define MUST_CHECK
#endif

#ifdef ALLOC_RANDOM_FAILURE

#ifndef RAND_MODULO
#define RAND_MODULO 31
#endif

static inline void *rand_fail_malloc(unsigned long malloc_size)
{
    return ((rand() % RAND_MODULO) == 0) ? 0L : malloc(malloc_size);
}

static inline void *rand_fail_calloc(int n, unsigned long alloc_size)
{
    return ((rand() % RAND_MODULO) == 0) ? 0L : calloc(n, alloc_size);
}

#define malloc(x) rand_fail_malloc(x)
#define calloc(x, y) rand_fail_calloc(x, y)

#endif

#ifdef AVM_VERBOSE_ABORT
#define AVM_ABORT()                                                           \
    {                                                                         \
        fprintf(stderr, "Abort in file %s at line %i\n", __FILE__, __LINE__); \
        abort();                                                              \
    }
#else
#define AVM_ABORT() abort()
#endif

/*
 * The following are workarounds for disabling following GCC pedantic warnings:
 * - "warning: ISO C forbids conversion of function pointer to object pointer type"
 * - "warning: ISO C forbids conversion of object pointer to function pointer type"
 * It also makes use of _Static_assert to actually check if it is safe or not.
 */

typedef void (*func_ptr_t)(void);

#ifdef __GNUC__

static inline __attribute__((always_inline)) void *cast_func_to_void_ptr(func_ptr_t func)
{
#if __STDC_VERSION__ >= 201112L
    _Static_assert(sizeof(void *) >= sizeof(func_ptr_t), "function ptr cannot be casted to void *");
#endif

/*
    The following workaround is way more standard, but overcomplicated:

    union {
        func_ptr_t fp;
        void *vp;
    } u;

    u.fp = func;

    return u.vp;
*/

// Let's rather play with GCC diagnostic
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    return (void *) func;
#pragma GCC diagnostic pop
}

/**
 * Cast any function pointer to \c void *
 */
#define CAST_FUNC_TO_VOID_PTR(f) cast_func_to_void_ptr((func_ptr_t) (f))

// else ifdef __GNUC__
#else

#define CAST_FUNC_TO_VOID_PTR(f) ((void *) (f))

#endif

#ifdef __GNUC__

static inline __attribute__((always_inline)) func_ptr_t cast_void_to_func_ptr(void *ptr)
{
#if __STDC_VERSION__ >= 201112L
    _Static_assert(sizeof(func_ptr_t) >= sizeof(void *), "void * cannot be casted to function ptr");
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
    return (func_ptr_t) ptr;
#pragma GCC diagnostic pop
}

/**
 * Cast any function pointer to \c void *
 */
#define CAST_VOID_TO_FUNC_PTR(f) cast_void_to_func_ptr((void *) (f))

// else ifdef __GNUC__
#else

#define CAST_VOID_TO_FUNC_PTR(f) ((func_ptr_t) (f))

#endif

/*
 * CONTAINER_OF is useful for obtaining the outer struct.
 * It makes use of offsetof() from stddef.h.
 */

#define CONTAINER_OF(ptr, type, member) \
    ((type *) (((char *) (ptr)) - offsetof(type, member)))

#ifdef __GNUC__
    #define PRINTF_FORMAT_ARGS(str_pos, arg_pos) \
        __attribute__ ((format (printf, str_pos, arg_pos)))
#else
    #define PRINTF_FORMAT_ARGS(...)
#endif

#ifdef __GNUC__
    #define NO_DISCARD \
        __attribute__ ((warn_unused_result))
#else
    #define NO_DISCARD(...)
#endif

#ifdef __GNUC__
    #define UNREACHABLE() \
        __builtin_unreachable()
#else
    #define UNREACHABLE(...)
#endif

#if defined(__GNUC__) && !defined(__clang__)
#if __GNUC__ >= 13
#define HAVE_ASSUME 1
#define ASSUME(x) __attribute__((assume((x))))
#endif
#endif

#ifndef HAVE_ASSUME
#if defined __has_builtin
#if __has_builtin(__builtin_assume)
#define HAVE_ASSUME 1
#define ASSUME(x) __builtin_assume((x))
#endif
#endif
#endif

#ifndef ASSUME
#define ASSUME(...)
#endif

static inline int32_t int32_neg_unsigned(uint32_t u32)
{
    return (UINT32_C(0) - u32);
}

static inline int64_t int64_neg_unsigned(uint64_t u64)
{
    return (UINT64_C(0) - u64);
}

static inline int32_t int32_cond_neg_unsigned(bool negative, uint32_t u32)
{
    return negative ? int32_neg_unsigned(u32) : (int32_t) u32;
}

static inline int64_t int64_cond_neg_unsigned(bool negative, uint64_t u64)
{
    return negative ? int64_neg_unsigned(u64) : (int64_t) u64;
}

static inline bool uint32_does_overflow_int32(uint32_t u32, bool is_negative)
{
    return ((is_negative && (u32 > ((uint32_t) INT32_MAX) + 1))
        || (!is_negative && (u32 > ((uint32_t) INT32_MAX))));
}

static inline bool uint64_does_overflow_int64(uint64_t u64, bool is_negative)
{
    return ((is_negative && (u64 > ((uint64_t) INT64_MAX) + 1))
        || (!is_negative && (u64 > ((uint64_t) INT64_MAX))));
}

static inline uint32_t int32_safe_unsigned_abs(int32_t i32)
{
    return (i32 < 0) ? ((uint32_t) - (i32 + 1)) + 1 : (uint32_t) i32;
}

static inline uint64_t int64_safe_unsigned_abs(int64_t i64)
{
    return (i64 < 0) ? ((uint64_t) - (i64 + 1)) + 1 : (uint64_t) i64;
}

static inline bool int32_is_negative(int32_t i32)
{
    return ((uint32_t) i32) >> 31;
}

static inline bool int64_is_negative(int64_t i64)
{
    return ((uint64_t) i64) >> 63;
}

static inline uint32_t int32_safe_unsigned_abs_set_flag(int32_t i32, bool *is_negative)
{
    *is_negative = int32_is_negative(i32);
    return int32_safe_unsigned_abs(i32);
}

static inline uint64_t int64_safe_unsigned_abs_set_flag(int64_t i64, bool *is_negative)
{
    *is_negative = int64_is_negative(i64);
    return int64_safe_unsigned_abs(i64);
}

/**
 * @brief Check if 64-bit integer value fits within int32_t range
 *
 * Tests whether a given int64_t value can be safely represented as
 * an int32_t without overflow or truncation.
 *
 * @param value The 64-bit integer value to check
 * @return true if value is within [INT32_MIN, INT32_MAX], false otherwise
 */
static inline bool int64_is_int32(int64_t value)
{
    return ((value >= (int64_t) INT32_MIN) && (value <= (int64_t) INT32_MAX));
}

#if INTPTR_MAX <= INT32_MAX
#define INTPTR_WRITE_TO_ASCII_BUF_LEN (32 + 1)
#elif INTPTR_MAX <= INT64_MAX
#define INTPTR_WRITE_TO_ASCII_BUF_LEN (64 + 1)
#endif

#define INT32_WRITE_TO_ASCII_BUF_LEN (32 + 1)
#define INT64_WRITE_TO_ASCII_BUF_LEN (64 + 1)

size_t intptr_write_to_ascii_buf(intptr_t n, unsigned int base, char *out_end);

#if INTPTR_MAX >= INT32_MAX
static inline size_t int32_write_to_ascii_buf(int32_t n, unsigned int base, char *out_end)
{
    return intptr_write_to_ascii_buf(n, base, out_end);
}
#endif

#if INT64_MAX > INTPTR_MAX
size_t int64_write_to_ascii_buf(int64_t n, unsigned int base, char *out_end);
#else
static inline size_t int64_write_to_ascii_buf(int64_t n, unsigned int base, char *out_end)
{
    return intptr_write_to_ascii_buf(n, base, out_end);
}
#endif

typedef enum
{
    BufToInt64NoOptions,
    BufToInt64RejectSign
} buf_to_int64_options_t;

int int64_parse_ascii_buf(const char buf[], size_t buf_len, unsigned int base,
    buf_to_int64_options_t options, int64_t *out);

#ifdef __cplusplus
}
#endif

#endif
