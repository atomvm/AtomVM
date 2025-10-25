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

    #if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86) || defined(__aarch64__) || defined(_M_ARM64)
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

    #ifdef __GNUC__
        #define ENDIAN_SWAP_16(value) __builtin_bswap16(value)
    #else
        #define ENDIAN_SWAP_16(value) ((((value) & 0xFF) << 8) | (((value) & 0xFF00) >> 8))
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
    #define ENDIAN_SWAP_16(value) (value)

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

#define MAXI(A, B) ((A > B) ? (A) : (B))
#define MINI(A, B) ((A > B) ? (B) : (A))

/**
 * @brief Align size up to power-of-2 boundary
 *
 * Rounds up a size value to the next multiple of a power-of-2 alignment.
 * This function uses bit manipulation for efficient alignment calculation
 * and is faster than the general-purpose \c size_align_up().
 *
 * @param n Size value to align
 * @param align Power-of-2 alignment boundary
 * @return Size rounded up to next multiple of align
 *
 * @pre align must be a power of 2 (e.g., 2, 4, 8, 16, 32, ...)
 * @warning Undefined behavior if align is not a power of 2
 * @warning Undefined behavior if align is 0
 *
 * @note Result is always >= n
 *
 * @code
 * size_t aligned = size_align_up_pow2(17, 8);  // Returns 24
 * size_t aligned = size_align_up_pow2(16, 8);  // Returns 16 (already aligned)
 * @endcode
 *
 * @see size_align_up() for arbitrary alignment values
 */
static inline size_t size_align_up_pow2(size_t n, size_t align)
{
    return (n + (align - 1)) & ~(align - 1);
}

/**
 * @brief Align size up to arbitrary boundary
 *
 * Rounds up a size value to the next multiple of an alignment boundary.
 * Works with any alignment value, not just powers of 2.
 *
 * @param n Size value to align
 * @param align Alignment boundary (any positive value, or 0)
 * @return Size rounded up to next multiple of align, or n if align is 0
 *
 * @note Returns n unchanged if align is 0 (no alignment)
 * @note Result is always >= n
 * @note For power-of-2 alignments, \c size_align_up_pow2() is more efficient
 *
 * @code
 * size_t aligned = size_align_up(17, 10);  // Returns 20
 * size_t aligned = size_align_up(20, 10);  // Returns 20 (already aligned)
 * size_t aligned = size_align_up(17, 0);   // Returns 17 (no alignment)
 * @endcode
 *
 * @see size_align_up_pow2() for optimized power-of-2 alignment
 * @see size_align_down() for rounding down instead of up
 */
static inline size_t size_align_up(size_t n, size_t align)
{
    if (align == 0) {
        return n;
    }
    return ((n + align - 1) / align) * align;
}

/**
 * @brief Align size down to arbitrary boundary
 *
 * Rounds down a size value to the previous multiple of an alignment boundary.
 * Works with any alignment value, not just powers of 2.
 *
 * @param n Size value to align
 * @param align Alignment boundary (any positive value, or 0)
 * @return Size rounded down to previous multiple of align, or n if align is 0
 *
 * @note Returns n unchanged if align is 0 (no alignment)
 * @note Result is always <= n
 * @note Commonly used for finding aligned base addresses within buffers
 *
 * @code
 * size_t aligned = size_align_down(17, 10);  // Returns 10
 * size_t aligned = size_align_down(20, 10);  // Returns 20 (already aligned)
 * size_t aligned = size_align_down(7, 10);   // Returns 0
 * @endcode
 *
 * @see size_align_up() for rounding up instead of down
 */
static inline size_t size_align_down(size_t n, size_t align)
{
    if (align == 0) {
        return n;
    }
    return (n / align) * align;
}

/**
 * @brief Negate unsigned 32-bit value (\c uint32_t) to signed integer (\c int32_t)
 *
 * Converts an unsigned 32-bit value to its negated signed representation.
 * This function performs the negation operation while avoiding undefined
 * behavior that would occur with direct cast and negation due to two's
 * complement asymmetry.
 *
 * @param u32 Unsigned 32-bit value to negate
 * @return Negated value as signed 32-bit integer (\c int32_t)
 *
 * @note Handles \c INT32_MIN case correctly (u32 = 2147483648 returns -2147483648)
 * @note Useful for parsing negative integers from text where magnitude is
 *       parsed as unsigned to avoid overflow
 * @warning Values greater than 2147483648 have undefined results
 */
static inline int32_t int32_neg_unsigned(uint32_t u32)
{
    return (UINT32_C(0) - u32);
}

/**
 * @brief Negate unsigned 64-bit value (\c uint64_t) to signed integer (\c int64_t)
 *
 * Converts an unsigned 64-bit value to its negated signed representation.
 * This function performs the negation operation while avoiding undefined
 * behavior that would occur with direct cast and negation due to two's
 * complement asymmetry.
 *
 * @param u64 Unsigned 64-bit value to negate
 * @return Negated value as signed 64-bit integer (\c int64_t)
 *
 * @note Handles \c INT64_MIN case correctly (u64 = 9223372036854775808 returns \c INT64_MIN)
 * @note Useful for parsing negative integers from text where magnitude is
 *       parsed as unsigned to avoid overflow
 * @warning Values greater than 9223372036854775808 have undefined results
 */
static inline int64_t int64_neg_unsigned(uint64_t u64)
{
    return (UINT64_C(0) - u64);
}

/**
 * @brief Conditionally negate unsigned 32-bit value (\c uint32_t) to signed integer (\c int32_t)
 *
 * Converts an unsigned 32-bit value to signed, optionally negating based
 * on a flag. This function safely handles the negation while avoiding
 * undefined behavior from two's complement asymmetry. Commonly used when
 * parsing integers where the sign is determined separately from the magnitude.
 *
 * @param negative If true, negate the value; if false, cast directly to signed
 * @param u32 Unsigned 32-bit magnitude
 * @return Signed 32-bit integer (\c int32_t), negated if negative flag is true
 *
 * @warning Caller must ensure the value fits in signed range using
 *          \c uint32_does_overflow_int32() before calling this function
 *
 * @see uint32_does_overflow_int32() to check for overflow before conversion
 * @see int32_neg_unsigned() for unconditional negation
 */
static inline int32_t int32_cond_neg_unsigned(bool negative, uint32_t u32)
{
    return negative ? int32_neg_unsigned(u32) : (int32_t) u32;
}

/**
 * @brief Conditionally negate unsigned 64-bit value (\c uint64_t) to signed integer (\c int64_t)
 *
 * Converts an unsigned 64-bit value to signed, optionally negating based
 * on a flag. This function safely handles the negation while avoiding
 * undefined behavior from two's complement asymmetry. Commonly used when
 * parsing integers where the sign is determined separately from the magnitude.
 *
 * @param negative If true, negate the value; if false, cast directly to signed
 * @param u64 Unsigned 64-bit magnitude
 * @return Signed 64-bit integer (\c int64_t), negated if negative flag is true
 *
 * @warning Caller must ensure the value fits in signed range using
 *          \c uint64_does_overflow_int64() before calling this function
 *
 * @see uint64_does_overflow_int64() to check for overflow before conversion
 * @see int64_neg_unsigned() for unconditional negation
 */
static inline int64_t int64_cond_neg_unsigned(bool negative, uint64_t u64)
{
    return negative ? int64_neg_unsigned(u64) : (int64_t) u64;
}

/**
 * @brief Check if unsigned 32-bit value (\c uint32_t) would overflow when converted to signed
 * (\c int32_t)
 *
 * Tests whether an unsigned 32-bit value can be represented as a signed
 * 32-bit integer, accounting for whether it will be negated. Negative
 * values can represent one more magnitude (\c INT32_MIN = -2147483648) than
 * positive values (\c INT32_MAX = 2147483647).
 *
 * @param u32 Unsigned magnitude to check
 * @param is_negative Whether the value will be negated
 * @return true if conversion would overflow, false if safe to convert
 *
 * @note Maximum representable positive: 2147483647 (\c INT32_MAX)
 * @note Maximum representable negative magnitude: 2147483648 (|\c INT32_MIN|)
 *
 * @see int32_cond_neg_unsigned() to perform the conversion after checking
 */
static inline bool uint32_does_overflow_int32(uint32_t u32, bool is_negative)
{
    return ((is_negative && (u32 > ((uint32_t) INT32_MAX) + 1))
        || (!is_negative && (u32 > ((uint32_t) INT32_MAX))));
}

/**
 * @brief Check if unsigned 64-bit value (\c uint64_t) would overflow when converted to signed
 * (\c int64_t)
 *
 * Tests whether an unsigned 64-bit value can be represented as a signed
 * 64-bit integer, accounting for whether it will be negated. Negative
 * values can represent one more magnitude (\c INT64_MIN) than positive
 * values (\c INT64_MAX).
 *
 * @param u64 Unsigned magnitude to check
 * @param is_negative Whether the value will be negated
 * @return true if conversion would overflow, false if safe to convert
 *
 * @note Maximum representable positive: 9223372036854775807 (\c INT64_MAX)
 * @note Maximum representable negative magnitude: 9223372036854775808 (|\c INT64_MIN|)
 *
 * @see int64_cond_neg_unsigned() to perform the conversion after checking
 */
static inline bool uint64_does_overflow_int64(uint64_t u64, bool is_negative)
{
    return ((is_negative && (u64 > ((uint64_t) INT64_MAX) + 1))
        || (!is_negative && (u64 > ((uint64_t) INT64_MAX))));
}

/**
 * @brief Compute absolute value of signed 32-bit integer (\c int32_t) as unsigned (\c uint32_t)
 *
 * Returns the absolute value of a signed 32-bit integer (\c int32_t) as an
 * unsigned 32-bit value (\c uint32_t). This function avoids undefined behavior
 * that would occur when negating \c INT32_MIN in signed arithmetic.
 *
 * @param i32 Signed integer (\c int32_t) to get absolute value of
 * @return Absolute value as unsigned 32-bit integer (\c uint32_t)
 *
 * @note Handles \c INT32_MIN correctly (returns 2147483648 as unsigned)
 * @note Caller can use result without concern for undefined behavior
 */
static inline uint32_t int32_safe_unsigned_abs(int32_t i32)
{
    return (i32 < 0) ? ((uint32_t) - (i32 + 1)) + 1 : (uint32_t) i32;
}

/**
 * @brief Compute absolute value of signed 64-bit integer (\c int64_t) as unsigned (\c uint64_t)
 *
 * Returns the absolute value of a signed 64-bit integer (\c int64_t) as an
 * unsigned 64-bit value (\c uint64_t). This function avoids undefined behavior
 * that would occur when negating \c INT64_MIN in signed arithmetic.
 *
 * @param i64 Signed integer (\c int64_t) to get absolute value of
 * @return Absolute value as unsigned 64-bit integer (\c uint64_t)
 *
 * @note Handles \c INT64_MIN correctly (returns 9223372036854775808 as unsigned)
 * @note Caller can use result without concern for undefined behavior
 */
static inline uint64_t int64_safe_unsigned_abs(int64_t i64)
{
    return (i64 < 0) ? ((uint64_t) - (i64 + 1)) + 1 : (uint64_t) i64;
}

/**
 * @brief Check if 32-bit signed integer (\c int32_t) is negative
 *
 * Efficient predicate to test if a 32-bit signed integer is negative,
 * equivalent to \c (i32 < 0).
 *
 * @param i32 Signed 32-bit integer to test
 * @return true if negative, false if zero or positive
 */
static inline bool int32_is_negative(int32_t i32)
{
    return ((uint32_t) i32) >> 31;
}

/**
 * @brief Check if 64-bit signed integer (\c int64_t) is negative
 *
 * Efficient predicate to test if a 64-bit signed integer is negative,
 * equivalent to (i64 < 0).
 *
 * @param i64 Signed 64-bit integer to test
 * @return true if negative, false if zero or positive
 */
static inline bool int64_is_negative(int64_t i64)
{
    return ((uint64_t) i64) >> 63;
}

/**
 * @brief Get absolute value as uint32_t and sign of 32-bit integer
 *
 * Computes the absolute value of a signed 32-bit integer (\c int32_t) as
 * unsigned (\c uint32_t) and sets a flag indicating whether the original
 * value was negative. Combines sign extraction and absolute value computation
 * for efficiency. Commonly used when serializing integers where the sign is
 * stored separately from the magnitude.
 *
 * @param i32 Signed integer to process
 * @param[out] is_negative Set to true if i32 is negative, false otherwise
 * @return Absolute value as unsigned 32-bit integer (\c uint32_t)
 *
 * @pre is_negative != NULL
 *
 * @note Useful for integer formatting and parsing operations
 * @note Handles \c INT32_MIN correctly
 *
 * @see int32_safe_unsigned_abs() for absolute value without sign flag
 * @see int32_is_negative() for sign checking only
 */
static inline uint32_t int32_safe_unsigned_abs_set_flag(int32_t i32, bool *is_negative)
{
    *is_negative = int32_is_negative(i32);
    return int32_safe_unsigned_abs(i32);
}

/**
 * @brief Get absolute value as uint64_t and sign of 64-bit integer
 *
 * Computes the absolute value of a signed 64-bit integer (\c int64_t) as
 * unsigned (\c uint64_t) and sets a flag indicating whether the original
 * value was negative. Combines sign extraction and absolute value computation
 * for efficiency. Commonly used when serializing integers where the sign is
 * stored separately from the magnitude.
 *
 * @param i64 Signed integer to process
 * @param[out] is_negative Set to true if i64 is negative, false otherwise
 * @return Absolute value as unsigned 64-bit integer (\c uint64_t)
 *
 * @pre is_negative != NULL
 *
 * @note Useful for integer formatting and parsing operations
 * @note Handles \c INT64_MIN correctly
 *
 * @see int64_safe_unsigned_abs() for absolute value without sign flag
 * @see int64_is_negative() for sign checking only
 */
static inline uint64_t int64_safe_unsigned_abs_set_flag(int64_t i64, bool *is_negative)
{
    *is_negative = int64_is_negative(i64);
    return int64_safe_unsigned_abs(i64);
}

/**
 * @brief Perform arithmetic right shift on 32-bit signed integer (\c int32_t)
 *
 * Performs a portable arithmetic right shift that preserves sign extension
 * across different compilers and architectures. Unlike the C >> operator
 * on signed integers (which has implementation-defined behavior for negative
 * values), this function guarantees arithmetic shift semantics.
 *
 * @param n Signed 32-bit integer (\c int32_t) to shift
 * @param rshift Number of bit positions to shift right
 * @return Right-shifted value with sign extension preserved
 *
 * @warning For shift amounts >= 32, behavior is undefined. Use \c int32_bsr_safe()
 *          for defined behavior with large shift amounts
 *
 * @note Negative values are sign-extended (arithmetic shift)
 * @note Positive values are zero-extended (logical shift)
 * @note Portable replacement for implementation-defined signed right shift
 *
 * @see int32_bsr_safe() for safe version with large shift handling
 */
static inline int32_t int32_bsr(int32_t n, size_t rshift)
{
    return (int32_t) ((n < 0) ? ~(~((uint32_t) n) >> rshift) : (((uint32_t) n) >> rshift));
}

/**
 * @brief Perform arithmetic right shift on 64-bit signed integer (\c int64_t)
 *
 * Performs a portable arithmetic right shift that preserves sign extension
 * across different compilers and architectures. Unlike the C >> operator
 * on signed integers (which has implementation-defined behavior for negative
 * values), this function guarantees arithmetic shift semantics.
 *
 * @param n Signed 64-bit integer (\c int64_t) to shift
 * @param rshift Number of bit positions to shift right
 * @return Right-shifted value with sign extension preserved
 *
 * @warning For shift amounts >= 64, behavior is undefined. Use \c int64_bsr_safe()
 *          for defined behavior with large shift amounts
 *
 * @note Negative values are sign-extended (arithmetic shift)
 * @note Positive values are zero-extended (logical shift)
 * @note Portable replacement for implementation-defined signed right shift
 *
 * @see int64_bsr_safe() for safe version with large shift handling
 */
static inline int64_t int64_bsr(int64_t n, size_t rshift)
{
    return (int64_t) ((n < 0) ? ~(~((uint64_t) n) >> rshift) : (((uint64_t) n) >> rshift));
}

/**
 * @brief Safely perform arithmetic right shift on 32-bit signed integer (\c int32_t)
 *
 * Performs a portable arithmetic right shift with defined behavior for
 * shift amounts >= 32 bits. This follows Erlang's semantics where right
 * shifts beyond the bit width converge to -1 for negative values and 0
 * for non-negative values.
 *
 * @param n Signed 32-bit integer (\c int32_t) to shift
 * @param rshift Number of bit positions to shift right
 * @return Right-shifted value, or -1 (negative) / 0 (non-negative) for shifts >= 32
 *
 * @note For rshift >= 32: returns -1 if n < 0, returns 0 if n >= 0
 * @note For rshift < 32: performs standard arithmetic right shift
 * @note Erlang-inspired semantics for large shifts
 *
 * @see int32_bsr() for version without large shift protection
 */
static inline int32_t int32_bsr_safe(int32_t n, size_t rshift)
{
    if (rshift >= 32) {
        return n < 0 ? -1 : 0;
    }
    return int32_bsr(n, rshift);
}

/**
 * @brief Safely perform arithmetic right shift on 64-bit signed integer (\c int64_t)
 *
 * Performs a portable arithmetic right shift with defined behavior for
 * shift amounts >= 64 bits. This follows Erlang's semantics where right
 * shifts beyond the bit width converge to -1 for negative values and 0
 * for non-negative values.
 *
 * @param n Signed 64-bit integer (\c int64_t) to shift
 * @param rshift Number of bit positions to shift right
 * @return Right-shifted value, or -1 (negative) / 0 (non-negative) for shifts >= 64
 *
 * @note For rshift >= 64: returns -1 if n < 0, returns 0 if n >= 0
 * @note For rshift < 64: performs standard arithmetic right shift
 * @note Erlang-inspired semantics for large shifts
 *
 * @see int64_bsr() for version without large shift protection
 */
static inline int64_t int64_bsr_safe(int64_t n, size_t rshift)
{
    if (rshift >= 64) {
        return n < 0 ? -1 : 0;
    }
    return int64_bsr(n, rshift);
}

/**
 * @brief Perform left shift on 32-bit signed integer (\c int32_t) with overflow detection
 *
 * Performs a left shift operation with overflow detection. The shift is
 * always defined (even for shift amounts >= 32), and the function reports
 * whether the operation would lose information. This provides safe,
 * portable bit shifting with predictable overflow semantics.
 *
 * @param n Signed 32-bit integer (\c int32_t) to shift
 * @param lshift Number of bit positions to shift left
 * @param[out] out Result of the shift operation (0 for shifts >= 32)
 * @return true if overflow occurred (information lost), false if exact
 *
 * @pre out != NULL
 *
 * @note For lshift >= 32: sets *out to 0, returns true if n != 0
 * @note For lshift < 32: performs shift and checks if reversible
 *
 * @see int32_bsr() used internally for overflow checking
 */
static inline bool int32_bsl_overflow(int32_t n, size_t lshift, int32_t *out)
{
    if (lshift >= 32) {
        *out = 0;
        return (n != 0);
    }

    int32_t res = (int32_t) (((uint32_t) n) << lshift);
    *out = res;
    int32_t check = int32_bsr(res, lshift);
    return check != n;
}

/**
 * @brief Perform left shift on 64-bit signed integer (\c int64_t) with overflow detection
 *
 * Performs a left shift operation with overflow detection. The shift is
 * always defined (even for shift amounts >= 64), and the function reports
 * whether the operation would lose information. This provides safe,
 * portable bit shifting with predictable overflow semantics.
 *
 * @param n Signed 64-bit integer (\c int64_t) to shift
 * @param lshift Number of bit positions to shift left
 * @param[out] out Result of the shift operation (0 for shifts >= 64)
 * @return true if overflow occurred (information lost), false if exact
 *
 * @pre out != NULL
 *
 * @note For lshift >= 64: sets *out to 0, returns true if n != 0
 * @note For lshift < 64: performs shift and checks if reversible
 * @note Overflow detection works by shifting back and comparing with original
 *
 * @see int64_bsr() used internally for overflow checking
 */
static inline bool int64_bsl_overflow(int64_t n, size_t lshift, int64_t *out)
{
    if (lshift >= 64) {
        *out = 0;
        return (n != 0);
    }

    int64_t res = (int64_t) (((uint64_t) n) << lshift);
    *out = res;
    int64_t check = int64_bsr(res, lshift);
    return check != n;
}

/**
 * @def INTPTR_WRITE_TO_ASCII_BUF_LEN
 * @brief Required buffer size for \c intptr_t to ASCII conversion
 *
 * Defines the maximum buffer size needed to hold any \c intptr_t value
 * converted to ASCII in any base (2-36), including sign character.
 * This constant ensures safe buffer allocation for \c intptr_write_to_ascii_buf().
 *
 * @note Value depends on platform pointer size (33 bytes for 32-bit, 65 bytes for 64-bit)
 * @warning Always use this constant to allocate buffers for \c intptr_write_to_ascii_buf()
 *
 * @see intptr_write_to_ascii_buf()
 */
#if INTPTR_MAX <= INT32_MAX
#define INTPTR_WRITE_TO_ASCII_BUF_LEN (32 + 1)
#elif INTPTR_MAX <= INT64_MAX
#define INTPTR_WRITE_TO_ASCII_BUF_LEN (64 + 1)
#endif

/**
 * @def INT32_WRITE_TO_ASCII_BUF_LEN
 * @brief Required buffer size for \c int32_t to ASCII conversion
 *
 * Defines the maximum buffer size needed to hold any \c int32_t value
 * converted to ASCII in any base (2-36), including sign character.
 * This constant ensures safe buffer allocation for \c int32_write_to_ascii_buf().
 *
 * @note Always 33 bytes (32 digits for base 2 plus sign)
 * @warning Always use this constant to allocate buffers for \c int32_write_to_ascii_buf()
 *
 * @see int32_write_to_ascii_buf()
 */
#define INT32_WRITE_TO_ASCII_BUF_LEN (32 + 1)

/**
 * @def INT64_WRITE_TO_ASCII_BUF_LEN
 * @brief Required buffer size for \c int64_t to ASCII conversion
 *
 * Defines the maximum buffer size needed to hold any \c int64_t value
 * converted to ASCII in any base (2-36), including sign character.
 * This constant ensures safe buffer allocation for \c int64_write_to_ascii_buf().
 *
 * @note Always 65 bytes (64 digits for base 2 plus sign)
 * @warning Always use this constant to allocate buffers for \c int64_write_to_ascii_buf()
 *
 * @see int64_write_to_ascii_buf()
 */
#define INT64_WRITE_TO_ASCII_BUF_LEN (64 + 1)

/**
 * @brief Convert \c intptr_t to ASCII representation in specified base
 *
 * Writes the ASCII representation of a signed integer to a buffer, starting
 * from the end and working backwards. The function returns the number of
 * characters written. This design allows efficient conversion without
 * requiring string reversal.
 *
 * @param n Integer value (\c intptr_t) to convert
 * @param base Number base for conversion (2-36)
 * @param out_end Pointer to one-past-last position of output buffer
 * @return Number of characters written to buffer
 *
 * @pre base >= 2 && base <= 36
 * @pre out_end points to valid buffer with at least \c INTPTR_WRITE_TO_ASCII_BUF_LEN bytes before
 * it
 * @post Characters written to [(out_end - return_value), out_end)
 * @post No null terminator is added
 *
 * @warning Buffer must be at least \c INTPTR_WRITE_TO_ASCII_BUF_LEN bytes
 * @warning Insufficient buffer size causes undefined behavior (buffer overflow)
 * @warning Caller must add null terminator if using result as C string
 *
 * @note Optimized implementations for base 10 and base 16
 * @note Negative numbers include leading '-' character
 * @note Digits > 9 represented as uppercase letters (A-Z)
 *
 * @code
 * char buffer[INTPTR_WRITE_TO_ASCII_BUF_LEN];
 * size_t len = intptr_write_to_ascii_buf(-42, 10, buffer + INTPTR_WRITE_TO_ASCII_BUF_LEN);
 * // Characters written at: buffer + INTPTR_WRITE_TO_ASCII_BUF_LEN - len
 * // Result: "-42" (3 characters)
 * @endcode
 */
size_t intptr_write_to_ascii_buf(intptr_t n, unsigned int base, char *out_end);

/**
 * @brief Convert \c int32_t to ASCII representation in specified base
 *
 * Writes the ASCII representation of a 32-bit signed integer to a buffer,
 * starting from the end and working backwards. The function returns the
 * number of characters written. This design allows efficient conversion
 * without requiring string reversal.
 *
 * @param n Integer value (\c int32_t) to convert
 * @param base Number base for conversion (2-36)
 * @param out_end Pointer to one-past-last position of output buffer
 * @return Number of characters written to buffer
 *
 * @pre base >= 2 && base <= 36
 * @pre out_end points to valid buffer with at least \c INT32_WRITE_TO_ASCII_BUF_LEN bytes before it
 * @post Characters written to [(out_end - return_value), out_end)
 * @post No null terminator is added
 *
 * @warning Buffer must be at least \c INT32_WRITE_TO_ASCII_BUF_LEN bytes
 * @warning Insufficient buffer size causes undefined behavior (buffer overflow)
 * @warning Caller must add null terminator if using result as C string
 *
 * @note Optimized implementations for base 10 and base 16
 * @note Negative numbers include leading '-' character
 * @note Digits > 9 represented as uppercase letters (A-Z)
 *
 * @code
 * char buffer[INT32_WRITE_TO_ASCII_BUF_LEN];
 * size_t len = int32_write_to_ascii_buf(-42, 10, buffer + INT32_WRITE_TO_ASCII_BUF_LEN);
 * // Characters written at: buffer + INT32_WRITE_TO_ASCII_BUF_LEN - len
 * // Result: "-42" (3 characters)
 * @endcode
 */
#if INTPTR_MAX >= INT32_MAX
static inline size_t int32_write_to_ascii_buf(int32_t n, unsigned int base, char *out_end)
{
    return intptr_write_to_ascii_buf(n, base, out_end);
}
#endif

/**
 * @brief Convert \c int64_t to ASCII representation in specified base
 *
 * Writes the ASCII representation of a 64-bit signed integer to a buffer,
 * starting from the end and working backwards. The function returns the
 * number of characters written. This design allows efficient conversion
 * without requiring string reversal.
 *
 * @param n Integer value (\c int64_t) to convert
 * @param base Number base for conversion (2-36)
 * @param out_end Pointer to one-past-last position of output buffer
 * @return Number of characters written to buffer
 *
 * @pre base >= 2 && base <= 36
 * @pre out_end points to valid buffer with at least \c INT64_WRITE_TO_ASCII_BUF_LEN bytes before it
 * @post Characters written to [(out_end - return_value), out_end)
 * @post No null terminator is added
 *
 * @warning Buffer must be at least \c INT64_WRITE_TO_ASCII_BUF_LEN bytes
 * @warning Insufficient buffer size causes undefined behavior (buffer overflow)
 * @warning Caller must add null terminator if using result as C string
 *
 * @note Optimized implementations for base 10 and base 16
 * @note Negative numbers include leading '-' character
 * @note Digits > 9 represented as uppercase letters (A-Z)
 *
 * @code
 * char buffer[INT64_WRITE_TO_ASCII_BUF_LEN];
 * size_t len = int64_write_to_ascii_buf(INT64_MIN, 10, buffer + INT64_WRITE_TO_ASCII_BUF_LEN);
 * // Characters written at: buffer + INT64_WRITE_TO_ASCII_BUF_LEN - len
 * // Result: "-9223372036854775808" (20 characters)
 * @endcode
 */
#if INT64_MAX > INTPTR_MAX
size_t int64_write_to_ascii_buf(int64_t n, unsigned int base, char *out_end);
#else
static inline size_t int64_write_to_ascii_buf(int64_t n, unsigned int base, char *out_end)
{
    return intptr_write_to_ascii_buf(n, base, out_end);
}
#endif

/**
 * @brief Options for integer parsing behavior
 *
 * Controls how \c int64_parse_ascii_buf() handles signs and other
 * parsing options. Options can be combined using bitwise OR.
 */
typedef enum
{
    /** @brief Default parsing behavior - accepts signs (+/-) */
    BufToInt64NoOptions,

    /** @brief Reject sign characters - parse unsigned magnitude only */
    BufToInt64RejectSign
} buf_to_int64_options_t;

/**
 * @brief Parse ASCII buffer to \c int64_t in specified base
 *
 * Parses an ASCII representation of an integer from a buffer (not necessarily
 * null-terminated) into a 64-bit signed integer. Supports bases 2-36 with
 * optimized paths for base 10 and 16. The function is designed to support
 * parsing arbitrarily large integers by processing them in chunks - it returns
 * the position where parsing stopped, allowing callers to continue parsing
 * from that point.
 *
 * @param buf Buffer containing ASCII digits to parse
 * @param buf_len Length of buffer in bytes
 * @param base Number base for parsing (2-36)
 * @param options Parsing options (e.g., reject sign characters)
 * @param[out] out Parsed integer value (valid even on overflow)
 * @return Position after last successfully parsed character, or -1 on format error
 *
 * @pre base >= 2 && base <= 36
 * @pre buf != NULL when buf_len > 0 (NULL allowed only for zero-length buffer)
 * @pre out != NULL
 * @post On success: *out contains parsed value up to position returned
 * @post On overflow: *out contains value parsed before overflow, returns position where overflow
 * occurred
 * @post On format error: returns -1, *out is undefined
 *
 * @note Leading zeros are skipped automatically
 * @note Signs (+/-) accepted unless \c BufToInt64RejectSign is set
 * @note Case-insensitive for letter digits (a-z, A-Z)
 * @note Optimized implementations for base 10 and base 16
 * @note Stops parsing at first invalid character or overflow
 *
 * @warning Return value -1 indicates format error (invalid digit for base)
 * @warning Return value < buf_len may indicate overflow or invalid character
 *
 * @code
 * // Simple parsing
 * int64_t value;
 * const char *number = "12345";
 * int pos = int64_parse_ascii_buf(number, strlen(number), 10, BufToInt64NoOptions, &value);
 * if (pos == strlen(number)) {
 *     // Successfully parsed entire buffer: value = 12345
 * }
 *
 * // Parsing with overflow detection
 * const char *big_num = "99999999999999999999999";
 * int pos = int64_parse_ascii_buf(big_num, strlen(big_num), 10, BufToInt64NoOptions, &value);
 * if (pos < strlen(big_num)) {
 *     // Overflow occurred at position pos
 *     // value contains the maximum representable value before overflow
 * }
 *
 * // Chunk parsing for arbitrarily large integers
 * const char *chunks[] = {"12345", "67890", "12345"};
 * int64_t accumulated = 0;
 * for (int i = 0; i < 3; i++) {
 *     int64_t chunk;
 *     int pos = int64_parse_ascii_buf(chunks[i], 5, 10, BufToInt64RejectSign, &chunk);
 *     // Process chunk value...
 * }
 * @endcode
 *
 * @see int64_write_to_ascii_buf() for the inverse operation
 */
int int64_parse_ascii_buf(const char buf[], size_t buf_len, unsigned int base,
    buf_to_int64_options_t options, int64_t *out);

#ifdef __cplusplus
}
#endif

#endif
