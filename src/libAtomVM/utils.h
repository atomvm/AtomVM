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

/**
 * @file utils.h
 * @brief Misc functions and macros.
 *
 * @details Miscellaneous functions and macros useful for different tasks, like endian byteswap, unaligned reads, marking unused vars, etc...
 */

#ifndef _UTILS_H_
#define _UTILS_H_

#ifdef __ORDER_LITTLE_ENDIAN__
    #ifdef __GNUC__
        #define READ_32_ALIGNED(ptr) \
            __builtin_bswap32(*((uint32_t *) (ptr)))
    #else
        #define READ_32_ALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )
    #endif

    #if defined(__x86_64__) || defined(_M_X64) || defined(__i386__) || defined(_M_IX86)
        #define READ_32_UNALIGNED(ptr) \
            __builtin_bswap32(*((uint32_t *) (ptr)))

        #define READ_16_UNALIGNED(ptr) \
            __builtin_bswap16(*((uint16_t *) (ptr)))
    #else
        #define READ_32_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

        #define READ_16_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )
    #endif

    #ifdef __GNUC__
        #define ENDIAN_SWAP_32(value) __builtin_bswap32(value)
    #else
        #define ENDIAN_SWAP_32(value) ((((value) & 0xFF) << 24) | (((value) & 0xFF00) << 8) | (((value) & 0xFF0000) >> 8) | (((value) & 0xFF000000) >> 24))
    #endif

#else
    #define READ_32_ALIGNED(ptr) \
        (*((uint32_t *) (ptr)))

    #define READ_32_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

    #define READ_16_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

    #define ENDIAN_SWAP_32(value) (value)
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

#ifdef __GNUC__
#if __GNUC__ >= 5
    #define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
    #define BUILTIN_SUB_OVERFLOW __builtin_sub_overflow
    #define BUILTIN_MUL_OVERFLOW __builtin_mul_overflow
#endif
#endif

#ifdef __has_builtin
#if __has_builtin(__builtin_add_overflow)
    #define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
#endif
#if __has_builtin(__builtin_sub_overflow)
    #define BUILTIN_SUB_OVERFLOW __builtin_sub_overflow
#endif
#if __has_builtin(__builtin_mul_overflow)
    #define BUILTIN_MUL_OVERFLOW __builtin_mul_overflow
#endif
#endif

#ifndef BUILTIN_ADD_OVERFLOW
#define BUILTIN_ADD_OVERFLOW atomvm_add_overflow

#include <stdint.h>
#include "term.h"

static inline int atomvm_add_overflow(long a, long b, long *res)
{
    // a and b are shifted integers
    long sum = (a >> 4) + (b >> 4);
    *res = sum << 4;
    return ((sum > MAX_NOT_BOXED_INT) || (sum < MIN_NOT_BOXED_INT));
}
#endif

#ifndef BUILTIN_SUB_OVERFLOW
#define BUILTIN_SUB_OVERFLOW atomvm_sub_overflow

#include <stdint.h>

static inline int atomvm_sub_overflow(int32_t a, int32_t b, int32_t *res)
{
    // a and b are shifted integers
    int32_t diff = (a >> 4) - (b >> 4);
    *res = diff << 4;
    return ((diff > 134217727) || (diff < -134217728));
}
#endif

#ifndef BUILTIN_MUL_OVERFLOW
#define BUILTIN_MUL_OVERFLOW atomvm_mul_overflow

#include <stdint.h>

static inline int atomvm_mul_overflow(int32_t a, int32_t b, int32_t *res)
{
    int64_t mul = (a >> 2) * (b >> 2);
    *res = (mul << 4);
    return ((mul > 134217727) || (mul < -134217728));
}
#endif

#ifdef ALLOC_RANDOM_FAILURE

#ifndef RAND_MODULO
#define RAND_MODULO 31
#endif

#include <stdlib.h>
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

#endif
