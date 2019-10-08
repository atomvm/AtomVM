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
        #define READ_64_UNALIGNED(ptr) \
            __builtin_bswap64(*((uint64_t *) (ptr)))

        #define READ_32_UNALIGNED(ptr) \
            __builtin_bswap32(*((uint32_t *) (ptr)))

        #define READ_16_UNALIGNED(ptr) \
            __builtin_bswap16(*((uint16_t *) (ptr)))
    #else
        #define READ_64_UNALIGNED(ptr) \
            ( (((uint64_t) ((uint8_t *)(ptr))[0]) << 56) | (((uint64_t) ((uint8_t *) (ptr))[1]) << 48) | \
              (((uint64_t) ((uint8_t *)(ptr))[2]) << 40) | (((uint64_t) ((uint8_t *) (ptr))[3]) << 32) | \
              (((uint64_t) ((uint8_t *)(ptr))[4]) << 24) | (((uint64_t) ((uint8_t *) (ptr))[5]) << 16) | \
              (((uint64_t) ((uint8_t *)(ptr))[6]) << 8) | (((uint64_t) ((uint8_t *) (ptr))[7])) )

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
