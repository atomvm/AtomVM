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
            *((uint64_t *) (ptr)) = ( \
                ((uint64_t) ((uint8_t *)(&val))[0] << 56) | ((uint64_t) ((uint8_t *) (&val))[1] << 48) | \
                ((uint64_t) ((uint8_t *)(&val))[2] << 40) | ((uint64_t) ((uint8_t *) (&val))[3] << 32) | \
                ((uint64_t) ((uint8_t *)(&val))[4] << 24) | ((uint64_t) ((uint8_t *) (&val))[5] << 16) | \
                ((uint64_t) ((uint8_t *)(&val))[6] <<  8) | (            (uint8_t *) (&val))[7] \
            )

        #define READ_32_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

        #define WRITE_32_UNALIGNED(ptr, val) \
            *((uint32_t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 24) | (((uint8_t *) (&val))[1] << 16) | (((uint8_t *)(&val))[2] << 8) | ((uint8_t *)(&val))[3] )

        #define READ_16_UNALIGNED(ptr) \
            ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

        #define WRITE_16_UNALIGNED(ptr, val) \
            *((uint16_t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 8) | ((uint8_t *)(&val))[1] )
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

    #define WRITE_32_UNALIGNED(ptr, val) \
        *((uint32_t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 24) | (((uint8_t *) (&val))[1] << 16) | (((uint8_t *)(&val))[2] << 8) | ((uint8_t *)(&val))[3] )

    #define READ_16_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

    #define WRITE_16_UNALIGNED(ptr, val) \
        *((uint16_t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 8) | ((uint8_t *)(&val))[1] )

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

#ifdef AVM_VERBOSE_ABORT
#include <stdio.h>
#define AVM_ABORT()                                                           \
    {                                                                         \
        fprintf(stderr, "Abort in file %s at line %i\n", __FILE__, __LINE__); \
        abort();                                                              \
    }
#else
#define AVM_ABORT() abort()
#endif

#endif
