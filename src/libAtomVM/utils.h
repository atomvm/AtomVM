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

#include <stddef.h>

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

#endif
