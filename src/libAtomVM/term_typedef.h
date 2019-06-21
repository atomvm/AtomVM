/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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
 * @file term_typedef.h
 * @brief term type definition
 *
 * @details This header defines term time and few related macros.
 */

#ifndef _TERM_TYPEDEF_H_
#define _TERM_TYPEDEF_H_

#include <limits.h>
#include <stdint.h>

/**
 * A value of any data type, types bigger than a machine word will require some additional space on heap.
 */
typedef uintptr_t term;

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
    #define AVM_INT64_FMT "%li"
#elif INT64_MAX == LLONG_MAX
    #define AVM_INT64_FMT "%lli"
#else
    #error "cannot define AVM_INT64_FMT: invalid build env."
#endif

#endif
