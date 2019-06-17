/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
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

#ifndef _OVERFLOW_HELPERS_H_
#define _OVERFLOW_HELPERS_H_

#ifdef __GNUC__
#if __GNUC__ >= 5
    #define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
    #define BUILTIN_SUB_OVERFLOW __builtin_sub_overflow
    #define BUILTIN_MUL_OVERFLOW __builtin_mul_overflow

    #define BUILTIN_ADD_OVERFLOW_INT __builtin_add_overflow
#endif
#endif

#ifdef __has_builtin
#if __has_builtin(__builtin_add_overflow)
    #define BUILTIN_ADD_OVERFLOW __builtin_add_overflow
    #define BUILTIN_ADD_OVERFLOW_INT __builtin_add_overflow
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
#define BUILTIN_ADD_OVERFLOW_INT atomvm_add_overflow_int

#include <stdint.h>
#include "term.h"

static inline int atomvm_add_overflow(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    // a and b are shifted integers
    avm_int_t sum = (a >> 4) + (b >> 4);
    *res = sum << 4;
    return ((sum > MAX_NOT_BOXED_INT) || (sum < MIN_NOT_BOXED_INT));
}

static inline int atomvm_add_overflow_int(avm_int_t a, avm_int_t b, avm_int_t *res)
{
    avm_int64_t tmp_res = a + b;
    *res = tmp_res;
    return ((tmp_res > AVM_INT_MAX) || (tmp_res < AVM_INT_MIN));
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

#endif
