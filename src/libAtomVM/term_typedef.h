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

#include "limits.h"

/**
 * A value of any data type, types bigger than a machine word will require some additional space on heap.
 */
typedef unsigned long term;

#if ULONG_MAX == 4294967295UL
    #define TERM_BITS 32
    #define TERM_BYTES 4

#elif ULONG_MAX == 18446744073709551615UL
    #define TERM_BITS 64
    #define TERM_BYTES 8

#else
    #error "Cannot detect unsigned long size."
#endif

#endif
