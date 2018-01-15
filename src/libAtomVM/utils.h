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

#endif
