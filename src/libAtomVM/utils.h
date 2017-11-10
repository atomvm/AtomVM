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

#ifndef _UTILS_H_
#define _UTILS_H_

#include <byteswap.h>

#ifdef __ORDER_LITTLE_ENDIAN__
    #define READ_32_ALIGNED(ptr) \
        bswap_32(*((uint32_t *) (ptr)))

    #define ENDIAN_SWAP_32(value) bswap_32(value)
#else
    #define READ_32_ALIGNED(ptr) \
        (*((uint32_t *) (ptr)))

    #define ENDIAN_SWAP_32(value) (value)
#endif

#define UNUSED(x) (void) (x);

#endif
