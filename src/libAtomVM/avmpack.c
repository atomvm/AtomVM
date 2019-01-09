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

#include "avmpack.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#define AVMPACK_SIZE 24

static inline int pad(int size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

int avmpack_is_valid(const void *avmpack_binary, uint32_t size)
{
    const unsigned char pack_header[AVMPACK_SIZE] =
    {
        0x23, 0x21, 0x2f, 0x75,
        0x73, 0x72, 0x2f, 0x62,
        0x69, 0x6e, 0x2f, 0x65,
        0x6e, 0x76, 0x20, 0x41,
        0x74, 0x6f, 0x6d, 0x56,
        0x4d, 0x0a, 0x00, 0x00
    };

    if (UNLIKELY(size < 24)) {
        return 0;
    }

    return memcmp(avmpack_binary, pack_header, AVMPACK_SIZE) == 0;
}

int avmpack_find_section_by_flag(const void *avmpack_binary, uint32_t flags_mask, const void **ptr, uint32_t *size, const char **name)
{
    int offset = AVMPACK_SIZE;
    const uint32_t *flags;

    do {
        const uint32_t *sizes = ((const uint32_t *) (avmpack_binary)) + offset/sizeof(uint32_t);
        flags = ((const uint32_t *) (avmpack_binary)) + 1 + offset/sizeof(uint32_t);

        if ((ENDIAN_SWAP_32(*flags) & flags_mask) == flags_mask) {
            const char *found_section_name = (const char *) (sizes + 3);
            int section_name_len = pad(strlen(found_section_name) + 1);

            *ptr = sizes + 3 + section_name_len/sizeof(uint32_t);
            *size = ENDIAN_SWAP_32(*sizes);
            *name = (const char *) (sizes + 3);
            return 1;
        }

        offset += ENDIAN_SWAP_32(*sizes);

    } while(*flags);

    return 0;
}

int avmpack_find_section_by_name(const void *avmpack_binary, const char *name, const void **ptr, uint32_t *size)
{
    int offset = AVMPACK_SIZE;
    const uint32_t *flags;

    do {
        const uint32_t *sizes = ((const uint32_t *) (avmpack_binary)) + offset/sizeof(uint32_t);
        flags = ((const uint32_t *) (avmpack_binary)) + 1 + offset/sizeof(uint32_t);

        const char *found_section_name = (const char *) (sizes + 3);
        if (!strcmp(name, found_section_name)) {
            int section_name_len = pad(strlen(found_section_name) + 1);

            *ptr = sizes + 3 + section_name_len/sizeof(uint32_t);
            *size = ENDIAN_SWAP_32(*sizes);
            return 1;
        }

        offset += ENDIAN_SWAP_32(*sizes);

    } while(*flags);

    return 0;
}

void *avmpack_fold(void *accum, const void *avmpack_binary, avmpack_fold_fun fold_fun)
{
    int offset = AVMPACK_SIZE;
    uint32_t size = 0;

    do {
        const uint32_t *size_ptr = ((const uint32_t *) (avmpack_binary)) + offset/sizeof(uint32_t);
        size = ENDIAN_SWAP_32(*size_ptr);
        if (size > 0) {
            const uint32_t *flags_ptr = size_ptr + 1;
            uint32_t flags = ENDIAN_SWAP_32(*flags_ptr);
            const char *section_name = (const char *) (size_ptr + 3);
            int section_name_len = pad(strlen(section_name) + 1);
            accum = fold_fun(
                accum,
                size_ptr, size,
                size_ptr + 3 + section_name_len/sizeof(uint32_t),
                flags,
                section_name
            );
            offset += size;
        }
    } while(size > 0);

    return accum;
}
