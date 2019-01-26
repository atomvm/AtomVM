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

#include "iff.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct IFFRecord
{
    const char name[4];
    uint32_t size;
};

static uint32_t iff_align(uint32_t size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

int iff_is_valid_beam(const void *beam_data)
{
    return memcmp(beam_data, "FOR1", 4) == 0;
}

void scan_iff(const void *iff_binary, int buf_size, unsigned long *offsets, unsigned long *sizes)
{
    const uint8_t *data = iff_binary;

    memset(offsets, 0, sizeof(unsigned long) * MAX_OFFS);
    memset(sizes, 0, sizeof(unsigned long) * MAX_SIZES);

    int current_pos = 12;

    uint32_t iff_size = READ_32_ALIGNED(data + 4);
    int file_size = iff_size;
    if (UNLIKELY(buf_size < file_size)) {
        fprintf(stderr, "warning: buffer holding IFF is smaller than IFF size: %i", buf_size);
    }

    do {
        struct IFFRecord *current_record = (struct IFFRecord *) (data + current_pos);

        if (!memcmp(current_record->name, "AtU8", 4)) {
            offsets[AT8U] = current_pos;
            sizes[AT8U] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "Code", 4)) {
            offsets[CODE] = current_pos;
            sizes[CODE] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "ExpT", 4)) {
            offsets[EXPT] = current_pos;
            sizes[EXPT] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "LocT", 4)) {
            offsets[LOCT] = current_pos;
            sizes[LOCT] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "LitT", 4)) {
            offsets[LITT] = current_pos;
            sizes[LITT] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "LitU", 4)) {
            offsets[LITU] = current_pos;
            sizes[LITU] = ENDIAN_SWAP_32(current_record->size);

        } else if (!memcmp(current_record->name, "ImpT", 4)) {
            offsets[IMPT] = current_pos;
            sizes[IMPT] = ENDIAN_SWAP_32(current_record->size);
        } else if (!memcmp(current_record->name, "FunT", 4)) {
            offsets[FUNT] = current_pos;
            sizes[FUNT] = ENDIAN_SWAP_32(current_record->size);
        }

        current_pos += iff_align(ENDIAN_SWAP_32(current_record->size) + 8);
    } while (current_pos < file_size);
}
