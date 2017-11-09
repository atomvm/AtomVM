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

#include <stdlib.h>
#include <string.h>

#include "utils.h"

struct IFFRecord
{
    const char name[4];
    uint32_t size;
};

static uint32_t iff_align(uint32_t size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

void scan_iff(uint8_t *data, int file_size, unsigned long *offsets)
{
    int current_pos = 12;

    do {
        struct IFFRecord *current_record = (struct IFFRecord *) (data + current_pos);

        if (!memcmp(current_record->name, "AtU8", 4)) {
            offsets[AT8U] = current_pos;

        } else if (!memcmp(current_record->name, "Code", 4)) {
            offsets[CODE] = current_pos;

        } else if (!memcmp(current_record->name, "LocT", 4)) {
            offsets[LOCT] = current_pos;

        } else if (!memcmp(current_record->name, "ImpT", 4)) {
            offsets[IMPT] = current_pos;
        }

        current_pos += iff_align(bswap_32(current_record->size) + 8);
    } while (current_pos < file_size);
}


