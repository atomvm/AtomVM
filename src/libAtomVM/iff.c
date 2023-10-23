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
        fprintf(stderr, "error: buffer holding IFF is smaller than IFF size: %i\n", buf_size);
        // TODO: return error instead of crashing
        AVM_ABORT();
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
        } else if (!memcmp(current_record->name, "StrT", 4)) {
            offsets[STRT] = current_pos;
            sizes[STRT] = ENDIAN_SWAP_32(current_record->size);
        } else if (!memcmp(current_record->name, "Line", 4)) {
            offsets[LINT] = current_pos;
            sizes[LINT] = ENDIAN_SWAP_32(current_record->size);
        }

        current_pos += iff_align(ENDIAN_SWAP_32(current_record->size) + 8);
    } while (current_pos < file_size);
}
