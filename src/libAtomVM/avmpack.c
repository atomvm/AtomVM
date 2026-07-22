/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

#include "avmpack.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

#include <stdio.h>

#define AVMPACK_SIZE 24
#define AVMPACK_SECTION_HEADER_SIZE 12
#define AVMPACK_MIN_SECTION_SIZE 16
#define AVMPACK_END_MARKER_SIZE 16

static inline size_t pad(size_t size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

bool avmpack_is_valid(const void *avmpack_binary, uint32_t size)
{
    // "#!/usr/bin/env AtomVM"
    const unsigned char pack_header[AVMPACK_SIZE] = {
        0x23, 0x21, 0x2f, 0x75,
        0x73, 0x72, 0x2f, 0x62,
        0x69, 0x6e, 0x2f, 0x65,
        0x6e, 0x76, 0x20, 0x41,
        0x74, 0x6f, 0x6d, 0x56,
        0x4d, 0x0a, 0x00, 0x00
    };

    if (UNLIKELY(size < 24)) {
        return false;
    }

    return memcmp(avmpack_binary, pack_header, AVMPACK_SIZE) == 0;
}

enum AVMPackSectionKind
{
    AVMPackSectionInvalid,
    AVMPackSectionRegular,
    AVMPackSectionEnd
};

struct AVMPackSection
{
    uint32_t size;
    uint32_t data_size;
    uint32_t flags;
    const char *name;
    const void *data;
};

static enum AVMPackSectionKind read_section(const void *avmpack_binary, uint32_t avmpack_size,
    uint32_t offset, struct AVMPackSection *section)
{
    if (offset > avmpack_size || avmpack_size - offset < AVMPACK_MIN_SECTION_SIZE) {
        return AVMPackSectionInvalid;
    }

    // A pack may sit at any byte offset (e.g. atomvm:add_avm_pack_binary/2 with a sub-binary).
    const uint8_t *header = (const uint8_t *) avmpack_binary + offset;
    uint32_t section_size = READ_32_UNALIGNED(header);
    uint32_t flags = READ_32_UNALIGNED(header + 4);
    const char *name = (const char *) (header + AVMPACK_SECTION_HEADER_SIZE);

    if (section_size == 0) {
        // The min-size guard above keeps the terminator name bytes in bounds for strcmp.
        if (flags == 0 && (strcmp(name, "end") == 0 || strcmp(name, "END") == 0)) {
            section->size = 0;
            section->data_size = 0;
            section->flags = 0;
            section->name = name;
            section->data = (const uint8_t *) avmpack_binary + offset + AVMPACK_END_MARKER_SIZE;
            return AVMPackSectionEnd;
        }
        return AVMPackSectionInvalid;
    }

    if (section_size < AVMPACK_MIN_SECTION_SIZE || (section_size & 3) != 0
        || section_size > avmpack_size - offset) {
        return AVMPackSectionInvalid;
    }

    size_t name_region = section_size - AVMPACK_SECTION_HEADER_SIZE;
    const void *nul = memchr(name, '\0', name_region);
    if (nul == NULL) {
        return AVMPackSectionInvalid;
    }
    size_t padded_name_len = pad((size_t) ((const char *) nul - name) + 1);
    if (padded_name_len > name_region) {
        return AVMPackSectionInvalid;
    }

    section->size = section_size;
    section->data_size = section_size - AVMPACK_SECTION_HEADER_SIZE - (uint32_t) padded_name_len;
    section->flags = flags;
    section->name = name;
    section->data
        = (const uint8_t *) avmpack_binary + offset + AVMPACK_SECTION_HEADER_SIZE + padded_name_len;

    return AVMPackSectionRegular;
}

int avmpack_find_section_by_flag(const void *avmpack_binary, uint32_t avmpack_size,
    uint32_t flags_mask, uint32_t flags_val, const void **ptr, uint32_t *size, const char **name)
{
    uint32_t offset = AVMPACK_SIZE;
    struct AVMPackSection section;
    enum AVMPackSectionKind kind;

    while ((kind = read_section(avmpack_binary, avmpack_size, offset, &section))
        != AVMPackSectionInvalid) {
        if ((section.flags & flags_mask) == flags_val) {
            *ptr = section.data;
            *size = section.data_size;
            *name = section.name;
            return 1;
        }
        if (kind == AVMPackSectionEnd) {
            break;
        }
        offset += section.size;
    }

    return 0;
}

int avmpack_find_section_by_name(const void *avmpack_binary, uint32_t avmpack_size,
    const char *name, const void **ptr, uint32_t *size)
{
    uint32_t offset = AVMPACK_SIZE;
    struct AVMPackSection section;

    while (read_section(avmpack_binary, avmpack_size, offset, &section) == AVMPackSectionRegular) {
        if (!strcmp(name, section.name)) {
            *ptr = section.data;
            *size = section.data_size;
            return 1;
        }
        offset += section.size;
    }

    return 0;
}

void *avmpack_fold(
    void *accum, const void *avmpack_binary, uint32_t avmpack_size, avmpack_fold_fun fold_fun)
{
    uint32_t offset = AVMPACK_SIZE;
    struct AVMPackSection section;

    while (read_section(avmpack_binary, avmpack_size, offset, &section) == AVMPackSectionRegular) {
        accum = fold_fun(accum, (const uint8_t *) avmpack_binary + offset, section.size,
            section.data, section.flags, section.name);
        offset += section.size;
    }

    return accum;
}

bool avmpack_is_complete(const void *avmpack_binary, uint32_t exact_size)
{
    // The header and every section are multiples of 4, so a complete pack's size must be too.
    if (!avmpack_is_valid(avmpack_binary, exact_size)
        || exact_size < AVMPACK_SIZE + AVMPACK_END_MARKER_SIZE || (exact_size & 3) != 0) {
        return false;
    }

    struct AVMPackSection section;

    return read_section(avmpack_binary, exact_size, exact_size - AVMPACK_END_MARKER_SIZE, &section)
        == AVMPackSectionEnd;
}

bool avmpack_compute_size(const void *avmpack_binary, uint32_t max_size, uint32_t *real_size)
{
    if (!avmpack_is_valid(avmpack_binary, max_size)) {
        return false;
    }

    uint32_t offset = AVMPACK_SIZE;
    struct AVMPackSection section;
    enum AVMPackSectionKind kind;
    while ((kind = read_section(avmpack_binary, max_size, offset, &section))
        == AVMPackSectionRegular) {
        offset += section.size;
    }

    if (kind == AVMPackSectionEnd) {
        *real_size = offset + AVMPACK_END_MARKER_SIZE;
        return true;
    }

    return false;
}

static void in_memory_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

const struct AVMPackInfo in_memory_avm_pack_info = {
    .destructor = in_memory_avm_pack_destructor
};

static void in_memory_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);

    free((void *) obj->data);
    free(obj);
}

static void const_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

const struct AVMPackInfo const_avm_pack_info = {
    .destructor = const_avm_pack_destructor
};

static void const_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    UNUSED(global);

    free(obj);
}
