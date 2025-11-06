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

static inline int pad(int size)
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

int avmpack_find_section_by_flag(const void *avmpack_binary, uint32_t flags_mask, uint32_t flags_val, const void **ptr, uint32_t *size, const char **name)
{
    int offset = AVMPACK_SIZE;
    const uint32_t *flags;

    do {
        const uint32_t *sizes = ((const uint32_t *) (avmpack_binary)) + offset / sizeof(uint32_t);
        flags = ((const uint32_t *) (avmpack_binary)) + 1 + offset / sizeof(uint32_t);

        if ((ENDIAN_SWAP_32(*flags) & flags_mask) == flags_val) {
            const char *found_section_name = (const char *) (sizes + 3);
            int section_name_len = pad(strlen(found_section_name) + 1);

            *ptr = sizes + 3 + section_name_len / sizeof(uint32_t);
            *size = ENDIAN_SWAP_32(*sizes);
            *name = (const char *) (sizes + 3);
            return 1;
        }

        offset += ENDIAN_SWAP_32(*sizes);

    } while (*flags);

    return 0;
}

int avmpack_find_section_by_name(const void *avmpack_binary, const char *name, const void **ptr, uint32_t *size)
{
    int offset = AVMPACK_SIZE;
    const uint32_t *flags;

    do {
        const uint32_t *sizes = ((const uint32_t *) (avmpack_binary)) + offset / sizeof(uint32_t);
        flags = ((const uint32_t *) (avmpack_binary)) + 1 + offset / sizeof(uint32_t);

        const char *found_section_name = (const char *) (sizes + 3);
        if (!strcmp(name, found_section_name)) {
            int section_name_len = pad(strlen(found_section_name) + 1);

            *ptr = sizes + 3 + section_name_len / sizeof(uint32_t);
            *size = ENDIAN_SWAP_32(*sizes);
            return 1;
        }

        offset += ENDIAN_SWAP_32(*sizes);

    } while (*flags);

    return 0;
}

void *avmpack_fold(void *accum, const void *avmpack_binary, avmpack_fold_fun fold_fun)
{
    int offset = AVMPACK_SIZE;
    uint32_t size = 0;

    do {
        const uint32_t *size_ptr = ((const uint32_t *) (avmpack_binary)) + offset / sizeof(uint32_t);
        size = ENDIAN_SWAP_32(*size_ptr);
        if (size > 0) {
            const uint32_t *flags_ptr = size_ptr + 1;
            uint32_t flags = ENDIAN_SWAP_32(*flags_ptr);
            const char *section_name = (const char *) (size_ptr + 3);
            int section_name_len = pad(strlen(section_name) + 1);
            accum = fold_fun(
                accum,
                size_ptr, size,
                size_ptr + 3 + section_name_len / sizeof(uint32_t),
                flags,
                section_name);
            offset += size;
        }
    } while (size > 0);

    return accum;
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
