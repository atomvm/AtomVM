/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include "app_loader.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

static uint32_t read_be32(const uint8_t *p)
{
    return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16)
        | ((uint32_t) p[2] << 8) | p[3];
}

static bool valid_beam(const uint8_t *data, size_t size, uint32_t *beam_size)
{
    if (size < 12 || memcmp(data, "FOR1", 4) != 0 || memcmp(data + 8, "BEAM", 4) != 0) {
        return false;
    }
    size_t length = read_be32(data + 4);
    if (length < 4 || length > size - 8 || size - 8 - length > 3) {
        return false;
    }
    length += 8;
    unsigned required_chunks = 0;
    size_t offset = 12;
    while (offset < length) {
        if (length - offset < 8) {
            return false;
        }
        const uint8_t *chunk = data + offset;
        size_t chunk_size = read_be32(chunk + 4);
        if (chunk_size > length - offset - 8) {
            return false;
        }
        size_t padded_size = (chunk_size + 3) & ~(size_t) 3;
        if (padded_size > length - offset - 8) {
            return false;
        }
        if (memcmp(chunk, "AtU8", 4) == 0 && chunk_size >= 4) {
            required_chunks |= 1;
        } else if (memcmp(chunk, "Code", 4) == 0 && chunk_size > 20) {
            required_chunks |= 2;
        } else if (memcmp(chunk, "ExpT", 4) == 0 && chunk_size >= 4) {
            required_chunks |= 4;
        } else if (memcmp(chunk, "ImpT", 4) == 0 && chunk_size >= 4) {
            required_chunks |= 8;
        } else if (memcmp(chunk, "StrT", 4) == 0) {
            required_chunks |= 16;
        }
        offset += 8 + padded_size;
    }
    *beam_size = (uint32_t) length;
    return required_chunks == 31;
}

bool rtems_app_validate(const void *data, size_t size, struct RtemsApp *app)
{
    static const char header[24] = "#!/usr/bin/env AtomVM\n";
    static const uint8_t footer[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'e', 'n', 'd', 0 };
    const uint8_t *bytes = data;
    struct RtemsApp candidate = { .data = data, .size = size };
    *app = (struct RtemsApp){ 0 };
    if (data == NULL || size < sizeof(header) + sizeof(footer) || size > RTEMS_APP_MAX_SIZE
        || memcmp(data, header, sizeof(header)) != 0) {
        return false;
    }

    size_t offset = sizeof(header);
    while (size - offset >= sizeof(footer)) {
        const uint8_t *section = bytes + offset;
        size_t section_size = read_be32(section);
        uint32_t flags = read_be32(section + 4);
        if (section_size == 0) {
            if (size - offset != sizeof(footer) || memcmp(section, footer, sizeof(footer)) != 0
                || candidate.startup_beam == NULL) {
                return false;
            }
            *app = candidate;
            return true;
        }
        if (flags == 0 || section_size < 16 || section_size % 4 != 0 || section_size > size - offset) {
            return false;
        }
        const uint8_t *name = section + 12;
        const uint8_t *name_end = memchr(name, 0, section_size - 12);
        if (name_end == NULL || name_end == name) {
            return false;
        }
        size_t name_size = ((size_t) (name_end - name) + 4) & ~(size_t) 3;
        if (name_size > section_size - 12) {
            return false;
        }
        const uint8_t *payload = name + name_size;
        uint32_t beam_size = 0;
        // PackBEAM flags: bit 0 = entry point, bit 1 = BEAM code.
        if ((flags & 3) != 0) {
            if ((flags & 2) == 0 || !valid_beam(payload, section_size - 12 - name_size, &beam_size)) {
                return false;
            }
        }
        if (flags & 1) {
            if (candidate.startup_beam != NULL) {
                return false;
            }
            candidate.startup_beam = payload;
            candidate.startup_beam_size = beam_size;
            candidate.startup_name = (const char *) name;
        }
        offset += section_size;
    }
    return false;
}

enum RtemsAppLoadResult rtems_app_load_file(const char *path, struct RtemsApp *app)
{
    *app = (struct RtemsApp){ 0 };
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        return errno == ENOENT || errno == ENOTDIR ? RTEMS_APP_NOT_FOUND : RTEMS_APP_IO_ERROR;
    }
    struct stat st;
    if (fstat(fileno(file), &st) != 0) {
        fclose(file);
        return RTEMS_APP_IO_ERROR;
    }
    if (!S_ISREG(st.st_mode) || st.st_size <= 0 || st.st_size > RTEMS_APP_MAX_SIZE) {
        fclose(file);
        return RTEMS_APP_INVALID;
    }
    size_t size = (size_t) st.st_size;
    void *data = malloc(size);
    if (data == NULL) {
        fclose(file);
        return RTEMS_APP_NO_MEMORY;
    }
    size_t count = fread(data, 1, size, file);
    int extra = fgetc(file);
    bool read_ok = count == size && extra == EOF && !ferror(file);
    int close_result = fclose(file);
    if (!read_ok || close_result != 0) {
        free(data);
        return RTEMS_APP_IO_ERROR;
    }
    if (!rtems_app_validate(data, size, app)) {
        free(data);
        return RTEMS_APP_INVALID;
    }
    return RTEMS_APP_LOADED;
}
