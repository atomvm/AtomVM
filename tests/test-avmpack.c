/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Davide Bettio <davide@uninstall.it>
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

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "avmpack.h"
#include "utils.h"

static int failures = 0;

#define CHECK(cond)                                                 \
    do {                                                            \
        if (!(cond)) {                                              \
            fprintf(stderr, "FAIL line %d: %s\n", __LINE__, #cond); \
            failures++;                                             \
        }                                                           \
    } while (0)

static const uint8_t avmpack_header[24] = { 0x23, 0x21, 0x2F, 0x75, 0x73, 0x72, 0x2F, 0x62, 0x69,
    0x6E, 0x2F, 0x65, 0x6E, 0x76, 0x20, 0x41, 0x74, 0x6F, 0x6D, 0x56, 0x4D, 0x0A, 0x00, 0x00 };

static void put_u32_be(uint8_t *p, uint32_t v)
{
    p[0] = v >> 24;
    p[1] = v >> 16;
    p[2] = v >> 8;
    p[3] = v;
}

static size_t pad4(size_t n)
{
    return (n + 3) & ~3;
}

static size_t put_section(
    uint8_t *buf, size_t off, const char *name, uint32_t flags, size_t content_size)
{
    size_t name_len = strlen(name) + 1;
    size_t name_pad = pad4(name_len);
    size_t content_pad = pad4(content_size);
    put_u32_be(buf + off, 12 + name_pad + content_pad);
    put_u32_be(buf + off + 4, flags);
    put_u32_be(buf + off + 8, 0);
    memcpy(buf + off + 12, name, name_len);
    memset(buf + off + 12 + name_len, 0, name_pad - name_len);
    memset(buf + off + 12 + name_pad, 0, content_pad);

    return off + 12 + name_pad + content_pad;
}

static void *count_sections(void *accum, const void *section_ptr, uint32_t section_size,
    const void *beam_ptr, uint32_t flags, const char *section_name)
{
    UNUSED(section_ptr);
    UNUSED(section_size);
    UNUSED(beam_ptr);
    UNUSED(flags);
    UNUSED(section_name);

    return (void *) ((uintptr_t) accum + 1);
}

static size_t put_end(uint8_t *buf, size_t off)
{
    put_u32_be(buf + off, 0);
    put_u32_be(buf + off + 4, 0);
    put_u32_be(buf + off + 8, 0);
    memcpy(buf + off + 12, "end", 4);

    return off + 16;
}

int main(void)
{
    uint8_t buf[4096];
    const void *ptr = NULL;
    uint32_t sz = 0;
    const char *name = NULL;

    // --- A valid pack: header + one BEAM section + terminator ---
    memset(buf, 0, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    size_t off = put_section(buf, 24, "mymod.beam", BEAM_START_FLAG, 16);
    uint32_t pack_size = put_end(buf, off);

    CHECK(avmpack_is_valid(buf, pack_size));
    CHECK(avmpack_find_section_by_name(buf, pack_size, "mymod.beam", &ptr, &sz) == 1);
    CHECK(ptr == buf + 24 + 12 + 12); // header + section header + padded "mymod.beam\0"
    CHECK(sz == 16); // the payload size, not the whole section size
    CHECK(avmpack_find_section_by_name(buf, pack_size, "missing", &ptr, &sz) == 0);
    CHECK(avmpack_find_section_by_flag(
              buf, pack_size, BEAM_START_FLAG, BEAM_START_FLAG, &ptr, &sz, &name)
        == 1);
    CHECK(name != NULL && strcmp(name, "mymod.beam") == 0);
    CHECK(sz == 16);
    // find_by_flag for flags 0 returns the terminator
    CHECK(avmpack_find_section_by_flag(
              buf, pack_size, END_OF_FILE_MASK, END_OF_FILE, &ptr, &sz, &name)
        == 1);
    CHECK(name != NULL && strcmp(name, "end") == 0);
    CHECK(ptr == buf + pack_size);

    // --- Sections after a section with flags 0 stay reachable ---
    memset(buf, 0, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    off = put_section(buf, 24, "app.beam", BEAM_START_FLAG, 16);
    off = put_section(buf, off, "app/priv/a.txt", 0, 8);
    off = put_section(buf, off, "app/priv/b.txt", 0, 8);
    pack_size = put_end(buf, off);
    CHECK(avmpack_find_section_by_name(buf, pack_size, "app/priv/b.txt", &ptr, &sz) == 1);
    CHECK(sz == 8);
    CHECK(avmpack_fold(NULL, buf, pack_size, count_sections) == (void *) (uintptr_t) 3);

    // --- Truncated pack: section present, terminator chopped off, 0xFF tail ---
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    put_section(buf, 24, "mymod.beam", BEAM_CODE_FLAG, 16);
    CHECK(avmpack_find_section_by_name(buf, sizeof(buf), "missing", &ptr, &sz) == 0);
    // a hit before the truncation is still reachable
    CHECK(avmpack_find_section_by_name(buf, sizeof(buf), "mymod.beam", &ptr, &sz) == 1);

    // --- Oversized section size ---
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    put_u32_be(buf + 24, 0x10000); // claims 64 KiB, far past the bounded view
    put_u32_be(buf + 24 + 4, 0);
    put_u32_be(buf + 24 + 8, 0);
    memcpy(buf + 24 + 12, "x", 2);
    CHECK(avmpack_find_section_by_name(buf, 64, "anything", &ptr, &sz) == 0);

    // --- 0x00-filled region must not be mistaken for the terminator ---
    memset(buf, 0, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    CHECK(avmpack_find_section_by_flag(
              buf, sizeof(buf), END_OF_FILE_MASK, END_OF_FILE, &ptr, &sz, &name)
        == 0);

    // --- Misaligned pack base (an in-place const binary may sit at any byte offset) ---
    _Alignas(uint32_t) uint8_t storage[512 + 1];
    uint8_t *pack = storage + 1;
    memset(storage, 0xFF, sizeof(storage));
    memcpy(pack, avmpack_header, 24);
    off = put_section(pack, 24, "mymod.beam", BEAM_START_FLAG, 16);
    pack_size = put_end(pack, off);
    CHECK(avmpack_find_section_by_name(pack, pack_size, "mymod.beam", &ptr, &sz) == 1 && sz == 16);
    CHECK(avmpack_find_section_by_flag(
              pack, pack_size, END_OF_FILE_MASK, END_OF_FILE, &ptr, &sz, &name)
        == 1);
    CHECK(avmpack_find_section_by_name(pack, pack_size, "missing", &ptr, &sz) == 0);

    // --- Section name with no NUL terminator inside the section ---
    memset(buf, 'A', sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    put_u32_be(buf + 24, 64); // in-range size, but the name region is all 'A'
    put_u32_be(buf + 24 + 4, 0);
    put_u32_be(buf + 24 + 8, 0);
    CHECK(avmpack_find_section_by_name(buf, 24 + 64 + 16, "x", &ptr, &sz) == 0);

    // --- Misaligned section size ---
    memset(buf, 0xFF, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    put_u32_be(buf + 24, 17);
    put_u32_be(buf + 24 + 4, 0);
    put_u32_be(buf + 24 + 8, 0);
    memcpy(buf + 24 + 12, "x", 2);
    CHECK(avmpack_find_section_by_name(buf, sizeof(buf), "x", &ptr, &sz) == 0);

    // --- Terminator name in any mix of case ---
    memset(buf, 0, sizeof(buf));
    memcpy(buf, avmpack_header, 24);
    off = put_section(buf, 24, "mymod.beam", BEAM_START_FLAG, 16);
    pack_size = put_end(buf, off);
    memcpy(buf + off + 12, "eNd", 4);
    CHECK(avmpack_find_section_by_flag(
              buf, pack_size, END_OF_FILE_MASK, END_OF_FILE, &ptr, &sz, &name)
        == 1);
    CHECK(name != NULL && strcmp(name, "eNd") == 0 && ptr == buf + pack_size);
    memcpy(buf + off + 12, "enx", 4);
    CHECK(avmpack_find_section_by_flag(
              buf, pack_size, END_OF_FILE_MASK, END_OF_FILE, &ptr, &sz, &name)
        == 0);

    if (failures == 0) {
        fprintf(stderr, "All avmpack tests passed!\n");
        return 0;
    }
    fprintf(stderr, "%d avmpack test(s) failed.\n", failures);

    return 1;
}
