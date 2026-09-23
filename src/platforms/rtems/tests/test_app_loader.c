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

#undef NDEBUG
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static uint32_t read_be32(const unsigned char *p)
{
    return ((uint32_t) p[0] << 24) | ((uint32_t) p[1] << 16) | ((uint32_t) p[2] << 8) | p[3];
}

static void write_be32(unsigned char *p, uint32_t n)
{
    p[0] = n >> 24;
    p[1] = n >> 16;
    p[2] = n >> 8;
    p[3] = n;
}

static void expect_invalid(const void *data, size_t size)
{
    struct RtemsApp app;
    memset(&app, 0xff, sizeof(app));
    assert(!rtems_app_validate(data, size, &app));
    assert(app.data == NULL && app.startup_beam == NULL && app.size == 0);
}

int main(int argc, char **argv)
{
    assert(argc == 2);
    struct RtemsApp app;
    assert(rtems_app_load_file(argv[1], &app) == RTEMS_APP_LOADED);
    assert(app.data && app.startup_name && app.startup_beam_size > 12);
    // The startup-flag corruption checks below modify the first section.
    assert(read_be32((const unsigned char *) app.data + 28) & 1);
    assert((const unsigned char *) app.startup_beam > (const unsigned char *) app.data);
    assert((const unsigned char *) app.startup_beam + app.startup_beam_size
        <= (const unsigned char *) app.data + app.size);

    // Every possible truncation of a real pack must be rejected, even when the
    // header is intact.
    for (size_t size = 0; size < app.size; ++size) {
        expect_invalid(app.data, size);
    }
    expect_invalid(NULL, 0);
    expect_invalid(app.data, RTEMS_APP_MAX_SIZE + 1);

    unsigned char *copy = malloc(app.size + 4);
    assert(copy);
    memcpy(copy, app.data, app.size);
    copy[0] ^= 1;
    expect_invalid(copy, app.size);
    memcpy(copy, app.data, app.size);
    memset(copy + app.size, 0, 4);
    expect_invalid(copy, app.size + 4);

    // Corrupt each section length and its name, not just the startup section.
    size_t offset = 24;
    while (offset < app.size - 16) {
        uint32_t section_size = read_be32((const unsigned char *) app.data + offset);
        assert(section_size >= 16);
        static const uint32_t bad_sizes[] = { 0, 1, 12, 17, UINT32_MAX, UINT32_MAX - 3 };
        for (size_t i = 0; i < sizeof(bad_sizes) / sizeof(bad_sizes[0]); ++i) {
            memcpy(copy, app.data, app.size);
            write_be32(copy + offset, bad_sizes[i]);
            expect_invalid(copy, app.size);
        }
        memcpy(copy, app.data, app.size);
        memset(copy + offset + 12, 'x', section_size - 12);
        expect_invalid(copy, app.size);
        offset += section_size;
    }
    assert(offset == app.size - 16);

    // A library-only pack cannot replace a runnable application.
    memcpy(copy, app.data, app.size);
    write_be32(copy + 28, 2);
    expect_invalid(copy, app.size);
    memcpy(copy, app.data, app.size);
    write_be32(copy + 28, 1); // startup flag without BEAM code flag
    expect_invalid(copy, app.size);
    memcpy(copy, app.data, app.size);
    write_be32(copy + app.size - 12, 1); // corrupt footer
    expect_invalid(copy, app.size);

    size_t beam_offset = (const unsigned char *) app.startup_beam - (const unsigned char *) app.data;
    memcpy(copy, app.data, app.size);
    write_be32(copy + beam_offset + 4, UINT32_MAX); // IFF length exceeds section
    expect_invalid(copy, app.size);
    memcpy(copy, app.data, app.size);
    write_be32(copy + beam_offset + 16, UINT32_MAX); // chunk length exceeds IFF
    expect_invalid(copy, app.size);

    // Exercise file errors and replacement using actual files.
    char directory[] = "/tmp/atomvm-app-loader-XXXXXX";
    assert(mkdtemp(directory));
    char path[256];
    assert(snprintf(path, sizeof(path), "%s/app.avm", directory) > 0);
    struct RtemsApp loaded;
    assert(rtems_app_load_file(path, &loaded) == RTEMS_APP_NOT_FOUND);
    assert(rtems_app_load_file(directory, &loaded) == RTEMS_APP_INVALID);
    FILE *file = fopen(path, "wb");
    assert(file && fclose(file) == 0);
    assert(rtems_app_load_file(path, &loaded) == RTEMS_APP_INVALID);
    file = fopen(path, "wb");
    assert(file && fwrite(app.data, 1, app.size - 1, file) == app.size - 1 && fclose(file) == 0);
    assert(rtems_app_load_file(path, &loaded) == RTEMS_APP_INVALID);
    file = fopen(path, "wb");
    assert(file && ftruncate(fileno(file), RTEMS_APP_MAX_SIZE + 1) == 0 && fclose(file) == 0);
    assert(rtems_app_load_file(path, &loaded) == RTEMS_APP_INVALID);
    file = fopen(path, "wb");
    assert(file && fwrite(app.data, 1, app.size, file) == app.size && fclose(file) == 0);
    assert(rtems_app_load_file(path, &loaded) == RTEMS_APP_LOADED);
    assert(strcmp(loaded.startup_name, app.startup_name) == 0);
    assert(loaded.size == app.size && memcmp(loaded.data, app.data, app.size) == 0);
    free((void *) loaded.data);
    assert(unlink(path) == 0 && rmdir(directory) == 0);
    free(copy);
    free((void *) app.data);
    puts("RTEMS app loader: valid pack, truncations, malformed containers and file replacement passed");
    return 0;
}
