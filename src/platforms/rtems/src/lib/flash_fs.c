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

#include "flash_fs.h"
#include "flash_driver.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include <rtems.h>
#include <rtems/jffs2.h>
#include <rtems/libio.h>

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <posix_nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

static bool s_fs_mounted = false;
static char s_mount_point[64] = { 0 };

static int jffs2_flash_read(
    rtems_jffs2_flash_control *self,
    uint32_t offset,
    unsigned char *buffer,
    size_t size_of_buffer)
{
    UNUSED(self);
    if (esp32c3_flash_read(flash_partition_fs_offset() + offset, buffer, size_of_buffer) != 0) {
        return -EIO;
    }
    return 0;
}

static int jffs2_flash_write(
    rtems_jffs2_flash_control *self,
    uint32_t offset,
    const unsigned char *buffer,
    size_t size_of_buffer)
{
    UNUSED(self);
    if (esp32c3_flash_write(flash_partition_fs_offset() + offset, buffer, size_of_buffer) != 0) {
        return -EIO;
    }
    return 0;
}

static int jffs2_flash_erase(
    rtems_jffs2_flash_control *self,
    uint32_t offset)
{
    UNUSED(self);
    uint32_t sector_num = (flash_partition_fs_offset() + offset) / FLASH_SECTOR_SIZE;
    if (esp32c3_flash_erase_sector(sector_num) != 0) {
        return -EIO;
    }
    return 0;
}

static struct
{
    rtems_jffs2_flash_control super;
} s_jffs2_flash_device = {
    .super = {
        .block_size = FLASH_SECTOR_SIZE,
        .flash_size = DEFAULT_FS_FLASH_SIZE,
        .write_size = 4,
        .read = jffs2_flash_read,
        .write = jffs2_flash_write,
        .erase = jffs2_flash_erase,
        .block_is_bad = NULL }
};

static rtems_jffs2_mount_data s_jffs2_mount_config = {
    .flash_control = &s_jffs2_flash_device.super,
    .compressor_control = NULL
};

bool esp32c3_flash_fs_mount(const char *mount_point)
{
    if (s_fs_mounted) {
        return true;
    }
    if (mount_point == NULL || strlen(mount_point) == 0) {
        mount_point = DEFAULT_FLASH_FS_MOUNT_POINT;
    }

    esp32c3_flash_init();

    mkdir(mount_point, 0755);

    int rv = mount(
        NULL,
        mount_point,
        RTEMS_FILESYSTEM_TYPE_JFFS2,
        RTEMS_FILESYSTEM_READ_WRITE,
        &s_jffs2_mount_config);

    if (rv != 0) {
        // If mounting failed, erase the partition and retry cleanly
        TRACE("Initial JFFS2 mount failed (%d); formatting partition...\n", errno);
        esp32c3_flash_erase_range(flash_partition_fs_offset(), DEFAULT_FS_FLASH_SIZE);

        rv = mount(
            NULL,
            mount_point,
            RTEMS_FILESYSTEM_TYPE_JFFS2,
            RTEMS_FILESYSTEM_READ_WRITE,
            &s_jffs2_mount_config);
    }

    if (rv == 0) {
        s_fs_mounted = true;
        strncpy(s_mount_point, mount_point, sizeof(s_mount_point) - 1);
        s_mount_point[sizeof(s_mount_point) - 1] = '\0';
        TRACE("JFFS2 filesystem mounted successfully at %s\n", mount_point);
        return true;
    }

    TRACE("Failed to mount JFFS2 filesystem at %s: error %d\n", mount_point, errno);
    return false;
}

bool esp32c3_flash_fs_unmount(const char *mount_point)
{
    if (!s_fs_mounted) {
        return true;
    }
    if (mount_point == NULL || strlen(mount_point) == 0) {
        mount_point = s_mount_point;
    }

    int rv = unmount(mount_point);
    if (rv == 0) {
        s_fs_mounted = false;
        s_mount_point[0] = '\0';
        return true;
    }
    return false;
}

bool esp32c3_flash_fs_is_mounted(void)
{
    return s_fs_mounted;
}

//
// NIF implementations
//

// esp:flash_fs_mount/0, atomvm_rtems:flash_fs_mount/0
static term nif_esp_flash_fs_mount_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    if (esp32c3_flash_fs_mount(DEFAULT_FLASH_FS_MOUNT_POINT)) {
        return OK_ATOM;
    }
    return ERROR_ATOM;
}

// esp:flash_fs_mount/1, atomvm_rtems:flash_fs_mount/1 (MountPoint)
static term nif_esp_flash_fs_mount_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    int ok;
    char *mount_point = interop_term_to_string(argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    bool res = esp32c3_flash_fs_mount(mount_point);
    free(mount_point);

    return res ? OK_ATOM : ERROR_ATOM;
}

// esp:flash_fs_unmount/0, atomvm_rtems:flash_fs_unmount/0
static term nif_esp_flash_fs_unmount_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    if (esp32c3_flash_fs_unmount(NULL)) {
        return OK_ATOM;
    }
    return ERROR_ATOM;
}

// esp:flash_fs_unmount/1, atomvm_rtems:flash_fs_unmount/1 (MountPoint)
static term nif_esp_flash_fs_unmount_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    int ok;
    char *mount_point = interop_term_to_string(argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    bool res = esp32c3_flash_fs_unmount(mount_point);
    free(mount_point);

    return res ? OK_ATOM : ERROR_ATOM;
}

// esp:flash_fs_is_mounted/0, atomvm_rtems:flash_fs_is_mounted/0
static term nif_esp_flash_fs_is_mounted(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    return esp32c3_flash_fs_is_mounted() ? TRUE_ATOM : FALSE_ATOM;
}

// file:read_file/1, atomvm_rtems:read_file/1, esp:read_file/1
static term nif_file_read_file(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    int ok;
    char *path = interop_term_to_string(argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    FILE *f = fopen(path, "rb");
    if (f == NULL) {
        int err = errno;
        free(path);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(err_tuple, 1, posix_errno_to_term(err, ctx->global));
        return err_tuple;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size < 0) {
        fclose(f);
        free(path);
        return ERROR_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size((size_t) size) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        fclose(f);
        free(path);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term binary = term_create_uninitialized_binary((size_t) size, &ctx->heap, ctx->global);
    void *buf = (void *) term_binary_data(binary);
    size_t read_bytes = fread(buf, 1, (size_t) size, f);
    fclose(f);
    free(path);

    if (read_bytes != (size_t) size) {
        return ERROR_ATOM;
    }

    term res = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(res, 0, OK_ATOM);
    term_put_tuple_element(res, 1, binary);
    return res;
}

// file:write_file/2, atomvm_rtems:write_file/2, esp:write_file/2
static term nif_file_write_file(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[1], term_is_binary);

    int ok;
    char *path = interop_term_to_string(argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    FILE *f = fopen(path, "wb");
    if (f == NULL) {
        int err = errno;
        free(path);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term err_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(err_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(err_tuple, 1, posix_errno_to_term(err, ctx->global));
        return err_tuple;
    }

    term binary = argv[1];
    size_t size = term_binary_size(binary);
    const void *data = (const void *) term_binary_data(binary);

    size_t written = fwrite(data, 1, size, f);
    fclose(f);
    free(path);

    if (written != size) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static const struct Nif esp_flash_fs_mount_0_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_fs_mount_0
};
static const struct Nif esp_flash_fs_mount_1_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_fs_mount_1
};
static const struct Nif esp_flash_fs_unmount_0_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_fs_unmount_0
};
static const struct Nif esp_flash_fs_unmount_1_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_fs_unmount_1
};
static const struct Nif esp_flash_fs_is_mounted_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_fs_is_mounted
};
static const struct Nif file_read_file_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_file_read_file
};
static const struct Nif file_write_file_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_file_write_file
};

void flash_fs_nif_init(GlobalContext *global)
{
    UNUSED(global);
}

const struct Nif *flash_fs_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:flash_fs_mount/0", nifname) == 0 || strcmp("atomvm_rtems:flash_fs_mount/0", nifname) == 0) {
        return &esp_flash_fs_mount_0_nif;
    }
    if (strcmp("esp:flash_fs_mount/1", nifname) == 0 || strcmp("atomvm_rtems:flash_fs_mount/1", nifname) == 0) {
        return &esp_flash_fs_mount_1_nif;
    }
    if (strcmp("esp:flash_fs_unmount/0", nifname) == 0 || strcmp("atomvm_rtems:flash_fs_unmount/0", nifname) == 0) {
        return &esp_flash_fs_unmount_0_nif;
    }
    if (strcmp("esp:flash_fs_unmount/1", nifname) == 0 || strcmp("atomvm_rtems:flash_fs_unmount/1", nifname) == 0) {
        return &esp_flash_fs_unmount_1_nif;
    }
    if (strcmp("esp:flash_fs_is_mounted/0", nifname) == 0 || strcmp("atomvm_rtems:flash_fs_is_mounted/0", nifname) == 0) {
        return &esp_flash_fs_is_mounted_nif;
    }
    if (strcmp("file:read_file/1", nifname) == 0 || strcmp("atomvm_rtems:read_file/1", nifname) == 0 || strcmp("esp:read_file/1", nifname) == 0) {
        return &file_read_file_nif;
    }
    if (strcmp("file:write_file/2", nifname) == 0 || strcmp("atomvm_rtems:write_file/2", nifname) == 0 || strcmp("esp:write_file/2", nifname) == 0) {
        return &file_write_file_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(flash_fs, flash_fs_nif_init, NULL, flash_fs_nif_get_nif)
