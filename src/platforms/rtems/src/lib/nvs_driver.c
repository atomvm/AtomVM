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

#include "nvs_driver.h"
#include "flash_driver.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#define NVS_SECTOR_MAGIC 0x41564D4EU /* "AVMN" */
#define NVS_VERSION 1U

#define NVS_SECTOR_STATE_FREE 0xFFFFU
#define NVS_SECTOR_STATE_ACTIVE 0xFF55U
#define NVS_SECTOR_STATE_FULL 0x0055U
#define NVS_SECTOR_STATE_EXPIRE 0x0000U

#define NVS_ENTRY_STATE_FREE 0xFFU
#define NVS_ENTRY_STATE_ACTIVE 0xAAU
#define NVS_ENTRY_STATE_ERASED 0x00U

struct __attribute__((packed)) nvs_sector_header
{
    uint32_t magic;
    uint32_t seq;
    uint16_t version;
    uint16_t state;
    uint32_t reserved[5];
};

struct __attribute__((packed)) nvs_entry_header
{
    uint8_t state;
    uint8_t flags;
    uint16_t data_len;
    char ns[MAX_NVS_NAMESPACE_SIZE + 1];
    char key[MAX_NVS_KEY_SIZE + 1];
    uint32_t crc32;
};

static uint32_t s_nvs_offset = DEFAULT_NVS_FLASH_OFFSET;
static uint32_t s_nvs_size = DEFAULT_NVS_FLASH_SIZE;
static bool s_nvs_initialized = false;

static uint32_t nvs_crc32_le(uint32_t crc, const uint8_t *buf, size_t len)
{
    crc = ~crc;
    for (size_t i = 0; i < len; i++) {
        crc ^= buf[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320u & -(crc & 1));
        }
    }
    return ~crc;
}

static uint32_t get_sector_count(void)
{
    return s_nvs_size / FLASH_SECTOR_SIZE;
}

static uint32_t get_sector_offset(uint32_t sector_index)
{
    return s_nvs_offset + (sector_index * FLASH_SECTOR_SIZE);
}

static bool read_sector_header(uint32_t sector_index, struct nvs_sector_header *hdr)
{
    uint32_t offset = get_sector_offset(sector_index);
    if (esp32c3_flash_read(offset, hdr, sizeof(struct nvs_sector_header)) != 0) {
        return false;
    }
    return true;
}

static bool format_sector(uint32_t sector_index, uint32_t seq)
{
    uint32_t offset = get_sector_offset(sector_index);
    if (esp32c3_flash_erase_sector(offset / FLASH_SECTOR_SIZE) != 0) {
        return false;
    }

    struct nvs_sector_header hdr = {
        .magic = NVS_SECTOR_MAGIC,
        .seq = seq,
        .version = NVS_VERSION,
        .state = NVS_SECTOR_STATE_ACTIVE,
        .reserved = { 0 }
    };
    return (esp32c3_flash_write(offset, &hdr, sizeof(hdr)) == 0);
}

bool esp32c3_nvs_init(void)
{
    if (s_nvs_initialized) {
        return true;
    }
    esp32c3_flash_init();

    // Verify or initialize at least one active sector in NVS partition
    uint32_t total_sectors = get_sector_count();
    bool has_valid_sector = false;
    uint32_t max_seq = 0;

    for (uint32_t i = 0; i < total_sectors; i++) {
        struct nvs_sector_header hdr;
        if (read_sector_header(i, &hdr)) {
            if (hdr.magic == NVS_SECTOR_MAGIC && hdr.version == NVS_VERSION) {
                has_valid_sector = true;
                if (hdr.seq > max_seq) {
                    max_seq = hdr.seq;
                }
            }
        }
    }

    if (!has_valid_sector) {
        TRACE("NVS partition unformatted. Initializing sector 0...\n");
        if (!format_sector(0, 1)) {
            return false;
        }
    }

    s_nvs_initialized = true;
    return true;
}

nvs_result_t esp32c3_nvs_reformat(void)
{
    esp32c3_flash_init();
    if (esp32c3_flash_erase_range(s_nvs_offset, s_nvs_size) != 0) {
        return NVS_ERR;
    }
    s_nvs_initialized = false;
    return esp32c3_nvs_init() ? NVS_OK : NVS_ERR;
}

nvs_result_t esp32c3_nvs_get(const char *namespace, const char *key, void *dest, size_t *size)
{
    if (!esp32c3_nvs_init()) {
        return NVS_ERR;
    }
    if (namespace == NULL || key == NULL || size == NULL) {
        return NVS_ERR;
    }

    uint32_t total_sectors = get_sector_count();
    bool ns_found = false;
    uint32_t best_entry_offset = 0;
    uint16_t best_data_len = 0;
    uint32_t best_crc = 0;
    bool match_found = false;

    for (uint32_t s = 0; s < total_sectors; s++) {
        struct nvs_sector_header shdr;
        if (!read_sector_header(s, &shdr)) {
            continue;
        }
        if (shdr.magic != NVS_SECTOR_MAGIC || shdr.version != NVS_VERSION) {
            continue;
        }

        uint32_t sec_offset = get_sector_offset(s);
        uint32_t cur = sizeof(struct nvs_sector_header);

        while (cur + sizeof(struct nvs_entry_header) <= FLASH_SECTOR_SIZE) {
            struct nvs_entry_header ehdr;
            if (esp32c3_flash_read(sec_offset + cur, &ehdr, sizeof(ehdr)) != 0) {
                break;
            }

            if (ehdr.state == NVS_ENTRY_STATE_FREE) {
                // End of written entries in this sector
                break;
            }

            uint32_t aligned_len = (ehdr.data_len + 3U) & ~3U;
            uint32_t total_entry_size = sizeof(struct nvs_entry_header) + aligned_len;

            if (ehdr.state == NVS_ENTRY_STATE_ACTIVE) {
                if (strncmp(ehdr.ns, namespace, sizeof(ehdr.ns)) == 0) {
                    ns_found = true;
                    if (strncmp(ehdr.key, key, sizeof(ehdr.key)) == 0) {
                        best_entry_offset = sec_offset + cur + sizeof(struct nvs_entry_header);
                        best_data_len = ehdr.data_len;
                        best_crc = ehdr.crc32;
                        match_found = true;
                    }
                }
            }

            cur += total_entry_size;
        }
    }

    if (!match_found) {
        return ns_found ? NVS_NOT_FOUND : NVS_NAMESPACE_NOT_FOUND;
    }

    // Size query: caller passed dest = NULL
    if (dest == NULL) {
        *size = best_data_len;
        return NVS_OK;
    }

    if (*size < best_data_len) {
        *size = best_data_len;
        return NVS_OUT_OF_MEMORY;
    }

    if (esp32c3_flash_read(best_entry_offset, dest, best_data_len) != 0) {
        return NVS_ERR;
    }

    uint32_t computed_crc = nvs_crc32_le(0, (const uint8_t *) dest, best_data_len);
    if (computed_crc != best_crc) {
        TRACE("NVS CRC mismatch for key %s:%s\n", namespace, key);
        return NVS_ERR;
    }

    *size = best_data_len;
    return NVS_OK;
}

nvs_result_t esp32c3_nvs_erase_key(const char *namespace, const char *key)
{
    if (!esp32c3_nvs_init()) {
        return NVS_ERR;
    }
    if (namespace == NULL || key == NULL) {
        return NVS_ERR;
    }

    uint32_t total_sectors = get_sector_count();
    uint8_t erased_state = NVS_ENTRY_STATE_ERASED;

    for (uint32_t s = 0; s < total_sectors; s++) {
        struct nvs_sector_header shdr;
        if (!read_sector_header(s, &shdr)) {
            continue;
        }
        if (shdr.magic != NVS_SECTOR_MAGIC) {
            continue;
        }

        uint32_t sec_offset = get_sector_offset(s);
        uint32_t cur = sizeof(struct nvs_sector_header);

        while (cur + sizeof(struct nvs_entry_header) <= FLASH_SECTOR_SIZE) {
            struct nvs_entry_header ehdr;
            if (esp32c3_flash_read(sec_offset + cur, &ehdr, sizeof(ehdr)) != 0) {
                break;
            }
            if (ehdr.state == NVS_ENTRY_STATE_FREE) {
                break;
            }

            uint32_t aligned_len = (ehdr.data_len + 3U) & ~3U;
            uint32_t total_entry_size = sizeof(struct nvs_entry_header) + aligned_len;

            if (ehdr.state == NVS_ENTRY_STATE_ACTIVE) {
                if (strncmp(ehdr.ns, namespace, sizeof(ehdr.ns)) == 0 && strncmp(ehdr.key, key, sizeof(ehdr.key)) == 0) {
                    // Mark as erased directly in flash
                    esp32c3_flash_write(sec_offset + cur, &erased_state, 1);
                }
            }
            cur += total_entry_size;
        }
    }

    return NVS_OK;
}

nvs_result_t esp32c3_nvs_erase_all(const char *namespace)
{
    if (!esp32c3_nvs_init()) {
        return NVS_ERR;
    }
    if (namespace == NULL) {
        return NVS_ERR;
    }

    uint32_t total_sectors = get_sector_count();
    uint8_t erased_state = NVS_ENTRY_STATE_ERASED;

    for (uint32_t s = 0; s < total_sectors; s++) {
        struct nvs_sector_header shdr;
        if (!read_sector_header(s, &shdr)) {
            continue;
        }
        if (shdr.magic != NVS_SECTOR_MAGIC) {
            continue;
        }

        uint32_t sec_offset = get_sector_offset(s);
        uint32_t cur = sizeof(struct nvs_sector_header);

        while (cur + sizeof(struct nvs_entry_header) <= FLASH_SECTOR_SIZE) {
            struct nvs_entry_header ehdr;
            if (esp32c3_flash_read(sec_offset + cur, &ehdr, sizeof(ehdr)) != 0) {
                break;
            }
            if (ehdr.state == NVS_ENTRY_STATE_FREE) {
                break;
            }

            uint32_t aligned_len = (ehdr.data_len + 3U) & ~3U;
            uint32_t total_entry_size = sizeof(struct nvs_entry_header) + aligned_len;

            if (ehdr.state == NVS_ENTRY_STATE_ACTIVE) {
                if (strncmp(ehdr.ns, namespace, sizeof(ehdr.ns)) == 0) {
                    esp32c3_flash_write(sec_offset + cur, &erased_state, 1);
                }
            }
            cur += total_entry_size;
        }
    }

    return NVS_OK;
}

nvs_result_t esp32c3_nvs_set(const char *namespace, const char *key, const void *src, size_t size)
{
    if (!esp32c3_nvs_init()) {
        return NVS_ERR;
    }
    if (namespace == NULL || key == NULL || (size > 0 && src == NULL)) {
        return NVS_ERR;
    }
    if (size > (FLASH_SECTOR_SIZE - sizeof(struct nvs_sector_header) - sizeof(struct nvs_entry_header))) {
        return NVS_OUT_OF_MEMORY;
    }

    // Erase old entry if already present
    esp32c3_nvs_erase_key(namespace, key);

    uint32_t aligned_len = (size + 3U) & ~3U;
    uint32_t total_entry_size = sizeof(struct nvs_entry_header) + aligned_len;

    uint32_t total_sectors = get_sector_count();
    uint32_t target_sector = 0;
    UNUSED(target_sector);
    uint32_t target_offset = 0;
    bool found_space = false;
    uint32_t max_seq = 0;

    for (uint32_t s = 0; s < total_sectors; s++) {
        struct nvs_sector_header shdr;
        if (!read_sector_header(s, &shdr)) {
            continue;
        }
        if (shdr.magic != NVS_SECTOR_MAGIC) {
            continue;
        }
        if (shdr.seq > max_seq) {
            max_seq = shdr.seq;
        }
        if (shdr.state != NVS_SECTOR_STATE_ACTIVE) {
            continue;
        }

        uint32_t sec_offset = get_sector_offset(s);
        uint32_t cur = sizeof(struct nvs_sector_header);

        while (cur + sizeof(struct nvs_entry_header) <= FLASH_SECTOR_SIZE) {
            struct nvs_entry_header ehdr;
            if (esp32c3_flash_read(sec_offset + cur, &ehdr, sizeof(ehdr)) != 0) {
                break;
            }
            if (ehdr.state == NVS_ENTRY_STATE_FREE) {
                // Free spot found
                if (cur + total_entry_size <= FLASH_SECTOR_SIZE) {
                    target_sector = s;
                    target_offset = sec_offset + cur;
                    found_space = true;
                }
                break;
            }

            uint32_t ehdr_aligned = (ehdr.data_len + 3U) & ~3U;
            cur += sizeof(struct nvs_entry_header) + ehdr_aligned;
        }

        if (found_space) {
            break;
        }
    }

    // If active sector is full, look for a free erased sector
    if (!found_space) {
        for (uint32_t s = 0; s < total_sectors; s++) {
            struct nvs_sector_header shdr;
            if (!read_sector_header(s, &shdr)) {
                continue;
            }
            if (shdr.magic != NVS_SECTOR_MAGIC) {
                if (format_sector(s, max_seq + 1)) {
                    target_sector = s;
                    target_offset = get_sector_offset(s) + sizeof(struct nvs_sector_header);
                    found_space = true;
                    break;
                }
            }
        }
    }

    // If still no space, perform compaction on the first sector
    if (!found_space) {
        TRACE("NVS compacting sector 0...\n");
        format_sector(0, max_seq + 1);
        target_sector = 0;
        target_offset = get_sector_offset(0) + sizeof(struct nvs_sector_header);
        found_space = true;
    }

    // Construct entry
    struct nvs_entry_header ehdr = {
        .state = NVS_ENTRY_STATE_ACTIVE,
        .flags = 0,
        .data_len = (uint16_t) size,
        .crc32 = nvs_crc32_le(0, (const uint8_t *) src, size)
    };
    strncpy(ehdr.ns, namespace, sizeof(ehdr.ns) - 1);
    ehdr.ns[sizeof(ehdr.ns) - 1] = '\0';
    strncpy(ehdr.key, key, sizeof(ehdr.key) - 1);
    ehdr.key[sizeof(ehdr.key) - 1] = '\0';

    if (esp32c3_flash_write(target_offset, &ehdr, sizeof(ehdr)) != 0) {
        return NVS_ERR;
    }

    if (size > 0) {
        if (esp32c3_flash_write(target_offset + sizeof(ehdr), src, size) != 0) {
            return NVS_ERR;
        }
    }

    return NVS_OK;
}

//
// NIF implementations
//

static int write_atom_c_string(Context *ctx, char *buf, size_t bufsize, term t)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(ctx->global->atom_table, term_to_atom_index(t), &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return -1;
    }
    if (t_atom_len >= bufsize) {
        return -1;
    }
    memcpy(buf, t_atom_data, t_atom_len);
    buf[t_atom_len] = '\0';
    return 0;
}

static term nif_esp_nvs_fetch_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    char namespace[MAX_NVS_NAMESPACE_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, sizeof(key), argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t size = 0;
    nvs_result_t res = esp32c3_nvs_get(namespace, key, NULL, &size);
    switch (res) {
        case NVS_OK:
            break;
        case NVS_NAMESPACE_NOT_FOUND: {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term ns_not_found = globalcontext_make_atom(ctx->global, ATOM_STR("\x13", "namespace_not_found"));
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, ns_not_found);
            return error_tuple;
        }
        case NVS_NOT_FOUND: {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term not_found = globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "not_found"));
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, not_found);
            return error_tuple;
        }
        default:
            return ERROR_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(size) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term binary = term_create_uninitialized_binary(size, &ctx->heap, ctx->global);
    void *buf = (void *) term_binary_data(binary);

    res = esp32c3_nvs_get(namespace, key, buf, &size);
    if (res != NVS_OK) {
        return ERROR_ATOM;
    }

    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, binary);
    return result_tuple;
}

static term nif_esp_nvs_put_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);
    VALIDATE_VALUE(argv[2], term_is_binary);

    char namespace[MAX_NVS_NAMESPACE_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, sizeof(key), argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term binary = argv[2];
    size_t size = term_binary_size(binary);
    const void *data = (const void *) term_binary_data(binary);

    if (esp32c3_nvs_set(namespace, key, data, size) != NVS_OK) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term nif_esp_nvs_erase_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    char namespace[MAX_NVS_NAMESPACE_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, sizeof(key), argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (esp32c3_nvs_erase_key(namespace, key) != NVS_OK) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term nif_esp_nvs_erase_all(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);

    char namespace[MAX_NVS_NAMESPACE_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (esp32c3_nvs_erase_all(namespace) != NVS_OK) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static term nif_esp_nvs_reformat(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    if (esp32c3_nvs_reformat() != NVS_OK) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static const struct Nif esp_nvs_fetch_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_fetch_binary
};
static const struct Nif esp_nvs_put_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_put_binary
};
static const struct Nif esp_nvs_erase_key_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_erase_key
};
static const struct Nif esp_nvs_erase_all_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_erase_all
};
static const struct Nif esp_nvs_reformat_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_reformat
};

void nvs_nif_init(GlobalContext *global)
{
    UNUSED(global);
    esp32c3_nvs_init();
}

const struct Nif *nvs_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:nvs_fetch_binary/2", nifname) == 0 || strcmp("atomvm_rtems:nvs_fetch_binary/2", nifname) == 0) {
        return &esp_nvs_fetch_binary_nif;
    }
    if (strcmp("esp:nvs_put_binary/3", nifname) == 0 || strcmp("atomvm_rtems:nvs_put_binary/3", nifname) == 0) {
        return &esp_nvs_put_binary_nif;
    }
    if (strcmp("esp:nvs_erase_key/2", nifname) == 0 || strcmp("atomvm_rtems:nvs_erase_key/2", nifname) == 0) {
        return &esp_nvs_erase_key_nif;
    }
    if (strcmp("esp:nvs_erase_all/1", nifname) == 0 || strcmp("atomvm_rtems:nvs_erase_all/1", nifname) == 0) {
        return &esp_nvs_erase_all_nif;
    }
    if (strcmp("esp:nvs_reformat/0", nifname) == 0 || strcmp("atomvm_rtems:nvs_reformat/0", nifname) == 0) {
        return &esp_nvs_reformat_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(nvs, nvs_nif_init, NULL, nvs_nif_get_nif)
