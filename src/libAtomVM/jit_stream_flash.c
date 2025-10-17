/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 by Paul Guyot <pguyot@kallisys.net>
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

#include "jit_stream_flash.h"

#include "avmpack.h"
#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "jit.h"
#include "module.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "sys.h"
#include "term.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define ENABLE_TRACE
#include "trace.h"

#define JIT_ENTRY_MAGIC 0x4A74

#ifdef ENABLE_TRACE
// Simple CRC32 for verification
static uint32_t crc32(const uint8_t *data, size_t len)
{
    uint32_t crc = 0xFFFFFFFF;
    for (size_t i = 0; i < len; i++) {
        crc ^= data[i];
        for (int j = 0; j < 8; j++) {
            crc = (crc >> 1) ^ (0xEDB88320 & -(crc & 1));
        }
    }
    return ~crc;
}
#endif

/**
 * @brief JIT entry header stored in flash
 *
 * Each compiled module has an entry with this header followed by the native code.
 */
struct JITEntry
{
    uint16_t magic; ///< Magic number (JIT_ENTRY_MAGIC) or 0xFFFF for free space
    uint16_t version; ///< Module version
    uint32_t code; ///< Pointer to original BEAM code (32-bit for flash storage)
    uint32_t labels; ///< Number of labels
    uint32_t size; ///< Size of native code in bytes
} __attribute__((packed));

_Static_assert(sizeof(struct JITEntry) == 16, "sizeof(struct JITEntry) must be 16");

/**
 * @brief JIT stream flash state
 *
 * Maintains the state for writing JIT code to flash with page buffering.
 */
struct JITStreamFlash
{
    struct JITEntry *jit_entry; ///< Pointer to current JIT entry in flash
    uintptr_t page_base_addr; ///< Base address of current page
    uint8_t page_buffer[FLASH_PAGE_SIZE]; ///< Page buffer for writing
    uint8_t page_offset; ///< Current offset within page
    struct JSFlashPlatformContext *pf_ctx; ///< Platform-specific context
};

static ErlNifResourceType *jit_stream_flash_resource_type;
static void jit_stream_flash_dtor(ErlNifEnv *caller_env, void *obj);
static bool jit_stream_flash_replace_at_addr(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data, size_t len);

const ErlNifResourceTypeInit jit_stream_flash_resource_type_init = {
    .members = 1,
    .dtor = jit_stream_flash_dtor
};

static struct JITEntry *jit_entry_next(struct JITEntry *jit_entry)
{
    uintptr_t next_entry_addr = ((uintptr_t) jit_entry) + sizeof(struct JITEntry) + jit_entry->size;
    // Align to 4 bytes boundaries
    next_entry_addr = (next_entry_addr + 3) & ~3;

    TRACE("jit_entry_next: jit_entry = %p, return %p\n", (void *) jit_entry, (void *) next_entry_addr);

    return (struct JITEntry *) next_entry_addr;
}

/**
 * @brief Check if a sector needs to be erased
 *
 * Scans the entire sector to check if it contains any non-0xFF bytes.
 * Uses word-by-word comparison for efficiency since sectors are aligned.
 *
 * @param sector_addr Address of the sector (must be sector-aligned)
 * @return true if sector needs erasing, false if already erased
 */
static bool jit_stream_flash_sector_needs_erase(uintptr_t sector_addr)
{
    const uintptr_t *sector_ptr = (const uintptr_t *) sector_addr;
    const uintptr_t erased_pattern = ~((uintptr_t) 0); // All bits set to 1 (0xFF...FF)
    size_t num_words = FLASH_SECTOR_SIZE / sizeof(uintptr_t);

    // Check if entire sector is all 0xFF by comparing word-by-word
    for (size_t i = 0; i < num_words; i++) {
        if (sector_ptr[i] != erased_pattern) {
            return true;
        }
    }

    return false;
}

static struct JITEntry *globalcontext_find_first_jit_entry(GlobalContext *global, bool *is_valid)
{
    const void *max_end_offset = NULL;
    uint32_t end_size;
    const void *end_offset;
    const char *end_name;
    bool valid_cache = true;

    struct ListHead *item;
    struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        avmpack_find_section_by_flag(avmpack_data->data, END_OF_FILE_MASK, END_OF_FILE, &end_offset, &end_size, &end_name);
        valid_cache = valid_cache && (strcmp(end_name, "END") == 0);

        if (end_offset > max_end_offset) {
            max_end_offset = end_offset;
        }
    }
    synclist_unlock(&global->avmpack_data);

    uintptr_t max_end_offset_page = ((((uintptr_t) max_end_offset) - 1) & ~(FLASH_SECTOR_SIZE - 1));
    *is_valid = valid_cache;

    TRACE("globalcontext_find_first_jit_entry: return %p\n", (void *) (max_end_offset_page + FLASH_SECTOR_SIZE));

    return (struct JITEntry *) (max_end_offset_page + FLASH_SECTOR_SIZE);
}

static void globalcontext_set_cache_valid(GlobalContext *global)
{
    TRACE("globalcontext_set_cache_valid\n");

    uint32_t end_size;
    const void *end_offset;
    const char *end_name;
    bool valid_cache;

    // Create platform context for flash operations
    struct JSFlashPlatformContext *pf_ctx = jit_stream_flash_platform_init();
    if (IS_NULL_PTR(pf_ctx)) {
        fprintf(stderr, "Failed to initialize platform flash context\n");
        return;
    }

    do {
        valid_cache = true;
        struct ListHead *item;
        struct ListHead *avmpack_data = synclist_rdlock(&global->avmpack_data);
        LIST_FOR_EACH (item, avmpack_data) {
            struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
            avmpack_find_section_by_flag(avmpack_data->data, END_OF_FILE_MASK, END_OF_FILE, &end_offset, &end_size, &end_name);
            if (strcmp(end_name, "END")) {
                valid_cache = false;
                break;
            }
        }
        synclist_unlock(&global->avmpack_data);
        if (!valid_cache) {
            // Replace "end" with "END" - this is a 3-byte string replacement
            const uint8_t end_str[] = "END";
            if (!jit_stream_flash_replace_at_addr(pf_ctx, (uintptr_t) end_name, end_str, 3)) {
                fprintf(stderr, "Failed to update cache validity marker from 'end' to 'END'\n");
                break;
            }
        }
    } while (!valid_cache);

    jit_stream_flash_platform_destroy(pf_ctx);
}

static struct JITEntry *globalcontext_find_last_jit_entry(GlobalContext *global)
{
    bool is_valid;
    struct JITEntry *jit_entry = globalcontext_find_first_jit_entry(global, &is_valid);
    if (!is_valid) {
        TRACE("globalcontext_find_last_jit_entry, cache not valid, returning NULL\n");
        return NULL;
    }

    // Find the last valid entry
    struct JITEntry *last_valid = jit_entry;
    while (jit_entry->magic == JIT_ENTRY_MAGIC) {
        last_valid = jit_entry;
        jit_entry = jit_entry_next(jit_entry);
    }
    TRACE("globalcontext_find_last_jit_entry, returning last valid entry at %p\n", (void *) last_valid);
    return last_valid;
}

static bool jit_stream_flash_flush_page(struct JITStreamFlash *js)
{
    // Write the page
    // Note: sector is already erased by nif_jit_stream_flash_new (first sector)
    // or jit_stream_flash_append (subsequent sectors when crossing boundaries)
    if (!jit_stream_flash_platform_write_page(js->pf_ctx, js->page_base_addr, js->page_buffer)) {
        fprintf(stderr, "Failed to write page at address 0x%lx\n", (unsigned long) js->page_base_addr);
        return false;
    }

    return true;
}

static bool jit_stream_flash_finalize_entry(struct JSFlashPlatformContext *pf_ctx, struct JITEntry *jit_entry, uint16_t magic, uint16_t version, uint32_t code, uint32_t labels)
{
    uintptr_t entry_addr = (uintptr_t) jit_entry;
    uint8_t page_buffer[FLASH_PAGE_SIZE];
    uintptr_t page_base_addr = entry_addr & ~(FLASH_PAGE_SIZE - 1);

    // Read current page contents
    memcpy(page_buffer, (const uint8_t *) page_base_addr, FLASH_PAGE_SIZE);

    // Calculate offset within page
    size_t entry_offset = entry_addr - page_base_addr;
    struct JITEntry *updated_entry = (struct JITEntry *) (page_buffer + entry_offset);

    // Update fields
    updated_entry->magic = magic;
    updated_entry->version = version;
    updated_entry->code = code;
    updated_entry->labels = labels;

    // Write back to flash
    if (!jit_stream_flash_platform_write_page(pf_ctx, page_base_addr, page_buffer)) {
        fprintf(stderr, "Failed to finalize entry at address 0x%lx\n", (unsigned long) page_base_addr);
        return false;
    }

    return true;
}

// Replace data in flash at the given absolute address
// Returns true on success, false if validation fails (trying to set bits 0->1)
static bool jit_stream_flash_replace_at_addr(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data, size_t len)
{
    uintptr_t replace_start = addr;
    uintptr_t replace_end = replace_start + len;

    // Iterate over all pages that need to be updated
    uintptr_t current_page_addr = replace_start & ~(FLASH_PAGE_SIZE - 1);
    size_t data_offset = 0;

    while (current_page_addr < replace_end) {
        // Calculate the range within this page that needs to be replaced
        uintptr_t page_start_offset = 0;
        uintptr_t page_end_offset = FLASH_PAGE_SIZE;

        if (current_page_addr < replace_start) {
            page_start_offset = replace_start - current_page_addr;
        }

        if (current_page_addr + FLASH_PAGE_SIZE > replace_end) {
            page_end_offset = replace_end - current_page_addr;
        }

        size_t copy_len = page_end_offset - page_start_offset;

        // Prepare page buffer
        uint8_t page_buffer[FLASH_PAGE_SIZE];
        uintptr_t page_base_addr = current_page_addr;

        // Read current page contents
        memcpy(page_buffer, (const uint8_t *) page_base_addr, FLASH_PAGE_SIZE);

        // Verify that we're only clearing bits (1->0), not setting them (0->1)
        const uint8_t *flash_ptr = (const uint8_t *) page_base_addr;
        for (size_t i = 0; i < copy_len; i++) {
            uint8_t flash_byte = flash_ptr[page_start_offset + i];
            uint8_t new_byte = data[data_offset + i];

            // Check if we're trying to set any bits (0->1)
            if ((new_byte & ~flash_byte) != 0) {
                fprintf(stderr, "\n=== FLASH REPLACE VALIDATION FAILED ===\n");
                fprintf(stderr, "Attempting to set bits (0->1) without erase!\n");
                fprintf(stderr, "Page address: 0x%lx\n", (unsigned long) page_base_addr);
                fprintf(stderr, "Offset in page: %zu, flash byte: 0x%02x, new byte: 0x%02x\n",
                    page_start_offset + i, (int) flash_byte, (int) new_byte);
                fprintf(stderr, "Bits being set (0->1): 0x%02x\n", (int) (new_byte & ~flash_byte));
                fprintf(stderr, "Replace address: 0x%lx, len=%zu\n", (unsigned long) addr, len);
                fprintf(stderr, "========================================\n\n");
                return false;
            }
        }

        // Update with new data
        memcpy(page_buffer + page_start_offset, data + data_offset, copy_len);

        // Write back to flash
        if (!jit_stream_flash_platform_write_page(pf_ctx, page_base_addr, page_buffer)) {
            fprintf(stderr, "Failed to replace data at address 0x%lx\n", (unsigned long) page_base_addr);
            return false;
        }

        data_offset += copy_len;
        current_page_addr += FLASH_PAGE_SIZE;
    }

    return true;
}

static bool jit_stream_flash_append(struct JITStreamFlash *js, const uint8_t *buffer, size_t count)
{
    while (count > 0) {
        // Validate flash constraints: can only write to erased (0xFF) bytes
        uint8_t current_byte = js->page_buffer[js->page_offset];
        uint8_t new_byte = *buffer;
        if ((~current_byte & new_byte) != 0) {
            // Trying to set bits from 0->1 without erase
            fprintf(stderr, "\n=== JIT STREAM FLASH APPEND ERROR ===\n");
            fprintf(stderr, "Attempting to write 0x%02x over 0x%02x at page offset %u\n",
                new_byte, current_byte, js->page_offset);
            fprintf(stderr, "Page base address: 0x%lx\n", (unsigned long) js->page_base_addr);
            fprintf(stderr, "Flash address: 0x%lx\n", (unsigned long) (js->page_base_addr + js->page_offset));
            fprintf(stderr, "Bits being set 0->1: 0x%02x\n", (~current_byte & new_byte));
            fprintf(stderr, "This indicates the sector was not properly erased!\n");
            fprintf(stderr, "=====================================\n\n");
            return false;
        }

        js->page_buffer[js->page_offset] = *buffer;
        if (js->page_offset == (FLASH_PAGE_SIZE - 1)) {
            if (!jit_stream_flash_flush_page(js)) {
                fprintf(stderr, "jit_stream_flash_flush_page failed\n");
                return false;
            }
            // Move to the next page after flushing
            uintptr_t previous_sector = js->page_base_addr & ~(FLASH_SECTOR_SIZE - 1);
            js->page_base_addr += FLASH_PAGE_SIZE;
            js->page_offset = 0;
            uintptr_t new_sector = js->page_base_addr & ~(FLASH_SECTOR_SIZE - 1);

            // Check if we've entered a new sector and erase if needed
            if (new_sector != previous_sector) {
                if (jit_stream_flash_sector_needs_erase(new_sector)) {
                    TRACE("jit_stream_flash_append -- erasing new sector at %lx\n", (unsigned long) new_sector);
                    if (!jit_stream_flash_platform_erase_sector(js->pf_ctx, new_sector)) {
                        fprintf(stderr, "Failed to erase new sector at address 0x%lx\n", (unsigned long) new_sector);
                        return false;
                    }
                }
            }

            // Read the new page contents into the buffer
            memcpy(js->page_buffer, (const uint8_t *) js->page_base_addr, FLASH_PAGE_SIZE);
        } else {
            js->page_offset++;
        }
        buffer++;
        count--;
    }
    return true;
}

static term nif_jit_stream_flash_new(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    struct JITEntry *last_valid_entry = globalcontext_find_last_jit_entry(ctx->global);
    struct JITEntry *new_entry;

    if (last_valid_entry == NULL) {
        // No valid entries, get the first position
        bool is_valid;
        new_entry = globalcontext_find_first_jit_entry(ctx->global, &is_valid);
    } else {
        // Get position after last valid entry
        new_entry = jit_entry_next(last_valid_entry);
    }

    // Return a resource object
    struct JITStreamFlash *js = enif_alloc_resource(jit_stream_flash_resource_type, sizeof(struct JITStreamFlash));
    if (IS_NULL_PTR(js)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    // Initialize platform context
    js->pf_ctx = jit_stream_flash_platform_init();
    if (IS_NULL_PTR(js->pf_ctx)) {
        fprintf(stderr, "Failed to initialize platform flash context\n");
        enif_release_resource(js);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    js->jit_entry = new_entry;
    js->page_base_addr = (uintptr_t) new_entry & ~(FLASH_PAGE_SIZE - 1);

    // Handle sector erasing for the sector where JIT entry starts
    uintptr_t new_entry_addr = (uintptr_t) new_entry;
    uintptr_t new_entry_sector = new_entry_addr & ~(FLASH_SECTOR_SIZE - 1);
    uintptr_t sector_end = new_entry_sector + FLASH_SECTOR_SIZE;

    // Check if there's stale data from entry position to end of sector
    bool has_stale_data = false;
    const uintptr_t *check_ptr = (const uintptr_t *) new_entry_addr;
    const uintptr_t *check_end = (const uintptr_t *) sector_end;
    const uintptr_t erased_pattern = ~((uintptr_t) 0);

    while (check_ptr < check_end) {
        if (*check_ptr != erased_pattern) {
            has_stale_data = true;
            break;
        }
        check_ptr++;
    }

    if (has_stale_data) {
        // There's stale data (from failed compilation) - need to erase but preserve data before entry
        size_t preserve_size = new_entry_addr - new_entry_sector;

        if (preserve_size > 0) {
            // Allocate buffer for the sector
            uint8_t *sector_buffer = malloc(FLASH_SECTOR_SIZE);
            if (IS_NULL_PTR(sector_buffer)) {
                fprintf(stderr, "Failed to allocate sector buffer\n");
                jit_stream_flash_platform_destroy(js->pf_ctx);
                enif_release_resource(js);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            // Copy data to preserve (before the entry)
            memcpy(sector_buffer, (const uint8_t *) new_entry_sector, preserve_size);

            // Fill rest with 0xFF (erased state)
            memset(sector_buffer + preserve_size, 0xFF, FLASH_SECTOR_SIZE - preserve_size);

            // Erase the sector
            TRACE("nif_jit_stream_flash_new -- erasing sector with stale data at %lx (preserving %zu bytes)\n",
                (unsigned long) new_entry_sector, preserve_size);
            if (!jit_stream_flash_platform_erase_sector(js->pf_ctx, new_entry_sector)) {
                fprintf(stderr, "Failed to erase sector with stale data\n");
                free(sector_buffer);
                jit_stream_flash_platform_destroy(js->pf_ctx);
                enif_release_resource(js);
                RAISE_ERROR(BADARG_ATOM);
            }

            // Write back the preserved data page by page
            for (size_t page_offset = 0; page_offset < preserve_size; page_offset += FLASH_PAGE_SIZE) {
                if (!jit_stream_flash_platform_write_page(js->pf_ctx, new_entry_sector + page_offset,
                        sector_buffer + page_offset)) {
                    fprintf(stderr, "Failed to write back preserved data\n");
                    free(sector_buffer);
                    jit_stream_flash_platform_destroy(js->pf_ctx);
                    enif_release_resource(js);
                    RAISE_ERROR(BADARG_ATOM);
                }
            }

            free(sector_buffer);
        } else {
            // Entry is at sector boundary, just erase
            TRACE("nif_jit_stream_flash_new -- erasing sector with stale data at %lx\n",
                (unsigned long) new_entry_sector);
            if (!jit_stream_flash_platform_erase_sector(js->pf_ctx, new_entry_sector)) {
                fprintf(stderr, "Failed to erase sector for new JIT entry\n");
                jit_stream_flash_platform_destroy(js->pf_ctx);
                enif_release_resource(js);
                RAISE_ERROR(BADARG_ATOM);
            }
        }
    } else {
        TRACE("nif_jit_stream_flash_new -- sector at %lx is clean (no stale data)\n",
            (unsigned long) new_entry_sector);
    }

    // Now handle the sector where JIT entry ends (if different from start sector)
    uintptr_t entry_end = new_entry_addr + sizeof(struct JITEntry);
    uintptr_t entry_end_sector = entry_end & ~(FLASH_SECTOR_SIZE - 1);

    if (entry_end_sector != new_entry_sector) {
        // Entry spans two sectors - erase the end sector if needed
        if (jit_stream_flash_sector_needs_erase(entry_end_sector)) {
            TRACE("nif_jit_stream_flash_new -- erasing end sector at %lx\n",
                (unsigned long) entry_end_sector);
            if (!jit_stream_flash_platform_erase_sector(js->pf_ctx, entry_end_sector)) {
                fprintf(stderr, "Failed to erase end sector for new JIT entry\n");
                jit_stream_flash_platform_destroy(js->pf_ctx);
                enif_release_resource(js);
                RAISE_ERROR(BADARG_ATOM);
            }
        }
    }

    memcpy(js->page_buffer, (const uint8_t *) js->page_base_addr, FLASH_PAGE_SIZE);
    js->page_offset = (uintptr_t) new_entry & (FLASH_PAGE_SIZE - 1);

    TRACE("nif_jit_stream_flash_new entry is %p, page_offset is %lx\n", (void *) new_entry, (unsigned long) js->page_offset);

    // Append the first bytes, which may flush the page
    struct JITEntry header;
    header.magic = 0xFFFF;
    header.version = 0xFFFF;
    header.code = 0xFFFFFFFF;
    header.labels = 0xFFFFFFFF;
    header.size = 0xFFFFFFFF;
    if (!jit_stream_flash_append(js, (const uint8_t *) &header, sizeof(header))) {
        jit_stream_flash_platform_destroy(js->pf_ctx);
        enif_release_resource(js);
        RAISE_ERROR(BADARG_ATOM);
    }

    term obj = enif_make_resource(erl_nif_env_from_context(ctx), js);
    enif_release_resource(js); // decrement refcount after enif_alloc_resource
    return obj;
}

static term nif_jit_stream_flash_offset(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_flash_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    uintptr_t current_addr = js_obj->page_base_addr + js_obj->page_offset;
    uintptr_t base_addr = ((uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry));

    int offset = current_addr - base_addr;

    return term_from_int(offset);
}

static term nif_jit_stream_flash_append(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_flash_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    size_t binary_size = term_binary_size(argv[1]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[1]);

    if (!jit_stream_flash_append(js_obj, binary_data, binary_size)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return argv[0];
}

static term nif_jit_stream_flash_replace(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_flash_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t binary_size = term_binary_size(argv[2]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[2]);
    avm_int_t offset = term_to_int(argv[1]);

    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    uintptr_t base_addr = ((uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry));
    uintptr_t replace_start = base_addr + offset;
    uintptr_t replace_end = replace_start + binary_size;

    // Iterate over all pages that need to be updated
    uintptr_t current_page_addr = replace_start & ~(FLASH_PAGE_SIZE - 1);
    size_t binary_offset = 0;

    while (current_page_addr < replace_end) {
        // Calculate the range within this page that needs to be replaced
        uintptr_t page_start_offset = 0;
        uintptr_t page_end_offset = FLASH_PAGE_SIZE;

        if (current_page_addr < replace_start) {
            page_start_offset = replace_start - current_page_addr;
        }

        if (current_page_addr + FLASH_PAGE_SIZE > replace_end) {
            page_end_offset = replace_end - current_page_addr;
        }

        size_t copy_len = page_end_offset - page_start_offset;

        // Check if this is the current buffer page
        if (current_page_addr == js_obj->page_base_addr) {
            // Update current buffer directly
            memcpy(js_obj->page_buffer + page_start_offset, binary_data + binary_offset, copy_len);
        } else {
            // This is an already-flushed page, need to update flash
            if (!jit_stream_flash_replace_at_addr(js_obj->pf_ctx, current_page_addr + page_start_offset,
                    binary_data + binary_offset,
                    copy_len)) {
                RAISE_ERROR(BADARG_ATOM);
            }
        }

        binary_offset += copy_len;
        current_page_addr += FLASH_PAGE_SIZE;
    }

    return argv[0];
}

static term nif_jit_stream_flash_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_flash_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    avm_int_t offset = term_to_int(argv[1]);
    avm_int_t len = term_to_int(argv[2]);

    // Validate parameters
    if (UNLIKELY(len <= 0 || offset < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Calculate current stream position
    uintptr_t current_addr = js_obj->page_base_addr + js_obj->page_offset;
    uintptr_t base_addr = ((uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry));
    size_t stream_offset = current_addr - base_addr;

    // Check if read is within bounds
    if (UNLIKELY((size_t) (offset + len) > stream_offset)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TERM_BINARY_HEAP_SIZE(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    uintptr_t read_addr = base_addr + offset;
    return term_from_literal_binary((const uint8_t *) read_addr, len, &ctx->heap, ctx->global);
}

static term nif_jit_stream_flash_flush(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_flash_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    // Calculate the size BEFORE flushing
    uintptr_t current_addr = js_obj->page_base_addr + js_obj->page_offset;
    uintptr_t code_start = (uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry);
    uint32_t code_size = current_addr - code_start;

    // Check if the size field is in the current unflushed page buffer or in an already-flushed page
    uintptr_t size_field_addr = (uintptr_t) &js_obj->jit_entry->size;
    uintptr_t size_field_page = size_field_addr & ~(FLASH_PAGE_SIZE - 1);

    if (size_field_page == js_obj->page_base_addr) {
        // Size field is in the current buffer, update it directly before flushing
        size_t offset_in_page = size_field_addr - js_obj->page_base_addr;
        memcpy(js_obj->page_buffer + offset_in_page, &code_size, sizeof(uint32_t));
    } else {
        // Size field is in an already-flushed page, use replace
        if (!jit_stream_flash_replace_at_addr(js_obj->pf_ctx, size_field_addr,
                (const uint8_t *) &code_size,
                sizeof(uint32_t))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    // Flush the final page
    if (!jit_stream_flash_flush_page(js_obj)) {
        fprintf(stderr, "jit_stream_flash_flush_page failed\n");
        RAISE_ERROR(BADARG_ATOM);
    }

    return argv[0];
}

static term nif_jit_stream_module(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    return globalcontext_make_atom(ctx->global, ATOM_STR("\x10", "jit_stream_flash"));
}

static const struct Nif jit_stream_module_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_module
};
static const struct Nif jit_stream_flash_new_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_new
};
static const struct Nif jit_stream_flash_offset_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_offset
};
static const struct Nif jit_stream_flash_append_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_append
};
static const struct Nif jit_stream_flash_replace_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_replace
};
static const struct Nif jit_stream_flash_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_read
};
static const struct Nif jit_stream_flash_flush_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_flash_flush
};

ModuleNativeEntryPoint jit_stream_flash_entry_point(Context *ctx, term jit_stream)
{
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), jit_stream, jit_stream_flash_resource_type, &js_obj_ptr))) {
        return NULL;
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    uintptr_t base_addr = ((uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry));

    // Convert to executable address (handles DBUS->IBUS, Thumb bit, etc.)
    base_addr = jit_stream_flash_platform_ptr_to_executable(base_addr);

    return (ModuleNativeEntryPoint) base_addr;
}

static void jit_stream_flash_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) obj;
    if (js_obj->pf_ctx) {
        jit_stream_flash_platform_destroy(js_obj->pf_ctx);
    }
}

const struct Nif *jit_stream_flash_get_nif(const char *nifname)
{
    if (strcmp("jit:stream_module/0", nifname) == 0) {
        return &jit_stream_module_nif;
    }
    if (strncmp("jit_stream_flash:", nifname, 17) == 0) {
        const char *rest = nifname + 17;
        if (strcmp("new/1", rest) == 0) {
            return &jit_stream_flash_new_nif;
        }
        if (strcmp("offset/1", rest) == 0) {
            return &jit_stream_flash_offset_nif;
        }
        if (strcmp("append/2", rest) == 0) {
            return &jit_stream_flash_append_nif;
        }
        if (strcmp("replace/3", rest) == 0) {
            return &jit_stream_flash_replace_nif;
        }
        if (strcmp("read/3", rest) == 0) {
            return &jit_stream_flash_read_nif;
        }
        if (strcmp("flush/1", rest) == 0) {
            return &jit_stream_flash_flush_nif;
        }
    }
    return NULL;
}

void jit_stream_flash_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    jit_stream_flash_resource_type = enif_init_resource_type(&env, "jit_stream_flash", &jit_stream_flash_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

void globalcontext_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    bool is_valid;
    (void) globalcontext_find_first_jit_entry(global, &is_valid);

    struct JSFlashPlatformContext *pf_ctx = jit_stream_flash_platform_init();
    if (IS_NULL_PTR(pf_ctx)) {
        fprintf(stderr, "Failed to initialize platform flash context\n");
        return;
    }

    // Reverse the executable address transformation to get data address
    // Platform-specific: Thumb (clear bit 0), RISC-V (IBUS->DBUS conversion)
    uintptr_t data_addr = jit_stream_flash_platform_executable_to_ptr((uintptr_t) entry_point);

    struct JITEntry *jit_entry = (struct JITEntry *) (data_addr - sizeof(struct JITEntry));
    uintptr_t code = (uintptr_t) mod->code;

    // Finalize the entry
    if (!jit_stream_flash_finalize_entry(pf_ctx, jit_entry, JIT_ENTRY_MAGIC, version, (uint32_t) code, labels)) {
        fprintf(stderr, "jit_stream_flash_finalize_entry failed\n");
        jit_stream_flash_platform_destroy(pf_ctx);
        return;
    }

#ifdef ENABLE_TRACE
    // Compute CRC of entire module for verification
    uint32_t module_crc = crc32((const uint8_t *) jit_entry, sizeof(struct JITEntry) + jit_entry->size);
    TRACE("After finalize - jit_entry=%p CRC32=0x%08x (entry+code size=%u)\n",
        (void *) jit_entry, (unsigned int) module_crc, (unsigned int) (sizeof(struct JITEntry) + jit_entry->size));
#endif

    // Erase next sector if it's completely after the current module
    struct JITEntry *current_entry = (struct JITEntry *) (data_addr - sizeof(struct JITEntry));
    struct JITEntry *next_entry = jit_entry_next(current_entry);
    uintptr_t next_entry_addr = (uintptr_t) next_entry;
    uintptr_t next_sector = next_entry_addr & ~(FLASH_SECTOR_SIZE - 1);

    // Calculate the sector where the current module ENDS (not where it starts)
    uintptr_t current_module_end = (uintptr_t) current_entry + sizeof(struct JITEntry) + current_entry->size;
    uintptr_t current_end_sector = current_module_end & ~(FLASH_SECTOR_SIZE - 1);

    // Only erase next sector if it's completely after the current module's end
    // This prevents erasing a sector that contains the tail of the current module
    if (next_sector > current_end_sector) {
        // Next entry is in a sector completely after current module, erase it if it has stale data
        if (next_entry->magic != 0xFFFF) {
            TRACE("globalcontext_set_cache_native_code -- NOT erasing new sector at %lx\n", (unsigned long) next_sector);
            if (!jit_stream_flash_platform_erase_sector(pf_ctx, next_sector)) {
                fprintf(stderr, "jit_stream_flash_platform_erase_sector failed\n");
                jit_stream_flash_platform_destroy(pf_ctx);
                return;
            }
        } else {
            TRACE("globalcontext_set_cache_native_code -- NOT erasing new sector at %lx\n", (unsigned long) next_sector);
        }
    }

    if (!is_valid) {
        // Mark that cache entry is valid by replacing end with END in installed AVM
        globalcontext_set_cache_valid(global);
    }

    jit_stream_flash_platform_destroy(pf_ctx);
}

// Implementation of jit_stream_entry_point, sys_get_cache_native_code and
// sys_set_cache_native_code using this jit_stream
#ifndef TEST_JIT_STREAM_FLASH
ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    return jit_stream_flash_entry_point(ctx, jit_stream);
}

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version, ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    bool is_valid;
    struct JITEntry *jit_entry = globalcontext_find_first_jit_entry(global, &is_valid);
    if (!is_valid) {
        return false;
    }
    uintptr_t code = (uintptr_t) mod->code;
    while (jit_entry->magic == JIT_ENTRY_MAGIC) {
        if (jit_entry->code == (uint32_t) code) {
            *version = jit_entry->version;
            uintptr_t ep_addr = (uintptr_t) jit_entry + sizeof(struct JITEntry);
            ep_addr = jit_stream_flash_platform_ptr_to_executable(ep_addr);
            *entry_point = (ModuleNativeEntryPoint) ep_addr;
            *labels = jit_entry->labels;

#ifdef ENABLE_TRACE
            // Compute CRC of entire module for verification
            uint32_t module_crc = crc32((const uint8_t *) jit_entry, sizeof(struct JITEntry) + jit_entry->size);
            TRACE("Loading from cache - jit_entry=%p CRC32=0x%08x (entry+code size=%u)\n",
                (void *) jit_entry, (unsigned int) module_crc, (unsigned int) (sizeof(struct JITEntry) + jit_entry->size));
#endif

            return true;
        }
        jit_entry = jit_entry_next(jit_entry);
    }
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    globalcontext_set_cache_native_code(global, mod, version, entry_point, labels);
}
#endif
