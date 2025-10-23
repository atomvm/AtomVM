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

#ifndef AVM_NO_JIT

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
#include "rp2_sys.h"
#include "term.h"

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <hardware/flash.h>
#include <pico/flash.h>

static ErlNifResourceType *jit_stream_flash_resource_type;
static void jit_stream_flash_dtor(ErlNifEnv *caller_env, void *obj);
static bool jit_stream_flash_replace_at_addr(uintptr_t addr, const uint8_t *data, size_t len);
static void __not_in_flash_func(jit_stream_flash_erase_block)(void *addr_ptr);

const ErlNifResourceTypeInit jit_stream_flash_resource_type_init = {
    .members = 1,
    .dtor = jit_stream_flash_dtor
};

struct JITEntry
{
    uint16_t magic;
    uint16_t version;
    uintptr_t code;
    uint32_t labels;
    uint32_t size;
} __attribute__((packed));

_Static_assert(sizeof(struct JITEntry) == 16, "sizeof(struct JITEntry) is supposed to be 16");

struct JITStreamFlash
{
    struct JITEntry *jit_entry;
    uintptr_t page_base_addr;
    uint8_t page_buffer[256];
    uint8_t page_offset;
};

#define BLOCK_SIZE 4096
#define PAGE_SIZE 256
#define JIT_ENTRY_MAGIC 0x4A74

static struct JITEntry *jit_entry_next(struct JITEntry *jit_entry)
{
    uintptr_t next_entry_addr = ((uintptr_t) jit_entry) + sizeof(struct JITEntry) + jit_entry->size;
    // Align to 4 bytes boundaries
    next_entry_addr = (next_entry_addr + 3) & ~3;
    return (struct JITEntry *) next_entry_addr;
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

    uintptr_t max_end_offset_page = ((((uintptr_t) max_end_offset) - 1) & ~(BLOCK_SIZE - 1));
    *is_valid = valid_cache;
    return (struct JITEntry *) (max_end_offset_page + BLOCK_SIZE);
}

static void globalcontext_set_cache_valid(GlobalContext *global)
{
    uint32_t end_size;
    const void *end_offset;
    const char *end_name;
    bool valid_cache;

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
            // The helper function handles page boundaries automatically
            const uint8_t end_str[] = "END";
            if (!jit_stream_flash_replace_at_addr((uintptr_t) end_name, end_str, 3)) {
                fprintf(stderr, "Failed to update cache validity marker from 'end' to 'END'\n");
                // This is a critical error but we can't easily propagate it
                // The cache will remain invalid and code won't be cached
                break;
            }
        }
    } while (!valid_cache);
}

static struct JITEntry *globalcontext_find_last_jit_entry(GlobalContext *global)
{
    bool is_valid;
    struct JITEntry *jit_entry = globalcontext_find_first_jit_entry(global, &is_valid);
    if (!is_valid) {
        return jit_entry;
    }
    while (jit_entry->magic == JIT_ENTRY_MAGIC) {
        jit_entry = jit_entry_next(jit_entry);
    }
    return jit_entry;
}

static void __not_in_flash_func(jit_stream_flash_flush_page)(void *js_ptr)
{
    struct JITStreamFlash *js = (struct JITStreamFlash *) js_ptr;
    if ((js->page_base_addr & (BLOCK_SIZE - 1)) == 0) {
        flash_range_erase(js->page_base_addr - XIP_BASE, BLOCK_SIZE);
    }
    flash_range_program(js->page_base_addr - XIP_BASE, js->page_buffer, PAGE_SIZE);
    // Note: Don't update page_base_addr or page_offset here
    // They will be updated by the caller if needed (e.g., in jit_stream_flash_append)
}

static void __not_in_flash_func(jit_stream_flash_update_page)(void *js_ptr)
{
    struct JITStreamFlash *js = (struct JITStreamFlash *) js_ptr;
    flash_range_program(js->page_base_addr - XIP_BASE, js->page_buffer, PAGE_SIZE);
}

static void __not_in_flash_func(jit_stream_flash_erase_block)(void *addr_ptr)
{
    uintptr_t addr = (uintptr_t) addr_ptr;
    flash_range_erase(addr - XIP_BASE, BLOCK_SIZE);
}

// Replace data in flash at the given absolute address
// Returns true on success, false if validation fails (trying to set bits 0→1)
static bool jit_stream_flash_replace_at_addr(uintptr_t addr, const uint8_t *data, size_t len)
{
    uintptr_t replace_start = addr;
    uintptr_t replace_end = replace_start + len;

    // Iterate over all pages that need to be updated
    uintptr_t current_page_addr = replace_start & ~(PAGE_SIZE - 1);
    size_t data_offset = 0;

    while (current_page_addr < replace_end) {
        // Calculate the range within this page that needs to be replaced
        uintptr_t page_start_offset = 0;
        uintptr_t page_end_offset = PAGE_SIZE;

        if (current_page_addr < replace_start) {
            page_start_offset = replace_start - current_page_addr;
        }

        if (current_page_addr + PAGE_SIZE > replace_end) {
            page_end_offset = replace_end - current_page_addr;
        }

        size_t copy_len = page_end_offset - page_start_offset;

        // Prepare page buffer
        struct JITStreamFlash temp;
        temp.page_base_addr = current_page_addr;

        // Read current page contents
        memcpy(temp.page_buffer, (const uint8_t *) temp.page_base_addr, PAGE_SIZE);

        // Verify that we're only clearing bits (1→0), not setting them (0→1)
        // Flash can only program bits from 1 to 0; to set bits back to 1 requires erase
        const uint8_t *flash_ptr = (const uint8_t *) temp.page_base_addr;
        for (size_t i = 0; i < copy_len; i++) {
            uint8_t flash_byte = flash_ptr[page_start_offset + i];
            uint8_t new_byte = data[data_offset + i];

            // Check if we're trying to set any bits (0→1)
            // This is invalid: (new_byte & ~flash_byte) shows bits that would go from 0→1
            if ((new_byte & ~flash_byte) != 0) {
                fprintf(stderr, "\n=== FLASH REPLACE VALIDATION FAILED ===\n");
                fprintf(stderr, "Attempting to set bits (0→1) without erase!\n");
                fprintf(stderr, "Page address: %x\n", temp.page_base_addr);
                fprintf(stderr, "Offset in page: %zu, flash byte: 0x%02hhx, new byte: 0x%02hhx\n",
                    page_start_offset + i, flash_byte, new_byte);
                fprintf(stderr, "Bits being set (0→1): 0x%02hhx\n", (new_byte & ~flash_byte));
                fprintf(stderr, "Replace address: %x, len=%zu\n", (unsigned int) addr, len);
                fprintf(stderr, "========================================\n\n");
                return false;
            }
        }

        // Update with new data
        memcpy(temp.page_buffer + page_start_offset, data + data_offset, copy_len);

        // Write back to flash
        int r = flash_safe_execute(jit_stream_flash_update_page, &temp, UINT32_MAX);

        if (r != PICO_OK) {
            fprintf(stderr, "flash_safe_execute failed with error %d\n", r);
            return false;
        }

        data_offset += copy_len;
        current_page_addr += PAGE_SIZE;
    }

    return true;
}

static void jit_stream_flash_append(struct JITStreamFlash *js, const uint8_t *buffer, size_t count)
{
    while (count > 0) {
        js->page_buffer[js->page_offset] = *buffer;
        if (js->page_offset == (PAGE_SIZE - 1)) {
            int r = flash_safe_execute(jit_stream_flash_flush_page, js, UINT32_MAX);
            assert(r == PICO_OK);
            // Move to the next page after flushing
            js->page_base_addr += PAGE_SIZE;
            js->page_offset = 0;
            // Read the new page contents into the buffer
            memcpy(js->page_buffer, (const uint8_t *) js->page_base_addr, PAGE_SIZE);
        } else {
            js->page_offset++;
        }
        buffer++;
        count--;
    }
}

static term nif_jit_stream_flash_new(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct JITEntry *jit_entry = globalcontext_find_last_jit_entry(ctx->global);

    // Return a resource object
    struct JITStreamFlash *js = enif_alloc_resource(jit_stream_flash_resource_type, sizeof(struct JITStreamFlash));
    if (IS_NULL_PTR(js)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    js->jit_entry = jit_entry;
    js->page_base_addr = (uintptr_t) jit_entry & ~(PAGE_SIZE - 1);
    memcpy(js->page_buffer, (const uint8_t *) js->page_base_addr, PAGE_SIZE);
    js->page_offset = (uintptr_t) jit_entry & (PAGE_SIZE - 1);

    // Append the first bytes, which may flush the page
    struct JITEntry header;
    header.magic = 0xFFFF;
    header.version = 0xFFFF;
    header.code = 0xFFFFFFFF;
    header.labels = 0xFFFFFFFF;
    header.size = 0xFFFFFFFF;
    jit_stream_flash_append(js, (const uint8_t *) &header, sizeof(header));

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

    jit_stream_flash_append(js_obj, binary_data, binary_size);

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
    uintptr_t current_page_addr = replace_start & ~(PAGE_SIZE - 1);
    size_t binary_offset = 0;

    while (current_page_addr < replace_end) {
        // Calculate the range within this page that needs to be replaced
        uintptr_t page_start_offset = 0;
        uintptr_t page_end_offset = PAGE_SIZE;

        if (current_page_addr < replace_start) {
            page_start_offset = replace_start - current_page_addr;
        }

        if (current_page_addr + PAGE_SIZE > replace_end) {
            page_end_offset = replace_end - current_page_addr;
        }

        size_t copy_len = page_end_offset - page_start_offset;

        // Check if this is the current buffer page
        if (current_page_addr == js_obj->page_base_addr) {
            // Update current buffer directly
            memcpy(js_obj->page_buffer + page_start_offset, binary_data + binary_offset, copy_len);
        } else {
            // This is an already-flushed page, need to update flash
            if (!jit_stream_flash_replace_at_addr(current_page_addr + page_start_offset,
                    binary_data + binary_offset,
                    copy_len)) {
                RAISE_ERROR(BADARG_ATOM);
            }
        }

        binary_offset += copy_len;
        current_page_addr += PAGE_SIZE;
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

    // Calculate the size BEFORE flushing, because flush modifies page_base_addr and page_offset
    uintptr_t current_addr = js_obj->page_base_addr + js_obj->page_offset;
    uintptr_t code_start = (uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry);
    uint32_t code_size = current_addr - code_start;

    // Check if the size field is in the current unflushed page buffer or in an already-flushed page
    uintptr_t size_field_addr = (uintptr_t) &js_obj->jit_entry->size;
    uintptr_t size_field_page = size_field_addr & ~(PAGE_SIZE - 1);

    if (size_field_page == js_obj->page_base_addr) {
        // Size field is in the current buffer, update it directly before flushing
        size_t offset_in_page = size_field_addr - js_obj->page_base_addr;
        memcpy(js_obj->page_buffer + offset_in_page, &code_size, sizeof(uint32_t));
    } else {
        // Size field is in an already-flushed page, use replace
        if (!jit_stream_flash_replace_at_addr(size_field_addr,
                (const uint8_t *) &code_size,
                sizeof(uint32_t))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    // Flush the final page (which will now include the updated size)
    // Don't update page_base_addr/page_offset so subsequent offset() calls work correctly
    int r = flash_safe_execute(jit_stream_flash_flush_page, js_obj, UINT32_MAX);
    assert(r == PICO_OK);

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

ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), jit_stream, jit_stream_flash_resource_type, &js_obj_ptr))) {
        return NULL;
    }
    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) js_obj_ptr;

    uintptr_t base_addr = ((uintptr_t) js_obj->jit_entry + sizeof(struct JITEntry));

    // Set thumb bit
    ModuleNativeEntryPoint result = (ModuleNativeEntryPoint) ((uintptr_t) base_addr | 0x1);
    return result;
}

static void jit_stream_flash_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct JITStreamFlash *js_obj = (struct JITStreamFlash *) obj;
}

//
// Entrypoints
//

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

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version, ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    bool is_valid;
    struct JITEntry *jit_entry = globalcontext_find_first_jit_entry(global, &is_valid);
    if (!is_valid) {
        return false;
    }
    uintptr_t code = (uintptr_t) mod->code;
    while (jit_entry->magic == JIT_ENTRY_MAGIC) {
        if (jit_entry->code == code) {
            *version = jit_entry->version;
            // Add 1 for thumb bit
            *entry_point = (ModuleNativeEntryPoint) ((uintptr_t) jit_entry + sizeof(struct JITEntry) + 1);
            *labels = jit_entry->labels;
            return true;
        }
        jit_entry = jit_entry_next(jit_entry);
    }
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    bool is_valid;
    (void) globalcontext_find_first_jit_entry(global, &is_valid);
    struct JITStreamFlash temp;
    temp.page_base_addr = ((uintptr_t) entry_point - 1 - sizeof(struct JITEntry));
    // Read current page contents
    memcpy(temp.page_buffer, (const uint8_t *) temp.page_base_addr, PAGE_SIZE);
    struct JITEntry *updated_entry = (struct JITEntry *) temp.page_buffer;
    updated_entry->magic = JIT_ENTRY_MAGIC;
    updated_entry->labels = labels;
    updated_entry->version = version;
    uintptr_t code = (uintptr_t) mod->code;
    updated_entry->code = code;

    // Write back to flash
    int r = flash_safe_execute(jit_stream_flash_update_page, &temp, UINT32_MAX);
    assert(r == PICO_OK);

    // Erase next block if next entry starts at a block boundary
    // Otherwise, next entry is in same block and magic is already 0xFFFF
    struct JITEntry *current_entry = (struct JITEntry *) temp.page_base_addr;
    struct JITEntry *next_entry = jit_entry_next(current_entry);
    uintptr_t next_entry_addr = (uintptr_t) next_entry;
    if ((next_entry_addr & (BLOCK_SIZE - 1)) == 0) {
        r = flash_safe_execute(jit_stream_flash_erase_block, (void *) next_entry_addr, UINT32_MAX);
        assert(r == PICO_OK);
    }

    if (!is_valid) {
        // Mark that cache entry is valid by replacing end with END in installed AVM
        globalcontext_set_cache_valid(global);
    }
}

#endif
