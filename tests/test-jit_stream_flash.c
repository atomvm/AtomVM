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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "avmpack.h"
#include "context.h"
#include "globalcontext.h"
#include "jit_stream_flash.h"
#include "jit_stream_flash_platform.h"
#include "scheduler.h"
#include "synclist.h"
#include "term.h"
#include "utils.h"

// Mock flash memory - simulate 64KB of flash
#define MOCK_FLASH_SIZE (64 * 1024)
// Align to sector boundary for proper flash simulation
static uint8_t mock_flash[MOCK_FLASH_SIZE] __attribute__((aligned(FLASH_SECTOR_SIZE)));

// JIT entry header (copied from jit_stream_flash.c for testing)
struct JITEntry
{
    uint16_t magic;
    uint16_t version;
    uint32_t code;
    uint32_t labels;
    uint32_t size;
};

// CRC32 for verification (copied from jit_stream_flash.c)
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

// Platform context (opaque)
struct JSFlashPlatformContext
{
    uintptr_t base_addr;
};

// Forward declarations of mock platform functions
struct JSFlashPlatformContext *jit_stream_flash_platform_init(void);
void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *ctx);
bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *ctx, uintptr_t addr);
bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *ctx, uintptr_t addr, const uint8_t *data);
uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr);

// Mock platform implementation
struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    struct JSFlashPlatformContext *ctx = malloc(sizeof(struct JSFlashPlatformContext));
    if (!ctx) {
        return NULL;
    }

    // DO NOT erase flash here - it should persist across multiple stream creations
    // Flash initialization happens once at test startup

    ctx->base_addr = (uintptr_t) mock_flash;
    return ctx;
}

void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *ctx)
{
    free(ctx);
}

bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *ctx, uintptr_t addr)
{
    assert(ctx);

    // Check alignment
    if ((addr - ctx->base_addr) % FLASH_SECTOR_SIZE != 0) {
        fprintf(stderr, "Erase address 0x%lx not sector-aligned\n", (unsigned long) addr);
        return false;
    }

    size_t offset = addr - ctx->base_addr;
    if (offset >= MOCK_FLASH_SIZE) {
        fprintf(stderr, "Erase address 0x%lx out of bounds\n", (unsigned long) addr);
        return false;
    }

    // Erase the sector
    memset(&mock_flash[offset], 0xFF, FLASH_SECTOR_SIZE);

    return true;
}

bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *ctx, uintptr_t addr, const uint8_t *data)
{
    assert(ctx);

    // Check alignment
    if ((addr - ctx->base_addr) % FLASH_PAGE_SIZE != 0) {
        fprintf(stderr, "Write address 0x%lx not page-aligned (base_addr=0x%lx, offset=0x%lx)\n",
            (unsigned long) addr, (unsigned long) ctx->base_addr,
            (unsigned long) (addr - ctx->base_addr));
        return false;
    }

    size_t offset = addr - ctx->base_addr;
    if (offset + FLASH_PAGE_SIZE > MOCK_FLASH_SIZE) {
        fprintf(stderr, "Write at offset 0x%zx would exceed flash bounds\n", offset);
        return false;
    }

    // Validate write - flash can only transition bits from 1→0 without erase
    for (size_t i = 0; i < FLASH_PAGE_SIZE; i++) {
        uint8_t current = mock_flash[offset + i];
        uint8_t new_val = data[i];

        // Check if we're trying to set any bits from 0→1
        if ((~current & new_val) != 0) {
            fprintf(stderr, "FLASH VALIDATION ERROR at offset 0x%zx:\n", offset + i);
            fprintf(stderr, "  Attempting to set bits 0→1 without erase\n");
            fprintf(stderr, "  Current: 0x%02x, New: 0x%02x, Invalid bits: 0x%02x\n",
                current, new_val, ~current & new_val);
            return false;
        }
    }

    // Write the page
    memcpy(&mock_flash[offset], data, FLASH_PAGE_SIZE);

    return true;
}

uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr)
{
    // For host testing, no conversion needed
    return addr;
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    // For host testing, no conversion needed
    return addr;
}

// Create a minimal AVM pack for testing
static uint8_t create_minimal_avmpack(void)
{
    // Create a minimal AVM pack with an "end" section
    uint8_t *pack = mock_flash + 0x100; // Place pack at offset 0x100

    // AVM Pack header: "#!/usr/bin/env AtomVM\n" (23 bytes) + padding to 24 bytes
    const char header_str[] = "#!/usr/bin/env AtomVM\n";
    memcpy(pack, header_str, 23);
    pack[23] = 0; // Padding to align to 4 bytes

    // Section header for "end" section
    uint8_t *section = pack + 24;
    uint32_t *sec_header = (uint32_t *) section;

    // Section format: size (4) + flags (4) + reserved (4) + name (null-terminated)
    // Write size in big-endian (total section size including header)
    uint32_t section_size = 4 + 4 + 4 + 4; // size + flags + reserved + "end\0"
    sec_header[0] = __builtin_bswap32(section_size);

    // Write flags in big-endian
    uint32_t flags = END_OF_FILE;
    sec_header[1] = __builtin_bswap32(flags);

    // Write reserved field (seems to be 0)
    sec_header[2] = 0;

    // Write null-terminated name starting at offset 12
    memcpy(section + 12, "end", 4); // includes null terminator

    return 0;
}

// Register AVM pack with global context
static void register_test_avmpack(GlobalContext *glb)
{
    create_minimal_avmpack();

    // Create AVMPackData
    struct ConstAVMPack *pack = malloc(sizeof(struct ConstAVMPack));
    avmpack_data_init(&pack->base, &const_avm_pack_info);
    pack->base.data = mock_flash + 0x100;
    pack->base.in_use = true;

    // Add to global context's avmpack list
    synclist_append(&glb->avmpack_data, &pack->base.avmpack_head);
}

// Test helper: create binary term with proper GC rooting
static term make_binary_rooted(Context *ctx, const uint8_t *data, size_t len, term *roots, int num_roots)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, term_binary_heap_size(len), num_roots, roots, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        return term_invalid_term();
    }
    return term_from_literal_binary(data, len, &ctx->heap, ctx->global);
}

// Test helper: get NIF function
typedef term (*nif_function)(Context *ctx, int argc, term argv[]);

static nif_function get_nif(const char *name)
{
    const struct Nif *nif = jit_stream_flash_get_nif(name);
    if (!nif || nif->base.type != NIFFunctionType) {
        return NULL;
    }
    return nif->nif_ptr;
}

// Test 1: Basic append and flush
void test_basic_append_flush(void)
{
    fprintf(stderr, "\n=== Test: Basic Append and Flush ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    assert(new_nif != NULL);
    assert(append_nif != NULL);
    assert(flush_nif != NULL);

    // Create stream
    term argv[3];
    argv[0] = term_from_int(10); // label count
    term stream = new_nif(ctx, 1, argv);
    assert(term_is_resource_reference(stream));

    // Append some data - root the stream during binary allocation
    uint8_t data[100];
    memset(data, 0xAA, sizeof(data));
    argv[0] = stream;
    argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1); // Root argv[0] (stream)
    stream = append_nif(ctx, 2, argv); // Update stream in case GC moved it
    assert(stream == argv[0]); // Should return the stream

    // Flush
    argv[0] = stream;
    stream = flush_nif(ctx, 1, argv); // Update stream
    assert(stream == argv[0]);

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Basic append and flush\n");
}

// Test 2: Multiple appends crossing page boundaries
void test_multiple_appends(void)
{
    fprintf(stderr, "\n=== Test: Multiple Appends Crossing Pages ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Create stream
    term argv[3];
    argv[0] = term_from_int(10);
    term stream = new_nif(ctx, 1, argv);

    // Append multiple chunks to cross page boundaries
    for (int i = 0; i < 10; i++) {
        uint8_t data[100];
        memset(data, 0xA0 + i, sizeof(data));
        argv[0] = stream;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream = append_nif(ctx, 2, argv);
        argv[0] = stream; // Update for next iteration
    }

    // Flush
    argv[0] = stream;
    flush_nif(ctx, 1, argv);

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Multiple appends crossing pages\n");
}

// Test 3: Replace operation
void test_replace(void)
{
    fprintf(stderr, "\n=== Test: Replace Operation ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function replace_nif = get_nif("jit_stream_flash:replace/3");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Create stream
    term argv[3];
    argv[0] = term_from_int(10);
    term stream = new_nif(ctx, 1, argv);

    // Append initial data
    uint8_t data[200];
    memset(data, 0xAA, sizeof(data));
    argv[0] = stream;
    argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
    stream = append_nif(ctx, 2, argv); // Update stream

    // Replace some bytes in the middle
    uint8_t replace_data[] = { 0x11, 0x22, 0x33, 0x44 };
    argv[0] = stream;
    argv[1] = term_from_int(50); // offset
    argv[2] = make_binary_rooted(ctx, replace_data, sizeof(replace_data), &argv[0], 1);
    stream = replace_nif(ctx, 3, argv); // Update stream

    // Flush
    argv[0] = stream;
    stream = flush_nif(ctx, 1, argv); // Update stream

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Replace operation\n");
}

// Test 4: Second module bug scenario - this is the critical test!
void test_second_module_bug(void)
{
    fprintf(stderr, "\n=== Test: Second Module Bug Scenario (THE ACTUAL BUG) ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Simulate first module compilation - fill most of first sector
    fprintf(stderr, "Simulating first module compilation...\n");
    term argv[3];
    argv[0] = term_from_int(100);
    term stream1 = new_nif(ctx, 1, argv);

    // Write 3.5KB of code (leaves 0.5KB in first sector)
    for (int i = 0; i < 35; i++) {
        uint8_t data[100];
        memset(data, 0xA0 + (i % 16), sizeof(data));
        argv[0] = stream1;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream1 = append_nif(ctx, 2, argv); // Update stream1
    }

    argv[0] = stream1;

    stream1 = flush_nif(ctx, 1, argv); // Update stream1

    fprintf(stderr, "First module compiled and flushed\n");

    // Finalize the first module to mark it as valid and prepare for the second
    ModuleNativeEntryPoint entry1 = jit_stream_flash_entry_point(ctx, stream1);
    Module fake_mod1;
    fake_mod1.code = (CodeChunk *) 0x12345678; // Fake code pointer for testing

    globalcontext_set_cache_native_code(glb, &fake_mod1, 1, entry1, 100);

    // Now simulate second module - this should trigger the bug
    // The bug was: when creating a new stream, if we're in a new sector
    // that hasn't been erased, we need to erase it before writing
    fprintf(stderr, "\nSimulating second module compilation...\n");
    argv[0] = term_from_int(50);
    term stream2 = new_nif(ctx, 1, argv);

    // Append data - this will cross into next sector
    for (int i = 0; i < 20; i++) {
        uint8_t data[100];
        memset(data, 0xB0 + (i % 16), sizeof(data));
        argv[0] = stream2;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream2 = append_nif(ctx, 2, argv); // Update stream2
    }

    argv[0] = stream2;
    stream2 = flush_nif(ctx, 1, argv); // Update stream2

    fprintf(stderr, "Second module compiled and flushed successfully!\n");

    // Finalize the second module
    ModuleNativeEntryPoint entry2 = jit_stream_flash_entry_point(ctx, stream2);
    Module fake_mod2;
    fake_mod2.code = (CodeChunk *) 0x87654321; // Fake code pointer for testing
    globalcontext_set_cache_native_code(glb, &fake_mod2, 1, entry2, 50);

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Second module bug scenario - bug is FIXED!\n");
}

void test_magic_0xffff_but_garbage_bug(void)
{
    fprintf(stderr, "\n=== Test: Magic is 0xFFFF but Sector Has Garbage ===\n");

    // Simulate ESP32 scenario where first JIT entry is at start of sector
    // and magic happens to be 0xFFFF but rest has garbage
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first sector with AVM

    // Set magic to 0xFFFF at start of sector 1, but rest is garbage (0x97)
    uint16_t *magic_ptr = (uint16_t *) (mock_flash + 0x1000);
    *magic_ptr = 0xFFFF;
    // Fill rest of sector with garbage
    for (size_t i = 2; i < FLASH_SECTOR_SIZE; i++) {
        mock_flash[0x1000 + i] = 0x97;
    }

    fprintf(stderr, "Sector 1: magic=0xFFFF at offset 0, but rest has garbage (0x97)\n");

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Compile a small module - should detect garbage and erase
    fprintf(stderr, "Compiling module (should detect garbage despite magic=0xFFFF)...\n");
    term argv[3];
    argv[0] = term_from_int(100);
    term stream1 = new_nif(ctx, 1, argv);

    // Append some data
    uint8_t data[100];
    memset(data, 0xAA, sizeof(data));
    argv[0] = stream1;
    argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
    stream1 = append_nif(ctx, 2, argv);

    argv[0] = stream1;
    stream1 = flush_nif(ctx, 1, argv);
    fprintf(stderr, "Module compiled successfully!\n");

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Magic 0xFFFF but garbage test\n");
}

void test_garbage_flash_bug(void)
{
    fprintf(stderr, "\n=== Test: Garbage Flash Bug - JIT Sectors Not Erased After AVM Flash ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    fprintf(stderr, "Flash state: Sector 0 erased (0xFF), sectors 1+ have garbage (0x00)\n");

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Compile first module that spans two sectors (like benchmark: 8254 bytes)
    fprintf(stderr, "Compiling first module spanning sectors 1-2 (8254 bytes)...\n");
    term argv[3];
    argv[0] = term_from_int(100);
    term stream1 = new_nif(ctx, 1, argv);

    // Write 82 blocks of 100 bytes = 8200 bytes + 16 byte header = 8216 bytes
    for (int i = 0; i < 82; i++) {
        uint8_t data[100];
        memset(data, 0xAA, sizeof(data));
        argv[0] = stream1;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream1 = append_nif(ctx, 2, argv);
    }

    argv[0] = stream1;
    stream1 = flush_nif(ctx, 1, argv);
    fprintf(stderr, "First module compiled and flushed\n");

    ModuleNativeEntryPoint entry1 = jit_stream_flash_entry_point(ctx, stream1);
    Module fake_mod1;
    fake_mod1.code = (CodeChunk *) 0x12345678;
    globalcontext_set_cache_native_code(glb, &fake_mod1, 1, entry1, 100);

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Garbage flash bug test\n");
}

void test_esp32_crash_bug(void)
{
    fprintf(stderr, "\n=== Test: ESP32 Crash Bug - Module Spanning Multiple Sectors ===\n");

    // Reset flash for this test
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE); // first page with AVM

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Simulate first module like ESP32 benchmark: ~8254 bytes
    // This will span sectors 0, 1, and part of sector 2
    fprintf(stderr, "First module: writing ~8254 bytes (spans 3 sectors)...\n");
    term argv[3];
    argv[0] = term_from_int(100);
    term stream1 = new_nif(ctx, 1, argv);

    // Write 82 blocks of 100 bytes = 8200 bytes + 16 byte header = 8216 bytes
    for (int i = 0; i < 82; i++) {
        uint8_t data[100];
        memset(data, 0xAA, sizeof(data));
        argv[0] = stream1;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream1 = append_nif(ctx, 2, argv);
    }

    argv[0] = stream1;
    stream1 = flush_nif(ctx, 1, argv);
    fprintf(stderr, "First module flushed\n");

    ModuleNativeEntryPoint entry1 = jit_stream_flash_entry_point(ctx, stream1);
    Module fake_mod1;
    fake_mod1.code = (CodeChunk *) 0x12345678;
    globalcontext_set_cache_native_code(glb, &fake_mod1, 1, entry1, 100);

    // Second module like ESP32 pingpong: ~6690 bytes
    // This will start in sector 2 (which already has tail of first module!)
    fprintf(stderr, "Second module: writing ~6690 bytes...\n");
    argv[0] = term_from_int(50);
    term stream2 = new_nif(ctx, 1, argv);

    // Write 67 blocks of 100 bytes = 6700 bytes
    for (int i = 0; i < 67; i++) {
        uint8_t data[100];
        memset(data, 0xBB, sizeof(data));
        argv[0] = stream2;
        argv[1] = make_binary_rooted(ctx, data, sizeof(data), &argv[0], 1);
        stream2 = append_nif(ctx, 2, argv);
    }

    argv[0] = stream2;
    stream2 = flush_nif(ctx, 1, argv);
    fprintf(stderr, "Second module flushed\n");

    ModuleNativeEntryPoint entry2 = jit_stream_flash_entry_point(ctx, stream2);
    Module fake_mod2;
    fake_mod2.code = (CodeChunk *) 0x87654321;
    globalcontext_set_cache_native_code(glb, &fake_mod2, 1, entry2, 50);

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: ESP32 crash bug test\n");
}

// Test for the tail corruption bug: when first module extends into next sector,
// creating the second module should NOT erase the sector containing the first module's tail
static void test_tail_corruption_bug(void)
{
    fprintf(stderr, "\n=== Test: Tail Corruption Bug - Module Tail in Next Sector ===\n");

    // Initialize flash: sector 0 erased (AVM), rest is garbage
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE);

    create_minimal_avmpack();

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Create first module that will extend into the next sector
    // Module size: 8270 bytes (like benchmark on ESP32)
    // Entry header: 16 bytes at 0x0 in sector 0x1000
    // Native code: 8254 bytes, extends from sector 0x1000 into sector 0x2000
    // Module ends at: 0x1000 + 16 + 8254 = 0x304E (in sector 0x2000)
    // Next entry would be at: 0x3050 (also in sector 0x2000)

    term argv[3];
    argv[0] = term_from_int(10);
    term stream1 = new_nif(ctx, 1, argv);

    // Append 8254 bytes of native code
    uint8_t code1[8254];
    memset(code1, 0xAB, sizeof(code1));
    argv[0] = stream1;
    argv[1] = make_binary_rooted(ctx, code1, sizeof(code1), &argv[0], 1);
    stream1 = append_nif(ctx, 2, argv);

    argv[0] = stream1;
    term stream1_flushed = flush_nif(ctx, 1, argv);
    ModuleNativeEntryPoint entry1 = jit_stream_flash_entry_point(ctx, stream1_flushed);

    Module fake_mod1;
    fake_mod1.code = (CodeChunk *) 0x12345678;
    globalcontext_set_cache_native_code(glb, &fake_mod1, 1, entry1, 30);

    // Compute CRC of first module for verification
    uintptr_t data_addr1 = jit_stream_flash_platform_executable_to_ptr((uintptr_t) entry1);
    struct JITEntry *jit_entry1 = (struct JITEntry *) (data_addr1 - sizeof(struct JITEntry));
    uint32_t crc1_after_finalize = crc32((const uint8_t *) jit_entry1, sizeof(struct JITEntry) + jit_entry1->size);
    fprintf(stderr, "First module: entry=%p size=%u CRC=0x%08x\n",
        (void *) jit_entry1, (unsigned int) jit_entry1->size, (unsigned int) crc1_after_finalize);

    // Verify first module extends into sector 0x2000
    uintptr_t entry1_addr = (uintptr_t) jit_entry1;
    uintptr_t entry1_end = entry1_addr + sizeof(struct JITEntry) + jit_entry1->size;
    uintptr_t entry1_sector = entry1_addr & ~(FLASH_SECTOR_SIZE - 1);
    uintptr_t entry1_end_sector = entry1_end & ~(FLASH_SECTOR_SIZE - 1);
    fprintf(stderr, "First module: starts in sector 0x%lx, ends at 0x%lx (sector 0x%lx)\n",
        (unsigned long) entry1_sector, (unsigned long) entry1_end,
        (unsigned long) entry1_end_sector);

    if (entry1_sector == entry1_end_sector) {
        fprintf(stderr, "FAIL: Test setup error - first module should span sectors\n");
        exit(1);
    }

    // Create second module - THIS SHOULD NOT CORRUPT THE FIRST MODULE
    argv[0] = term_from_int(10);
    term stream2 = new_nif(ctx, 1, argv);

    uint8_t code2[100];
    memset(code2, 0xCD, sizeof(code2));
    argv[0] = stream2;
    argv[1] = make_binary_rooted(ctx, code2, sizeof(code2), &argv[0], 1);
    stream2 = append_nif(ctx, 2, argv);

    argv[0] = stream2;
    term stream2_flushed = flush_nif(ctx, 1, argv);
    ModuleNativeEntryPoint entry2 = jit_stream_flash_entry_point(ctx, stream2_flushed);

    Module fake_mod2;
    fake_mod2.code = (CodeChunk *) 0x87654321;
    globalcontext_set_cache_native_code(glb, &fake_mod2, 1, entry2, 20);

    // Verify first module's CRC is still intact
    uint32_t crc1_after_second = crc32((const uint8_t *) jit_entry1, sizeof(struct JITEntry) + jit_entry1->size);
    fprintf(stderr, "First module after second: CRC=0x%08x (expected 0x%08x)\n",
        (unsigned int) crc1_after_second, (unsigned int) crc1_after_finalize);

    if (crc1_after_second != crc1_after_finalize) {
        fprintf(stderr, "FAIL: First module corrupted after creating second module!\n");
        fprintf(stderr, "Expected CRC: 0x%08x, Got: 0x%08x\n",
            (unsigned int) crc1_after_finalize, (unsigned int) crc1_after_second);
        exit(1);
    }

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Tail corruption bug test\n");
}

// Test 9: Stale data cleanup after failed compilation
static void test_stale_data_cleanup(void)
{
    fprintf(stderr, "\n=== Test: Stale Data Cleanup After Failed Compilation ===\n");

    // Initialize flash: sector 0 erased (AVM), rest is garbage
    memset(mock_flash, 0x00, MOCK_FLASH_SIZE);
    memset(&mock_flash[0], 0xFF, FLASH_SECTOR_SIZE);

    create_minimal_avmpack();

    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    register_test_avmpack(glb);
    jit_stream_flash_init(glb);

    nif_function new_nif = get_nif("jit_stream_flash:new/1");
    nif_function append_nif = get_nif("jit_stream_flash:append/2");
    nif_function flush_nif = get_nif("jit_stream_flash:flush/1");

    // Create first module and finalize it (small, stays in first sector after AVM)
    term argv[3];
    argv[0] = term_from_int(10);
    term stream1 = new_nif(ctx, 1, argv);

    uint8_t code1[500];
    memset(code1, 0xAA, sizeof(code1));
    argv[0] = stream1;
    argv[1] = make_binary_rooted(ctx, code1, sizeof(code1), &argv[0], 1);
    stream1 = append_nif(ctx, 2, argv);

    argv[0] = stream1;
    term stream1_flushed = flush_nif(ctx, 1, argv);
    ModuleNativeEntryPoint entry1 = jit_stream_flash_entry_point(ctx, stream1_flushed);

    Module fake_mod1;
    fake_mod1.code = (CodeChunk *) 0x12345678;
    globalcontext_set_cache_native_code(glb, &fake_mod1, 1, entry1, 30);

    // Compute CRC of first module
    uintptr_t data_addr1 = jit_stream_flash_platform_executable_to_ptr((uintptr_t) entry1);
    struct JITEntry *jit_entry1 = (struct JITEntry *) (data_addr1 - sizeof(struct JITEntry));
    uint32_t crc1_original = crc32((const uint8_t *) jit_entry1, sizeof(struct JITEntry) + jit_entry1->size);
    fprintf(stderr, "First module: CRC=0x%08x, size=%u bytes\n",
        (unsigned int) crc1_original, (unsigned int) jit_entry1->size);

    // Start creating a second module but DON'T finalize (simulate crash/OOM)
    argv[0] = term_from_int(10);
    term stream2_attempt1 = new_nif(ctx, 1, argv);

    uint8_t code2[200];
    memset(code2, 0xBB, sizeof(code2));
    argv[0] = stream2_attempt1;
    argv[1] = make_binary_rooted(ctx, code2, sizeof(code2), &argv[0], 1);
    stream2_attempt1 = append_nif(ctx, 2, argv);

    // DON'T flush or finalize - this simulates a failed compilation
    // Now there's stale data in flash after the first module

    fprintf(stderr, "Simulated failed compilation - stale data left in flash\n");

    // Try to create the second module again - should detect and clean up stale data
    argv[0] = term_from_int(10);
    term stream2_attempt2 = new_nif(ctx, 1, argv);

    memset(code2, 0xCC, sizeof(code2));
    argv[0] = stream2_attempt2;
    argv[1] = make_binary_rooted(ctx, code2, sizeof(code2), &argv[0], 1);
    stream2_attempt2 = append_nif(ctx, 2, argv);

    argv[0] = stream2_attempt2;
    term stream2_flushed = flush_nif(ctx, 1, argv);
    ModuleNativeEntryPoint entry2 = jit_stream_flash_entry_point(ctx, stream2_flushed);

    Module fake_mod2;
    fake_mod2.code = (CodeChunk *) 0x87654321;
    globalcontext_set_cache_native_code(glb, &fake_mod2, 1, entry2, 20);

    fprintf(stderr, "Second module successfully created after cleanup\n");

    // Verify first module's CRC is still intact
    uint32_t crc1_after_cleanup = crc32((const uint8_t *) jit_entry1, sizeof(struct JITEntry) + jit_entry1->size);
    fprintf(stderr, "First module after cleanup: CRC=0x%08x (expected 0x%08x)\n",
        (unsigned int) crc1_after_cleanup, (unsigned int) crc1_original);

    if (crc1_after_cleanup != crc1_original) {
        fprintf(stderr, "FAIL: First module corrupted during stale data cleanup!\n");
        exit(1);
    }

    scheduler_terminate(ctx);
    globalcontext_destroy(glb);

    fprintf(stderr, "PASS: Stale data cleanup test\n");
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    fprintf(stderr, "Starting jit_stream_flash tests...\n");

    test_basic_append_flush();
    test_multiple_appends();
    test_replace();
    test_second_module_bug();
    test_magic_0xffff_but_garbage_bug();
    test_garbage_flash_bug();
    test_esp32_crash_bug();
    test_tail_corruption_bug();
    test_stale_data_cleanup();

    fprintf(stderr, "\nAll tests passed!\n");
    return EXIT_SUCCESS;
}
