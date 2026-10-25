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

#include "flash_driver.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <rtems.h>

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#if defined(RTEMS_BOARD_ESP32)

#include <esp_rom_spiflash.h>

// Prototypes for ROM symbols
extern uint32_t spi_flash_get_chip_size(void);
#if defined(RTEMS_BOARD_ESP32P4)
extern int Cache_Invalidate_Addr(uint32_t map, uint32_t addr, uint32_t size);
#elif defined(RTEMS_BOARD_ESP32S31)
/* The S31 ROM exposes the generic cache invalidate rather than the I-cache
 * specific one named by the other parts. */
extern void Cache_Invalidate_All(void);
#define Cache_Invalidate_ICache_All Cache_Invalidate_All
#else
extern void Cache_Invalidate_ICache_All(void);
#endif
extern void spi_flash_check_and_flush_cache(uint32_t);
extern esp_rom_spiflash_result_t esp_rom_spiflash_unlock(void);

static uint32_t s_flash_chip_size = 0;
RTEMS_INTERRUPT_LOCK_DEFINE(static, s_flash_lock, "flash_lock")

static void flash_cache_flush(uint32_t offset, uint32_t size)
{
#if defined(RTEMS_BOARD_ESP32P4)
    /* ESP-IDF rom/cache.h CACHE_MAP_MASK: both instruction caches, D-cache
     * and L2. Invalidate only mapped flash, preserving dirty RAM cache lines. */
    Cache_Invalidate_Addr((1U << 0) | (1U << 1) | (1U << 4) | (1U << 5), 0x40000000U + offset, size);
#else
    UNUSED(offset);
    UNUSED(size);
    Cache_Invalidate_ICache_All();
#endif
}

bool esp32c3_flash_init(void)
{
    if (s_flash_chip_size == 0) {
        if (rom_spiflash_legacy_data != NULL) {
            if (rom_spiflash_legacy_data->chip.chip_size < 4 * 1024 * 1024) {
                rom_spiflash_legacy_data->chip.chip_size = 4 * 1024 * 1024;
            }
            s_flash_chip_size = rom_spiflash_legacy_data->chip.chip_size;
        } else {
            s_flash_chip_size = 4 * 1024 * 1024;
        }
        TRACE("Flash initialized: size = %u bytes (%u MiB)\n", (unsigned) s_flash_chip_size, (unsigned) (s_flash_chip_size / (1024 * 1024)));
    }
    return true;
}

uint32_t esp32c3_flash_get_size(void)
{
    if (s_flash_chip_size == 0) {
        esp32c3_flash_init();
    }
    return s_flash_chip_size;
}

int esp32c3_flash_read(uint32_t offset, void *dest, size_t size)
{
    if (size == 0) {
        return 0;
    }
    if (dest == NULL) {
        return -1;
    }
    uint32_t chip_size = esp32c3_flash_get_size();
    if (offset >= chip_size || size > chip_size - offset) {
        return -1;
    }

    uint8_t *out = (uint8_t *) dest;

    // Fast path: 4-byte aligned offset, length, and destination pointer
    if ((offset & 3U) == 0 && (size & 3U) == 0 && (((uintptr_t) dest) & 3U) == 0) {
        esp_rom_spiflash_result_t res = esp_rom_spiflash_read(offset, (uint32_t *) dest, (int32_t) size);
        return (res == ESP_ROM_SPIFLASH_RESULT_OK) ? 0 : -1;
    }

    // Generic path: handle unaligned offsets, sizes, and buffer pointers
    uint32_t current_offset = offset;
    size_t remaining = size;
    uint32_t word_buf[16]; // 64-byte stack chunk, 4-byte aligned

    while (remaining > 0) {
        uint32_t aligned_start = current_offset & ~3U;
        uint32_t chunk_words = sizeof(word_buf) / 4;
        uint32_t chunk_bytes = chunk_words * 4;

        if (aligned_start + chunk_bytes > chip_size) {
            chunk_bytes = chip_size - aligned_start;
            chunk_words = (chunk_bytes + 3) / 4;
            chunk_bytes = chunk_words * 4;
        }

        esp_rom_spiflash_result_t res = esp_rom_spiflash_read(aligned_start, word_buf, (int32_t) chunk_bytes);
        if (res != ESP_ROM_SPIFLASH_RESULT_OK) {
            return -1;
        }

        uint32_t lead_in = current_offset - aligned_start;
        uint32_t available = (chunk_bytes > lead_in) ? (chunk_bytes - lead_in) : 0;
        size_t to_copy = (remaining < available) ? remaining : available;

        memcpy(out, ((const uint8_t *) word_buf) + lead_in, to_copy);
        out += to_copy;
        current_offset += to_copy;
        remaining -= to_copy;
    }

    return 0;
}

int esp32c3_flash_write(uint32_t offset, const void *src, size_t size)
{
    if (size == 0) {
        return 0;
    }
    if (src == NULL) {
        return -1;
    }
    uint32_t chip_size = esp32c3_flash_get_size();
    if (offset >= chip_size || size > chip_size - offset) {
        return -1;
    }

    const uint8_t *in = (const uint8_t *) src;
    uint32_t current_offset = offset;
    size_t remaining = size;
    uint32_t word_buf[16]; // 64-byte stack chunk, 4-byte aligned

    while (remaining > 0) {
        uint32_t aligned_offset = current_offset & ~3U;
        uint32_t lead_in = current_offset - aligned_offset;
        uint32_t chunk_size = (remaining > (sizeof(word_buf) - lead_in)) ? (sizeof(word_buf) - lead_in) : remaining;
        uint32_t aligned_write_len = ((lead_in + chunk_size + 3U) & ~3U);

        if (aligned_write_len > sizeof(word_buf)) {
            aligned_write_len = sizeof(word_buf);
            chunk_size = aligned_write_len - lead_in;
        }

        // If not completely aligned at start or end, read existing words first
        if (lead_in != 0 || (chunk_size & 3U) != 0) {
            if (esp_rom_spiflash_read(aligned_offset, word_buf, (int32_t) aligned_write_len) != ESP_ROM_SPIFLASH_RESULT_OK) {
                return -1;
            }
        }

        memcpy(((uint8_t *) word_buf) + lead_in, in, chunk_size);

        // Lock interrupts and cross-core access while writing flash to protect cache coherence
        rtems_interrupt_lock_context lock_context;
        rtems_interrupt_lock_acquire(&s_flash_lock, &lock_context);

        esp_rom_spiflash_unlock();
        esp_rom_spiflash_result_t res = esp_rom_spiflash_write(aligned_offset, word_buf, (int32_t) aligned_write_len);
        flash_cache_flush(aligned_offset, aligned_write_len);

        rtems_interrupt_lock_release(&s_flash_lock, &lock_context);

        if (res != ESP_ROM_SPIFLASH_RESULT_OK) {
            return -1;
        }

        in += chunk_size;
        current_offset += chunk_size;
        remaining -= chunk_size;
    }

    return 0;
}

int esp32c3_flash_erase_sector(uint32_t sector_num)
{
    uint32_t chip_size = esp32c3_flash_get_size();
    uint32_t total_sectors = chip_size / FLASH_SECTOR_SIZE;
    if (sector_num >= total_sectors) {
        return -1;
    }

    rtems_interrupt_lock_context lock_context;
    rtems_interrupt_lock_acquire(&s_flash_lock, &lock_context);

    esp_rom_spiflash_unlock();
    esp_rom_spiflash_result_t res = esp_rom_spiflash_erase_sector(sector_num);
    flash_cache_flush(sector_num * FLASH_SECTOR_SIZE, FLASH_SECTOR_SIZE);

    rtems_interrupt_lock_release(&s_flash_lock, &lock_context);

    return (res == ESP_ROM_SPIFLASH_RESULT_OK) ? 0 : -1;
}

int esp32c3_flash_erase_range(uint32_t offset, size_t size)
{
    if (size == 0) {
        return 0;
    }
    if ((offset % FLASH_SECTOR_SIZE) != 0 || (size % FLASH_SECTOR_SIZE) != 0) {
        return -1;
    }
    uint32_t chip_size = esp32c3_flash_get_size();
    if (offset >= chip_size || size > chip_size - offset) {
        return -1;
    }

    uint32_t start_sector = offset / FLASH_SECTOR_SIZE;
    uint32_t num_sectors = size / FLASH_SECTOR_SIZE;

    for (uint32_t i = 0; i < num_sectors; i++) {
        if (esp32c3_flash_erase_sector(start_sector + i) != 0) {
            return -1;
        }
    }

    return 0;
}

#else /* !defined(RTEMS_BOARD_ESP32) */

bool esp32c3_flash_init(void)
{
    return false;
}

uint32_t esp32c3_flash_get_size(void)
{
    return 0;
}

int esp32c3_flash_read(uint32_t offset, void *dest, size_t size)
{
    UNUSED(offset);
    UNUSED(dest);
    UNUSED(size);
    return -1;
}

int esp32c3_flash_write(uint32_t offset, const void *src, size_t size)
{
    UNUSED(offset);
    UNUSED(src);
    UNUSED(size);
    return -1;
}

int esp32c3_flash_erase_sector(uint32_t sector_num)
{
    UNUSED(sector_num);
    return -1;
}

int esp32c3_flash_erase_range(uint32_t offset, size_t size)
{
    UNUSED(offset);
    UNUSED(size);
    return -1;
}

#endif /* !defined(RTEMS_BOARD_ESP32) */

//
// NIF implementations
//

// esp:flash_get_size/0, atomvm_rtems:flash_get_size/0
static term nif_esp_flash_get_size(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    uint32_t size = esp32c3_flash_get_size();
    return term_from_int(size);
}

// esp:flash_read/2, atomvm_rtems:flash_read/2 (Offset, Length)
static term nif_esp_flash_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    avm_int_t offset = term_to_int(argv[0]);
    avm_int_t len = term_to_int(argv[1]);

    if (offset < 0 || len < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint32_t chip_size = esp32c3_flash_get_size();
    if ((uint32_t) offset >= chip_size || (uint32_t) len > chip_size - (uint32_t) offset) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(len) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term binary = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    void *buf = (void *) term_binary_data(binary);

    if (esp32c3_flash_read((uint32_t) offset, buf, (size_t) len) != 0) {
        return ERROR_ATOM;
    }

    term res = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(res, 0, OK_ATOM);
    term_put_tuple_element(res, 1, binary);
    return res;
}

// esp:flash_write/2, atomvm_rtems:flash_write/2 (Offset, Binary)
static term nif_esp_flash_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_binary);

    avm_int_t offset = term_to_int(argv[0]);
    if (offset < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term binary = argv[1];
    size_t size = term_binary_size(binary);
    const void *data = (const void *) term_binary_data(binary);

    uint32_t chip_size = esp32c3_flash_get_size();
    if ((uint32_t) offset >= chip_size || size > chip_size - (uint32_t) offset) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (esp32c3_flash_write((uint32_t) offset, data, size) != 0) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

// esp:flash_erase_sector/1, atomvm_rtems:flash_erase_sector/1 (SectorNum)
static term nif_esp_flash_erase_sector(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    avm_int_t sector = term_to_int(argv[0]);
    if (sector < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint32_t chip_size = esp32c3_flash_get_size();
    if ((uint32_t) sector >= chip_size / FLASH_SECTOR_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (esp32c3_flash_erase_sector((uint32_t) sector) != 0) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

// esp:flash_erase_range/2, atomvm_rtems:flash_erase_range/2 (Offset, Length)
static term nif_esp_flash_erase_range(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    avm_int_t offset = term_to_int(argv[0]);
    avm_int_t len = term_to_int(argv[1]);

    if (offset < 0 || len < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if ((offset % FLASH_SECTOR_SIZE) != 0 || (len % FLASH_SECTOR_SIZE) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint32_t chip_size = esp32c3_flash_get_size();
    if ((uint32_t) offset >= chip_size || (uint32_t) len > chip_size - (uint32_t) offset) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (esp32c3_flash_erase_range((uint32_t) offset, (size_t) len) != 0) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

static const struct Nif esp_flash_get_size_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_get_size
};
static const struct Nif esp_flash_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_read
};
static const struct Nif esp_flash_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_write
};
static const struct Nif esp_flash_erase_sector_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_erase_sector
};
static const struct Nif esp_flash_erase_range_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_flash_erase_range
};

void flash_nif_init(GlobalContext *global)
{
    UNUSED(global);
    esp32c3_flash_init();
}

const struct Nif *flash_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:flash_get_size/0", nifname) == 0 || strcmp("atomvm_rtems:flash_get_size/0", nifname) == 0) {
        return &esp_flash_get_size_nif;
    }
    if (strcmp("esp:flash_read/2", nifname) == 0 || strcmp("atomvm_rtems:flash_read/2", nifname) == 0) {
        return &esp_flash_read_nif;
    }
    if (strcmp("esp:flash_write/2", nifname) == 0 || strcmp("atomvm_rtems:flash_write/2", nifname) == 0) {
        return &esp_flash_write_nif;
    }
    if (strcmp("esp:flash_erase_sector/1", nifname) == 0 || strcmp("atomvm_rtems:flash_erase_sector/1", nifname) == 0) {
        return &esp_flash_erase_sector_nif;
    }
    if (strcmp("esp:flash_erase_range/2", nifname) == 0 || strcmp("atomvm_rtems:flash_erase_range/2", nifname) == 0) {
        return &esp_flash_erase_range_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(flash, flash_nif_init, NULL, flash_nif_get_nif)
