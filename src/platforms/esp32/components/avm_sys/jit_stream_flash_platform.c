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

#include "jit_stream_flash.h"

#include <esp_partition.h>
#include <stdio.h>

#include "esp32_sys.h"

#if ESP_IDF_VERSION_MAJOR >= 5
#include <spi_flash_mmap.h>
#endif

#ifdef CONFIG_IDF_TARGET_ARCH_RISCV
#include <soc/ext_mem_defs.h>
#endif

struct JSFlashPlatformContext
{
    const esp_partition_t *partition;
};

struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    const esp_partition_t *partition = esp_partition_find_first(
        ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, JIT_PARTITION_NAME);
    if (IS_NULL_PTR(partition)) {
        fprintf(stderr, "Failed to find partition '%s' for JIT cache\n", JIT_PARTITION_NAME);
        return NULL;
    }

    struct JSFlashPlatformContext *pf_ctx = malloc(sizeof(struct JSFlashPlatformContext));
    if (IS_NULL_PTR(pf_ctx)) {
        return NULL;
    }

    pf_ctx->partition = partition;
    return pf_ctx;
}

void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *ctx)
{
    free(ctx);
}

bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *ctx, uintptr_t addr)
{
    if (UNLIKELY(!ctx || !ctx->partition)) {
        return false;
    }

    size_t flash_offset = spi_flash_cache2phys((const void *) addr);
    if (UNLIKELY(flash_offset == SPI_FLASH_CACHE2PHYS_FAIL)) {
        fprintf(stderr, "Failed to convert cache address 0x%lx to physical address\n", (unsigned long) addr);
        return false;
    }

    esp_err_t err = esp_partition_erase_range(ctx->partition,
        flash_offset - ctx->partition->address, FLASH_SECTOR_SIZE);
    if (UNLIKELY(err != ESP_OK)) {
        fprintf(stderr, "Failed to erase sector at offset 0x%lx: %d\n", (unsigned long) flash_offset, err);
        return false;
    }

    return true;
}

bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *ctx, uintptr_t addr, const uint8_t *data)
{
    if (UNLIKELY(!ctx || !ctx->partition)) {
        return false;
    }

    size_t flash_offset = spi_flash_cache2phys((const void *) addr);
    if (UNLIKELY(flash_offset == SPI_FLASH_CACHE2PHYS_FAIL)) {
        fprintf(stderr, "Failed to convert cache address 0x%lx to physical address\n", (unsigned long) addr);
        return false;
    }

    esp_err_t err = esp_partition_write(ctx->partition,
        flash_offset - ctx->partition->address, data, FLASH_PAGE_SIZE);
    if (UNLIKELY(err != ESP_OK)) {
        fprintf(stderr, "Failed to write page at offset 0x%lx: %d\n", (unsigned long) flash_offset, err);
        return false;
    }

    return true;
}

uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr)
{
    // Convert data cache address to instruction cache address for RISC-V targets
    // On ESP32-C3/C6/H2, flash is mapped to both DBUS (0x3C...) and IBUS (0x42...)
    // but only IBUS addresses are executable
#ifdef CONFIG_IDF_TARGET_ARCH_RISCV
    if ((addr & ~SOC_MMU_VADDR_MASK) == SOC_MMU_DBUS_VADDR_BASE) {
        return (addr & SOC_MMU_VADDR_MASK) | SOC_MMU_IBUS_VADDR_BASE;
    }
    return addr;
#else
    return addr;
#endif
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    // Convert instruction cache address to data cache address for RISC-V targets
    // This is the reverse of ptr_to_executable
#ifdef CONFIG_IDF_TARGET_ARCH_RISCV
    if ((addr & ~SOC_MMU_VADDR_MASK) == SOC_MMU_IBUS_VADDR_BASE) {
        return (addr & SOC_MMU_VADDR_MASK) | SOC_MMU_DBUS_VADDR_BASE;
    }
    return addr;
#else
    return addr;
#endif
}

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

#endif // AVM_NO_JIT
