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

#ifndef CONFIG_IDF_TARGET_ARCH_RISCV
// On Xtensa, flash DROM (0x3F4xxxxx) is not executable; code must run from IROM.
// We keep a permanent IBUS mmap alongside the existing DBUS mmap so that
// ptr_to_executable / executable_to_ptr can convert between the two.
static spi_flash_mmap_handle_t g_ibus_handle;
static uintptr_t g_ibus_base = 0;
static uintptr_t g_dbus_base = 0;
static bool g_xtensa_mmap_initialized = false;
#endif

struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    const esp_partition_t *partition = esp_partition_find_first(
        ESP_PARTITION_TYPE_DATA, ESP_PARTITION_SUBTYPE_ANY, JIT_PARTITION_NAME);
    if (IS_NULL_PTR(partition)) {
        fprintf(stderr, "Failed to find partition '%s' for JIT cache\n", JIT_PARTITION_NAME);
        return NULL;
    }

#ifndef CONFIG_IDF_TARGET_ARCH_RISCV
    if (!g_xtensa_mmap_initialized) {
        // Map partition via instruction bus so that the native code is executable.
        const void *ibus_ptr;
        spi_flash_mmap_handle_t ibus_handle;
        esp_err_t err = esp_partition_mmap(partition, 0, partition->size, SPI_FLASH_MMAP_INST, &ibus_ptr, &ibus_handle);
        if (UNLIKELY(err != ESP_OK)) {
            fprintf(stderr, "Failed to map JIT partition for instruction access: %d\n", err);
            return NULL;
        }

        // Map via data bus as well to learn the DBUS base address.
        // ESP-IDF reuses existing MMU pages, so this returns the same virtual
        // address as the mapping already created by esp32_sys_mmap_partition.
        const void *dbus_ptr;
        spi_flash_mmap_handle_t dbus_handle;
        err = esp_partition_mmap(partition, 0, partition->size, SPI_FLASH_MMAP_DATA, &dbus_ptr, &dbus_handle);
        if (UNLIKELY(err != ESP_OK)) {
            spi_flash_munmap(ibus_handle);
            fprintf(stderr, "Failed to map JIT partition for data access: %d\n", err);
            return NULL;
        }
        // Release our extra DBUS reference; the original mapping from
        // esp32_sys_mmap_partition keeps the pages live for the lifetime of the VM.
        spi_flash_munmap(dbus_handle);

        g_ibus_handle = ibus_handle;
        g_ibus_base = (uintptr_t) ibus_ptr;
        g_dbus_base = (uintptr_t) dbus_ptr;
        g_xtensa_mmap_initialized = true;
    }
#endif

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
    // Convert data cache (DBUS) address to instruction cache (IBUS) address.
    // On RISC-V targets the DBUS and IBUS windows share the same MMU pages but
    // live at different base addresses that differ only in the upper bits.
    // On Xtensa targets we maintain a permanent IBUS mmap whose base address is
    // stored in g_ibus_base; the corresponding DBUS base is in g_dbus_base.
#ifdef CONFIG_IDF_TARGET_ARCH_RISCV
    if ((addr & ~SOC_MMU_VADDR_MASK) == SOC_MMU_DBUS_VADDR_BASE) {
        return (addr & SOC_MMU_VADDR_MASK) | SOC_MMU_IBUS_VADDR_BASE;
    }
    return addr;
#else
    if (UNLIKELY(!g_xtensa_mmap_initialized)) {
        // Returning addr here would hand out a non-executable DROM pointer.
        // Return 0 so callers crash early with a clear NULL deref rather than
        // jumping into data memory.
        fprintf(stderr, "jit_stream_flash_platform_ptr_to_executable: "
                        "g_xtensa_mmap_initialized is false for addr 0x%lx\n",
            (unsigned long) addr);
        return 0;
    }
    return g_ibus_base + (addr - g_dbus_base);
#endif
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    // Reverse of ptr_to_executable: IBUS address -> DBUS address.
#ifdef CONFIG_IDF_TARGET_ARCH_RISCV
    if ((addr & ~SOC_MMU_VADDR_MASK) == SOC_MMU_IBUS_VADDR_BASE) {
        return (addr & SOC_MMU_VADDR_MASK) | SOC_MMU_DBUS_VADDR_BASE;
    }
    return addr;
#else
    if (UNLIKELY(!g_xtensa_mmap_initialized)) {
        fprintf(stderr, "jit_stream_flash_platform_executable_to_ptr: "
                        "g_xtensa_mmap_initialized is false for addr 0x%lx\n",
            (unsigned long) addr);
        return 0;
    }
    return g_dbus_base + (addr - g_ibus_base);
#endif
}

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

#endif // AVM_NO_JIT
