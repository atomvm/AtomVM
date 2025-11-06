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

#include <hardware/flash.h>
#include <pico/flash.h>
#include <stdio.h>
#include <stdlib.h>

#include "rp2_sys.h"

// Helper structures for flash_safe_execute
struct EraseParams
{
    uintptr_t addr;
};

struct WriteParams
{
    uintptr_t addr;
    const uint8_t *data;
    size_t len;
};

static void __not_in_flash_func(do_erase_sector)(void *params_ptr)
{
    struct EraseParams *params = (struct EraseParams *) params_ptr;
    flash_range_erase(params->addr - XIP_BASE, FLASH_SECTOR_SIZE);
}

static void __not_in_flash_func(do_write_page)(void *params_ptr)
{
    struct WriteParams *params = (struct WriteParams *) params_ptr;
    flash_range_program(params->addr - XIP_BASE, params->data, params->len);
}

struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    return (struct JSFlashPlatformContext *) 1;
}

void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *pf_ctx)
{
    UNUSED(pf_ctx);
}

bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr)
{
    UNUSED(pf_ctx);

    struct EraseParams params = {
        .addr = addr
    };

    int r = flash_safe_execute(do_erase_sector, &params, UINT32_MAX);
    if (UNLIKELY(r != PICO_OK)) {
        fprintf(stderr, "flash_safe_execute (erase) failed with error %d\n", r);
        return false;
    }

    return true;
}

bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data)
{
    UNUSED(pf_ctx);

    struct WriteParams params = {
        .addr = addr,
        .data = data,
        .len = FLASH_PAGE_SIZE
    };

    int r = flash_safe_execute(do_write_page, &params, UINT32_MAX);
    if (UNLIKELY(r != PICO_OK)) {
        fprintf(stderr, "flash_safe_execute (write) failed with error %d\n", r);
        return false;
    }

    return true;
}

uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr)
{
    // Set Thumb bit
    return addr | 0x1;
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    // Clear Thumb bit
    return addr & ~0x1UL;
}

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

#endif // AVM_NO_JIT
