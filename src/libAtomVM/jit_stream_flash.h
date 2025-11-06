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

/**
 * @file jit_stream_flash.h
 * @brief JIT code caching in flash memory - common implementation
 */

#ifndef _JIT_STREAM_FLASH_H_
#define _JIT_STREAM_FLASH_H_

#include "globalcontext.h"
#include "jit_stream_flash_platform.h"
#include "module.h"

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Platform-specific flash context (opaque)
 */
struct JSFlashPlatformContext;

/**
 * @brief Initialize JIT stream flash subsystem
 *
 * @param global Global context
 */
void jit_stream_flash_init(GlobalContext *global);

/**
 * @brief Get NIF for jit_stream_flash operations
 *
 * @param nifname NIF name
 * @return NIF pointer or NULL
 */
const struct Nif *jit_stream_flash_get_nif(const char *nifname);

/**
 * @brief Get entry point from jit_stream_flash.
 * Called by `jit_stream_entry_point`
 *
 * @param ctx Context
 * @param jit_stream JIT stream term
 * @return Entry point or NULL
 */
ModuleNativeEntryPoint jit_stream_flash_entry_point(Context *ctx, term jit_stream);

/**
 * @brief Finalize flash operation by marking an entry point as valid for
 * a given module. This is called by `sys_set_cache_native_code`.
 *
 * @param global Global context
 * @param mod Module
 * @param version Module version
 * @param entry_point Entry point
 * @param labels Number of labels
 */
void globalcontext_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels);

/**
 * @brief Initialize platform flash context
 * @return Platform flash context, or NULL on error
 */
struct JSFlashPlatformContext *jit_stream_flash_platform_init(void);

/**
 * @brief Destroy platform flash context
 * @param pf_ctx Platform flash context to destroy
 */
void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *pf_ctx);

/**
 * @brief Erase a flash sector at the given address
 * @param pf_ctx Platform flash context
 * @param addr Virtual address of the sector to erase (must be sector-aligned)
 * @return true on success, false on error
 */
bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr);

/**
 * @brief Write a page to flash
 * @param pf_ctx Platform flash context
 * @param addr Virtual address to write to (must be page-aligned)
 * @param data Data to write (must be FLASH_PAGE_SIZE bytes)
 * @return true on success, false on error
 */
bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data);

/**
 * @brief Convert data bus address to instruction bus address
 * @param addr Data bus address
 * @return Instruction bus address (executable pointer)
 */
uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr);

/**
 * @brief Convert instruction bus address to data bus address
 * @param addr Instruction bus address (executable pointer)
 * @return Data bus address
 */
uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr);

#ifdef __cplusplus
}
#endif

#endif // _JIT_STREAM_FLASH_H_
