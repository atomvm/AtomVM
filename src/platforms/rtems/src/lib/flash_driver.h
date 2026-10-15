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

#ifndef _FLASH_DRIVER_H
#define _FLASH_DRIVER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <context.h>
#include <globalcontext.h>
#include <nifs.h>

#ifdef __cplusplus
extern "C" {
#endif

#define FLASH_SECTOR_SIZE 4096U
#define FLASH_PAGE_SIZE 256U
#define FLASH_BLOCK_SIZE 65536U

// Default flash partition layout on 4 MiB chip (0x400000):
// 0x000000 - ~0x39A000: Direct-boot firmware (text + data + embedded avm)
// 0x3C0000 -  0x3E0000: NVS partition (128 KiB = 32 sectors)
// 0x3E0000 -  0x400000: Flash filesystem mount / storage partition (128 KiB = 32 sectors)
#ifndef DEFAULT_NVS_FLASH_OFFSET
#define DEFAULT_NVS_FLASH_OFFSET 0x003C0000U
#endif

#ifndef DEFAULT_NVS_FLASH_SIZE
#define DEFAULT_NVS_FLASH_SIZE 0x00020000U /* 128 KiB (32 sectors) */
#endif

#ifndef DEFAULT_FS_FLASH_OFFSET
#define DEFAULT_FS_FLASH_OFFSET 0x003E0000U
#endif

#ifndef DEFAULT_FS_FLASH_SIZE
#define DEFAULT_FS_FLASH_SIZE 0x00020000U /* 128 KiB (32 sectors) */
#endif

bool esp32c3_flash_init(void);
uint32_t esp32c3_flash_get_size(void);
int esp32c3_flash_read(uint32_t offset, void *dest, size_t size);
int esp32c3_flash_write(uint32_t offset, const void *src, size_t size);
int esp32c3_flash_erase_sector(uint32_t sector_num);
int esp32c3_flash_erase_range(uint32_t offset, size_t size);

void flash_nif_init(GlobalContext *global);
const struct Nif *flash_nif_get_nif(const char *nifname);

#ifdef __cplusplus
}
#endif

#endif /* _FLASH_DRIVER_H */
