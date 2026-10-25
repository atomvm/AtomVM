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

/*
 * The NVS and filesystem partitions take the last 256 KiB of the flash chip,
 * so their offsets follow the size the part reports: 0x3C0000 and 0x3E0000 on
 * a 4 MiB chip, 0x1C0000 and 0x1E0000 on a 2 MiB one.  The direct-boot
 * firmware occupies everything below them.
 */
#ifndef DEFAULT_NVS_FLASH_SIZE
#define DEFAULT_NVS_FLASH_SIZE 0x00020000U /* 128 KiB (32 sectors) */
#endif

#ifndef DEFAULT_FS_FLASH_SIZE
#define DEFAULT_FS_FLASH_SIZE 0x00020000U /* 128 KiB (32 sectors) */
#endif

#define FLASH_PARTITION_RESERVE (DEFAULT_NVS_FLASH_SIZE + DEFAULT_FS_FLASH_SIZE)

uint32_t flash_partition_nvs_offset(void);
uint32_t flash_partition_fs_offset(void);

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
