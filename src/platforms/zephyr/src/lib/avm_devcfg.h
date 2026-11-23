/* This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#ifndef _AVM_DEVCFG_H_
#define _AVM_DEVCFG_H_

#include <stdint.h>

#include <zephyr/autoconf.h>
#include <zephyr/devicetree.h>

#ifndef CONFIG_FLASH_BASE_ADDRESS
#define CONFIG_FLASH_BASE_ADDRESS 0U
#endif

#if DT_HAS_CHOSEN(zephyr_flash)
#define AVM_FLASH_SIZE_BYTES DT_REG_SIZE(DT_CHOSEN(zephyr_flash))
#else
#ifndef CONFIG_FLASH_SIZE
#define CONFIG_FLASH_SIZE 2048U
#endif
#define AVM_FLASH_SIZE_BYTES ((CONFIG_FLASH_SIZE) * 1024U)
#endif

#if defined(CONFIG_AVM_APP_ADDRESS) && (CONFIG_AVM_APP_ADDRESS != 0)
#define AVM_APP_ADDRESS (CONFIG_AVM_APP_ADDRESS)
#elif (AVM_FLASH_SIZE_BYTES <= (512U * 1024U))
#define AVM_APP_ADDRESS ((CONFIG_FLASH_BASE_ADDRESS) + 0x60000U)
#elif (AVM_FLASH_SIZE_BYTES <= (1024U * 1024U))
#define AVM_APP_ADDRESS ((CONFIG_FLASH_BASE_ADDRESS) + 0x80000U)
#else
#define AVM_APP_ADDRESS ((CONFIG_FLASH_BASE_ADDRESS) + 0x100000U)
#endif

#if defined(CONFIG_AVM_APP_SIZE) && (CONFIG_AVM_APP_SIZE != 0)
#define AVM_APP_MAX_SIZE ((uint32_t) (CONFIG_AVM_APP_SIZE))
#define CFG_FLASH_END ((uintptr_t) (AVM_APP_ADDRESS) + (uintptr_t) (CONFIG_AVM_APP_SIZE))
#else
#define CFG_FLASH_END ((uintptr_t) (CONFIG_FLASH_BASE_ADDRESS) + (uintptr_t) AVM_FLASH_SIZE_BYTES)
#define AVM_APP_MAX_SIZE ((uint32_t) (CFG_FLASH_END - (uintptr_t) (AVM_APP_ADDRESS)))
#endif

#endif /* _AVM_DEVCFG_H_ */
