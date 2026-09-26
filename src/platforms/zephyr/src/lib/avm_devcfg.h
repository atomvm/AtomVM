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

#include <autoconf.h>

#if (CONFIG_FLASH_SIZE == 512)
#define AVM_APP_ADDRESS ((CONFIG_FLASH_BASE_ADDRESS) + 0x60000U)
#else
#define AVM_APP_ADDRESS ((CONFIG_FLASH_BASE_ADDRESS) + 0x80000U)
#endif

#define CFG_FLASH_END ((uint32_t) ((CONFIG_FLASH_BASE_ADDRESS) + ((CONFIG_FLASH_SIZE) *1024)))

#endif /* _AVM_DEVCFG_H_ */
