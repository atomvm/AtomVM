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

#ifndef _JIT_STREAM_FLASH_PLATFORM_H_
#define _JIT_STREAM_FLASH_PLATFORM_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Host test flash constants
#define FLASH_SECTOR_SIZE 4096
#define FLASH_PAGE_SIZE 256

#ifdef __cplusplus
}
#endif

#endif // _JIT_STREAM_FLASH_PLATFORM_H_
