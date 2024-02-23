/*
 * This file is part of AtomVM.
 *
 * Copyright 2024 Davide Bettio <davide@uninstall.it>
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

#ifndef _UNICODE_H_
#define _UNICODE_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

size_t unicode_buf_utf8_len(const uint8_t *buf, size_t buf_len);
bool unicode_buf_is_ascii(const uint8_t *buf, size_t buf_len);

static inline bool unicode_is_valid_codepoint(uint32_t codepoint)
{
    // 0x110000 - 0x1FFFFF are not valid codepoints
    // 0xD800 - 0xDFFF are surrogates
    return (codepoint < 0x110000) && !((codepoint > 0xD800) && (codepoint < 0xDFFF));
}

#ifdef __cplusplus
}
#endif

#endif
