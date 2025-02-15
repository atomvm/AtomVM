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

enum UnicodeTransformDecodeResult
{
    UnicodeTransformDecodeSuccess,
    UnicodeTransformDecodeFail,
    UnicodeTransformDecodeIncomplete
};

size_t unicode_buf_utf8_len(const uint8_t *buf, size_t buf_len);
bool unicode_buf_is_ascii(const uint8_t *buf, size_t buf_len);
size_t unicode_latin1_buf_size_as_utf8(const uint8_t *buf, size_t len);

static inline bool unicode_is_valid_codepoint(uint32_t codepoint)
{
    // 0x110000 - 0x1FFFFF are not valid codepoints
    // 0xD800 - 0xDFFF are surrogates
    return (codepoint < 0x110000) && !((codepoint > 0xD800) && (codepoint < 0xDFFF));
}

/**
 * @brief Decode a character from UTF-8.
 *
 * @param buf the buffer from which to decode the string
 * @param len the length (in bytes) of the bytes in buf
 * @param c int value to decode to
 * @param out_size the size in bytes, on output (if not NULL)
 * @return \c UnicodeTransformDecodeSuccess if decoding was successful,
 * \c UnicodeTransformDecodeFail if character starting at buf is not a valid
 * unicode character or \c UnicodeTransformDecodeIncomplete if character
 * starting at buf is a valid but incomplete transformation
 */
enum UnicodeTransformDecodeResult unicode_utf8_decode(
    const uint8_t *buf, size_t len, uint32_t *c, size_t *out_size);

bool unicode_is_valid_utf8_buf(const uint8_t *buf, size_t len);

#ifdef __cplusplus
}
#endif

#endif
