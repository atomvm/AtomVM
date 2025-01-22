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
 * SPDX-License-Identifier: (Apache-2.0 OR LGPL-2.1-or-later) AND MIT
 */

#include <stdbool.h>
#include <stddef.h>

#include "utils.h"

#include "unicode.h"

// clang-format off

// Following utf8d table and decode function are covered by MIT license
// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

static inline uint32_t decode(uint32_t* state, uint32_t* codep, uint32_t byte)
{
  uint32_t type = utf8d[byte];

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  *state = utf8d[256 + *state + type];
  return *state;
}

// clang-format on

enum UnicodeTransformDecodeResult unicode_utf8_decode(
    const uint8_t *buf, size_t len, uint32_t *c, size_t *out_size)
{
    uint32_t codepoint = 0;
    uint32_t state = 0;
    size_t i = 0;
    while (i < len) {
        state = decode(&state, &codepoint, buf[i]);
        i++;

        if (state == UTF8_ACCEPT) {
            *c = codepoint;
            *out_size = i;
            return UnicodeTransformDecodeSuccess;
        } else if (UNLIKELY(state == UTF8_REJECT)) {
            return UnicodeTransformDecodeFail;
        }
    }

    return UnicodeTransformDecodeIncomplete;
}

bool unicode_is_valid_utf8_buf(const uint8_t *buf, size_t len)
{
    uint32_t codepoint = 0;
    uint32_t state = 0;

    for (size_t i = 0; i < len; i++) {
        state = decode(&state, &codepoint, buf[i]);
    }

    return state == UTF8_ACCEPT;
}

size_t unicode_buf_utf8_len(const uint8_t *buf, size_t buf_len)
{
    size_t count = 0;

    for (size_t i = 0; i < buf_len; i++) {
        // we count either ASCII characters or the first byte of a unicode sequence
        if ((buf[i] & 0xC0) != 0x80) {
            count++;
        }
    }

    return count;
}

bool unicode_buf_is_ascii(const uint8_t *buf, size_t len)
{
    for (size_t i = 0; i < len; i++) {
        if (buf[i] > 0x7F) {
            return false;
        }
    }

    return true;
}

size_t unicode_latin1_buf_size_as_utf8(const uint8_t *buf, size_t len)
{
    size_t required_size = 0;
    for (size_t i = 0; i < len; i++) {
        required_size += (buf[i] > 0x7F) ? 2 : 1;
    }

    return required_size;
}
