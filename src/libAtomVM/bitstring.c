/*
 * This file is part of AtomVM.
 *
 * Copyright 2020 Davide Bettio <davide@uninstall.it>
 * Copyright 2020 Fred Dushin <fred@dushin.net>
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

#include "bitstring.h"

static inline uint64_t from_le64(uint64_t value)
{
    return ((((value) & 0xFF) << 56) | (((value) & 0xFF00) << 40) | (((value) & 0xFF0000) << 24) | \
        (((value) & 0xFF000000) << 8) | (((value) & 0xFF00000000) >> 8) | (((value) & 0xFF0000000000) >> 24) |  \
         (((value) & 0xFF000000000000) >> 40) | (((value) & 0xFF00000000000000) >> 56));
}

bool bitstring_extract_any_integer(const uint8_t *src, size_t offset, avm_int_t n,
    enum BitstringFlags bs_flags, union maybe_unsigned_int64 *dst)
{
    uint64_t out = 0;

    int i;
    for (i = 0; i < n; i++) {
        int bit_pos = offset + i;
        int byte_pos = bit_pos >> 3;
        int shift = 7 - (bit_pos & 7);

        if (src[byte_pos] & (1 << shift)) {
            out |= (uint64_t) 1 << (n - i - 1);
        }
    }

    if (bs_flags & LittleEndianIntegerMask) {
        out = from_le64(out) >> (64 - n);
    }

    if ((bs_flags & SignedInteger) && (out & ((uint64_t) 1) << (i - 1))) {
        dst->u = ((uint64_t) 0xFFFFFFFFFFFFFFFF << i) | out;
    } else {
        dst->u = out;
    }

    return true;
}

bool bitstring_insert_any_integer(uint8_t *dst, avm_int_t offset, avm_int64_t value, size_t n, enum BitstringFlags bs_flags)
{
    // TODO support big/little endian signedness flags
    if (bs_flags != 0) {
        return false;
    }
    for (int i = 0; i < n; ++i) {
        int k = (n - 1) - i;
        int bit_val = (value & (0x01 << k)) >> k;
        if (bit_val) {
            int bit_pos = offset + i;
            int byte_pos = bit_pos >> 3; // div 8
            uint8_t *pos = (uint8_t *) (dst + byte_pos);
            int shift = 7 - (bit_pos & 7); // mod 8
            *pos ^= (0x01 << shift);
        }
    }
    return true;
}
