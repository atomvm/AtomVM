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

#include <assert.h>
#include <math.h>

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
        // Inverse of the little-endian insertion layout: the field stores the
        // complete low-order bytes first (LSB byte first), then the remaining
        // n rem 8 high-order bits.
        size_t rem = (size_t) n & 0x7;
        uint64_t raw = out;
        uint64_t value = raw & ((((uint64_t) 1) << rem) - 1);
        raw >>= rem;
        for (size_t j = 0; j < ((size_t) n >> 3); ++j) {
            value = (value << 8) | (raw & 0xFF);
            raw >>= 8;
        }
        out = value;
    }

    if ((bs_flags & SignedInteger) && i > 0 && (out & ((uint64_t) 1) << (i - 1))) {
        dst->u = (i < 64) ? (((uint64_t) 0xFFFFFFFFFFFFFFFF << i) | out) : out;
    } else {
        dst->u = out;
    }

    return true;
}

static void insert_bits_msb_first(uint8_t *dst, size_t offset, uint64_t value, size_t n)
{
    for (size_t i = 0; i < n; ++i) {
        size_t k = (n - 1) - i;
        int bit_val = (value & (0x01ULL << k)) >> k;
        if (bit_val) {
            size_t bit_pos = offset + i;
            size_t byte_pos = bit_pos >> 3; // div 8
            uint8_t *pos = dst + byte_pos;
            int shift = 7 - (bit_pos & 7); // mod 8
            *pos ^= (0x01 << shift);
        }
    }
}

static void insert_sign_bits(uint8_t *dst, size_t offset, size_t count, bool negative)
{
    if (!negative) {
        return;
    }
    while (count > 0) {
        size_t chunk = count < 64 ? count : 64;
        uint64_t ones = (chunk == 64) ? ~UINT64_C(0) : ((UINT64_C(1) << chunk) - 1);
        insert_bits_msb_first(dst, offset, ones, chunk);
        offset += chunk;
        count -= chunk;
    }
}

bool bitstring_insert_any_integer(uint8_t *dst, avm_int_t offset, avm_int64_t value, size_t n, enum BitstringFlags bs_flags)
{
    // SignedInteger flag does not affect insertion (caller handles sign extension)
    bool little_endian = bs_flags & LittleEndianIntegerMask;
    uint8_t sign_fill = (value < 0) ? 0xFF : 0x00;

    if (little_endian && (offset & 0x7) == 0 && (n & 0x7) == 0) {
        // Byte-aligned little-endian: write bytes LSB first
        size_t byte_offset = offset >> 3;
        size_t num_bytes = n >> 3;
        size_t val_bytes = sizeof(value);
        uint64_t uvalue = (uint64_t) value;
        for (size_t i = 0; i < num_bytes; ++i) {
            if (i < val_bytes) {
                dst[byte_offset + i] = (uint8_t) (uvalue & 0xFF);
                uvalue >>= 8;
            } else {
                dst[byte_offset + i] = sign_fill;
            }
        }
    } else if (little_endian) {
        // low-order bytes first (LSB byte first), then remaining high-order bits
        size_t whole_bytes = n >> 3;
        size_t rem = n & 0x7;
        uint64_t uvalue = (uint64_t) value;
        for (size_t i = 0; i < whole_bytes; ++i) {
            uint8_t byte = (i < 8) ? (uint8_t) ((uvalue >> (8 * i)) & 0xFF) : sign_fill;
            insert_bits_msb_first(dst, offset + 8 * i, byte, 8);
        }
        if (rem != 0) {
            uint8_t byte
                = (whole_bytes < 8) ? (uint8_t) ((uvalue >> (8 * whole_bytes)) & 0xFF) : sign_fill;
            insert_bits_msb_first(dst, offset + 8 * whole_bytes, byte, rem);
        }
    } else {
        // Big-endian: write bits MSB first, high-order sign bits first
        uint64_t write_value = (uint64_t) value;
        if (n > 8 * sizeof(value)) {
            size_t high_bits = n - (8 * sizeof(value));
            insert_sign_bits(dst, offset, high_bits, value < 0);
            offset += high_bits;
            n = 8 * sizeof(value);
        }
        insert_bits_msb_first(dst, offset, write_value, n);
    }
    return true;
}

static bool is_invalid_codepoint(int32_t v)
{
    return (v < 0) || (v > 0x10FFFF) || (v >= 0xD800 && v <= 0xDFFF);
}

//
// UTF-8 encoding
// https://en.wikipedia.org/wiki/UTF-8
// +----------+----------+----------+----------+----------+
// | code pt  |  buf[0]  |  buf[1]  |  buf[2]  |  buf[3]  |
// +----------+----------+----------+----------+----------+
// | U+0000   | 0xxxxxxx |          |          |          |
// +----------+----------+----------+----------+----------+
// | U+0080   | 110xxxxx | 10xxxxxx |          |          |
// +----------+----------+----------+----------+----------+
// | U+0800   | 1110xxxx | 10xxxxxx | 10xxxxxx |          |
// +----------+----------+----------+----------+----------+
// | U+10000  | 11110xxx | 10xxxxxx | 10xxxxxx | 10xxxxxx |
// +----------+----------+----------+----------+----------+
//

bool bitstring_utf8_encode(uint32_t c, uint8_t *buf, size_t *out_size)
{
    size_t sz = 0;
    if (is_invalid_codepoint(c)) {
        return false;
    }
    if (c < 0x80) {
        if (buf) {
            *buf++ = c;
        }
        sz++;
    } else if (c < 0x800) {
        if (buf) {
            *buf++ = (c >> 6) | 0xC0;
            *buf++ = (c & 0x3F) | 0x80;
        }
        sz += 2;
    } else if (c < 0x10000) {
        if (buf) {
            *buf++ = (c >> 12) | 0xE0;
            *buf++ = ((c >> 6) & 0x3F) | 0x80;
            *buf++ = (c & 0x3F) | 0x80;
        }
        sz += 3;
    } else {
        if (buf) {
            *buf++ = (c >> 18) | 0xF0;
            *buf++ = ((c >> 12) & 0x3F) | 0x80;
            *buf++ = ((c >> 6) & 0x3F) | 0x80;
            *buf++ = (c & 0x3F) | 0x80;
        }
        sz += 4;
    }
    *out_size = sz;
    return true;
}

// UTF-16 encoding, when U in U+010000 to U+10FFFF:
//
//  U' = yyyyyyyyyyxxxxxxxxxx  // U - 0x10000
//  W1 = 110110yyyyyyyyyy      // 0xD800 + yyyyyyyyyy
//  W2 = 110111xxxxxxxxxx      // 0xDC00 + xxxxxxxxxx

bool bitstring_utf16_encode(uint32_t c, uint8_t *buf, enum BitstringFlags bs_flags, size_t *out_size)
{
    size_t sz = 0;
    if (is_invalid_codepoint(c)) {
        return false;
    }
    if (c < 0x10000) {
        // Ignore D800-DFFF range
        if (buf) {
            if (bs_flags & LittleEndianIntegerMask) {
                *buf++ = c & 0xFF;
                *buf++ = c >> 8;
            } else {
                *buf++ = c >> 8;
                *buf++ = c & 0xFF;
            }
        }
        sz += 2;
    } else {
        if (buf) {
            c -= 0x10000;
            if (bs_flags & LittleEndianIntegerMask) {
                *buf++ = ((c >> 10) & 0xFF);
                *buf++ = (c >> 18) | 0xD8;
                *buf++ = c & 0xFF;
                *buf++ = ((c >> 8) & 0x03) | 0xDC;
            } else {
                *buf++ = (c >> 18) | 0xD8;
                *buf++ = ((c >> 10) & 0xFF);
                *buf++ = ((c >> 8) & 0x03) | 0xDC;
                *buf++ = c & 0xFF;
            }
        }
        sz += 4;
    }
    *out_size = sz;
    return true;
}

bool bitstring_utf16_decode(const uint8_t *buf, size_t len, int32_t *c, size_t *out_size, enum BitstringFlags bs_flags)
{
    if (len == 0) {
        return false;
    } else if (bs_flags & LittleEndianIntegerMask) {
        if (len >= 4 && ((buf[1] & 0xFC) == 0xD8) && ((buf[3] & 0xFC) == 0xDC)) {
            int32_t v = 0;
            v |= (buf[1] & 0x03) << 18;
            v |= (buf[0] & 0xFF) << 10;
            v |= (buf[3] & 0x03) << 8;
            v |= (buf[2] & 0xFF);
            v += 0x10000;
            if (is_invalid_codepoint(v)) {
                return false;
            }
            *c = v;
            *out_size = 4;
            return true;
        } else if (len >= 2) {
            int32_t v = 0;
            v = READ_16LE_UNALIGNED(buf);
            if (is_invalid_codepoint(v)) {
                return false;
            }
            *c = v;
            *out_size = 2;
            return true;
        }
    } else {
        if (len >= 4 && ((buf[0] & 0xFC) == 0xD8) && ((buf[2] & 0xFC) == 0xDC)) {
            int32_t v = 0;
            v |= (buf[0] & 0x03) << 18;
            v |= (buf[1] & 0xFF) << 10;
            v |= (buf[2] & 0x03) << 8;
            v |= (buf[3] & 0xFF);
            v += 0x10000;
            if (is_invalid_codepoint(v)) {
                return false;
            }
            *c = v;
            *out_size = 4;
            return true;
        } else if (len >= 2) {
            int32_t v = 0;
            v = READ_16_UNALIGNED(buf);
            if (is_invalid_codepoint(v)) {
                return false;
            }
            *c = v;
            *out_size = 2;
            return true;
        }
    }
    return false;
}

bool bitstring_utf32_encode(uint32_t c, uint8_t *buf, enum BitstringFlags bs_flags)
{
    UNUSED(bs_flags);
    if (is_invalid_codepoint(c)) {
        return false;
    }
    if (bs_flags & LittleEndianIntegerMask) {
        *buf++ = c & 0xFF;
        *buf++ = (c >> 8) & 0xFF;
        *buf++ = (c >> 16) & 0xFF;
        *buf++ = c >> 24;
    } else {
        *buf++ = c >> 24;
        *buf++ = (c >> 16) & 0xFF;
        *buf++ = (c >> 8) & 0xFF;
        *buf++ = c & 0xFF;
    }
    return true;
}

bool bitstring_utf32_decode(const uint8_t *buf, size_t len, int32_t *c, enum BitstringFlags bs_flags)
{
    if (len < 4) {
        return false;
    } else if (bs_flags & LittleEndianIntegerMask) {
        int32_t v = 0;
        v |= (buf[3] & 0xFF) << 24;
        v |= (buf[2] & 0xFF) << 16;
        v |= (buf[1] & 0xFF) << 8;
        v |= buf[0] & 0xFF;
        if (is_invalid_codepoint(v)) {
            return false;
        }
        *c = v;
        return true;
    } else {
        int32_t v = 0;
        v |= (buf[0] & 0xFF) << 24;
        v |= (buf[1] & 0xFF) << 16;
        v |= (buf[2] & 0xFF) << 8;
        v |= buf[3] & 0xFF;
        if (is_invalid_codepoint(v)) {
            return false;
        }
        *c = v;
        return true;
    }
}

void bitstring_copy_bits_incomplete_bytes(uint8_t *dst, size_t bits_offset, const uint8_t *src, size_t bits_count)
{
    size_t byte_offset = bits_offset / 8;
    size_t bit_offset = bits_offset - (8 * byte_offset);
    if (bit_offset == 0 && bits_count >= 8) {
        size_t bytes_count = bits_count / 8;
        memcpy(dst + byte_offset, src, bytes_count);
        src += bytes_count;
        byte_offset += bytes_count;
        bits_count -= bytes_count * 8;
    }
    if (bits_count == 0) {
        return;
    }
    // Eventually copy bit by bit
    dst += byte_offset;
    uint8_t dest_byte = *dst;
    int dest_bit_ix = 7 - (int) bit_offset;
    for (size_t i = 0; i < bits_count; i++) {
        if (src[i / 8] & (1 << (7 - (i % 8)))) {
            dest_byte |= 1 << dest_bit_ix;
        } else {
            dest_byte &= ~(1 << dest_bit_ix);
        }
        if (dest_bit_ix == 0) {
            *dst++ = dest_byte;
            if (i + 1 < bits_count) {
                dest_byte = *dst;
            }
            dest_bit_ix = 7;
        } else {
            dest_bit_ix--;
        }
    }
    // The last byte was already written if the final bit completed it
    if (dest_bit_ix != 7) {
        *dst = dest_byte;
    }
}

void bitstring_copy_bits_from(uint8_t *dst, const uint8_t *src, size_t src_offset, size_t bits_count)
{
    for (size_t i = 0; i < bits_count; i++) {
        size_t s = src_offset + i;
        if (src[s / 8] & (uint8_t) (1 << (7 - (s % 8)))) {
            dst[i / 8] |= (uint8_t) (1 << (7 - (i % 8)));
        }
    }
}

size_t bitstring_slice_heap_size(term bs_bin, size_t offset, size_t len_bits)
{
    if (offset % 8 == 0 && len_bits % 8 == 0) {
        return term_sub_binary_heap_size(bs_bin, len_bits / 8);
    }
    size_t words = term_binary_heap_size((len_bits + 7) / 8);
    if (len_bits % 8 != 0) {
        words += TERM_BOXED_SUB_BINARY_SIZE;
    }
    return words;
}

term bitstring_slice(term bs_bin, size_t offset, size_t len_bits, Heap *heap, GlobalContext *glb)
{
    if (offset % 8 == 0 && len_bits % 8 == 0) {
        return term_maybe_create_sub_binary(bs_bin, offset / 8, len_bits / 8, heap, glb);
    }
    size_t result_bytes = (len_bits + 7) / 8;
    // term_create_empty_binary zero-fills, as bitstring_copy_bits_from requires
    term bin = term_create_empty_binary(result_bytes, heap, glb);
    if (UNLIKELY(term_is_invalid_term(bin))) {
        // the caller reserved heap words, but a refc binary buffer is allocated
        // separately and that allocation may still fail
        return bin;
    }
    uint8_t *dst = (uint8_t *) term_binary_data(bin);
    bitstring_copy_bits_from(dst, (const uint8_t *) term_binary_data(bs_bin), offset, len_bits);
    size_t trailing = len_bits % 8;
    if (trailing != 0) {
        return term_alloc_sub_binary_bits(bin, 0, len_bits / 8, (uint8_t) trailing, heap);
    }
    return bin;
}

bool bitstring_extract_f16(
    term src_bin, size_t offset, avm_int_t n, enum BitstringFlags bs_flags, avm_float_t *dst)
{
    unsigned long capacity = term_binary_size(src_bin);
    if (8 * capacity - offset < (unsigned long) n) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        const uint8_t *src = (const uint8_t *) term_binary_data(src_bin) + byte_offset;

        // Read 16-bit value
        uint16_t f16_bits;
        if (bs_flags & LittleEndianIntegerMask) {
            f16_bits = READ_16LE_UNALIGNED(src);
        } else {
            f16_bits = READ_16_UNALIGNED(src);
        }

        // Convert IEEE 754 half-precision to single-precision
        uint32_t sign = (f16_bits >> 15) & 0x1;
        uint32_t f16_exp = (f16_bits >> 10) & 0x1F;
        uint32_t f16_mantissa = f16_bits & 0x3FF;

        uint32_t f32_bits;
        if (f16_exp == 0) {
            if (f16_mantissa == 0) {
                // Zero
                f32_bits = sign << 31;
            } else {
                // Subnormal number - normalize it
                int e = -1;
                uint32_t m = f16_mantissa;
                do {
                    e++;
                    m <<= 1;
                } while ((m & 0x400) == 0);
                f16_mantissa = m & 0x3FF;
                f16_exp = -e;
                int32_t f32_exp = (int32_t) f16_exp + 127 - 15;
                f32_bits = (sign << 31) | (f32_exp << 23) | (f16_mantissa << 13);
            }
        } else if (f16_exp == 0x1F) {
            // Inf or NaN - not finite
            return false;
        } else {
            // Normalized number
            int32_t f32_exp = (int32_t) f16_exp + 127 - 15;
            f32_bits = (sign << 31) | (f32_exp << 23) | (f16_mantissa << 13);
        }

        union
        {
            uint32_t bits;
            float fvalue;
        } f32;
        f32.bits = f32_bits;

        *dst = f32.fvalue;
        return true;
    } else {
        // TODO: add support to floats not aligned to byte boundary
        return false;
    }
}

bool bitstring_extract_f32(
    term src_bin, size_t offset, avm_int_t n, enum BitstringFlags bs_flags, avm_float_t *dst)
{
    unsigned long capacity = term_binary_size(src_bin);
    if (8 * capacity - offset < (unsigned long) n) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        const uint8_t *src = (const uint8_t *) term_binary_data(src_bin) + byte_offset;

        _Static_assert(sizeof(float) == 4, "Unsupported float size");

        union
        {
            uint32_t bits;
            float fvalue;
        } f32;

        if (bs_flags & LittleEndianIntegerMask) {
            f32.bits = READ_32LE_UNALIGNED(src);
        } else {
            f32.bits = READ_32_UNALIGNED(src);
        }
        if (UNLIKELY(!isfinite(f32.fvalue))) {
            return false;
        }
        *dst = f32.fvalue;
        return true;
    } else {
        // TODO: add support to doubles not aligned to byte boundary
        return false;
    }
}

bool bitstring_extract_f64(
    term src_bin, size_t offset, avm_int_t n, enum BitstringFlags bs_flags, avm_float_t *dst)
{
    unsigned long capacity = term_binary_size(src_bin);
    if (8 * capacity - offset < (unsigned long) n) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        const uint8_t *src = (const uint8_t *) term_binary_data(src_bin) + byte_offset;

        uint64_t bin64;
        if (bs_flags & LittleEndianIntegerMask) {
            bin64 = READ_64LE_UNALIGNED(src);
        } else {
            bin64 = READ_64_UNALIGNED(src);
        }

        _Static_assert(sizeof(double) == 8, "Unsupported double size");

        union
        {
            uint64_t bits;
            double fvalue;
        } f64;

        f64.bits = bin64;
        if (UNLIKELY(!isfinite(f64.fvalue))) {
            return false;
        }
        *dst = f64.fvalue;

        return true;

    } else {
        // TODO: add support to doubles not aligned to byte boundary
        return false;
    }
}

intn_from_integer_options_t bitstring_flags_to_intn_opts(enum BitstringFlags bf)
{
    intn_from_integer_options_t converted = IntnUnsignedBigEndian;
    if (bf & LittleEndianInteger) {
        converted |= IntnLittleEndian;
    }
    if (bf & SignedInteger) {
        converted |= IntnSigned;
    }
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    if (bf & NativeEndianInteger) {
        converted |= IntnLittleEndian;
    }
#endif
    return converted;
}

bool bitstring_insert_f16(
    term dst_bin, size_t offset, avm_float_t value, enum BitstringFlags bs_flags)
{
    unsigned long capacity = term_binary_size(dst_bin);
    if (8 * capacity - offset < 16) {
        return false;
    }

    if (!isfinite(value)) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        uint8_t *dst = (uint8_t *) term_binary_data(dst_bin) + byte_offset;

        _Static_assert(sizeof(float) == 4, "Unsupported float size");

        // Convert double to float first
        union
        {
            uint32_t bits;
            float fvalue;
        } f32;

        f32.fvalue = (float) value;
        uint32_t f32_bits = f32.bits;

        // Extract components from float (32-bit)
        uint32_t sign = (f32_bits >> 31) & 0x1;
        int32_t exp = ((f32_bits >> 23) & 0xFF) - 127; // Remove float bias
        uint32_t mantissa = f32_bits & 0x7FFFFF;

        uint16_t f16_bits;

        // Handle special cases
        if (exp > 15) {
            // Overflow to infinity
            f16_bits = (sign << 15) | 0x7C00;
        } else if (exp < -14) {
            // Underflow to zero or denormal
            if (exp < -24) {
                // Too small, round to zero
                f16_bits = sign << 15;
            } else {
                // Denormal number
                uint32_t denorm_mantissa = (mantissa | 0x800000) >> (-14 - exp);
                f16_bits = (sign << 15) | (denorm_mantissa >> 13);
            }
        } else {
            // Normal number
            uint32_t f16_exp = exp + 15; // Add half-precision bias
            // Round to nearest even (bit 12 is the rounding bit)
            uint32_t f16_mantissa = (mantissa + 0x1000) >> 13; // Round and keep top 10 bits
            // Handle mantissa overflow
            if (f16_mantissa > 0x3FF) {
                f16_mantissa = 0;
                f16_exp++;
            }
            if (f16_exp > 30) {
                // Overflow to infinity
                f16_bits = (sign << 15) | 0x7C00;
            } else {
                f16_bits = (sign << 15) | (f16_exp << 10) | f16_mantissa;
            }
        }

        if (bs_flags & LittleEndianIntegerMask) {
            WRITE_16LE_UNALIGNED(dst, f16_bits);
        } else {
            WRITE_16_UNALIGNED(dst, f16_bits);
        }
        return true;
    } else {
        // TODO: add support to floats not aligned to byte boundary
        return false;
    }
}

bool bitstring_insert_f32(
    term dst_bin, size_t offset, avm_float_t value, enum BitstringFlags bs_flags)
{
    unsigned long capacity = term_binary_size(dst_bin);
    if (8 * capacity - offset < 32) {
        return false;
    }

    if (!isfinite(value)) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        uint8_t *dst = (uint8_t *) term_binary_data(dst_bin) + byte_offset;

        _Static_assert(sizeof(float) == 4, "Unsupported float size");

        union
        {
            uint32_t bits;
            float fvalue;
        } f32;

        f32.fvalue = (float) value;

        if (bs_flags & LittleEndianIntegerMask) {
            WRITE_32LE_UNALIGNED(dst, f32.bits);
        } else {
            WRITE_32_UNALIGNED(dst, f32.bits);
        }
        return true;
    } else {
        // TODO: add support to floats not aligned to byte boundary
        return false;
    }
}

bool bitstring_insert_f64(
    term dst_bin, size_t offset, avm_float_t value, enum BitstringFlags bs_flags)
{
    unsigned long capacity = term_binary_size(dst_bin);
    if (8 * capacity - offset < 64) {
        return false;
    }

    if (!isfinite(value)) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        uint8_t *dst = (uint8_t *) term_binary_data(dst_bin) + byte_offset;

        _Static_assert(sizeof(double) == 8, "Unsupported double size");

        union
        {
            uint64_t bits;
            double fvalue;
        } f64;

        f64.fvalue = value;

        if (bs_flags & LittleEndianIntegerMask) {
            WRITE_64LE_UNALIGNED(dst, f64.bits);
        } else {
            WRITE_64_UNALIGNED(dst, f64.bits);
        }
        return true;
    } else {
        // TODO: add support to doubles not aligned to byte boundary
        return false;
    }
}
