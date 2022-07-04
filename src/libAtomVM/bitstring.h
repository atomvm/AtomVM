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

#ifndef _BITSTRING_H_
#define _BITSTRING_H_

#include "term.h"

#include <stdbool.h>
#include <stdint.h>

#ifdef __ORDER_LITTLE_ENDIAN__
    #define READ_16LE_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[1] << 8) | ((uint8_t *)(ptr))[0] )

    #define WRITE_16LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( (((uint8_t *)(&val))[1] << 8) | ((uint8_t *)(&val))[0] )

    #define READ_32LE_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[3] << 24) | (((uint8_t *) (ptr))[2] << 16) | (((uint8_t *)(ptr))[1] << 8) | ((uint8_t *)(ptr))[0] )

    #define WRITE_32LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( (((uint8_t *)(&val))[3] << 24) | (((uint8_t *) (&val))[2] << 16) | (((uint8_t *)(&val))[1] << 8) | ((uint8_t *)(&val))[0] )

    #define READ_64LE_UNALIGNED(ptr) \
        ( (((uint64_t) ((uint8_t *)(ptr))[7]) << 56) | (((uint64_t) ((uint8_t *) (ptr))[6]) << 48) | \
            (((uint64_t) ((uint8_t *)(ptr))[5]) << 40) | (((uint64_t) ((uint8_t *) (ptr))[4]) << 32) | \
            (((uint64_t) ((uint8_t *)(ptr))[3]) << 24) | (((uint64_t) ((uint8_t *) (ptr))[2]) << 16) | \
            (((uint64_t) ((uint8_t *)(ptr))[1]) << 8) | (((uint64_t) ((uint8_t *) (ptr))[0])) )

    #define WRITE_64LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( \
            ((t) ((uint8_t *)(&val))[7] << 56) | ((t) ((uint8_t *) (&val))[6] << 48) | \
            ((t) ((uint8_t *)(&val))[5] << 40) | ((t) ((uint8_t *) (&val))[4] << 32) | \
            ((t) ((uint8_t *)(&val))[3] << 24) | ((t) ((uint8_t *) (&val))[2] << 16) | \
            ((t) ((uint8_t *)(&val))[1] <<  8) | (     (uint8_t *) (&val))[0] \
        )


#else
    #define READ_16LE_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 8) | ((uint8_t *)(ptr))[1] )

    #define WRITE_16LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 8) | ((uint8_t *)(&val))[1] )

    #define READ_32LE_UNALIGNED(ptr) \
        ( (((uint8_t *)(ptr))[0] << 24) | (((uint8_t *) (ptr))[1] << 16) | (((uint8_t *)(ptr))[2] << 8) | ((uint8_t *)(ptr))[3] )

    #define WRITE_32LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( (((uint8_t *)(&val))[0] << 24) | (((uint8_t *) (&val))[1] << 16) | (((uint8_t *)(&val))[2] << 8) | ((uint8_t *)(&val))[3] )

    #define READ_64LE_UNALIGNED(ptr) \
        ( (((uint64_t) ((uint8_t *)(ptr))[0]) << 56) | (((uint64_t) ((uint8_t *) (ptr))[1]) << 48) | \
            (((uint64_t) ((uint8_t *)(ptr))[2]) << 40) | (((uint64_t) ((uint8_t *) (ptr))[3]) << 32) | \
            (((uint64_t) ((uint8_t *)(ptr))[4]) << 24) | (((uint64_t) ((uint8_t *) (ptr))[5]) << 16) | \
            (((uint64_t) ((uint8_t *)(ptr))[6]) << 8) | (((uint64_t) ((uint8_t *) (ptr))[7])) )

    #define WRITE_64LE_UNALIGNED(t, ptr, val) \
        *((t *) (ptr)) = ( \
            ((t) ((uint8_t *)(&val))[0] << 56) | ((t) ((uint8_t *) (&val))[1] << 48) | \
            ((t) ((uint8_t *)(&val))[2] << 40) | ((t) ((uint8_t *) (&val))[3] << 32) | \
            ((t) ((uint8_t *)(&val))[4] << 24) | ((t) ((uint8_t *) (&val))[5] << 16) | \
            ((t) ((uint8_t *)(&val))[6] <<  8) | (     (uint8_t *) (&val))[7] \
        )
#endif

enum BitstringFlags
{
    LittleEndianInteger = 0x2,
    SignedInteger = 0x4
};

union maybe_unsigned_int8
{
    uint8_t u;
    int8_t s;
};

union maybe_unsigned_int16
{
    uint16_t u;
    int16_t s;
};

union maybe_unsigned_int32
{
    uint32_t u;
    int32_t s;
};

union maybe_unsigned_int64
{
    uint64_t u;
    int64_t s;
};

bool bitstring_extract_any_integer(const uint8_t *src, size_t offset, avm_int_t n,
    enum BitstringFlags bs_flags, union maybe_unsigned_int64 *dst);

bool bitstring_insert_any_integer(uint8_t *dst, avm_int_t offset, avm_int64_t value, size_t n, enum BitstringFlags bs_flags);

static inline bool bitstring_extract_integer(term src_bin, size_t offset, avm_int_t n,
    enum BitstringFlags bs_flags, union maybe_unsigned_int64 *dst)
{
    unsigned long capacity = term_binary_size(src_bin);
    if (8 * capacity - offset < (unsigned long) n) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        const uint8_t *src = (const uint8_t *) term_binary_data(src_bin) + byte_offset;

        switch (n) {
            case 8: {
                union maybe_unsigned_int8 i8;

                i8.u = *src;
                if (bs_flags & SignedInteger) {
                    dst->s = i8.s;
                } else {
                    dst->u = i8.u;
                }
                return true;
            }

            case 16: {
                union maybe_unsigned_int16 i16;

                if (bs_flags & LittleEndianInteger) {
                    i16.u = READ_16LE_UNALIGNED(src);
                } else {
                    i16.u = READ_16_UNALIGNED(src);
                }
                if (bs_flags & SignedInteger) {
                    dst->s = i16.s;
                } else {
                    dst->u = i16.u;
                }
                return true;
            }

            case 32: {
                union maybe_unsigned_int32 i32;

                if (bs_flags & LittleEndianInteger) {
                    i32.u = READ_32LE_UNALIGNED(src);
                } else {
                    i32.u = READ_32_UNALIGNED(src);
                }
                if (bs_flags & SignedInteger) {
                    dst->s = i32.s;
                } else {
                    dst->u = i32.u;
                }
                return true;
            }

            case 64: {
                union maybe_unsigned_int64 i64;

                if (bs_flags & LittleEndianInteger) {
                    i64.u = READ_64LE_UNALIGNED(src);
                } else {
                    i64.u = READ_64_UNALIGNED(src);
                }
                if (bs_flags & SignedInteger) {
                    if (UNLIKELY(i64.u & ((uint64_t) 1 << 63))) {
                        return false;
                    }
                    dst->s = i64.s;
                } else {
                    dst->u = i64.u;
                }
                return true;
            }

            default:
                return bitstring_extract_any_integer(src, 0, n, bs_flags, dst);
        }
    } else {
        int byte_offset = offset / 8;
        const uint8_t *src = (const uint8_t *) term_binary_data(src_bin) + byte_offset;
        return bitstring_extract_any_integer(src, offset - (byte_offset * 8), n, bs_flags, dst);
    }
}

static inline bool bitstring_insert_integer(term dst_bin, size_t offset, avm_int64_t value, size_t n, enum BitstringFlags bs_flags)
{
    unsigned long capacity = term_binary_size(dst_bin);
    if (8 * capacity - offset < (unsigned long) n) {
        return false;
    }

    if ((offset & 0x7) == 0) {
        int byte_offset = offset >> 3;
        uint8_t *dst = (uint8_t *) term_binary_data(dst_bin) + byte_offset;

        switch (n) {
            case 8: {
                *dst = value & 0xFF;
                return true;
            }

            case 16: {
                if (bs_flags & LittleEndianInteger) {
                    if (bs_flags & SignedInteger) {
                        int16_t signed_value = (int16_t) value;
                        WRITE_16LE_UNALIGNED(int16_t, dst, signed_value);
                    } else {
                        uint16_t unsigned_value = (uint16_t) value;
                        WRITE_16LE_UNALIGNED(uint16_t, dst, unsigned_value);
                    }
                } else {
                    if (bs_flags & SignedInteger) {
                        int16_t signed_value = (int16_t) value;
                        WRITE_16_UNALIGNED(dst, signed_value);
                    } else {
                        uint16_t unsigned_value = (uint16_t) value;
                        WRITE_16_UNALIGNED(dst, unsigned_value);
                    }
                }
                return true;
            }

            case 32: {
                if (bs_flags & LittleEndianInteger) {
                    if (bs_flags & SignedInteger) {
                        int32_t signed_value = (int32_t) value;
                        WRITE_32LE_UNALIGNED(int32_t, dst, signed_value);
                    } else {
                        uint32_t unsigned_value = (uint32_t) value;
                        WRITE_32LE_UNALIGNED(uint32_t, dst, unsigned_value);
                    }
                } else {
                    if (bs_flags & SignedInteger) {
                        int32_t signed_value = (int32_t) value;
                        WRITE_32_UNALIGNED(dst, signed_value);
                    } else {
                        uint32_t unsigned_value = (uint32_t) value;
                        WRITE_32_UNALIGNED(dst, unsigned_value);
                    }
                }
                return true;
            }

            case 64: {
                if (bs_flags & LittleEndianInteger) {
                    if (bs_flags & SignedInteger) {
                        avm_int64_t signed_value = value;
                        WRITE_64LE_UNALIGNED(avm_int64_t, dst, signed_value);
                    } else {
                        uint64_t unsigned_value = (uint64_t) value;
                        WRITE_64LE_UNALIGNED(avm_int64_t, dst, unsigned_value);
                    }
                } else {
                    if (bs_flags & SignedInteger) {
                        int64_t signed_value = value;
                        WRITE_64_UNALIGNED(dst, signed_value);
                    } else {
                        uint64_t unsigned_value = (uint64_t) value;
                        WRITE_64_UNALIGNED(dst, unsigned_value);
                    }
                }
                return true;
            }

            default:
                return bitstring_insert_any_integer(dst, offset - (byte_offset * 8), value, n, bs_flags);
        }
    }

    return bitstring_insert_any_integer((uint8_t *) term_binary_data(dst_bin), offset, value, n, bs_flags);
}

#endif
