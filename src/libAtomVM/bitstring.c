#include "bitstring.h"

static inline uint64_t from_le64(uint64_t value)
{
    return ((((value) & 0xFF) << 56) | (((value) & 0xFF00) << 40) | (((value) & 0xFF0000) << 24) | \
        (((value) & 0xFF000000) << 8) | (((value) & 0xFF00000000) >> 8) | (((value) & 0xFF0000000000) >> 24) |  \
         (((value) & 0xFF000000000000) >> 40) | (((value) & 0xFF00000000000000) >> 56));
}

bool extract_any_integer(const uint8_t *src, size_t offset, avm_int_t n,
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

    if (bs_flags & LittleEndianInteger) {
        out = from_le64(out) >> (64 - n);
    }

    if ((bs_flags & SignedInteger) && (out & ((uint64_t) 1) << (i - 1))) {
        dst->u = ((uint64_t) 0xFFFFFFFFFFFFFFFF << i) | out;
    } else {
        dst->u = out;
    }

    return true;
}
