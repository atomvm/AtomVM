/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Davide Bettio <davide@uninstall.it>
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

#include "utils.h"

#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#define MIN(a, b) (a < b) ? a : b;

#if INTPTR_MAX == 2147483647 // INT32_MAX
#define INTPTR_MAX_BASE_10_DIGITS 10
#define INTPTR_MAX_BASE_16_DIGITS 8

#elif INTPTR_MAX == 9223372036854775807 // INT64_MAX
#define INTPTR_MAX_BASE_10_DIGITS 19
#define INTPTR_MAX_BASE_16_DIGITS 16
#endif

#define INT64_MAX_BASE_10_DIGITS 19
#define INT64_MAX_BASE_16_DIGITS 16

static char *uintptr_to_a_n(uintptr_t n, unsigned int base, char *out_end)
{
    ASSUME((base >= 2) && (base <= 36));

    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        uintptr_t r = (q % base);
        *c = (r <= 9) ? '0' + r : 'A' + r - 10;
        q /= base;
    } while (q);

    return c;
}

static char *uintptr_to_a_10(uintptr_t n, char *out_end)
{
    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        *c = '0' + (q % 10);
        q /= 10;
    } while (q);

    return c;
}

static char *uintptr_to_a_16(uintptr_t n, char *out_end)
{
    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        uintptr_t r = (q & 0xF);
        *c = (r <= 9) ? '0' + r : 'A' + r - 10;
        q >>= 4;
    } while (q);

    return c;
}

size_t intptr_write_to_ascii_buf(intptr_t n, unsigned int base, char *out_end)
{
    ASSUME((base >= 2) && (base <= 36));

    // let's avoid undefined behaviors
    // -INTPTR_MIN is INTPTR_MAX + 1
    uintptr_t pos_n;
    if (n >= 0) {
        pos_n = n;
    } else if (n == INTPTR_MIN) {
        pos_n = ((uintptr_t) INTPTR_MAX) + 1;
    } else {
        pos_n = -n;
    }

    char *c;

    // use optimized versions for 10 and 16
    switch (base) {
        case 10:
            c = uintptr_to_a_10(pos_n, out_end);
            break;
        case 16:
            c = uintptr_to_a_16(pos_n, out_end);
            break;
        default:
            c = uintptr_to_a_n(pos_n, base, out_end);
            break;
    }

    if (n < 0) {
        c--;
        *c = '-';
    }

    return out_end - c;
}

#if INT64_MAX > INTPTR_MAX

static char *uint64_to_a_n(uint64_t n, unsigned int base, char *out_end)
{
    ASSUME((base >= 2) && (base <= 36));

    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        uint64_t r = (q % base);
        *c = (r <= 9) ? '0' + r : 'A' + r - 10;
        q /= base;
    } while (q);

    return c;
}

static char *uint64_to_a_10(uint64_t n, char *out_end)
{
    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        *c = '0' + (q % 10);
        q /= 10;
    } while (q);

    return c;
}

static char *uint64_to_a_16(uint64_t n, char *out_end)
{
    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        uint64_t r = (q & 0xF);
        *c = (r <= 9) ? '0' + r : 'A' + r - 10;
        q >>= 4;
    } while (q);

    return c;
}

size_t int64_write_to_ascii_buf(int64_t n, unsigned int base, char *out_end)
{
    ASSUME((base >= 2) && (base <= 36));

    // let's avoid undefined behaviors
    // -INT64_MIN is INT64_MAX + 1
    uint64_t pos_n;
    if (n >= 0) {
        pos_n = n;
    } else if (n == INT64_MIN) {
        pos_n = ((uint64_t) INT64_MAX) + 1;
    } else {
        pos_n = -n;
    }

    char *c;

    // use optimized versions for 10 and 16
    switch (base) {
        case 10:
            c = uint64_to_a_10(pos_n, out_end);
            break;
        case 16:
            c = uint64_to_a_16(pos_n, out_end);
            break;
        default:
            c = uint64_to_a_n(pos_n, base, out_end);
            break;
    }

    if (n < 0) {
        c--;
        *c = '-';
    }

    return out_end - c;
}

#endif

static inline int64_t int64_safe_neg_unsigned(uint64_t u64)
{
    return (-((int64_t) (u64 - 1)) - 1);
}

static inline int64_t uint64_does_overflow_int64(uint64_t val, bool is_negative)
{
    return ((is_negative && (val > ((uint64_t) INT64_MAX) + 1))
        || (!is_negative && (val > ((uint64_t) INT64_MAX))));
}

static inline bool is_base_10_digit(char c)
{
    return (c >= '0') && (c <= '9');
}

static int buf10_to_smallu64(
    const char buf[], size_t buf_len, size_t first_digit_index, uint64_t *out)
{
    size_t i = first_digit_index;

    size_t safe_len = MIN(INTPTR_MAX_BASE_10_DIGITS + i, buf_len);

    // we always process the last digit a special case, so safe_len - 1
    uintptr_t acc = 0;
    for (; i < safe_len - 1; i++) {
        char digit_char = buf[i];
        if (!is_base_10_digit(digit_char)) {
            return -1;
        }
        acc = (acc * 10) + ((intptr_t) (digit_char - '0'));
    }

    // let's process last digit (and single digit integers as well)

    char last_digit_char = buf[i];
    if (!is_base_10_digit(last_digit_char)) {
        return -1;
    }
    uintptr_t last_digit_num = (last_digit_char - '0');

    *out = (((uint64_t) acc) * 10) + last_digit_num;
    return i + 1;
}

static int buf10_to_int64(
    const char buf[], size_t buf_len, size_t first_digit_index, bool is_negative, int64_t *out)
{
#if INTPTR_MAX == INT64_MAX
    uint64_t utmp;
    int pos = buf10_to_smallu64(buf, buf_len, first_digit_index, &utmp);
    if (UNLIKELY(pos <= 0)) {
        return pos;
    }
    if (uint64_does_overflow_int64(utmp, is_negative)) {
        utmp /= 10;
        pos--;
    }
    *out = is_negative ? int64_safe_neg_unsigned(utmp) : (int64_t) utmp;
    return pos;

#elif INTPTR_MAX == INT32_MAX
    // here we try to minimize the number of 64-bit multiplications on 32-bit CPUs
    // we parse the number in 2 chunks, using 32-bit ints as much as possible,
    // and then everything is combined.
    // When a number has <= 10 digits, the short path is used.
    uint64_t uhigh = 0;
    int pos = buf10_to_smallu64(buf, buf_len, first_digit_index, &uhigh);
    if (pos == (int) buf_len) {
        *out = is_negative ? -((int64_t) uhigh) : ((int64_t) uhigh);
        return pos;
    } else if (UNLIKELY(pos <= 0)) {
        return pos;
    }

    uint64_t ulow = 0;
    int new_pos = buf10_to_smallu64(buf, buf_len, pos, &ulow);
    if (UNLIKELY(new_pos <= 0)) {
        return pos;
    }
    int high_parsed_count = new_pos - pos;
    pos = new_pos;

    int64_t shigh;
    int64_t slow;
    if (is_negative) {
        shigh = -((int64_t) uhigh);
        slow = -((int64_t) ulow);
    } else {
        shigh = (int64_t) uhigh;
        slow = (int64_t) ulow;
    }

    static const uint64_t pows10[] = { 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000,
        100000000, 1000000000, 10000000000 };

    _Static_assert((sizeof(pows10) / sizeof(uint64_t)) - 1 == INTPTR_MAX_BASE_10_DIGITS,
        "Only 10 digits are supported");
    ASSUME(high_parsed_count <= INTPTR_MAX_BASE_10_DIGITS);

    bool overflowed;
    int64_t maybe_overflowed_add = 0;
    do {
        int64_t maybe_overflowed_mul;
        overflowed
            = __builtin_mul_overflow(shigh, pows10[high_parsed_count], &maybe_overflowed_mul);
        if (!overflowed) {
            overflowed = __builtin_add_overflow(maybe_overflowed_mul, slow, &maybe_overflowed_add);
        }
        if (overflowed) {
            slow /= 10;
            high_parsed_count--;
            pos--;
        }
    } while (overflowed);

    *out = maybe_overflowed_add;

    return pos;
#else
#error "INTPTR_MAX is not either a 32 or 64 bit signed integer"
#endif
}

static inline intptr_t char_to_base_n_digit(char c)
{
    if ((c >= '0') && (c <= '9')) {
        return c - '0';
    } else {
        // 5th bit is the lower case bit
        uint8_t upper = (c & 0xEF);
        if ((upper >= 'A') && (upper <= 'Z')) {
            return 10 + (upper - 'A');
        }
    }
    return 36;
}

static int buf16_to_uintptr(const char buf[], size_t buf_len, size_t pos, uintptr_t *out)
{
    size_t i = pos;

    size_t safe_len = MIN(INTPTR_MAX_BASE_16_DIGITS + i, buf_len);

    uintptr_t uacc = 0;
    for (; i < safe_len; i++) {
        uintptr_t digit_val = char_to_base_n_digit(buf[i]);
        if (UNLIKELY(digit_val >= 16)) {
            return -1;
        }
        uacc = (uacc << 4) | digit_val;
    }

    *out = uacc;
    return i;
}

static int buf16_to_int64(
    const char buf[], size_t buf_len, size_t first_digit_index, bool is_negative, int64_t *out)
{
#if INTPTR_MAX == INT64_MAX
    _Static_assert(sizeof(uintptr_t) == sizeof(int64_t), "Unsupported intptr size or definition");

    uintptr_t utmp;
    int pos = buf16_to_uintptr(buf, buf_len, first_digit_index, &utmp);
    if (UNLIKELY(pos <= 0)) {
        return pos;
    }
    if (uint64_does_overflow_int64(utmp, is_negative)) {
        utmp >>= 16;
        pos--;
    }
    *out = is_negative ? int64_safe_neg_unsigned(utmp) : (int64_t) utmp;
    return pos;

#elif INTPTR_MAX == INT32_MAX
    _Static_assert(sizeof(uintptr_t) == sizeof(uint32_t), "Unsupported uintptr size or definition");

    uintptr_t uhigh = 0;
    int pos = buf16_to_uintptr(buf, buf_len, first_digit_index, &uhigh);
    if (pos == (int) buf_len) {
        *out = is_negative ? -((int64_t) uhigh) : ((int64_t) uhigh);
        return pos;
    } else if (UNLIKELY(pos <= 0)) {
        return pos;
    }

    uintptr_t ulow = 0;
    int new_pos = buf16_to_uintptr(buf, buf_len, pos, &ulow);
    if (UNLIKELY(new_pos <= 0)) {
        return pos;
    }
    int high_parsed_count = new_pos - pos;
    pos = new_pos;
    uint64_t combined = (uhigh << (high_parsed_count * 4)) + ulow;
    if (uint64_does_overflow_int64(combined, is_negative)) {
        combined >>= 16;
        pos--;
    }
    // this trick is useful to avoid any intermediate undefined/overflow
    *out = is_negative ? int64_safe_neg_unsigned(combined) : (int64_t) combined;

    return pos;
#else
#error "INTPTR_MAX is not either a 32 or 64 bit signed integer"
#endif
}

static int bufn_to_int64(const char buf[], size_t buf_len, size_t first_digit_index, bool negative,
    unsigned int base, int64_t *out)
{
    size_t i = first_digit_index;

    int64_t acc = 0;

    for (; i < buf_len; i++) {
        uintptr_t digit_val = char_to_base_n_digit(buf[i]);
        if (UNLIKELY(digit_val >= base)) {
            return -1;
        }
        int64_t maybe_overflowed_mul;
        if (__builtin_mul_overflow(acc, base, &maybe_overflowed_mul)) {
            *out = acc;
            return i;
        }
        intptr_t signed_digit = negative ? -((intptr_t) digit_val) : (intptr_t) digit_val;
        int64_t maybe_overflowed_add;
        if (__builtin_add_overflow(maybe_overflowed_mul, signed_digit, &maybe_overflowed_add)) {
            *out = acc;
            return i;
        }
        acc = maybe_overflowed_add;
    }

    *out = acc;
    return i;
}

int int64_parse_ascii_buf(const char buf[], size_t buf_len, unsigned int base,
    buf_to_int64_options_t options, int64_t *out)
{
    assert((base >= 2) || (base <= 36) || (buf_len < INT_MAX));

    if (buf_len < 1) {
        return -1;
    }

    size_t i = 0;

    bool negative = false;
    if ((options & BufToInt64RejectSign) == 0) {
        if (buf[0] == '-') {
            negative = true;
            i = 1;
        } else if (buf[0] == '+') {
            i = 1;
        }
    }

    while ((i < buf_len - 1) && (buf[i] == '0')) {
        i++;
    }

    // 10 and 16 bases are optimized, since they are likely more widely used
    switch (base) {
        case 10:
            return buf10_to_int64(buf, buf_len, i, negative, out);
        case 16:
            return buf16_to_int64(buf, buf_len, i, negative, out);
        default:
            return bufn_to_int64(buf, buf_len, i, negative, base, out);
    }
}
