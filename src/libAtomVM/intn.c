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

#include "intn.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USE_64BIT_MUL

#include "utils.h"

#define INTN_DIVMNU_MAX_IN_LEN (INTN_MAX_IN_LEN + 1)

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/*
 * Multiplication
 */

#ifdef USE_64BIT_MUL

// Code based on Hacker's Delight book
// Compared to the original version this version uses 32x32 bit multiplication
static void mulmnu32(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
    uint64_t k, t;

    for (size_t i = 0; i < m; i++)
        w[i] = 0;

    for (size_t j = 0; j < n; j++) {
        k = 0;
        for (size_t i = 0; i < m; i++) {
            uint64_t u_i = u[i];
            uint64_t v_j = v[j];
            uint64_t w_i_j = w[i + j];
            t = u_i * v_j + w_i_j + k;
            w[i + j] = t; // (I.e., t & 0xFFFFFFFF).
            k = t >> 32;
        }
        w[j + m] = k;
    }

    /*
        Original code had support to signed mul in 2-complement

        // Now w[] has the unsigned product.  Correct by
        // subtracting v*2**32m if u < 0, and
        // subtracting u*2**32n if v < 0.

        uint64_t b;

        if ((int32_t) u[m - 1] < 0) {
            b = 0; // Initialize borrow.
            for (size_t j = 0; j < n; j++) {
                uint64_t w_j_m = w[j + m];
                uint64_t v_j = v[j];
                t = w_j_m - v_j - b;
                w[j + m] = t;
                b = t >> 63;
            }
        }
        if ((int32_t) v[n - 1] < 0) {
            b = 0;
            for (size_t i = 0; i < m; i++) {
                uint64_t w_i_n = w[i + n];
                uint64_t u_i = u[i];
                t = w_i_n - u_i - b;
                w[i + n] = t;
                b = t >> 63;
            }
        }
    */
}

void intn_mulmnu(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
    mulmnu32(u, m, v, n, w);
}

#else

// Code based on Hacker's Delight book
static void mulmnu16(const uint16_t u[], size_t m, const uint16_t v[], size_t n, uint16_t w[])
{
    unsigned int k, t, b;

    for (size_t i = 0; i < m; i++)
        w[i] = 0;

    for (size_t j = 0; j < n; j++) {
        k = 0;
        for (size_t i = 0; i < m; i++) {
            t = u[i] * v[j] + w[i + j] + k;
            w[i + j] = t; // (I.e., t & 0xFFFF).
            k = t >> 16;
        }
        w[j + m] = k;
    }

    /*
        Original code had support to signed mul in 2-complement

        // Now w[] has the unsigned product.  Correct by
        // subtracting v*2**16m if u < 0, and
        // subtracting u*2**16n if v < 0.

        if ((int16_t) u[m - 1] < 0) {
            b = 0; // Initialize borrow.
            for (size_t j = 0; j < n; j++) {
                t = w[j + m] - v[j] - b;
                w[j + m] = t;
                b = t >> 31;
            }
        }
        if ((int16_t) v[n - 1] < 0) {
            b = 0;
            for (size_t i = 0; i < m; i++) {
                t = w[i + n] - u[i] - b;
                w[i + n] = t;
                b = t >> 31;
            }
        }
        return;
    */
}

void intn_mulmnu(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    mulmnu16((const uint16_t *) u, m * 2, (const uint16_t *) v, n * 2, (uint16_t *) w);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "Big endian not yet supported"
#else
#error "Unsupported endianness"
#endif
}

#endif

void intn_mul_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign)
{
    intn_digit_t u[2];
    intn_integer_sign_t u_sign;
    int64_to_intn_2(num1, u, &u_sign);
    intn_digit_t v[2];
    intn_integer_sign_t v_sign;
    int64_to_intn_2(num2, v, &v_sign);

    *out_sign = intn_muldiv_sign(u_sign, v_sign);
    intn_mulmnu(u, 2, v, 2, (uint32_t *) out);
}

/*
 * Division
 */

static size_t count16(const uint16_t *num, size_t num_len)
{
    int i;
    for (i = num_len - 1; i >= 0; i--) {
        if (num[i] != 0) {
            break;
        }
    }
    size_t count = i + 1;

    return count;
}

static inline uint32_t nlz(uint32_t x)
{
    // This function is used only from divmnu, that doesn't allow 32 leading zeros
    ASSUME(x != 0);

#ifdef __has_builtin
#define HAS_BUILTIN(x) __has_builtin(x)
#else
#define HAS_BUILTIN(x) 0
#endif

#if defined(__GNUC__) \
    || (HAS_BUILTIN(__builtin_clz) && HAS_BUILTIN(__builtin_clzl) && HAS_BUILTIN(__builtin_clzll))
    if (sizeof(unsigned int) == sizeof(uint32_t)) {
        return __builtin_clz(x);
    } else if (sizeof(unsigned long) == sizeof(uint32_t)) {
        return __builtin_clzl(x);
    } else if (sizeof(unsigned long long) == sizeof(uint32_t)) {
        return __builtin_clzll(x);
    }
#elif __STDC_VERSION == 202311L
    return stdc_leading_zeros(x);
#else
    uint32_t n;
    if (x == 0) {
        // Original version was returning 32, but in our version 32 zeros are not allowed
        UNREACHABLE();
        // return (32);
    }
    n = 1;
    if ((x >> 16) == 0) {
        n = n + 16;
        x = x << 16;
    }
    if ((x >> 24) == 0) {
        n = n + 8;
        x = x << 8;
    }
    if ((x >> 28) == 0) {
        n = n + 4;
        x = x << 4;
    }
    if ((x >> 30) == 0) {
        n = n + 2;
        x = x << 2;
    }
    n = n - (x >> 31);
    return n;
#endif
}

// this function doesn't use alloca as the original one
// but it is limited to INTN_DIVMNU_MAX_IN_LEN * 2 16 bit digits
static int divmnu16(
    uint16_t q[], uint16_t r[], const uint16_t u[], const uint16_t v[], int m, int n)
{

    const unsigned b = 65536; // Number base (16 bits).
    unsigned qhat; // Estimated quotient digit.
    unsigned rhat; // A remainder.
    unsigned p; // Product of two digits.
    int s, i, j, t, k;

    if (m < n || n <= 0 || v[n - 1] == 0)
        return 1; // Return if invalid param.

    if (n == 1) { // Take care of
        k = 0; // the case of a
        for (j = m - 1; j >= 0; j--) { // single-digit
            q[j] = (k * b + u[j]) / v[0]; // divisor here.
            k = (k * b + u[j]) - q[j] * v[0];
        }
        if (r != NULL)
            r[0] = k;
        return 0;
    }

    // Normalize by shifting v left just enough so that
    // its high-order bit is on, and shift u left the
    // same amount.  We may have to append a high-order
    // digit on the dividend; we do that unconditionally.

    s = nlz(v[n - 1]) - 16; // 0 <= s <= 15.
    uint16_t vn[INTN_DIVMNU_MAX_IN_LEN * (sizeof(intn_digit_t) / sizeof(uint16_t))];
    for (i = n - 1; i > 0; i--)
        vn[i] = (v[i] << s) | (v[i - 1] >> (16 - s));
    vn[0] = v[0] << s;

    uint16_t un[(INTN_DIVMNU_MAX_IN_LEN * (sizeof(intn_digit_t) / sizeof(uint16_t))) + 1];
    un[m] = u[m - 1] >> (16 - s);
    for (i = m - 1; i > 0; i--)
        un[i] = (u[i] << s) | (u[i - 1] >> (16 - s));
    un[0] = u[0] << s;

    for (j = m - n; j >= 0; j--) { // Main loop.
        // Compute estimate qhat of q[j].
        qhat = (un[j + n] * b + un[j + n - 1]) / vn[n - 1];
        rhat = (un[j + n] * b + un[j + n - 1]) - qhat * vn[n - 1];
    again:
        if (qhat >= b || qhat * vn[n - 2] > b * rhat + un[j + n - 2]) {
            qhat = qhat - 1;
            rhat = rhat + vn[n - 1];
            if (rhat < b)
                goto again;
        }

        // Multiply and subtract.
        k = 0;
        for (i = 0; i < n; i++) {
            p = qhat * vn[i];
            t = un[i + j] - k - (p & 0xFFFF);
            un[i + j] = t;
            k = (p >> 16) - (t >> 16);
        }
        t = un[j + n] - k;
        un[j + n] = t;

        q[j] = qhat; // Store quotient digit.
        if (t < 0) { // If we subtracted too
            q[j] = q[j] - 1; // much, add back.
            k = 0;
            for (i = 0; i < n; i++) {
                t = un[i + j] + vn[i] + k;
                un[i + j] = t;
                k = t >> 16;
            }
            un[j + n] = un[j + n] + k;
        }
    } // End j.
    // If the caller wants the remainder, unnormalize
    // it and pass it back.
    if (r != NULL) {
        for (i = 0; i < n; i++)
            r[i] = (un[i] >> s) | (un[i + 1] << (16 - s));
    }
    return 0;
}

void print_num(const uint32_t num[], int len)
{
    for (int i = 0; i < len; i++) {
        fprintf(stderr, "0x%x ", (unsigned int) num[i]);
    }
    fprintf(stderr, "\n");
}

size_t intn_addmnu(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[])
{
    size_t n = MIN(a_len, b_len);

    ASSUME(n >= 1);

    uint32_t a_i = 0;
    uint32_t b_i = 0;
    uint32_t carry = 0;
    size_t i;
    for (i = 0; i < n; i++) {
        a_i = a[i];
        b_i = b[i];
        uint64_t temp = (uint64_t) a_i + (uint64_t) b_i + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }

    size_t m;
    const uint32_t *longest;
    if (a_len >= b_len) {
        m = a_len;
        longest = (const uint32_t *) a;
    } else {
        m = b_len;
        longest = (const uint32_t *) b;
    }

    for (; i < m; i++) {
        uint32_t longest_i = longest[i];
        uint64_t temp = (uint64_t) longest_i + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }

    if (carry) {
        out[i] = carry;
        i++;
    }

    return i;
}

size_t intn_count_digits(const intn_digit_t *num, size_t num_len)
{
    int i;
    for (i = num_len - 1; i >= 0; i--) {
        if (num[i] != 0) {
            break;
        }
    }
    size_t count = i + 1;

    return count;
}

double intn_to_double(const intn_digit_t *num, size_t len, intn_integer_sign_t sign)
{
    double acc = 0.0;
    double base = ((double) (UINT32_MAX)) + 1;

    for (int i = len - 1; i >= 0; i--) {
        acc = acc * base + ((double) num[i]);
    }

    return (sign == IntNNegativeInteger) ? -acc : acc;
}

int intn_from_double(double dnum, intn_digit_t *out, intn_integer_sign_t *out_sign)
{
    double d;
    if (dnum >= 0) {
        *out_sign = IntNPositiveInteger;
        d = dnum;
    } else {
        *out_sign = IntNNegativeInteger;
        d = -dnum;
    }

    size_t digits = 0;
    double base = ((double) (UINT32_MAX)) + 1;

    while (d >= 1.0) {
        d /= base;
        digits++;
    }

    if (digits >= INTN_MAX_RES_LEN) {
        return -1;
    }

    for (int i = digits - 1; i >= 0; i--) {
        d *= base;
        uint32_t integer_part = d;
        out[i] = integer_part;
        d -= integer_part;
    }

    return digits;
}

char *intn_to_string(
    const intn_digit_t *num, size_t len, intn_integer_sign_t num_sign, int base, size_t *string_len)
{
    // First base is 2, last is 36
    // This is the maximum divisor that can fit a signed int16
    static const uint16_t bases[] = { 16384, 19683, 16384, 15625, 7776, 16807, 4096, 6561, 10000,
        14641, 20736, 28561, 2744, 3375, 4096, 4913, 5832, 6859, 8000, 9261, 10648, 12167, 13824,
        15625, 17576, 19683, 21952, 24389, 27000, 29791, 1024, 1089, 1156, 1225, 1296 };

    /*
        TODO: do not use division for powers of 2, use this table that marks them with 0
        static const uin16_t bases[] = { 0, 19683, 0, 15625, 7776, 16807, 0, 6561, 10000, 14641,
       20736, 28561, 2744, 3375, 0, 4913, 5832, 6859, 8000, 9261, 10648, 12167, 13824, 15625, 17576,
            19683, 21952, 24389, 27000, 29791, 0, 1089, 1156, 1225, 1296
        };
    */

    static const uint8_t pad[] = { 14, 9, 7, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2 };

    // let's keep space for abs(INT_MIN), that is bigger than INT_MAX
    // and it must be supported, since we must allow converting to string INT_MIN as well
    int tmp_buf_size = (256 / (sizeof(uint32_t) * 8)) + 1;
    uint32_t tmp_buf1[tmp_buf_size];
    uint32_t tmp_buf2[tmp_buf_size];

    char *outbuf = malloc(258);
    if (IS_NULL_PTR(outbuf)) {
        return NULL;
    }
    char *end = outbuf + 257;
    *end = '\0';

    uint16_t *u;
    size_t m;

    bool negative_integer = num_sign == IntNNegativeInteger;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(tmp_buf1, num, len * sizeof(uint32_t));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint16_t *dest_buf = (uint16_t *) tmp_buf1;
    const uint16_t *num16 = (const uint16_t *) num;
    for (size_t i = 0; i < len * 2; i += 2) {
        dest_buf[i] = num16[i + 1];
        dest_buf[i + 1] = num16[i];
    }
#endif
    m = len;
    u = (uint16_t *) tmp_buf1;

    int m16 = count16(u, m * 2);

    uint16_t *q = (uint16_t *) tmp_buf2;

    do {
        uint16_t r;

        // divide in chunks that can be converted later
        // using a bigger divisor like 10000 reduces the calls to this function
        // so regular division on a smaller number can be used later
        // example: 123456789 % 10000 = 6789, 123456789 / 10000 = 12345
        if (UNLIKELY(divmnu16(q, &r, u, &bases[base - 2], m16, 1) != 0)) {
            abort();
        }

        size_t intlen = intptr_write_to_ascii_buf(r, base, end);
        end -= intlen;

        m16 = count16(q, m16);

        // add padding: `intptr_write_to_ascii_buf(7, 10, ptr)` will write just '7',
        // but when dealing with base 10 we need 0007
        // in order to handle numbers such as 110007 (note: 110007  / 10000 -> 11.0007, those
        // digits cannot be discarded)
        if (m16) {
            int padsize = pad[base - 2] - intlen;
            end -= padsize;
            for (int i = 0; i < padsize; i++) {
                end[i] = '0';
            }
        }

        // swap q (output) and u (input) buffers
        uint16_t *swap_tmp = u;
        u = q;
        q = swap_tmp;
    } while (m16 != 0);

    if (negative_integer) {
        end -= 1;
        *end = '-';
    }

    size_t str_size = 258 - (end - outbuf);
    memmove(outbuf, end, str_size);

    *string_len = str_size - 1;
    char *shrunk = realloc(outbuf, str_size);
    if (IS_NULL_PTR(shrunk)) {
// GCC 12 is raising here a false positive warning, according to man realloc:
// "If realloc() fails, the original block is left untouched; it is not freed or moved."
#pragma GCC diagnostic push
#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ == 12
#pragma GCC diagnostic ignored "-Wuse-after-free"
#endif
        free(outbuf);
#pragma GCC diagnostic pop
        return NULL;
    }
    return shrunk;
}

static void ipow(int base, int exp, intn_digit_t *out)
{
    uint64_t acc = 1;
    for (int i = 0; i < exp; i++) {
        acc *= base;
    }
    out[0] = acc & 0xFFFFFFFF;
    out[1] = acc >> 32;
}

int intn_parse(
    const char buf[], size_t buf_len, int base, intn_digit_t *out, intn_integer_sign_t *out_sign)
{
    static const uint8_t base_max_digits[] = { 63, 40, 31, 27, 24, 22, 21, 20, 19, 18, 17, 17, 16,
        16, 15, 15, 15, 15, 14, 14, 14, 14, 13, 13, 13, 13,
        13, 13, 13, 12, 12, 12, 12, 12, 12 };

    buf_to_int64_options_t buf_to_int64_opts = BufToInt64NoOptions;
    size_t max_digits = SIZE_MAX;

    size_t pos = 0;

    memset(out, 0, sizeof(intn_digit_t) * INTN_MAX_RES_LEN);
    size_t out_len = 2;

    *out_sign = IntNPositiveInteger;
    do {
        int64_t parsed_chunk = 0;
        // at first iteration `parsed_digits` will be wrong since it will contain any leading zero
        // or sign, but on first iteration we are not going use it
        int parsed_digits = int64_parse_ascii_buf(
            buf + pos, MIN(buf_len - pos, max_digits), base, buf_to_int64_opts, &parsed_chunk);
        if (parsed_chunk < 0) {
            parsed_chunk = -parsed_chunk;
            *out_sign = IntNNegativeInteger;
        }

        if (UNLIKELY(parsed_digits <= 0)) {
            return -1;
        }

        intn_digit_t new_out[INTN_MAX_RES_LEN + 5];
        size_t new_out_len;
        if (buf_to_int64_opts == BufToInt64NoOptions) {
            // first iteration here, just set to 0
            memset(new_out, 0, sizeof(intn_digit_t) * INTN_MAX_RES_LEN);
            new_out_len = 2;
        } else {
            // 10^19 takes 64 unsigned bits, so 3 digits
            intn_digit_t mult[2];
            ipow(base, parsed_digits, mult);
            // TODO: check overflows
            intn_mulmnu(out, out_len, mult, 2, new_out);
            new_out_len = MAX(2, intn_count_digits(new_out, INTN_MUL_OUT_LEN(out_len, 2)));
        }

        intn_integer_sign_t ignored_sign;
        intn_digit_t parsed_as_intn[2];
        int64_to_intn_2(parsed_chunk, parsed_as_intn, &ignored_sign);

        // TODO: check overflows
        out_len = intn_addmnu(new_out, new_out_len, parsed_as_intn, 2, out);

        pos += parsed_digits;
        buf_to_int64_opts = BufToInt64RejectSign;
        max_digits = base_max_digits[base - 2];
    } while (pos < buf_len);

    // let's count at the end
    return out_len;
}

static size_t cond_neg_in_place(intn_integer_sign_t sign, intn_digit_t out[])
{
    if (sign == IntNNegativeInteger) {
        uint32_t carry = 1;
        size_t i;
        int last_non_zero = -1;
        for (i = 0; i < INTN_MAX_RES_LEN - 1; i++) {
            uint64_t temp = (uint64_t) (~out[i]) + (uint64_t) carry;
            if ((uint32_t) temp != 0) {
                last_non_zero = i;
            }
            out[i] = (uint32_t) temp;
            carry = temp >> 32;
        }
        if (carry) {
            out[i] = carry;
            return i;
        } else {
            return last_non_zero + 1;
        }
    } else {
        return intn_count_digits(out, INTN_MAX_IN_LEN);
    }
}

int intn_from_integer_bytes(const uint8_t in[], size_t in_size, intn_from_integer_options_t opts,
    intn_digit_t out[], intn_integer_sign_t *out_sign)
{
    size_t msb_index;
    if (opts & IntnLittleEndian) {
        msb_index = in_size - 1;
    } else {
        msb_index = 0;
    }

    uint8_t filler = 0x00;
    intn_integer_sign_t sign = IntNPositiveInteger;
    if (opts & IntnSigned) {
        if (in[msb_index] & 0x80) {
            filler = 0xFF;
            sign = IntNNegativeInteger;
        }
        *out_sign = sign;
    }

    memset(out, filler, INTN_MAX_RES_LEN * sizeof(intn_digit_t));

    size_t dest_j = in_size;

    if (UNLIKELY(dest_j / sizeof(intn_digit_t) >= INTN_MAX_RES_LEN)) {
        return -1;
    }

    if (opts & IntnLittleEndian) {
        for (int i = in_size - 1; i >= 0; i--) {
            dest_j--;
            size_t dest_block = dest_j / sizeof(intn_digit_t);
            out[dest_block] <<= 8;
            out[dest_block] |= in[i];
        }
    } else {
        for (size_t i = 0; i < in_size; i++) {
            dest_j--;
            size_t dest_block = dest_j / sizeof(intn_digit_t);
            out[dest_block] <<= 8;
            out[dest_block] |= in[i];
        }
    }

    return cond_neg_in_place(sign, out);
}
