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
 * Neg
 */

void intn_neg(const intn_digit_t num[], size_t num_len, intn_digit_t out[], size_t *out_len)
{
    size_t i;
    uint32_t carry = 1;
    for (i = 0; i < num_len; i++) {
        uint64_t temp = (uint64_t) (~num[i]) + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
    if ((carry != 0) && !(out[i - 1] >> 31)) {
        out[i] = 0xFFFFFFFF;
        i++;
    }
    *out_len = i;
}

static size_t neg_inplace(uint32_t num[], size_t num_len)
{
    size_t i;
    uint32_t carry = 1;
    for (i = 0; i < num_len; i++) {
        uint64_t temp = (uint64_t) (~num[i]) + (uint64_t) carry;
        num[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
    if ((carry != 0) && !(num[i - 1] >> 31)) {
        num[i] = 0xFFFFFFFF;
        return i;
    }
    return i - 1;
}

static bool is_negative(const uint32_t num[], size_t num_len)
{
    return (num[num_len - 1] >> 31) != 0;
}

void intn_abs(const intn_digit_t num[], size_t num_len, intn_digit_t out[], size_t *out_len)
{
    if (is_negative(num, num_len)) {
        intn_neg(num, num_len, out, out_len);
    } else {
        memcpy(out, num, num_len * sizeof(uint32_t));
        *out_len = num_len;
    }
}

/*
 * Multiplication
 */

#ifdef USE_64BIT_MUL

// Code based on Hacker's Delight book
// Compared to the original version parameters order has been changed
// also this version uses 64 bit multiplication
static void mulmns32(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
    uint64_t k, t, b;

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

    // Now w[] has the unsigned product.  Correct by
    // subtracting v*2**32m if u < 0, and
    // subtracting u*2**32n if v < 0.

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
}

void intn_mulmns(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
    mulmns32(u, m, v, n, w);
}

#else

// Code based on Hacker's Delight book
// Original code with mostly no changes, except for parameters order
static void mulmns16(const uint16_t u[], size_t m, const uint16_t v[], size_t n, uint16_t w[])
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
}

void intn_mulmns(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    mulmns16((const uint16_t *) u, m * 2, (const uint16_t *) v, n * 2, (uint16_t *) w);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "Big endian not yet supported"
#else
#error "Unsupported endianness"
#endif
}

#endif

void intn_mul_int64(int64_t num1, int64_t num2, intn_digit_t *out)
{
    intn_digit_t u[2];
    int64_to_intn_2(num1, u);
    intn_digit_t v[2];
    int64_to_intn_2(num2, v);

    intn_mulmns(u, 2, v, 2, (uint32_t *) out);
}

/*
 * Division
 */

static size_t count16(const uint16_t *num, size_t num_len)
{
    size_t count = 0;
    if (num[num_len - 1] == ((uint16_t) -1)) {
        for (int i = num_len - 2; i >= 0; i--) {
            uint16_t num_i = num[i];
            if (num_i != ((uint16_t) -1)) {
                if (num_i >> 31) {
                    count = i + 1;
                } else {
                    count = i + 2;
                }
                break;
            }
        }
    } else {
        for (int i = num_len - 1; i >= 0; i--) {
            uint16_t num_i = num[i];
            if (num_i != 0) {
                count = i + 1;
                break;
            }
        }
    }

    return count;
}

static int nlz(unsigned x)
{
    int n;
    if (x == 0)
        return (32);
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

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static inline void big_endian_in_place_swap_16(uint32_t u[], size_t m)
{
    uint16_t *dest_buf = (uint16_t *) u;
    for (size_t i = 0; i < m * 2; i += 2) {
        uint16_t tmp = dest_buf[i];
        dest_buf[i] = dest_buf[i + 1];
        dest_buf[i + 1] = tmp;
    }
}
#endif

void intn_divmns(const intn_digit_t u[], int m, const intn_digit_t v[], int n, intn_digit_t q[])
{
    uint32_t u_abs[INTN_ABS_OUT_LEN(INTN_MAX_IN_LEN)];
    size_t m_abs;
    bool u_neg = is_negative(u, m);
    intn_abs(u, m, u_abs, &m_abs);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    big_endian_in_place_swap_16(u_abs, m_abs);
#endif

    uint32_t v_abs[INTN_ABS_OUT_LEN(INTN_MAX_IN_LEN)];
    size_t n_abs;
    bool v_neg = is_negative(v, n);
    intn_abs(v, n, v_abs, &n_abs);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    big_endian_in_place_swap_16(v_abs, n_abs);
#endif

    int m_abs16 = count16((const uint16_t *) u_abs, m_abs * 2);
    int n_abs16 = count16((const uint16_t *) v_abs, n_abs * 2);

    uint16_t *q16 = (uint16_t *) q;

    if (divmnu16(q16, NULL, (uint16_t *) u_abs, (uint16_t *) v_abs, m_abs16, n_abs16) != 0) {
        abort();
    }

    int out_len16 = m_abs16 - n_abs16 + 1;
    if (out_len16 % 2 != 0) {
        q16[out_len16] = 0;
        out_len16++;
    }

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    big_endian_in_place_swap_16(q, out_len16 / 2);
#endif

    if (u_neg != v_neg) {
        neg_inplace(q, out_len16 / 2);
    }
}

size_t intn_addmns(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[])
{
    size_t n = MIN(a_len, b_len);
    size_t m = MAX(a_len, b_len);

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

    if (a_len == b_len) {
        out[i] = (uint32_t) (((int32_t) 0) - ((int32_t) carry));
        return i + 1;
    }

    uint32_t sign_extend;
    const uint32_t *longest;
    if (a_len > b_len) {
        int64_t sign_extend_temp = (int32_t) b_i;
        sign_extend = (((uint64_t) sign_extend_temp) >> 32);
        longest = (const uint32_t *) a;
    } else if (b_len > a_len) {
        int64_t sign_extend_temp = (int32_t) a_i;
        sign_extend = (((uint64_t) sign_extend_temp) >> 32);
        longest = (const uint32_t *) b;
    } else {
        ASSUME(i == m);
        goto last_step;
    }

    for (; i < m; i++) {
        uint32_t longest_i = longest[i];
        uint64_t temp = (uint64_t) longest_i + (uint64_t) sign_extend + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }

last_step:
    out[i] = (uint32_t) (((int32_t) 0) - ((int32_t) carry));

    return i + 1;
}

size_t intn_count_digits(const intn_digit_t *num, size_t num_len)
{
    if (num_len <= INTN_INT64_LEN) {
        return num_len;
    }

    size_t count = 0;
    if (num[num_len - 1] == ((uint32_t) -1)) {
        for (int i = num_len - 2; i >= 0; i--) {
            uint32_t num_i = num[i];
            if (num_i != ((uint32_t) -1)) {
                if (num_i >> 31) {
                    count = i + 1;
                } else {
                    count = i + 2;
                }
                break;
            }
        }
    } else if (num[num_len - 1] == 0) {
        for (int i = num_len - 1; i >= 0; i--) {
            uint32_t num_i = num[i];
            if (num_i != 0) {
                if (num_i >> 31) {
                    count = i + 2;
                } else {
                    count = i + 1;
                }
                break;
            }
        }
    } else {
        count = num_len;
    }

    return count;
}

void intn_sign_extend(const intn_digit_t *num, size_t num_len, size_t extend_to, intn_digit_t *out)
{
    int sign = (num[num_len - 1] >> 31) ? 0xFF : 0x00;

    memcpy(out, num, num_len * sizeof(uint32_t));
    memset(out + num_len, sign, (extend_to - num_len) * sizeof(uint32_t));
}

double intn_to_double(const intn_digit_t *num, size_t len)
{
    uint32_t num_abs[INTN_ABS_OUT_LEN(INTN_MAX_IN_LEN)];
    size_t num_abs_len;
    bool num_neg = is_negative(num, len);
    intn_abs(num, len, num_abs, &num_abs_len);

    double acc = 0.0;
    double base = ((double) (UINT32_MAX)) + 1;

    for (int i = num_abs_len - 1; i >= 0; i--) {
        acc = acc * base + ((double) num_abs[i]);
    }

    return num_neg ? -acc : acc;
}

int intn_from_double(double dnum, intn_digit_t *out)
{
    bool is_negative;
    double d;
    if (dnum >= 0) {
        is_negative = false;
        d = dnum;
    } else {
        is_negative = true;
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

    if (is_negative) {
        digits = neg_inplace(out, digits);
    }

    return digits;
}

char *intn_to_string(const intn_digit_t *num, size_t len, int base, size_t *string_len)
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

    char *outbuf = malloc(257);
    if (IS_NULL_PTR(outbuf)) {
        return NULL;
    }
    char *end = outbuf + 256;
    *end = '\0';

    uint16_t *u;
    size_t m;

    bool negative_integer = is_negative(num, len);

    if (negative_integer) {
        size_t m_abs;
        intn_abs(num, len, tmp_buf1, &m_abs);
        m = m_abs;
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
        big_endian_in_place_swap_16(tmp_buf1, m);
#endif
    } else {
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
    }
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

    size_t str_size = 257 - (end - outbuf);
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
    out[2] = 0;
}

int intn_parse(const char buf[], size_t buf_len, int base, intn_digit_t *out)
{
    buf_to_int64_options_t buf_to_int64_opts = BufToInt64NoOptions;

    size_t pos = 0;

    memset(out, 0, sizeof(intn_digit_t) * INTN_MAX_RES_LEN);
    size_t out_len = 2;

    bool is_negative = false;
    int parsed_digits;
    do {
        int64_t parsed_chunk = 0;
        parsed_digits = int64_parse_ascii_buf(
            buf + pos, buf_len - pos, base, buf_to_int64_opts, &parsed_chunk);
        if (parsed_chunk < 0) {
            parsed_chunk = -parsed_chunk;
            is_negative = true;
        }

        if (UNLIKELY(parsed_digits <= 0)) {
            return -1;
        }

        // 10^19 takes 64 unsigned bits, so 3 digits
        intn_digit_t mult[3];
        ipow(base, parsed_digits, mult);

        intn_digit_t new_out[INTN_MAX_RES_LEN];
        // TODO: check overflows
        intn_mulmns(out, out_len, mult, 3, new_out);
        size_t new_out_len = MAX(2, intn_count_digits(new_out, INTN_MUL_OUT_LEN(out_len, 2)));

        intn_digit_t parsed_as_intn[2];
        int64_to_intn_2(parsed_chunk, parsed_as_intn);

        // TODO: check overflows
        out_len = intn_addmns(new_out, new_out_len, parsed_as_intn, 2, out);

        pos += parsed_digits;
        buf_to_int64_opts = BufToInt64RejectSign;
    } while (pos < buf_len);

    if (is_negative) {
        out_len = neg_inplace(out, out_len);
    }

    return out_len;
}
