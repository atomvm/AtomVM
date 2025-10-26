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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

#define USE_64BIT_MUL

#define UINT16_IN_A_DIGIT (sizeof(intn_digit_t) / sizeof(uint16_t))

#define INTN_DIVMNU_MAX_IN_LEN (INTN_MAX_IN_LEN + 1)

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

/* Uncomment this for debug:
void print_num(const intn_digit_t num[], int len)
{
    for (int i = 0; i < len; i++) {
        fprintf(stderr, "0x%x ", (unsigned int) num[i]);
    }
    fprintf(stderr, "\n");
}
*/

static size_t neg_and_count_in_place(intn_digit_t out[], size_t len);

static inline size_t pad_uint16_to_digits(uint16_t n16[], size_t n16_len)
{
    _Static_assert(UINT16_IN_A_DIGIT == 2, "assuming 32-bit intn_digit_t");
    if ((n16_len % UINT16_IN_A_DIGIT) != 0) {
        // change this the day sizeof(intn_digit_t) != 4
        n16[n16_len] = 0;
        return n16_len + 1;
    }
    return n16_len;
}

/*
 * Multiplication
 */

#ifdef USE_64BIT_MUL

// Code based on Hacker's Delight book
// Compared to the original version this version uses 32x32 bit multiplication
static void mulmnu32(const uint32_t u[], size_t m, const uint32_t v[], size_t n, uint32_t w[])
{
    uint64_t k, t;

    for (size_t i = 0; i < m; i++) {
        w[i] = 0;
    }

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

void intn_mulu(const uint32_t m[], size_t m_len, const uint32_t n[], size_t n_len, uint32_t out[])
{
    mulmnu32(m, m_len, n, n_len, out);
}

#else

// Code based on Hacker's Delight book
static void mulmnu16(const uint16_t u[], size_t m, const uint16_t v[], size_t n, uint16_t w[])
{
    unsigned int k, t, b;

    for (size_t i = 0; i < m; i++) {
        w[i] = 0;
    }

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

void intn_mulu(const uint32_t m[], size_t m_len, const uint32_t n[], size_t n_len, uint32_t out[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    mulmnu16((const uint16_t *) m, m_len * 2, (const uint16_t *) n, n_len * 2, (uint16_t *) out);
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
    intn_from_int64(num1, u, &u_sign);
    intn_digit_t v[2];
    intn_integer_sign_t v_sign;
    intn_from_int64(num2, v, &v_sign);

    *out_sign = intn_muldiv_sign(u_sign, v_sign);
    intn_mulu(u, 2, v, 2, (uint32_t *) out);
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

// make sure that x != 0 before calling this function
static inline uint32_t uint32_nlz(uint32_t x)
{
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
#elif __STDC_VERSION__ >= 202311L
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

    if (m < n || n <= 0 || v[n - 1] == 0) {
        return 1; // Return if invalid param.
    }

    if (n == 1) { // Take care of
        k = 0; // the case of a
        for (j = m - 1; j >= 0; j--) { // single-digit
            q[j] = (k * b + u[j]) / v[0]; // divisor here.
            k = (k * b + u[j]) - q[j] * v[0];
        }
        if (r != NULL) {
            r[0] = k;
        }
        return 0;
    }

    // Normalize by shifting v left just enough so that
    // its high-order bit is on, and shift u left the
    // same amount.  We may have to append a high-order
    // digit on the dividend; we do that unconditionally.

    s = uint32_nlz(v[n - 1]) - 16; // 0 <= s <= 15.
    uint16_t vn[INTN_DIVMNU_MAX_IN_LEN * UINT16_IN_A_DIGIT];
    for (i = n - 1; i > 0; i--) {
        vn[i] = (v[i] << s) | (v[i - 1] >> (16 - s));
    }
    vn[0] = v[0] << s;

    uint16_t un[(INTN_DIVMNU_MAX_IN_LEN * UINT16_IN_A_DIGIT) + 1];
    un[m] = u[m - 1] >> (16 - s);
    for (i = m - 1; i > 0; i--) {
        un[i] = (u[i] << s) | (u[i - 1] >> (16 - s));
    }
    un[0] = u[0] << s;

    for (j = m - n; j >= 0; j--) { // Main loop.
        // Compute estimate qhat of q[j].
        qhat = (un[j + n] * b + un[j + n - 1]) / vn[n - 1];
        rhat = (un[j + n] * b + un[j + n - 1]) - qhat * vn[n - 1];
    again:
        if (qhat >= b || qhat * vn[n - 2] > b * rhat + un[j + n - 2]) {
            qhat = qhat - 1;
            rhat = rhat + vn[n - 1];
            if (rhat < b) {
                goto again;
            }
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
        for (i = 0; i < n; i++) {
            r[i] = (un[i] >> s) | (un[i + 1] << (16 - s));
        }
    }
    return 0;
}

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
static void big_endian_digits_to_uint16(const intn_digit_t num[], size_t len, uint16_t dest_buf[])
{
    const uint16_t *num16 = (const uint16_t *) num;
    for (size_t i = 0; i < len * UINT16_IN_A_DIGIT; i += UINT16_IN_A_DIGIT) {
        // change this the day sizeof(intn_digit_t) != 4
        dest_buf[i] = num16[i + 1];
        dest_buf[i + 1] = num16[i];
    }
}

static void big_endian_uint16_to_digit_in_place(uint16_t num16[], size_t len16)
{
    for (size_t i = 0; i < len16; i += UINT16_IN_A_DIGIT) {
        // change this the day sizeof(intn_digit_t) != 4
        uint16_t num16_i = num16[i];
        num16[i] = num16[i + 1];
        num16[i + 1] = num16_i;
    }
}
#endif

size_t intn_divu(const intn_digit_t m[], size_t m_len, const intn_digit_t n[], size_t n_len,
    intn_digit_t q_out[], intn_digit_t r_out[], size_t *r_out_len)
{
    uint16_t *u;
    uint16_t *v;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    u = (uint16_t *) m;
    v = (uint16_t *) n;
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint16_t u_buf16[INTN_DIVMNU_MAX_IN_LEN * UINT16_IN_A_DIGIT];
    big_endian_digits_to_uint16(m, m_len, u_buf16);
    u = u_buf16;
    uint16_t v_buf16[INTN_DIVMNU_MAX_IN_LEN * UINT16_IN_A_DIGIT];
    big_endian_digits_to_uint16(n, n_len, v_buf16);
    v = v_buf16;
#endif

    size_t u_len16 = count16(u, m_len * UINT16_IN_A_DIGIT);
    size_t v_len16 = count16(v, n_len * UINT16_IN_A_DIGIT);

    uint16_t *q = (uint16_t *) q_out;
    uint16_t *r = (uint16_t *) r_out;
    if (UNLIKELY(divmnu16(q, r, u, v, u_len16, v_len16) != 0)) {
        abort();
    }

    size_t counted_q16_len = count16(q, u_len16 - v_len16 + 1);
    size_t padded_q_len = pad_uint16_to_digits(q, counted_q16_len);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    big_endian_uint16_to_digit_in_place(q, padded_q_len);
#endif

    if (r_out != NULL) {
        size_t counted_r16_len = count16(r, v_len16);
        size_t padded_r_len = pad_uint16_to_digits(r, counted_r16_len);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
        big_endian_uint16_to_digit_in_place(r, padded_r_len);
#endif

        if (r_out_len != NULL) {
            *r_out_len = padded_r_len / UINT16_IN_A_DIGIT;
        }
    }

    return padded_q_len / UINT16_IN_A_DIGIT;
}

int intn_cmp(const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len)
{
    size_t normal_a_len = intn_count_digits(a, a_len);
    size_t normal_b_len = intn_count_digits(b, b_len);

    if (normal_a_len > normal_b_len) {
        return 1;
    }
    if (normal_a_len < normal_b_len) {
        return -1;
    }

    for (size_t i = normal_a_len; i > 0; i--) {
        if (a[i - 1] > b[i - 1]) {
            return 1;
        }
        if (a[i - 1] < b[i - 1]) {
            return -1;
        }
    }

    return 0;
}

size_t intn_addu(
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

size_t intn_add(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    size_t result_len;

    // Case 1: Same sign - add magnitudes, keep sign
    if (m_sign == n_sign) {
        *out_sign = m_sign;
        result_len = intn_addu(m, m_len, n, n_len, out);
    }
    // Case 2: Different signs - subtract smaller from larger
    else {
        int cmp = intn_cmp(m, m_len, n, n_len);
        if (cmp >= 0) {
            // |m| >= |n|, result takes sign of m
            *out_sign = m_sign;
            result_len = intn_subu(m, m_len, n, n_len, out);
        } else {
            // |m| < |n|, result takes sign of n
            *out_sign = n_sign;
            result_len = intn_subu(n, n_len, m, m_len, out);
        }
    }

    // Normalize 0 sign
    if (result_len == 1 && out[0] == 0) {
        *out_sign = IntNPositiveInteger;
    }

    return result_len;
}

size_t intn_add_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign)
{
    intn_digit_t u[2];
    intn_integer_sign_t u_sign;
    intn_from_int64(num1, u, &u_sign);
    intn_digit_t v[2];
    intn_integer_sign_t v_sign;
    intn_from_int64(num2, v, &v_sign);

    return intn_add(u, 2, u_sign, v, 2, v_sign, out, out_sign);
}

// This function assumes a >= b
// Caller must ensure this precondition
size_t intn_subu(
    const intn_digit_t a[], size_t a_len, const intn_digit_t b[], size_t b_len, intn_digit_t out[])
{
    uint32_t borrow = 0;
    size_t i;

    for (i = 0; i < b_len; i++) {
        uint64_t temp = (uint64_t) a[i] - (uint64_t) b[i] - (uint64_t) borrow;
        out[i] = (uint32_t) temp; // Lower 32 bits
        borrow = (temp >> 63) & 1; // Check if result was negative (borrow needed)
    }

    for (; i < a_len; i++) {
        uint64_t temp = (uint64_t) a[i] - (uint64_t) borrow;
        out[i] = (uint32_t) temp;
        borrow = (temp >> 63) & 1;
    }

    return i;
}

size_t intn_sub(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    // m - n = m + (-n)
    // Just flip the sign of n and call addition
    return intn_add(m, m_len, m_sign, n, n_len, intn_negate_sign(n_sign), out, out_sign);
}

size_t intn_sub_int64(int64_t num1, int64_t num2, intn_digit_t *out, intn_integer_sign_t *out_sign)
{
    intn_digit_t u[2];
    intn_integer_sign_t u_sign;
    intn_from_int64(num1, u, &u_sign);
    intn_digit_t v[2];
    intn_integer_sign_t v_sign;
    intn_from_int64(num2, v, &v_sign);

    return intn_sub(u, 2, u_sign, v, 2, v_sign, out, out_sign);
}

static void neg(const intn_digit_t in[], size_t in_len, intn_digit_t out[])
{
    uint32_t carry = 1;
    for (size_t i = 0; i < in_len; i++) {
        uint64_t temp = (uint64_t) (~in[i]) + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
}

static void cond_neg(
    intn_integer_sign_t sign, const intn_digit_t in[], size_t in_len, intn_digit_t out[])
{
    if (sign == IntNPositiveInteger) {
        memcpy(out, in, sizeof(intn_digit_t) * in_len);
    } else {
        neg(in, in_len, out);
    }
}

static size_t prepare_working_buf(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, const intn_digit_t *b[],
    size_t *b_len, intn_integer_sign_t *b_sign, intn_digit_t out[])
{
    const intn_digit_t *longest;
    size_t longest_len;
    intn_integer_sign_t longest_sign;

    if (m_len > n_len) {
        longest = m;
        longest_len = m_len;
        longest_sign = m_sign;
        *b = n;
        *b_len = n_len;
        *b_sign = n_sign;
    } else {
        longest = n;
        longest_len = n_len;
        longest_sign = n_sign;
        *b = m;
        *b_len = m_len;
        *b_sign = m_sign;
    }

    cond_neg(longest_sign, longest, longest_len, out);
    return longest_len;
}

typedef intn_digit_t (*bit_op_t)(intn_digit_t a, intn_digit_t b);

static inline void signed_bitwise(const intn_digit_t b[], size_t b_len, intn_integer_sign_t b_sign,
    intn_digit_t out[], size_t out_len, bit_op_t bit_op)
{
    if (b_sign == IntNPositiveInteger) {
        for (size_t i = 0; i < b_len; i++) {
            out[i] = bit_op(out[i], b[i]);
        }
        for (size_t i = b_len; i < out_len; i++) {
            out[i] = bit_op(out[i], 0);
        }
    } else {
        uint32_t carry = 1;
        for (size_t i = 0; i < b_len; i++) {
            uint64_t temp = (uint64_t) (~b[i]) + (uint64_t) carry;
            out[i] = bit_op(out[i], (uint32_t) temp);
            carry = temp >> 32;
        }
        if (b_len < out_len) {
            out[b_len] = bit_op(out[b_len], (UINT32_MAX) + carry);
        }
        for (size_t i = b_len + 1; i < out_len; i++) {
            out[i] = bit_op(out[i], UINT32_MAX);
        }
    }
}

static inline intn_integer_sign_t sign_bitwise(
    intn_integer_sign_t m_sign, intn_integer_sign_t n_sign, bit_op_t bit_op)
{
    return (intn_integer_sign_t) bit_op((unsigned int) m_sign, (unsigned int) n_sign)
        & IntNNegativeInteger;
}

// normalizes -0 to 0
static inline size_t count_and_normalize_sign(
    const intn_digit_t num[], size_t len, intn_integer_sign_t sign, intn_integer_sign_t *out_sign)
{
    size_t count = intn_count_digits(num, len);
    if ((count == 0) && (sign == IntNNegativeInteger)) {
        *out_sign = IntNPositiveInteger;
    } else {
        *out_sign = sign;
    }
    return count;
}

static inline intn_digit_t digit_bor(intn_digit_t a, intn_digit_t b)
{
    return a | b;
}

size_t intn_bor(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    intn_digit_t working_buf[INTN_MAX_IN_LEN];

    const intn_digit_t *b;
    size_t b_len;
    intn_integer_sign_t b_sign;

    size_t count
        = prepare_working_buf(m, m_len, m_sign, n, n_len, n_sign, &b, &b_len, &b_sign, working_buf);

    signed_bitwise(b, b_len, b_sign, working_buf, count, digit_bor);
    intn_integer_sign_t res_sign = sign_bitwise(m_sign, n_sign, digit_bor);

    cond_neg(res_sign, working_buf, count, out);
    *out_sign = res_sign;

    return count;
}

static inline intn_digit_t digit_band(intn_digit_t a, intn_digit_t b)
{
    return a & b;
}

size_t intn_band(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    intn_digit_t working_buf[INTN_MAX_IN_LEN];

    const intn_digit_t *b;
    size_t b_len;
    intn_integer_sign_t b_sign;

    size_t count
        = prepare_working_buf(m, m_len, m_sign, n, n_len, n_sign, &b, &b_len, &b_sign, working_buf);

    signed_bitwise(b, b_len, b_sign, working_buf, count, digit_band);
    intn_integer_sign_t res_sign = sign_bitwise(m_sign, n_sign, digit_band);

    cond_neg(res_sign, working_buf, count, out);
    size_t res_count = count_and_normalize_sign(out, count, res_sign, out_sign);

    return res_count;
}

static inline intn_digit_t digit_bxor(intn_digit_t a, intn_digit_t b)
{
    return a ^ b;
}

size_t intn_bxor(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    const intn_digit_t n[], size_t n_len, intn_integer_sign_t n_sign, intn_digit_t out[],
    intn_integer_sign_t *out_sign)
{
    intn_digit_t working_buf[INTN_MAX_IN_LEN];

    const intn_digit_t *b;
    size_t b_len;
    intn_integer_sign_t b_sign;

    size_t count
        = prepare_working_buf(m, m_len, m_sign, n, n_len, n_sign, &b, &b_len, &b_sign, working_buf);

    signed_bitwise(b, b_len, b_sign, working_buf, count, digit_bxor);
    intn_integer_sign_t res_sign = sign_bitwise(m_sign, n_sign, digit_bxor);

    cond_neg(res_sign, working_buf, count, out);
    size_t res_count = count_and_normalize_sign(out, count, res_sign, out_sign);

    return res_count;
}

size_t intn_bnot(const intn_digit_t m[], size_t m_len, intn_integer_sign_t m_sign,
    intn_digit_t out[], intn_integer_sign_t *out_sign)
{
    cond_neg(m_sign, m, m_len, out);
    for (size_t i = 0; i < m_len; i++) {
        out[i] = ~out[i];
    }
    intn_integer_sign_t res_sign = intn_negate_sign(m_sign);

    if (res_sign == IntNNegativeInteger) {
        neg_and_count_in_place(out, m_len);
    }
    size_t res_count = count_and_normalize_sign(out, m_len, res_sign, out_sign);

    return res_count;
}

size_t intn_bsl(const intn_digit_t num[], size_t len, size_t n, intn_digit_t *out)
{
    size_t digit_left_bit_shift = n % 32;
    size_t right_shift_n = (32 - digit_left_bit_shift);

    size_t counted_digits = intn_count_digits(num, len);
    size_t ms_digit_bits = 32 - uint32_nlz(num[counted_digits - 1]);
    size_t effective_bits_len = (counted_digits - 1) * INTN_DIGIT_BITS + ms_digit_bits;
    size_t new_bits_len = size_align_up_pow2(effective_bits_len + n, INTN_DIGIT_BITS);

    size_t new_digits_count = new_bits_len / INTN_DIGIT_BITS;

    if (new_digits_count > INTN_BSL_MAX_RES_LEN) {
        return new_digits_count;
    }

    size_t initial_zeros = MIN(n / INTN_DIGIT_BITS, INTN_BSL_MAX_RES_LEN);
    memset(out, 0, initial_zeros * sizeof(uint32_t));

    if (right_shift_n == 32) {
        memcpy(out + initial_zeros, num, len * sizeof(uint32_t));
        return initial_zeros + len;
    }

    uint32_t last_digit = 0;
    size_t i;
    for (i = 0; i < counted_digits; i++) {
        uint32_t digit = num[i];
        out[initial_zeros + i] = (digit << digit_left_bit_shift) | (last_digit >> right_shift_n);
        last_digit = digit;
    }
    uint32_t maybe_last_out = (last_digit >> right_shift_n);

    assert(initial_zeros + i <= new_digits_count);

    if (maybe_last_out) {
        out[initial_zeros + i] = maybe_last_out;
        return initial_zeros + i + 1;
    }

    return initial_zeros + i;
}

void bsru(
    const uint32_t num[], size_t effective_bits_len, size_t n, uint32_t last_digit, uint32_t *out)
{
    size_t digit_right_bit_shift = n % INTN_DIGIT_BITS;
    size_t left_shift_n = (INTN_DIGIT_BITS - digit_right_bit_shift);

    size_t len_in_digits
        = size_align_up_pow2(effective_bits_len, INTN_DIGIT_BITS) / INTN_DIGIT_BITS;

    // caller makes sure that discarded < len_in_digits
    size_t discarded = n / INTN_DIGIT_BITS;

    if (left_shift_n == 32) {
        memcpy(out, num + discarded, (len_in_digits - discarded) * sizeof(uint32_t));
        return;
    }

    size_t i;
    for (i = discarded; i < len_in_digits - 1; i++) {
        uint32_t next_digit = num[i + 1];
        uint32_t digit = num[i];
        out[i - discarded] = (digit >> digit_right_bit_shift) | (next_digit << left_shift_n);
    }
    uint32_t maybe_last_out = (num[i] >> digit_right_bit_shift) | (last_digit << left_shift_n);

    if (maybe_last_out) {
        out[i - discarded] = maybe_last_out;
    }
}

size_t intn_bsr(
    const intn_digit_t num[], size_t len, intn_integer_sign_t num_sign, size_t n, intn_digit_t *out)
{
    size_t counted_digits = intn_count_digits(num, len);
    size_t ms_digit_bits = 32 - uint32_nlz(num[counted_digits - 1]);
    size_t effective_bits_len = (counted_digits - 1) * INTN_DIGIT_BITS + ms_digit_bits;

    if (n >= effective_bits_len) {
        out[0] = (num_sign == IntNPositiveInteger) ? 0 : 1;
        return 1;
    }

    size_t shifted_len
        = size_align_up_pow2(effective_bits_len - n, INTN_DIGIT_BITS) / INTN_DIGIT_BITS;

    if (num_sign == IntNPositiveInteger) {
        bsru(num, effective_bits_len, n, 0, out);

    } else {
        uint32_t tmp_buf[INTN_MAX_RES_LEN];
        neg(num, counted_digits, tmp_buf);
        bsru(tmp_buf, effective_bits_len, n, (uint32_t) -1, out);
        neg_and_count_in_place(out, shifted_len);
    }

    return shifted_len;
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

double intn_to_double(const intn_digit_t *num, size_t num_len, intn_integer_sign_t sign)
{
    size_t len = intn_count_digits(num, num_len);

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

    uint32_t tmp_buf1[INTN_DIVMNU_MAX_IN_LEN];
    uint32_t tmp_buf2[INTN_DIVMNU_MAX_IN_LEN];

    // First base is 2, last is 36
    // Used code:
    // Enum.map(2..36, fn(x) ->
    //   ((Integer.pow(2, 256) - 1) |> Integer.to_string(x) |> String.length()) - 1
    // end)
    // I did - 1 so they can fit an uint8_t, otherwise max for base 2 is 256
    static const uint8_t base_max_lens[] = { 255, 161, 127, 110, 99, 91, 85, 80, 77, 74, 71, 69,
        67, 65, 63, 62, 61, 60, 59, 58, 57, 56, 55, 55, 54, 53, 53, 52, 52, 51, 51, 50, 50, 49, 49 };
    _Static_assert(INTN_MAX_UNSIGNED_BITS_SIZE == 256, "Assuming INTN_MAX_UNSIGNED_BITS_SIZE is 256");

    size_t outbuf_size = base_max_lens[base - 2] + 1 /* see above */ + 1 /* sign */ + 1 /* \0 */;
    char *outbuf = malloc(outbuf_size);
    if (IS_NULL_PTR(outbuf)) {
        return NULL;
    }
    char *end = outbuf + (outbuf_size - 1);
    *end = '\0';

    uint16_t *u;
    size_t m;

    bool negative_integer = num_sign == IntNNegativeInteger;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(tmp_buf1, num, len * sizeof(uint32_t));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    big_endian_digits_to_uint16(num, len, (uint16_t *) tmp_buf1);
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

    size_t str_size = outbuf_size - (end - outbuf);
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
    // maximum number of digits for every chunk that is parsed using int64_parse_ascii_buf
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
            intn_mulu(out, out_len, mult, 2, new_out);
            new_out_len = MAX(2, intn_count_digits(new_out, INTN_MUL_OUT_LEN(out_len, 2)));
            if (UNLIKELY(out_len > INTN_MAX_IN_LEN)) {
                assert(out_len <= INTN_MAX_RES_LEN);
                // we are above the allowed 256 bits, so it is going to be overflow
                // if still have some room in our buffer, so we are safe
                return -1;
            }
        }

        intn_integer_sign_t ignored_sign;
        intn_digit_t parsed_as_intn[2];
        intn_from_int64(parsed_chunk, parsed_as_intn, &ignored_sign);

        out_len = intn_addu(new_out, new_out_len, parsed_as_intn, 2, out);
        if (UNLIKELY(out_len > INTN_MAX_IN_LEN)) {
            assert(out_len <= INTN_MAX_RES_LEN);
            // we are above the allowed 256 bits, so it is going to be overflow
            // if still have some room in our buffer, so we are safe
            return -1;
        }

        pos += parsed_digits;
        buf_to_int64_opts = BufToInt64RejectSign;
        max_digits = base_max_digits[base - 2];
    } while (pos < buf_len);

    // let's count at the end
    return out_len;
}

static size_t neg_and_count_in_place(intn_digit_t out[], size_t len)
{
    uint32_t carry = 1;
    size_t i;
    int last_non_zero = -1;
    for (i = 0; i < len; i++) {
        uint64_t temp = (uint64_t) (~out[i]) + (uint64_t) carry;
        if ((uint32_t) temp != 0) {
            last_non_zero = i;
        }
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
    // carry is non zero here only when input is only made of 0s

    return last_non_zero + 1;
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

    if (sign == IntNNegativeInteger) {
        return neg_and_count_in_place(out, INTN_MAX_RES_LEN - 1);
    } else {
        return intn_count_digits(out, INTN_MAX_IN_LEN);
    }
}

int intn_to_integer_bytes(const intn_digit_t in[], size_t in_len, intn_integer_sign_t in_sign,
    intn_from_integer_options_t opts, uint8_t out[], size_t out_len)
{
    size_t count = intn_count_digits(in, in_len);
    if (UNLIKELY(count == 0)) {
        memset(out, 0, out_len);
        return out_len;
    }

    size_t to_copy = (count - 1);
    size_t to_copy_bytes = to_copy * sizeof(intn_digit_t);

    if (UNLIKELY(to_copy_bytes > out_len)) {
        return -1;
    }

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(out, in, to_copy_bytes);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    for (size_t i = 0; i < to_copy; i++) {
        out[i * 4] = in[i] & 0xFF;
        out[i * 4 + 1] = (in[i] >> 8) & 0xFF;
        out[i * 4 + 2] = (in[i] >> 16) & 0xFF;
        out[i * 4 + 3] = (in[i] >> 24) & 0xFF;
    }
#else
#error "Unsupported endianess"
#endif

    intn_digit_t last_in = in[to_copy];
    size_t k;
    for (k = to_copy * 4; k < (to_copy + 1) * 4; k++) {
        if (last_in == 0) {
            break;
        }
        if (UNLIKELY(k >= out_len)) {
            return -1;
        }
        out[k] = last_in & 0xFF;
        last_in >>= 8;
    }
    size_t copied_len = k;

    bool negate = false;
    if ((opts & IntnSigned) && (in_sign == IntNNegativeInteger)) {
        negate = true;
    }

    uint8_t filler = 0x00;
    if (negate) {
        filler = 0xFF;
        unsigned int carry = 1;
        for (size_t i = 0; i < copied_len; i++) {
            unsigned int temp = ((int) (~out[i])) + carry;
            out[i] = temp & 0xFF;
            carry = temp >> 8;
        }
    }

    if ((opts & IntnSigned) && (copied_len == out_len)) {
        uint8_t last_byte = out[copied_len - 1];
        if (UNLIKELY(
                (negate && ((last_byte & 0x80) == 0)) || (!negate && ((last_byte & 0x80) != 0)))) {
            return -1;
        }
    }

    memset(out + copied_len, filler, out_len - copied_len);

    // rotate when big endian
    if (!(opts & IntnLittleEndian)) {
        for (size_t i = 0; i < out_len / 2; i++) {
            uint8_t tmp = out[i];
            out[i] = out[out_len - 1 - i];
            out[out_len - 1 - i] = tmp;
        }
    }

    return out_len;
}

size_t intn_required_unsigned_integer_bytes(const intn_digit_t in[], size_t in_len)
{
    int i;
    for (i = in_len - 1; i >= 0; i--) {
        uint32_t in_i = in[i];
        if (in_i != 0) {
            return (i + 1) * sizeof(uint32_t) - (uint32_nlz(in_i) / 8);
        }
    }

    return 0;
}
