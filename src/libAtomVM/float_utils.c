/*
 * This file is part of AtomVM.
 *
 * Copyright 2014-2020 Jukka Jylänki
 * Copyright 2026 Davide Bettio <davide@uninstall.it>
 *
 * Original Grisu3 implementation:
 * https://github.com/juj/MathGeoLib/blob/master/src/Math/grisu3.c
 *
 * Based on the Grisu3 algorithm described in:
 * "Printing Floating-Point Numbers Quickly And Accurately with Integers"
 * by Florian Loitsch (PLDI 2010)
 * http://www.cs.tufts.edu/~nr/cs257/archive/florian-loitsch/printf.pdf
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

#include "float_utils.h"

#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "unlocalized.h"
#include "utils.h"

#ifdef FLOAT_UTILS_ENABLE_DOUBLE_API
_Static_assert(sizeof(double) == sizeof(uint64_t), "double must be 64 bits");
_Static_assert(DBL_MANT_DIG == 53, "double must be IEEE 754 binary64");
#endif

#ifdef _MSC_VER
#pragma warning(disable : 4204)
#endif

#ifdef FLOAT_UTILS_ENABLE_DOUBLE_API

/* ------------------------------------------------------------------ */
/*  Grisu3 internals                                                  */
/* ------------------------------------------------------------------ */

#define D64_SIGN UINT64_C(0x8000000000000000)
#define D64_EXP_MASK UINT64_C(0x7FF0000000000000)
#define D64_FRACT_MASK UINT64_C(0x000FFFFFFFFFFFFF)
#define D64_IMPLICIT_ONE UINT64_C(0x0010000000000000)
#define D64_EXP_POS 52
#define D64_EXP_BIAS 1075
#define DIYFP_FRACT_SIZE 64
#define D_1_LOG2_10 0.30102999566398114
#define MIN_TARGET_EXP -60
#define MASK32 UINT64_C(0xFFFFFFFF)

/* 2^53: threshold for forcing scientific notation in short format */
#define TWO_POW_53 9007199254740992.0

static inline uint64_t cast_u64(double d)
{
    uint64_t u;
    memcpy(&u, &d, sizeof(u));
    return u;
}

#define MIN(x, y) ((x) <= (y) ? (x) : (y))
#define MAX(x, y) ((x) >= (y) ? (x) : (y))

#define MIN_CACHED_EXP -348
#define CACHED_EXP_STEP 8

typedef struct diy_fp
{
    uint64_t f;
    int32_t e;
} diy_fp;

static const uint64_t pow_cache_fract[] = { UINT64_C(0xfa8fd5a0081c0288),
    UINT64_C(0xbaaee17fa23ebf76), UINT64_C(0x8b16fb203055ac76), UINT64_C(0xcf42894a5dce35ea),
    UINT64_C(0x9a6bb0aa55653b2d), UINT64_C(0xe61acf033d1a45df), UINT64_C(0xab70fe17c79ac6ca),
    UINT64_C(0xff77b1fcbebcdc4f), UINT64_C(0xbe5691ef416bd60c), UINT64_C(0x8dd01fad907ffc3c),
    UINT64_C(0xd3515c2831559a83), UINT64_C(0x9d71ac8fada6c9b5), UINT64_C(0xea9c227723ee8bcb),
    UINT64_C(0xaecc49914078536d), UINT64_C(0x823c12795db6ce57), UINT64_C(0xc21094364dfb5637),
    UINT64_C(0x9096ea6f3848984f), UINT64_C(0xd77485cb25823ac7), UINT64_C(0xa086cfcd97bf97f4),
    UINT64_C(0xef340a98172aace5), UINT64_C(0xb23867fb2a35b28e), UINT64_C(0x84c8d4dfd2c63f3b),
    UINT64_C(0xc5dd44271ad3cdba), UINT64_C(0x936b9fcebb25c996), UINT64_C(0xdbac6c247d62a584),
    UINT64_C(0xa3ab66580d5fdaf6), UINT64_C(0xf3e2f893dec3f126), UINT64_C(0xb5b5ada8aaff80b8),
    UINT64_C(0x87625f056c7c4a8b), UINT64_C(0xc9bcff6034c13053), UINT64_C(0x964e858c91ba2655),
    UINT64_C(0xdff9772470297ebd), UINT64_C(0xa6dfbd9fb8e5b88f), UINT64_C(0xf8a95fcf88747d94),
    UINT64_C(0xb94470938fa89bcf), UINT64_C(0x8a08f0f8bf0f156b), UINT64_C(0xcdb02555653131b6),
    UINT64_C(0x993fe2c6d07b7fac), UINT64_C(0xe45c10c42a2b3b06), UINT64_C(0xaa242499697392d3),
    UINT64_C(0xfd87b5f28300ca0e), UINT64_C(0xbce5086492111aeb), UINT64_C(0x8cbccc096f5088cc),
    UINT64_C(0xd1b71758e219652c), UINT64_C(0x9c40000000000000), UINT64_C(0xe8d4a51000000000),
    UINT64_C(0xad78ebc5ac620000), UINT64_C(0x813f3978f8940984), UINT64_C(0xc097ce7bc90715b3),
    UINT64_C(0x8f7e32ce7bea5c70), UINT64_C(0xd5d238a4abe98068), UINT64_C(0x9f4f2726179a2245),
    UINT64_C(0xed63a231d4c4fb27), UINT64_C(0xb0de65388cc8ada8), UINT64_C(0x83c7088e1aab65db),
    UINT64_C(0xc45d1df942711d9a), UINT64_C(0x924d692ca61be758), UINT64_C(0xda01ee641a708dea),
    UINT64_C(0xa26da3999aef774a), UINT64_C(0xf209787bb47d6b85), UINT64_C(0xb454e4a179dd1877),
    UINT64_C(0x865b86925b9bc5c2), UINT64_C(0xc83553c5c8965d3d), UINT64_C(0x952ab45cfa97a0b3),
    UINT64_C(0xde469fbd99a05fe3), UINT64_C(0xa59bc234db398c25), UINT64_C(0xf6c69a72a3989f5c),
    UINT64_C(0xb7dcbf5354e9bece), UINT64_C(0x88fcf317f22241e2), UINT64_C(0xcc20ce9bd35c78a5),
    UINT64_C(0x98165af37b2153df), UINT64_C(0xe2a0b5dc971f303a), UINT64_C(0xa8d9d1535ce3b396),
    UINT64_C(0xfb9b7cd9a4a7443c), UINT64_C(0xbb764c4ca7a44410), UINT64_C(0x8bab8eefb6409c1a),
    UINT64_C(0xd01fef10a657842c), UINT64_C(0x9b10a4e5e9913129), UINT64_C(0xe7109bfba19c0c9d),
    UINT64_C(0xac2820d9623bf429), UINT64_C(0x80444b5e7aa7cf85), UINT64_C(0xbf21e44003acdd2d),
    UINT64_C(0x8e679c2f5e44ff8f), UINT64_C(0xd433179d9c8cb841), UINT64_C(0x9e19db92b4e31ba9),
    UINT64_C(0xeb96bf6ebadf77d9), UINT64_C(0xaf87023b9bf0ee6b) };

static const int16_t pow_cache_b_exp[] = { -1220, -1193, -1166, -1140, -1113, -1087, -1060, -1034,
    -1007, -980, -954, -927, -901, -874, -847, -821, -794, -768, -741, -715, -688, -661, -635, -608,
    -582, -555, -529, -502, -475, -449, -422, -396, -369, -343, -316, -289, -263, -236, -210, -183,
    -157, -130, -103, -77, -50, -24, 3, 30, 56, 83, 109, 136, 162, 189, 216, 242, 269, 295, 322,
    348, 375, 402, 428, 455, 481, 508, 534, 561, 588, 614, 641, 667, 694, 720, 747, 774, 800, 827,
    853, 880, 907, 933, 960, 986, 1013, 1039, 1066 };

static const int16_t pow_cache_d_exp[] = { -348, -340, -332, -324, -316, -308, -300, -292, -284,
    -276, -268, -260, -252, -244, -236, -228, -220, -212, -204, -196, -188, -180, -172, -164, -156,
    -148, -140, -132, -124, -116, -108, -100, -92, -84, -76, -68, -60, -52, -44, -36, -28, -20, -12,
    -4, 4, 12, 20, 28, 36, 44, 52, 60, 68, 76, 84, 92, 100, 108, 116, 124, 132, 140, 148, 156, 164,
    172, 180, 188, 196, 204, 212, 220, 228, 236, 244, 252, 260, 268, 276, 284, 292, 300, 308, 316,
    324, 332, 340 };

static int32_t cached_pow(int32_t exp, diy_fp *p)
{
    volatile double val = (exp + DIYFP_FRACT_SIZE - 1) * D_1_LOG2_10;
    int32_t k = (int32_t) ceil(val);
    int32_t i = (k - MIN_CACHED_EXP - 1) / CACHED_EXP_STEP + 1;
    p->f = pow_cache_fract[i];
    p->e = pow_cache_b_exp[i];
    return pow_cache_d_exp[i];
}

static diy_fp minus(diy_fp x, diy_fp y)
{
    diy_fp d;
    d.f = x.f - y.f;
    d.e = x.e;
    assert(x.e == y.e && x.f >= y.f);
    return d;
}

static diy_fp multiply(diy_fp x, diy_fp y)
{
    uint64_t a, b, c, d, ac, bc, ad, bd, tmp;
    diy_fp r;
    a = x.f >> 32;
    b = x.f & MASK32;
    c = y.f >> 32;
    d = y.f & MASK32;
    ac = a * c;
    bc = b * c;
    ad = a * d;
    bd = b * d;
    tmp = (bd >> 32) + (ad & MASK32) + (bc & MASK32);
    tmp += UINT64_C(1) << 31; // round
    r.f = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32);
    r.e = x.e + y.e + 64;
    return r;
}

static diy_fp normalize_diy_fp(diy_fp n)
{
    assert(n.f != 0);
    while (!(n.f & UINT64_C(0xFFC0000000000000))) {
        n.f <<= 10;
        n.e -= 10;
    }
    while (!(n.f & D64_SIGN)) {
        n.f <<= 1;
        --n.e;
    }
    return n;
}

static diy_fp double2diy_fp(double d)
{
    diy_fp fp;
    uint64_t u64 = cast_u64(d);
    if (!(u64 & D64_EXP_MASK)) {
        fp.f = u64 & D64_FRACT_MASK;
        fp.e = 1 - D64_EXP_BIAS;
    } else {
        fp.f = (u64 & D64_FRACT_MASK) + D64_IMPLICIT_ONE;
        fp.e = (int32_t) ((u64 & D64_EXP_MASK) >> D64_EXP_POS) - D64_EXP_BIAS;
    }
    return fp;
}

// pow10_cache[i] = 10^(i-1)
static const uint32_t pow10_cache[]
    = { 0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000 };

static int32_t largest_pow10(uint32_t n, int32_t n_bits, uint32_t *power)
{
    int32_t guess = ((n_bits + 1) * 1233 >> 12) + 1 /*skip first entry*/;
    if (n < pow10_cache[guess]) {
        --guess; // We don't have any guarantees that 2^n_bits <= n.
    }
    *power = pow10_cache[guess];
    return guess;
}

static int32_t round_weed(char *buffer, int32_t len, uint64_t wp_W, uint64_t delta, uint64_t rest,
    uint64_t ten_kappa, uint64_t ulp)
{
    uint64_t wp_Wup = wp_W - ulp;
    uint64_t wp_Wdown = wp_W + ulp;
    while (rest < wp_Wup && delta - rest >= ten_kappa
        && (rest + ten_kappa < wp_Wup || wp_Wup - rest >= rest + ten_kappa - wp_Wup)) {
        --buffer[len - 1];
        rest += ten_kappa;
    }
    if (rest < wp_Wdown && delta - rest >= ten_kappa
        && (rest + ten_kappa < wp_Wdown || wp_Wdown - rest > rest + ten_kappa - wp_Wdown)) {
        return 0;
    }

    return 2 * ulp <= rest && rest <= delta - 4 * ulp;
}

static int32_t digit_gen(
    diy_fp low, diy_fp w, diy_fp high, char *buffer, int32_t *length, int32_t *kappa)
{
    uint64_t unit = 1;
    diy_fp too_low = { low.f - unit, low.e };
    diy_fp too_high = { high.f + unit, high.e };
    diy_fp unsafe_interval = minus(too_high, too_low);
    diy_fp one = { UINT64_C(1) << -w.e, w.e };
    uint32_t p1 = (uint32_t) (too_high.f >> -one.e);
    uint64_t p2 = too_high.f & (one.f - 1);
    uint32_t div;
    *kappa = largest_pow10(p1, DIYFP_FRACT_SIZE + one.e, &div);
    *length = 0;

    while (*kappa > 0) {
        uint64_t rest;
        int32_t digit = p1 / div;
        buffer[*length] = (char) ('0' + digit);
        ++*length;
        p1 %= div;
        --*kappa;
        rest = ((uint64_t) p1 << -one.e) + p2;
        if (rest < unsafe_interval.f) {
            return round_weed(buffer, *length, minus(too_high, w).f, unsafe_interval.f, rest,
                (uint64_t) div << -one.e, unit);
        }
        div /= 10;
    }

    for (;;) {
        int32_t digit;
        p2 *= 10;
        unit *= 10;
        unsafe_interval.f *= 10;
        // Integer division by one.
        digit = (int32_t) (p2 >> -one.e);
        buffer[*length] = (char) ('0' + digit);
        ++*length;
        p2 &= one.f - 1; // Modulo by one.
        --*kappa;
        if (p2 < unsafe_interval.f) {
            return round_weed(
                buffer, *length, minus(too_high, w).f * unit, unsafe_interval.f, p2, one.f, unit);
        }
    }
}

static int32_t grisu3(double v, char *buffer, int32_t *length, int32_t *d_exp)
{
    int32_t mk, kappa, success;
    diy_fp dfp = double2diy_fp(v);
    diy_fp w = normalize_diy_fp(dfp);

    // normalize boundaries
    diy_fp t = { (dfp.f << 1) + 1, dfp.e - 1 };
    diy_fp b_plus = normalize_diy_fp(t);
    diy_fp b_minus;
    diy_fp c_mk; // Cached power of ten: 10^-k
    uint64_t u64 = cast_u64(v);
    if (!(v > 0 && v <= 1.7976931348623157e308)) {
        return 0; // Grisu only handles strictly positive finite numbers.
    }
    if (!(u64 & D64_FRACT_MASK) && (u64 & D64_EXP_MASK) != 0) {
        b_minus.f = (dfp.f << 2) - 1;
        b_minus.e = dfp.e - 2;
    } // lower boundary is closer?
    else {
        b_minus.f = (dfp.f << 1) - 1;
        b_minus.e = dfp.e - 1;
    }
    b_minus.f = b_minus.f << (b_minus.e - b_plus.e);
    b_minus.e = b_plus.e;

    mk = cached_pow(MIN_TARGET_EXP - DIYFP_FRACT_SIZE - w.e, &c_mk);

    w = multiply(w, c_mk);
    b_minus = multiply(b_minus, c_mk);
    b_plus = multiply(b_plus, c_mk);

    success = digit_gen(b_minus, w, b_plus, buffer, length, &kappa);
    *d_exp = kappa - mk;
    return success;
}

/* ------------------------------------------------------------------ */
/*  Grisu3 formatting (dtoa_grisu3)                                   */
/* ------------------------------------------------------------------ */

static int32_t write_exponent(int32_t exp, char *dst)
{
    char tmp[INT32_WRITE_TO_ASCII_BUF_LEN];
    char *end = tmp + sizeof(tmp);
    size_t exp_len = int32_write_to_ascii_buf(exp, 10, end);
    memcpy(dst, end - exp_len, exp_len);
    return (int32_t) exp_len;
}

#endif /* FLOAT_UTILS_ENABLE_DOUBLE_API — Grisu3 internals */

/*
 * Post-process %g output to match Erlang/OTP short format:
 *  - Ensure decimal point with at least one fractional digit
 *  - Normalize exponent: lowercase 'e', remove '+', remove leading zeros
 */
static int fixup_g_format(char *buf, int len)
{
    /* Find 'e' or 'E' */
    char *e_pos = NULL;
    for (int i = 0; i < len; i++) {
        if (buf[i] == 'e' || buf[i] == 'E') {
            e_pos = buf + i;
            break;
        }
    }

    if (e_pos) {
        /* Normalize to lowercase */
        *e_pos = 'e';

        /* Ensure decimal point exists before 'e' */
        if (!memchr(buf, '.', e_pos - buf)) {
            int e_len = len - (e_pos - buf);
            memmove(e_pos + 2, e_pos, e_len + 1);
            e_pos[0] = '.';
            e_pos[1] = '0';
            len += 2;
            e_pos += 2;
        }

        /* Remove '+' from positive exponent: "e+10" -> "e10" */
        if (e_pos[1] == '+') {
            int tail = len - (e_pos + 2 - buf);
            memmove(e_pos + 1, e_pos + 2, tail + 1);
            len--;
        }

        /* Remove leading zeros from exponent: "e-05" -> "e-5", "e02" -> "e2" */
        char *exp_start = e_pos + 1;
        if (*exp_start == '-') {
            exp_start++;
        }
        char *first_nonzero = exp_start;
        while (*first_nonzero == '0' && first_nonzero[1] != '\0') {
            first_nonzero++;
        }
        if (first_nonzero > exp_start) {
            int shift = first_nonzero - exp_start;
            int tail = len - (first_nonzero - buf);
            memmove(exp_start, first_nonzero, tail + 1);
            len -= shift;
        }
    } else {
        /* No exponent - ensure decimal point exists */
        if (!memchr(buf, '.', len)) {
            buf[len++] = '.';
            buf[len++] = '0';
            buf[len] = '\0';
        }
    }

    return len;
}

#ifdef FLOAT_UTILS_ENABLE_DOUBLE_API
static int32_t dtoa_grisu3(double v, char *dst)
{
    int32_t d_exp, len, success, decimals, i;
    uint64_t u64 = cast_u64(v);
    char *s2 = dst;
    assert(dst);

    // Prehandle negative values.
    if ((u64 & D64_SIGN) != 0) {
        *s2++ = '-';
        v = -v;
        u64 = cast_u64(v);
    }
    // Prehandle zero.
    if (!u64) {
        *s2++ = '0';
        *s2++ = '.';
        *s2++ = '0';
        *s2 = '\0';
        return (int32_t) (s2 - dst);
    }

    success = grisu3(v, s2, &len, &d_exp);
    // If grisu3 was not able to convert the number to a string,
    // use unlocalized_snprintf (locale-independent).
    if (!success) {
        // Use %.16e (17 sig digits, scientific) for values >= 2^53,
        // matching the force_scientific rule in the main path.
        // Otherwise %.17g picks the shorter notation automatically.
        const char *fmt = (v >= TWO_POW_53) ? "%.16e" : "%.17g";
        int32_t fb_len = unlocalized_snprintf(s2, 64, fmt, v);
        if (fb_len < 0 || fb_len >= 64) {
            return -1;
        }
        fb_len = fixup_g_format(s2, fb_len);
        return fb_len + (int32_t) (s2 - dst);
    }

    /* Force scientific notation for values outside (-2^53, 2^53) */
    int32_t force_scientific = (v >= TWO_POW_53);

    if (d_exp < 0 && (len + d_exp) > 0 && !force_scientific) {
        // Decimal point falls within digits: "314159" d_exp=-5 -> "3.14159"
        decimals = -d_exp;
        for (i = 0; i < decimals; ++i) {
            s2[len - i] = s2[len - i - 1];
        }
        s2[len++ - decimals] = '.';
        d_exp = 0;
    } else if (d_exp < 0 && (len + d_exp) >= -3 && !force_scientific) {
        // Small value: "25" d_exp=-2 -> "0.25", "5" d_exp=-3 -> "0.005"
        int32_t leading_zeros = -(len + d_exp);
        // Shift digits right to make room for "0." and leading zeros
        for (i = len - 1; i >= 0; --i) {
            s2[2 + leading_zeros + i] = s2[i];
        }
        s2[0] = '0';
        s2[1] = '.';
        for (i = 0; i < leading_zeros; ++i) {
            s2[2 + i] = '0';
        }
        len = 2 + leading_zeros + len;
    } else if (d_exp < 0 || d_exp > 2 || force_scientific) {
        // Add scientific notation
        if (len > 1) {
            // Insert decimal point: "12345" -> "1.2345"
            for (i = len; i > 1; --i) {
                s2[i] = s2[i - 1];
            }
            s2[1] = '.';
            len++;
            d_exp += len - 2;
        } else {
            // Single digit: "1" -> "1.0", adjust exponent
            s2[1] = '.';
            s2[2] = '0';
            len = 3;
        }
        s2[len++] = 'e';
        len += write_exponent(d_exp, s2 + len);
    } else if (d_exp > 0) {
        // Add trailing zeroes instead of scientific notation
        while (d_exp-- > 0) {
            s2[len++] = '0';
        }
        s2[len++] = '.';
        s2[len++] = '0';
    } else {
        // d_exp == 0, len == 1: single digit like "5" -> "5.0"
        s2[len++] = '.';
        s2[len++] = '0';
    }
    s2[len] = '\0';
    return (int32_t) (s2 + len - dst);
}
#endif /* FLOAT_UTILS_ENABLE_DOUBLE_API — Grisu3 formatting */

/* ------------------------------------------------------------------ */
/*  Public API                                                        */
/* ------------------------------------------------------------------ */

static void strip_trailing_zeros(char *buf, size_t *len)
{
    char *dot = memchr(buf, '.', *len);
    if (!dot) {
        return;
    }

    /* Find 'e'/'E' if present — don't strip into the exponent. */
    char *exp_start = NULL;
    for (char *p = dot; p < buf + *len; p++) {
        if (*p == 'e' || *p == 'E') {
            exp_start = p;
            break;
        }
    }

    char *strip_end = exp_start ? exp_start : buf + *len;
    char *p = strip_end - 1;

    /* Strip trailing zeros, but keep at least one digit after '.' */
    while (p > dot + 1 && *p == '0') {
        p--;
    }
    p++;

    if (exp_start && p < exp_start) {
        size_t exp_len = (buf + *len) - exp_start;
        memmove(p, exp_start, exp_len);
        *len = (p - buf) + exp_len;
    } else if (!exp_start) {
        *len = p - buf;
    }
    buf[*len] = '\0';
}

#ifdef FLOAT_UTILS_ENABLE_DOUBLE_API
int double_write_to_ascii_buf(double value, float_format_t format, int precision, char *buf)
{
    int ret;

    switch (format) {
        case FloatFormatShort:
            return dtoa_grisu3(value, buf);

        case FloatFormatScientific:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*e", precision, value);
            break;

        case FloatFormatDecimals:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*f", precision, value);
            break;

        case FloatFormatDecimalsCompact:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*f", precision, value);
            if (ret > 0 && ret < DOUBLE_WRITE_TO_ASCII_BUF_LEN) {
                size_t slen = ret;
                strip_trailing_zeros(buf, &slen);
                ret = slen;
            }
            break;

        default:
            buf[0] = '\0';
            return -1;
    }

    if (ret < 0 || ret >= DOUBLE_WRITE_TO_ASCII_BUF_LEN) {
        return -1;
    }
    return ret;
}
#endif /* FLOAT_UTILS_ENABLE_DOUBLE_API */

/* ------------------------------------------------------------------ */
/*  32-bit float support (best-effort short via snprintf)             */
/* ------------------------------------------------------------------ */

#ifdef FLOAT_UTILS_ENABLE_FLOAT_API

#define F32_SIGN UINT32_C(0x80000000)

static inline uint32_t cast_u32(float f)
{
    uint32_t u;
    memcpy(&u, &f, sizeof(u));
    return u;
}

static int ftoa_short(float value, char *buf)
{
    char *s = buf;
    uint32_t u32 = cast_u32(value);

    /* Handle sign */
    if (u32 & F32_SIGN) {
        *s++ = '-';
        value = -value;
        u32 = cast_u32(value);
    }

    /* Handle zero */
    if (!u32) {
        *s++ = '0';
        *s++ = '.';
        *s++ = '0';
        *s = '\0';
        return s - buf;
    }

    /* Best-effort: 9 significant digits is FLT_DECIMAL_DIG */
    int slen = unlocalized_snprintf(
        s, DOUBLE_WRITE_TO_ASCII_BUF_LEN - (s - buf), "%.9g", (double) value);
    if (slen < 0 || slen >= DOUBLE_WRITE_TO_ASCII_BUF_LEN - (s - buf)) {
        return -1;
    }

    slen = fixup_g_format(s, slen);
    return (s - buf) + slen;
}

int float_write_to_ascii_buf(float value, float_format_t format, int precision, char *buf)
{
    int ret;

    switch (format) {
        case FloatFormatShort:
            return ftoa_short(value, buf);

        case FloatFormatScientific:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*e", precision, (double) value);
            break;

        case FloatFormatDecimals:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*f", precision, (double) value);
            break;

        case FloatFormatDecimalsCompact:
            ret = unlocalized_snprintf(
                buf, DOUBLE_WRITE_TO_ASCII_BUF_LEN, "%.*f", precision, (double) value);
            if (ret > 0 && ret < DOUBLE_WRITE_TO_ASCII_BUF_LEN) {
                size_t slen = ret;
                strip_trailing_zeros(buf, &slen);
                ret = slen;
            }
            break;

        default:
            buf[0] = '\0';
            return -1;
    }

    if (ret < 0 || ret >= DOUBLE_WRITE_TO_ASCII_BUF_LEN) {
        return -1;
    }
    return ret;
}

#endif /* FLOAT_UTILS_ENABLE_FLOAT_API */
