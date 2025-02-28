#include <alloca.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define USE_64BIT_MUL

#include "biggerint.h"
#include "utils.h"

/*
 * Neg
 */

static void neg(const uint32_t num[], int num_len, uint32_t out[], int *out_len)
{
    int i;
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

static void neg_inplace(uint32_t num[], int num_len, int *out_len)
{
    int i;
    uint32_t carry = 1;
    for (i = 0; i < num_len; i++) {
        uint64_t temp = (uint64_t) (~num[i]) + (uint64_t) carry;
        num[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
    if ((carry != 0) && !(num[i - 1] >> 31)) {
        num[i] = 0xFFFFFFFF;
        i++;
    }
    *out_len = i;
}

static bool is_negative(const uint32_t num[], int num_len)
{
    return (num[num_len - 1] >> 31) != 0;
}

static void myabs(const uint32_t num[], int num_len, uint32_t out[], int *out_len)
{
    if (is_negative(num, num_len)) {
        neg(num, num_len, out, out_len);
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
static void mulmns32(const uint32_t u[], int m, const uint32_t v[], int n, uint32_t w[])
{
    uint64_t k, t, b;
    int i, j;

    for (i = 0; i < m; i++)
        w[i] = 0;

    for (j = 0; j < n; j++) {
        k = 0;
        for (i = 0; i < m; i++) {
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
        for (j = 0; j < n; j++) {
            uint64_t w_j_m = w[j + m];
            uint64_t v_j = v[j];
            t = w_j_m - v_j - b;
            w[j + m] = t;
            b = t >> 63;
        }
    }
    if ((int32_t) v[n - 1] < 0) {
        b = 0;
        for (i = 0; i < m; i++) {
            uint64_t w_i_n = w[i + n];
            uint64_t u_i = u[i];
            t = w_i_n - u_i - b;
            w[i + n] = t;
            b = t >> 63;
        }
    }
}

void biggerint_mulmns(const uint32_t u[], int m, const uint32_t v[], int n, uint32_t w[])
{
    mulmns32(u, m, v, n, w);
}

#else

// Code based on Hacker's Delight book
// Original code with mostly no changes, except for parameters order
static void mulmns16(const uint16_t u[], int m, const uint16_t v[], int n, uint16_t w[])
{
    unsigned int k, t, b;
    int i, j;

    for (i = 0; i < m; i++)
        w[i] = 0;

    for (j = 0; j < n; j++) {
        k = 0;
        for (i = 0; i < m; i++) {
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
        for (j = 0; j < n; j++) {
            t = w[j + m] - v[j] - b;
            w[j + m] = t;
            b = t >> 31;
        }
    }
    if ((int16_t) v[n - 1] < 0) {
        b = 0;
        for (i = 0; i < m; i++) {
            t = w[i + n] - u[i] - b;
            w[i + n] = t;
            b = t >> 31;
        }
    }
    return;
}

void biggerint_mulmns(const uint32_t u[], int m, const uint32_t v[], int n, uint32_t w[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    mulmns16((const uint16_t *) u, m * 2, (const uint16_t *) v, n * 2, (uint16_t *) w);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "Big endian not yet supported"
#else
#error "Unsupproted endiness"
#endif
}

#endif

void biggerint_mul_int64(int64_t num1, int64_t num2, biggerdigit_t *out)
{
    biggerdigit_t u[2];
    int64_to_biggerint_2(num1, u);
    biggerdigit_t v[2];
    int64_to_biggerint_2(num2, v);

    biggerint_mulmns(u, 2, v, 2, (uint32_t *) out);
}

/*
 * Division
 */

size_t count16(const uint16_t *num, size_t num_len)
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

int nlz(unsigned x)
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

int divmnu16(uint16_t q[], uint16_t r[],
    const uint16_t u[], const uint16_t v[],
    int m, int n)
{

    const unsigned b = 65536; // Number base (16 bits).
    uint16_t *un, *vn; // Normalized form of u, v.
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
    vn = (uint16_t *) alloca(2 * n);
    for (i = n - 1; i > 0; i--)
        vn[i] = (v[i] << s) | (v[i - 1] >> (16 - s));
    vn[0] = v[0] << s;

    un = (uint16_t *) alloca(2 * (m + 1));
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

void biggerint_divmns(const uint32_t u[], int m, const uint32_t v[], int n, uint32_t q[])
{
    uint32_t u_abs[9];
    int m_abs;
    bool u_neg = is_negative(u, m);
    myabs(u, m, u_abs, &m_abs);

    uint32_t v_abs[9];
    int n_abs;
    bool v_neg = is_negative(v, n);
    myabs(v, n, v_abs, &n_abs);

    print_num(u_abs, m_abs);
    print_num(v_abs, n_abs);

    int m_abs16 = count16((const uint16_t *) u_abs, m_abs * 2);
    int n_abs16 = count16((const uint16_t *) v_abs, n_abs * 2);

    fprintf(stderr, "%i %i\n", m_abs16, n_abs16);

    if (divmnu16((uint16_t *) q, NULL, (uint16_t *) u_abs, (uint16_t *) v_abs, m_abs16, n_abs16) != 0) {
        abort();
    }
    if (u_neg != v_neg) {
        fprintf(stderr, "negate!!!!\n");
        neg_inplace(q, 8, &m_abs);
    }
}

#define MIN(a, b) (a < b) ? a : b;
#define MAX(a, b) (a > b) ? a : b;

size_t add_2(uint32_t a[], size_t a_len, uint32_t b[], size_t b_len, uint32_t out[])
{
    size_t n = MIN(a_len, b_len);
    size_t m = MAX(a_len, b_len);

/*
   This helps the compiler understanding that a_i and b_i are always initialized
   however without assume available they are manually intialized.
*/
#ifdef HAVE_ASSUME
    ASSUME(n >= 1);
#endif

    uint32_t a_i;
    uint32_t b_i;
#ifndef HAVE_ASSUME
    a_i = 0;
    b_i = 0;
#endif
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
    uint32_t *longest;
    if (a_len > b_len) {
        int64_t sign_extend_temp = (int32_t) b_i;
        sign_extend = (((uint64_t) sign_extend_temp) >> 32);
        longest = a;
    } else if (b_len > a_len) {
        int64_t sign_extend_temp = (int32_t) a_i;
        sign_extend = (((uint64_t) sign_extend_temp) >> 32);
        longest = b;
    } else {
// avoid warning due to unitialized variables
#ifdef HAVE_ASSUME
        ASSUME(i == m);
#else
        goto last_step;
#endif
    }

    for (; i < m; i++) {
        uint32_t longest_i = longest[i];
        uint64_t temp = (uint64_t) longest_i + (uint64_t) sign_extend + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }

#ifndef HAVE_ASSUME
last_step:
#endif
    out[i] = (uint32_t) (((int32_t) 0) - ((int32_t) carry));

    return i + 1;
}

size_t biggerint_to_int64_when_fits(const biggerdigit_t *num, size_t num_len, int64_t *out)
{
    switch (num_len) {
        case 2: {
            memcpy(out, num, sizeof(int64_t));
            return 0;
        }
        case 1: {
            int64_t tmp = (int32_t) num[0];
            memcpy(out, &tmp, sizeof(int64_t));
            return 0;
        }
        case 0: {
            *out = 0;
            break;
        }
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
    } else {
        for (int i = num_len - 1; i >= 0; i--) {
            uint32_t num_i = num[i];
            if (num_i != 0) {
                count = i + 1;
                break;
            }
        }
    }

    if (count <= 2) {
        memcpy(out, num, sizeof(int64_t));
        return 0;
    }

    return count;
}

void biggerint_sign_extend(const biggerdigit_t *num, size_t num_len, size_t extend_to, biggerdigit_t *out)
{
    int sign = (num[num_len - 1] == ((uint32_t) -1)) ? 0xFF : 0x00;

    memcpy(out, num, num_len * sizeof(uint32_t));
    memset(out + num_len, sign, (extend_to - num_len) * sizeof(uint32_t));
}

char *biggerint_to_string(const biggerdigit_t *num, size_t len, int base)
{
    // First base is 2, last is 36
    // This is the maximum divisor that can fit a signed int16
    static const uint16_t bases[] = { 16384, 19683, 16384, 15625, 7776, 16807, 4096, 6561, 10000,
        14641, 20736, 28561, 2744, 3375, 4096, 4913, 5832, 6859, 8000, 9261, 10648, 12167, 13824,
        15625, 17576, 19683, 21952, 24389, 27000, 29791, 1024, 1089, 1156, 1225, 1296 };

    /*
        TODO: do not use division for powers of 2, use this table that marks them with 0
        static const uin16_t bases[] = { 0, 19683, 0, 15625, 7776, 16807, 0, 6561, 10000, 14641, 20736,
            28561, 2744, 3375, 0, 4913, 5832, 6859, 8000, 9261, 10648, 12167, 13824, 15625, 17576,
            19683, 21952, 24389, 27000, 29791, 0, 1089, 1156, 1225, 1296
        };
    */

    static const uint8_t pad[] = { 14, 9, 7, 6, 5, 5, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2 };

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
    end[0] = '\0';

    uint16_t *u;
    size_t m;

    bool negative_integer = is_negative(num, len);

    if (negative_integer) {
        int m_abs;
        myabs(num, len, tmp_buf1, &m_abs);
        m = m_abs;
    } else {
        memcpy(tmp_buf1, num, len * sizeof(uint32_t));
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
        int intlen = lltoa(r, base, NULL);
        end -= intlen;
        lltoa(r, base, end);

        m16 = count16(q, m16);

        // add padding: lltoa(7, 10, ptr) -> 7, but when dealing with base 10 we need 0007
        // in order to handle numbers such as 110007 (note: 110007  / 10000 -> 11.0007, those
        // digits cannot be discarded)
        if (m16) {
            int padsize = pad[base] - intlen;
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

    int str_size = 256 - (end - outbuf);
    memmove(outbuf, end, str_size);

    return realloc(outbuf, str_size);
}
