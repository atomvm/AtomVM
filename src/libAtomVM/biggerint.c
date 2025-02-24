#include <alloca.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define USE_64BIT_MUL

#include "biggerint.h"

static inline void int64_to_biggerint_2(int64_t i64, uint32_t out[]);

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

size_t count16(uint16_t *num, size_t num_len)
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
        vn[i] = (v[i] << s) | (v[i - 1] >> 16 - s);
    vn[0] = v[0] << s;

    un = (uint16_t *) alloca(2 * (m + 1));
    un[m] = u[m - 1] >> 16 - s;
    for (i = m - 1; i > 0; i--)
        un[i] = (u[i] << s) | (u[i - 1] >> 16 - s);
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
            r[i] = (un[i] >> s) | (un[i + 1] << 16 - s);
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

static void biggerint_divmns(const uint32_t u[], int m, const uint32_t v[], int n, uint32_t q[])
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

    uint32_t a_i;
    uint32_t b_i;
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
        int64_t sign_extend_temp = (int32_t) b_i;
        sign_extend = (((uint64_t) sign_extend_temp) >> 32);
        longest = b;
    }

    for (; i < m; i++) {
        uint32_t longest_i = longest[i];
        uint64_t temp = (uint64_t) longest_i + (uint64_t) sign_extend + (uint64_t) carry;
        out[i] = (uint32_t) temp;
        carry = temp >> 32;
    }
    out[i] = (uint32_t) (((int32_t) 0) - ((int32_t) carry));

    return i + 1;
}

#if 1
#endif

#if 1
#endif


size_t count_u32digits(const biggerdigit_t *biggerint)
{
    size_t i = 0;
    while (biggerint[i] != 0) {
        i++;
    }
    return i;
}

static inline void int64_to_biggerint_2(int64_t i64, uint32_t out[])
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(out, &i64, sizeof(i64));
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    uint64_t le64 = __builtin_bswap64(i64);
    memcpy(out, &le64, sizeof(le64));
#else
#error "Unsupproted endiness"
#endif
}

#if 0

void biggerint_mul(const biggerdigit_t *num1, size_t num1_len, const biggerdigit_t *num2, size_t num2_len, biggerdigit_t *out)
{
    mul_var(num1, num1_len, num2, num2_len, out);
}
#endif

size_t biggerint_to_int64_when_fits(const biggerdigit_t *num, size_t num_len, int64_t *out)
{
    switch (num_len) {
        case 2:
            memcpy(out, num, sizeof(int64_t));
            return 0;
        case 1:
            int64_t tmp = (int32_t) num[0];
            memcpy(out, &tmp, sizeof(int64_t));
            return 0;
        case 0:
            *out = 0;
            break;
        default:
            // continue
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
#if 0
int main()
{
    uint32_t num1[] = { 0x10101010, 0x10, 0xCAFEBABE, 0 };
    uint32_t num2[] = { -1, -1, -1, 0 };
    uint32_t tmp[] = { 0xAAAAAAAA, 0xBBBBBBBB, 0xCCCCCCCC, 0xDDDDDDDD, 0x11223344, 0x55667788, 0x99887766, 0x11223344 };
    uint32_t out[] = { 0xAAAAAAAA, 0xBBBBBBBB, 0xCCCCCCCC, 0xDDDDDDDD, 0x11223344, 0x55667788, 0x99887766, 0x11223344 };

    // mulmns(out, num1, num2, 4, 2);
    biggerint_mulmns(num1, 2, num2, 3, tmp);
    //neg(tmp, 5, out);

    // divmnu((uint16_t *) out, NULL, (uint16_t *) num1, (uint16_t *) num2, 8, 1);

    int64_t myint = 42;
    size_t count = biggerint_to_int64_when_fits(out, 5, &myint);
    fprintf(stderr, "result: outsize: %i, count: %ul, val: %lli\n", (int) BIGGERINT_MUL_OUT_LEN(2, 3), count, myint);

    uint32_t out3[] = { 0xAAAAAAAA, 0xBBBBBBBB, 0xCCCCCCCC, 0xDDDDDDDD, 0x11223344, 0x55667788, 0x99887766, 0x11223344 };
    biggerint_divmns(num1, 3, num2, 1, out3);

    for (int i = 0; i < 8; i++) {
        fprintf(stderr, "0x%x ", out3[i]);
    }
    fprintf(stderr, "\n");

#if 0
    //uint32_t num1[] = { 0x10101010, 0x10101010, 1, 0 };
    //uint32_t num2[] = { 0x12345678, 0x01234567, 0xFFFFFFFF, 0xFFFFFFFF };
    uint32_t num1[] = {0x70000000};
    uint32_t num2[] = {0x70000000};
    uint32_t out[] = { 0xBBBBBBBB, 0xBBBBBBBB, 0xBBBBBBBB, 0xBBBBBBBB };

    add_2(num1, 1, num2, 1, out);

    fprintf(stderr, "res: %lli\n", *((int64_t *) out));

    for (int i = 0; i < 4; i++) {
        fprintf(stderr, "0x%x ", out[i]);
    }
    fprintf(stderr, "\n");


    uint32_t out2[] = { 0x00000000, 0x00000000, 0x80000000, 0xFFFFFFFF };
    int64_t out64;
    size_t len = biggerint_to_int64_when_fits(out2, 4, &out64);

    fprintf(stderr, "l: %lli, o64: %lli\n", len, out64);
#endif
}
#endif

#if 0
int main()
{
    uint32_t num1[] = { 0xCAFEBABE, 0xDEADBEEF, 0, 0 };
    uint32_t num2[] = { 0x12345678, 0x31312312, 0, 0 };
    // uint32_t num2[] = {1, 0, 0, 0};
    uint32_t mout[] = { 0xAAAAAAAA, 0xBBBBBBBB, 0xCCCCCCCC, 0xDDDDDDDD, 0, 0, 0, 0 };
    uint32_t out[] = { 0xAAAAAAAA, 0xBBBBBBBB, 0xCCCCCCCC, 0xDDDDDDDD, 0, 0, 0, 0 };

    mul_var(num1, 4, num2, 4, mout);

    divmnu((uint16_t *) out, NULL, (uint16_t *) mout, (uint16_t *) num2, 8, 4);

    for (int i = 0; i < 4; i++) {
        fprintf(stderr, "0x%x ", out[i]);
    }
    fprintf(stderr, "\n");

    __int128 a = 0x4AFEBABEDEADBEEFLL; //(__int128 *) num1;
    __int128 b = /*0x12345678ABCDEF01LL*/ 2; //(__int128 *) num2;
    __int128 out2;

    out2 = a * b;

    for (int i = 0; i < 4; i++) {
        fprintf(stderr, "0x%x ", ((uint32_t *) &out2)[i]);
    }
    fprintf(stderr, "\n");

    uint32_t out3[12];
    size_t out3_size;
    // biggerint_int64_mul(0x4AFEBABEDEADBEEFLL, /*0x12345678ABCDEF01LL*/100, out3, &out3_size);
    biggerint_int64_mul(INT64_MIN, 1, out3);

    int64_t the_num;
    out3_size = biggerint_to_int64_when_fits(out3, &the_num);
    fprintf(stderr, "the_num: %lx\n", the_num);
    for (int i = 0; i < out3_size; i++) {
        fprintf(stderr, "0x%x ", ((uint32_t *) out3)[i]);
    }
    fprintf(stderr, "\n");
}
#endif
