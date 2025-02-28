
#ifndef _BIGGERINT_H_
#define _BIGGERINT_H_

#define INT64_LEN 2

#define BIGGERINT_NEG_OUT_LEN(num_len) ((num_len) + 1)
#define BIGGERINT_MUL_OUT_LEN(m, n) ((m) + (n))

typedef uint32_t biggerdigit_t;

void biggerint_mulmns(const biggerdigit_t u[], int m, const biggerdigit_t v[], int n, biggerdigit_t w[]);
void biggerint_mul_int64(int64_t num1, int64_t num2, biggerdigit_t *out);

void biggerint_sign_extend(const biggerdigit_t *num, size_t num_len, size_t extend_to, biggerdigit_t *out);

void print_num(const uint32_t num[], int len);

size_t biggerint_to_int64_when_fits(const biggerdigit_t *num, size_t num_len, int64_t *out);

char *biggerint_to_string(const biggerdigit_t *num, size_t len, int base);

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

#endif
