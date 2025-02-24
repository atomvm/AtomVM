
#ifndef _BIGGERINT_H_
#define _BIGGERINT_H_

#define INT64_LEN 2

#define BIGGERINT_NEG_OUT_LEN(num_len) ((num_len) + 1)
#define BIGGERINT_MUL_OUT_LEN(m, n) ((m) + (n))

typedef uint32_t biggerdigit_t;

void biggerint_mulmns(const biggerdigit_t u[], int m, const biggerdigit_t v[], int n, biggerdigit_t w[]);
void biggerint_mul_int64(int64_t num1, int64_t num2, biggerdigit_t *out);

void print_num(const uint32_t num[], int len);

#endif
