// SPDX-CopyrightText: Copyright (c) Microsoft Corporation.
// SPDX-CopyrightText: Copyright 2018 Ulf Adams
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception AND BSL-1.0
//
// This float32 formatter mirrors the Erlang-oriented formatting used by the
// double-precision path in to_chars.h, but operates on floating_decimal_32.

static inline int to_chars32(const floating_decimal_32 v, const bool sign, char *const result)
{
    uint32_t output = v.mantissa;
    const int32_t ryu_exponent = v.exponent;
    const uint32_t olength = decimalLength9(output);
    int32_t scientific_exponent = ryu_exponent + ((int32_t) olength) - 1;

    int32_t lower;
    int32_t upper;
    if (olength == 1) {
        lower = -4;
        upper = 2;
    } else if (scientific_exponent >= 10) {
        lower = -((int32_t) olength + 2);
        upper = 2;
    } else {
        lower = -((int32_t) olength + 2);
        upper = 1;
    }

    bool fixed = false;
    if (lower <= ryu_exponent && ryu_exponent <= upper) {
        if ((output >= (1u << 24) && ryu_exponent == 0)
            || (output > ((1u << 23) / 5u) && ryu_exponent == 1)
            || (output > ((1u << 22) / 25u) && ryu_exponent == 2)) {
            fixed = false;
        } else {
            fixed = true;
        }
    }

    if (sign) {
        result[0] = '-';
    }
    char *const out = result + sign;

    if (fixed) {
        const int32_t whole_digits = (int32_t) olength + ryu_exponent;

        uint32_t total_fixed_length;
        if (ryu_exponent >= 0) {
            total_fixed_length = (uint32_t) whole_digits + 2;
        } else if (whole_digits > 0) {
            total_fixed_length = olength + 1;
        } else {
            total_fixed_length = (uint32_t) (2 - ryu_exponent);
        }

        char *mid;
        if (ryu_exponent >= 0) {
            mid = out + olength;
        } else {
            mid = out + total_fixed_length;
        }

        uint32_t output2 = output;
        while (output2 >= 10000) {
#ifdef __clang__
            const uint32_t c = output2 - 10000 * (output2 / 10000);
#else
            const uint32_t c = output2 % 10000;
#endif
            output2 /= 10000;
            const uint32_t c0 = (c % 100) << 1;
            const uint32_t c1 = (c / 100) << 1;
            memcpy(mid -= 2, DIGIT_TABLE + c0, 2);
            memcpy(mid -= 2, DIGIT_TABLE + c1, 2);
        }
        if (output2 >= 100) {
            const uint32_t c = (output2 % 100) << 1;
            output2 /= 100;
            memcpy(mid -= 2, DIGIT_TABLE + c, 2);
        }
        if (output2 >= 10) {
            const uint32_t c = output2 << 1;
            memcpy(mid -= 2, DIGIT_TABLE + c, 2);
        } else {
            *--mid = (char) ('0' + output2);
        }

        if (ryu_exponent > 0) {
            memset(out + olength, '0', (size_t) ryu_exponent);
            out[olength + (size_t) ryu_exponent] = '.';
            out[olength + (size_t) ryu_exponent + 1] = '0';
        } else if (ryu_exponent == 0) {
            out[olength] = '.';
            out[olength + 1] = '0';
        } else if (whole_digits > 0) {
            memmove(out, out + 1, (size_t) whole_digits);
            out[whole_digits] = '.';
        } else {
            out[0] = '0';
            out[1] = '.';
            memset(out + 2, '0', (size_t) (-whole_digits));
        }

        return total_fixed_length + sign;
    }

    uint32_t scientific_exponent_length;
    if (scientific_exponent <= -100) {
        scientific_exponent_length = 5;
    } else if (scientific_exponent <= -10 || scientific_exponent >= 100) {
        scientific_exponent_length = 4;
    } else if ((scientific_exponent > -10 && scientific_exponent < 0) || scientific_exponent >= 10) {
        scientific_exponent_length = 3;
    } else {
        scientific_exponent_length = 2;
    }

    const uint32_t total_scientific_length = olength + 1 + (olength == 1) + scientific_exponent_length;

    uint32_t i = 0;
    uint32_t output2 = output;
    while (output2 >= 10000) {
#ifdef __clang__
        const uint32_t c = output2 - 10000 * (output2 / 10000);
#else
        const uint32_t c = output2 % 10000;
#endif
        output2 /= 10000;
        const uint32_t c0 = (c % 100) << 1;
        const uint32_t c1 = (c / 100) << 1;
        memcpy(out + olength - i - 1, DIGIT_TABLE + c0, 2);
        memcpy(out + olength - i - 3, DIGIT_TABLE + c1, 2);
        i += 4;
    }
    if (output2 >= 100) {
        const uint32_t c = (output2 % 100) << 1;
        output2 /= 100;
        memcpy(out + olength - i - 1, DIGIT_TABLE + c, 2);
        i += 2;
    }
    if (output2 >= 10) {
        const uint32_t c = output2 << 1;
        out[2] = DIGIT_TABLE[c + 1];
        out[0] = DIGIT_TABLE[c];
    } else {
        out[0] = (char) ('0' + output2);
    }

    uint32_t index;
    if (olength > 1) {
        out[1] = '.';
        index = olength + 1;
    } else {
        out[1] = '.';
        out[2] = '0';
        index = olength + 2;
    }

    out[index++] = 'e';
    if (scientific_exponent < 0) {
        out[index++] = '-';
        scientific_exponent = -scientific_exponent;
    }

    if (scientific_exponent >= 100) {
        const int32_t c = scientific_exponent % 10;
        memcpy(out + index, DIGIT_TABLE + 2 * (scientific_exponent / 10), 2);
        out[index + 2] = (char) ('0' + c);
        index += 3;
    } else if (scientific_exponent >= 10) {
        memcpy(out + index, DIGIT_TABLE + 2 * scientific_exponent, 2);
        index += 2;
    } else {
        out[index++] = (char) ('0' + scientific_exponent);
    }

    return total_scientific_length + sign;
}
