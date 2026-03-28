/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Davide Bettio <davide@uninstall.it>
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

/**
 * @file float_utils.h
 * @brief Floating-point to ASCII conversion.
 */

#ifndef _FLOAT_UTILS_H_
#define _FLOAT_UTILS_H_

#include <stddef.h>
#include <stdint.h>

#include "term_typedef.h"

#ifdef AVM_USE_SINGLE_PRECISION
#define FLOAT_UTILS_ENABLE_FLOAT_API
#else
#define FLOAT_UTILS_ENABLE_DOUBLE_API
#endif

#ifdef __cplusplus
extern "C" {
#endif

/** Maximum buffer size for double_write_to_ascii_buf / float_write_to_ascii_buf. */
#define DOUBLE_WRITE_TO_ASCII_BUF_LEN 256

/**
 * @brief Output format for double_write_to_ascii_buf / float_write_to_ascii_buf.
 */
typedef enum
{
    /** Shortest roundtrip representation (Grisu3 for doubles, best-effort for floats).
     *  Within (-2^53, 2^53): whichever notation is shorter.
     *  Outside that range: always scientific notation.
     *  Precision parameter is ignored. */
    FloatFormatShort,

    /** Scientific notation (e.g. "3.14000e+00").
     *  Precision = number of digits after the decimal point. */
    FloatFormatScientific,

    /** Fixed-point decimal (e.g. "3.14000").
     *  Precision = max digits after the decimal point. */
    FloatFormatDecimals,

    /** Fixed-point decimal with trailing zeros stripped (e.g. "3.14").
     *  Like FloatFormatDecimals but trailing zeros after the decimal
     *  point are removed, keeping at least one fractional digit
     *  (e.g. "3.0" not "3."). */
    FloatFormatDecimalsCompact
} float_format_t;

#ifdef FLOAT_UTILS_ENABLE_DOUBLE_API
/**
 * @brief Convert a finite double to an ASCII string.
 *
 * Caller must ensure isfinite(value) is true.
 * Output always uses '.' as decimal separator regardless of locale.
 *
 * buf must be at least DOUBLE_WRITE_TO_ASCII_BUF_LEN bytes.
 *
 * @param value the double to convert
 * @param format output format
 * @param precision digits after decimal point (ignored for FloatFormatShort)
 * @param buf output buffer (at least DOUBLE_WRITE_TO_ASCII_BUF_LEN bytes)
 * @return number of characters written (excluding null terminator),
 *         or -1 if the output was truncated
 */
int double_write_to_ascii_buf(double value, float_format_t format, int precision, char *buf);
#endif

#ifdef FLOAT_UTILS_ENABLE_FLOAT_API
/**
 * @brief Convert a finite float to an ASCII string.
 *
 * Same interface as double_write_to_ascii_buf but for 32-bit floats.
 * For FloatFormatShort, uses snprintf with trailing-zero removal
 * (best-effort, output may be longer than the Grisu3 double version).
 *
 * @param value the float to convert
 * @param format output format
 * @param precision digits after decimal point (ignored for FloatFormatShort)
 * @param buf output buffer (at least DOUBLE_WRITE_TO_ASCII_BUF_LEN bytes)
 * @return number of characters written (excluding null terminator),
 *         or -1 if the output was truncated
 */
int float_write_to_ascii_buf(float value, float_format_t format, int precision, char *buf);
#endif

/**
 * @brief Convert a finite avm_float_t to an ASCII string.
 *
 * Dispatches to double_write_to_ascii_buf or float_write_to_ascii_buf
 * depending on AVM_USE_SINGLE_PRECISION.
 */
static inline int avm_float_write_to_ascii_buf(
    avm_float_t value, float_format_t format, int precision, char *buf)
{
#ifdef FLOAT_UTILS_ENABLE_FLOAT_API
    return float_write_to_ascii_buf(value, format, precision, buf);
#else
    return double_write_to_ascii_buf(value, format, precision, buf);
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* _FLOAT_UTILS_H_ */
