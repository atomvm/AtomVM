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
 * @file unlocalized.h
 * @brief Locale-independent functions for floating-point numbers.
 *
 * @details Provides formatting and parsing of floating-point numbers that
 * always use '.' as the decimal separator regardless of LC_NUMERIC.
 * Thread-safe when snprintf_l()/strtod_l() or uselocale() is available.
 * On pure C99 platforms, best-effort assuming no concurrent setlocale().
 */

#ifndef _UNLOCALIZED_H_
#define _UNLOCALIZED_H_

#include <stdbool.h>
#include <stddef.h>

#include "term_typedef.h"

#ifdef AVM_USE_SINGLE_PRECISION
#define UNLOCALIZED_ENABLE_FLOAT_API
#else
#define UNLOCALIZED_ENABLE_DOUBLE_API
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Locale-independent snprintf.
 *
 * Behaves like snprintf but always uses '.' as decimal separator
 * regardless of LC_NUMERIC for floating-point conversions.
 */
int unlocalized_snprintf(char *buf, size_t size, const char *fmt, ...);

/**
 * @brief Validate strict bare decimal float format.
 *
 * Checks that buf[0..len-1] matches the grammar:
 *   [+|-] DIGITS "." DIGITS [("e"|"E") [+|-] DIGITS]
 * where DIGITS is one or more '0'-'9' characters.
 *
 * Rejects hex floats, inf, nan, whitespace, commas, and any other
 * characters not in the grammar.
 *
 * @param buf  pointer to the character data (need not be null-terminated)
 * @param len  number of characters to validate
 * @return true if the format is valid, false otherwise
 */
bool unlocalized_validate_bare_float_format(const char buf[], size_t len);

#ifdef UNLOCALIZED_ENABLE_DOUBLE_API
/**
 * @brief Parse a double from a string locale-independently with strict validation.
 *
 * Validates the input against the bare float format (see
 * unlocalized_validate_bare_float_format), then parses with strtod using '.' as
 * decimal separator regardless of LC_NUMERIC. Rejects overflow (inf)
 * but allows underflow (returns 0.0).
 *
 * @param buf     pointer to the character data (need not be null-terminated)
 * @param len     number of characters to parse (max 255)
 * @param[out] result  on success, the parsed double value
 * @return 0 on success, -1 on error (caller should raise badarg)
 */
int unlocalized_strtod(const char buf[], size_t len, double *result);
#endif

#ifdef UNLOCALIZED_ENABLE_FLOAT_API
/**
 * @brief Parse a float from a string locale-independently with strict validation.
 *
 * Same as unlocalized_strtod but uses strtof for native float-precision
 * parsing. Rejects values that overflow to infinity.
 *
 * @param buf     pointer to the character data (need not be null-terminated)
 * @param len     number of characters to parse (max 255)
 * @param[out] result  on success, the parsed float value
 * @return 0 on success, -1 on error (caller should raise badarg)
 */
int unlocalized_strtof(const char buf[], size_t len, float *result);
#endif

/**
 * @brief Parse an avm_float_t from a string locale-independently.
 *
 * Dispatches to unlocalized_strtod or unlocalized_strtof
 * depending on AVM_USE_SINGLE_PRECISION.
 */
static inline int unlocalized_strto_avm_float(const char buf[], size_t len, avm_float_t *result)
{
#ifdef UNLOCALIZED_ENABLE_FLOAT_API
    return unlocalized_strtof(buf, len, result);
#else
    return unlocalized_strtod(buf, len, result);
#endif
}

#ifdef __cplusplus
}
#endif

#endif /* _UNLOCALIZED_H_ */
