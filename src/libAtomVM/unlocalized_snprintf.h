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
 * @file unlocalized_snprintf.h
 * @brief Locale-independent snprintf for floating-point numbers.
 *
 * @details Guarantees '.' as the decimal separator regardless of LC_NUMERIC,
 * for floating-point format specifiers only (%e, %f, %g and variants).
 * Thread-safe when snprintf_l() or uselocale() is available.
 * On pure C99 platforms, best-effort assuming no concurrent setlocale().
 */

#ifndef _UNLOCALIZED_SNPRINTF_H_
#define _UNLOCALIZED_SNPRINTF_H_

#include <stddef.h>

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

#ifdef __cplusplus
}
#endif

#endif /* _UNLOCALIZED_SNPRINTF_H_ */
