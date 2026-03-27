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

/*
 * Compile-time selection, in priority order:
 *
 *   1. snprintf_l()    — BSD/macOS extension.
 *                         Pass a C locale_t directly; no locale switching.
 *
 *   2. uselocale()     — POSIX.1-2008 thread-local locale.
 *                         Temporarily switch this thread to the C locale,
 *                         call plain snprintf, switch back.
 *
 *   3. Post-processing — Pure C99 fallback.
 *                         Call plain snprintf, then replace the locale's
 *                         decimal separator with '.'.
 */

/* Feature test macros must come before any includes.
 * glibc needs _GNU_SOURCE for uselocale/newlocale. */
#if defined(__GLIBC__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include "unlocalized_snprintf.h"

#include <locale.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#if !defined(UNLOCALIZED_SNPRINTF_STRATEGY)
#if defined(HAS_SNPRINTF_L)
#define UNLOCALIZED_SNPRINTF_STRATEGY 1
#elif defined(__APPLE__)
#define UNLOCALIZED_SNPRINTF_STRATEGY 1
#elif defined(__FreeBSD__) || defined(__DragonFly__)
#define UNLOCALIZED_SNPRINTF_STRATEGY 1
#endif
#endif

#if !defined(UNLOCALIZED_SNPRINTF_STRATEGY)
#if defined(HAS_USELOCALE)
#define UNLOCALIZED_SNPRINTF_STRATEGY 2
#elif defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L
#define UNLOCALIZED_SNPRINTF_STRATEGY 2
#elif defined(__GLIBC__)
#define UNLOCALIZED_SNPRINTF_STRATEGY 2
#endif
#endif

#if !defined(UNLOCALIZED_SNPRINTF_STRATEGY)
#define UNLOCALIZED_SNPRINTF_STRATEGY 3
#endif

#if UNLOCALIZED_SNPRINTF_STRATEGY == 1 || UNLOCALIZED_SNPRINTF_STRATEGY == 2
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__DragonFly__)
#include <xlocale.h>
#endif
#endif

/* ------------------------------------------------------------------ */
/*  Cached "C" locale for strategies 1 and 2                          */
/* ------------------------------------------------------------------ */

#if UNLOCALIZED_SNPRINTF_STRATEGY == 1 || UNLOCALIZED_SNPRINTF_STRATEGY == 2

static locale_t c_locale;

#if defined(AVM_NO_SMP) || !defined(_POSIX_THREADS)

static int c_locale_init_done;

static locale_t get_c_locale(void)
{
    if (!c_locale_init_done) {
        c_locale = newlocale(LC_NUMERIC_MASK, "C", (locale_t) 0);
        c_locale_init_done = 1;
    }
    return c_locale;
}

#else

#include <pthread.h>

static pthread_once_t c_locale_once = PTHREAD_ONCE_INIT;

static void init_c_locale(void)
{
    c_locale = newlocale(LC_NUMERIC_MASK, "C", (locale_t) 0);
}

static locale_t get_c_locale(void)
{
    pthread_once(&c_locale_once, init_c_locale);
    return c_locale;
}

#endif

#endif /* strategy 1 or 2 */

/* ------------------------------------------------------------------ */
/*  Strategy 1: snprintf_l                                            */
/* ------------------------------------------------------------------ */

#if UNLOCALIZED_SNPRINTF_STRATEGY == 1

static int vsnprintf_c(char *buf, size_t size, const char *fmt, va_list ap)
{
    locale_t cloc = get_c_locale();
    if (cloc != (locale_t) 0) {
        return vsnprintf_l(buf, size, cloc, fmt, ap);
    }
    return vsnprintf(buf, size, fmt, ap);
}

/* ------------------------------------------------------------------ */
/*  Strategy 2: uselocale                                             */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_SNPRINTF_STRATEGY == 2

static int vsnprintf_c(char *buf, size_t size, const char *fmt, va_list ap)
{
    locale_t cloc = get_c_locale();
    if (cloc != (locale_t) 0) {
        locale_t prev = uselocale(cloc);
        int ret = vsnprintf(buf, size, fmt, ap);
        uselocale(prev);
        return ret;
    }
    return vsnprintf(buf, size, fmt, ap);
}

/* ------------------------------------------------------------------ */
/*  Strategy 3: post-processing (pure C99)                            */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_SNPRINTF_STRATEGY == 3

static void fix_decimal_separator(char *buf)
{
    struct lconv *lc = localeconv();
    if (!lc || !lc->decimal_point || lc->decimal_point[0] == '\0') {
        return;
    }

    const char *sep = lc->decimal_point;
    if (sep[0] == '.' && sep[1] == '\0') {
        return;
    }

    /* Fast path: single-byte separator (covers all real-world locales). */
    if (sep[1] == '\0') {
        char old = sep[0];
        for (char *p = buf; *p; ++p) {
            if (*p == old) {
                *p = '.';
            }
        }
        return;
    }

    /* Multi-byte separator: replace in-place, string can only shrink. */
    size_t seplen = strlen(sep);
    char *dst = buf;
    char *src = buf;
    while (*src) {
        if (strncmp(src, sep, seplen) == 0) {
            *dst++ = '.';
            src += seplen;
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';
}

static int vsnprintf_c(char *buf, size_t size, const char *fmt, va_list ap)
{
    int ret = vsnprintf(buf, size, fmt, ap);
    if (ret > 0 && size > 0) {
        fix_decimal_separator(buf);
    }
    return ret;
}

#endif /* UNLOCALIZED_SNPRINTF_STRATEGY */

/* ------------------------------------------------------------------ */
/*  Public API                                                        */
/* ------------------------------------------------------------------ */

int unlocalized_snprintf(char *buf, size_t size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf_c(buf, size, fmt, ap);
    va_end(ap);
    return ret;
}
