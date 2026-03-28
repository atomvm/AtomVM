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
 * Compile-time strategy selection, in priority order:
 *
 *   1. snprintf_l() / strtod_l() — BSD/macOS extension.
 *      Pass a C locale_t directly; no locale switching.
 *
 *   2. uselocale()               — POSIX.1-2008 thread-local locale.
 *      Temporarily switch this thread to the C locale,
 *      call plain snprintf/strtod, switch back.
 *
 *   3. Post-processing           — Pure C99 fallback.
 *      For output: call plain snprintf, replace locale's decimal
 *      separator with '.'.
 *      For input: call plain strtod, on locale mismatch replace '.'
 *      with locale's separator and retry.
 */

/* Feature test macros must come before any includes.
 * glibc needs _GNU_SOURCE for uselocale/newlocale. */
#if defined(__GLIBC__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include "unlocalized.h"

#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

#define PARSE_FLOAT_BUF_SIZE 256

/* ------------------------------------------------------------------ */
/*  Strategy selection                                                */
/* ------------------------------------------------------------------ */

#if !defined(UNLOCALIZED_STRATEGY)
#if defined(HAS_SNPRINTF_L)
#define UNLOCALIZED_STRATEGY 1
#elif defined(__APPLE__)
#define UNLOCALIZED_STRATEGY 1
#elif defined(__FreeBSD__) || defined(__DragonFly__)
#define UNLOCALIZED_STRATEGY 1
#endif
#endif

#if !defined(UNLOCALIZED_STRATEGY)
#if defined(HAS_USELOCALE)
#define UNLOCALIZED_STRATEGY 2
#elif defined(_POSIX_VERSION) && _POSIX_VERSION >= 200809L
#define UNLOCALIZED_STRATEGY 2
#elif defined(__GLIBC__)
#define UNLOCALIZED_STRATEGY 2
#endif
#endif

#if !defined(UNLOCALIZED_STRATEGY)
#define UNLOCALIZED_STRATEGY 3
#endif

#if UNLOCALIZED_STRATEGY == 1 || UNLOCALIZED_STRATEGY == 2
#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__DragonFly__)
#include <xlocale.h>
#endif
#endif

/* ------------------------------------------------------------------ */
/*  Cached "C" locale for strategies 1 and 2                          */
/* ------------------------------------------------------------------ */

#if UNLOCALIZED_STRATEGY == 1 || UNLOCALIZED_STRATEGY == 2

static locale_t c_locale;

#if defined(AVM_NO_SMP) || !defined(_POSIX_THREADS)

static int c_locale_init_done;

static locale_t get_c_locale(void)
{
    if (!c_locale_init_done) {
        c_locale = newlocale(LC_NUMERIC_MASK, "C", (locale_t) 0);
        if (c_locale == (locale_t) 0) {
            AVM_ABORT();
        }
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
    if (c_locale == (locale_t) 0) {
        AVM_ABORT();
    }
}

static locale_t get_c_locale(void)
{
    pthread_once(&c_locale_once, init_c_locale);
    return c_locale;
}

#endif

#endif /* strategy 1 or 2 */

/* ================================================================== */
/*  snprintf: locale-independent output                               */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/*  Strategy 1: snprintf_l                                            */
/* ------------------------------------------------------------------ */

#if UNLOCALIZED_STRATEGY == 1

static int vsnprintf_c(char *buf, size_t size, const char *fmt, va_list ap)
{
    return vsnprintf_l(buf, size, get_c_locale(), fmt, ap);
}

/* ------------------------------------------------------------------ */
/*  Strategy 2: uselocale                                             */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_STRATEGY == 2

static int vsnprintf_c(char *buf, size_t size, const char *fmt, va_list ap)
{
    locale_t prev = uselocale(get_c_locale());
    int ret = vsnprintf(buf, size, fmt, ap);
    uselocale(prev);

    return ret;
}

/* ------------------------------------------------------------------ */
/*  Strategy 3: post-processing (pure C99)                            */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_STRATEGY == 3

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

#endif /* UNLOCALIZED_STRATEGY snprintf */

int unlocalized_snprintf(char *buf, size_t size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    int ret = vsnprintf_c(buf, size, fmt, ap);
    va_end(ap);
    return ret;
}

/* ================================================================== */
/*  strtod/strtof: locale-independent input                           */
/* ================================================================== */

/* ------------------------------------------------------------------ */
/*  Strategy 1: strtod_l / strtof_l                                   */
/* ------------------------------------------------------------------ */

#if UNLOCALIZED_STRATEGY == 1

#ifdef UNLOCALIZED_ENABLE_DOUBLE_API
static double strtod_c(const char *buf, char **endp)
{
    return strtod_l(buf, endp, get_c_locale());
}
#endif

#ifdef UNLOCALIZED_ENABLE_FLOAT_API
static float strtof_c(const char *buf, char **endp)
{
    return strtof_l(buf, endp, get_c_locale());
}
#endif

/* ------------------------------------------------------------------ */
/*  Strategy 2: uselocale + strtod/strtof                             */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_STRATEGY == 2

#ifdef UNLOCALIZED_ENABLE_DOUBLE_API
static double strtod_c(const char *buf, char **endp)
{
    locale_t prev = uselocale(get_c_locale());
    double val = strtod(buf, endp);
    uselocale(prev);

    return val;
}
#endif

#ifdef UNLOCALIZED_ENABLE_FLOAT_API
static float strtof_c(const char *buf, char **endp)
{
    locale_t prev = uselocale(get_c_locale());
    float val = strtof(buf, endp);
    uselocale(prev);

    return val;
}
#endif

/* ------------------------------------------------------------------ */
/*  Strategy 3: dot-to-locale retry                                   */
/* ------------------------------------------------------------------ */

#elif UNLOCALIZED_STRATEGY == 3

/*
 * On pure C99 platforms without strtod_l/uselocale, we call the
 * standard parse function with '.' as-is.  If the locale uses a
 * different decimal separator the parser will stop at the dot.
 * Detect this and retry with '.' replaced by the locale separator.
 *
 * The input has been pre-validated by unlocalized_validate_bare_float_format(),
 * so we know there is exactly one '.' in the string.
 */

/*
 * Build a locale-adjusted copy of buf in retry_buf, replacing '.'
 * with the locale's decimal separator.
 *
 * The locale separator may be multi-byte (UTF-8), so replacing 1-byte
 * '.' may expand the string.  retry_buf must be at least
 * PARSE_FLOAT_BUF_SIZE + MB_LEN_MAX bytes.
 *
 * Returns the dot position in the original buffer (needed to adjust
 * endp after parsing), or (size_t)-1 if no retry is needed.
 */
static size_t prepare_locale_retry_buf(
    const char *buf, char *retry_buf, size_t retry_buf_size, size_t *out_sep_len)
{
    struct lconv *lc = localeconv();
    if (!lc || !lc->decimal_point || lc->decimal_point[0] == '\0') {
        return (size_t) -1;
    }

    const char *sep = lc->decimal_point;
    if (sep[0] == '.' && sep[1] == '\0') {
        return (size_t) -1;
    }

    size_t sep_len = strlen(sep);
    size_t buf_len = strlen(buf);

    if (buf_len + sep_len > retry_buf_size - 1) {
        return (size_t) -1;
    }

    char *dst = retry_buf;
    const char *src = buf;
    while (*src) {
        if (*src == '.') {
            memcpy(dst, sep, sep_len);
            dst += sep_len;
            src++;
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';

    *out_sep_len = sep_len;

    const char *dot = strchr(buf, '.');

    return (size_t) (dot - buf);
}

#ifdef UNLOCALIZED_ENABLE_DOUBLE_API
static double strtod_c(const char *buf, char **endp)
{
    double val = strtod(buf, endp);
    if (**endp == '\0') {
        return val;
    }

    char retry_buf[PARSE_FLOAT_BUF_SIZE + MB_LEN_MAX];
    size_t sep_len;
    size_t dot_pos = prepare_locale_retry_buf(buf, retry_buf, sizeof(retry_buf), &sep_len);
    if (dot_pos == (size_t) -1) {
        return val;
    }

    errno = 0;
    val = strtod(retry_buf, endp);

    /* Adjust endp back to the original buffer. */
    size_t consumed = (size_t) (*endp - retry_buf);
    if (consumed > dot_pos) {
        consumed -= (sep_len - 1);
    }
    *endp = (char *) (buf + consumed);

    return val;
}
#endif

#ifdef UNLOCALIZED_ENABLE_FLOAT_API
static float strtof_c(const char *buf, char **endp)
{
    float val = strtof(buf, endp);
    if (**endp == '\0') {
        return val;
    }

    char retry_buf[PARSE_FLOAT_BUF_SIZE + MB_LEN_MAX];
    size_t sep_len;
    size_t dot_pos = prepare_locale_retry_buf(buf, retry_buf, sizeof(retry_buf), &sep_len);
    if (dot_pos == (size_t) -1) {
        return val;
    }

    errno = 0;
    val = strtof(retry_buf, endp);

    /* Adjust endp back to the original buffer. */
    size_t consumed = (size_t) (*endp - retry_buf);
    if (consumed > dot_pos) {
        consumed -= (sep_len - 1);
    }
    *endp = (char *) (buf + consumed);

    return val;
}
#endif

#endif /* UNLOCALIZED_STRATEGY strtod/strtof */

/* ================================================================== */
/*  Format validation                                                 */
/* ================================================================== */

bool unlocalized_validate_bare_float_format(const char buf[], size_t len)
{
    size_t pos = 0;

    if (len == 0) {
        return false;
    }

    /* Optional sign. */
    if (buf[pos] == '+' || buf[pos] == '-') {
        pos++;
    }

    /* One or more digits before the dot. */
    if (pos >= len || buf[pos] < '0' || buf[pos] > '9') {
        return false;
    }
    while (pos < len && buf[pos] >= '0' && buf[pos] <= '9') {
        pos++;
    }

    /* Mandatory dot. */
    if (pos >= len || buf[pos] != '.') {
        return false;
    }
    pos++;

    /* One or more digits after the dot. */
    if (pos >= len || buf[pos] < '0' || buf[pos] > '9') {
        return false;
    }
    while (pos < len && buf[pos] >= '0' && buf[pos] <= '9') {
        pos++;
    }

    /* Optional exponent. */
    if (pos < len && (buf[pos] == 'e' || buf[pos] == 'E')) {
        pos++;

        /* Optional exponent sign. */
        if (pos < len && (buf[pos] == '+' || buf[pos] == '-')) {
            pos++;
        }

        /* One or more exponent digits. */
        if (pos >= len || buf[pos] < '0' || buf[pos] > '9') {
            return false;
        }
        while (pos < len && buf[pos] >= '0' && buf[pos] <= '9') {
            pos++;
        }
    }

    /* Must have consumed all characters. */
    return pos == len;
}

/* ================================================================== */
/*  Public: unlocalized_strtod / unlocalized_strtof                   */
/* ================================================================== */

#ifdef UNLOCALIZED_ENABLE_DOUBLE_API
int unlocalized_strtod(const char buf[], size_t len, double *result)
{
    if (len == 0 || len >= PARSE_FLOAT_BUF_SIZE) {
        return -1;
    }

    if (!unlocalized_validate_bare_float_format(buf, len)) {
        return -1;
    }

    char tmp[PARSE_FLOAT_BUF_SIZE];
    memcpy(tmp, buf, len);
    tmp[len] = '\0';

    errno = 0;
    char *endp;
    double val = strtod_c(tmp, &endp);

    if (endp != tmp + len) {
        return -1;
    }

    if (!isfinite(val)) {
        return -1;
    }

    *result = val;

    return 0;
}
#endif

#ifdef UNLOCALIZED_ENABLE_FLOAT_API
int unlocalized_strtof(const char buf[], size_t len, float *result)
{
    if (len == 0 || len >= PARSE_FLOAT_BUF_SIZE) {
        return -1;
    }

    if (!unlocalized_validate_bare_float_format(buf, len)) {
        return -1;
    }

    char tmp[PARSE_FLOAT_BUF_SIZE];
    memcpy(tmp, buf, len);
    tmp[len] = '\0';

    errno = 0;
    char *endp;
    float val = strtof_c(tmp, &endp);

    if (endp != tmp + len) {
        return -1;
    }

    if (!isfinite(val)) {
        return -1;
    }

    *result = val;

    return 0;
}
#endif
