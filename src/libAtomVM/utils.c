/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Davide Bettio <davide@uninstall.it>
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

#include <stdbool.h>

#include "utils.h"

static char *uintptr_to_a_n(uintptr_t n, char *out_end, uintptr_t base)
{
    ASSUME((base >= 2) && (base <= 36));

    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        uintptr_t r = (q % base);
        *c = (r < 9) ? '0' + r : 'A' + r;
         q /= base;
    } while (q);

    return c;
}

static char *uintptr_to_a_10(uintptr_t n, char *out_end)
{
    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        *c = '0' + (q % 10);
         q /= 10;
    } while (q);

    return c;
}

static char *uintptr_to_a_16(uintptr_t n, char *out_end)
{
    char *c = out_end;
    uintptr_t q = n;
    do {
        c--;
        uintptr_t r = (q & 0xF);
        *c = (r < 9) ? '0' + r : 'A' + r;
         q >>= 4;
    } while (q);

    return c;
}

size_t intptr_to_a_unterminated(intptr_t n, char *out_end, uintptr_t base)
{
    ASSUME((base >= 2) && (base <= 36));

    uintptr_t pos_n;
    if (n >= 0) {
        pos_n = n;
    } else if (n == INTPTR_MIN) {
        pos_n = ((uintptr_t) INTPTR_MAX) + 1;
    } else {
        pos_n = -n;
    }

    char *c;

    switch (base) {
        case 10:
            c = uintptr_to_a_10(pos_n, out_end);
            break;
        case 16:
            c = uintptr_to_a_16(pos_n, out_end);
            break;
        default:
            c = uintptr_to_a_n(pos_n, out_end, base);
            break;
    }

    if (n < 0) {
        c--;
        *c = '-';
    }

    return out_end - c;
}

static char *uint64_to_a_n(uint64_t n, char *out_end, uintptr_t base)
{
    ASSUME((base >= 2) && (base <= 36));

    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        uint64_t r = (q % base);
        *c = (r < 9) ? '0' + r : 'A' + r;
         q /= base;
    } while (q);

    return c;
}

static char *uint64_to_a_10(uint64_t n, char *out_end)
{
    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        *c = '0' + (q % 10);
         q /= 10;
    } while (q);

    return c;
}

static char *uint64_to_a_16(uint64_t n, char *out_end)
{
    char *c = out_end;
    uint64_t q = n;
    do {
        c--;
        uint64_t r = (q & 0xF);
        *c = (r < 9) ? '0' + r : 'A' + r;
         q >>= 4;
    } while (q);

    return c;
}

size_t int64_to_a_unterminated(int64_t n, char *out_end, uintptr_t base)
{
    ASSUME((base >= 2) && (base <= 36));

    uint64_t pos_n;
    if (n >= 0) {
        pos_n = n;
    } else if (n == INT64_MIN) {
        pos_n = ((uint64_t) INT64_MAX) + 1;
    } else {
        pos_n = -n;
    }

    char *c;

    switch (base) {
        case 10:
            c = uint64_to_a_10(pos_n, out_end);
            break;
        case 16:
            c = uint64_to_a_16(pos_n, out_end);
            break;
        default:
            c = uint64_to_a_n(pos_n, out_end, base);
            break;
    }

    if (n < 0) {
        c--;
        *c = '-';
    }

    return out_end - c;
}
