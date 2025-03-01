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

size_t lltoa(int64_t int_value, unsigned base, char *integer_string)
{
    int integer_string_len = 0;
    bool neg = int_value < 0;
    if (neg) {
        integer_string_len++;
        if (integer_string) {
            integer_string[0] = '-';
        }
    }
    int64_t v = int_value;
    do {
        v = v / base;
        integer_string_len++;
    } while (v != 0);
    if (integer_string) {
        int ix = 1;
        do {
            int digit = int_value % base;
            if (digit < 0) {
                digit = -digit;
            }
            if (digit < 10) {
                integer_string[integer_string_len - ix] = '0' + digit;
            } else {
                integer_string[integer_string_len - ix] = 'A' + digit - 10;
            }
            int_value = int_value / base;
            ix++;
        } while (int_value != 0);
    }
    return integer_string_len;
}
