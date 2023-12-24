/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

#include "atom.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

void atom_string_to_c(AtomString atom_string, char *buf, size_t bufsize)
{
    size_t atom_len = *((const uint8_t *) atom_string);

    if (bufsize <= atom_len) {
        atom_len = bufsize - 1;
    }
    memcpy(buf, ((const uint8_t *) atom_string) + 1, atom_len);
    buf[atom_len] = '\0';
}

int atom_are_equals(AtomString a, AtomString b)
{
    size_t atom_len_a = *((const uint8_t *) a);
    size_t atom_len_b = *((const uint8_t *) b);

    if (atom_len_a != atom_len_b) {
        return 0;
    }

    if (!memcmp((const uint8_t *) a + 1, (const uint8_t *) b + 1, atom_len_a)) {
        return 1;

    } else {
        return 0;
    }
}

void atom_write_mfa(char *buf, size_t buf_size, AtomString module, AtomString function, unsigned int arity)
{
    size_t module_name_len = atom_string_len(module);
    memcpy(buf, atom_string_data(module), module_name_len);

    buf[module_name_len] = ':';

    size_t function_name_len = atom_string_len(function);
    if (UNLIKELY((arity > 9) || (module_name_len + function_name_len + 4 > buf_size))) {
        fprintf(stderr, "Insufficient room to write mfa.\n");
        AVM_ABORT();
    }
    memcpy(buf + module_name_len + 1, atom_string_data(function), function_name_len);

    // TODO: handle functions with more than 9 parameters
    buf[module_name_len + function_name_len + 1] = '/';
    buf[module_name_len + function_name_len + 2] = (char) ('0' + arity);
    buf[module_name_len + function_name_len + 3] = 0;
}
