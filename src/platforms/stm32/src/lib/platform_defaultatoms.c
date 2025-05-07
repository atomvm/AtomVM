/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#include "platform_defaultatoms.h"

#include <stdlib.h>
#include <string.h>

void platform_defaultatoms_init(GlobalContext *glb)
{
// About X macro: https://en.wikipedia.org/wiki/X_macro
#define X(name, lenstr, str) \
    lenstr str,

    static const char *const atoms[] = {
#include "platform_defaultatoms.def"

        // dummy value
        NULL
    };
#undef X

    for (int i = 0; i < ATOM_FIRST_AVAIL_INDEX - PLATFORM_ATOMS_BASE_INDEX; i++) {
        if (UNLIKELY((size_t) atoms[i][0] != strlen(atoms[i] + 1))) {
            AVM_ABORT();
        }

        if (UNLIKELY(globalcontext_insert_atom(glb, atoms[i]) != i + PLATFORM_ATOMS_BASE_INDEX)) {
            AVM_ABORT();
        }
    }
}
