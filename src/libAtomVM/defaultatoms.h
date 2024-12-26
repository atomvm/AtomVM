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

#ifndef _DEFAULTATOMS_H_
#define _DEFAULTATOMS_H_

#include "globalcontext.h"

#ifdef __cplusplus
extern "C" {
#endif

// About X macro: https://en.wikipedia.org/wiki/X_macro

#define X(name, lenstr, str) \
    name##_INDEX,

enum
{
#include "defaultatoms.def"

    // The first index for platform specific atoms, should always be last in the list
    PLATFORM_ATOMS_BASE_INDEX
};

#undef X

_Static_assert(FALSE_ATOM_INDEX == 0, "false atom index must be 0");
_Static_assert(TRUE_ATOM_INDEX == 1, "true atom index must be 1");

#define X(name, lenstr, str) \
    name = TERM_FROM_ATOM_INDEX(name##_INDEX),

enum
{
#include "defaultatoms.def"

    // dummy last item
    PLATFORM_ATOMS_BASE_DUMMY = TERM_FROM_ATOM_INDEX(PLATFORM_ATOMS_BASE_INDEX)
};

#undef X

_Static_assert(FALSE_ATOM == TERM_FROM_ATOM_INDEX(0), "FALSE_ATOM has an unexpected value");
_Static_assert(TRUE_ATOM == TERM_FROM_ATOM_INDEX(1), "TRUE_ATOM has an unexpected value");

void defaultatoms_init(GlobalContext *glb);

void platform_defaultatoms_init(GlobalContext *glb);

#ifdef __cplusplus
}
#endif

#endif
