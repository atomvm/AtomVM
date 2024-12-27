/*
 * This file is part of AtomVM.
 *
 * Copyright 2019-2024 Davide Bettio <davide@uninstall.it>
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

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

// About X macro: https://en.wikipedia.org/wiki/X_macro

#define X(name, lenstr, str) \
    name##_INDEX,

enum
{
    PLATFORM_ATOMS_BASE_INDEX_MINUS_ONE = PLATFORM_ATOMS_BASE_INDEX - 1,

#include "platform_defaultatoms.def"

    ATOM_FIRST_AVAIL_INDEX
};

#undef X

_Static_assert((int) ATOM_FIRST_AVAIL_INDEX > (int) PLATFORM_ATOMS_BASE_INDEX,
    "default atoms and platform ones are overlapping");

#define X(name, lenstr, str) \
    name = TERM_FROM_ATOM_INDEX(name##_INDEX),

enum
{
#include "platform_defaultatoms.def"

    // dummy last item
    ATOM_FIRST_AVAIL_DUMMY = TERM_FROM_ATOM_INDEX(ATOM_FIRST_AVAIL_INDEX)
};

#undef X

#endif
