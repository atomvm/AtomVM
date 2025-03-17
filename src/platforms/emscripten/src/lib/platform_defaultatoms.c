/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

static const char *const emscripten_atom = ATOM_STR("\xA", "emscripten");

void platform_defaultatoms_init(GlobalContext *glb)
{
    term atom_term = globalcontext_make_atom(glb, emscripten_atom);
    if (UNLIKELY(term_to_atom_index(atom_term) != EMSCRIPTEN_ATOM_INDEX)) {
        AVM_ABORT();
    }
}
