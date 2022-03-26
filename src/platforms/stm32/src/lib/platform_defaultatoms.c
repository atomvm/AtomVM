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

static const char *const set_level_atom = "\x9" "set_level";
static const char *const input_atom = "\x5" "input";
static const char *const output_atom = "\x6" "output";
static const char *const set_direction_atom ="\xD" "set_direction";
static const char *const set_int_atom = "\x7" "set_int";
static const char *const gpio_interrupt_atom = "\xE" "gpio_interrupt";
static const char *const a_atom = "\x01" "a";
static const char *const b_atom = "\x01" "b";
static const char *const c_atom = "\x01" "c";
static const char *const d_atom = "\x01" "d";
static const char *const e_atom = "\x01" "e";
static const char *const f_atom = "\x01" "f";
static const char *const stm32_atom = "\x5" "stm32";

void platform_defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, set_level_atom) == SET_LEVEL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, input_atom) == INPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, output_atom) == OUTPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_direction_atom) == SET_DIRECTION_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, set_int_atom) == SET_INT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, gpio_interrupt_atom) == GPIO_INTERRUPT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, a_atom) == A_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, b_atom) == B_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, c_atom) == C_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, d_atom) == D_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, e_atom) == E_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, f_atom) == F_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, stm32_atom) == STM32_ATOM_INDEX;

    if (!ok) {
        AVM_ABORT();
    }
}
