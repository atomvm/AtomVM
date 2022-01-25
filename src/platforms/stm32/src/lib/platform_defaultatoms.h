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

#ifndef _PLATFORM_DEFAULTATOMS_H_
#define _PLATFORM_DEFAULTATOMS_H_

#include "defaultatoms.h"

#define SET_LEVEL_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 0)
#define INPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 1)
#define OUTPUT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 2)
#define SET_DIRECTION_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 3)
#define SET_INT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 4)
#define GPIO_INTERRUPT_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 5)
#define A_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 6)
#define B_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 7)
#define C_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 8)
#define D_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 9)
#define E_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 10)
#define F_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 11)
#define STM32_ATOM_INDEX (PLATFORM_ATOMS_BASE_INDEX + 12)

#define SET_LEVEL_ATOM term_from_atom_index(SET_LEVEL_ATOM_INDEX)
#define INPUT_ATOM term_from_atom_index(INPUT_ATOM_INDEX)
#define OUTPUT_ATOM term_from_atom_index(OUTPUT_ATOM_INDEX)
#define SET_DIRECTION_ATOM term_from_atom_index(SET_DIRECTION_ATOM_INDEX)
#define SET_INT_ATOM term_from_atom_index(SET_INT_ATOM_INDEX)
#define GPIO_INTERRUPT_ATOM term_from_atom_index(GPIO_INTERRUPT_ATOM_INDEX)
#define A_ATOM term_from_atom_index(A_ATOM_INDEX)
#define B_ATOM term_from_atom_index(B_ATOM_INDEX)
#define C_ATOM term_from_atom_index(C_ATOM_INDEX)
#define D_ATOM term_from_atom_index(D_ATOM_INDEX)
#define E_ATOM term_from_atom_index(E_ATOM_INDEX)
#define F_ATOM term_from_atom_index(F_ATOM_INDEX)
#define STM32_ATOM term_from_atom_index(STM32_ATOM_INDEX)

#endif
