/***************************************************************************
 *   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

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
        abort();
    }
}
