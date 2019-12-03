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
