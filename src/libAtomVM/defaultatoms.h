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

#ifndef _DEFAULTATOMS_H_
#define _DEFAULTATOMS_H_

#include "globalcontext.h"

#define FALSE_ATOM_INDEX 0
#define TRUE_ATOM_INDEX 1

#define OK_ATOM_INDEX 2
#define ERROR_ATOM_INDEX 3

#define UNDEFINED_ATOM_INDEX 4

#define BADARG_ATOM_INDEX 5
#define BADARITH_ATOM_INDEX 6
#define BADARITY_ATOM_INDEX 7
#define BADFUN_ATOM_INDEX 8
#define FUNCTION_CLAUSE_ATOM_INDEX 9
#define OUT_OF_MEMORY_ATOM_INDEX 10
#define OVERFLOW_ATOM_INDEX 11
#define SYSTEM_LIMIT_ATOM_INDEX 12

#define FLUSH_ATOM_INDEX 13
#define HEAP_SIZE_ATOM_INDEX 14
#define LATIN1_ATOM_INDEX 15
#define MAX_HEAP_SIZE_ATOM_INDEX 16
#define MEMORY_ATOM_INDEX 17
#define MESSAGE_QUEUE_LEN_ATOM_INDEX 18
#define PUTS_ATOM_INDEX 19
#define STACK_SIZE_ATOM_INDEX 20
#define MIN_HEAP_SIZE_ATOM_INDEX 21

#define FALSE_ATOM term_from_atom_index(FALSE_ATOM_INDEX)
#define TRUE_ATOM term_from_atom_index(TRUE_ATOM_INDEX)

#define OK_ATOM term_from_atom_index(OK_ATOM_INDEX)
#define ERROR_ATOM term_from_atom_index(ERROR_ATOM_INDEX)

#define UNDEFINED_ATOM term_from_atom_index(UNDEFINED_ATOM_INDEX)

#define BADARG_ATOM  term_from_atom_index(BADARG_ATOM_INDEX)
#define BADARITH_ATOM term_from_atom_index(BADARITH_ATOM_INDEX)
#define BADARITY_ATOM term_from_atom_index(BADARITY_ATOM_INDEX)
#define BADFUN_ATOM term_from_atom_index(BADFUN_ATOM_INDEX)
#define FUNCTION_CLAUSE_ATOM term_from_atom_index(FUNCTION_CLAUSE_ATOM_INDEX)
#define OUT_OF_MEMORY_ATOM term_from_atom_index(OUT_OF_MEMORY_ATOM_INDEX)
#define OVERFLOW_ATOM term_from_atom_index(OVERFLOW_ATOM_INDEX)
#define SYSTEM_LIMIT_ATOM term_from_atom_index(SYSTEM_LIMIT_ATOM_INDEX)

#define LATIN1_ATOM term_from_atom_index(LATIN1_ATOM_INDEX)
#define FLUSH_ATOM term_from_atom_index(FLUSH_ATOM_INDEX)
#define HEAP_SIZE_ATOM term_from_atom_index(HEAP_SIZE_ATOM_INDEX)
#define MAX_HEAP_SIZE_ATOM term_from_atom_index(MAX_HEAP_SIZE_ATOM_INDEX)
#define MEMORY_ATOM term_from_atom_index(MEMORY_ATOM_INDEX)
#define MESSAGE_QUEUE_LEN_ATOM term_from_atom_index(MESSAGE_QUEUE_LEN_ATOM_INDEX)
#define PUTS_ATOM term_from_atom_index(PUTS_ATOM_INDEX)
#define STACK_SIZE_ATOM term_from_atom_index(STACK_SIZE_ATOM_INDEX)
#define MIN_HEAP_SIZE_ATOM term_from_atom_index(MIN_HEAP_SIZE_ATOM_INDEX)

void defaultatoms_init(GlobalContext *glb);

#endif
