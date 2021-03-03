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
#define TRY_CLAUSE_ATOM_INDEX 10
#define OUT_OF_MEMORY_ATOM_INDEX 11
#define OVERFLOW_ATOM_INDEX 12
#define SYSTEM_LIMIT_ATOM_INDEX 13

#define FLUSH_ATOM_INDEX 14
#define HEAP_SIZE_ATOM_INDEX 15
#define LATIN1_ATOM_INDEX 16
#define MAX_HEAP_SIZE_ATOM_INDEX 17
#define MEMORY_ATOM_INDEX 18
#define MESSAGE_QUEUE_LEN_ATOM_INDEX 19
#define PUTS_ATOM_INDEX 20
#define STACK_SIZE_ATOM_INDEX 21
#define MIN_HEAP_SIZE_ATOM_INDEX 22

#define PROCESS_COUNT_ATOM_INDEX 23
#define PORT_COUNT_ATOM_INDEX 24
#define ATOM_COUNT_ATOM_INDEX 25
#define SYSTEM_ARCHITECTURE_ATOM_INDEX 26
#define WORDSIZE_ATOM_INDEX 27

#define DECIMALS_ATOM_INDEX 28
#define SCIENTIFIC_ATOM_INDEX 29
#define COMPACT_ATOM_INDEX 30

#define BADMATCH_ATOM_INDEX 31
#define CASE_CLAUSE_ATOM_INDEX 32
#define IF_CLAUSE_ATOM_INDEX 33
#define THROW_ATOM_INDEX 34
#define LOW_ENTROPY_ATOM_INDEX 35
#define UNSUPPORTED_ATOM_INDEX 36
#define USED_ATOM_INDEX 37
#define ALL_ATOM_INDEX 38
#define START_ATOM_INDEX 39

#define UNDEF_ATOM_INDEX 40
#define VM_ABORT_ATOM_INDEX 41

#define LINK_ATOM_INDEX 42
#define MONITOR_ATOM_INDEX 43
#define NORMAL_ATOM_INDEX 44
#define DOWN_ATOM_INDEX 45
#define PROCESS_ATOM_INDEX 46
#define NOCATCH_ATOM_INDEX 47

#define REFC_BINARY_INFO_ATOM_INDEX 48

#define NOPROC_ATOM_INDEX 49
#define TRAP_EXIT_ATOM_INDEX 50
#define EXIT_ATOM_INDEX 51

#define PLATFORM_ATOMS_BASE_INDEX 52

#define FALSE_ATOM TERM_FROM_ATOM_INDEX(FALSE_ATOM_INDEX)
#define TRUE_ATOM TERM_FROM_ATOM_INDEX(TRUE_ATOM_INDEX)

#define OK_ATOM term_from_atom_index(OK_ATOM_INDEX)
#define ERROR_ATOM term_from_atom_index(ERROR_ATOM_INDEX)

#define UNDEFINED_ATOM term_from_atom_index(UNDEFINED_ATOM_INDEX)

#define BADARG_ATOM term_from_atom_index(BADARG_ATOM_INDEX)
#define BADARITH_ATOM term_from_atom_index(BADARITH_ATOM_INDEX)
#define BADARITY_ATOM term_from_atom_index(BADARITY_ATOM_INDEX)
#define BADFUN_ATOM term_from_atom_index(BADFUN_ATOM_INDEX)
#define FUNCTION_CLAUSE_ATOM term_from_atom_index(FUNCTION_CLAUSE_ATOM_INDEX)
#define TRY_CLAUSE_ATOM term_from_atom_index(TRY_CLAUSE_ATOM_INDEX)
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

#define PROCESS_COUNT_ATOM term_from_atom_index(PROCESS_COUNT_ATOM_INDEX)
#define PORT_COUNT_ATOM term_from_atom_index(PORT_COUNT_ATOM_INDEX)
#define ATOM_COUNT_ATOM term_from_atom_index(ATOM_COUNT_ATOM_INDEX)
#define SYSTEM_ARCHITECTURE_ATOM term_from_atom_index(SYSTEM_ARCHITECTURE_ATOM_INDEX)
#define WORDSIZE_ATOM term_from_atom_index(WORDSIZE_ATOM_INDEX)

#define DECIMALS_ATOM TERM_FROM_ATOM_INDEX(DECIMALS_ATOM_INDEX)
#define SCIENTIFIC_ATOM TERM_FROM_ATOM_INDEX(SCIENTIFIC_ATOM_INDEX)
#define DEFAULTATOMS_COMPACT_ATOM TERM_FROM_ATOM_INDEX(COMPACT_ATOM_INDEX)

#define BADMATCH_ATOM TERM_FROM_ATOM_INDEX(BADMATCH_ATOM_INDEX)
#define CASE_CLAUSE_ATOM TERM_FROM_ATOM_INDEX(CASE_CLAUSE_ATOM_INDEX)
#define IF_CLAUSE_ATOM TERM_FROM_ATOM_INDEX(IF_CLAUSE_ATOM_INDEX)
#define THROW_ATOM TERM_FROM_ATOM_INDEX(THROW_ATOM_INDEX)
#define LOW_ENTROPY_ATOM TERM_FROM_ATOM_INDEX(LOW_ENTROPY_ATOM_INDEX)
#define UNSUPPORTED_ATOM TERM_FROM_ATOM_INDEX(UNSUPPORTED_ATOM_INDEX)
#define USED_ATOM TERM_FROM_ATOM_INDEX(USED_ATOM_INDEX)
#define ALL_ATOM TERM_FROM_ATOM_INDEX(ALL_ATOM_INDEX)
#define START_ATOM TERM_FROM_ATOM_INDEX(START_ATOM_INDEX)

#define UNDEF_ATOM TERM_FROM_ATOM_INDEX(UNDEF_ATOM_INDEX)
#define VM_ABORT_ATOM TERM_FROM_ATOM_INDEX(VM_ABORT_ATOM_INDEX)

#define LINK_ATOM TERM_FROM_ATOM_INDEX(LINK_ATOM_INDEX)
#define MONITOR_ATOM TERM_FROM_ATOM_INDEX(MONITOR_ATOM_INDEX)
#define NORMAL_ATOM TERM_FROM_ATOM_INDEX(NORMAL_ATOM_INDEX)
#define DOWN_ATOM TERM_FROM_ATOM_INDEX(DOWN_ATOM_INDEX)
#define PROCESS_ATOM TERM_FROM_ATOM_INDEX(PROCESS_ATOM_INDEX)
#define NOCATCH_ATOM TERM_FROM_ATOM_INDEX(NOCATCH_ATOM_INDEX)

#define REFC_BINARY_INFO_ATOM TERM_FROM_ATOM_INDEX(REFC_BINARY_INFO_ATOM_INDEX)

#define NOPROC_ATOM TERM_FROM_ATOM_INDEX(NOPROC_ATOM_INDEX)
#define TRAP_EXIT_ATOM TERM_FROM_ATOM_INDEX(TRAP_EXIT_ATOM_INDEX)
#define EXIT_ATOM TERM_FROM_ATOM_INDEX(EXIT_ATOM_INDEX)

void defaultatoms_init(GlobalContext *glb);

void platform_defaultatoms_init(GlobalContext *glb);

#endif
