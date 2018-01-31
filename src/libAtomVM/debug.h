/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

/**
 * @file debug.h
 * @brief Debug functions and macros.
 *
 * @details Miscellaneous functions and macros useful for debug.
 */

#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "context.h"

/**
 * @brief Print stack content to stderr.
 *
 * @details Print the dump of the stack of the given context to stderr.
 * @param ctx the process context.
 */
extern void debug_dump_stack(Context *ctx);

/**
 * @brief Gets a printable char for a given register type.
 *
 * @details Returns a printable char such as x or y for the given register type.
 * @param reg_type register type.
 * @return printable register type.
 */
extern char reg_type_c(int reg_type);

/**
 * @brief Prints a list of processes.
 *
 * @details Prints to stderr a list of processes.
 * @param processes the list of processes that will be printed.
 */
void debug_print_processes_list(struct ListHead *processes);

#ifdef ENABLE_STACK_TRACE
    #define DEBUG_DUMP_STACK debug_dump_stack
#else
    #define DEBUG_DUMP_STACK(...)
#endif

#endif
