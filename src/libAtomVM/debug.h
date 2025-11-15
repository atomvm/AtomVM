/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
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

/**
 * @file debug.h
 * @brief Debug functions and macros.
 *
 * @details Miscellaneous functions and macros useful for debug.
 */

#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "context.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Print a repreentation of the context to stderr.
 *
 * @details Print heap, stack, and registers of the given context to stderr.
 * @param ctx the process context.
 */
void debug_dump_context(Context *ctx);

/**
 * @brief Print heap contents to stderr.
 *
 * @details Print the dump of the heap of the given context to stderr.
 * @param ctx the process context.
 */
void debug_dump_heap(Context *ctx);

/**
 * @brief Print stack contents to stderr.
 *
 * @details Print the dump of the stack of the given context to stderr.
 * @param ctx the process context.
 */
void debug_dump_stack(Context *ctx);

/**
 * @brief Print register contents to stderr.
 *
 * @details Print the dump of the registers of the given context to stderr.
 * @param ctx the process context.
 */
void debug_dump_registers(Context *ctx);

/**
 * @brief Print a region of (term) memory to stderr.
 *
 * @details Print the dump of the memory the given context to stderr.
 * @param ctx the process context.
 * @param start the start address.
 * @param end the end address.
 * @param region the name of the region to display.
 */
void debug_dump_memory(Context *ctx, term *start, term *end, const char *region);

/**
 * @brief Gets a printable char for a given register type.
 *
 * @details Returns a printable char such as x or y for the given register type.
 * @param reg_type register type.
 * @return printable register type.
 */
char reg_type_c(int reg_type);

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

#ifdef __cplusplus
}
#endif

#endif
