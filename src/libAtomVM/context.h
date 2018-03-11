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
 * @file context.h
 * @brief Context struct and releated management functions
 *
 * @details A context represent the state of a running erlang process or port.
 */

#ifndef _CONTEXT_H_
#define _CONTEXT_H_

#include "linkedlist.h"
#include "globalcontext.h"
#include "term.h"

#define DEFAULT_STACK_SIZE 32

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

typedef void (*native_handler)(Context *ctx);

struct Context
{
    struct ListHead processes_list_head;

    struct ListHead processes_table_head;
    int32_t process_id;

    term x[16];

    term *stack;
    unsigned long stack_size;
    term *e;

    unsigned long cp;

    //needed for wait and wait_timeout
    Module *saved_module;
    const void *saved_ip;
    const void *jump_to_on_restore;

    struct ListHead *mailbox;

    GlobalContext *global;

    //Ports support
    native_handler native_handler;
};

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

/**
 * @brief Creates a new context
 *
 * @details Allocates a new Context struct and initialize it, the newly created context is also inserted into the processes table.
 * @param glb The global context of this virtual machine instance.
 * @returns created context.
 */
extern Context *context_new(GlobalContext *glb);

/**
 * @brief Destorys a context
 *
 * @details Frees context resources and memory and removes it from the processes table.
 * @param c the context that will be destroyed.
 */
extern void context_destroy(Context *c);

/**
 * @brief Starts executing a function
 *
 * @details Start executing bytecode for the specified function, this function will block until it terminates. The outcome is saved to x[0] register.
 * @param ctx the context that will be used to run the specificed functions, x registers must be set to function arguments.
 * @param function_name the function name C string.
 * @param the function arity (number of arguments that are required).
 * @returns 1 if an error occoured, otherwise 0 is always returned.
 */
extern int context_execute_loop(Context *ctx, Module *mod, uint8_t *beam_file, const char *function_name, int arity);

#endif
