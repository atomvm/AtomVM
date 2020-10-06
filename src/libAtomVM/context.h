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

#include "globalcontext.h"
#include "linkedlist.h"
#include "term.h"
#include "timer_wheel.h"

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

typedef void (*native_handler)(Context *ctx);

enum ContextFlags
{
    NoFlags = 0,
    WaitingMessages = 1,
    WaitingTimeout = 2,
    WaitingTimeoutExpired = 4
};

struct Context
{
    struct ListHead processes_list_head;

    struct ListHead processes_table_head;
    int32_t process_id;

    struct TimerWheelItem timer_wheel_head;

    struct ListHead monitors_head;

    term x[16];
    int avail_registers;

    term *heap_start;
    term *stack_base;
    term *heap_ptr;
    term *e;

    avm_int_t min_heap_size;
    avm_int_t max_heap_size;

    unsigned long cp;

    //needed for wait and wait_timeout
    Module *saved_module;
    const void *saved_ip;
    const void *jump_to_on_restore;

    struct ListHead mailbox;
    struct ListHead save_queue;

    struct ListHead dictionary;

    GlobalContext *global;

    //Ports support
    native_handler native_handler;

    uint64_t reductions;

    unsigned int leader : 1;
    unsigned int has_min_heap_size : 1;
    unsigned int has_max_heap_size : 1;

    #ifdef ENABLE_ADVANCED_TRACE
        unsigned int trace_calls : 1;
        unsigned int trace_call_args : 1;
        unsigned int trace_returns : 1;
        unsigned int trace_send : 1;
        unsigned int trace_receive : 1;
    #endif


    struct ListHead heap_fragments;
    int heap_fragments_size;

    enum ContextFlags flags;

    void *platform_data;

    term bs;
    size_t bs_offset;

    term exit_reason;
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
Context *context_new(GlobalContext *glb);

/**
 * @brief Destorys a context
 *
 * @details Frees context resources and memory and removes it from the processes table.
 * @param c the context that will be destroyed.
 */
void context_destroy(Context *c);

/**
 * @brief Starts executing a function
 *
 * @details Start executing bytecode for the specified function, this function will block until it terminates. The outcome is saved to x[0] register.
 * @param ctx the context that will be used to run the specificed functions, x registers must be set to function arguments.
 * @param function_name the function name C string.
 * @param the function arity (number of arguments that are required).
 * @returns 1 if an error occoured, otherwise 0 is always returned.
 */
int context_execute_loop(Context *ctx, Module *mod, const char *function_name, int arity);

/**
 * @brief Retuns 1 if the context is a port driver
 *
 * @details Checks if the given context has a native_handler or not.
 * @param ctx a valid context
 * @returns 1 if ctx is a port driver, otherwise 0 is returned.
 */
static inline int context_is_port_driver(const Context *ctx)
{
    return ctx->native_handler != NULL;
}

/**
 * @brief Cleans up unused registers
 *
 * @details Sets to NIL unused registers, x[0] - x[live - 1] will not be overwritten.
 * @param ctx a valid context
 * @param live number of used registers
 */
static inline void context_clean_registers(Context *ctx, int live)
{
    for (int i = live; i < ctx->avail_registers; i++) {
        ctx->x[i] = term_nil();
    }
}

/**
 * @brief Returns available free memory in term units
 *
 * @details Returns the number of terms that can fit either on the stack or on the heap.
 * @param ctx a valid context.
 * @returns available free memory that is avail_size_in_bytes / sizeof(term).
 */
static inline unsigned long context_avail_free_memory(const Context *ctx)
{
    return ctx->e - ctx->heap_ptr;
}

/**
 * @brief Returns context total memory in term units
 *
 * @details Returns the total memory reserved for stack and heap in term units.
 * @param ctx a valid context.
 * @returns total memory that is total_size_in_bytes / sizeof(term).
 */
static inline unsigned long context_memory_size(const Context *ctx)
{
    return ctx->stack_base - ctx->heap_start;
}

/**
 * @brief Returns context heap size in term units
 *
 * @param ctx a valid context.
 * @returns context heap size in term units
 */
static inline unsigned long context_heap_size(const Context *ctx)
{
    return ctx->heap_ptr - ctx->heap_start;
}

/**
 * @brief Returns context heap size in term units
 *
 * @param ctx a valid context.
 * @returns context heap size in term units
 */
static inline unsigned long context_stack_size(const Context *ctx)
{
    return ctx->stack_base - ctx->e;
}

/**
 * @brief Checks if a contex is waiting a timeout.
 *
 * @details Check if given context has a timeout timestamp set, regardless current timestamp.
 * @param ctx a valid context.
 * @returns 1 if context has a timeout, otherwise 0.
 */
static inline int context_is_waiting_timeout(const Context *ctx)
{
    return ctx->timer_wheel_head.callback != NULL;
}

/**
 * @brief Returns a term representing an atom, from the suppliend string
 *
 * @details Converns a string to an atom.  Note that this function may have a side-effect on the
 *          global context.
 * @param glb pointer to the global context
 * @param string an AtomString
 * @return an atom term formed from the supplied atom string.
 */
static inline term context_make_atom(Context *ctx, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(ctx->global, string);
    return term_from_atom_index(global_atom_index);
}

/**
 * @brief Returns number of messages in the process's mailbox
 *
 * @param ctx a valid context.
 * @returns the number of messages in the process's mailbox
 */
size_t context_message_queue_len(Context *ctx);

/**
 * @brief Returns total amount of size (in byes) occuped by the process.
 *
 * @param ctx a valid context.
 * @returns total amount of size (in byes) occuped by the process
 */
size_t context_size(Context *ctx);

uint64_t context_monitor(Context *ctx, term monitor_pid, bool linked);

#endif
