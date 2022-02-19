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
 * @file context.h
 * @brief Context struct and related management functions
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

    bool trap_exit : 1;
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

    term group_leader;

    term bs;
    size_t bs_offset;

    term exit_reason;
    term mso_list;

    size_t min_free_space;
    uint32_t shrink_free_space_factor;

    unsigned num_gcs;
    unsigned num_gc_shrinks;
};

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct Monitor
{
    struct ListHead monitor_list_head;

    term monitor_pid;
    uint64_t ref_ticks;

    // this might be replaced with a handler function, this might be useful as a replacement
    // to leader process field or for any other purposes.
    // TODO: we might save useful bytes by assuming that ref_links == 0 means linked
    bool linked : 1;
};

/**
 * @brief Creates a new context
 *
 * @details Allocates a new Context struct and initialize it, the newly created context is also inserted into the processes table.
 * @param glb The global context of this virtual machine instance.
 * @returns created context.
 */
Context *context_new(GlobalContext *glb);

/**
 * @brief Destroys a context
 *
 * @details Frees context resources and memory and removes it from the processes table.
 * @param c the context that will be destroyed.
 */
void context_destroy(Context *c);

/**
 * @brief Starts executing a function
 *
 * @details Start executing bytecode for the specified function, this function will block until it terminates. The outcome is saved to x[0] register.
 * @param ctx the context that will be used to run the specified functions, x registers must be set to function arguments.
 * @param function_name the function name C string.
 * @param the function arity (number of arguments that are required).
 * @returns 1 if an error occurred, otherwise 0 is always returned.
 */
int context_execute_loop(Context *ctx, Module *mod, const char *function_name, int arity);

/**
 * @brief Returns 1 if the context is a port driver
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
 * @brief Checks if a context is waiting a timeout.
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
 * @brief Compares a term with an AtomString.
 *
 * @details Checks if the given term and the given AtomString refers to the same atom.
 * This function is just a shortcut that uses the corresponding funtion from globalcontext.
 * @param ctx the current Context.
 * @param atom_a any term of any type, when it is not an atom false is always returned.
 * @param AtomString an atom string, which is the atom length followed by atom characters.
 * @returns true if they both refer to the same atom, otherwise false.
 */
static inline bool context_is_term_equal_to_atom_string(Context *ctx, term atom_a, AtomString atom_string_b)
{
    return globalcontext_is_term_equal_to_atom_string(ctx->global, atom_a, atom_string_b);
}

/**
 * @brief Returns number of messages in the process's mailbox
 *
 * @param ctx a valid context.
 * @returns the number of messages in the process's mailbox
 */
size_t context_message_queue_len(Context *ctx);

/**
 * @brief Returns total amount of size (in byes) occupied by the process.
 *
 * @param ctx a valid context.
 * @returns total amount of size (in byes) occupied by the process
 */
size_t context_size(Context *ctx);

uint64_t context_monitor(Context *ctx, term monitor_pid, bool linked);
void context_demonitor(Context *ctx, term monitor_pid, bool linked);

#endif
