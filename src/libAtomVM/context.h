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

#ifdef __cplusplus
extern "C" {
#endif

#include "globalcontext.h"
#include "linkedlist.h"
#include "mailbox.h"
#include "term.h"
#include "timer_list.h"

#if !defined(AVM_NO_SMP) && !defined(__cplusplus)
#include <stdatomic.h>
#define ATOMIC _Atomic
#else
#define ATOMIC
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

typedef enum NativeHandlerResult
{
    NativeTerminate = 1,
    NativeContinue
} NativeHandlerResult;

// Native handlers. Return NativeTerminate to terminate, NativeContinue
// to keep the handler in the process table.
typedef NativeHandlerResult (*native_handler_f)(Context *ctx);

enum ContextFlags
{
    NoFlags = 0,
    WaitingTimeout = 1,
    WaitingTimeoutExpired = 2,
    Running = 4,
    Ready = 8,
    Killed = 16,
    Trap = 32,
};

// Max number of x(N) & fr(N) registers
// BEAM sets this to 1024.
#define MAX_REG 16

struct Context
{
    struct ListHead processes_list_head;

    struct ListHead processes_table_head;
    int32_t process_id;

    struct TimerListItem timer_list_head;

    struct ListHead monitors_head;

    term x[MAX_REG];

    term *heap_start;
    term *stack_base;
    term *heap_ptr;
    term *e;

    avm_int_t min_heap_size;
    avm_int_t max_heap_size;

    unsigned long cp;

    // saved state when scheduled out
    Module *saved_module;
    const void *saved_ip;
    // pointer to a trap or wait timeout handler
    void *restore_trap_handler;

    Mailbox mailbox;

    struct ListHead dictionary;

    GlobalContext *global;

    // Ports support
    native_handler_f native_handler;

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

    enum ContextFlags ATOMIC flags;

    void *platform_data;

    term group_leader;

    term bs;
    size_t bs_offset;

    term exit_reason;
    term mso_list;
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
 * @details Allocates a new Context struct and initialize it. The newly created
 * context is also inserted into the processes table, however it is not scheduled,
 * allowing for further initialization.
 * @param glb The global context of this virtual machine instance.
 * @returns created context.
 */
Context *context_new(GlobalContext *glb);

/**
 * @brief Destroys a context
 *
 * @details Frees context resources and memory and removes it from the processes table.
 * This should be called from the scheduler only. To actually delete a context that
 * was created with context_new, use scheduler_terminate.
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
    for (int i = live; i < MAX_REG; i++) {
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

/**
 * @brief Set or clear a flag on another context.
 *
 * @details atomically update flags <- (flags & mask) | value
 * @param ctx the context to set/clear flag on.
 * @param mask the mask to apply on flags
 * @param value the value to set
 */
void context_update_flags(Context *ctx, int mask, int value);

/**
 * @brief Get flags on a given context.
 *
 * @param ctx the context to get flags on.
 * @param mask the mask to apply on flags
 */
static inline int context_get_flags(Context *ctx, int mask)
{
    return ctx->flags & mask;
}

/**
 * @brief Process a kill signal, setting the exit reason and changing the
 * killed flag.
 *
 * @param ctx the context being executed
 * @param signal the kill message
 */
void context_process_kill_signal(Context *ctx, struct TermSignal *signal);

/**
 * @brief Process a process info request signal.
 *
 * @param ctx the context being executed
 * @param signal the kill message
 */
void context_process_process_info_request_signal(Context *ctx, struct BuiltInAtomRequestSignal *signal);

/**
 * @brief Process a trap answer signal.
 *
 * @param ctx the context being executed
 * @param signal the answer message
 * @return \c true if successful, \c false in case of memory error
 */
bool context_process_signal_trap_answer(Context *ctx, struct TermSignal *signal);

/**
 * @brief Get process information.
 *
 * @param ctx the context being executed
 * @param out the answer term
 * @param atom_key the key representing the info to get
 * @return \c true if successful, \c false in case of an error in which case
 * *out is filled with an exception atom
 */
bool context_get_process_info(Context *ctx, term *out, term atom_key);

uint64_t context_monitor(Context *ctx, term monitor_pid, bool linked);
void context_demonitor(Context *ctx, term monitor_pid, bool linked);

#ifdef __cplusplus
}
#endif

#endif
