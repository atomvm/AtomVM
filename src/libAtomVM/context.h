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
#include "list.h"
#include "mailbox.h"
#include "smp.h"
#include "term.h"
#include "timer_list.h"

#ifdef __cplusplus
extern "C" {
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

enum HeapGrowthStrategy
{
    BoundedFreeHeapGrowth = 0,
    MinimumHeapGrowth,
    FibonacciHeapGrowth
};

// Max number of x(N) & fr(N) registers
// BEAM sets this to 1024.
// x regs have an additional register that is used for storing an additional working element
// so the number of registers is MAX_REG + 1, but only MAX_REG are addressable as x registers
#define MAX_REG 16

struct Context
{
    // First fields matches ErlNifEnv structure.
    GlobalContext *global;
    Heap heap;
    term *e;
    term x[MAX_REG + 1];
    struct ListHead extended_x_regs;

    struct ListHead processes_list_head;

    struct ListHead processes_table_head;
    int32_t process_id;

    struct TimerListItem timer_list_head;

    struct ListHead monitors_head;

    avm_float_t *fr;

    size_t min_heap_size;
    size_t max_heap_size;
    enum HeapGrowthStrategy heap_growth_strategy;

    unsigned long cp;

    // saved state when scheduled out
    Module *saved_module;
    const void *saved_ip;
    // pointer to a trap or wait timeout handler
    void *restore_trap_handler;

    Mailbox mailbox;

    struct ListHead dictionary;

    // Ports support
    native_handler_f native_handler;

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

    enum ContextFlags ATOMIC flags;

    void *platform_data;

    term group_leader;

    term bs;
    size_t bs_offset;

    term exit_reason;
};

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

#define CONTEXT_MONITOR_RESOURCE_TAG 0x2
#define CONTEXT_MONITOR_MONITORED_PID_TAG 0x3
#define CONTEXT_MONITOR_MONITORING_PID_TAG 0x1

/**
 * @brief A regular monitor or a half link.
 */
struct Monitor
{
    struct ListHead monitor_list_head;
    uint64_t ref_ticks; // 0 for links
    term monitor_obj; // pid for links, CONTEXT_MONITOR_*_TAG for monitors
};

struct ExtendedRegister
{
    struct ListHead head;
    unsigned int index;
    term value;
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
 * @brief Ensure we have FP registers, allocating them if necessary.
 * @param c context fo allocate FP registers for
 */
static inline void context_ensure_fpregs(Context *c)
{
    if (UNLIKELY(c->fr == NULL)) {
        c->fr = (avm_float_t *) malloc(sizeof(avm_float_t) * MAX_REG);
        if (UNLIKELY(c->fr == NULL)) {
            fprintf(stderr, "Could not allocate FP registers\n");
            AVM_ABORT();
        }
    }
}

/**
 * @brief Starts executing a function
 *
 * @details Start executing bytecode for the specified function, this function will block until it terminates. The outcome is saved to x[0] register.
 * @param ctx the context that will be used to run the specified functions, x registers must be set to function arguments.
 * @param mod the module name C string.
 * @param function_name the function name C string.
 * @param arity the function arity (number of arguments that are required).
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
 * @brief Returns a context's stack base
 *
 * @details Used for stack traces
 * @param ctx a valid context.
 * @returns the stack base
 */
static inline term *context_stack_base(const Context *ctx)
{
    // Find which fragment the stack belongs to.
    if (ctx->e >= ctx->heap.heap_start && ctx->e <= ctx->heap.heap_end) {
        return ctx->heap.heap_end;
    }
    HeapFragment *fragment = ctx->heap.root->next;
    while (fragment) {
        if (ctx->e >= fragment->storage && ctx->e <= fragment->heap_end) {
            return fragment->heap_end;
        }
        fragment = fragment->next;
    }
    fprintf(stderr, "Context heap is corrupted, cannot find fragment with stack pointer.\n");
    AVM_ABORT();
}

/**
 * @brief Returns a context's stack size
 *
 * @details Return the number of terms currently on the stack. Used for
 * stack traces.
 * @param ctx a valid context.
 * @returns stack size in terms
 */
static inline size_t context_stack_size(const Context *ctx)
{
    term *stack_base = context_stack_base(ctx);
    return stack_base - ctx->e;
}

/**
 * @brief Returns available free memory in term units
 *
 * @details Returns the number of terms that can fit either on the heap.
 * @param ctx a valid context.
 * @returns available free memory that is avail_size_in_bytes / sizeof(term).
 */
static inline size_t context_avail_free_memory(const Context *ctx)
{
    // Check if stack is on current fragment
    if (ctx->e <= ctx->heap.heap_end && ctx->e >= ctx->heap.heap_start) {
        return ctx->e - ctx->heap.heap_ptr;
    }
    return ctx->heap.heap_end - ctx->heap.heap_ptr;
}

/**
 * @brief Compares a term with an AtomString.
 *
 * @details Checks if the given term and the given AtomString refers to the same atom.
 * This function is just a shortcut that uses the corresponding funtion from globalcontext.
 * @param ctx the current Context.
 * @param atom_a any term of any type, when it is not an atom false is always returned.
 * @param atom_string_b an atom string, which is the atom length followed by atom characters.
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
 * @brief Process a flush monitor signal.
 *
 * @param ctx the context being executed
 * @param ref_ticks the monitor reference
 * @param info whether to return FALSE_ATOM if no message was flushed.
 */
void context_process_flush_monitor_signal(Context *ctx, uint64_t ref_ticks, bool info);

/**
 * @brief Get process information.
 *
 * @param ctx the context being executed
 * @param out the answer term. Can be NULL if only the size matters.
 * @param term_size the size of the answer term, in words.
 * @param atom_key the key representing the info to get
 * @param heap the heap to allocate the answer to
 * @return \c true if successful, \c false in case of an error in which case
 * *out is filled with an exception atom if it was not NULL
 */
bool context_get_process_info(Context *ctx, term *out, size_t *term_size, term atom_key, Heap *heap);

/**
 * @brief Half-link process to another process
 *
 * @param monitor_pid process to link to
 * @return the allocated monitor or NULL if allocation failed
 */
struct Monitor *monitor_link_new(term monitor_pid);

/**
 * @brief Create a monitor on a process.
 *
 * @param monitor_pid monitoring process
 * @param ref_ticks reference of the monitor
 * @param is_monitoring if ctx is the monitoring process
 * @return the allocated monitor or NULL if allocation failed
 */
struct Monitor *monitor_new(term monitor_pid, uint64_t ref_ticks, bool is_monitoring);

/**
 * @brief Create a resource monitor.
 *
 * @param resource resource object
 * @param ref_ticks reference associated with the monitor
 * @param process_id process being monitored
 * @return the allocated resource monitor or NULL if allocation failed
 */
struct Monitor *monitor_resource_monitor_new(void *resource, uint64_t ref_ticks);

/**
 * @brief Half-unlink process to another process
 * @details Called within the process only. For the other end of the
 * link, an UnlinkSignal is sent that calls this function.
 *
 * @param ctx the context being executed
 * @param monitor_pid process to unlink from
 * @return 0 on success
 */
void context_unlink(Context *ctx, term monitor_pid);

/**
 * @brief Destroy a monitor on a process.
 * @details Called within the process only. This function is called from
 * DemonitorSignal.
 *
 * @param ctx the context being executed
 * @param ref_ticks reference of the monitor to remove
 * @return 0 on success
 */
void context_demonitor(Context *ctx, uint64_t ref_ticks);

/**
 * @brief Get target of a monitor.
 *
 * @param ctx the context being executed
 * @param ref_ticks reference of the monitor to remove
 * @param is_monitoring whether ctx is the monitoring process.
 * @return pid of monitoring process, self() if process is monitoring (and not
 * monitored) or term_invalid() if no monitor could be found.
 */
term context_get_monitor_pid(Context *ctx, uint64_t ref_ticks, bool *is_monitoring);

/**
 * @brief Add a monitor on a process.
 * @details Called within the process only. This function is called from
 * MonitorSignal. Monitor is not added if it already exists. Monitors are
 * identified by a reference, but links have no reference and a link can
 * only exist once.
 *
 * @param ctx the context being executed
 * @param new_monitor monitor object to add
 */
void context_add_monitor(Context *ctx, struct Monitor *new_monitor);

#ifdef __cplusplus
}
#endif

#endif
