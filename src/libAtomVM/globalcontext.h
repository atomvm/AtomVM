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
 * @file globalcontext.h
 * @brief GlobalContext struct and related management functions
 *
 * @details GlobalContext keeps the state of an AtomVM instance, multiple instances can run simultaneously.
 */

#ifndef _GLOBALCONTEXT_H_
#define _GLOBALCONTEXT_H_

#include <stdint.h>

#include "atom.h"
#include "atom_table.h"
#include "erl_nif.h"
#include "ets.h"
#include "list.h"
#include "mailbox.h"
#include "smp.h"
#include "synclist.h"
#include "term.h"
#include "timer_list.h"
#include "valueshashtable.h"

#ifdef __cplusplus
extern "C" {
#endif

#define INVALID_PROCESS_ID 0

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

#ifndef TYPEDEF_GLOBALCONTEXT
#define TYPEDEF_GLOBALCONTEXT
typedef struct GlobalContext GlobalContext;
#endif

#ifndef TYPEDEF_MAILBOXMESSAGE
#define TYPEDEF_MAILBOXMESSAGE
typedef struct MailboxMessage MailboxMessage;
#endif

#ifndef TYPEDEF_MESSAGE
#define TYPEDEF_MESSAGE
typedef struct Message Message;
#endif

struct MessageQueueItem
{
    struct MessageQueueItem *next;
    MailboxMessage *message;
    int32_t process_id;
};

struct RefcBinaryQueueItem
{
    struct RefcBinaryQueueItem *next;
    struct RefcBinary *refc;
};

struct GlobalContext
{
    struct ListHead ready_processes;
    struct ListHead running_processes;
    struct ListHead waiting_processes;
    // This lock is held when manipulating the process list and also
    // when running native handlers.
#ifndef AVM_NO_SMP
    SpinLock processes_spinlock;
#endif
#ifdef AVM_TASK_DRIVER_ENABLED
    // Queue of messages that could not be directly sent from a driver task
    struct MessageQueueItem *ATOMIC message_queue;
    // Queue of refc binaries that could not be directly ref decremented from a driver task
    struct RefcBinaryQueueItem *ATOMIC refc_queue;
#endif
    struct SyncList refc_binaries;
    struct SyncList processes_table;
    struct SyncList registered_processes;
    struct SyncList listeners;
    struct SyncList resource_types;
    struct SyncList select_events;

    struct Ets ets;

    int32_t last_process_id;

    struct AtomTable *atom_table;
    struct ValuesHashTable *modules_table;

#ifndef AVM_NO_SMP
    RWLock *modules_lock;
#endif
    Module **modules_by_index;
    int ATOMIC loaded_modules_count;

    struct SyncList avmpack_data;

    struct TimerList timer_list;
#ifndef AVM_NO_SMP
    SpinLock timer_spinlock;
#endif

#if !defined(AVM_NO_SMP) && ATOMIC_LLONG_LOCK_FREE == 2
    unsigned long long ATOMIC ref_ticks;
#else
    unsigned long long ref_ticks;
#ifndef AVM_NO_SMP
    SpinLock ref_ticks_spinlock;
#endif
#endif

#ifndef AVM_NO_SMP
    int ATOMIC online_schedulers;
    int running_schedulers; // GUARDED_BY(schedulers_mutex)
    bool ATOMIC waiting_scheduler;
    Mutex *schedulers_mutex;
    CondVar *schedulers_cv;
    bool ATOMIC scheduler_stop_all;
#else
    bool scheduler_stop_all;
#endif

#ifndef AVM_NO_SMP
    SpinLock env_spinlock;
#endif

    term ATOMIC node_name;
    uint32_t ATOMIC creation;
    ErlNifResourceType *dist_connection_resource_type;
    struct SyncList dist_connections;

#if HAVE_OPEN && HAVE_CLOSE
    ErlNifResourceType *posix_fd_resource_type;
#endif

#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
    ErlNifResourceType *posix_dir_resource_type;
#endif

    void *platform_data;
};

enum SendMessageResult
{
    SEND_MESSAGE_OK = 0,
    SEND_MESSAGE_PROCESS_NOT_FOUND = 1
};

/**
 * @brief Creates a new GlobalContext
 *
 * @details Allocates a new GlobalContext struct and initialize it, the newly created global context is a new AtomVM instance.
 * @returns A newly created GlobalContext.
 */
GlobalContext *globalcontext_new();

/**
 * @brief Destoys an existing GlobalContext
 *
 * @details Frees global context resources and memory and removes it from the processes table.
 * @param glb the global context that will be destroyed.
 */
void globalcontext_destroy(GlobalContext *glb);

/**
 * @brief Gets a Context from the process table, without acquiring a lock on the process table.
 *
 * @details Retrieves from the process table the context with the given local
 * process id. If the process can be found, without locking the process table.
 * This is unsafe unless a lock on the process table has been obtained previously.
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @returns a Context * with the requested local process id or NULL if not found.
 */
Context *globalcontext_get_process_nolock(GlobalContext *glb, int32_t process_id);

/**
 * @brief Gets a Context from the process table, acquiring a lock on the process
 * table.
 *
 * @details Retrieves from the process table the context with the given local
 * process id. If the process can be found, the process table is locked.
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @returns a Context * with the requested local process id or NULL if not found.
 */
Context *globalcontext_get_process_lock(GlobalContext *glb, int32_t process_id);

/**
 * @brief Unlock the process table after c was gotten.
 *
 * @param glb the global context (that owns the process table).
 * @param c the result of `globalcontext_get_process_lock`. If NULL, does
 * nothing, otherwise releases the lock on process table.
 */
void globalcontext_get_process_unlock(GlobalContext *glb, Context *c);

/**
 * @brief Determine if a process exists.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @returns true if a process exists with this id.
 */
bool globalcontext_process_exists(GlobalContext *glb, int32_t process_id);

/**
 * @brief Send a message to a process identified by its id.
 *
 * @details Safely send a message to the process, doing nothing is the process
 * cannot be found.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @param t the message to send.
 */
void globalcontext_send_message(GlobalContext *glb, int32_t process_id, term t);

/**
 * @brief Send a message to a process from another process.
 * There should be a lock on the process table. This variant can be used by
 * listener handlers as an optimization (instead of sending a message to the
 * port context which should then forward it to the target context).
 *
 * @details Safely send a message to the process, doing nothing if the process
 * cannot be found.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the target process id.
 * @param t the message to send.
 */
void globalcontext_send_message_nolock(GlobalContext *glb, int32_t process_id, term t);

/**
 * @brief Post a mailbox message to a process identified by its id.
 * @details This function is only used by enif_select_read/enif_select_write to
 * post a message that is built before.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @param m the mailbox message to send.
 * @return SEND_MESSAGE_OK if the message was sent (and ownership transfered).
 */
enum SendMessageResult globalcontext_post_message(GlobalContext *glb, int32_t process_id, Message *m);

#ifdef AVM_TASK_DRIVER_ENABLED
/**
 * @brief Send a message to a process identified by its id. This variant is to
 * be used from task drivers. It tries to acquire the necessary locks and if it
 * fails, it enqueues the message which will be delivered on the next scheduler
 * context switch.
 *
 * @details Safely send a message to the process, doing nothing if the process
 * cannot be found.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the target process id.
 * @param type the type of message to send, can be NormalMessage or a signal
 * @param t the message to send.
 */
void globalcontext_send_message_from_task(GlobalContext *glb, int32_t process_id, enum MessageType type, term t);

/**
 * @brief Post a mailbox message to a process identified by its id. Variant
 * to be used from task drivers.
 * @details This function is only used by enif_select_read/enif_select_write to
 * post a message that is built before.
 *
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @param m the mailbox message to send.
 * @return SEND_MESSAGE_OK if the message was sent (and ownership transfered).
 */
enum SendMessageResult globalcontext_post_message_from_task(GlobalContext *glb, int32_t process_id, Message *m);

/**
 * @brief Enqueue a refc binary from a task driver, to be refc decremented
 * later from the scheduler.
 *
 * @param glb the global context
 * @param refc the refc binary to enqueue
 */
void globalcontext_refc_decrement_refcount_from_task(GlobalContext *glb, struct RefcBinary *refc);

/**
 * @brief Process refc binaries and messages enqueued from task drivers.
 *
 * @details This function is called from the scheduler.
 *
 * @param glb the global context
 */
void globalcontext_process_task_driver_queues(GlobalContext *glb);
#endif

/**
 * @brief Initialize a new process, providing it with a process id.
 *
 * @details This function also inserts the process into the process and waiting
 * tables.
 * @param glb the global context.
 * @param ctx the process to initialize
 */
void globalcontext_init_process(GlobalContext *glb, Context *ctx);

/**
 * @brief Register a process
 *
 * @details Register a process with a certain name (atom) so it can be easily retrieved later.
 * @param glb the global context, each registered process will be globally available for that context.
 * @param atom_index the atom table index.
 * @param local_pid_or_port the local pid or port
 * @returns \c true if the process was registered, \c false if another process with the same name already existed
 */
bool globalcontext_register_process(GlobalContext *glb, int atom_index, term local_pid_or_port);

/**
 * @brief Get registered name for a process/port
 *
 * @details Gets a process or port name (atom).
 * @param glb the global context.
 * @param local_process_id the process local id.
 * @returns
 * - `{registered_name, RegisteredName}` if the process/port was registered,
 * - `[]` if the process/port is not registered.
 */
term globalcontext_get_registered_name_process(GlobalContext *glb, int local_process_id);

/**
 * @brief Get a registered process
 *
 * @details Returns the local process id of a previously registered process.
 * @param glb the global context.
 * @param atom_index the atom table index.
 * @returns a previously registered process local id or UNDEFINED_ATOM
 */
term globalcontext_get_registered_process(GlobalContext *glb, int atom_index);

/**
 * @brief Unregister a process by name
 *
 * @details Unregister a process with a certain name (atom).
 * @param glb the global context, each registered process will be globally available for that context.
 * @param atom_index the atom table index.
 * @returns \c true if the process was unregistered, \c false otherwise
 */
bool globalcontext_unregister_process(GlobalContext *glb, int atom_index);

/**
 * @brief Remove entry(ies) from registered atoms by process id
 *
 * @details Unregister a process with a certain process id. This is used when a process dies to ensure
 * the process is not registered and remove it from the registered atoms table if it is.
 * @param glb the global context, each registered process will be globally available for that context.
 * @param process_id the process id of the entry to remove.
 */
void globalcontext_maybe_unregister_process_id(GlobalContext *glb, int process_id);

/**
 * @brief Inserts an atom into the global atoms table, making a copy of the supplied atom
 * string, if copy is non-zero.
 *
 * @details Inserts an atom into the global atoms table and returns its id.
 * @param glb the global context.
 * @param atom_string the atom string that will be added to the global atoms table, it will not be copied so it must stay allocated and valid.
 * @param copy if `true`, make a copy of the input atom_string if the atom is not already in the table.  The table
 * assumes "ownership" of the allocated memory.
 * @returns newly added atom id or term_invalid_term() in case of failure.
 */
static inline term globalcontext_insert_atom_maybe_copy(GlobalContext *glb, const uint8_t *atom_data, size_t atom_len, bool copy)
{
    atom_index_t global_atom_index;
    enum AtomTableEnsureAtomResult ensure_result = atom_table_ensure_atom(
        glb->atom_table, atom_data, atom_len, copy ? AtomTableCopyAtom : AtomTableNoOpts, &global_atom_index);
    if (UNLIKELY(ensure_result != AtomTableEnsureAtomOk)) {
        return term_invalid_term();
    }
    return term_from_atom_index(global_atom_index);
}

/**
 * @brief Compares an atom table index with an AtomString.
 *
 * @details Checks if the given atom table index and the given AtomString refers to the same atom.
 * @param glb the global context.
 * @param atom_index_a an atom table index.
 * @param atom_string_b an atom string, which is the atom length followed by atom characters.
 * @returns true if they both refer to the same atom, otherwise false.
 */
bool globalcontext_is_atom_index_equal_to_atom_string(GlobalContext *glb, atom_index_t atom_index_a, AtomString atom_string_b);

/**
 * @brief Compares a term with an AtomString.
 *
 * @details Checks if the given term and the given AtomString refers to the same atom.
 * @param global the global context.
 * @param atom_a any term of any type, when it is not an atom false is always returned.
 * @param atom_string_b an atom string, which is the atom length followed by atom characters.
 * @returns true if they both refer to the same atom, otherwise false.
 */
static inline bool globalcontext_is_term_equal_to_atom_string(GlobalContext *global, term atom_a, AtomString atom_string_b)
{
    if (!term_is_atom(atom_a)) {
        return false;
    }

    atom_index_t atom_index_a = term_to_atom_index(atom_a);
    return globalcontext_is_atom_index_equal_to_atom_string(global, atom_index_a, atom_string_b);
}

/**
 * @brief Returns a term representing an atom, from the supplied string.
 *
 * @details Converts a string to an atom.  Note that this function may have a side-effect on the
 *          global context. It doesn't copy the atom string and is meant to be called with
 *          constant atom strings in code.
 * @param glb pointer to the global context
 * @param atom_string an AtomString
 * @return an atom term formed from the supplied atom string.
 */
static inline term globalcontext_make_atom(GlobalContext *glb, AtomString atom_string)
{
    return globalcontext_insert_atom_maybe_copy(glb, atom_string_data(atom_string), atom_string_len(atom_string), false);
}

/**
 * @brief Returns the term for an existing atom.
 *
 * @details This function allows to get an atom term associated to the given atom string, if and
 *          only if the given atom is already in the atom table, otherwise an invalid term is
 *          returned.
 * @param   glb the global context.
 * @param   atom_string the atom string that will be looked into the atom table.
 * @returns the term associated with the supplied atom string when already existing in the atom
 *          table, otherwise an invalid term.
 */
term globalcontext_existing_term_from_atom_string(GlobalContext *glb, AtomString atom_string);

/**
 * @brief Inserts a module to the modules table.
 *
 * @details Inserts an already loaded module to the modules table and assigns and index to it so it can be retrieved later by name or index. The module is then owned by the global context that will destroy it when globalcontext_destroy is invoked.
 * @param global the global context.
 * @param module the module that will be added to the modules table.
 * @returns the module index if successful, otherwise -1.
 */
int globalcontext_insert_module(GlobalContext *global, Module *module);

/**
 * @brief Get a module by index.
 *
 * @details Safely retrieve the module by index considering the table can be
 * reallocated by globalcontext_insert_module.
 * @param global the global context.
 * @param index the module index.
 * @returns the module
 */
Module *globalcontext_get_module_by_index(GlobalContext *global, int index);

/**
 * @brief Returns the module with the given name
 *
 * @details Tries to get the module with the given name from the modules table and eventually loads it.
 * @param global the global context.
 * @param module_name_atom the module name.
 * @returns a pointer to a Module struct.
 */
Module *globalcontext_get_module(GlobalContext *global, atom_index_t module_name_atom);

/**
 * @brief Load a given module from registered AVM packs
 *
 * @details This function is typically called from sys_load_module. It does
 * not check if the module is already loaded and allocates a new module
 * structure.
 * @param global the global context.
 * @param module_name_atom the module name.
 * @returns a pointer to a Module struct or NULL if the module could not be
 * found.
 */
Module *globalcontext_load_module_from_avm(GlobalContext *global, const char *module_name);

#ifndef __cplusplus
static inline uint64_t globalcontext_get_ref_ticks(GlobalContext *global)
{
#if defined(AVM_NO_SMP) || ATOMIC_LLONG_LOCK_FREE == 2
    return ++global->ref_ticks;
#else
    smp_spinlock_lock(&global->ref_ticks_spinlock);
    unsigned long long value = ++global->ref_ticks;
    smp_spinlock_unlock(&global->ref_ticks_spinlock);
    return value;
#endif
}
#endif

#ifdef __cplusplus
}
#endif

#endif
