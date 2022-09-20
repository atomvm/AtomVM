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

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "atom.h"
#include "linkedlist.h"
#include "term.h"

#define INVALID_PROCESS_ID 0

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

#ifndef __cplusplus
struct GlobalContext;
#endif

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

struct Module;

typedef struct
{
    struct ListHead ready_processes;
    struct ListHead waiting_processes;
    struct ListHead refc_binaries;
    struct ListHead processes_table;
    struct ListHead *registered_processes;

    int32_t last_process_id;

    struct AtomsHashTable *atoms_table;
    struct ValuesHashTable *atoms_ids_table;
    struct AtomsHashTable *modules_table;
    Module **modules_by_index;
    int loaded_modules_count;

    struct ListHead avmpack_data;
    const void **avmpack_platform_data;

    struct TimerWheel *timer_wheel;
    uint32_t last_seen_millis;

    uint64_t ref_ticks;

    void *platform_data;
} GlobalContext;

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
 * @param c the global context that will be destroyed.
 */
void globalcontext_destroy(GlobalContext *glb);

/**
 * @brief Gets a Context from the process table
 *
 * @details Retrieves from the process table the context with the given local process id.
 * @param glb the global context (that owns the process table).
 * @param process_id the local process id.
 * @returns a Context * with the requested local process id.
 */
Context *globalcontext_get_process(GlobalContext *glb, int32_t process_id);

/**
 * @brief Gets a new process id
 *
 * @details Returns a new (unused) process id, this functions should be used to allocate a new local process id.
 * @param glb the global context.
 * @returns A new local process id integer.
 */
int32_t globalcontext_get_new_process_id(GlobalContext *glb);

/**
 * @brief Register a process
 *
 * @details Register a process with a certain name (atom) so it can be easily retrieved later.
 * @param glb the global context, each registered process will be globally available for that context.
 * @param atom_index the atom table index.
 * @param local_process_id the process local id.
 */
void globalcontext_register_process(GlobalContext *glb, int atom_index, int local_process_id);

/**
 * @brief Get a registered process
 *
 * @details Returns the local process id of a previously registered process.
 * @param glb the global context.
 * @param atom_index the atom table index.
 * @returns a previously registered process local id.
 */
int globalcontext_get_registered_process(GlobalContext *glb, int atom_index);

/**
 * @brief equivalent to globalcontext_insert_atom_maybe_copy(glb, atom_string, 0);
 */
int globalcontext_insert_atom(GlobalContext *glb, AtomString atom_string);

/**
 * @brief Inserts an atom into the global atoms table, making a copy of the supplied atom
 * string, if copy is non-zero.
 *
 * @details Inserts an atom into the global atoms table and returns its id.
 * @param glb the global context.
 * @param atom_string the atom string that will be added to the global atoms table, it will not be copied so it must stay allocated and valid.
 * @param copy if non-zero, make a copy of the input atom_string if the atom is not already in the table.  The table
 * assumes "ownership" of the allocated memory.
 * @returns newly added atom id or -1 in case of failure.
 */
int globalcontext_insert_atom_maybe_copy(GlobalContext *glb, AtomString atom_string, int copy);

/**
 * @brief Compares an atom table index with an AtomString.
 *
 * @details Checks if the given atom table index and the given AtomString refers to the same atom.
 * @param glb the global context.
 * @param atom_index_a an atom table index.
 * @param AtomString an atom string, which is the atom length followed by atom characters.
 * @returns true if they both refer to the same atom, otherwise false.
 */
bool globalcontext_is_atom_index_equal_to_atom_string(GlobalContext *glb, int atom_index_a, AtomString atom_string_b);

/**
 * @brief Compares a term with an AtomString.
 *
 * @details Checks if the given term and the given AtomString refers to the same atom.
 * @param glb the global context.
 * @param atom_a any term of any type, when it is not an atom false is always returned.
 * @param AtomString an atom string, which is the atom length followed by atom characters.
 * @returns true if they both refer to the same atom, otherwise false.
 */
static inline bool globalcontext_is_term_equal_to_atom_string(GlobalContext *global, term atom_a, AtomString atom_string_b)
{
    if (!term_is_atom(atom_a)) {
        return false;
    }

    int atom_index_a = term_to_atom_index(atom_a);
    return globalcontext_is_atom_index_equal_to_atom_string(global, atom_index_a, atom_string_b);
}

/**
 * @brief   Returns the AtomString value of a term.
 *
 * @details This function fetches the AtomString value of the atom associated
 *          with the supplied term.  The input term must be an atom type.
 *          If no such atom is registered in the global table, this function
 *          returns NULL.  The caller should NOT free the data associated with
 *          the returned value.
 * @param   glb the global context
 * @param   t the atom term
 * @returns the AtomString associated with the supplied atom term.
 */
AtomString globalcontext_atomstring_from_term(GlobalContext *glb, term t);

/*
 * @brief Insert an already loaded module with a certain filename to the modules table.
 *
 * @details Insert an already loaded module to the modules table using the filename without ".beam" as the module name.
 * @param glb the global context.
 * @param module the module that will be added to the modules table.
 * @param filename module filename (without the path).
 */
void globalcontext_insert_module_with_filename(GlobalContext *glb, Module *module, const char *filename);

/**
 * @brief Inserts a module to the modules table.
 *
 * @details Inserts an already loaded module to the modules table and assigns and index to it so it can be retrieved later by name or index.
 * @param global the global context.
 * @param module the module that will be added to the modules table.
 * @param module_name_atom the module name (as AtomString).
 * @returns the module index if successful, otherwise -1.
 */
int globalcontext_insert_module(GlobalContext *global, Module *module, AtomString module_name_atom);

/**
 * @brief Returns the module with the given name
 *
 * @details Tries to get the module with the given name from the modules table and eventually loads it.
 * @param global the global context.
 * @param module_name_atom the module name.
 * @returns a pointer to a Module struct.
 */
Module *globalcontext_get_module(GlobalContext *global, AtomString module_name_atom);

void globalcontext_demonitor(GlobalContext *global, uint64_t ref_ticks);
void globalcontext_unlink(GlobalContext *global, term pid);

static inline uint64_t globalcontext_get_ref_ticks(GlobalContext *global)
{
    return ++global->ref_ticks;
}

#ifdef __cplusplus
}
#endif

#endif
