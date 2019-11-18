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
 * @file globalcontext.h
 * @brief GlobalContext struct and releated management functions
 *
 * @details GlobalContext keeps the state of an AtomVM instance, multiple instances can run simultanously.
 */

#ifndef _GLOBALCONTEXT_H_
#define _GLOBALCONTEXT_H_

#include <stdint.h>
#include <time.h>

#include "atom.h"
#include "term.h"
#include "linkedlist.h"

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct GlobalContext;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

struct Module;

typedef struct
{
    struct ListHead ready_processes;
    struct ListHead waiting_processes;
    struct ListHead *listeners;
    struct ListHead *processes_table;
    struct ListHead *registered_processes;

    int32_t last_process_id;

    struct AtomsHashTable *atoms_table;
    struct ValuesHashTable *atoms_ids_table;
    struct AtomsHashTable *modules_table;
    Module **modules_by_index;
    int loaded_modules_count;

    const void *avmpack_data;
    const void *avmpack_platform_data;

    struct TimerWheel *timer_wheel;
    uint32_t last_seen_millis;

    uint64_t ref_ticks;

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
 * @brief Inserts an atom into the global atoms table
 *
 * @details Inserts an atom into the global atoms table and returns its id.
 * @param glb the global context.
 * @param atom_string the atom string that will be added to the global atoms table, it will not be copied so it must stay allocated and valid.
 * @returns newly added atom id or -1 in case of failure.
 */
int globalcontext_insert_atom(GlobalContext *glb, AtomString atom_string);

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

static inline uint64_t globalcontext_get_ref_ticks(GlobalContext *global)
{
    return ++global->ref_ticks;
}

#endif
