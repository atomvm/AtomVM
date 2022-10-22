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
 * @file module.h
 * @brief Module loading functions
 *
 * @details This header defines all the module loading functions and the Module struct.
 */

#ifndef _MODULE_H_
#define _MODULE_H_

#include <stdint.h>

#include "atom.h"
#include "context.h"
#include "globalcontext.h"
#include "valueshashtable.h"

typedef struct
{
    char magic[4];
    uint32_t size;
    uint32_t info_size;
    uint32_t version;
    uint32_t opcode_max;
    uint32_t labels;
    uint32_t functions_count;

    uint8_t code[1];
} __attribute__((packed)) CodeChunk;

struct ExportedFunction;

struct LiteralEntry
{
    uint32_t size;
    void const *data;
};

struct Module
{
    GlobalContext *global;

#ifdef ENABLE_ADVANCED_TRACE
    void *import_table;
#endif

    CodeChunk *code;
    void *export_table;
    void *atom_table;
    void *fun_table;
    void *str_table;
    size_t str_table_len;

    union imported_func *imported_funcs;

    void **labels;

    void *literals_data;

    struct LiteralEntry *literals_table;

    int *local_atoms_to_global_table;

    void *module_platform_data;

    int module_index;

    int end_instruction_ii;

    unsigned int free_literals_data : 1;
};

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

enum ModuleLoadResult
{
    MODULE_LOAD_OK = 0,
    MODULE_ERROR_FAILED_ALLOCATION = 1
};

#ifdef ENABLE_ADVANCED_TRACE
/**
 * @briefs Gets imported function module and name
 *
 * @details Gets imported function module and name given its import table index.
 * @param this_module the module on which the function will be searched.
 * @param module_atom module name atom string.
 * @param function_atom function name atom string.
 */
void module_get_imported_function_module_and_name(const Module *this_module, int index, AtomString *module_atom, AtomString *function_atom);
#endif

/**
 * @briefs Gets exported function index by searching it by function name and arity
 *
 * @details Gets exported function index by searching it by function name and arity
 * @param this_module the module on which the function will be searched.
 * @param func_name function name atom string.
 * @param func_arity function arity.
 */
uint32_t module_search_exported_function(Module *this_module, AtomString func_name, int func_arity);

/***
 * @brief Destoys an existing Module
 *
 * @details Destroys a module and free Module resources.
 * @param module the module that will be freed.
 */
void module_destroy(Module *module);

/**
 * @brief Parse a BEAM file and returns a Module
 *
 * @details Parse a BEAM file a returns a newly allocated and initialized Module struct.
 * @param iff_binary the IFF file data.
 * @param size the size of the buffer containing the IFF data.
 */
Module *module_new_from_iff_binary(GlobalContext *global, const void *iff_binary, unsigned long size);

/**
 * @brief Gets a literal stored on the literal table of the specified module
 *
 * @details Loads and deserialize a term stored in the literal table and returns a term.
 * @param mod The module that owns that is going to be loaded.
 * @param index a valid literal index.
 */
term module_load_literal(Module *mod, int index, Context *ctx);

/**
 * @brief Gets the AtomString for the given local atom id
 *
 * @details Gets an AtomString for the given local atom id from the global table.
 * @param mod the module that owns the atom.
 * @param local_atom_id module atom table index.
 * @return the AtomString for the given module atom index.
 */
static inline AtomString module_get_atom_string_by_id(const Module *mod, int local_atom_id)
{
    int global_id = mod->local_atoms_to_global_table[local_atom_id];
    return (AtomString) valueshashtable_get_value(mod->global->atoms_ids_table, global_id, (unsigned long) NULL);
}

/**
 * @brief Gets a term for the given local atom id
 *
 * @details Gets the global atom id for the the given local atom id and casts it to a term.
 * @param mod the module that owns the atom.
 * @param local_atom_id module atom table index.
 * @return a term for the given module atom index.
 */
static inline term module_get_atom_term_by_id(const Module *mod, int local_atom_id)
{
    int global_id = mod->local_atoms_to_global_table[local_atom_id];
    return term_from_atom_index(global_id);
}

/**
 * @brief Resolves an unresolved function reference
 *
 * @details Resolves an unresolved function reference and it replaces the unresolved reference with a ModuleFunction struct,
 * also it loads the referenced module if it hasn't been loaded yet.
 * @param import_table_index the unresolved function index.
 * @param func the unresolved function placeholder struct.
 */
const struct ExportedFunction *module_resolve_function(Module *mod, int import_table_index);

/*
 * @brief Casts an instruction index and module index to a return address
 *
 * @details Casts an instruction index and module index to a value that return instruction can restore later.
 * @param module_index the module index
 * @param the instruction index (0 is the first module instruction)
 * @return casted return address
 */
static inline term module_address(unsigned int module_index, unsigned int instruction_index)
{
    return (term) ((module_index << 24) | (instruction_index << 2));
}

static inline uint32_t module_get_fun_freeze(const Module *this_module, int fun_index)
{
    const uint8_t *table_data = (const uint8_t *) this_module->fun_table;
    int funs_count = READ_32_ALIGNED(table_data + 8);

    if (UNLIKELY(fun_index >= funs_count)) {
        abort();
    }

    // fun atom index
    // arity
    // label
    // index
    uint32_t n_freeze = READ_32_ALIGNED(table_data + fun_index * 24 + 16 + 12);
    // ouniq

    return n_freeze;
}

static inline void module_get_fun(const Module *this_module, int fun_index, uint32_t *label, uint32_t *arity, uint32_t *n_freeze)
{
    const uint8_t *table_data = (const uint8_t *) this_module->fun_table;
    int funs_count = READ_32_ALIGNED(table_data + 8);

    if (UNLIKELY(fun_index >= funs_count)) {
        abort();
    }

    // fun atom index
    *arity = READ_32_ALIGNED(table_data + fun_index * 24 + 4 + 12);
    *label = READ_32_ALIGNED(table_data + fun_index * 24 + 8 + 12);
    // index
    *n_freeze = READ_32_ALIGNED(table_data + fun_index * 24 + 16 + 12);
    // ouniq
}

static inline const uint8_t *module_get_str(Module *mod, size_t offset, size_t *remaining)
{
    if (offset >= mod->str_table_len) {
        return NULL;
    }
    *remaining = mod->str_table_len - offset;
    return ((const uint8_t *) mod->str_table) + 8 + offset;
}

#endif
