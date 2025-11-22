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

#include <stdbool.h>
#include <stdint.h>

#include "atom.h"
#include "atom_table.h"
#include "context.h"
#include "exportedfunction.h"
#include "globalcontext.h"
#include "jit.h"
#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef AVM_NO_SMP

#ifndef TYPEDEF_MUTEX
#define TYPEDEF_MUTEX
typedef struct Mutex Mutex;
#endif

#endif

#ifndef AVM_NO_SMP
#define SMP_MODULE_LOCK(mod) smp_mutex_lock(mod->mutex)
#define SMP_MODULE_UNLOCK(mod) smp_mutex_unlock(mod->mutex)
#else
#define SMP_MODULE_LOCK(mod)
#define SMP_MODULE_UNLOCK(mod)
#endif

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

typedef struct
{
    uint16_t architecture;
    uint16_t variant;
    uint32_t offset;
} __attribute__((packed)) NativeCodeArch;

typedef struct
{
    char magic[4];
    uint32_t size;
    uint32_t info_size;
    uint32_t labels;
    uint16_t version;
    uint16_t architectures_count;
    NativeCodeArch architectures[1];
} __attribute__((packed)) NativeCodeChunk;

struct ExportedFunction;

struct LiteralEntry
{
    uint32_t size;
    void const *data;
};

struct ModuleFilename
{
    uint8_t *data;
    size_t len;
};

struct Module
{
    int module_index;

#ifdef ENABLE_ADVANCED_TRACE
    void *import_table;
#endif

    CodeChunk *code;
    void *export_table;
    void *local_table;
    void *atom_table;
    void *fun_table;
    void *str_table;
    size_t str_table_len;
    size_t line_refs_count;
    const uint8_t *line_refs_table;
    size_t locations_count;
    const uint8_t *locations_table;
#ifndef AVM_NO_JIT
    ModuleNativeEntryPoint native_code;
#endif

    unsigned int *line_refs_offsets;
    size_t line_refs_offsets_count;

    const struct ExportedFunction **imported_funcs;

    const uint8_t **labels;

    void *literals_data;

    struct LiteralEntry *literals_table;

    void *types_data;

    atom_index_t *local_atoms_to_global_table;

    void *module_platform_data;

    int end_instruction_ii;

    unsigned int free_literals_data : 1;

#ifndef AVM_NO_SMP
    Mutex *mutex;
#endif
};

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

enum ModuleLoadResult
{
    MODULE_LOAD_OK = 0,
    MODULE_ERROR_FAILED_ALLOCATION = 1,
    MODULE_ERROR_INVALID = 2
};

#ifdef ENABLE_ADVANCED_TRACE
/**
 * @brief Gets imported function module and name
 *
 * @details Gets imported function module and name given its import table index.
 * @param this_module the module on which the function will be searched.
 * @param index the modules import table offset to begin searching.
 * @param module_atom module name atom string.
 * @param function_atom function name atom string.
 * @param glb the global context.
 */
void module_get_imported_function_module_and_name(const Module *this_module, int index, AtomString *module_atom, AtomString *function_atom, GlobalContext *glb);
#endif

/**
 * @brief Count exported functions of a given module
 *
 * @details Get the number of exported functions.
 * This function is used to compute the required heap size of the list of
 * exported functions.
 *
 * @param this_module the module to count exported functions of
 * @return the number of exported functions
 */
size_t module_get_exported_functions_count(Module *this_module);

/**
 * @brief Gets exported function index by searching it by function name and arity
 *
 * @details Gets exported function index by searching it by function name and arity
 * @param this_module the module on which the function will be searched.
 * @param func_name function name atom string.
 * @param func_arity function arity.
 */
uint32_t module_search_exported_function(Module *this_module, atom_index_t func_name, int func_arity);

/**
 * @brief Determine heap size of exported functions list.
 *
 * @param this_module the module to count exported functions of
 * @return the size, in terms, of the exported function list
 */
static inline size_t module_get_exported_functions_list_size(Module *this_module)
{
    return (TUPLE_SIZE(2) + CONS_SIZE) * module_get_exported_functions_count(this_module);
}

/**
 * @brief Get the list of exported functions
 * @details Create a list of exported functions of the form `{FunctionName, Arity}`
 * To create this list, the heap must be grown by `module_get_exported_functions_list_size`
 * terms.
 *
 * @param this_module the module to count exported functions of
 * @param heap heap to allocate tuples
 * @return a list of exported functions
 */
term module_get_exported_functions(Module *this_module, Heap *heap);

/***
 * @brief Destroys an existing Module
 *
 * @details Destroys a module and free Module resources.
 * @param module the module that will be freed.
 */
void module_destroy(Module *module);

/**
 * @brief Parse a BEAM file and returns a Module
 *
 * @details Parse a BEAM file a returns a newly allocated and initialized Module struct.
 * @param global the global context.
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
 * @param ctx the target context.
 */
term module_load_literal(Module *mod, int index, Context *ctx);

/**
 * @brief Gets type information for the given type index
 *
 * @details Loads and parses type information from the Type chunk and returns the type.
 * @param mod The module that owns the type information.
 * @param type_index a valid type index.
 * @param ctx the target context.
 */
term module_get_type_by_index(const Module *mod, int type_index, Context *ctx);

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

const struct ExportedFunction *module_resolve_function0(Module *mod, int import_table_index, struct UnresolvedFunctionCall *unresolved, GlobalContext *glb);

/**
 * @brief Get the module name, as an atom term.
 *
 * @param mod the module to get the name of
 * @return a term for the given module atom index.
 */
static inline term module_get_name(const Module *mod)
{
    // The module name always is the first atom, stored at index 1 for
    // historical reasons.
    return module_get_atom_term_by_id(mod, 1);
}

/**
 * @brief Resolves an unresolved function reference
 *
 * @details Resolves an unresolved function reference and it replaces the unresolved reference with a ModuleFunction struct,
 * also it loads the referenced module if it hasn't been loaded yet.
 * @param mod the module containing the function to resolve.
 * @param import_table_index the unresolved function index.
 * @param glb the global context
 */
static inline const struct ExportedFunction *module_resolve_function(Module *mod, int import_table_index, GlobalContext *glb)
{
    SMP_MODULE_LOCK(mod);
    // We cannot optimistically read the unresolved function call.
    // While reads of imported_funcs can happen outside the lock, read of the func
    // pointer itself cannot, as UnresolvedFunctionCall pointers are freed.
    const struct ExportedFunction *func = mod->imported_funcs[import_table_index];
    if (func->type != UnresolvedFunctionCall) {
        SMP_MODULE_UNLOCK(mod);
        return func;
    }
    struct UnresolvedFunctionCall *unresolved = EXPORTED_FUNCTION_TO_UNRESOLVED_FUNCTION_CALL(func);
    const struct ExportedFunction *result = module_resolve_function0(mod, import_table_index, unresolved, glb);
    SMP_MODULE_UNLOCK(mod);
    return result;
}

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
    int funs_count = READ_32_UNALIGNED(table_data + 8);

    if (UNLIKELY(fun_index >= funs_count)) {
        AVM_ABORT();
    }

    // fun atom index
    // arity
    // label
    // index
    uint32_t n_freeze = READ_32_UNALIGNED(table_data + fun_index * 24 + 16 + 12);
    // ouniq

    return n_freeze;
}

static inline void module_get_fun_arity_old_index_uniq(const Module *this_module, int fun_index, uint32_t *arity, uint32_t *index, uint32_t *uniq)
{
    const uint8_t *table_data = (const uint8_t *) this_module->fun_table;
    int funs_count = READ_32_UNALIGNED(table_data + 8);

    assert(fun_index < funs_count);

    // fun atom index
    *arity = READ_32_UNALIGNED(table_data + fun_index * 24 + 4 + 12);
    // label
    *index = READ_32_UNALIGNED(table_data + fun_index * 24 + 12 + 12);
    // n_freeze
    *uniq = READ_32_UNALIGNED(table_data + fun_index * 24 + 20 + 12);
}

static inline void module_get_fun(const Module *this_module, int fun_index, uint32_t *label, uint32_t *arity, uint32_t *n_freeze)
{
    const uint8_t *table_data = (const uint8_t *) this_module->fun_table;
    int funs_count = READ_32_UNALIGNED(table_data + 8);

    if (UNLIKELY(fun_index >= funs_count)) {
        AVM_ABORT();
    }

    // fun atom index
    *arity = READ_32_UNALIGNED(table_data + fun_index * 24 + 4 + 12);
    *label = READ_32_UNALIGNED(table_data + fun_index * 24 + 8 + 12);
    // index
    *n_freeze = READ_32_UNALIGNED(table_data + fun_index * 24 + 16 + 12);
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

/*
 * @brief Get the function name and arity of a function from a label.
 *
 * @details This function is used to get the function name and arity from a
 * given label inside the function. It is based the property of labels are
 * being allocated monotonously within functions.
 * @param mod the module
 * @param label the current label used to look up the function/arity
 * @param function_name (output) the function name, as an AtomString.
 * @param arity (output) the function arity
 */
bool module_get_function_from_label(Module *this_module, int label, atom_index_t *function_name, int *arity);

/**
 * @brief Get the entry point to native code for a given exported label
 *
 * @details This function is used to call native code.
 * @param module the module
 * @param exported_label label to get the native entry point to
 * @return the native entry point
 */
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label);

/*
 * @brief Insert the instruction offset for a given module at a line reference instruction.
 *
 * @details This function is used when loading a module.  When a line instruction is
 * scanned, this function is used to record the instruction offset at which the line
 * instruction occurred.
 *
 * Note that if the module (BEAM file) does not contain a Line chunk, then this function
 * is a no-op.
 *
 * @param mod the module
 * @param line_refs the list of line references to append to
 * @param line_ref the line reference (index)
 * @param offset the instruction offset at which the line instruction occurred.
 */
void module_insert_line_ref_offset(Module *mod, struct ListHead *line_refs, uint32_t line_ref, int offset);

/*
 * @brief Find the latest line reference (index) before or at which the instruction offset
 * occurs.
 *
 * @details This function searches for the most recent line instruction scanned during
 * code loading at or before the specified instruction offset.  This function is used to
 * locate the line number from a continuation pointer in a stack trace.
 *
 * @param mod the module
 * @param offset
 * @param line on output the line number or 0 for the undefined location
 * @param filename_len on output the length of the filename or NULL if it's default "module.erl"
 * @param filename on output the filename or NULL if it's module.erl. Can be NULL.
 * @return \c true if the line was found
 */
bool module_find_line(Module *mod, unsigned int offset, uint32_t *line, size_t *filename_len, const uint8_t **filename);

/**
 * @return true if the module has line information, false, otherwise.
 */
static inline bool module_has_line_chunk(Module *mod)
{
    return mod->line_refs_table != NULL;
}

/*
 * @brief Get the module, offset, label and label offset from module start from a continuation pointer.
 *
 * @param cp continuation pointer to parse
 * @param cp_mod if not null, set to found module
 * @param label if not null, set to found label
 * @param l_off if not null, set to offset of label from module start
 * @param mod_offset if not null, set to offset of cp from module start
 * @param global the global context
 */
void module_cp_to_label_offset(term cp, Module **cp_mod, int *label, int *l_off, long *mod_offset, GlobalContext *global);

/**
 * @brief Get the offset of a given label from the beginning of the code, emulated or native
 *
 * @param mod the module
 * @param label the label to get the offset of
 * @return the offset to the label code
 */
uint32_t module_label_code_offset(Module *mod, int label);

/**
 * @brief set native code for given module, typically after it is jit-compiled
 *
 * @param mod the module to set native code for
 * @param labels_count number of labels
 * @param entry_point the native entry point
 */
void module_set_native_code(Module *mod, uint32_t labels_count, ModuleNativeEntryPoint entry_point);

#ifdef __cplusplus
}
#endif

#endif
