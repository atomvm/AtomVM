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

#include "module.h"

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "externalterm.h"
#include "iff.h"
#include "nifs.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef WITH_ZLIB
#include <zlib.h>
#endif

#define LITT_UNCOMPRESSED_SIZE_OFFSET 8
#define LITT_HEADER_SIZE 12

#ifdef WITH_ZLIB
    static void *module_uncompress_literals(const uint8_t *litT, int size);
#endif
static struct LiteralEntry *module_build_literals_table(const void *literalsBuf);
static void module_add_label(Module *mod, int index, void *ptr);
static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data);
static void module_add_label(Module *mod, int index, void *ptr);

#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER

static enum ModuleLoadResult module_populate_atoms_table(Module *this_module, uint8_t *table_data)
{
    int atoms_count = READ_32_ALIGNED(table_data + 8);
    const char *current_atom = (const char *) table_data + 12;

    this_module->local_atoms_to_global_table = calloc(atoms_count + 1, sizeof(int));
    if (IS_NULL_PTR(this_module->local_atoms_to_global_table)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    const char *atom = NULL;
    for (int i = 1; i <= atoms_count; i++) {
        int atom_len = *current_atom;
        atom = current_atom;

        int global_atom_id = globalcontext_insert_atom(this_module->global, (AtomString) atom);
        if (UNLIKELY(global_atom_id < 0)) {
            fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
            return MODULE_ERROR_FAILED_ALLOCATION;
        }

        this_module->local_atoms_to_global_table[i] = global_atom_id;

        current_atom += atom_len + 1;
    }

    return MODULE_LOAD_OK;
}

static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data)
{
    int functions_count = READ_32_ALIGNED(table_data + 8);

    this_module->imported_funcs = calloc(functions_count, sizeof(void *));
    if (IS_NULL_PTR(this_module->imported_funcs)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    for (int i = 0; i < functions_count; i++) {
        int local_module_atom_index = READ_32_ALIGNED(table_data + i * 12 + 12);
        int local_function_atom_index = READ_32_ALIGNED(table_data + i * 12 + 4 + 12);
        AtomString module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index);
        AtomString function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index);
        uint32_t arity = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);

        BifImpl bif_handler = bif_registry_get_handler(module_atom, function_atom, arity);

        if (bif_handler) {
            this_module->imported_funcs[i].bif = bif_handler;
        } else {
            this_module->imported_funcs[i].func = &nifs_get(module_atom, function_atom, arity)->base;
        }

        if (!this_module->imported_funcs[i].func) {
            struct UnresolvedFunctionCall *unresolved = malloc(sizeof(struct UnresolvedFunctionCall));
            if (IS_NULL_PTR(unresolved)) {
                fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
                return MODULE_ERROR_FAILED_ALLOCATION;
            }
            unresolved->base.type = UnresolvedFunctionCall;
            unresolved->module_atom_index = this_module->local_atoms_to_global_table[local_module_atom_index];
            unresolved->function_atom_index = this_module->local_atoms_to_global_table[local_function_atom_index];
            unresolved->arity = arity;

            this_module->imported_funcs[i].func = &unresolved->base;
        }
    }

    return MODULE_LOAD_OK;
}

#ifdef ENABLE_ADVANCED_TRACE
void module_get_imported_function_module_and_name(const Module *this_module, int index, AtomString *module_atom, AtomString *function_atom)
{
    const uint8_t *table_data = (const uint8_t *) this_module->import_table;
    int functions_count = READ_32_ALIGNED(table_data + 8);

    if (UNLIKELY(index > functions_count)) {
        abort();
    }
    int local_module_atom_index = READ_32_ALIGNED(table_data + index * 12 + 12);
    int local_function_atom_index = READ_32_ALIGNED(table_data + index * 12 + 4 + 12);
    *module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index);
    *function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index);
}
#endif

uint32_t module_search_exported_function(Module *this_module, AtomString func_name, int func_arity)
{
    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    int functions_count = READ_32_ALIGNED(table_data + 8);

    for (int i = 0; i < functions_count; i++) {
        AtomString function_atom = module_get_atom_string_by_id(this_module, READ_32_ALIGNED(table_data + i * 12 + 12));
        int32_t arity = READ_32_ALIGNED(table_data + i * 12 + 4 + 12);
        if ((func_arity == arity) && atom_are_equals(func_name, function_atom)) {
            uint32_t label = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);
            return label;
        }
    }

    return 0;
}

static void module_add_label(Module *mod, int index, void *ptr)
{
    mod->labels[index] = ptr;
}

Module *module_new_from_iff_binary(GlobalContext *global, const void *iff_binary, unsigned long size)
{
    uint8_t *beam_file = (void *) iff_binary;

    unsigned long offsets[MAX_OFFS];
    unsigned long sizes[MAX_SIZES];
    scan_iff(beam_file, size, offsets, sizes);

    Module *mod = malloc(sizeof(Module));
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, "Error: Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
    memset(mod, 0, sizeof(Module));

    mod->module_index = -1;
    mod->global = global;

    if (UNLIKELY(module_populate_atoms_table(mod, beam_file + offsets[AT8U]) != MODULE_LOAD_OK)) {
        fprintf(stderr, "Error: Failed to populate atoms table: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

    if (UNLIKELY(module_build_imported_functions_table(mod, beam_file + offsets[IMPT]) != MODULE_LOAD_OK)) {
        fprintf(stderr, "Error: Failed to build imported functions table: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

#ifdef ENABLE_ADVANCED_TRACE
    mod->import_table = beam_file + offsets[IMPT];
#endif
    mod->code = (CodeChunk *) (beam_file + offsets[CODE]);
    mod->export_table = beam_file + offsets[EXPT];
    mod->atom_table = beam_file + offsets[AT8U];
    mod->fun_table = beam_file + offsets[FUNT];
    mod->str_table = beam_file + offsets[STRT];
    mod->str_table_len = sizes[STRT];
    mod->labels = calloc(ENDIAN_SWAP_32(mod->code->labels), sizeof(void *));
    if (IS_NULL_PTR(mod->labels)) {
        fprintf(stderr, "Error: Null module labels: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

    if (offsets[LITT]) {
        #ifdef WITH_ZLIB
            mod->literals_data = module_uncompress_literals(beam_file + offsets[LITT], sizes[LITT]);
            if (IS_NULL_PTR(mod->literals_data)) {
                module_destroy(mod);
                return NULL;
            }
        #else
            fprintf(stderr, "Error: zlib required to uncompress literals.\n");
            module_destroy(mod);
            return NULL;
        #endif

        mod->literals_table = module_build_literals_table(mod->literals_data);
        mod->free_literals_data = 1;

    } else if (offsets[LITU]) {
        mod->literals_data = beam_file + offsets[LITU] + IFF_SECTION_HEADER_SIZE;
        mod->literals_table = module_build_literals_table(mod->literals_data);
        mod->free_literals_data = 0;

    } else {
        mod->literals_data = NULL;
        mod->literals_table = NULL;
        mod->free_literals_data = 0;
    }

    mod->end_instruction_ii = read_core_chunk(mod);

    return mod;
}

COLD_FUNC void module_destroy(Module *module)
{
    free(module->labels);
    free(module->imported_funcs);
    free(module->literals_table);
    if (module->free_literals_data) {
        free(module->literals_data);
    }
    free(module);
}

#ifdef WITH_ZLIB
static void *module_uncompress_literals(const uint8_t *litT, int size)
{
    unsigned int required_buf_size = READ_32_ALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);

    uint8_t *outBuf = malloc(required_buf_size);
    if (IS_NULL_PTR(outBuf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    z_stream infstream;
    infstream.zalloc = Z_NULL;
    infstream.zfree = Z_NULL;
    infstream.opaque = Z_NULL;
    infstream.avail_in = (uInt) (size - IFF_SECTION_HEADER_SIZE);
    infstream.next_in = (Bytef *) (litT + LITT_HEADER_SIZE);
    infstream.avail_out = (uInt) required_buf_size;
    infstream.next_out = (Bytef *) outBuf;

    int ret = inflateInit(&infstream);
    if (ret != Z_OK) {
        fprintf(stderr, "Failed inflateInit\n");
        return NULL;
    }
    ret = inflate(&infstream, Z_NO_FLUSH);
    if (ret != Z_OK) {
        fprintf(stderr, "Failed inflate\n");
        return NULL;
    }
    inflateEnd(&infstream);

    return outBuf;
}
#endif

static struct LiteralEntry *module_build_literals_table(const void *literalsBuf)
{
    uint32_t terms_count = READ_32_ALIGNED(literalsBuf);

    const uint8_t *pos = (const uint8_t *) literalsBuf + sizeof(uint32_t);

    struct LiteralEntry *literals_table = calloc(terms_count, sizeof(struct LiteralEntry));
    if (IS_NULL_PTR(literals_table)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
    for (uint32_t i = 0; i < terms_count; i++) {
        uint32_t term_size = READ_32_UNALIGNED(pos);
        literals_table[i].size = term_size;
        literals_table[i].data = pos + sizeof(uint32_t);

        pos += term_size + sizeof(uint32_t);
    }

    return literals_table;
}

term module_load_literal(Module *mod, int index, Context *ctx)
{
    term t = externalterm_to_term(mod->literals_table[index].data, mod->literals_table[index].size, ctx, 1);
    if (term_is_invalid_term(t)) {
        fprintf(stderr, "Invalid term reading literals_table[%i] from module\n", index);
        abort();
    }
    return t;
}

const struct ExportedFunction *module_resolve_function(Module *mod, int import_table_index)
{
    struct ExportedFunction *func = (struct ExportedFunction *) mod->imported_funcs[import_table_index].func;
    struct UnresolvedFunctionCall *unresolved = EXPORTED_FUNCTION_TO_UNRESOLVED_FUNCTION_CALL(func);

    AtomString module_name_atom = (AtomString) valueshashtable_get_value(mod->global->atoms_ids_table, unresolved->module_atom_index, (unsigned long) NULL);
    AtomString function_name_atom = (AtomString) valueshashtable_get_value(mod->global->atoms_ids_table, unresolved->function_atom_index, (unsigned long) NULL);
    int arity = unresolved->arity;

    Module *found_module = globalcontext_get_module(mod->global, module_name_atom);

    if (LIKELY(found_module != NULL)) {
        int exported_label = module_search_exported_function(found_module, function_name_atom, arity);
        if (exported_label == 0) {
            char buf[256];
            atom_write_mfa(buf, 256, module_name_atom, function_name_atom, arity);
            fprintf(stderr, "Warning: function %s cannot be resolved.\n", buf);
            return NULL;
        }
        struct ModuleFunction *mfunc = malloc(sizeof(struct ModuleFunction));
        if (IS_NULL_PTR(mfunc)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return NULL;
        }
        mfunc->base.type = ModuleFunction;
        mfunc->target = found_module;
        mfunc->label = exported_label;

        free(unresolved);
        mod->imported_funcs[import_table_index].func = &mfunc->base;
        return &mfunc->base;
    } else {
        char buf[256];
        atom_string_to_c(module_name_atom, buf, 256);
        fprintf(stderr, "Warning: module %s cannot be resolved.\n", buf);
        return NULL;
    }
}
