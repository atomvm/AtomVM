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
#include "atom_table.h"
#include "bif.h"
#include "context.h"
#include "externalterm.h"
#include "globalcontext.h"
#include "iff.h"
#include "jit.h"
#include "list.h"
#include "nifs.h"
#include "term.h"
#include "utils.h"
#include "sys.h"
#include "smp.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef WITH_ZLIB
#include <zlib.h>
#endif

// BEAM Type constants from OTP source code:
// /opt/src/otp/lib/compiler/src/beam_types.erl lines 1446-1461
#define BEAM_TYPE_ATOM          (1 << 0)
#define BEAM_TYPE_BITSTRING     (1 << 1)
#define BEAM_TYPE_CONS          (1 << 2)
#define BEAM_TYPE_FLOAT         (1 << 3)
#define BEAM_TYPE_FUN           (1 << 4)
#define BEAM_TYPE_INTEGER       (1 << 5)
#define BEAM_TYPE_MAP           (1 << 6)
#define BEAM_TYPE_NIL           (1 << 7)
#define BEAM_TYPE_PID           (1 << 8)
#define BEAM_TYPE_PORT          (1 << 9)
#define BEAM_TYPE_REFERENCE     (1 << 10)
#define BEAM_TYPE_TUPLE         (1 << 11)

#define BEAM_TYPE_HAS_LOWER_BOUND (1 << 12)
#define BEAM_TYPE_HAS_UPPER_BOUND (1 << 13)
#define BEAM_TYPE_HAS_UNIT        (1 << 14)

// BEAM Types version from OTP source code:
// /opt/src/otp/lib/compiler/src/beam_types.hrl line 22
#define BEAM_TYPES_VERSION 3

#define LITT_UNCOMPRESSED_SIZE_OFFSET 8
#define LITT_HEADER_SIZE 12

#define CHECK_FREE_SPACE(space, error)           \
    if ((size_t) ((pos + space) - data) > len) { \
        fprintf(stderr, error);                  \
        return;                                  \
    }

static bool module_are_literals_compressed(const uint8_t *litT);
#ifdef WITH_ZLIB
    static void *module_uncompress_literals(const uint8_t *litT, int size);
#endif
static struct LiteralEntry *module_build_literals_table(const void *literalsBuf);
#ifndef AVM_NO_EMU
static void module_add_label(Module *mod, int index, const uint8_t *ptr);
#endif
static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data, GlobalContext *glb);
static void module_parse_line_table(Module *mod, const uint8_t *data, size_t len);

struct LineRefOffset
{
    struct ListHead head;
    unsigned int offset;
};

#ifndef AVM_NO_EMU
#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER
#endif

static enum ModuleLoadResult module_populate_atoms_table(Module *this_module, uint8_t *table_data, GlobalContext *glb)
{
    int atoms_count = READ_32_UNALIGNED(table_data + 8);

    enum EnsureAtomsOpt ensure_opts = EnsureAtomsNoOpts;
    if (atoms_count < 0) {
        ensure_opts = EnsureLongEncoding;
        atoms_count = -atoms_count;
    }

    const char *current_atom = (const char *) table_data + 12;

    this_module->local_atoms_to_global_table = calloc(atoms_count + 1, sizeof(int));
    if (IS_NULL_PTR(this_module->local_atoms_to_global_table)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    enum AtomTableEnsureAtomResult ensure_result = atom_table_ensure_atoms(
        glb->atom_table, current_atom, atoms_count, this_module->local_atoms_to_global_table + 1, ensure_opts);
    switch (ensure_result) {
        case AtomTableEnsureAtomAllocFail: {
            fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
            return MODULE_ERROR_FAILED_ALLOCATION;
        }
        case AtomTableEnsureAtomInvalidLen:
        case AtomTableEnsureAtomNotFound: {
            return MODULE_ERROR_INVALID;
        }
        case AtomTableEnsureAtomOk: {
            return MODULE_LOAD_OK;
        }
        default:
            UNREACHABLE();
    }
}

static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data, GlobalContext *glb)
{
    int functions_count = READ_32_UNALIGNED(table_data + 8);

    this_module->imported_funcs = calloc(functions_count, sizeof(struct ExportedFunction *));
    if (IS_NULL_PTR(this_module->imported_funcs)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    for (int i = 0; i < functions_count; i++) {
        int local_module_atom_index = READ_32_UNALIGNED(table_data + i * 12 + 12);
        int local_function_atom_index = READ_32_UNALIGNED(table_data + i * 12 + 4 + 12);
        atom_index_t module_atom = this_module->local_atoms_to_global_table[local_module_atom_index];
        atom_index_t function_atom = this_module->local_atoms_to_global_table[local_function_atom_index];
        uint32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 8 + 12);
        char mfa[MAX_MFA_NAME_LEN];
        atom_table_write_mfa(glb->atom_table, mfa, sizeof(mfa), module_atom, function_atom, arity);

        const struct ExportedFunction *bif = bif_registry_get_handler(mfa);

        if (bif) {
            this_module->imported_funcs[i] = bif;
        } else {
            this_module->imported_funcs[i] = &nifs_get(mfa)->base;
        }

        if (!this_module->imported_funcs[i]) {
            struct UnresolvedFunctionCall *unresolved = malloc(sizeof(struct UnresolvedFunctionCall));
            if (IS_NULL_PTR(unresolved)) {
                fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
                return MODULE_ERROR_FAILED_ALLOCATION;
            }
            unresolved->base.type = UnresolvedFunctionCall;
            unresolved->module_atom_index = this_module->local_atoms_to_global_table[local_module_atom_index];
            unresolved->function_atom_index = this_module->local_atoms_to_global_table[local_function_atom_index];
            unresolved->arity = arity;

            this_module->imported_funcs[i] = &unresolved->base;
        }
    }

    return MODULE_LOAD_OK;
}

#ifdef ENABLE_ADVANCED_TRACE
void module_get_imported_function_module_and_name(const Module *this_module, int index, AtomString *module_atom, AtomString *function_atom, GlobalContext *glb)
{
    const uint8_t *table_data = (const uint8_t *) this_module->import_table;
    int functions_count = READ_32_UNALIGNED(table_data + 8);

    if (UNLIKELY(index > functions_count)) {
        AVM_ABORT();
    }
    int local_module_atom_index = READ_32_UNALIGNED(table_data + index * 12 + 12);
    int local_function_atom_index = READ_32_UNALIGNED(table_data + index * 12 + 4 + 12);
    *module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index, glb);
    *function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index, glb);
}
#endif

bool module_get_function_from_label(Module *this_module, int label, atom_index_t *function_name, int *arity)
{
    int best_label = -1;
    const uint8_t *export_table_data = (const uint8_t *) this_module->export_table;
    int exports_count = READ_32_UNALIGNED(export_table_data + 8);
    for (int export_index = exports_count - 1; export_index >= 0; export_index--) {
        int fun_atom_index = READ_32_UNALIGNED(export_table_data + (export_index * 12) + 12);
        int fun_arity = READ_32_UNALIGNED(export_table_data + (export_index * 12) + 4 + 12);
        int fun_label = READ_32_UNALIGNED(export_table_data + (export_index * 12) + 8 + 12);
        if (fun_label <= label && best_label < fun_label) {
            best_label = fun_label;
            *arity = fun_arity;
            *function_name = this_module->local_atoms_to_global_table[fun_atom_index];
        }
    }

    const uint8_t *local_table_data = (const uint8_t *) this_module->local_table;
    int locals_count = READ_32_UNALIGNED(local_table_data + 8);
    for (int local_index = locals_count - 1; local_index >= 0; local_index--) {
        int fun_atom_index = READ_32_UNALIGNED(local_table_data + (local_index * 12) + 12);
        int fun_arity = READ_32_UNALIGNED(local_table_data + (local_index * 12) + 4 + 12);
        int fun_label = READ_32_UNALIGNED(local_table_data + (local_index * 12) + 8 + 12);
        if (fun_label <= label && best_label < fun_label) {
            best_label = fun_label;
            *arity = fun_arity;
            *function_name = this_module->local_atoms_to_global_table[fun_atom_index];
        }
    }
    if (UNLIKELY(best_label == -1)) {
        // Couldn't find the function.
        return false;
    }
    return true;
}

size_t module_get_exported_functions_count(Module *this_module)
{
    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    size_t functions_count = READ_32_UNALIGNED(table_data + 8);
    return functions_count;
}

uint32_t module_search_exported_function(Module *this_module, atom_index_t func_name, int func_arity)
{
    size_t functions_count = module_get_exported_functions_count(this_module);

    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    for (unsigned int i = 0; i < functions_count; i++) {
        int local_atom_id = READ_32_UNALIGNED(table_data + i * 12 + 12);
        int32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 4 + 12);
        if (func_arity == arity) {
            atom_index_t function_atom_ix = this_module->local_atoms_to_global_table[local_atom_id];
            if (func_name == function_atom_ix) {
                uint32_t label = READ_32_UNALIGNED(table_data + i * 12 + 8 + 12);
                return label;
            }
        }
    }

    return 0;
}

term module_get_exported_functions(Module *this_module, Heap *heap)
{
    size_t functions_count = module_get_exported_functions_count(this_module);
    term result_list = term_nil();

    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    for (unsigned int i = 0; i < functions_count; i++) {
        int local_atom_id = READ_32_UNALIGNED(table_data + i * 12 + 12);
        atom_index_t function_atom_ix = this_module->local_atoms_to_global_table[local_atom_id];
        int32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 4 + 12);
        term function_tuple = term_alloc_tuple(2, heap);
        term_put_tuple_element(function_tuple, 0, term_from_atom_index(function_atom_ix));
        term_put_tuple_element(function_tuple, 1, term_from_int(arity));
        result_list = term_list_prepend(function_tuple, result_list, heap);
    }
    return result_list;
}

#ifndef AVM_NO_EMU
static void module_add_label(Module *mod, int index, const uint8_t *ptr)
{
    mod->labels[index] = ptr;
}
#endif

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

#ifndef AVM_NO_SMP
    mod->mutex = smp_mutex_create();
#endif

    if (UNLIKELY(module_populate_atoms_table(mod, beam_file + offsets[AT8U], global) != MODULE_LOAD_OK)) {
        fprintf(stderr, "Error: Failed to populate atoms table: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

    if (UNLIKELY(module_build_imported_functions_table(mod, beam_file + offsets[IMPT], global) != MODULE_LOAD_OK)) {
        fprintf(stderr, "Error: Failed to build imported functions table: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

#ifdef ENABLE_ADVANCED_TRACE
    mod->import_table = beam_file + offsets[IMPT];
#endif
    if (offsets[CODE]) {
        mod->code = (CodeChunk *) (beam_file + offsets[CODE]);
    }
    mod->export_table = beam_file + offsets[EXPT];
    mod->local_table = beam_file + offsets[LOCT];
    mod->atom_table = beam_file + offsets[AT8U];
    mod->fun_table = beam_file + offsets[FUNT];
    mod->str_table = beam_file + offsets[STRT];
    mod->str_table_len = sizes[STRT];
#ifndef AVM_NO_JIT
    if (offsets[AVMN]) {
        NativeCodeChunk *native_code = (NativeCodeChunk *) (beam_file + offsets[AVMN]);
        // Check compatibility
        if (ENDIAN_SWAP_16(native_code->version) != JIT_FORMAT_VERSION) {
            fprintf(stderr, "Unknown native code chunk version (%d)\n", ENDIAN_SWAP_16(native_code->version));
        } else {
            for (int arch_index = 0; arch_index < ENDIAN_SWAP_16(native_code->architectures_count); arch_index++) {
                if (ENDIAN_SWAP_16(native_code->architectures[arch_index].architecture) == JIT_ARCH_TARGET && ENDIAN_SWAP_16(native_code->architectures[arch_index].variant) == JIT_VARIANT_PIC) {
                    size_t offset = ENDIAN_SWAP_32(native_code->info_size) + ENDIAN_SWAP_32(native_code->architectures[arch_index].offset) + sizeof(native_code->info_size);
                    ModuleNativeEntryPoint module_entry_point = sys_map_native_code((const uint8_t *) &native_code->info_size, ENDIAN_SWAP_32(native_code->size), offset);
                    module_set_native_code(mod, ENDIAN_SWAP_32(native_code->labels), module_entry_point);
                    break;
                }
            }
            if (mod->native_code == NULL) {
                fprintf(stderr, "Native code chunk found but no compatible architecture or variant found\n");
            }
        }
    }
#endif

#if !defined(AVM_NO_JIT) && !defined(AVM_NO_EMU)
    if (mod->native_code == NULL) {
#endif
#ifndef AVM_NO_EMU
        uint32_t num_labels = ENDIAN_SWAP_32(mod->code->labels);
        mod->labels = calloc(num_labels, sizeof(void *));
        if (IS_NULL_PTR(mod->labels)) {
            fprintf(stderr, "Error: Null module labels: %s:%i.\n", __FILE__, __LINE__);
            module_destroy(mod);
            return NULL;
        }
#endif
#if !defined(AVM_NO_JIT) && !defined(AVM_NO_EMU)
    }
#endif

    module_parse_line_table(mod, beam_file + offsets[LINT] + 8, sizes[LINT]);

    if (offsets[LITT]) {
        if (!module_are_literals_compressed(beam_file + offsets[LITT])) {
            mod->literals_data = beam_file + offsets[LITT] + LITT_HEADER_SIZE;
            mod->free_literals_data = 0;

        } else {
            #ifdef WITH_ZLIB
                mod->literals_data = module_uncompress_literals(beam_file + offsets[LITT], sizes[LITT]);
                if (IS_NULL_PTR(mod->literals_data)) {
                    module_destroy(mod);
                    return NULL;
                }
                mod->free_literals_data = 1;
            #else
                fprintf(stderr, "Error: zlib required to uncompress literals.\n");
                module_destroy(mod);
                return NULL;
            #endif
        }

        mod->literals_table = module_build_literals_table(mod->literals_data);

    } else if (offsets[LITU]) {
        mod->literals_data = beam_file + offsets[LITU] + IFF_SECTION_HEADER_SIZE;
        mod->literals_table = module_build_literals_table(mod->literals_data);
        mod->free_literals_data = 0;

    } else {
        mod->literals_data = NULL;
        mod->literals_table = NULL;
        mod->free_literals_data = 0;
    }

    if (offsets[TYPE]) {
        mod->types_data = beam_file + offsets[TYPE] + IFF_SECTION_HEADER_SIZE;
    } else {
        mod->types_data = NULL;
    }

#ifndef AVM_NO_JIT
    if (mod->native_code == NULL) {
#endif
#ifndef AVM_NO_EMU
        struct ListHead line_refs;
        list_init(&line_refs);
        mod->end_instruction_ii = read_core_chunk(mod, &line_refs);

        // Create the list of offsets if the module has line informations.
        if (mod->line_refs_table != NULL) {
            // Compute the size of the list
            size_t num_offsets = 0;
            struct ListHead *item = line_refs.next;
            while (item != &line_refs) {
                num_offsets++;
                item = item->next;
            }
            if (num_offsets > 0) {
                mod->line_refs_offsets = malloc(num_offsets * sizeof(unsigned int));
                if (IS_NULL_PTR(mod->line_refs_offsets)) {
                    fprintf(stderr, "Warning: Unable to allocate space for line refs offset, module has %zu offsets.  Line information in stacktraces may be missing\n", num_offsets);
                } else {
                    size_t index = 0;
                    item = line_refs.next;
                    while (item != &line_refs) {
                        struct LineRefOffset *offset = CONTAINER_OF(item, struct LineRefOffset, head);
                        mod->line_refs_offsets[index] = offset->offset;
                        index++;
                        item = item->next;
                    }
                    mod->line_refs_offsets_count = num_offsets;
                }
            }
        }
        // Empty the list
        while (!list_is_empty(&line_refs)) {
            struct ListHead *item = line_refs.next;
            list_remove(item);
            free(item);
        }
#endif
#ifndef AVM_NO_JIT
    }
#endif

    return mod;
}

COLD_FUNC void module_destroy(Module *module)
{
    free(module->labels);
    free(module->imported_funcs);
    free(module->literals_table);
    free(module->local_atoms_to_global_table);
    free(module->line_refs_offsets);
    if (module->free_literals_data) {
        free(module->literals_data);
    }
#ifndef AVM_NO_SMP
    smp_mutex_destroy(module->mutex);
#endif
    free(module);
}

static bool module_are_literals_compressed(const uint8_t *litT)
{
    uint32_t required_buf_size = READ_32_UNALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);
    return (required_buf_size != 0);
}

#ifdef WITH_ZLIB
static void *module_uncompress_literals(const uint8_t *litT, int size)
{
    unsigned int required_buf_size = READ_32_UNALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);

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
    uint32_t terms_count = READ_32_UNALIGNED(literalsBuf);

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
    term t = externalterm_from_const_literal(mod->literals_table[index].data, mod->literals_table[index].size, ctx);
    if (UNLIKELY(term_is_invalid_term(t))) {
        fprintf(stderr, "Either OOM or invalid term while reading literals_table[%i] from module\n", index);
    }
    return t;
}

term module_get_type_by_index(const Module *mod, int type_index, Context *ctx)
{
    if (IS_NULL_PTR(mod->types_data)) {
        // No Type chunk available, return 'any'
        return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
    }

    const uint8_t *types_data = (const uint8_t *) mod->types_data;

    // Parse Type chunk header: Version:32, Count:32
    uint32_t version = READ_32_UNALIGNED(types_data);
    uint32_t count = READ_32_UNALIGNED(types_data + 4);

    // Check if version is supported
    if (version != BEAM_TYPES_VERSION) {
        return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
    }

    // Check bounds
    if (type_index >= (int) count) {
        return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
    }

    // Skip to type data
    const uint8_t *type_entries = types_data + 8;
    const uint8_t *pos = type_entries;

    // Skip to the requested type index
    for (int i = 0; i < type_index; i++) {
        uint16_t type_bits = READ_16_UNALIGNED(pos);
        pos += 2;

        // Skip extra data if present
        if (type_bits & BEAM_TYPE_HAS_LOWER_BOUND) pos += 8;
        if (type_bits & BEAM_TYPE_HAS_UPPER_BOUND) pos += 8;
        if (type_bits & BEAM_TYPE_HAS_UNIT) pos += 1;
    }

    // Read the target type
    uint16_t type_bits = READ_16_UNALIGNED(pos);
    pos += 2;

    // Parse extra data for bounds and unit
    int64_t lower_bound = INT64_MIN;
    int64_t upper_bound = INT64_MAX;
    uint8_t unit = 1;
    bool has_lower = false;
    bool has_upper = false;

    if (type_bits & BEAM_TYPE_HAS_LOWER_BOUND) {
        lower_bound = (int64_t) READ_64_UNALIGNED(pos);
        pos += 8;
        has_lower = true;
    }
    if (type_bits & BEAM_TYPE_HAS_UPPER_BOUND) {
        upper_bound = (int64_t) READ_64_UNALIGNED(pos);
        pos += 8;
        has_upper = true;
    }
    if (type_bits & BEAM_TYPE_HAS_UNIT) {
        unit = *pos + 1; // Stored as unit-1
        pos += 1;
    }

    // Decode type based on TypeBits (matching jit_precompile.erl exact pattern matching)
    // From OTP source code: /opt/src/otp/lib/compiler/src/beam_types.erl decode_type function
    uint16_t type_pattern = type_bits & 0xFFF; // Mask out flags, keep type bits

    switch (type_pattern) {
        case BEAM_TYPE_ATOM:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "t_atom"));

        case BEAM_TYPE_BITSTRING:
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
            }
            term type_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(type_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\xE", "t_bs_matchable")));
            term_put_tuple_element(type_tuple, 1, term_from_int32(unit));
            return type_tuple;

        case BEAM_TYPE_CONS:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "t_cons"));

        case BEAM_TYPE_FLOAT:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "t_float"));

        case BEAM_TYPE_FUN:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "t_fun"));

        case (BEAM_TYPE_FLOAT | BEAM_TYPE_INTEGER):
            // {t_number, {LowerBound, UpperBound}}
            if (has_lower || has_upper) {
                size_t heap_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
                if (has_lower && (lower_bound < MIN_NOT_BOXED_INT || lower_bound > MAX_NOT_BOXED_INT)) {
                    heap_size += BOXED_INT64_SIZE;
                }
                if (has_upper && (upper_bound < MIN_NOT_BOXED_INT || upper_bound > MAX_NOT_BOXED_INT)) {
                    heap_size += BOXED_INT64_SIZE;
                }
                if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                    return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
                }
                term bounds_tuple = term_alloc_tuple(2, &ctx->heap);

                if (has_lower) {
                    term_put_tuple_element(bounds_tuple, 0, term_make_maybe_boxed_int64(lower_bound, &ctx->heap));
                } else {
                    term_put_tuple_element(bounds_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "-inf")));
                }

                if (has_upper) {
                    term_put_tuple_element(bounds_tuple, 1, term_make_maybe_boxed_int64(upper_bound, &ctx->heap));
                } else {
                    term_put_tuple_element(bounds_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "+inf")));
                }

                term type_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(type_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "t_number")));
                term_put_tuple_element(type_tuple, 1, bounds_tuple);
                return type_tuple;
            }
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "t_number"));

        case BEAM_TYPE_INTEGER:
            if (has_lower || has_upper) {
                size_t heap_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
                if (has_lower && (lower_bound < MIN_NOT_BOXED_INT || lower_bound > MAX_NOT_BOXED_INT)) {
                    heap_size += BOXED_INT64_SIZE;
                }
                if (has_upper && (upper_bound < MIN_NOT_BOXED_INT || upper_bound > MAX_NOT_BOXED_INT)) {
                    heap_size += BOXED_INT64_SIZE;
                }
                if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
                    return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
                }
                term bounds_tuple = term_alloc_tuple(2, &ctx->heap);

                if (has_lower) {
                    term_put_tuple_element(bounds_tuple, 0, term_make_maybe_boxed_int64(lower_bound, &ctx->heap));
                } else {
                    term_put_tuple_element(bounds_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "-inf")));
                }

                if (has_upper) {
                    term_put_tuple_element(bounds_tuple, 1, term_make_maybe_boxed_int64(upper_bound, &ctx->heap));
                } else {
                    term_put_tuple_element(bounds_tuple, 1, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "+inf")));
                }

                term type_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(type_tuple, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "t_integer")));
                term_put_tuple_element(type_tuple, 1, bounds_tuple);
                return type_tuple;
            }
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "t_integer"));

        case BEAM_TYPE_MAP:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "t_map"));

        case BEAM_TYPE_NIL:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "nil"));

        case (BEAM_TYPE_NIL | BEAM_TYPE_CONS):
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "t_list"));

        case BEAM_TYPE_PID:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "pid"));

        case BEAM_TYPE_PORT:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "port"));

        case BEAM_TYPE_REFERENCE:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "reference"));

        case BEAM_TYPE_TUPLE:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "t_tuple"));

        default:
            // Default fallback for any other combination or union types
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "any"));
    }
}

#ifndef AVM_NO_JIT
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label)
{
    assert(module->native_code);
    return (ModuleNativeEntryPoint) (((const uint8_t *) module->native_code) + JIT_JUMPTABLE_ENTRY_SIZE * exported_label);
}
#endif

static const struct ExportedFunction *module_create_function(Module *found_module, int exported_label)
{
#ifndef AVM_NO_JIT
    if (found_module->native_code) {
        struct ModuleFunction *mfunc = malloc(sizeof(struct ModuleFunction));
        if (IS_NULL_PTR(mfunc)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return NULL;
        }
        mfunc->base.type = ModuleNativeFunction;
        mfunc->target = found_module;
        mfunc->entry_point = module_get_native_entry_point(found_module, exported_label);

        return &mfunc->base;
    } else {
#endif
        struct ModuleFunction *mfunc = malloc(sizeof(struct ModuleFunction));
        if (IS_NULL_PTR(mfunc)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            return NULL;
        }
        mfunc->base.type = ModuleFunction;
        mfunc->target = found_module;
        mfunc->label = exported_label;

        return &mfunc->base;
#ifndef AVM_NO_JIT
    }
#endif
}

const struct ExportedFunction *module_resolve_function0(Module *mod, int import_table_index, struct UnresolvedFunctionCall *unresolved, GlobalContext *glb)
{
    Module *found_module = globalcontext_get_module(glb, unresolved->module_atom_index);

    if (LIKELY(found_module != NULL)) {
        int exported_label = module_search_exported_function(found_module, unresolved->function_atom_index, unresolved->arity);
        if (exported_label == 0) {
            char buf[MAX_MFA_NAME_LEN];
            atom_table_write_mfa(glb->atom_table, buf, MAX_MFA_NAME_LEN, unresolved->module_atom_index, unresolved->function_atom_index, unresolved->arity);
            fprintf(stderr, "Warning: function %s cannot be resolved.\n", buf);
            return NULL;
        }
        const struct ExportedFunction *exported_function = module_create_function(found_module, exported_label);
        if (IS_NULL_PTR(exported_function)) {
            return NULL;
        }
        mod->imported_funcs[import_table_index] = exported_function;
        free(unresolved);
        return exported_function;
    } else {
        size_t atom_string_len;
        const uint8_t *atom_string_data = atom_table_get_atom_string(glb->atom_table, unresolved->module_atom_index, &atom_string_len);
        fprintf(stderr, "Warning: module %.*s cannot be resolved.\n", (int) atom_string_len, atom_string_data);
        return NULL;
    }
}

static bool module_check_line_refs(Module *mod, const uint8_t **data, size_t len)
{
    // assert pos >= *data
    const uint8_t *pos = *data;
    size_t i = 0;
    while (i < mod->line_refs_count) {
        if ((size_t) (pos - *data) > len) {
            fprintf(stderr, "Invalid line_ref: expected tag.\n");
            return false;
        }
        uint8_t tag = *pos;
        switch (tag & 0x0F) {
            case COMPACT_INTEGER: {
                ++i;
                ++pos;
                break;
            }
            case COMPACT_LARGE_INTEGER: {
                ++pos;
                switch (tag & COMPACT_LARGE_IMM_MASK) {
                    case COMPACT_11BITS_VALUE: {
                        ++pos;
                        break;
                    }
                    case COMPACT_NBITS_VALUE: {
                        int sz = (tag >> 5) + 2;
                        if (UNLIKELY(sz > 4)) {
                            fprintf(stderr, "Invalid line_ref: expected extended int with sz <= 4 (line number <= 2^31)");
                            return false;
                        }
                        pos += sz;
                        break;
                    }
                    default:
                        fprintf(stderr, "Invalid line_ref: expected extended int -- tag = %u", (unsigned int) tag);
                        return false;
                }
                if ((size_t) (pos - *data) > len) {
                    fprintf(stderr, "Invalid line_ref: expected extended int.\n");
                    return false;
                }
                ++i;
                break;
            }
            case COMPACT_ATOM: {
                uint16_t location_ix = ((tag & 0xF0) >> 4);
                if (location_ix > mod->locations_count) {
                    fprintf(stderr, "Invalid line_ref: location_ix = %d is greater than locations_count = %d.\n", (int) location_ix, (int) mod->locations_count);
                    return false;
                }
                ++pos;
                break;
            }
            case COMPACT_LARGE_ATOM: {
                // We don't support more than 11bits (2048) locations.
                if (UNLIKELY((tag & COMPACT_LARGE_IMM_MASK) != COMPACT_11BITS_VALUE)) {
                    fprintf(stderr, "Invalid line_ref: location_ix is larger than 2048.\n");
                    return false;
                }
                uint16_t high_order_3_bits = (tag & 0xE0);
                ++pos;
                if ((size_t) (pos - *data) > len) {
                    fprintf(stderr, "Invalid line_ref: expected extended atom.\n");
                    return false;
                }
                uint8_t next_byte = *pos;
                uint16_t location_ix = ((high_order_3_bits << 3) | next_byte);
                if (location_ix > mod->locations_count) {
                    fprintf(stderr, "Invalid line_ref: location_ix = %d is greater than locations_count = %d.\n", (int) location_ix, (int) mod->locations_count);
                    return false;
                }
                ++pos;
                break;
            }
            default:
                fprintf(stderr, "Unsupported line_ref tag: %u\n", tag);
                return false;
        }
    }

    *data = pos;
    return true;
}

static bool module_check_locations(Module *mod, const uint8_t *data, size_t len)
{
    const uint8_t *pos = data;
    for (size_t i = 1; i <= mod->locations_count; i++) {
        if ((size_t) ((pos + 2) - data) > len) {
            fprintf(stderr, "Invalid filename: expected 16-bit size.\n");
            return false;
        }
        uint16_t size = READ_16_UNALIGNED(pos);
        pos += 2;
        if ((size_t) ((pos + size) - data) > len) {
            fprintf(stderr, "Invalid filename: expected filename data (%u bytes).\n", size);
            return false;
        }
        pos += size;
    }

    return true;
}

static bool module_get_line_ref(Module *mod, uint16_t line_ref, uint32_t *out_line, uint16_t *out_location)
{
    // First is undefined
    if (line_ref == 0) {
        *out_line = 0;
        *out_location = 0;
        return true;
    }

    const uint8_t *pos = mod->line_refs_table;
    uint16_t location_ix = 0;
    size_t i = 1;
    while (i <= mod->line_refs_count) {
        uint8_t tag = *pos;
        switch (tag & 0x0F) {
            case COMPACT_INTEGER: {
                if (i == line_ref) {
                    uint32_t line_idx = ((tag & 0xF0) >> 4);
                    *out_line = line_idx;
                    *out_location = location_ix;
                    return true;
                }
                ++i;
                ++pos;
                break;
            }
            case COMPACT_LARGE_INTEGER: {
                uint32_t line_idx;
                switch (tag & COMPACT_LARGE_IMM_MASK) {
                    case COMPACT_11BITS_VALUE: {
                        uint16_t high_order_3_bits = (tag & 0xE0);
                        line_idx = ((high_order_3_bits << 3) | pos[1]);
                        pos += 2;
                        break;
                    }
                    case COMPACT_NBITS_VALUE: {
                        pos++;
                        int sz = (tag >> 5) + 2;
                        line_idx = 0;
                        for (int i = 0; i < sz; i++) {
                            line_idx = line_idx * 256 + pos[i];
                        }
                        pos += sz;
                        break;
                    }
                    default:
                        UNREACHABLE();
                }
                if (i == line_ref) {
                    *out_line = line_idx;
                    *out_location = location_ix;
                    return true;
                }
                ++i;
                break;
            }
            case COMPACT_ATOM: {
                location_ix = ((tag & 0xF0) >> 4);
                ++pos;
                break;
            }
            case COMPACT_LARGE_ATOM: {
                uint16_t high_order_3_bits = (tag & 0xE0);
                location_ix = ((high_order_3_bits << 3) | pos[1]);
                pos += 2;
                break;
            }
            default:
                UNREACHABLE();
        }
    }

    return false;
}


static bool module_get_location(Module *mod, uint16_t location_ix, size_t *filename_len, const uint8_t **filename)
{
    // 0 is module.erl
    if (location_ix == 0) {
        *filename_len = 0;
        if (filename) {
            *filename = NULL;
        }
        return true;
    }

    const uint8_t *pos = mod->locations_table;
    for (size_t i = 1; i <= mod->locations_count; i++) {
        uint16_t size = READ_16_UNALIGNED(pos);
        pos +=2;
        if (i == location_ix) {
            *filename_len = size;
            if (filename) {
                *filename = pos;
            }
            return true;
        }
        pos += size;
    }

    return false;
}

static void module_parse_line_table(Module *mod, const uint8_t *data, size_t len)
{
    if (len == 0) {
        mod->line_refs_count = 0;
        mod->line_refs_table = NULL;
        mod->locations_count = 0;
        mod->locations_table = NULL;
        return;
    }

    const uint8_t *pos = data;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: version\n");
    uint32_t version = READ_32_UNALIGNED(pos);
    if (UNLIKELY(version != 0)) {
        fprintf(stderr, "Warning: Unsupported line version %" PRIu32 ". Line information in stacktraces may be missing\n", version);
        return;
    }
    pos += 4;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: flags\n");
    uint32_t _flags = READ_32_UNALIGNED(pos);
    UNUSED(_flags);
    pos += 4;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: num_instr\n");
    uint32_t _num_instr = READ_32_UNALIGNED(pos);
    UNUSED(_num_instr);
    pos += 4;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: num_refs\n");
    mod->line_refs_count = READ_32_UNALIGNED(pos);
    pos += 4;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: num_filenames\n");
    mod->locations_count = READ_32_UNALIGNED(pos);
    pos += 4;

    mod->line_refs_table = pos;

    if (UNLIKELY(!module_check_line_refs(mod, &pos, len - (pos - data)))) {
        mod->line_refs_count = 0;
        mod->line_refs_table = NULL;
        mod->locations_count = 0;
        mod->locations_table = NULL;
        return;
    }

    mod->locations_table = pos;

    if (UNLIKELY(!module_check_locations(mod, pos, len - (pos - data)))) {
        mod->line_refs_count = 0;
        mod->line_refs_table = NULL;
        mod->locations_count = 0;
        mod->locations_table = NULL;
    }
}

void module_insert_line_ref_offset(Module *mod, struct ListHead *line_refs, uint32_t line_ref, int offset)
{
    if (IS_NULL_PTR(mod->line_refs_table) || line_ref == 0) {
        return;
    }
    struct LineRefOffset *ref_offset = malloc(sizeof(struct LineRefOffset));
    if (IS_NULL_PTR(ref_offset)) {
        size_t num_refs = 0;
        // Empty the list
        while (!list_is_empty(line_refs)) {
            struct ListHead *item = line_refs->next;
            list_remove(item);
            free(item);
            num_refs++;
        }
        fprintf(stderr, "Warning: Unable to allocate space for an additional line ref offset (we had %zu).  Line information in stacktraces may be missing\n", num_refs);
        // Give up having line numbers for this module.
        mod->line_refs_table = NULL;
        return;
    }
    ref_offset->offset = offset;
    list_append(line_refs, &ref_offset->head);
}

static bool module_find_line_ref(Module *mod, uint16_t line_ref, uint32_t *line, size_t *filename_len, const uint8_t **filename)
{
    uint16_t location_ix;
    if (UNLIKELY(!module_get_line_ref(mod, line_ref, line, &location_ix))) {
        return false;
    }
    return module_get_location(mod, location_ix, filename_len, filename);
}

bool module_find_line(Module *mod, unsigned int offset, uint32_t *line, size_t *filename_len, const uint8_t **filename)
{
    size_t i;
#ifndef AVM_NO_JIT
    if (mod->native_code) {
        const uint8_t *labels_and_lines = (const uint8_t *) mod->native_code(NULL, NULL, NULL);
        int labels_count = READ_16_UNALIGNED(labels_and_lines);
        labels_and_lines += 2 + labels_count * 6;
        size_t lines_count = READ_16_UNALIGNED(labels_and_lines);
        if (lines_count == 0) {
            return false;
        }
        labels_and_lines += 2;
        uint16_t prev_line_ref = 0;
        for (i = 0; i < lines_count; i++) {
            uint16_t line_ref = READ_16_UNALIGNED(labels_and_lines);
            labels_and_lines += 2;
            uint32_t ref_offset = READ_32_UNALIGNED(labels_and_lines);
            labels_and_lines += 4;
            if (offset == ref_offset) {
                return module_find_line_ref(mod, line_ref, line, filename_len, filename);
            } else if (i == 0 && offset < ref_offset) {
                return false;
            } else if (offset < ref_offset) {
                return module_find_line_ref(mod, prev_line_ref, line, filename_len, filename);
            }
            prev_line_ref = line_ref;
        }
        return module_find_line_ref(mod, prev_line_ref, line, filename_len, filename);
    } else {
#if defined(AVM_NO_EMU)
        return false;
#endif
#endif
#ifndef AVM_NO_EMU
        uint32_t line_ref;
        unsigned int ref_offset;
        const uint8_t *ref_pc;
        if (IS_NULL_PTR(mod->line_refs_offsets) || UNLIKELY(mod->line_refs_offsets_count == 0)) {
            return false;
        }
        for (i = 0; i < mod->line_refs_offsets_count; i++) {
            ref_offset = mod->line_refs_offsets[i];
            if (offset == ref_offset) {
                ref_pc = &mod->code->code[ref_offset];
                DECODE_LITERAL(line_ref, ref_pc);
                return module_find_line_ref(mod, line_ref, line, filename_len, filename);
            } else if (i == 0 && offset < ref_offset) {
                return false;
            } else if (offset < ref_offset) {
                ref_offset = mod->line_refs_offsets[i - 1];
                ref_pc = &mod->code->code[ref_offset];
                DECODE_LITERAL(line_ref, ref_pc);
                return module_find_line_ref(mod, line_ref, line, filename_len, filename);
            }
        }
        ref_offset = mod->line_refs_offsets[i - 1];
        ref_pc = &mod->code->code[ref_offset];
        DECODE_LITERAL(line_ref, ref_pc);
        return module_find_line_ref(mod, line_ref, line, filename_len, filename);
#endif
#ifndef AVM_NO_JIT
    }
#endif
}

COLD_FUNC void module_cp_to_label_offset(term cp, Module **cp_mod, int *label, int *l_off, long *out_mod_offset, GlobalContext *global)
{
    Module *mod = globalcontext_get_module_by_index(global, ((uintptr_t) cp) >> 24);
    long mod_offset = (cp & 0xFFFFFF) >> 2;
    if (out_mod_offset) {
        *out_mod_offset = mod_offset;
    }

    if (cp_mod) {
        *cp_mod = mod;
    }

#ifndef AVM_NO_JIT
    if (mod->native_code) {
        const uint8_t *labels_and_lines = (const uint8_t *) mod->native_code(NULL, NULL, NULL);
        int labels_count = READ_16_UNALIGNED(labels_and_lines);
        labels_and_lines += 2;
        uint32_t label_offset = 0;
        uint16_t label_id = 0;
        while (labels_count > 0) {
            uint16_t new_label_id = READ_16_UNALIGNED(labels_and_lines);
            labels_and_lines += 2;
            uint32_t new_label_offset = READ_32_UNALIGNED(labels_and_lines);
            labels_and_lines += 4;
            if (new_label_offset > mod_offset) {
                if (label) {
                    *label = label_id;
                }
                if (l_off) {
                    *l_off = mod_offset - label_offset;
                }
                return;
            }
            if (new_label_offset == mod_offset) {
                if (label) {
                    *label = new_label_id;
                }
                if (l_off) {
                    *l_off = mod_offset - new_label_offset;
                }
                return;
            }
            label_id = new_label_id;
            label_offset = new_label_offset;
            labels_count--;
        }
        if (label) {
            *label = label_id;
        }
        if (l_off) {
            *l_off = 0;
        }
#endif
#if !defined(AVM_NO_JIT) && !defined(AVM_NO_EMU)
    } else {
#endif
#ifndef AVM_NO_EMU
        uint8_t *code = &mod->code->code[0];
        int labels_count = ENDIAN_SWAP_32(mod->code->labels);

        int i = 1;
        const uint8_t *l = mod->labels[1];
        while (mod_offset > l - code) {
            i++;
            if (i >= labels_count) {
                // last label + 1 is reserved for end of module.
                if (label) {
                    *label = i;
                }
                if (l_off) {
                    *l_off = 0;
                }
                return;
            }
            l = mod->labels[i];
        }

        if (label) {
            *label = i - 1;
        }
        if (l_off) {
            *l_off = mod_offset - (mod->labels[*label] - code);
        }
#endif
#ifndef AVM_NO_JIT
    }
#endif
}

uint32_t module_label_code_offset(Module *mod, int label)
{
#ifndef AVM_NO_JIT
    if (mod->native_code) {
        const uint8_t *labels_and_lines = (const uint8_t *) mod->native_code(NULL, NULL, NULL);
        int labels_count = READ_16_UNALIGNED(labels_and_lines);
        labels_and_lines += 2;
        while (labels_count > 0) {
            uint16_t label_id = READ_16_UNALIGNED(labels_and_lines);
            labels_and_lines += 2;
            if (label_id == label) {
                return READ_32_UNALIGNED(labels_and_lines);
            } else {
                labels_and_lines += 4;
            }
            labels_count--;
        }
        return 0;
    } else {
#endif
        uint8_t *code = &mod->code->code[0];
        return mod->labels[label] - code;
#ifndef AVM_NO_JIT
    }
#endif
}

#ifndef AVM_NO_JIT
void module_set_native_code(Module *mod, uint32_t labels_count, ModuleNativeEntryPoint entry_point)
{
    mod->native_code = entry_point;
    // Extra function is OP_INT_CALL_END
    mod->end_instruction_ii = JIT_JUMPTABLE_ENTRY_SIZE * labels_count;
}
#endif
