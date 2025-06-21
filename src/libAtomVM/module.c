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
#include "globalcontext.h"
#include "iff.h"
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
static void module_add_label(Module *mod, int index, const uint8_t *ptr);
static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data, GlobalContext *glb);
static void module_parse_line_table(Module *mod, const uint8_t *data, size_t len);

struct LineRefOffset
{
    struct ListHead head;
    unsigned int offset;
};

#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER

static enum ModuleLoadResult module_populate_atoms_table(Module *this_module, uint8_t *table_data, GlobalContext *glb)
{
    int atoms_count = READ_32_UNALIGNED(table_data + 8);

    if (UNLIKELY(atoms_count < 0)) {
        fprintf(stderr, "Code compiled with OTP-28 is not supported by this version of AtomVM.\n"
                "Please recompile your code using an earlier version, such as OTP-27,\n"
                "or switch to a newer version of AtomVM, such as a main snapshot.\n");
        AVM_ABORT();
    }

    const char *current_atom = (const char *) table_data + 12;

    this_module->local_atoms_to_global_table = calloc(atoms_count + 1, sizeof(int));
    if (IS_NULL_PTR(this_module->local_atoms_to_global_table)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    long ensure_result = atom_table_ensure_atoms(
        glb->atom_table, current_atom, atoms_count, this_module->local_atoms_to_global_table + 1);
    if (ensure_result == ATOM_TABLE_ALLOC_FAIL) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    return MODULE_LOAD_OK;
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
        AtomString module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index, glb);
        AtomString function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index, glb);
        uint32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 8 + 12);

        const struct ExportedFunction *bif = bif_registry_get_handler(module_atom, function_atom, arity);

        if (bif) {
            this_module->imported_funcs[i] = bif;
        } else {
            this_module->imported_funcs[i] = &nifs_get(module_atom, function_atom, arity)->base;
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

bool module_get_function_from_label(Module *this_module, int label, AtomString *function_name, int *arity, GlobalContext *glb)
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
            *function_name = module_get_atom_string_by_id(this_module, fun_atom_index, glb);
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
            *function_name = module_get_atom_string_by_id(this_module, fun_atom_index, glb);
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

uint32_t module_search_exported_function(Module *this_module, AtomString func_name, int func_arity, GlobalContext *glb)
{
    size_t functions_count = module_get_exported_functions_count(this_module);

    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    for (unsigned int i = 0; i < functions_count; i++) {
        AtomString function_atom = module_get_atom_string_by_id(this_module, READ_32_UNALIGNED(table_data + i * 12 + 12), glb);
        int32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 4 + 12);
        if ((func_arity == arity) && atom_are_equals(func_name, function_atom)) {
            uint32_t label = READ_32_UNALIGNED(table_data + i * 12 + 8 + 12);
            return label;
        }
    }

    return 0;
}

term module_get_exported_functions(Module *this_module, Heap *heap, GlobalContext *glb)
{
    size_t functions_count = module_get_exported_functions_count(this_module);
    term result_list = term_nil();

    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    for (unsigned int i = 0; i < functions_count; i++) {
        AtomString function_atom = module_get_atom_string_by_id(this_module, READ_32_UNALIGNED(table_data + i * 12 + 12), glb);
        int32_t arity = READ_32_UNALIGNED(table_data + i * 12 + 4 + 12);
        term function_tuple = term_alloc_tuple(2, heap);
        term_put_tuple_element(function_tuple, 0, globalcontext_existing_term_from_atom_string(glb, function_atom));
        term_put_tuple_element(function_tuple, 1, term_from_int(arity));
        result_list = term_list_prepend(function_tuple, result_list, heap);
    }
    return result_list;
}

static void module_add_label(Module *mod, int index, const uint8_t *ptr)
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
    mod->code = (CodeChunk *) (beam_file + offsets[CODE]);
    mod->export_table = beam_file + offsets[EXPT];
    mod->local_table = beam_file + offsets[LOCT];
    mod->atom_table = beam_file + offsets[AT8U];
    mod->fun_table = beam_file + offsets[FUNT];
    mod->str_table = beam_file + offsets[STRT];
    mod->str_table_len = sizes[STRT];
    uint32_t num_labels = ENDIAN_SWAP_32(mod->code->labels);
    mod->labels = calloc(num_labels, sizeof(void *));
    if (IS_NULL_PTR(mod->labels)) {
        fprintf(stderr, "Error: Null module labels: %s:%i.\n", __FILE__, __LINE__);
        module_destroy(mod);
        return NULL;
    }

    module_parse_line_table(mod, beam_file + offsets[LINT] + 8, sizes[LINT]);

    if (offsets[LITT]) {
        if (UNLIKELY(!module_are_literals_compressed(beam_file + offsets[LITT]))) {
            fprintf(stderr, "Code compiled with OTP-28 is not supported by this version of AtomVM.\n"
                    "Please recompile your code using an earlier version, such as OTP-27,\n"
                    "or switch to a newer version of AtomVM, such as a main snapshot.\n");
        }
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
    // Empty the list
    while (!list_is_empty(&line_refs)) {
        struct ListHead *item = line_refs.next;
        list_remove(item);
        free(item);
    }

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
    uint32_t required_buf_size = READ_32_ALIGNED(litT + LITT_UNCOMPRESSED_SIZE_OFFSET);
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
    term t = externalterm_to_term(mod->literals_table[index].data, mod->literals_table[index].size,
        ctx, ExternalTermToHeapFragment);
    if (UNLIKELY(term_is_invalid_term(t))) {
        fprintf(stderr, "Either OOM or invalid term while reading literals_table[%i] from module\n", index);
    }
    return t;
}

const struct ExportedFunction *module_resolve_function0(Module *mod, int import_table_index, struct UnresolvedFunctionCall *unresolved, GlobalContext *glb)
{

    AtomString module_name_atom = atom_table_get_atom_string(glb->atom_table, unresolved->module_atom_index);
    AtomString function_name_atom = atom_table_get_atom_string(glb->atom_table, unresolved->function_atom_index);
    int arity = unresolved->arity;

    Module *found_module = globalcontext_get_module(glb, module_name_atom);

    if (LIKELY(found_module != NULL)) {
        int exported_label = module_search_exported_function(found_module, function_name_atom, arity, glb);
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
        mod->imported_funcs[import_table_index] = &mfunc->base;
        return &mfunc->base;
    } else {
        char buf[256];
        atom_string_to_c(module_name_atom, buf, 256);
        fprintf(stderr, "Warning: module %s cannot be resolved.\n", buf);
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
    unsigned int ref_offset;
    uint32_t line_ref;
    const uint8_t *ref_pc;
    if (IS_NULL_PTR(mod->line_refs_offsets)) {
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
}
