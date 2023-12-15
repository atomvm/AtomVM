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

// TODO Constants similar to these are defined in opcodesswitch.h and should
// be refactored so they can be used here, as well.
#define TAG_COMPACT_INT 0x01
#define TAG_COMPACT_ATOM 0x02
#define TAG_EXTENDED_INT 0x09
#define TAG_EXTENDED_ATOM 0x0A

#define CHECK_FREE_SPACE(space, error)           \
    if ((size_t) ((pos + space) - data) > len) { \
        fprintf(stderr, error);                  \
        return;                                  \
    }

#ifdef WITH_ZLIB
    static void *module_uncompress_literals(const uint8_t *litT, int size);
#endif
static struct LiteralEntry *module_build_literals_table(const void *literalsBuf);
static void module_add_label(Module *mod, int index, const uint8_t *ptr);
static enum ModuleLoadResult module_build_imported_functions_table(Module *this_module, uint8_t *table_data, GlobalContext *glb);
static void parse_line_table(uint16_t **line_refs, struct ModuleFilename **filenames, uint8_t *data, size_t len);

#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER

static enum ModuleLoadResult module_populate_atoms_table(Module *this_module, uint8_t *table_data, GlobalContext *glb)
{
    int atoms_count = READ_32_ALIGNED(table_data + 8);
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
    int functions_count = READ_32_ALIGNED(table_data + 8);

    this_module->imported_funcs = calloc(functions_count, sizeof(struct ExportedFunction *));
    if (IS_NULL_PTR(this_module->imported_funcs)) {
        fprintf(stderr, "Cannot allocate memory while loading module (line: %i).\n", __LINE__);
        return MODULE_ERROR_FAILED_ALLOCATION;
    }

    for (int i = 0; i < functions_count; i++) {
        int local_module_atom_index = READ_32_ALIGNED(table_data + i * 12 + 12);
        int local_function_atom_index = READ_32_ALIGNED(table_data + i * 12 + 4 + 12);
        AtomString module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index, glb);
        AtomString function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index, glb);
        uint32_t arity = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);

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
    int functions_count = READ_32_ALIGNED(table_data + 8);

    if (UNLIKELY(index > functions_count)) {
        AVM_ABORT();
    }
    int local_module_atom_index = READ_32_ALIGNED(table_data + index * 12 + 12);
    int local_function_atom_index = READ_32_ALIGNED(table_data + index * 12 + 4 + 12);
    *module_atom = module_get_atom_string_by_id(this_module, local_module_atom_index, glb);
    *function_atom = module_get_atom_string_by_id(this_module, local_function_atom_index, glb);
}
#endif

bool module_get_function_from_label(Module *this_module, int label, AtomString *function_name, int *arity, GlobalContext *glb)
{
    int best_label = -1;
    const uint8_t *export_table_data = (const uint8_t *) this_module->export_table;
    int exports_count = READ_32_ALIGNED(export_table_data + 8);
    for (int export_index = exports_count - 1; export_index >= 0; export_index--) {
        int fun_atom_index = READ_32_ALIGNED(export_table_data + (export_index * 12) + 12);
        int fun_arity = READ_32_ALIGNED(export_table_data + (export_index * 12) + 4 + 12);
        int fun_label = READ_32_ALIGNED(export_table_data + (export_index * 12) + 8 + 12);
        if (fun_label <= label && best_label < fun_label) {
            best_label = fun_label;
            *arity = fun_arity;
            *function_name = module_get_atom_string_by_id(this_module, fun_atom_index, glb);
        }
    }

    const uint8_t *local_table_data = (const uint8_t *) this_module->local_table;
    int locals_count = READ_32_ALIGNED(local_table_data + 8);
    for (int local_index = locals_count - 1; local_index >= 0; local_index--) {
        int fun_atom_index = READ_32_ALIGNED(local_table_data + (local_index * 12) + 12);
        int fun_arity = READ_32_ALIGNED(local_table_data + (local_index * 12) + 4 + 12);
        int fun_label = READ_32_ALIGNED(local_table_data + (local_index * 12) + 8 + 12);
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
    size_t functions_count = READ_32_ALIGNED(table_data + 8);
    return functions_count;
}

uint32_t module_search_exported_function(Module *this_module, AtomString func_name, int func_arity, GlobalContext *glb)
{
    size_t functions_count = module_get_exported_functions_count(this_module);

    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    for (unsigned int i = 0; i < functions_count; i++) {
        AtomString function_atom = module_get_atom_string_by_id(this_module, READ_32_ALIGNED(table_data + i * 12 + 12), glb);
        int32_t arity = READ_32_ALIGNED(table_data + i * 12 + 4 + 12);
        if ((func_arity == arity) && atom_are_equals(func_name, function_atom)) {
            uint32_t label = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);
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
        AtomString function_atom = module_get_atom_string_by_id(this_module, READ_32_ALIGNED(table_data + i * 12 + 12), glb);
        int32_t arity = READ_32_ALIGNED(table_data + i * 12 + 4 + 12);
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

    parse_line_table(&mod->line_refs, &mod->filenames, beam_file + offsets[LINT] + 8, sizes[LINT]);
    list_init(&mod->line_ref_offsets);

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
    free(module->local_atoms_to_global_table);
    if (module->free_literals_data) {
        free(module->literals_data);
    }
#ifndef AVM_NO_SMP
    smp_mutex_destroy(module->mutex);
#endif
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
    term t = externalterm_to_term(mod->literals_table[index].data, mod->literals_table[index].size,
        ctx, ExternalTermToHeapFragment);
    if (term_is_invalid_term(t)) {
        fprintf(stderr, "Invalid term reading literals_table[%i] from module\n", index);
        AVM_ABORT();
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

static uint16_t *parse_line_refs(uint8_t **data, size_t num_refs, size_t len)
{
    uint16_t *ref_table = malloc((num_refs + 1) * sizeof(uint16_t));
    if (IS_NULL_PTR(ref_table)) {
        return NULL;
    }

    // assert pos >= *data
    uint8_t *pos = *data;
    for (size_t i = 0; i < num_refs + 1; ++i) {
        if ((size_t) (pos - *data) > len) {
            fprintf(stderr, "Invalid line_ref: expected tag.\n");
            free(ref_table);
            return NULL;
        }
        uint8_t tag = *pos;
        switch (tag & 0x0F) {
            case TAG_COMPACT_INT: {
                uint16_t line_idx = ((tag & 0xF0) >> 4);
                ref_table[i] = line_idx;
                ++pos;
                break;
            }
            case TAG_COMPACT_ATOM: {
                uint16_t line_idx = ((tag & 0xF0) >> 4);
                ref_table[i] = line_idx;
                ++pos;
                break;
            }
            case TAG_EXTENDED_INT: {
                uint16_t high_order_3_bits = (tag & 0xE0);
                ++pos;
                if ((size_t) (pos - *data) > len) {
                    fprintf(stderr, "Invalid line_ref: expected extended int.\n");
                    free(ref_table);
                    return NULL;
                }
                uint8_t next_byte = *pos;
                uint16_t line_idx = ((high_order_3_bits << 3) | next_byte);
                ++pos;
                ref_table[i] = line_idx;
                break;
            }
            case TAG_EXTENDED_ATOM: {
                uint16_t file_idx = ((tag & 0xF0) >> 4);
                ++pos;
                if ((size_t) (pos - *data) > len) {
                    fprintf(stderr, "Invalid line_ref: expected extended atom.\n");
                    free(ref_table);
                    return NULL;
                }
                uint8_t next_byte = *pos;
                uint16_t line_idx = ((next_byte & 0xF0) >> 4);
                ++pos;
                ref_table[file_idx - 1] = line_idx;
                break;
            }
            default:
                // TODO handle integer compact encodings > 2048
                fprintf(stderr, "Unsupported line_ref tag: %u\n", tag);
                free(ref_table);
                return NULL;
        }
    }

    *data = pos;
    return ref_table;
}

struct ModuleFilename *parse_filename_table(uint8_t **data, size_t num_filenames, size_t len)
{
    struct ModuleFilename *filenames = malloc(num_filenames * sizeof(struct ModuleFilename));
    if (IS_NULL_PTR(filenames)) {
        return NULL;
    }

    // assert pos >= *data
    uint8_t *pos = *data;
    for (size_t i = 0; i < num_filenames; ++i) {
        if ((size_t) ((pos + 2) - *data) > len) {
            fprintf(stderr, "Invalid filename: expected 16-bit size.\n");
            free(filenames);
            return NULL;
        }
        uint16_t size = READ_16_UNALIGNED(pos);
        pos +=2;
        if ((size_t) ((pos + size) - *data) > len) {
            fprintf(stderr, "Invalid filename: expected filename data (%u bytes).\n", size);
            free(filenames);
            return NULL;
        }
        filenames[i].len = size;
        filenames[i].data = pos;
        pos += size;
    }

    *data = pos;
    return filenames;
}

static void parse_line_table(uint16_t **line_refs, struct ModuleFilename **filenames, uint8_t *data, size_t len)
{

    *line_refs = NULL;
    *filenames = NULL;

    if (len == 0) {
        return;
    }

    uint8_t *pos = data;

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
    uint32_t num_refs = READ_32_UNALIGNED(pos);
    pos += 4;

    CHECK_FREE_SPACE(4, "Error reading Line chunk: num_filenames\n");
    uint32_t num_filenames = READ_32_UNALIGNED(pos);
    pos += 4;

    *line_refs = parse_line_refs(&pos, num_refs, len - (pos - data));
    if (IS_NULL_PTR(*line_refs)) {
        return;
    }

    *filenames = parse_filename_table(&pos, num_filenames, len - (pos - data));
    if (IS_NULL_PTR(*filenames)) {
        free(*line_refs);
        return;
    }
}

void module_insert_line_ref_offset(Module *mod, int line_ref, int offset)
{
    if (IS_NULL_PTR(mod->line_refs) || line_ref == 0) {
        return;
    }
    struct LineRefOffset *ref_offset = malloc(sizeof(struct LineRefOffset));
    if (IS_NULL_PTR(ref_offset)) {
        fprintf(stderr, "Warning: Unable to allocate space for line ref offset.  Line information in stacktraces may be missing\n");
        return;
    }
    ref_offset->line_ref = line_ref;
    ref_offset->offset = offset;
    list_append(&mod->line_ref_offsets, &ref_offset->head);
}

int module_find_line(Module *mod, unsigned int offset)
{
    int i = 0;
    struct LineRefOffset *head = GET_LIST_ENTRY(&mod->line_ref_offsets, struct LineRefOffset, head);
    struct ListHead *item;
    LIST_FOR_EACH (item, &mod->line_ref_offsets) {
        struct LineRefOffset *ref_offset = GET_LIST_ENTRY(item, struct LineRefOffset, head);

        if (offset == ref_offset->offset) {
            return mod->line_refs[ref_offset->line_ref];
        } else if (i == 0 && offset < ref_offset->offset) {
            return -1;
        } else {

            struct LineRefOffset *prev_ref_offset = GET_LIST_ENTRY(ref_offset->head.prev, struct LineRefOffset, head);
            if (prev_ref_offset->offset <= offset && offset < ref_offset->offset) {
                return mod->line_refs[prev_ref_offset->line_ref];
            }

            struct LineRefOffset *next_ref_offset = GET_LIST_ENTRY(ref_offset->head.next, struct LineRefOffset, head);
            if (next_ref_offset == head && ref_offset->offset <= offset) {
                return mod->line_refs[ref_offset->line_ref];
            }
        }

        ++i;
    }
    // should never occur, but return is needed to squelch compiler warnings
    AVM_ABORT();
    return -1;
}
