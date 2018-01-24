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

static void *module_uncompress_literals(const uint8_t *litT, int size);
static void const* *module_build_literals_table(const void *literalsBuf);
static void module_add_label(Module *mod, int index, void *ptr);
static void module_build_imported_functions_table(Module *this_module, uint8_t *table_data, uint8_t *atom_tab);
static void module_add_label(Module *mod, int index, void *ptr);

#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER

static void module_build_imported_functions_table(Module *this_module, uint8_t *table_data, uint8_t *atom_tab)
{
    int functions_count = READ_32_ALIGNED(table_data + 8);

    this_module->imported_funcs = calloc(functions_count, sizeof(void *));

    for (int i = 0; i < functions_count; i++) {
        AtomString module_atom = local_atom_string(atom_tab, READ_32_ALIGNED(table_data + i * 12 + 12));
        AtomString function_atom = local_atom_string(atom_tab, READ_32_ALIGNED(table_data + i * 12 + 4 + 12));
        uint32_t arity = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);

        BifImpl bif_handler = bif_registry_get_handler(module_atom, function_atom, arity);

        if (bif_handler) {
            this_module->imported_funcs[i].bif = bif_handler;
        } else {
            this_module->imported_funcs[i].func = &nifs_get(module_atom, function_atom, arity)->base;
        }
    }
}

uint32_t module_search_exported_function(Module *this_module, AtomString func_name, int func_arity)
{
    const uint8_t *table_data = (const uint8_t *) this_module->export_table;
    int functions_count = READ_32_ALIGNED(table_data + 8);

    for (int i = 0; i < functions_count; i++) {
        AtomString function_atom = local_atom_string(this_module->atom_table, READ_32_ALIGNED(table_data + i * 12 + 12));
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

Module *module_new_from_iff_binary(void *iff_binary, unsigned long size)
{
    uint8_t *beam_file = (void *) iff_binary;

    unsigned long offsets[MAX_OFFS];
    unsigned long sizes[MAX_SIZES];
    scan_iff(beam_file, size, offsets, sizes);

    Module *mod = malloc(sizeof(Module));

    module_build_imported_functions_table(mod, beam_file + offsets[IMPT], beam_file + offsets[AT8U]);

    mod->code = (CodeChunk *) (beam_file + offsets[CODE]);
    mod->export_table = beam_file + offsets[EXPT];
    mod->atom_table = beam_file + offsets[AT8U];
    mod->labels = calloc(ENDIAN_SWAP_32(mod->code->labels), sizeof(void *));

    if (offsets[LITT]) {
        #ifdef WITH_ZLIB
            mod->literals_data = module_uncompress_literals(beam_file + offsets[LITT], sizes[LITT]);
        #else
            fprintf(stderr, "zlib required to uncompress literals.\n");
            abort();
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

    read_core_chunk(mod);

    return mod;
}

void module_destroy(Module *module)
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
        abort();
    }
    ret = inflate(&infstream, Z_NO_FLUSH);
    if (ret != Z_OK) {
        fprintf(stderr, "Failed inflate\n");
        abort();
    }
    inflateEnd(&infstream);

    return outBuf;
}
#endif

static void const* *module_build_literals_table(const void *literalsBuf)
{
    uint32_t terms_count = READ_32_ALIGNED(literalsBuf);

    const uint8_t *pos = (const uint8_t *) literalsBuf + sizeof(uint32_t);

    void const* *literals_table = calloc(terms_count, sizeof(void *const));
    for (uint32_t i = 0; i < terms_count; i++) {
        uint32_t term_size = READ_32_UNALIGNED(pos);
        literals_table[i] = pos + sizeof(uint32_t);

        pos += term_size + sizeof(uint32_t);
    }

    return literals_table;
}

term module_load_literal(Module *mod, int index)
{
    return externalterm_to_term(mod->literals_table[index]);
}
