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

#include "Module.h"

#include "Context.h"
#include "atom.h"
#include "bif.h"
#include "iff.h"
#include "utils.h"

#include <stdio.h>
#include <stdlib.h>

#define IMPL_CODE_LOADER 1
#include "opcodesswitch.h"
#undef TRACE
#undef IMPL_CODE_LOADER

void module_build_imported_functions_table(Module *this_module, uint8_t *table_data, uint8_t *atom_tab)
{
    int functions_count = READ_32_ALIGNED(table_data + 8);

    fprintf(stderr, "Looking for bifs, found %i imported functions.\n", functions_count);

    this_module->imported_bifs = calloc(functions_count, sizeof(void *));

    for (int i = 0; i < functions_count; i++) {
        AtomString module_atom = local_atom_string(atom_tab, READ_32_ALIGNED(table_data + i * 12 + 12));
        AtomString function_atom = local_atom_string(atom_tab, READ_32_ALIGNED(table_data + i * 12 + 4 + 12));
        uint32_t arity = READ_32_ALIGNED(table_data + i * 12 + 8 + 12);

        char module_string[16];
        char function_string[16];
        atom_string_to_c(module_atom, module_string, sizeof(module_string));
        atom_string_to_c(function_atom, function_string, sizeof(function_string));


        printf("%s:%s\\%i\n", module_string, function_string, arity);

        if (bif_registry_is_bif(module_atom, function_atom, arity)) {
            this_module->imported_bifs[i] = bif_registry_get_handler(module_atom, function_atom, arity);
            printf("installed bif: %i\n", i);
        } else {
            this_module->imported_bifs[i] = NULL;
        }
    }
}

void module_add_label(Module *mod, int index, void *ptr)
{
    mod->labels[index] = ptr;
}

Module *module_new_from_iff_binary(void *iff_binary, unsigned long size)
{
    uint8_t *beam_file = (void *) iff_binary;

    unsigned long offsets[MAX_OFFS];
    scan_iff(beam_file, size, offsets);

    Module *mod = malloc(sizeof(Module));

    module_build_imported_functions_table(mod, beam_file + offsets[IMPT], beam_file + offsets[AT8U]);

    mod->code = (CodeChunk *) (beam_file + offsets[CODE]);
    mod->labels = calloc(ENDIAN_SWAP_32(mod->code->labels), sizeof(void *));

    read_core_chunk(mod);

    return mod;
}
