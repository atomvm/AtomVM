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

#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "Context.h"
#include "Module.h"
#include "Term.h"

#include "bif.h"
#include "utils.h"

#define AT8U 0
#define CODE 1
#define LOCT 2
#define IMPT 3
#define MAX_OFFS 4

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

struct IFFRecord
{
    const char name[4];
    uint32_t size;
};

uint32_t iff_align(uint32_t size)
{
    return ((size + 4 - 1) >> 2) << 2;
}

void scan_iff(uint8_t *data, int file_size, unsigned long *offsets)
{
    int current_pos = 12;

    do {
        struct IFFRecord *current_record = (struct IFFRecord *) (data + current_pos);

        if (!memcmp(current_record->name, "AtU8", 4)) {
            offsets[AT8U] = current_pos;

        } else if (!memcmp(current_record->name, "Code", 4)) {
            offsets[CODE] = current_pos;

        } else if (!memcmp(current_record->name, "LocT", 4)) {
            offsets[LOCT] = current_pos;

        } else if (!memcmp(current_record->name, "ImpT", 4)) {
            offsets[IMPT] = current_pos;
        }

        current_pos += iff_align(bswap_32(current_record->size) + 8);
    } while (current_pos < file_size);
}

char reg_type_c(int reg_type)
{
    switch (reg_type) {
        case 2:
            return 'a';

        case 3:
            return 'x';

        case 4:
            return 'y';

        default:
            return '?';
    }
}

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

#define IMPL_CODE_LOADER 1
#define ENABLE_TRACE
#include "opcodesswitch.h"
//#undef ENABLE_TRACE
//#undef TRACE
#undef IMPL_CODE_LOADER

#define IMPL_EXECUTE_LOOP
#include "opcodesswitch.h"
#undef IMPL_EXECUTE_LOOP

int main(int argc, char **argv)
{
    int fd = open(argv[1], O_RDONLY);
    if (argc < 2) {
        printf("Need .beam file\n");
        return EXIT_FAILURE;
    }

    struct stat file_stats;
    fstat(fd, &file_stats);

    uint8_t *beam_file = mmap(NULL, file_stats.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (!beam_file || memcmp(beam_file, "FOR1", 4)) {
        return EXIT_FAILURE;
    }

    unsigned long offsets[MAX_OFFS];
    scan_iff(beam_file, file_stats.st_size, offsets);

    Module *mod = malloc(sizeof(Module));

    module_build_imported_functions_table(mod, beam_file + offsets[IMPT], beam_file + offsets[AT8U]);

    CodeChunk *chunk = (CodeChunk *) (beam_file + offsets[CODE]);
    mod->labels = calloc(READ_32_ALIGNED(&chunk->labels), sizeof(void *));

    printf("STARTING\n");
    read_core_chunk(chunk, mod);

    Context *ctx = malloc(sizeof(Context));
    ctx->cp = (unsigned long) -1;

    ctx->stack = (term *) calloc(DEFAULT_STACK_SIZE, sizeof(term));
    ctx->stack_size = DEFAULT_STACK_SIZE;
    ctx->stack_frame = ctx->stack;
    ctx->e = ctx->stack;

    execute_loop(chunk, ctx, mod, beam_file, offsets);

    return EXIT_SUCCESS;
}
