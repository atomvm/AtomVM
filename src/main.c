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
#include "iff.h"
#include "utils.h"

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

    Context *ctx = context_new();

    execute_loop(chunk, ctx, mod, beam_file, offsets);

    return EXIT_SUCCESS;
}
