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

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "platforms/Linux/mapped_file.h"
#include "module.h"
#include "utils.h"
#include "term.h"

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("Need .beam file\n");
        return EXIT_FAILURE;
    }
    MappedFile *mapped_file = mapped_file_open_beam(argv[1]);
    if (!mapped_file) {
        return EXIT_FAILURE;
    }

    GlobalContext *glb = globalcontext_new();

    const void *startup_beam;
    uint32_t startup_beam_size;
    const char *startup_module_name = argv[1];

    if (avmpack_is_valid(mapped_file->mapped, mapped_file->size)) {
        glb->avmpack_data = mapped_file->mapped;
        glb->avmpack_platform_data = mapped_file;

        if (!avmpack_find_section_by_flag(mapped_file->mapped, 1, &startup_beam, &startup_beam_size, &startup_module_name)) {
            fprintf(stderr, "%s cannot be started.\n", argv[1]);
            mapped_file_close(mapped_file);
            return EXIT_FAILURE;
        }
    } else if (iff_is_valid_beam(mapped_file->mapped)) {
        glb->avmpack_data = NULL;
        glb->avmpack_platform_data = NULL;
        startup_beam = mapped_file->mapped;
        startup_beam_size = mapped_file->size;

    } else {
        fprintf(stderr, "%s is not a BEAM file.\n", argv[1]);
        mapped_file_close(mapped_file);
        return EXIT_FAILURE;
    }

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    globalcontext_insert_module_with_filename(glb, mod, startup_module_name);
    mod->module_platform_data = NULL;
    Context *ctx = context_new(glb);
    ctx->mod = mod;

    context_execute_loop(ctx, mod, mapped_file->mapped, "start", 0);

    printf("Return value: %lx\n", ctx->x[0]);

    context_destroy(ctx);
    globalcontext_destroy(glb);
    module_destroy(mod);
    mapped_file_close(mapped_file);

    return EXIT_SUCCESS;
}
