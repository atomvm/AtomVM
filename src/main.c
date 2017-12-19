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
#include "bif.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "utils.h"
#include "term.h"

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("Need .beam file\n");
        return EXIT_FAILURE;
    }
    MappedFile *beam_file = mapped_file_open_beam(argv[1]);
    if (!beam_file) {
        return EXIT_FAILURE;
    }

    Module *mod = module_new_from_iff_binary(beam_file->mapped, beam_file->size);
    GlobalContext *glb = globalcontext_new();
    Context *ctx = context_new(glb);

    context_execute_loop(ctx, mod, beam_file->mapped, "start", 0);

    printf("Return value: %lx\n", ctx->x[0]);

    context_destroy(ctx);
    globalcontext_destroy(glb);
    module_destroy(mod);
    mapped_file_close(beam_file);

    return EXIT_SUCCESS;
}
