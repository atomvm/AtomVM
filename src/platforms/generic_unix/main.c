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

#include <libgen.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "atom.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "globalcontext.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "term.h"
#include "utils.h"

static const char *ok_a = ATOM_STR("\x2", "ok");

void close_mapped_files(MappedFile **mapped_file, int len)
{
    for (int i = 0; i < len; ++i) {
        mapped_file_close(mapped_file[i]);
    }
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("Need .beam or .avm files\n");
        return EXIT_FAILURE;
    }

    int num_mapped_files = argc - 1;
    MappedFile **mapped_file = malloc(num_mapped_files * sizeof(MappedFile *));
    if (UNLIKELY(IS_NULL_PTR(mapped_file))) {
        fprintf(stderr, "Memory error:  Unable to allocate space for %d mapped files\n", num_mapped_files);
        return EXIT_FAILURE;
    }
    for (int i = 0; i < num_mapped_files; ++i) {
        mapped_file[i] = mapped_file_open_beam(argv[i + 1]);
        if (IS_NULL_PTR(mapped_file[i])) {
            fprintf(stderr, "Failed to map file %s\n", argv[i + 1]);
            return EXIT_FAILURE;
        }
    }

    GlobalContext *glb = globalcontext_new();

    const void *startup_beam = NULL;
    uint32_t startup_beam_size;
    const char *startup_module_name;

    if (argc == 2 && iff_is_valid_beam(mapped_file[0]->mapped)) {
        glb->avmpack_platform_data = NULL;
        startup_module_name = basename(argv[1]);
        startup_beam = mapped_file[0]->mapped;
        startup_beam_size = mapped_file[0]->size;
    } else {
        glb->avmpack_platform_data = (const void **) mapped_file;
        for (int i = 0; i < num_mapped_files; ++i) {
            if (avmpack_is_valid(mapped_file[i]->mapped, mapped_file[i]->size)) {
                struct AVMPackData *avmpack_data = malloc(sizeof(struct AVMPackData));
                if (IS_NULL_PTR(avmpack_data)) {
                    fprintf(stderr, "Memory error: Cannot allocate AVMPackData.\n");
                    close_mapped_files(mapped_file, i + 1);
                    return EXIT_FAILURE;
                }
                avmpack_data->data = mapped_file[i]->mapped;
                list_append(&glb->avmpack_data, (struct ListHead *) avmpack_data);

                if (IS_NULL_PTR(startup_beam)) {
                    avmpack_find_section_by_flag(mapped_file[i]->mapped, 1, &startup_beam, &startup_beam_size, &startup_module_name);
                }
            } else if (i == 0 && iff_is_valid_beam(mapped_file[i]->mapped)) {
                glb->avmpack_platform_data++;
                startup_module_name = basename(argv[1]);
                startup_beam = mapped_file[0]->mapped;
                startup_beam_size = mapped_file[0]->size;
            } else if (i == 0) {
                fprintf(stderr, "%s is not an AVM or a BEAM file.\n", argv[1 + i]);
                close_mapped_files(mapped_file, i + 1);
                return EXIT_FAILURE;
            } else {
                fprintf(stderr, "%s is not an AVM file.\n", argv[1 + i]);
                close_mapped_files(mapped_file, i + 1);
                return EXIT_FAILURE;
            }
        }
    }

    if (IS_NULL_PTR(startup_beam)) {
        fprintf(stderr, "Unable to locate entrypoint.\n");
        close_mapped_files(mapped_file, num_mapped_files);
        return EXIT_FAILURE;
    }

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, "Cannot load startup module: %s\n", startup_module_name);
        return EXIT_FAILURE;
    }
    globalcontext_insert_module_with_filename(glb, mod, startup_module_name);
    mod->module_platform_data = NULL;
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    context_execute_loop(ctx, mod, "start", 0);

    term ret_value = ctx->x[0];
    fprintf(stderr, "Return value: ");
    term_display(stderr, ret_value, ctx);
    fprintf(stderr, "\n");

    term ok_atom = context_make_atom(ctx, ok_a);

    context_destroy(ctx);
    globalcontext_destroy(glb);
    module_destroy(mod);
    close_mapped_files(mapped_file, num_mapped_files);

    if (ok_atom == ret_value) {
        return EXIT_SUCCESS;
    } else {
        return EXIT_FAILURE;
    }
}
