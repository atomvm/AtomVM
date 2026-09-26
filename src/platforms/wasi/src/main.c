/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>
#include <stdio.h>
#include <sys.h>
#include <unistd.h>

#include "wasi_sys.h"

static GlobalContext *global = NULL;
static Module *main_module = NULL;

/**
 * @brief Load a module in .avm or .beam format.
 * @param path Path to the module or avm package to load.
 * @return EXIT_SUCCESS or EXIT_FAILURE which is then returned by main
 */
static int load_module(const char *path)
{
    const char *ext = strrchr(path, '.');
    if (ext && strcmp(ext, ".avm") == 0) {
        struct AVMPackData *avmpack_data;
        if (sys_open_avm_from_file(global, path, &avmpack_data) != AVM_OPEN_OK) {
            fprintf(stderr, "Failed opening %s.\n", path);
            return EXIT_FAILURE;
        }
        synclist_append(&global->avmpack_data, &avmpack_data->avmpack_head);
        if (IS_NULL_PTR(main_module)) {
            const void *startup_beam = NULL;
            uint32_t startup_beam_size;
            const char *startup_module_name;
            avmpack_find_section_by_flag(avmpack_data->data, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name);
            if (startup_beam) {
                avmpack_data->in_use = true;
                main_module = module_new_from_iff_binary(global, startup_beam, startup_beam_size);
                if (IS_NULL_PTR(main_module)) {
                    fprintf(stderr, "Cannot load startup module: %s\n", startup_module_name);
                    return EXIT_FAILURE;
                }
                globalcontext_insert_module(global, main_module);
                main_module->module_platform_data = NULL;
            }
        }
    } else if (ext && strcmp(ext, ".beam") == 0) {
        Module *module = sys_load_module_from_file(global, path);
        if (IS_NULL_PTR(module)) {
            fprintf(stderr, "Failed loading %s.\n", path);
            return EXIT_FAILURE;
        }
        globalcontext_insert_module(global, module);
        if (IS_NULL_PTR(main_module) && module_search_exported_function(module, START_ATOM_INDEX, 0) != 0) {
            main_module = module;
        }
    } else {
        fprintf(stderr, "%s is not an AVM or BEAM file.\n", path);
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

/**
 * @brief Create first context and run main module's start function
 * @return EXIT_SUCCESS or EXIT_FAILURE which is then returned by main
 */
static int start(void)
{
    if (IS_NULL_PTR(main_module)) {
        fprintf(stderr, "main module not loaded\n");
        return EXIT_FAILURE;
    }
    run_result_t ret_value = globalcontext_run(global, main_module, stdout, 0, NULL);
    int status;
    if (ret_value == RUN_SUCCESS) {
        status = EXIT_SUCCESS;
    } else {
        status = EXIT_FAILURE;
    }
    return status;
}

/**
 * @brief WASI entry point
 * @details Runs the AtomVM interpreter with files passed as arguments.
 * WASI runtimes provide preopened directories for file access.
 *
 * @param argc number of arguments
 * @param argv arguments
 * @return EXIT_SUCCESS or EXIT_FAILURE
 */
int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <module.avm|module.beam> [additional_modules...]\n", argv[0]);
        fprintf(stderr, "\nWASI-specific:\n");
        fprintf(stderr, "  Make sure to grant directory access with --dir=.\n");
        fprintf(stderr, "  Example: wasmtime run --dir=. AtomVM.wasm myapp.avm\n");
        return EXIT_FAILURE;
    }

    int result = EXIT_SUCCESS;
    global = globalcontext_new();
    if (IS_NULL_PTR(global)) {
        fprintf(stderr, "Cannot allocate global context\n");
        return EXIT_FAILURE;
    }

    for (int i = 1; i < argc; ++i) {
        result = load_module(argv[i]);
        if (UNLIKELY(result != EXIT_SUCCESS)) {
            break;
        }
    }

    if (result == EXIT_SUCCESS) {
        result = start();
    }

    globalcontext_destroy(global);
    global = NULL;
    main_module = NULL;

    return result;
}
