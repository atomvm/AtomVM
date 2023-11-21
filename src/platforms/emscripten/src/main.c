/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>
#include <sys.h>

#include <emscripten.h>
#include <emscripten/promise.h>

#include "emscripten_sys.h"

static GlobalContext *global = NULL;
static Module *main_module = NULL;

/**
 * @brief Load a module in .avm or .beam format.
 * @param path Path or URL to the module or avm package to load.
 * @return EXIT_SUCESS or EXIT_FAILURE which is then returned by main
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
            avmpack_find_section_by_flag(avmpack_data->data, 1, &startup_beam, &startup_beam_size, &startup_module_name);
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
        globalcontext_insert_module(global, module);
        if (IS_NULL_PTR(main_module) && module_search_exported_function(module, ATOM_STR("\5", "start"), 0, global) != 0) {
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
 * @return EXIT_SUCESS or EXIT_FAILURE which is then returned by main
 */
static int start(void)
{
    if (IS_NULL_PTR(main_module)) {
        fprintf(stderr, "main module not loaded\n");
        return EXIT_FAILURE;
    }
    Context *ctx = context_new(global);
    ctx->leader = 1;
    context_execute_loop(ctx, main_module, "start", 0);
    term ret_value = ctx->x[0];
    fprintf(stdout, "Return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");

    int status;
    if (ret_value == OK_ATOM) {
        status = EXIT_SUCCESS;
    } else {
        status = EXIT_FAILURE;
    }

    context_destroy(ctx);

    return status;
}

/**
 * @brief Send a message to a (registered) erlang process.
 * @details This function is thread safe and is meant to be executed on the
 * browser's main thread. It is typically called by `Module.cast` wrapper in
 * `atomvm.pre.js`.
 * @param name Name of the process to send a message to
 * @param message Message to send to the process
 */
EMSCRIPTEN_KEEPALIVE
void cast(const char *name, const char *message)
{
    sys_enqueue_emscripten_cast_message(global, name, message);
}

/**
 * @brief Send a message to a (registered) erlang process and wait for
 * the result.
 * @details This function returns an Emscripten promise handle which needs to
 * be converted to the actual promise so Javascript can await on it.
 * This is typically done by using `Module.call` wrapper in `atomvm.pre.js`.
 * @param name Name of the process to send a message to
 * @param message Message to send to the process
 * @return a promise handle
 */
EMSCRIPTEN_KEEPALIVE
em_promise_t call(const char *name, const char *message)
{
    return sys_enqueue_emscripten_call_message(global, name, message);
}

/**
 * @brief Emscripten entry point
 * @details For node builds, this function is run in the main thread. For web
 * builds, this function actually is proxied by Emscripten SDK and runs on a
 * web worker thread.
 *
 * In both cases, the function requires paths to one or more Erlang `.beam` or
 * AtomVM `.avm` files.
 *
 * @param argc number of arguments
 * @param argv arguments
 * @return EXIT_SUCCESS or EXIT_FAILURE
 */
int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "No .avm or .beam module specified\n");
        return EXIT_FAILURE;
    }
    int result = EXIT_SUCCESS;

    global = globalcontext_new();

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

    if (main_module) {
        module_destroy(main_module);
        main_module = NULL;
    }

    return result;
}
