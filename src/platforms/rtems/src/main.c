/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Riccardo Binetti <rbino@gmx.com>
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <rtems.h>

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
#include <bsp/fdt.h>
#include <libfdt.h>
#endif

#ifdef RTEMS_HAS_STORAGE
#include "storage.h"
#endif

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

#include "app_loader.h"
#include "lib/network.h"
#include "lib/rtems_sys.h"

#define TAG "AtomVM"

#define ATOMVM_BANNER                                                   \
    "\n"                                                                \
    "    ###########################################################\n" \
    "\n"                                                                \
    "       ###    ########  #######  ##     ## ##     ## ##     ## \n" \
    "      ## ##      ##    ##     ## ###   ### ##     ## ###   ### \n" \
    "     ##   ##     ##    ##     ## #### #### ##     ## #### #### \n" \
    "    ##     ##    ##    ##     ## ## ### ## ##     ## ## ### ## \n" \
    "    #########    ##    ##     ## ##     ##  ##   ##  ##     ## \n" \
    "    ##     ##    ##    ##     ## ##     ##   ## ##   ##     ## \n" \
    "    ##     ##    ##     #######  ##     ##    ###    ##     ## \n" \
    "\n"                                                                \
    "    ###########################################################\n" \
    "\n"

extern const uint8_t embedded_avm[];
extern const size_t embedded_avm_size;

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
static bool board_fdt_is_valid(void)
{
    const void *fdt = bsp_fdt_get();
    return fdt != NULL && fdt_check_header(fdt) == 0
        && fdt_node_check_compatible(fdt, 0, RTEMS_BOARD_FDT_COMPATIBLE) == 0;
}
#endif

// The caller retains the pack until all modules and their borrowed pointers are destroyed.
// Return -1 for startup failure; application failures do not trigger the fallback.
static int run_app(const struct RtemsApp *app)
{
    GlobalContext *glb = globalcontext_new();
    if (IS_NULL_PTR(glb)) {
        fprintf(stderr, TAG ": Failed to create GlobalContext\n");
        return -1;
    }

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        fprintf(stderr, TAG ": Memory error: Cannot allocate AVMPackData.\n");
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = app->data;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, app->startup_beam, app->startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, TAG ": Cannot load startup module: %s\n", app->startup_name);
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }
    if (globalcontext_insert_module(glb, mod) < 0) {
        module_destroy(mod);
        nif_collection_destroy_all(glb);
        globalcontext_destroy(glb);
        return -1;
    }

    fprintf(stdout, "Starting: %s...\n", app->startup_name);
    fprintf(stdout, "---\n");

    run_result_t result = globalcontext_run(glb, mod, stdout, 0, NULL);

    nif_collection_destroy_all(glb);
    globalcontext_destroy(glb);

    return result == RUN_SUCCESS ? 0 : 1;
}

rtems_task Init(rtems_task_argument ignored)
{
    UNUSED(ignored);

    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    fprintf(stdout, "%s", ATOMVM_BANNER);
    fprintf(stdout, "Starting AtomVM revision " ATOMVM_VERSION " on RTEMS\n");

#ifdef RTEMS_BOARD_FDT_COMPATIBLE
    if (!board_fdt_is_valid()) {
        fprintf(stderr, TAG ": Bootloader FDT is missing or incompatible with " RTEMS_BOARD_FDT_COMPATIBLE "\n");
        exit(1);
    }
#endif

#ifdef RTEMS_HAS_STORAGE
    bool storage_ready = storage_init();
#endif

    if (rtems_atomvm_network_init() != 0) {
        fprintf(stderr, TAG ": Network stack initialization failed\n");
        exit(1);
    }

#ifdef RTEMS_HAS_STORAGE
    struct RtemsApp file_app;
    if (storage_ready && storage_load_app(&file_app)) {
        int result = run_app(&file_app);
        free((void *) file_app.data);
        if (result >= 0) {
            exit(result);
        }
        fprintf(stderr, TAG ": Cannot start app.avm; using embedded application\n");
    }
#endif

    struct RtemsApp embedded_app;
    if (!rtems_app_validate(embedded_avm, embedded_avm_size, &embedded_app)) {
        fprintf(stderr, TAG ": Invalid or missing embedded AVM pack\n");
        exit(1);
    }
    fprintf(stdout, TAG ": Using embedded application\n");
    exit(run_app(&embedded_app) == 0 ? 0 : 1);
}
