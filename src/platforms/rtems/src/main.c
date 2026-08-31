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

#include <stdio.h>
#include <stdlib.h>

#include <rtems.h>

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

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

rtems_task Init(rtems_task_argument ignored)
{
    UNUSED(ignored);

    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);

    fprintf(stdout, "%s", ATOMVM_BANNER);
    fprintf(stdout, "Starting AtomVM revision " ATOMVM_VERSION " on RTEMS\n");

    GlobalContext *glb = globalcontext_new();
    if (IS_NULL_PTR(glb)) {
        fprintf(stderr, TAG ": Failed to create GlobalContext\n");
        exit(1);
    }

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (embedded_avm_size == 0 || !avmpack_is_valid(embedded_avm, embedded_avm_size)) {
        fprintf(stderr, TAG ": Invalid or missing embedded AVM pack\n");
        exit(1);
    }

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;
    if (!avmpack_find_section_by_flag(embedded_avm, BEAM_START_FLAG, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        fprintf(stderr, TAG ": Failed to locate start module in embedded AVM pack\n");
        exit(1);
    }

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        fprintf(stderr, TAG ": Memory error: Cannot allocate AVMPackData.\n");
        exit(1);
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = embedded_avm;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, TAG ": Cannot load startup module: %s\n", startup_module_name);
        exit(1);
    }
    globalcontext_insert_module(glb, mod);

    fprintf(stdout, "Starting: %s...\n", startup_module_name);
    fprintf(stdout, "---\n");

    run_result_t result = globalcontext_run(glb, mod, stdout, 0, NULL);

    nif_collection_destroy_all(glb);
    globalcontext_destroy(glb);

    if (result == RUN_SUCCESS) {
        exit(0);
    }
    exit(1);
}
