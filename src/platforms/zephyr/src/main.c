
/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Winford (Uncle Grumpy) <winford@object.stream>
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

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include <zephyr/kernel.h>
#include <zephyr/sys/reboot.h>

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <module.h>
#include <utils.h>

#include "lib/avm_devcfg.h"
#include "lib/avm_log.h"
#include "lib/zephyros_sys.h"

#define AVM_ADDRESS (AVM_APP_ADDRESS)
#define AVM_FLASH_END (CFG_FLASH_END)

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

int main()
{

    GlobalContext *glb = globalcontext_new();

    printk("%s", ATOMVM_BANNER);
    AVM_LOGI(TAG, "Starting AtomVM revision %s", ATOMVM_VERSION);

    const void *flashed_avm = (void *) AVM_ADDRESS;
    uint32_t size = (AVM_FLASH_END - AVM_ADDRESS);

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    AVM_LOGD(TAG, "Maximum application size: %lu KiB", (size / 1024));

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    if (!avmpack_is_valid(flashed_avm, size) || !avmpack_find_section_by_flag(flashed_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        AVM_LOGE(TAG, "Invalid AVM Pack");
        AVM_ABORT();
    }
    AVM_LOGD(TAG, "Booting file mapped at: %p, size: %lu", flashed_avm, startup_beam_size);
    AVM_LOGI(TAG, "Application size: %lu B, free flash space: %lu KiB", startup_beam_size, ((size - startup_beam_size) / 1024));

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        AVM_LOGE(TAG, "Memory error: Cannot allocate AVMPackData.");
        AVM_ABORT();
    }
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = flashed_avm;
    avmpack_data->base.in_use = true;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    globalcontext_insert_module(glb, mod);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    printk("Starting: %s...\n", startup_module_name);
    printk("---\n");

    context_execute_loop(ctx, mod, "start", 0);

    term ret_value = ctx->x[0];
    char *ret_atom_string = interop_atom_to_string(ctx, ret_value);
    if (ret_atom_string != NULL) {
        AVM_LOGI(TAG, "Exited with return: %s", ret_atom_string);
    } else {
        AVM_LOGI(TAG, "Exited with return value: %lx", (long) term_to_int32(ret_value));
    }
    free(ret_atom_string);

    bool reboot_on_not_ok =
#if defined(CONFIG_REBOOT_ON_NOT_OK)
        CONFIG_REBOOT_ON_NOT_OK ? true : false;
#else
        false;
#endif
    if (reboot_on_not_ok && ret_value != OK_ATOM) {
        AVM_LOGE(TAG, "AtomVM application terminated with non-ok return value.  Rebooting ...");
        sys_reboot(SYS_REBOOT_COLD);
    } else {
        AVM_LOGI(TAG, "AtomVM application terminated.  Going to sleep forever ...");
        while (1) {
            ;
        }
    }
    return 0;
}
