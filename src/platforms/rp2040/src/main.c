/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <hardware/regs/addressmap.h>
#include <hardware/watchdog.h>
#include <pico/binary_info.h>
#include <pico/stdlib.h>

#pragma GCC diagnostic pop

#include <avm_version.h>
#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>

#include "rp2040_sys.h"

extern char __flash_binary_end;

#define LIB_AVM ((void *) 0x10100000)
#define MAIN_AVM ((void *) 0x10180000)

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

static int app_main()
{
    bi_decl(bi_program_description("Atom VM"));

    stdio_init_all();

    fprintf(stderr, "%s", ATOMVM_BANNER);
    fprintf(stderr, "Starting AtomVM revision " ATOMVM_VERSION "\n");

    if ((uintptr_t) &__flash_binary_end >= (uintptr_t) LIB_AVM) {
        sleep_ms(5000);
        fprintf(stderr, "This VM is too large (end %p >= LIB_AVM %p), please update LIB_AVM\n", &__flash_binary_end, LIB_AVM);
        AVM_ABORT();
    }

    GlobalContext *glb = globalcontext_new();
    nif_collection_init_all(glb);

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    if (!avmpack_is_valid(MAIN_AVM, XIP_SRAM_BASE - (uintptr_t) MAIN_AVM)) {
        sleep_ms(5000);
        fprintf(stderr, "Fatal error: invalid main.avm packbeam\n");
        AVM_ABORT();
    }
    if (!avmpack_find_section_by_flag(MAIN_AVM, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name)) {
        sleep_ms(5000);
        fprintf(stderr, "Fatal error: Failed to locate start module in main.avm packbeam.  (Did you flash a library by mistake?)");
        AVM_ABORT();
    }
    fprintf(stderr, "Found startup beam %s\n", startup_module_name);
    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    if (IS_NULL_PTR(avmpack_data)) {
        sleep_ms(5000);
        fprintf(stderr, "Fatal memory error: Cannot allocate AVMPackData for main.avm.");
        AVM_ABORT();
    }

    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = MAIN_AVM;
    avmpack_data->base.in_use = true;

    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    if (avmpack_is_valid(LIB_AVM, (uintptr_t) MAIN_AVM - (uintptr_t) LIB_AVM)) {
        avmpack_data = malloc(sizeof(struct ConstAVMPack));
        if (IS_NULL_PTR(avmpack_data)) {
            sleep_ms(5000);
            fprintf(stderr, "Memory error: Cannot allocate AVMPackData for lib.avm.");
            AVM_ABORT();
        }
        avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
        avmpack_data->base.data = LIB_AVM;
        avmpack_data->base.in_use = true;
        synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);
    } else {
        fprintf(stderr, "Warning: invalid lib.avm packbeam\n");
    }

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    if (IS_NULL_PTR(mod)) {
        sleep_ms(5000);
        fprintf(stderr, "Fatal error: unable to load startup module %s", startup_module_name);
        AVM_ABORT();
    }

    globalcontext_insert_module(glb, mod);
    mod->module_platform_data = NULL;
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    fprintf(stderr, "Starting %s...", startup_module_name);
    fprintf(stdout, "---\n");

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");
    int result = ret_value != OK_ATOM;

    context_destroy(ctx);

    nif_collection_destroy_all(glb);
    globalcontext_destroy(glb);

    return result;
}

static void exit_handler(bool reboot)
{
#if defined(CONFIG_REBOOT_ON_NOT_OK)
    reboot = reboot && (CONFIG_REBOOT_ON_NOT_OK ? true : false);
#else
    reboot = false;
#endif
    if (reboot) {
        fprintf(stderr, "AtomVM application terminated with non-ok return value.  Rebooting ...\n");
        watchdog_reboot(0, 0, 200);
    } else {
        fprintf(stderr, "AtomVM application terminated.  Going to sleep forever ...\n");
    }
}

/* newlib stubs to get AVM_ABORT to work */
pid_t _getpid()
{
    return 1; /* init :-) */
}

int _kill(pid_t pid, int sig)
{
    UNUSED(pid);
    if (sig == SIGABRT) {
        fprintf(stderr, "Aborted\n");
    } else {
        fprintf(stderr, "Unknown signal %d\n", sig);
    }
    exit_handler(true);
    return 0;
}

int main()
{
    int reboot = app_main();
    exit_handler(reboot);
    return 0;
}
