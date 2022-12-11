/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Davide Bettio <davide@uninstall.it>
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

#include "unity.h"
#include <stdio.h>
#include <string.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <nifs.h>
#include <platform_defaultatoms.h>

#include <esp32_sys.h>

#include <esp_log.h>

// test_avm_otp21.h contains an .avm file encoded as C array
// in order to generate it, erl sources in test_erl_sources must be compiled and the BEAM files
// must be packed together.
#include "test_avm_otp21.h"

#ifndef AVM_NO_SMP
#include "smp.h"
#endif

#define TAG "AtomVM"

const struct Nif *platform_nifs_get_nif(const char *nifname);

TEST_CASE("atomvm_platform_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:platform/0");
    TEST_ASSERT(nif->nif_ptr(NULL, 0, NULL) == ESP32_ATOM);
}

TEST_CASE("atomvm_missing_0", "[platform_nifs]")
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:missing/0");
    TEST_ASSERT(nif == NULL);
}

TEST_CASE("test_timers_and_messages", "[test_run]")
{
    esp32_sys_queue_init();

    int size = sizeof(test_avm_otp21);
    const void *main_avm = test_avm_otp21;

    uint32_t startup_beam_size;
    const void *startup_beam;
    const char *startup_module_name;

    GlobalContext *glb = globalcontext_new();
    TEST_ASSERT(glb != NULL);

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    ESP_LOGI(TAG, "Testing avm\n");

    TEST_ASSERT(avmpack_is_valid(main_avm, size) != 0);
    TEST_ASSERT(avmpack_find_section_by_flag(main_avm, BEAM_START_FLAG, &startup_beam, &startup_beam_size, &startup_module_name) != 0);

    struct AVMPackData *avmpack_data = malloc(sizeof(struct AVMPackData));
    TEST_ASSERT(avmpack_data != NULL);

    avmpack_data->data = main_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);
    glb->avmpack_platform_data = NULL;

    Module *mod = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
    TEST_ASSERT(mod != NULL);

    globalcontext_insert_module_with_filename(glb, mod, startup_module_name);

    Context *ctx = context_new(glb);
    TEST_ASSERT(ctx != NULL);
    ctx->leader = 1;

    ESP_LOGI(TAG, "Starting %s...\n", startup_module_name);

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    fprintf(stdout, "AtomVM finished with return value: ");
    term_display(stdout, ret_value, ctx);
    fprintf(stdout, "\n");

    TEST_ASSERT(term_to_int(ret_value) == 6);
}

#ifndef AVM_NO_SMP
TEST_CASE("atomvm_smp_0", "[smp]")
{
    int cores = smp_get_online_processors();
    ESP_LOGI(TAG, "Got %i cores\n", cores);
    TEST_ASSERT(cores == 2);
}
#endif

void app_main(void)
{
    UNITY_BEGIN();
    unity_run_all_tests();
    UNITY_END();
}
