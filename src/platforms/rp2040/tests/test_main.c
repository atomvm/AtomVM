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

#include "unity.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <hardware/regs/addressmap.h>
#include <hardware/watchdog.h>
#include <pico/binary_info.h>
#include <pico/stdlib.h>

#pragma GCC diagnostic pop

#include <avmpack.h>
#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <iff.h>
#include <module.h>
#include <sys.h>

#include "platform_defaultatoms.h"
#include "rp2040_sys.h"

#ifndef AVM_NO_SMP
#include "smp.h"
#endif

const struct Nif *platform_nifs_get_nif(const char *nifname);

void setUp()
{
}

void tearDown()
{
}

struct TestCase
{
    const char *name;
    void (*func)(void);
    int line;
    struct TestCase *next;
};

struct TestCase *test_case_list = NULL;

#define TEST_CASE(TEST_NAME)                                           \
    static void TEST_NAME##_test(void);                                \
    struct TestCase TEST_NAME##_test_case = {                          \
        .name = #TEST_NAME,                                            \
        .func = TEST_NAME##_test,                                      \
        .line = __LINE__                                               \
    };                                                                 \
    __attribute__((constructor)) void TEST_NAME##_test_case_register() \
    {                                                                  \
        TEST_NAME##_test_case.next = test_case_list;                   \
        test_case_list = &TEST_NAME##_test_case;                       \
    }                                                                  \
    static void TEST_NAME##_test(void)

static void unity_run_all_tests()
{
    for (struct TestCase *test_case = test_case_list; test_case != NULL; test_case = test_case->next) {
        UnityDefaultTestRun(test_case->func, test_case->name, test_case->line);
    }
}

#define MAIN_AVM ((void *) 0x10100000)

static term avm_test_case(const char *test_module)
{
    GlobalContext *glb = globalcontext_new();
    TEST_ASSERT_NOT_NULL(glb);

    nif_collection_init_all(glb);

    if (!avmpack_is_valid(MAIN_AVM, XIP_SRAM_BASE - (uintptr_t) MAIN_AVM)) {
        TEST_FAIL_MESSAGE("Fatal error: invalid main.avm packbeam");
        return term_invalid_term();
    }

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    TEST_ASSERT_NOT_NULL(avmpack_data);
    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.data = MAIN_AVM;
    avmpack_data->base.in_use = true;

    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = sys_load_module(glb, test_module);
    TEST_ASSERT(mod != NULL);

    globalcontext_insert_module(glb, mod);

    Context *ctx = context_new(glb);
    TEST_ASSERT(ctx != NULL);
    ctx->leader = 1;

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    nif_collection_destroy_all(glb);

    globalcontext_destroy(glb);

    return ret_value;
}

TEST_CASE(test_atomvm_platform0)
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:platform/0");
    TEST_ASSERT_EQUAL_INT(PICO_ATOM, nif->nif_ptr(NULL, 0, NULL));
}

TEST_CASE(test_atomvm_missing0)
{
    const struct Nif *nif = platform_nifs_get_nif("atomvm:missing/0");
    TEST_ASSERT_EQUAL_PTR(NULL, nif);
}

#ifndef AVM_NO_SMP
TEST_CASE(atomvm_smp_0)
{
    int cores = smp_get_online_processors();
    TEST_ASSERT_EQUAL_INT(2, cores);
}
#endif

TEST_CASE(test_clocks)
{
    term ret_value = avm_test_case("test_clocks.beam");
    TEST_ASSERT_EQUAL_INT(OK_ATOM, ret_value);
}

TEST_CASE(test_crypto)
{
    term ret_value = avm_test_case("test_crypto.beam");
    TEST_ASSERT_EQUAL_INT(OK_ATOM, ret_value);
}

TEST_CASE(test_smp)
{
    term ret_value = avm_test_case("test_smp.beam");
    TEST_ASSERT_EQUAL_INT(OK_ATOM, ret_value);
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
        TEST_FAIL_MESSAGE("Aborted");
    } else {
        char signal_msg[25];
        snprintf(signal_msg, sizeof(signal_msg), "Unknown signal %d", sig);
        TEST_FAIL_MESSAGE(signal_msg);
    }
    return 0;
}

int main()
{
    stdio_init_all();
    UNITY_BEGIN();
    unity_run_all_tests();
    return UNITY_END();
}
