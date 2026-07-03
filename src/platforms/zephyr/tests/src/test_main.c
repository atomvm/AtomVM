/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <zephyr/ztest.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <avmpack.h>
#include <context.h>
#include <globalcontext.h>
#include <module.h>
#include <nifs.h>
#include <defaultatoms.h>

#include "../../src/lib/avm_devcfg.h"
#include "../../src/lib/avm_log.h"
#include "../../src/lib/zephyros_sys.h"

#define TAG "AtomVM"

extern const uint8_t zephyr_test_modules_avm[];
extern const size_t zephyr_test_modules_avm_size;

term avm_test_case(const char *test_module)
{
    GlobalContext *glb = globalcontext_new();
    zassert_not_null(glb, "Failed to create GlobalContext");

    port_driver_init_all(glb);
    nif_collection_init_all(glb);

    zassert_true(avmpack_is_valid(zephyr_test_modules_avm, zephyr_test_modules_avm_size), "Invalid AVM pack");

    struct ConstAVMPack *avmpack_data = malloc(sizeof(struct ConstAVMPack));
    zassert_not_null(avmpack_data, "Failed to allocate AVMPack");

    avmpack_data_init(&avmpack_data->base, &const_avm_pack_info);
    avmpack_data->base.in_use = true;
    avmpack_data->base.data = zephyr_test_modules_avm;
    synclist_append(&glb->avmpack_data, &avmpack_data->base.avmpack_head);

    Module *mod = globalcontext_load_module_from_avm(glb, test_module);
    zassert_not_null(mod, "Failed to load module %s", test_module);

    globalcontext_insert_module(glb, mod);

    Context *ctx = context_new(glb);
    zassert_not_null(ctx, "Failed to create context");
    ctx->leader = 1;

    AVM_LOGI(TAG, "Running start/0 from %s...\n", test_module);

    context_execute_loop(ctx, mod, "start", 0);
    term ret_value = ctx->x[0];

    context_destroy(ctx);

    nif_collection_destroy_all(glb);
    port_driver_destroy_all(glb);

    globalcontext_destroy(glb);

    return ret_value;
}

ZTEST_SUITE(atomvm_tests, NULL, NULL, NULL, NULL, NULL);

ZTEST(atomvm_tests, test_list_to_atom)
{
    term ret_value = avm_test_case("test_list_to_atom.beam");
    zassert_equal(ret_value, OK_ATOM, "test_list_to_atom did not return 'ok'");
}

#if defined(AVM_ZEPHYR_NATIVE_SIM_I2C_TEST)
ZTEST(atomvm_tests, test_i2c_native_sim)
{
    term ret_value = avm_test_case("test_i2c_native_sim.beam");
    zassert_equal(ret_value, OK_ATOM, "test_i2c_native_sim did not return 'ok'");
}
#endif

