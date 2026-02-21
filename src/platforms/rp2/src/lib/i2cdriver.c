/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
#include <string.h>

#include <hardware/i2c.h>

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "interop.h"
#include "memory.h"
#include "nifs.h"
#include "rp2_sys.h"
#include "term.h"

// #define ENABLE_TRACE
#include "trace.h"

#define NUM_I2C_INSTANCES 2

static ErlNifResourceType *i2c_resource_type;

struct I2CResource
{
    i2c_inst_t *i2c_inst;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term create_error_tuple(Context *ctx, term reason)
{
    return create_pair(ctx, ERROR_ATOM, reason);
}

static term pico_err_to_error_tuple(Context *ctx, int err)
{
    if (err == PICO_ERROR_TIMEOUT) {
        return create_error_tuple(ctx, TIMEOUT_ATOM);
    }
    return create_error_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "eio")));
}

static bool get_i2c_resource(Context *ctx, term resource_term, struct I2CResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, i2c_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct I2CResource *) rsrc_obj_ptr;
    return true;
}

static bool term_to_nostop(term t)
{
    if (UNLIKELY(!term_is_atom(t))) {
        return false;
    }
    return t == TRUE_ATOM;
}

static term nif_i2c_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    int peripheral = term_to_int(argv[0]);
    if (UNLIKELY(peripheral < 0 || peripheral >= NUM_I2C_INSTANCES)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint baudrate = (uint) term_to_int(argv[1]);
    i2c_inst_t *inst = i2c_get_instance((uint) peripheral);

    uint actual_baudrate = i2c_init(inst, baudrate);

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        i2c_deinit(inst);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->i2c_inst = inst;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        i2c_deinit(inst);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
    enif_release_resource(rsrc_obj);

    // Return {ok, {ActualBaudrate, Resource}}
    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term inner = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(inner, 0, term_from_int(actual_baudrate));
    term_put_tuple_element(inner, 1, obj);

    return create_pair(ctx, OK_ATOM, inner);
}

static term nif_i2c_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    i2c_deinit(rsrc_obj->i2c_inst);
    rsrc_obj->i2c_inst = NULL;

    return OK_ATOM;
}

static term nif_i2c_set_baudrate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    uint baudrate = (uint) term_to_int(argv[1]);
    uint actual = i2c_set_baudrate(rsrc_obj->i2c_inst, baudrate);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, term_from_int(actual));
}

static term nif_i2c_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);
    VALIDATE_VALUE(argv[3], term_is_atom);

    uint8_t addr = (uint8_t) term_to_int(argv[1]);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop = term_to_nostop(argv[3]);

    int ret = i2c_write_blocking(rsrc_obj->i2c_inst, addr, data, len, nostop);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return term_from_int(ret);
}

static term nif_i2c_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_atom);

    uint8_t addr = (uint8_t) term_to_int(argv[1]);
    avm_int_t count = term_to_int(argv[2]);
    bool nostop = term_to_nostop(argv[3]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Allocate {ok, Data} tuple and binary
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = i2c_read_blocking(rsrc_obj->i2c_inst, addr, buf, (size_t) count, nostop);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_write_timeout_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);
    VALIDATE_VALUE(argv[3], term_is_atom);
    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr = (uint8_t) term_to_int(argv[1]);
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop = term_to_nostop(argv[3]);
    uint timeout_us = (uint) term_to_int(argv[4]);

    int ret = i2c_write_timeout_us(rsrc_obj->i2c_inst, addr, data, len, nostop, timeout_us);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return term_from_int(ret);
}

static term nif_i2c_read_timeout_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_atom);
    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr = (uint8_t) term_to_int(argv[1]);
    avm_int_t count = term_to_int(argv[2]);
    bool nostop = term_to_nostop(argv[3]);
    uint timeout_us = (uint) term_to_int(argv[4]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Allocate {ok, Data} tuple and binary
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = i2c_read_timeout_us(rsrc_obj->i2c_inst, addr, buf, (size_t) count, nostop, timeout_us);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->i2c_inst)) {
        i2c_deinit(rsrc_obj->i2c_inst);
        rsrc_obj->i2c_inst = NULL;
    }
}

static const ErlNifResourceTypeInit I2CResourceTypeInit = {
    .members = 1,
    .dtor = i2c_resource_dtor,
};

//
// NIF structs
//
static const struct Nif i2c_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_init
};
static const struct Nif i2c_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_deinit
};
static const struct Nif i2c_set_baudrate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_set_baudrate
};
static const struct Nif i2c_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_blocking
};
static const struct Nif i2c_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_blocking
};
static const struct Nif i2c_write_timeout_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_timeout_us
};
static const struct Nif i2c_read_timeout_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_timeout_us
};

static void i2c_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    i2c_resource_type = enif_init_resource_type(&env, "i2c_resource", &I2CResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *i2c_nif_get_nif(const char *nifname)
{
    if (strncmp("i2c:", nifname, 4) != 0) {
        return NULL;
    }
    const char *rest = nifname + 4;
    if (strcmp("init/2", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_deinit_nif;
    }
    if (strcmp("set_baudrate/2", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_set_baudrate_nif;
    }
    if (strcmp("write_blocking/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_blocking_nif;
    }
    if (strcmp("read_blocking/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_blocking_nif;
    }
    if (strcmp("write_timeout_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_timeout_us_nif;
    }
    if (strcmp("read_timeout_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_timeout_us_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(i2c, i2c_nif_init, NULL, i2c_nif_get_nif)
