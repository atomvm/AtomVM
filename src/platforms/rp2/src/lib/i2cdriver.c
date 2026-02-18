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
#include <pico/time.h>

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
static bool i2c_in_use[NUM_I2C_INSTANCES] = { false, false };

struct I2CResource
{
    i2c_inst_t *i2c_inst;
    uint peripheral_index;
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
    struct I2CResource *rsrc = (struct I2CResource *) rsrc_obj_ptr;
    if (IS_NULL_PTR(rsrc->i2c_inst)) {
        return false;
    }
    *rsrc_obj = rsrc;
    return true;
}

static bool term_to_bool(term t, bool *out)
{
    if (t == TRUE_ATOM) {
        *out = true;
        return true;
    }
    if (t == FALSE_ATOM) {
        *out = false;
        return true;
    }
    return false;
}

static bool term_to_i2c_addr(term t, uint8_t *addr)
{
    if (UNLIKELY(!term_is_integer(t))) {
        return false;
    }
    avm_int_t val = term_to_int(t);
    if (UNLIKELY(val < 0 || val > 127)) {
        return false;
    }
    *addr = (uint8_t) val;
    return true;
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

    avm_int_t baudrate = term_to_int(argv[1]);
    if (UNLIKELY(baudrate <= 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(i2c_in_use[peripheral])) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_error_tuple(ctx, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")));
    }

    i2c_inst_t *inst = i2c_get_instance((uint) peripheral);

    uint actual_baudrate = i2c_init(inst, (uint) baudrate);
    i2c_in_use[peripheral] = true;

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        i2c_in_use[peripheral] = false;
        i2c_deinit(inst);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->i2c_inst = inst;
    rsrc_obj->peripheral_index = (uint) peripheral;

    // Return {ok, {ActualBaudrate, Resource}}
    size_t requested_size = TERM_BOXED_RESOURCE_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        i2c_in_use[peripheral] = false;
        i2c_deinit(inst);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    term inner = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(inner, 0, term_from_int(actual_baudrate));
    term_put_tuple_element(inner, 1, obj);

    return create_pair(ctx, OK_ATOM, inner);
}

static term nif_i2c_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], i2c_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct I2CResource *rsrc_obj = (struct I2CResource *) rsrc_obj_ptr;
    if (!IS_NULL_PTR(rsrc_obj->i2c_inst)) {
        i2c_in_use[rsrc_obj->peripheral_index] = false;
        i2c_deinit(rsrc_obj->i2c_inst);
        rsrc_obj->i2c_inst = NULL;
    }

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

    avm_int_t baudrate = term_to_int(argv[1]);
    if (UNLIKELY(baudrate <= 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint actual = i2c_set_baudrate(rsrc_obj->i2c_inst, (uint) baudrate);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, term_from_int(actual));
}

static term nif_i2c_set_slave_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    bool slave;
    if (UNLIKELY(!term_to_bool(argv[1], &slave))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[2], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    i2c_set_slave_mode(rsrc_obj->i2c_inst, slave, addr);

    return OK_ATOM;
}

// NOTE: All blocking I2C NIF functions below block the Erlang scheduler
// for the duration of the I2C transfer. At typical I2C speeds (100-400kHz),
// a short transfer completes in hundreds of microseconds. Longer transfers
// or clock-stretching devices may block for longer.

static term nif_i2c_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_binary);
    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }

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
    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

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

static term nif_i2c_write_blocking_until(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);

    VALIDATE_VALUE(argv[4], term_is_int64);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t deadline_us = term_to_int64(argv[4]);
    if (UNLIKELY(deadline_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    absolute_time_t until = from_us_since_boot((uint64_t) deadline_us);
    int ret = i2c_write_blocking_until(rsrc_obj->i2c_inst, addr, data, len, nostop, until);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return term_from_int(ret);
}

static term nif_i2c_read_blocking_until(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    VALIDATE_VALUE(argv[4], term_is_int64);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t deadline_us = term_to_int64(argv[4]);

    if (UNLIKELY(count < 0 || deadline_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    absolute_time_t until = from_us_since_boot((uint64_t) deadline_us);
    int ret = i2c_read_blocking_until(rsrc_obj->i2c_inst, addr, buf, (size_t) count, nostop, until);
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

    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t timeout_us = term_to_int(argv[4]);
    if (UNLIKELY(timeout_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ret = i2c_write_timeout_us(rsrc_obj->i2c_inst, addr, data, len, nostop, (uint) timeout_us);
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

    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t timeout_us = term_to_int(argv[4]);

    if (UNLIKELY(count < 0 || timeout_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = i2c_read_timeout_us(rsrc_obj->i2c_inst, addr, buf, (size_t) count, nostop, (uint) timeout_us);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_write_timeout_per_char_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);

    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t timeout_per_char_us = term_to_int(argv[4]);
    if (UNLIKELY(timeout_per_char_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ret = i2c_write_timeout_per_char_us(rsrc_obj->i2c_inst, addr, data, len, nostop, (uint) timeout_per_char_us);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return term_from_int(ret);
}

static term nif_i2c_read_timeout_per_char_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    VALIDATE_VALUE(argv[4], term_is_integer);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);
    bool nostop;
    if (UNLIKELY(!term_to_bool(argv[3], &nostop))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t timeout_per_char_us = term_to_int(argv[4]);

    if (UNLIKELY(count < 0 || timeout_per_char_us < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = i2c_read_timeout_per_char_us(rsrc_obj->i2c_inst, addr, buf, (size_t) count, nostop, (uint) timeout_per_char_us);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_write_burst_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);

    int ret = i2c_write_burst_blocking(rsrc_obj->i2c_inst, addr, data, len);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return term_from_int(ret);
}

static term nif_i2c_read_burst_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    uint8_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = i2c_read_burst_blocking(rsrc_obj->i2c_inst, addr, buf, (size_t) count);
    if (UNLIKELY(ret < 0)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return pico_err_to_error_tuple(ctx, ret);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_write_raw_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    i2c_write_raw_blocking(rsrc_obj->i2c_inst, data, len);

    return OK_ATOM;
}

static term nif_i2c_read_raw_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    avm_int_t count = term_to_int(argv[1]);
    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    i2c_read_raw_blocking(rsrc_obj->i2c_inst, buf, (size_t) count);

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_get_write_available(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t avail = i2c_get_write_available(rsrc_obj->i2c_inst);
    return term_from_int((avm_int_t) avail);
}

static term nif_i2c_get_read_available(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t avail = i2c_get_read_available(rsrc_obj->i2c_inst);
    return term_from_int((avm_int_t) avail);
}

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->i2c_inst)) {
        i2c_in_use[rsrc_obj->peripheral_index] = false;
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
static const struct Nif i2c_set_slave_mode_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_set_slave_mode
};
static const struct Nif i2c_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_blocking
};
static const struct Nif i2c_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_blocking
};
static const struct Nif i2c_write_blocking_until_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_blocking_until
};
static const struct Nif i2c_read_blocking_until_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_blocking_until
};
static const struct Nif i2c_write_timeout_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_timeout_us
};
static const struct Nif i2c_read_timeout_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_timeout_us
};
static const struct Nif i2c_write_timeout_per_char_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_timeout_per_char_us
};
static const struct Nif i2c_read_timeout_per_char_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_timeout_per_char_us
};
static const struct Nif i2c_write_burst_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_burst_blocking
};
static const struct Nif i2c_read_burst_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_burst_blocking
};
static const struct Nif i2c_write_raw_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_write_raw_blocking
};
static const struct Nif i2c_read_raw_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_read_raw_blocking
};
static const struct Nif i2c_get_write_available_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_get_write_available
};
static const struct Nif i2c_get_read_available_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_get_read_available
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
    if (strcmp("set_slave_mode/3", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_set_slave_mode_nif;
    }
    if (strcmp("write_blocking/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_blocking_nif;
    }
    if (strcmp("read_blocking/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_blocking_nif;
    }
    if (strcmp("write_blocking_until/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_blocking_until_nif;
    }
    if (strcmp("read_blocking_until/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_blocking_until_nif;
    }
    if (strcmp("write_timeout_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_timeout_us_nif;
    }
    if (strcmp("read_timeout_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_timeout_us_nif;
    }
    if (strcmp("write_timeout_per_char_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_timeout_per_char_us_nif;
    }
    if (strcmp("read_timeout_per_char_us/5", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_timeout_per_char_us_nif;
    }
    if (strcmp("write_burst_blocking/3", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_burst_blocking_nif;
    }
    if (strcmp("read_burst_blocking/3", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_burst_blocking_nif;
    }
    if (strcmp("write_raw_blocking/2", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_write_raw_blocking_nif;
    }
    if (strcmp("read_raw_blocking/2", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_read_raw_blocking_nif;
    }
    if (strcmp("get_write_available/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_get_write_available_nif;
    }
    if (strcmp("get_read_available/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_get_read_available_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(i2c, i2c_nif_init, NULL, i2c_nif_get_nif)
