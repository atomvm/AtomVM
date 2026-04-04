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
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <hardware/spi.h>

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

#define NUM_SPI_INSTANCES 2

static ErlNifResourceType *spi_resource_type;
static bool spi_in_use[NUM_SPI_INSTANCES];

struct SPIResource
{
    spi_inst_t *spi_inst;
    int peripheral;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static bool get_spi_resource(Context *ctx, term resource_term, struct SPIResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, spi_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct SPIResource *) rsrc_obj_ptr;
    return (*rsrc_obj)->spi_inst != NULL;
}

static term nif_spi_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    int peripheral = term_to_int(argv[0]);
    if (UNLIKELY(peripheral < 0 || peripheral >= NUM_SPI_INSTANCES)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(spi_in_use[peripheral])) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "busy")));
    }

    uint baudrate = (uint) term_to_int(argv[1]);
    spi_inst_t *inst = (peripheral == 0) ? spi0 : spi1;

    uint actual_baudrate = spi_init(inst, baudrate);
    spi_in_use[peripheral] = true;

    struct SPIResource *rsrc_obj = enif_alloc_resource(spi_resource_type, sizeof(struct SPIResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        spi_deinit(inst);
        spi_in_use[peripheral] = false;
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->spi_inst = inst;
    rsrc_obj->peripheral = peripheral;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        spi_deinit(inst);
        spi_in_use[peripheral] = false;
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term inner = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(inner, 0, term_from_int(actual_baudrate));
    term_put_tuple_element(inner, 1, obj);

    return create_pair(ctx, OK_ATOM, inner);
}

static term nif_spi_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], spi_resource_type, &rsrc_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SPIResource *rsrc_obj = (struct SPIResource *) rsrc_obj_ptr;

    if (rsrc_obj->spi_inst != NULL) {
        spi_deinit(rsrc_obj->spi_inst);
        rsrc_obj->spi_inst = NULL;
        spi_in_use[rsrc_obj->peripheral] = false;
    }

    return OK_ATOM;
}

static term nif_spi_set_baudrate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    uint baudrate = (uint) term_to_int(argv[1]);
    uint actual = spi_set_baudrate(rsrc_obj->spi_inst, baudrate);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, term_from_int(actual));
}

static term nif_spi_set_format(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);

    uint data_bits = (uint) term_to_int(argv[1]);
    avm_int_t cpol_val = term_to_int(argv[2]);
    avm_int_t cpha_val = term_to_int(argv[3]);

    if (UNLIKELY(data_bits < 4 || data_bits > 16)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(cpol_val < 0 || cpol_val > 1 || cpha_val < 0 || cpha_val > 1)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    spi_set_format(rsrc_obj->spi_inst, data_bits, (spi_cpol_t) cpol_val, (spi_cpha_t) cpha_val, SPI_MSB_FIRST);

    return OK_ATOM;
}

static term nif_spi_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    int ret = spi_write_blocking(rsrc_obj->spi_inst, data, len);

    return term_from_int(ret);
}

static term nif_spi_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    uint8_t repeated_tx_data = (uint8_t) term_to_int(argv[1]);
    avm_int_t count = term_to_int(argv[2]);

    if (UNLIKELY(count < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int ret = spi_read_blocking(rsrc_obj->spi_inst, repeated_tx_data, buf, (size_t) count);
    if (UNLIKELY(ret != count)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_spi_write_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *src = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(len), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    uint8_t *dst = (uint8_t *) term_binary_data(data);

    int ret = spi_write_read_blocking(rsrc_obj->spi_inst, src, dst, len);
    if (UNLIKELY(ret != (int) len)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_spi_set_slave(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(argv[1] != TRUE_ATOM && argv[1] != FALSE_ATOM)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    bool slave = (argv[1] == TRUE_ATOM);
    spi_set_slave(rsrc_obj->spi_inst, slave);

    return OK_ATOM;
}

static term nif_spi_is_writable(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return spi_is_writable(rsrc_obj->spi_inst) ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_spi_is_readable(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return spi_is_readable(rsrc_obj->spi_inst) ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_spi_is_busy(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return spi_is_busy(rsrc_obj->spi_inst) ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_spi_get_baudrate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint baudrate = spi_get_baudrate(rsrc_obj->spi_inst);

    return term_from_int(baudrate);
}

static term nif_spi_write16_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t byte_len = term_binary_size(argv[1]);

    if (UNLIKELY(byte_len % 2 != 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t count = byte_len / 2;
    int ret;
    if (((uintptr_t) data % _Alignof(uint16_t)) != 0) {
        uint16_t *tmp = malloc(byte_len);
        if (IS_NULL_PTR(tmp)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        memcpy(tmp, data, byte_len);
        ret = spi_write16_blocking(rsrc_obj->spi_inst, tmp, count);
        free(tmp);
    } else {
        ret = spi_write16_blocking(rsrc_obj->spi_inst, (const uint16_t *) data, count);
    }

    return term_from_int(ret);
}

static term nif_spi_read16_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    uint16_t repeated_tx_data = (uint16_t) term_to_int(argv[1]);
    avm_int_t count = term_to_int(argv[2]);

    if (UNLIKELY(count < 0 || (size_t) count > SIZE_MAX / 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t byte_len = (size_t) count * 2;
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(byte_len), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(byte_len, &ctx->heap, ctx->global);
    uint8_t *raw_buf = (uint8_t *) term_binary_data(data);

    if (((uintptr_t) raw_buf % _Alignof(uint16_t)) != 0) {
        uint16_t *tmp = malloc(byte_len);
        if (IS_NULL_PTR(tmp)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        int ret = spi_read16_blocking(rsrc_obj->spi_inst, repeated_tx_data, tmp, (size_t) count);
        memcpy(raw_buf, tmp, byte_len);
        free(tmp);
        if (UNLIKELY(ret != count)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        int ret = spi_read16_blocking(rsrc_obj->spi_inst, repeated_tx_data, (uint16_t *) raw_buf, (size_t) count);
        if (UNLIKELY(ret != count)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_spi_write_read16_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *src = (const uint8_t *) term_binary_data(argv[1]);
    size_t byte_len = term_binary_size(argv[1]);

    if (UNLIKELY(byte_len % 2 != 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t count = byte_len / 2;
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(byte_len), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(byte_len, &ctx->heap, ctx->global);
    uint8_t *dst_raw = (uint8_t *) term_binary_data(data);

    bool src_unaligned = ((uintptr_t) src % _Alignof(uint16_t)) != 0;
    bool dst_unaligned = ((uintptr_t) dst_raw % _Alignof(uint16_t)) != 0;

    if (src_unaligned || dst_unaligned) {
        uint16_t *src_buf = NULL;
        uint16_t *dst_buf = NULL;
        const uint16_t *src_ptr;
        uint16_t *dst_ptr;

        if (src_unaligned) {
            src_buf = malloc(byte_len);
            if (IS_NULL_PTR(src_buf)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            memcpy(src_buf, src, byte_len);
            src_ptr = src_buf;
        } else {
            src_ptr = (const uint16_t *) src;
        }

        if (dst_unaligned) {
            dst_buf = malloc(byte_len);
            if (IS_NULL_PTR(dst_buf)) {
                free(src_buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            dst_ptr = dst_buf;
        } else {
            dst_ptr = (uint16_t *) dst_raw;
        }

        int ret = spi_write16_read16_blocking(rsrc_obj->spi_inst, src_ptr, dst_ptr, count);

        if (dst_unaligned) {
            memcpy(dst_raw, dst_buf, byte_len);
            free(dst_buf);
        }
        free(src_buf);
        if (UNLIKELY(ret != (int) count)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        int ret = spi_write16_read16_blocking(rsrc_obj->spi_inst, (const uint16_t *) src, (uint16_t *) dst_raw, count);
        if (UNLIKELY(ret != (int) count)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    return create_pair(ctx, OK_ATOM, data);
}

static void spi_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct SPIResource *rsrc_obj = (struct SPIResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->spi_inst)) {
        spi_deinit(rsrc_obj->spi_inst);
        rsrc_obj->spi_inst = NULL;
        spi_in_use[rsrc_obj->peripheral] = false;
    }
}

static const ErlNifResourceTypeInit SPIResourceTypeInit = {
    .members = 1,
    .dtor = spi_resource_dtor,
};

//
// NIF structs
//
static const struct Nif spi_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_init
};
static const struct Nif spi_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_deinit
};
static const struct Nif spi_set_baudrate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_set_baudrate
};
static const struct Nif spi_set_format_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_set_format
};
static const struct Nif spi_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_blocking
};
static const struct Nif spi_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_read_blocking
};
static const struct Nif spi_write_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_read_blocking
};
static const struct Nif spi_set_slave_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_set_slave
};
static const struct Nif spi_is_writable_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_is_writable
};
static const struct Nif spi_is_readable_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_is_readable
};
static const struct Nif spi_is_busy_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_is_busy
};
static const struct Nif spi_get_baudrate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_get_baudrate
};
static const struct Nif spi_write16_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write16_blocking
};
static const struct Nif spi_read16_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_read16_blocking
};
static const struct Nif spi_write_read16_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_read16_blocking
};

static void spi_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    spi_resource_type = enif_init_resource_type(&env, "spi_resource", &SPIResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *spi_nif_get_nif(const char *nifname)
{
    if (strncmp("spi:", nifname, 4) != 0) {
        return NULL;
    }
    const char *rest = nifname + 4;
    if (strcmp("init/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_deinit_nif;
    }
    if (strcmp("set_baudrate/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_set_baudrate_nif;
    }
    if (strcmp("set_format/4", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_set_format_nif;
    }
    if (strcmp("write_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write_blocking_nif;
    }
    if (strcmp("read_blocking/3", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_read_blocking_nif;
    }
    if (strcmp("write_read_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write_read_blocking_nif;
    }
    if (strcmp("set_slave/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_set_slave_nif;
    }
    if (strcmp("is_writable/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_is_writable_nif;
    }
    if (strcmp("is_readable/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_is_readable_nif;
    }
    if (strcmp("is_busy/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_is_busy_nif;
    }
    if (strcmp("get_baudrate/1", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_get_baudrate_nif;
    }
    if (strcmp("write16_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write16_blocking_nif;
    }
    if (strcmp("read16_blocking/3", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_read16_blocking_nif;
    }
    if (strcmp("write_read16_blocking/2", rest) == 0) {
        TRACE("Resolved spi nif %s ...\n", nifname);
        return &spi_write_read16_blocking_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(spi, spi_nif_init, NULL, spi_nif_get_nif)
