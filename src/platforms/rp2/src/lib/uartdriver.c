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

#include <hardware/uart.h>

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

#define NUM_UART_INSTANCES 2

static ErlNifResourceType *uart_resource_type;

struct UARTResource
{
    uart_inst_t *uart_inst;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static bool get_uart_resource(Context *ctx, term resource_term, struct UARTResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, uart_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct UARTResource *) rsrc_obj_ptr;
    return true;
}

static term nif_uart_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);

    int peripheral = term_to_int(argv[0]);
    if (UNLIKELY(peripheral < 0 || peripheral >= NUM_UART_INSTANCES)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint baudrate = (uint) term_to_int(argv[1]);
    uart_inst_t *inst = uart_get_instance((uint) peripheral);

    uint actual_baudrate = uart_init(inst, baudrate);

    struct UARTResource *rsrc_obj = enif_alloc_resource(uart_resource_type, sizeof(struct UARTResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        uart_deinit(inst);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->uart_inst = inst;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        uart_deinit(inst);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
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

static term nif_uart_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uart_deinit(rsrc_obj->uart_inst);
    rsrc_obj->uart_inst = NULL;

    return OK_ATOM;
}

static term nif_uart_set_baudrate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    uint baudrate = (uint) term_to_int(argv[1]);
    uint actual = uart_set_baudrate(rsrc_obj->uart_inst, baudrate);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, term_from_int(actual));
}

static term nif_uart_set_format(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_atom);

    uint data_bits = (uint) term_to_int(argv[1]);
    uint stop_bits = (uint) term_to_int(argv[2]);

    term parity_term = argv[3];
    uart_parity_t parity;
    if (parity_term == globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "none"))) {
        parity = UART_PARITY_NONE;
    } else if (parity_term == globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "even"))) {
        parity = UART_PARITY_EVEN;
    } else if (parity_term == globalcontext_make_atom(ctx->global, ATOM_STR("\x3", "odd"))) {
        parity = UART_PARITY_ODD;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    uart_set_format(rsrc_obj->uart_inst, data_bits, stop_bits, parity);

    return OK_ATOM;
}

static term nif_uart_set_hw_flow(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_atom);
    VALIDATE_VALUE(argv[2], term_is_atom);

    bool cts = (argv[1] == TRUE_ATOM);
    bool rts = (argv[2] == TRUE_ATOM);

    uart_set_hw_flow(rsrc_obj->uart_inst, cts, rts);

    return OK_ATOM;
}

static term nif_uart_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);

    uart_write_blocking(rsrc_obj->uart_inst, data, len);

    return OK_ATOM;
}

static term nif_uart_is_readable(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    bool readable = uart_is_readable(rsrc_obj->uart_inst);
    return readable ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_uart_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
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

    uart_read_blocking(rsrc_obj->uart_inst, buf, (size_t) count);

    return create_pair(ctx, OK_ATOM, data);
}

static term nif_uart_is_readable_within_us(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    uint32_t us = (uint32_t) term_to_int(argv[1]);
    bool readable = uart_is_readable_within_us(rsrc_obj->uart_inst, us);
    return readable ? TRUE_ATOM : FALSE_ATOM;
}

static void uart_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct UARTResource *rsrc_obj = (struct UARTResource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->uart_inst)) {
        uart_deinit(rsrc_obj->uart_inst);
        rsrc_obj->uart_inst = NULL;
    }
}

static const ErlNifResourceTypeInit UARTResourceTypeInit = {
    .members = 1,
    .dtor = uart_resource_dtor,
};

//
// NIF structs
//
static const struct Nif uart_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_init
};
static const struct Nif uart_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_deinit
};
static const struct Nif uart_set_baudrate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_set_baudrate
};
static const struct Nif uart_set_format_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_set_format
};
static const struct Nif uart_set_hw_flow_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_set_hw_flow
};
static const struct Nif uart_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_write_blocking
};
static const struct Nif uart_is_readable_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_is_readable
};
static const struct Nif uart_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_read_blocking
};
static const struct Nif uart_is_readable_within_us_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_is_readable_within_us
};

static void uart_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    uart_resource_type = enif_init_resource_type(&env, "uart_resource", &UARTResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *uart_nif_get_nif(const char *nifname)
{
    if (strncmp("uart:", nifname, 5) != 0) {
        return NULL;
    }
    const char *rest = nifname + 5;
    if (strcmp("init/2", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_deinit_nif;
    }
    if (strcmp("set_baudrate/2", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_set_baudrate_nif;
    }
    if (strcmp("set_format/4", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_set_format_nif;
    }
    if (strcmp("set_hw_flow/3", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_set_hw_flow_nif;
    }
    if (strcmp("write_blocking/2", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_write_blocking_nif;
    }
    if (strcmp("is_readable/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_is_readable_nif;
    }
    if (strcmp("read_blocking/2", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_read_blocking_nif;
    }
    if (strcmp("is_readable_within_us/2", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_is_readable_within_us_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(uart, uart_nif_init, NULL, uart_nif_get_nif)
