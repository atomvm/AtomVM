/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/kernel.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "avm_log.h"
#include "zephyros_sys.h"

#define TAG "uart_driver"
#define UART_DEFAULT_SPEED 115200
#define UART_MAX_SYNC_TIMEOUT_MS 10

static ErlNifResourceType *uart_resource_type;

struct UARTResource
{
    const struct device *dev;
    int last_error;
    bool closed;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term make_atom(Context *ctx, AtomString atom_string)
{
    return globalcontext_make_atom(ctx->global, atom_string);
}

static term make_error_tuple(Context *ctx, AtomString reason)
{
    return create_pair(ctx, ERROR_ATOM, make_atom(ctx, reason));
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

static bool get_timeout_ms(term timeout_term, int64_t *out)
{
    if (term_is_atom(timeout_term)) {
        if (timeout_term == INFINITY_ATOM) {
            *out = -1;
            return true;
        }
        return false;
    }
    if (!term_is_integer(timeout_term)) {
        return false;
    }
    avm_int_t val = term_to_int(timeout_term);
    if (val < 0) {
        return false;
    }
    *out = (int64_t) val;
    return true;
}

static int normalize_error(int err)
{
    return err < 0 ? -err : err;
}

static bool timeout_safe_for_sync_nif(int64_t timeout_ms)
{
    return timeout_ms >= 0 && timeout_ms <= UART_MAX_SYNC_TIMEOUT_MS;
}

static int64_t make_deadline(int64_t timeout_ms)
{
    int64_t now = k_uptime_get();
    return timeout_ms > INT64_MAX - now ? INT64_MAX : now + timeout_ms;
}

static bool has_uart_config_options(GlobalContext *glb, term opts)
{
    return !term_is_invalid_term(interop_kv_get_value(opts, ATOM_STR("\x5", "speed"), glb))
        || !term_is_invalid_term(interop_kv_get_value(opts, ATOM_STR("\x9", "data_bits"), glb))
        || !term_is_invalid_term(interop_kv_get_value(opts, ATOM_STR("\x9", "stop_bits"), glb))
        || !term_is_invalid_term(interop_kv_get_value(opts, ATOM_STR("\x6", "parity"), glb))
        || !term_is_invalid_term(interop_kv_get_value(opts, ATOM_STR("\xC", "flow_control"), glb));
}

static term make_timeout(Context *ctx, struct UARTResource *rsrc_obj)
{
    rsrc_obj->last_error = ETIMEDOUT;
    return create_pair(ctx, ERROR_ATOM, TIMEOUT_ATOM);
}

static const struct device *get_default_uart_device()
{
#if DT_HAS_CHOSEN(zephyr_console)
    return DEVICE_DT_GET(DT_CHOSEN(zephyr_console));
#else
    return NULL;
#endif
}

static const struct device *get_uart_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_uart0)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart0), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(uart0));
#endif
#endif
#if defined(DT_N_NODELABEL_uart1)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart1), okay)
        case 1:
            return DEVICE_DT_GET(DT_NODELABEL(uart1));
#endif
#endif
#if defined(DT_N_NODELABEL_uart2)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart2), okay)
        case 2:
            return DEVICE_DT_GET(DT_NODELABEL(uart2));
#endif
#endif
#if defined(DT_N_NODELABEL_uart3)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart3), okay)
        case 3:
            return DEVICE_DT_GET(DT_NODELABEL(uart3));
#endif
#endif
#if defined(DT_N_NODELABEL_uart4)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart4), okay)
        case 4:
            return DEVICE_DT_GET(DT_NODELABEL(uart4));
#endif
#endif
#if defined(DT_N_NODELABEL_uart5)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart5), okay)
        case 5:
            return DEVICE_DT_GET(DT_NODELABEL(uart5));
#endif
#endif
#if defined(DT_N_NODELABEL_uart6)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart6), okay)
        case 6:
            return DEVICE_DT_GET(DT_NODELABEL(uart6));
#endif
#endif
#if defined(DT_N_NODELABEL_uart7)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart7), okay)
        case 7:
            return DEVICE_DT_GET(DT_NODELABEL(uart7));
#endif
#endif
#if defined(DT_N_NODELABEL_uart8)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart8), okay)
        case 8:
            return DEVICE_DT_GET(DT_NODELABEL(uart8));
#endif
#endif
#if defined(DT_N_NODELABEL_uart9)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(uart9), okay)
        case 9:
            return DEVICE_DT_GET(DT_NODELABEL(uart9));
#endif
#endif
        default:
            return NULL;
    }
}

static const struct device *get_uart_device_by_name(const char *name)
{
    if (strcmp(name, "UART0") == 0 || strcmp(name, "uart0") == 0) {
        return get_uart_device_by_index(0);
    }
    if (strcmp(name, "UART1") == 0 || strcmp(name, "uart1") == 0) {
        return get_uart_device_by_index(1);
    }
    if (strcmp(name, "UART2") == 0 || strcmp(name, "uart2") == 0) {
        return get_uart_device_by_index(2);
    }
    if (strcmp(name, "UART3") == 0 || strcmp(name, "uart3") == 0) {
        return get_uart_device_by_index(3);
    }
    return device_get_binding(name);
}

static const struct device *get_uart_device(term peripheral_term)
{
    if (term_is_invalid_term(peripheral_term)) {
        return get_default_uart_device();
    }
    if (term_is_integer(peripheral_term)) {
        int peripheral = term_to_int(peripheral_term);
        if (peripheral < 0) {
            return NULL;
        }
        return get_uart_device_by_index(peripheral);
    }

    int ok;
    char *name = interop_term_to_string(peripheral_term, &ok);
    if (!ok) {
        return NULL;
    }
    const struct device *dev = get_uart_device_by_name(name);
    free(name);
    return dev;
}

#if defined(CONFIG_UART_USE_RUNTIME_CONFIGURE)
static bool parse_data_bits(term data_bits_term, enum uart_config_data_bits *out)
{
    if (!term_is_integer(data_bits_term)) {
        return false;
    }
    switch (term_to_int(data_bits_term)) {
        case 5:
            *out = UART_CFG_DATA_BITS_5;
            return true;
        case 6:
            *out = UART_CFG_DATA_BITS_6;
            return true;
        case 7:
            *out = UART_CFG_DATA_BITS_7;
            return true;
        case 8:
            *out = UART_CFG_DATA_BITS_8;
            return true;
        case 9:
            *out = UART_CFG_DATA_BITS_9;
            return true;
        default:
            return false;
    }
}

static bool parse_stop_bits(term stop_bits_term, enum uart_config_stop_bits *out)
{
    if (!term_is_integer(stop_bits_term)) {
        return false;
    }
    switch (term_to_int(stop_bits_term)) {
        case 1:
            *out = UART_CFG_STOP_BITS_1;
            return true;
        case 2:
            *out = UART_CFG_STOP_BITS_2;
            return true;
        default:
            return false;
    }
}

static bool parse_parity(GlobalContext *glb, term parity_term, enum uart_config_parity *out)
{
    if (term_is_integer(parity_term)) {
        switch (term_to_int(parity_term)) {
            case 0:
                *out = UART_CFG_PARITY_NONE;
                return true;
            case 1:
                *out = UART_CFG_PARITY_ODD;
                return true;
            case 2:
                *out = UART_CFG_PARITY_EVEN;
                return true;
            default:
                return false;
        }
    }
    if (!term_is_atom(parity_term)) {
        return false;
    }
    if (parity_term == globalcontext_make_atom(glb, ATOM_STR("\x4", "none"))) {
        *out = UART_CFG_PARITY_NONE;
        return true;
    }
    if (parity_term == globalcontext_make_atom(glb, ATOM_STR("\x3", "odd"))) {
        *out = UART_CFG_PARITY_ODD;
        return true;
    }
    if (parity_term == globalcontext_make_atom(glb, ATOM_STR("\x4", "even"))) {
        *out = UART_CFG_PARITY_EVEN;
        return true;
    }
    return false;
}

static bool parse_flow_control(GlobalContext *glb, term flow_control_term, enum uart_config_flow_control *out)
{
    if (term_is_integer(flow_control_term)) {
        switch (term_to_int(flow_control_term)) {
            case 0:
                *out = UART_CFG_FLOW_CTRL_NONE;
                return true;
            case 1:
                *out = UART_CFG_FLOW_CTRL_RTS_CTS;
                return true;
            default:
                return false;
        }
    }
    if (!term_is_atom(flow_control_term)) {
        return false;
    }
    if (flow_control_term == globalcontext_make_atom(glb, ATOM_STR("\x4", "none"))) {
        *out = UART_CFG_FLOW_CTRL_NONE;
        return true;
    }
    if (flow_control_term == globalcontext_make_atom(glb, ATOM_STR("\x8", "hardware"))) {
        *out = UART_CFG_FLOW_CTRL_RTS_CTS;
        return true;
    }
    return false;
}
#endif

static term configure_uart(Context *ctx, const struct device *dev, term opts)
{
    GlobalContext *glb = ctx->global;
    bool has_config_opts = has_uart_config_options(glb, opts);
#if defined(CONFIG_UART_USE_RUNTIME_CONFIGURE)
    static const char *const speed_str = ATOM_STR("\x5", "speed");
    static const char *const data_bits_str = ATOM_STR("\x9", "data_bits");
    static const char *const stop_bits_str = ATOM_STR("\x9", "stop_bits");
    static const char *const parity_str = ATOM_STR("\x6", "parity");
    static const char *const flow_control_str = ATOM_STR("\xC", "flow_control");

    if (!has_config_opts) {
        return OK_ATOM;
    }

    struct uart_config cfg;
    int err = uart_config_get(dev, &cfg);
    if (err != 0) {
        cfg.baudrate = UART_DEFAULT_SPEED;
        cfg.parity = UART_CFG_PARITY_NONE;
        cfg.stop_bits = UART_CFG_STOP_BITS_1;
        cfg.data_bits = UART_CFG_DATA_BITS_8;
        cfg.flow_ctrl = UART_CFG_FLOW_CTRL_NONE;
    }

    term speed_term = interop_kv_get_value_default(opts, speed_str, term_from_int((int) cfg.baudrate), glb);
    term data_bits_term = interop_kv_get_value_default(opts, data_bits_str, term_from_int(8), glb);
    term stop_bits_term = interop_kv_get_value_default(opts, stop_bits_str, term_from_int(1), glb);
    term parity_term = interop_kv_get_value_default(opts, parity_str, globalcontext_make_atom(glb, ATOM_STR("\x4", "none")), glb);
    term flow_control_term = interop_kv_get_value_default(opts, flow_control_str, globalcontext_make_atom(glb, ATOM_STR("\x4", "none")), glb);

    VALIDATE_VALUE(speed_term, term_is_integer);
    avm_int_t speed = term_to_int(speed_term);
    if (speed <= 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    cfg.baudrate = (uint32_t) speed;

    enum uart_config_data_bits data_bits;
    enum uart_config_stop_bits stop_bits;
    enum uart_config_parity parity;
    enum uart_config_flow_control flow_ctrl;
    if (!parse_data_bits(data_bits_term, &data_bits)
        || !parse_stop_bits(stop_bits_term, &stop_bits)
        || !parse_parity(glb, parity_term, &parity)
        || !parse_flow_control(glb, flow_control_term, &flow_ctrl)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    cfg.data_bits = data_bits;
    cfg.stop_bits = stop_bits;
    cfg.parity = parity;
    cfg.flow_ctrl = flow_ctrl;

    err = uart_configure(dev, &cfg);
    if (err != 0) {
        AVM_LOGE(TAG, "uart_configure failed: %d", err);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(ctx, ATOM_STR("\x9", "configure"));
    }
#else
    UNUSED(dev);
    if (has_config_opts) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
    }
#endif
    return OK_ATOM;
}

static term nif_uart_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    term peripheral_term = interop_kv_get_value(opts, peripheral_str, ctx->global);
    const struct device *dev = get_uart_device(peripheral_term);
    if (IS_NULL_PTR(dev)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (!device_is_ready(dev)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(ctx, ATOM_STR("\x9", "not_ready"));
    }

    term configure_result = configure_uart(ctx, dev, opts);
    if (configure_result != OK_ATOM) {
        return configure_result;
    }

    struct UARTResource *rsrc_obj = enif_alloc_resource(uart_resource_type, sizeof(struct UARTResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->dev = dev;
    rsrc_obj->last_error = 0;
    rsrc_obj->closed = false;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return create_pair(ctx, OK_ATOM, obj);
}

static term nif_uart_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    rsrc_obj->closed = true;
    rsrc_obj->dev = NULL;
    return OK_ATOM;
}

static term nif_uart_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->dev) || rsrc_obj->closed) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t len = term_binary_size(argv[1]);
    if (UNLIKELY(len > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (len == 0) {
        rsrc_obj->last_error = 0;
        return term_from_int(0);
    }
    if (UNLIKELY(!timeout_safe_for_sync_nif(timeout_ms))) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_timeout(ctx, rsrc_obj);
    }
    term data_term = argv[1];
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &data_term, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const uint8_t *data = (const uint8_t *) term_binary_data(data_term);
    int64_t deadline = make_deadline(timeout_ms);
    size_t written = 0;
    for (; written < len; written++) {
        if (timeout_ms == 0 || k_uptime_get() >= deadline) {
            rsrc_obj->last_error = ETIMEDOUT;
            return written > 0 ? term_from_int((int) written) : create_pair(ctx, ERROR_ATOM, TIMEOUT_ATOM);
        }
        uart_poll_out(rsrc_obj->dev, data[written]);
    }
    rsrc_obj->last_error = 0;
    return term_from_int((int) written);
}

static term nif_uart_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->dev) || rsrc_obj->closed) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);

    avm_int_t count = term_to_int(argv[1]);
    if (UNLIKELY(count < 0 || count > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t count_size = (size_t) count;
    if (count_size > 0 && UNLIKELY(!timeout_safe_for_sync_nif(timeout_ms))) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_timeout(ctx, rsrc_obj);
    }

    size_t needed = TUPLE_SIZE(2) + term_binary_heap_size(count_size) + TERM_BOXED_SUB_BINARY_SIZE;
    if (UNLIKELY(memory_ensure_free_opt(ctx, needed, MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(count_size, &ctx->heap, ctx->global);
    if (UNLIKELY(term_is_invalid_term(data))) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int64_t deadline = timeout_ms < 0 ? -1 : make_deadline(timeout_ms);
    avm_int_t read = 0;
    while (read < count) {
        unsigned char c;
        int err = uart_poll_in(rsrc_obj->dev, &c);
        if (err == 0) {
            buf[read++] = c;
            continue;
        }
        if (err != -1) {
            rsrc_obj->last_error = normalize_error(err);
            if (read > 0) {
                term partial = term_alloc_sub_binary(data, 0, (size_t) read, &ctx->heap);
                return create_pair(ctx, OK_ATOM, partial);
            }
            return make_error_tuple(ctx, ATOM_STR("\x3", "eio"));
        }
        if (timeout_ms == 0 || (deadline >= 0 && k_uptime_get() >= deadline)) {
            rsrc_obj->last_error = ETIMEDOUT;
            if (read > 0) {
                term partial = term_alloc_sub_binary(data, 0, (size_t) read, &ctx->heap);
                return create_pair(ctx, OK_ATOM, partial);
            }
            return create_pair(ctx, ERROR_ATOM, TIMEOUT_ATOM);
        }
        k_msleep(1);
    }

    rsrc_obj->last_error = 0;
    return create_pair(ctx, OK_ATOM, data);
}

static term nif_uart_abort(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->dev) || rsrc_obj->closed) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

static term nif_uart_get_state(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->dev) || rsrc_obj->closed) {
        return make_atom(ctx, ATOM_STR("\x5", "reset"));
    }
    if (rsrc_obj->last_error == ETIMEDOUT) {
        return TIMEOUT_ATOM;
    }
    if (rsrc_obj->last_error != 0) {
        return ERROR_ATOM;
    }
    return make_atom(ctx, ATOM_STR("\x5", "ready"));
}

static term nif_uart_get_error(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return term_from_int(rsrc_obj->last_error);
}

static void uart_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct UARTResource *rsrc_obj = (struct UARTResource *) obj;
    rsrc_obj->closed = true;
    rsrc_obj->dev = NULL;
}

static const ErlNifResourceTypeInit UARTResourceTypeInit = {
    .members = 1,
    .dtor = uart_resource_dtor,
};

static const struct Nif uart_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_init
};
static const struct Nif uart_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_deinit
};
static const struct Nif uart_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_write
};
static const struct Nif uart_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_read
};
static const struct Nif uart_abort_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_abort
};
static const struct Nif uart_get_state_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_get_state
};
static const struct Nif uart_get_error_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_get_error
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
    if (strcmp("init/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_deinit_nif;
    }
    if (strcmp("write/3", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_write_nif;
    }
    if (strcmp("read/3", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_read_nif;
    }
    if (strcmp("abort/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_abort_nif;
    }
    if (strcmp("get_state/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_get_state_nif;
    }
    if (strcmp("get_error/1", rest) == 0) {
        TRACE("Resolved uart nif %s ...\n", nifname);
        return &uart_get_error_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(uart, uart_nif_init, NULL, uart_nif_get_nif)
