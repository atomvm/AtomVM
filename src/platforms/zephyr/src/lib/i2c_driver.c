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
#include <zephyr/drivers/i2c.h>
#include <zephyr/kernel.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <port.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE
#include <trace.h>

#include "avm_log.h"
#include "zephyros_sys.h"

#define TAG "i2c_driver"

#ifdef CONFIG_I2C_TARGET
#define I2C_TARGET_MAX 256
#endif

static ErlNifResourceType *i2c_resource_type;

#ifdef CONFIG_I2C_TARGET
enum I2CTargetOp
{
    I2C_TARGET_IDLE,
    I2C_TARGET_TRANSMIT,
    I2C_TARGET_RECEIVE
};

struct I2CResource;

static int i2c_target_write_requested(struct i2c_target_config *config);
static int i2c_target_write_received(struct i2c_target_config *config, uint8_t val);
static int i2c_target_read_requested(struct i2c_target_config *config, uint8_t *val);
static int i2c_target_read_processed(struct i2c_target_config *config, uint8_t *val);
#ifdef CONFIG_I2C_TARGET_BUFFER_MODE
static void i2c_target_buf_write_received(struct i2c_target_config *config, uint8_t *ptr, uint32_t len);
static int i2c_target_buf_read_requested(struct i2c_target_config *config, uint8_t **ptr, uint32_t *len);
#endif
static int i2c_target_stop(struct i2c_target_config *config);
static void i2c_unregister_target(struct I2CResource *rsrc);

static const struct i2c_target_callbacks i2c_target_cbs = {
    .write_requested = i2c_target_write_requested,
    .write_received = i2c_target_write_received,
    .read_requested = i2c_target_read_requested,
    .read_processed = i2c_target_read_processed,
#ifdef CONFIG_I2C_TARGET_BUFFER_MODE
    .buf_write_received = i2c_target_buf_write_received,
    .buf_read_requested = i2c_target_buf_read_requested,
#endif
    .stop = i2c_target_stop,
};
#endif

struct I2CResource
{
    const struct device *dev;
    bool closed;
#ifdef CONFIG_I2C_TARGET
    bool target_registered;
    uint16_t own_address;
    struct i2c_target_config target_cfg;
    enum I2CTargetOp op;
    uint8_t buf[I2C_TARGET_MAX];
    size_t buf_len;
    size_t buf_pos;
    int32_t waiter_pid;
    GlobalContext *global;
#endif
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

static int normalize_error(int err)
{
    return err < 0 ? -err : err;
}

static term zephyr_i2c_error_to_term(Context *ctx, int err)
{
    int normalized = normalize_error(err);
    if (normalized == ETIMEDOUT) {
        return create_pair(ctx, ERROR_ATOM, TIMEOUT_ATOM);
    }
    if (normalized == EBUSY) {
        return make_error_tuple(ctx, ATOM_STR("\x4", "busy"));
    }
    if (normalized == ENODEV) {
        return make_error_tuple(ctx, ATOM_STR("\x6", "enodev"));
    }
#ifdef ENOTSUP
    if (normalized == ENOTSUP) {
        return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
    }
#endif
#if defined(EOPNOTSUPP) && (!defined(ENOTSUP) || EOPNOTSUPP != ENOTSUP)
    if (normalized == EOPNOTSUPP) {
        return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
    }
#endif
    return make_error_tuple(ctx, ATOM_STR("\x3", "eio"));
}

static term make_transfer_error(Context *ctx, int err)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return zephyr_i2c_error_to_term(ctx, err);
}

static bool get_i2c_resource(Context *ctx, term resource_term, struct I2CResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, i2c_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    struct I2CResource *rsrc = (struct I2CResource *) rsrc_obj_ptr;
    if (IS_NULL_PTR(rsrc->dev) || rsrc->closed) {
        return false;
    }
    *rsrc_obj = rsrc;
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

static bool term_to_i2c_addr(term addr_term, uint16_t *out)
{
    if (!term_is_integer(addr_term)) {
        return false;
    }
    avm_int_t addr = term_to_int(addr_term);
    if (addr < 0 || addr > 127) {
        return false;
    }
    *out = (uint16_t) addr;
    return true;
}

static const struct device *get_i2c_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_i2c0)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c0), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(i2c0));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c1)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c1), okay)
        case 1:
            return DEVICE_DT_GET(DT_NODELABEL(i2c1));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c2)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c2), okay)
        case 2:
            return DEVICE_DT_GET(DT_NODELABEL(i2c2));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c3)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c3), okay)
        case 3:
            return DEVICE_DT_GET(DT_NODELABEL(i2c3));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c4)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c4), okay)
        case 4:
            return DEVICE_DT_GET(DT_NODELABEL(i2c4));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c5)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c5), okay)
        case 5:
            return DEVICE_DT_GET(DT_NODELABEL(i2c5));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c6)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c6), okay)
        case 6:
            return DEVICE_DT_GET(DT_NODELABEL(i2c6));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c7)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c7), okay)
        case 7:
            return DEVICE_DT_GET(DT_NODELABEL(i2c7));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c8)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c8), okay)
        case 8:
            return DEVICE_DT_GET(DT_NODELABEL(i2c8));
#endif
#endif
#if defined(DT_N_NODELABEL_i2c9)
#if DT_NODE_HAS_STATUS(DT_NODELABEL(i2c9), okay)
        case 9:
            return DEVICE_DT_GET(DT_NODELABEL(i2c9));
#endif
#endif
        default:
            return NULL;
    }
}

static const struct device *get_default_i2c_device()
{
#if DT_HAS_CHOSEN(atomvm_i2c)
    return DEVICE_DT_GET(DT_CHOSEN(atomvm_i2c));
#else
    return get_i2c_device_by_index(0);
#endif
}

static bool parse_i2c_index_name(const char *name, int *out)
{
    bool starts_with_i2c = (name[0] == 'i' || name[0] == 'I') && name[1] == '2' && (name[2] == 'c' || name[2] == 'C');
    if (!starts_with_i2c) {
        return false;
    }
    int pos = 3;
    if (name[pos] == '_' || name[pos] == '-') {
        pos++;
    }
    if (name[pos] < '0' || name[pos] > '9') {
        return false;
    }
    int index = 0;
    while (name[pos] >= '0' && name[pos] <= '9') {
        index = index * 10 + (name[pos] - '0');
        pos++;
    }
    if (name[pos] != '\0') {
        return false;
    }
    *out = index;
    return true;
}

static const struct device *get_i2c_device_by_name(const char *name)
{
    int index;
    if (parse_i2c_index_name(name, &index)) {
        return get_i2c_device_by_index(index);
    }
    return device_get_binding(name);
}

static const struct device *get_i2c_device(term peripheral_term)
{
    if (term_is_invalid_term(peripheral_term)) {
        return get_default_i2c_device();
    }
    if (term_is_integer(peripheral_term)) {
        int peripheral = term_to_int(peripheral_term);
        if (peripheral < 0) {
            return NULL;
        }
        return get_i2c_device_by_index(peripheral);
    }

    int ok;
    char *name = interop_term_to_string(peripheral_term, &ok);
    if (!ok) {
        return NULL;
    }
    const struct device *dev = get_i2c_device_by_name(name);
    free(name);
    return dev;
}

static bool clock_speed_hz_to_zephyr_speed(uint32_t clock_speed_hz, uint32_t *out)
{
    if (clock_speed_hz == 0) {
        return false;
    }
    if (clock_speed_hz <= 100000) {
        *out = I2C_SPEED_STANDARD;
        return true;
    }
    if (clock_speed_hz <= 400000) {
        *out = I2C_SPEED_FAST;
        return true;
    }
    if (clock_speed_hz <= 1000000) {
        *out = I2C_SPEED_FAST_PLUS;
        return true;
    }
    *out = I2C_SPEED_HIGH;
    return true;
}

#ifdef CONFIG_I2C_TARGET
static struct I2CResource *i2c_resource_from_target(struct i2c_target_config *config)
{
    return CONTAINER_OF(config, struct I2CResource, target_cfg);
}

static int i2c_target_write_requested(struct i2c_target_config *config)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op != I2C_TARGET_RECEIVE) {
        return -EINVAL;
    }
    rsrc->buf_pos = 0;
    return 0;
}

static int i2c_target_write_received(struct i2c_target_config *config, uint8_t val)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op != I2C_TARGET_RECEIVE || rsrc->buf_pos >= rsrc->buf_len) {
        return -ENOMEM;
    }
    rsrc->buf[rsrc->buf_pos++] = val;
    return 0;
}

static int i2c_target_read_requested(struct i2c_target_config *config, uint8_t *val)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op != I2C_TARGET_TRANSMIT || rsrc->buf_pos >= rsrc->buf_len) {
        return -ENODATA;
    }
    *val = rsrc->buf[rsrc->buf_pos++];
    return 0;
}

static int i2c_target_read_processed(struct i2c_target_config *config, uint8_t *val)
{
    return i2c_target_read_requested(config, val);
}

#ifdef CONFIG_I2C_TARGET_BUFFER_MODE
static void i2c_target_buf_write_received(struct i2c_target_config *config, uint8_t *ptr, uint32_t len)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op != I2C_TARGET_RECEIVE) {
        return;
    }
    if (len > rsrc->buf_len) {
        len = (uint32_t) rsrc->buf_len;
    }
    memcpy(rsrc->buf, ptr, len);
    rsrc->buf_pos = len;
}

static int i2c_target_buf_read_requested(struct i2c_target_config *config, uint8_t **ptr, uint32_t *len)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op != I2C_TARGET_TRANSMIT) {
        return -ENODATA;
    }
    *ptr = rsrc->buf;
    *len = (uint32_t) rsrc->buf_len;
    rsrc->buf_pos = rsrc->buf_len;
    return 0;
}
#endif

static void i2c_target_complete(struct I2CResource *rsrc, term result)
{
    int32_t waiter_pid = rsrc->waiter_pid;
    rsrc->op = I2C_TARGET_IDLE;
    rsrc->waiter_pid = 0;
    rsrc->buf_len = 0;
    rsrc->buf_pos = 0;
    i2c_unregister_target(rsrc);
    if (waiter_pid != 0 && rsrc->global != NULL) {
        globalcontext_send_message(rsrc->global, waiter_pid, result);
    }
}

static int i2c_target_stop(struct i2c_target_config *config)
{
    struct I2CResource *rsrc = i2c_resource_from_target(config);
    if (rsrc->op == I2C_TARGET_IDLE || rsrc->waiter_pid == 0) {
        return 0;
    }

    if (rsrc->op == I2C_TARGET_TRANSMIT) {
        BEGIN_WITH_STACK_HEAP(1, heap);
        UNUSED(heap);
        i2c_target_complete(rsrc, term_from_int((avm_int_t) rsrc->buf_len));
        END_WITH_STACK_HEAP(heap, rsrc->global);
        return 0;
    }

    size_t received = rsrc->buf_pos;
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + term_binary_heap_size(received), heap);
    term data = term_from_literal_binary(rsrc->buf, received, &heap, rsrc->global);
    term result = port_heap_create_tuple2(&heap, OK_ATOM, data);
    i2c_target_complete(rsrc, result);
    END_WITH_STACK_HEAP(heap, rsrc->global);
    return 0;
}

static int i2c_ensure_target(struct I2CResource *rsrc)
{
    if (rsrc->target_registered) {
        return 0;
    }
    rsrc->target_cfg.address = rsrc->own_address;
    rsrc->target_cfg.flags = 0;
    rsrc->target_cfg.callbacks = &i2c_target_cbs;
    int err = i2c_target_register(rsrc->dev, &rsrc->target_cfg);
    if (err != 0) {
        return err;
    }
    rsrc->target_registered = true;
    return 0;
}

static void i2c_unregister_target(struct I2CResource *rsrc)
{
    if (!rsrc->target_registered) {
        return;
    }
    (void) i2c_target_unregister(rsrc->dev, &rsrc->target_cfg);
    rsrc->target_registered = false;
}
#endif

static term configure_i2c(Context *ctx, const struct device *dev, term opts)
{
    static const char *const clock_speed_hz_str = ATOM_STR("\xE", "clock_speed_hz");

    term clock_speed_term = interop_kv_get_value(opts, clock_speed_hz_str, ctx->global);
    if (term_is_invalid_term(clock_speed_term)) {
        return OK_ATOM;
    }
    VALIDATE_VALUE(clock_speed_term, term_is_integer);
    avm_int_t clock_speed_hz = term_to_int(clock_speed_term);
    if (clock_speed_hz <= 0 || clock_speed_hz > INT32_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint32_t zephyr_speed;
    if (!clock_speed_hz_to_zephyr_speed((uint32_t) clock_speed_hz, &zephyr_speed)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = i2c_configure(dev, I2C_MODE_CONTROLLER | I2C_SPEED_SET(zephyr_speed));
    if (err != 0) {
        AVM_LOGE(TAG, "i2c_configure failed: %d", err);
        return make_transfer_error(ctx, err);
    }
    return OK_ATOM;
}

static bool get_mem_addr(term mem_addr_term, term mem_addr_size_term, uint8_t *buf, size_t *len)
{
    if (!term_is_integer(mem_addr_term) || !term_is_integer(mem_addr_size_term)) {
        return false;
    }
    avm_int_t mem_addr = term_to_int(mem_addr_term);
    avm_int_t mem_addr_size = term_to_int(mem_addr_size_term);
    if (mem_addr < 0) {
        return false;
    }
    if (mem_addr_size == 8 && mem_addr <= UINT8_MAX) {
        buf[0] = (uint8_t) mem_addr;
        *len = 1;
        return true;
    }
    if (mem_addr_size == 16 && mem_addr <= UINT16_MAX) {
        buf[0] = (uint8_t) ((uint16_t) mem_addr >> 8);
        buf[1] = (uint8_t) mem_addr;
        *len = 2;
        return true;
    }
    return false;
}

static term nif_i2c_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    term peripheral_term = interop_kv_get_value(opts, peripheral_str, ctx->global);

    const struct device *dev = get_i2c_device(peripheral_term);
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

    term configure_result = configure_i2c(ctx, dev, opts);
    if (configure_result != OK_ATOM) {
        return configure_result;
    }

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->dev = dev;
    rsrc_obj->closed = false;
#ifdef CONFIG_I2C_TARGET
    static const char *const own_address_str = ATOM_STR("\xB", "own_address");
    term own_address_term = interop_kv_get_value_default(opts, own_address_str, term_from_int(0), ctx->global);
    uint16_t own_address;
    if (UNLIKELY(!term_to_i2c_addr(own_address_term, &own_address))) {
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(BADARG_ATOM);
    }
    rsrc_obj->target_registered = false;
    rsrc_obj->own_address = own_address;
    memset(&rsrc_obj->target_cfg, 0, sizeof(rsrc_obj->target_cfg));
    rsrc_obj->op = I2C_TARGET_IDLE;
    rsrc_obj->buf_len = 0;
    rsrc_obj->buf_pos = 0;
    rsrc_obj->waiter_pid = 0;
    rsrc_obj->global = ctx->global;
#endif

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

static term nif_i2c_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#ifdef CONFIG_I2C_TARGET
    i2c_unregister_target(rsrc_obj);
#endif
    rsrc_obj->closed = true;
    rsrc_obj->dev = NULL;

    return OK_ATOM;
}

static term nif_i2c_master_transmit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_binary);
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[3], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    if (UNLIKELY(len > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = i2c_write(rsrc_obj->dev, data, (uint32_t) len, addr);
    if (err != 0) {
        return make_transfer_error(ctx, err);
    }
    return term_from_int((avm_int_t) len);
}

static term nif_i2c_master_receive(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_integer);
    avm_int_t count = term_to_int(argv[2]);
    if (UNLIKELY(count < 0 || count > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[3], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    if (UNLIKELY(term_is_invalid_term(data))) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int err = i2c_read(rsrc_obj->dev, buf, (uint32_t) count, addr);
    if (err != 0) {
        return zephyr_i2c_error_to_term(ctx, err);
    }
    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_mem_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t mem_addr_buf[2];
    size_t mem_addr_len;
    if (UNLIKELY(!get_mem_addr(argv[2], argv[3], mem_addr_buf, &mem_addr_len))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(argv[4], term_is_integer);
    avm_int_t count = term_to_int(argv[4]);
    if (UNLIKELY(count < 0 || count > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[5], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size(count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary(count, &ctx->heap, ctx->global);
    if (UNLIKELY(term_is_invalid_term(data))) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    int err = i2c_write_read(rsrc_obj->dev, addr, mem_addr_buf, mem_addr_len, buf, (uint32_t) count);
    if (err != 0) {
        return zephyr_i2c_error_to_term(ctx, err);
    }
    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_mem_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t mem_addr_buf[2];
    size_t mem_addr_len;
    if (UNLIKELY(!get_mem_addr(argv[2], argv[3], mem_addr_buf, &mem_addr_len))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(argv[4], term_is_binary);
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[5], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[4]);
    size_t len = term_binary_size(argv[4]);
    size_t transfer_len = mem_addr_len + len;
    if (UNLIKELY(transfer_len > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t *transfer_buf = malloc(transfer_len);
    if (IS_NULL_PTR(transfer_buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memcpy(transfer_buf, mem_addr_buf, mem_addr_len);
    memcpy(transfer_buf + mem_addr_len, data, len);

    int err = i2c_write(rsrc_obj->dev, transfer_buf, (uint32_t) transfer_len, addr);
    free(transfer_buf);
    if (err != 0) {
        return make_transfer_error(ctx, err);
    }
    return term_from_int((avm_int_t) len);
}

static term nif_i2c_is_device_ready(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t addr;
    if (UNLIKELY(!term_to_i2c_addr(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_integer);
    avm_int_t trials = term_to_int(argv[2]);
    if (UNLIKELY(trials <= 0 || trials > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[3], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t dummy = 0;
    int err = -EIO;
    int64_t deadline = timeout_ms < 0 ? -1 : k_uptime_get() + timeout_ms;
    for (avm_int_t i = 0; i < trials; i++) {
        err = i2c_write(rsrc_obj->dev, &dummy, 0, addr);
        if (err == 0) {
            return OK_ATOM;
        }
        if (deadline >= 0 && k_uptime_get() >= deadline) {
            break;
        }
        if (i + 1 < trials && timeout_ms != 0) {
            k_msleep(1);
        }
    }
    return make_transfer_error(ctx, err);
}

#ifdef CONFIG_I2C_TARGET
static term nif_i2c_target_transmit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);

    size_t len = term_binary_size(argv[1]);
    if (UNLIKELY(len == 0 || len > I2C_TARGET_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->op != I2C_TARGET_IDLE) {
        return make_transfer_error(ctx, -EBUSY);
    }

    int err = i2c_ensure_target(rsrc_obj);
    if (err != 0) {
        return make_transfer_error(ctx, err);
    }

    memcpy(rsrc_obj->buf, term_binary_data(argv[1]), len);
    rsrc_obj->buf_len = len;
    rsrc_obj->buf_pos = 0;
    rsrc_obj->waiter_pid = ctx->process_id;
    rsrc_obj->global = ctx->global;
    rsrc_obj->op = I2C_TARGET_TRANSMIT;
    return OK_ATOM;
}

static term nif_i2c_target_receive(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t count = term_to_int(argv[1]);
    if (UNLIKELY(count <= 0 || count > I2C_TARGET_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    UNUSED(timeout_ms);
    if (rsrc_obj->op != I2C_TARGET_IDLE) {
        return make_transfer_error(ctx, -EBUSY);
    }

    int err = i2c_ensure_target(rsrc_obj);
    if (err != 0) {
        return make_transfer_error(ctx, err);
    }

    rsrc_obj->buf_len = (size_t) count;
    rsrc_obj->buf_pos = 0;
    rsrc_obj->waiter_pid = ctx->process_id;
    rsrc_obj->global = ctx->global;
    rsrc_obj->op = I2C_TARGET_RECEIVE;
    return OK_ATOM;
}
#else
static term nif_i2c_target_transmit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
}

static term nif_i2c_target_receive(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t count = term_to_int(argv[1]);
    if (UNLIKELY(count < 0 || count > INT32_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int64_t timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[2], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
}
#endif

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;
#ifdef CONFIG_I2C_TARGET
    i2c_unregister_target(rsrc_obj);
#endif
    rsrc_obj->closed = true;
    rsrc_obj->dev = NULL;
}

static const ErlNifResourceTypeInit I2CResourceTypeInit = {
    .members = 1,
    .dtor = i2c_resource_dtor,
};

static const struct Nif i2c_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_init
};
static const struct Nif i2c_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_deinit
};
static const struct Nif i2c_master_transmit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_master_transmit
};
static const struct Nif i2c_master_receive_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_master_receive
};
static const struct Nif i2c_mem_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_read
};
static const struct Nif i2c_mem_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_write
};
static const struct Nif i2c_is_device_ready_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_is_device_ready
};
static const struct Nif i2c_target_transmit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_target_transmit
};
static const struct Nif i2c_target_receive_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_target_receive
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
    if (strcmp("init/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_deinit_nif;
    }
    if (strcmp("master_transmit/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_master_transmit_nif;
    }
    if (strcmp("master_receive/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_master_receive_nif;
    }
    if (strcmp("mem_read/6", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_mem_read_nif;
    }
    if (strcmp("mem_write/6", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_mem_write_nif;
    }
    if (strcmp("is_device_ready/4", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_is_device_ready_nif;
    }
    if (strcmp("target_transmit_nif/3", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_target_transmit_nif;
    }
    if (strcmp("target_receive_nif/3", rest) == 0) {
        TRACE("Resolved i2c nif %s ...\n", nifname);
        return &i2c_target_receive_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(i2c, i2c_nif_init, NULL, i2c_nif_get_nif)
