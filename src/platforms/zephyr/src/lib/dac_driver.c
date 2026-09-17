/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
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
#include <zephyr/drivers/dac.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

static ErlNifResourceType *dac_resource_type;

struct DACResource
{
    const struct device *dev;
    uint8_t channel_id;
    uint8_t resolution;
    bool closed;
};

static term pair(Context *ctx, term first, term second)
{
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, first);
    term_put_tuple_element(result, 1, second);
    return result;
}

static term make_error(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

static term dac_error(Context *ctx, int err)
{
    int normalized = err < 0 ? -err : err;
    if (normalized == ENODEV) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (normalized == EBUSY) {
        return make_error(ctx, ATOM_STR("\x4", "busy"));
    }
    if (normalized == ENOTSUP) {
        return make_error(ctx, ATOM_STR("\x7", "enotsup"));
    }
    if (normalized == EINVAL) {
        return make_error(ctx, ATOM_STR("\x6", "einval"));
    }
    return make_error(ctx, ATOM_STR("\x3", "eio"));
}

static const struct device *get_dac_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_dac0) && DT_NODE_HAS_STATUS(DT_NODELABEL(dac0), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(dac0));
#elif defined(DT_N_NODELABEL_dac) && DT_NODE_HAS_STATUS(DT_NODELABEL(dac), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(dac));
#endif
#if defined(DT_N_NODELABEL_dac1) && DT_NODE_HAS_STATUS(DT_NODELABEL(dac1), okay)
        case 1:
            return DEVICE_DT_GET(DT_NODELABEL(dac1));
#endif
        default:
            return NULL;
    }
}

static const struct device *get_default_dac_device(void)
{
#if DT_HAS_CHOSEN(atomvm_dac)
    return DEVICE_DT_GET(DT_CHOSEN(atomvm_dac));
#else
    return get_dac_device_by_index(0);
#endif
}

static const struct device *get_dac_device(term peripheral)
{
    if (term_is_invalid_term(peripheral)) {
        return get_default_dac_device();
    }
    if (term_is_integer(peripheral)) {
        avm_int_t index = term_to_int(peripheral);
        return index >= 0 ? get_dac_device_by_index((int) index) : NULL;
    }
    int ok;
    char *name = interop_term_to_string(peripheral, &ok);
    if (!ok) {
        return NULL;
    }
    const struct device *dev = device_get_binding(name);
    free(name);
    return dev;
}

static struct DACResource *get_resource(Context *ctx, term resource_term)
{
    void *resource;
    if (!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, dac_resource_type, &resource)) {
        return NULL;
    }
    struct DACResource *dac = resource;
    return dac->closed ? NULL : dac;
}

static bool get_optional_uint8(term value, uint8_t fallback, uint8_t *out)
{
    if (term_is_invalid_term(value)) {
        *out = fallback;
        return true;
    }
    if (!term_is_integer(value)) {
        return false;
    }
    avm_int_t parsed = term_to_int(value);
    if (parsed < 0 || parsed > 255) {
        return false;
    }
    *out = (uint8_t) parsed;
    return true;
}

static term nif_dac_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_list);

    term peripheral = interop_kv_get_value(argv[0], ATOM_STR("\xA", "peripheral"), ctx->global);
    const struct device *dev = get_dac_device(peripheral);
    if (IS_NULL_PTR(dev)) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (!device_is_ready(dev)) {
        return make_error(ctx, ATOM_STR("\x9", "not_ready"));
    }

    uint8_t channel_id;
    uint8_t resolution;
    if (!get_optional_uint8(interop_kv_get_value(argv[0], ATOM_STR("\x7", "channel"), ctx->global), 0, &channel_id)
        || !get_optional_uint8(interop_kv_get_value(argv[0], ATOM_STR("\xA", "resolution"), ctx->global), 8, &resolution)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (resolution < 1 || resolution > 32) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term buffered_term = interop_kv_get_value(argv[0], ATOM_STR("\x8", "buffered"), ctx->global);
    term internal_term = interop_kv_get_value(argv[0], ATOM_STR("\x8", "internal"), ctx->global);
    bool buffered = term_is_invalid_term(buffered_term) ? false : (buffered_term == TRUE_ATOM);
    bool internal = term_is_invalid_term(internal_term) ? false : (internal_term == TRUE_ATOM);

    struct dac_channel_cfg cfg = {
        .channel_id = channel_id,
        .resolution = resolution,
        .buffered = buffered,
        .internal = internal,
    };
    int err = dac_channel_setup(dev, &cfg);
    if (err != 0) {
        return dac_error(ctx, err);
    }

    struct DACResource *resource = enif_alloc_resource(dac_resource_type, sizeof(struct DACResource));
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    resource->dev = dev;
    resource->channel_id = channel_id;
    resource->resolution = resolution;
    resource->closed = false;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(resource);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term resource_term = term_from_resource(resource, &ctx->heap);
    enif_release_resource(resource);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &resource_term, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return pair(ctx, OK_ATOM, resource_term);
}

static term nif_dac_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct DACResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    resource->closed = true;
    resource->dev = NULL;
    return OK_ATOM;
}

static term nif_dac_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct DACResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    avm_int_t value = term_to_int(argv[1]);
    uint32_t max_value = resource->resolution >= 32 ? UINT32_MAX : ((1U << resource->resolution) - 1U);
    if (value < 0 || (uint32_t) value > max_value) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = dac_write_value(resource->dev, resource->channel_id, (uint32_t) value);
    return err == 0 ? OK_ATOM : dac_error(ctx, err);
}

static void dac_resource_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    struct DACResource *resource = obj;
    resource->closed = true;
    resource->dev = NULL;
}

static const ErlNifResourceTypeInit DACResourceTypeInit = {
    .members = 1,
    .dtor = dac_resource_dtor,
};

static const struct Nif dac_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_dac_init };
static const struct Nif dac_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_dac_deinit };
static const struct Nif dac_write_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_dac_write };

static void dac_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    dac_resource_type = enif_init_resource_type(&env, "dac_resource", &DACResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *dac_nif_get_nif(const char *name)
{
    if (strncmp("dac:", name, 4) != 0) {
        return NULL;
    }
    const char *rest = name + 4;
    if (strcmp("init/1", rest) == 0) {
        return &dac_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &dac_deinit_nif;
    }
    if (strcmp("write/2", rest) == 0) {
        return &dac_write_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(dac, dac_nif_init, NULL, dac_nif_get_nif)
