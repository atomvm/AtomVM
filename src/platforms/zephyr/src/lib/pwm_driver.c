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
#include <zephyr/drivers/pwm.h>

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

static ErlNifResourceType *pwm_resource_type;

struct PWMResource
{
    const struct device *dev;
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

static term pwm_error(Context *ctx, int err)
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

static const struct device *get_pwm_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_pwm0) && DT_NODE_HAS_STATUS(DT_NODELABEL(pwm0), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(pwm0));
#elif defined(DT_N_NODELABEL_ledc0) && DT_NODE_HAS_STATUS(DT_NODELABEL(ledc0), okay)
        case 0:
            return DEVICE_DT_GET(DT_NODELABEL(ledc0));
#endif
#if defined(DT_N_NODELABEL_pwm1) && DT_NODE_HAS_STATUS(DT_NODELABEL(pwm1), okay)
        case 1:
            return DEVICE_DT_GET(DT_NODELABEL(pwm1));
#endif
#if defined(DT_N_NODELABEL_pwm2) && DT_NODE_HAS_STATUS(DT_NODELABEL(pwm2), okay)
        case 2:
            return DEVICE_DT_GET(DT_NODELABEL(pwm2));
#endif
#if defined(DT_N_NODELABEL_pwm3) && DT_NODE_HAS_STATUS(DT_NODELABEL(pwm3), okay)
        case 3:
            return DEVICE_DT_GET(DT_NODELABEL(pwm3));
#endif
        default:
            return NULL;
    }
}

static const struct device *get_default_pwm_device(void)
{
#if DT_HAS_CHOSEN(atomvm_pwm)
    return DEVICE_DT_GET(DT_CHOSEN(atomvm_pwm));
#else
    return get_pwm_device_by_index(0);
#endif
}

static const struct device *get_pwm_device(term peripheral)
{
    if (term_is_invalid_term(peripheral)) {
        return get_default_pwm_device();
    }
    if (term_is_integer(peripheral)) {
        avm_int_t index = term_to_int(peripheral);
        return index >= 0 ? get_pwm_device_by_index((int) index) : NULL;
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

static struct PWMResource *get_resource(Context *ctx, term resource_term)
{
    void *resource;
    if (!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, pwm_resource_type, &resource)) {
        return NULL;
    }
    struct PWMResource *pwm = resource;
    return pwm->closed ? NULL : pwm;
}

static bool get_flags(Context *ctx, term flags_term, pwm_flags_t *flags)
{
    *flags = PWM_POLARITY_NORMAL;
    if (term_is_invalid_term(flags_term)) {
        return true;
    }
    if (term_is_atom(flags_term)) {
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, flags_term, ATOM_STR("\x6", "normal"))) {
            *flags = PWM_POLARITY_NORMAL;
            return true;
        }
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, flags_term, ATOM_STR("\x8", "inverted"))) {
            *flags = PWM_POLARITY_INVERTED;
            return true;
        }
        return false;
    }
    if (term_is_list(flags_term)) {
        *flags = PWM_POLARITY_NORMAL;
        term list = flags_term;
        while (!term_is_nil(list)) {
            term head = term_get_list_head(list);
            list = term_get_list_tail(list);
            if (globalcontext_is_term_equal_to_atom_string(ctx->global, head, ATOM_STR("\x8", "inverted"))) {
                *flags = PWM_POLARITY_INVERTED;
            } else if (!globalcontext_is_term_equal_to_atom_string(ctx->global, head, ATOM_STR("\x6", "normal"))) {
                return false;
            }
        }
        return true;
    }
    return false;
}

static term nif_pwm_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_list);

    term peripheral = interop_kv_get_value(argv[0], ATOM_STR("\xA", "peripheral"), ctx->global);
    const struct device *dev = get_pwm_device(peripheral);
    if (IS_NULL_PTR(dev)) {
        return make_error(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (!device_is_ready(dev)) {
        return make_error(ctx, ATOM_STR("\x9", "not_ready"));
    }

    struct PWMResource *resource = enif_alloc_resource(pwm_resource_type, sizeof(struct PWMResource));
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    resource->dev = dev;
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

static term nif_pwm_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct PWMResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    resource->closed = true;
    resource->dev = NULL;
    return OK_ATOM;
}

static term nif_pwm_set(Context *ctx, int argc, term argv[])
{
    struct PWMResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1]) || !term_is_integer(argv[2]) || !term_is_integer(argv[3])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    avm_int_t channel = term_to_int(argv[1]);
    avm_int_t period_ns = term_to_int(argv[2]);
    avm_int_t pulse_ns = term_to_int(argv[3]);
    if (channel < 0 || period_ns < 0 || pulse_ns < 0 || pulse_ns > period_ns) {
        RAISE_ERROR(BADARG_ATOM);
    }

    pwm_flags_t flags = PWM_POLARITY_NORMAL;
    if (argc >= 5 && !get_flags(ctx, argv[4], &flags)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = pwm_set(resource->dev, (uint32_t) channel, (uint32_t) period_ns, (uint32_t) pulse_ns, flags);
    return err == 0 ? OK_ATOM : pwm_error(ctx, err);
}

static term nif_pwm_set_cycles(Context *ctx, int argc, term argv[])
{
    struct PWMResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1]) || !term_is_integer(argv[2]) || !term_is_integer(argv[3])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    avm_int_t channel = term_to_int(argv[1]);
    avm_int_t period_cycles = term_to_int(argv[2]);
    avm_int_t pulse_cycles = term_to_int(argv[3]);
    if (channel < 0 || period_cycles < 0 || pulse_cycles < 0 || pulse_cycles > period_cycles) {
        RAISE_ERROR(BADARG_ATOM);
    }

    pwm_flags_t flags = PWM_POLARITY_NORMAL;
    if (argc >= 5 && !get_flags(ctx, argv[4], &flags)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = pwm_set_cycles(resource->dev, (uint32_t) channel, (uint32_t) period_cycles,
        (uint32_t) pulse_cycles, flags);
    return err == 0 ? OK_ATOM : pwm_error(ctx, err);
}

static term nif_pwm_get_cycles_per_sec(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct PWMResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    avm_int_t channel = term_to_int(argv[1]);
    if (channel < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint64_t cycles = 0;
    int err = pwm_get_cycles_per_sec(resource->dev, (uint32_t) channel, &cycles);
    if (err != 0) {
        return pwm_error(ctx, err);
    }
    if (cycles > (uint64_t) INT32_MAX) {
        return make_error(ctx, ATOM_STR("\x8", "overflow"));
    }
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return pair(ctx, OK_ATOM, term_from_int((avm_int_t) cycles));
}

static void pwm_resource_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    struct PWMResource *resource = obj;
    resource->closed = true;
    resource->dev = NULL;
}

static const ErlNifResourceTypeInit PWMResourceTypeInit = {
    .members = 1,
    .dtor = pwm_resource_dtor,
};

static const struct Nif pwm_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_pwm_init };
static const struct Nif pwm_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_pwm_deinit };
static const struct Nif pwm_set_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_pwm_set };
static const struct Nif pwm_set_cycles_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_pwm_set_cycles };
static const struct Nif pwm_get_cycles_per_sec_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_pwm_get_cycles_per_sec,
};

static void pwm_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    pwm_resource_type = enif_init_resource_type(&env, "pwm_resource", &PWMResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *pwm_nif_get_nif(const char *name)
{
    if (strncmp("pwm:", name, 4) != 0) {
        return NULL;
    }
    const char *rest = name + 4;
    if (strcmp("init/1", rest) == 0) {
        return &pwm_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &pwm_deinit_nif;
    }
    if (strcmp("set/4", rest) == 0 || strcmp("set/5", rest) == 0) {
        return &pwm_set_nif;
    }
    if (strcmp("set_cycles/4", rest) == 0 || strcmp("set_cycles/5", rest) == 0) {
        return &pwm_set_cycles_nif;
    }
    if (strcmp("get_cycles_per_sec/2", rest) == 0) {
        return &pwm_get_cycles_per_sec_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(pwm, pwm_nif_init, NULL, pwm_nif_get_nif)
