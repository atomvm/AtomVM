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
#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/drivers/adc.h>

#include <context.h>
#include <defaultatoms.h>
#include <erl_nif.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

#include "avm_log.h"

#define TAG "adc_driver"
#define MAX_SAMPLES 1024

static ErlNifResourceType *adc_resource_type;

struct ADCResource
{
    struct adc_dt_spec spec;
    bool closed;
};

static term create_pair(Context *ctx, term first, term second)
{
    term pair = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(pair, 0, first);
    term_put_tuple_element(pair, 1, second);
    return pair;
}

static term make_error_tuple(Context *ctx, AtomString reason)
{
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

static term adc_error_to_term(Context *ctx, int err)
{
    int normalized = err < 0 ? -err : err;
    if (normalized == EBUSY) {
        return make_error_tuple(ctx, ATOM_STR("\x4", "busy"));
    }
    if (normalized == ENODEV) {
        return make_error_tuple(ctx, ATOM_STR("\x6", "enodev"));
    }
    if (normalized == ENOTSUP) {
        return make_error_tuple(ctx, ATOM_STR("\x7", "enotsup"));
    }
    return make_error_tuple(ctx, ATOM_STR("\x3", "eio"));
}

static struct ADCResource *get_adc_resource(Context *ctx, term resource_term)
{
    void *resource;
    if (!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, adc_resource_type, &resource)) {
        return NULL;
    }
    struct ADCResource *adc = resource;
    return adc->closed ? NULL : adc;
}

static term nif_adc_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_list);

#if DT_NODE_HAS_PROP(DT_PATH(zephyr_user), io_channels)
    static const struct adc_dt_spec adc_spec = ADC_DT_SPEC_GET_BY_IDX(DT_PATH(zephyr_user), 0);

    if (!adc_is_ready_dt(&adc_spec)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(ctx, ATOM_STR("\x9", "not_ready"));
    }

    int err = adc_channel_setup_dt(&adc_spec);
    if (err != 0) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return adc_error_to_term(ctx, err);
    }

    struct ADCResource *resource = enif_alloc_resource(adc_resource_type, sizeof(struct ADCResource));
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    resource->spec = adc_spec;
    resource->closed = false;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        enif_release_resource(resource);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term resource_term = term_from_resource(resource, &ctx->heap);
    enif_release_resource(resource);
    return create_pair(ctx, OK_ATOM, resource_term);
#else
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return make_error_tuple(ctx, ATOM_STR("\x6", "enodev"));
#endif
}

static term nif_adc_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct ADCResource *resource = get_adc_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    resource->closed = true;
    return OK_ATOM;
}

static term nif_adc_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct ADCResource *resource = get_adc_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    avm_int_t samples = term_to_int(argv[1]);
    if (samples < 1 || samples > MAX_SAMPLES) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int64_t total = 0;
    for (avm_int_t i = 0; i < samples; i++) {
        int16_t sample;
        struct adc_sequence sequence = {
            .buffer = &sample,
            .buffer_size = sizeof(sample),
        };
        int err = adc_sequence_init_dt(&resource->spec, &sequence);
        if (err == 0) {
            err = adc_read_dt(&resource->spec, &sequence);
        }
        if (err != 0) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return adc_error_to_term(ctx, err);
        }
        total += resource->spec.channel_cfg.differential ? sample : (uint16_t) sample;
    }

    int32_t raw = (int32_t) (total / samples);
    int32_t millivolts = raw;
    term millivolts_term = UNDEFINED_ATOM;
    if (adc_raw_to_millivolts_dt(&resource->spec, &millivolts) == 0) {
        millivolts_term = term_from_int(millivolts);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term reading = create_pair(ctx, term_from_int(raw), millivolts_term);
    return create_pair(ctx, OK_ATOM, reading);
}

static void adc_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct ADCResource *resource = obj;
    resource->closed = true;
}

static const ErlNifResourceTypeInit ADCResourceTypeInit = {
    .members = 1,
    .dtor = adc_resource_dtor,
};

static const struct Nif adc_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_adc_init };
static const struct Nif adc_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_adc_deinit };
static const struct Nif adc_read_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_adc_read };

static void adc_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    adc_resource_type = enif_init_resource_type(&env, "adc_resource", &ADCResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *adc_nif_get_nif(const char *nifname)
{
    if (strncmp("adc:", nifname, 4) != 0) {
        return NULL;
    }
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0) {
        return &adc_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &adc_deinit_nif;
    }
    if (strcmp("read/2", rest) == 0) {
        return &adc_read_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(adc, adc_nif_init, NULL, adc_nif_get_nif)
