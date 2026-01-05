/*
 * This file is part of AtomVM.
 *
 * Copyright (c) 2025 schnittchen <schnittchen@das-labor.org>
 * All rights reserved.
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

// References
// https://docs.espressif.com/projects/esp-idf/en/stable/esp32/api-reference/peripherals/dac.html

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_DAC_NIF
#include <context.h>
#include <defaultatoms.h>
#include <erl_nif_priv.h>
#include <esp32_sys.h>
#include <globalcontext.h>
#include <nifs.h>
#include <term.h>

// #define ENABLE_TRACE
#include <trace.h>

#include <driver/dac_oneshot.h>
#include <esp_log.h>

#define TAG "atomvm_dac"

static ErlNifResourceType *oneshot_channel_resource;

// All channel resource structs start with a `handle` field:
struct AnyChannelResource
{
    void *handle;
};

struct OneshotChannelResource
{
    dac_oneshot_handle_t handle;
};

//
// internal functions
//

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);

    return ret;
}

static term error_return_tuple(Context *ctx, term term)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, ERROR_ATOM, term);
}

static term get_channel_resource(Context *ctx, term t, ErlNifResourceType *res_type, struct AnyChannelResource **res)
{
    bool likely_valid = (term_is_tuple(t) && term_get_tuple_arity(t) == 3 && globalcontext_is_term_equal_to_atom_string(ctx->global, term_get_tuple_element(t, 0), ATOM_STR("\x4", "$dac")) && term_is_resource_reference(term_get_tuple_element(t, 1)) && term_is_reference(term_get_tuple_element(t, 2)));

    if (likely_valid) {
        if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(t, 1), res_type, (void **) res))) {
            ESP_LOGE(TAG, "resource is not a valid dac channel resource");

            return error_return_tuple(ctx, BADARG_ATOM);
        }

        if (LIKELY((*res)->handle)) {
            return 0;
        }
    }

    ESP_LOGE(TAG, "resource is not a valid adc channel resource");
    return error_return_tuple(ctx, BADARG_ATOM);
}

//
// Nif functions
//

static term nif_oneshot_new_channel_p(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct OneshotChannelResource *chan_rsrc = enif_alloc_resource(oneshot_channel_resource, sizeof(struct OneshotChannelResource));
    if (IS_NULL_PTR(chan_rsrc)) {
        ESP_LOGE(TAG, "failed to allocate resource: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    chan_rsrc->handle = 0;

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "failed to allocate memory for resource: %s:%i.", __FILE__, __LINE__);
        enif_release_resource(chan_rsrc);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    ERL_NIF_TERM chan_obj = enif_make_resource(erl_nif_env_from_context(ctx), chan_rsrc);

    const dac_oneshot_config_t config = {
        .chan_id = term_to_uint8(argv[0])
    };

    const esp_err_t err = dac_oneshot_new_channel(&config, &chan_rsrc->handle);

    enif_release_resource(chan_rsrc);

    if (!err) {
        term chan_tup = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(chan_tup, 0, globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "$dac")));
        term_put_tuple_element(chan_tup, 1, chan_obj);
        uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
        term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
        term_put_tuple_element(chan_tup, 2, ref);

        term ret = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ret, 0, OK_ATOM);
        term_put_tuple_element(ret, 1, chan_tup);

        return ret;
    }

    term reason = term_invalid_term();
    switch (err) {
        case ESP_ERR_INVALID_ARG:
            reason = BADARG_ATOM;
            break;
        case ESP_ERR_INVALID_STATE:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "invalid_state"));
            break;
        case ESP_ERR_NO_MEM:
            reason = OUT_OF_MEMORY_ATOM;
            break;
        default:
            reason = BADARG_ATOM;
    }

    return error_return_tuple(ctx, reason);
};

static term nif_oneshot_output_voltage(Context *ctx, int argc, term argv[])
{
    VALIDATE_VALUE(argv[1], term_is_uint8);

    struct OneshotChannelResource *chan_rsrc;

    term error_term = get_channel_resource(ctx, argv[0], oneshot_channel_resource, (struct AnyChannelResource **) &chan_rsrc);
    if (UNLIKELY(error_term)) {
        return error_term;
    }

    esp_err_t err = dac_oneshot_output_voltage(chan_rsrc->handle, term_to_uint8(argv[1]));
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "dac_oneshot_output_voltage failed");
        return error_return_tuple(ctx, BADARG_ATOM);
    }

    return OK_ATOM;
};

static term nif_oneshot_del_channel(Context *ctx, int argc, term argv[])
{
    struct OneshotChannelResource *chan_rsrc;

    term error_term = get_channel_resource(ctx, argv[0], oneshot_channel_resource, (struct AnyChannelResource **) &chan_rsrc);
    if (UNLIKELY(error_term)) {
        return error_term;
    }

    esp_err_t err = dac_oneshot_del_channel(chan_rsrc->handle);

    if (UNLIKELY(err != ESP_OK)) {
        return ERROR_ATOM;
    }

    chan_rsrc->handle = NULL;

    return OK_ATOM;
};

//
// Nif Entry/Exit
//

static void nif_dac_oneshot_chan_res_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct OneshotChannelResource *chan_rsrc = (struct OneshotChannelResource *) obj;

    if (chan_rsrc->handle) {
        dac_oneshot_del_channel(chan_rsrc->handle);
    }
}

static const ErlNifResourceTypeInit OneshotChannelResourceTypeInit = {
    .members = 1,
    .dtor = nif_dac_oneshot_chan_res_dtor,
};

static const struct Nif oneshot_new_channel_p_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_oneshot_new_channel_p
};

static const struct Nif oneshot_output_voltage_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_oneshot_output_voltage
};

static const struct Nif oneshot_del_channel_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_oneshot_del_channel
};

void atomvm_dac_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);

    oneshot_channel_resource = enif_init_resource_type(&env, "dac_oneshot_channel_resource", &OneshotChannelResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
};

const struct Nif *atomvm_dac_get_nif(const char *nifname)
{
    TRACE("Locating nif %s ...\n", nifname);
    if (strcmp("esp_dac:oneshot_new_channel_p/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &oneshot_new_channel_p_nif;
    }
    if (strcmp("esp_dac:oneshot_output_voltage/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &oneshot_output_voltage_nif;
    }

    if (strcmp("esp_dac:oneshot_del_channel/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &oneshot_del_channel_nif;
    }
    return NULL;
};

REGISTER_NIF_COLLECTION(atomvm_dac, atomvm_dac_init, NULL, atomvm_dac_get_nif)
#endif /* CONFIG_AVM_ENABLE_DAC_NIF */
