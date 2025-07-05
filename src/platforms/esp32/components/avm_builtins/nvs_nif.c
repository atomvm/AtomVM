/*
 * This file is part of AtomVM.
 *
 * Copyright 2020 Fred Dushin <fred@dushin.net>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_NVS_NIFS

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <nvs.h>
#include <nvs_flash.h>
#include <stdlib.h>
#include <term.h>

#include "esp32_sys.h"

//#define ENABLE_TRACE
#include "trace.h"

#define MAX_NVS_KEY_SIZE 15

static void nvs_nif_init(GlobalContext *global);
static const struct Nif *nvs_nif_get_nif(const char *nifname);

static int write_atom_c_string(Context *ctx, char *buf, size_t bufsize, term t);

static term nif_esp_nvs_fetch_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    char namespace[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, MAX_NVS_KEY_SIZE + 1, argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, MAX_NVS_KEY_SIZE + 1, argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    nvs_handle nvs;
    esp_err_t err = nvs_open(namespace, NVS_READONLY, &nvs);
    switch (err) {
        case ESP_OK:
            break;
        case ESP_ERR_NVS_NOT_FOUND: {
            TRACE("No such namespace.  namespace='%s'\n", namespace);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term ns_not_found = globalcontext_make_atom(ctx->global, ATOM_STR("\x13", "namespace_not_found"));
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, ns_not_found);
            return error_tuple;
        }
        default:
            TRACE("Unable to open NVS. namespace '%s' err=%i\n", namespace, err);
            RAISE_ERROR(esp_err_to_term(ctx->global, err));
    }

    size_t size = 0;
    err = nvs_get_blob(nvs, key, NULL, &size);
    switch (err) {
        case ESP_OK:
            break;
        case ESP_ERR_NVS_NOT_FOUND: {
            TRACE("No such entry.  namespace='%s' key='%s'\n", namespace, key);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term not_found = globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "not_found"));
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, not_found);
            return error_tuple;
        }
        default:
            TRACE("Unable to get NVS blob size. namespace '%s'  key='%s' err=%i\n", namespace, key, err);
            RAISE_ERROR(esp_err_to_term(ctx->global, err));
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_heap_size(size) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        TRACE("Unabled to ensure free space for binary.  namespace='%s' key='%s' size=%i\n", namespace, key, size);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_uninitialized_binary(size, &ctx->heap, ctx->global);

    err = nvs_get_blob(nvs, key, (void *) term_binary_data(binary), &size);
    nvs_close(nvs);
    switch (err) {
        case ESP_OK:
            TRACE("Found data for key.  namespace='%s' key='%s' size='%i'\n", namespace, key, size);
            term result_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result_tuple, 0, OK_ATOM);
            term_put_tuple_element(result_tuple, 1, binary);
            return result_tuple;
        default:
            fprintf(stderr, "Unable to get NVS blob. namespace='%s' key='%s' err=%i\n", namespace, key, err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_esp_nvs_put_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);
    VALIDATE_VALUE(argv[2], term_is_binary);

    char namespace[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, MAX_NVS_KEY_SIZE + 1, argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, MAX_NVS_KEY_SIZE + 1, argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term binary = argv[2];
    size_t size = term_binary_size(binary);

    nvs_handle nvs;
    esp_err_t err = nvs_open(namespace, NVS_READWRITE, &nvs);
    switch (err) {
        case ESP_OK:
            break;
        default:
            fprintf(stderr, "Unable to open NVS. namespace '%s' err=%i\n", namespace, err);
            RAISE_ERROR(term_from_int(err));
    }

    err = nvs_set_blob(nvs, key, term_binary_data(binary), size);
    nvs_close(nvs);
    switch (err) {
        case ESP_OK:
            TRACE("Wrote blob to NVS. namespace '%s' key '%s' size: %i\n", namespace, key, size);
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to set NVS blob. namespace='%s' key='%s' size=%i err=%i\n", namespace, key, size, err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_esp_nvs_erase_key(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    char namespace[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, MAX_NVS_KEY_SIZE + 1, argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char key[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, key, MAX_NVS_KEY_SIZE + 1, argv[1]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    nvs_handle nvs;
    esp_err_t err = nvs_open(namespace, NVS_READWRITE, &nvs);
    switch (err) {
        case ESP_OK:
            break;
        default:
            fprintf(stderr, "Unable to open NVS. namespace '%s' err=%i\n", namespace, err);
            RAISE_ERROR(term_from_int(err));
    }

    err = nvs_erase_key(nvs, key);
    nvs_close(nvs);
    switch (err) {
        case ESP_OK:
            TRACE("Erased key. namespace '%s' key '%s'\n", namespace, key);
            return OK_ATOM;
        case ESP_ERR_NVS_NOT_FOUND:
            TRACE("No such entry -- ok.  namespace='%s' key='%s'\n", namespace, key);
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to erase key. namespace='%s' key='%s' err=%i\n", namespace, key, err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_esp_nvs_erase_all(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);

    char namespace[MAX_NVS_KEY_SIZE + 1];
    if (write_atom_c_string(ctx, namespace, MAX_NVS_KEY_SIZE + 1, argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    nvs_handle nvs;
    esp_err_t err = nvs_open(namespace, NVS_READWRITE, &nvs);
    switch (err) {
        case ESP_OK:
            break;
        default:
            fprintf(stderr, "Unable to open NVS. namespace '%s' err=%i\n", namespace, err);
            RAISE_ERROR(term_from_int(err));
    }

    err = nvs_erase_all(nvs);
    nvs_close(nvs);
    switch (err) {
        case ESP_OK:
            TRACE("Erased all. namespace '%s'\n", namespace);
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to erase all. namespace='%s' err=%i\n", namespace, err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_esp_nvs_reformat(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    esp_err_t err = nvs_flash_erase();
    switch (err) {
        case ESP_OK:
            break;
        default:
            fprintf(stderr, "Unable to reformat NVS partition. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
    err = nvs_flash_init();
    switch (err) {
        case ESP_OK:
            fprintf(stderr, "Warning: Reformatted NVS partition!\n");
            return OK_ATOM;
        default:
            fprintf(stderr, "Unable to initialize NVS partition. err=%i\n", err);
            RAISE_ERROR(term_from_int(err));
    }
}

static const struct Nif esp_nvs_fetch_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_fetch_binary
};
static const struct Nif esp_nvs_put_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_put_binary
};
static const struct Nif esp_nvs_erase_key_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_erase_key
};
static const struct Nif esp_nvs_erase_all_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_erase_all
};
static const struct Nif esp_nvs_reformat_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_reformat
};

void nvs_nif_init(GlobalContext *gloabl)
{
    esp_err_t err = nvs_flash_init();
    if (err == ESP_ERR_NVS_NO_FREE_PAGES || err == ESP_ERR_NVS_NEW_VERSION_FOUND) {
        fprintf(stderr, "Warning: Erasing flash...\n");
        ESP_ERROR_CHECK(nvs_flash_erase());
        err = nvs_flash_init();
    }
    ESP_ERROR_CHECK(err);
}

const struct Nif *nvs_nif_get_nif(const char *nifname)
{
    if (strcmp("esp:nvs_fetch_binary/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_fetch_binary_nif;
    }
    if (strcmp("esp:nvs_put_binary/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_put_binary_nif;
    }
    if (strcmp("esp:nvs_erase_key/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_erase_key_nif;
    }
    if (strcmp("esp:nvs_erase_all/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_erase_all_nif;
    }
    if (strcmp("esp:nvs_reformat/0", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_reformat_nif;
    }
    return NULL;
}

//
// internal functions
//

static int write_atom_c_string(Context *ctx, char *buf, size_t bufsize, term t)
{
    size_t t_atom_len;
    const uint8_t *t_atom_data = atom_table_get_atom_string(ctx->global->atom_table, term_to_atom_index(t), &t_atom_len);
    if (IS_NULL_PTR(t_atom_data)) {
        return -1;
    }

    memcpy(buf, t_atom_data, t_atom_len);
    buf[t_atom_len] = 0;
    return 0;
}

REGISTER_NIF_COLLECTION(nvs, nvs_nif_init, NULL, nvs_nif_get_nif)

#endif
