/***************************************************************************
 *   Copyright 2020 by Fred Dushin <fred@dushin.net>                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include <atom.h>
#include <defaultatoms.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <nvs.h>
#include <nvs_flash.h>
#include <term.h>
#include <stdlib.h>

//#define ENABLE_TRACE
#include "trace.h"

#include "nvs_nif.h"

#define MAX_NVS_KEY_SIZE 15

static int write_atom_c_string(Context *ctx, char *buf, size_t bufsize, term t);

static term nif_esp_nvs_get_binary(Context *ctx, int argc, term argv[])
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
        case ESP_ERR_NVS_NOT_FOUND:
            TRACE("No such namespace.  namespace='%s'\n", namespace);
            return UNDEFINED_ATOM;
        default:
            fprintf(stderr, "Unable to open NVS. namespace '%s' err=%i\n", namespace, err);
            RAISE_ERROR(term_from_int(err));
    }

    size_t size = 0;
    err = nvs_get_blob(nvs, key, NULL, &size);
    switch (err) {
        case ESP_OK:
            break;
        case ESP_ERR_NVS_NOT_FOUND:
            TRACE("No such entry.  namespace='%s' key='%s'\n", namespace, key);
            return UNDEFINED_ATOM;
        default:
            fprintf(stderr, "Unable to get NVS blob size. namespace '%s'  key='%s' err=%i\n", namespace, key, err);
            RAISE_ERROR(term_from_int(err));
    }

    if (UNLIKELY(memory_ensure_free(ctx, size + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        TRACE("Unabled to ensure free space for binary.  namespace='%s' key='%s' size=%i\n", namespace, key, size);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_uninitialized_binary(size, ctx);

    err = nvs_get_blob(nvs, key, (void *) term_binary_data(binary), &size);
    nvs_close(nvs);
    switch (err) {
        case ESP_OK:
            TRACE("Found data for key.  namespace='%s' key='%s' size='%i'\n", namespace, key, size);
            return binary;
        default:
            fprintf(stderr, "Unable to get NVS blob. namespace='%s' key='%s' err=%i\n", namespace, key, err);
            RAISE_ERROR(term_from_int(err));
    }
}

static term nif_esp_nvs_set_binary(Context *ctx, int argc, term argv[])
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

static const struct Nif esp_nvs_get_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_get_binary
};
static const struct Nif esp_nvs_set_binary_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_esp_nvs_set_binary
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
    if (strcmp("esp:nvs_get_binary/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_get_binary_nif;
    }
    if (strcmp("esp:nvs_set_binary/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &esp_nvs_set_binary_nif;
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
    AtomString atom_string = globalcontext_atomstring_from_term(ctx->global, t);
    if (atom_string == NULL) {
        return -1;
    }
    atom_string_to_c(atom_string, buf, bufsize);
    return 0;
}
