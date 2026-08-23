/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifdef CONFIG_SETTINGS

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <atom_table.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <port.h>
#include <term.h>

#include <zephyr/settings/settings.h>

#include "zephyros_sys.h"

#define SETTINGS_KEY_MAX 31
#define SETTINGS_NAME_MAX (SETTINGS_KEY_MAX + 1 + SETTINGS_KEY_MAX)
#define SETTINGS_VALUE_MAX 1024
#define SETTINGS_ERASE_MAX 512

static bool settings_ready = false;
static bool settings_init_attempted = false;

static bool ensure_settings(void)
{
    if (settings_ready) {
        return true;
    }
    if (settings_init_attempted) {
        return false;
    }
    settings_init_attempted = true;
    if (settings_subsys_init() != 0) {
        return false;
    }
    settings_ready = true;
    return true;
}

static int write_atom_c_string(Context *ctx, char *buf, size_t bufsize, term t)
{
    size_t atom_len;
    const uint8_t *atom_data = atom_table_get_atom_string(ctx->global->atom_table, term_to_atom_index(t), &atom_len);
    if (IS_NULL_PTR(atom_data) || atom_len == 0 || atom_len >= bufsize) {
        return -1;
    }
    memcpy(buf, atom_data, atom_len);
    buf[atom_len] = 0;
    return 0;
}

static int make_settings_name(Context *ctx, term namespace_term, term key_term, char *name, size_t name_size)
{
    char namespace[SETTINGS_KEY_MAX + 1];
    char key[SETTINGS_KEY_MAX + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), namespace_term) != 0) {
        return -1;
    }
    if (write_atom_c_string(ctx, key, sizeof(key), key_term) != 0) {
        return -1;
    }
    int written = snprintf(name, name_size, "%s/%s", namespace, key);
    if (written < 0 || (size_t) written >= name_size) {
        return -1;
    }
    return 0;
}

static term settings_error_tuple(Context *ctx, const char *reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(ctx->global, reason));
    return error_tuple;
}

static term nif_zephyr_settings_get(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    if (!ensure_settings()) {
        return settings_error_tuple(ctx, ATOM_STR("\xe", "not_supported"));
    }

    char name[SETTINGS_NAME_MAX + 1];
    if (make_settings_name(ctx, argv[0], argv[1], name, sizeof(name)) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ssize_t value_len = settings_get_val_len(name);
    if (value_len <= 0) {
        return settings_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }
    if (value_len > SETTINGS_VALUE_MAX) {
        return settings_error_tuple(ctx, ATOM_STR("\x9", "too_large"));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size((size_t) value_len)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term binary = term_create_uninitialized_binary((size_t) value_len, &ctx->heap, ctx->global);
    ssize_t loaded = settings_load_one(name, (void *) term_binary_data(binary), (size_t) value_len);
    if (loaded < 0) {
        return settings_error_tuple(ctx, ATOM_STR("\x9", "not_found"));
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, binary);
    return result;
}

static term nif_zephyr_settings_put(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);
    VALIDATE_VALUE(argv[2], term_is_binary);

    if (!ensure_settings()) {
        return settings_error_tuple(ctx, ATOM_STR("\xe", "not_supported"));
    }

    char name[SETTINGS_NAME_MAX + 1];
    if (make_settings_name(ctx, argv[0], argv[1], name, sizeof(name)) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t size = term_binary_size(argv[2]);
    if (size > SETTINGS_VALUE_MAX) {
        return settings_error_tuple(ctx, ATOM_STR("\x9", "too_large"));
    }

    int err = settings_save_one(name, term_binary_data(argv[2]), size);
    if (err != 0) {
        return settings_error_tuple(ctx, ATOM_STR("\xb", "save_failed"));
    }
    return OK_ATOM;
}

static term nif_zephyr_settings_erase(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    VALIDATE_VALUE(argv[1], term_is_atom);

    if (!ensure_settings()) {
        return settings_error_tuple(ctx, ATOM_STR("\xe", "not_supported"));
    }

    char name[SETTINGS_NAME_MAX + 1];
    if (make_settings_name(ctx, argv[0], argv[1], name, sizeof(name)) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = settings_delete(name);
    if (err != 0) {
        return settings_error_tuple(ctx, ATOM_STR("\xc", "erase_failed"));
    }
    return OK_ATOM;
}

struct settings_capture_ctx
{
    const char *prefix;
    char *name;
    size_t name_size;
    bool found;
    int err;
};

static int settings_full_name(const char *prefix, const char *key, char *out, size_t out_size)
{
    if (prefix != NULL) {
        int written = snprintf(out, out_size, "%s%s%s", prefix, key ? "/" : "", key ? key : "");
        if (written < 0 || (size_t) written >= out_size) {
            return -ENAMETOOLONG;
        }
        return 0;
    }
    if (key == NULL) {
        return -ENOENT;
    }
    if (strlen(key) >= out_size) {
        return -ENAMETOOLONG;
    }
    memcpy(out, key, strlen(key) + 1);
    return 0;
}

static int settings_capture_one_cb(const char *key, size_t len, settings_read_cb read_cb, void *cb_arg, void *param)
{
    UNUSED(len);
    UNUSED(read_cb);
    UNUSED(cb_arg);

    struct settings_capture_ctx *capture = param;
    int err = settings_full_name(capture->prefix, key, capture->name, capture->name_size);
    if (err == -ENOENT) {
        return 0;
    }
    if (err != 0) {
        capture->err = err;
        return 1;
    }
    capture->found = true;
    return 1;
}

static int settings_erase_subtree(const char *subtree)
{
    char name[SETTINGS_NAME_MAX + 1];

    for (int i = 0; i < SETTINGS_ERASE_MAX; i++) {
        struct settings_capture_ctx capture = {
            .prefix = subtree,
            .name = name,
            .name_size = sizeof(name),
            .found = false,
            .err = 0
        };
        (void) settings_load_subtree_direct(subtree, settings_capture_one_cb, &capture);
        if (capture.err != 0) {
            return capture.err;
        }
        if (!capture.found) {
            return 0;
        }

        int err = settings_delete(name);
        if (err != 0) {
            return err;
        }
    }
    return -ENOMEM;
}

static term nif_zephyr_settings_erase_all(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);

    if (!ensure_settings()) {
        return settings_error_tuple(ctx, ATOM_STR("\xe", "not_supported"));
    }

    char namespace[SETTINGS_KEY_MAX + 1];
    if (write_atom_c_string(ctx, namespace, sizeof(namespace), argv[0]) != 0) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int err = settings_erase_subtree(namespace);
    if (err != 0) {
        return settings_error_tuple(ctx, ATOM_STR("\xc", "erase_failed"));
    }
    return OK_ATOM;
}

static term nif_zephyr_settings_reformat(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    if (!ensure_settings()) {
        return settings_error_tuple(ctx, ATOM_STR("\xe", "not_supported"));
    }

    int err = settings_erase_subtree(NULL);
    if (err != 0) {
        return settings_error_tuple(ctx, ATOM_STR("\xc", "erase_failed"));
    }
    return OK_ATOM;
}

static const struct Nif *settings_nif_get_nif(const char *nifname)
{
    if (strncmp("zephyr:", nifname, 7) != 0) {
        return NULL;
    }
    const char *rest = nifname + 7;
    if (strcmp("settings_get/2", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_settings_get };
        return &nif;
    }
    if (strcmp("settings_put/3", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_settings_put };
        return &nif;
    }
    if (strcmp("settings_erase/2", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_settings_erase };
        return &nif;
    }
    if (strcmp("settings_erase_all/1", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_settings_erase_all };
        return &nif;
    }
    if (strcmp("settings_reformat/0", rest) == 0) {
        static const struct Nif nif = { .base.type = NIFFunctionType, .nif_ptr = nif_zephyr_settings_reformat };
        return &nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(settings, NULL, NULL, settings_nif_get_nif)

#endif
