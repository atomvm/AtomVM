/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M <petermm@gmail.com>
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#include <errno.h>
#include <stdint.h>
#include <string.h>

#include <zephyr/device.h>
#include <zephyr/devicetree.h>
#include <zephyr/retention/retention.h>

#include <context.h>
#include <defaultatoms.h>
#include <globalcontext.h>
#include <memory.h>
#include <nifs.h>
#include <portnifloader.h>
#include <term.h>

#if DT_HAS_CHOSEN(atomvm_retention)
static const struct device *const retention_dev = DEVICE_DT_GET(DT_CHOSEN(atomvm_retention));
#endif

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

static term retention_error(Context *ctx, int err)
{
    int normalized = err < 0 ? -err : err;
    if (normalized == EINVAL) return make_error(ctx, ATOM_STR("\x6", "einval"));
    if (normalized == ENOTSUP) return make_error(ctx, ATOM_STR("\x7", "enotsup"));
    if (normalized == ENODEV) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    return make_error(ctx, ATOM_STR("\x3", "eio"));
}

static const struct device *get_retention_device(void)
{
#if DT_HAS_CHOSEN(atomvm_retention)
    return device_is_ready(retention_dev) ? retention_dev : NULL;
#else
    return NULL;
#endif
}

static bool get_range(term offset_term, term size_term, size_t capacity, size_t *offset, size_t *size)
{
    if (!term_is_integer(offset_term) || !term_is_integer(size_term)) return false;
    avm_int_t offset_value = term_to_int(offset_term);
    avm_int_t size_value = term_to_int(size_term);
    if (offset_value < 0 || size_value < 0) return false;
    size_t checked_offset = (size_t) offset_value;
    size_t checked_size = (size_t) size_value;
    if (checked_offset > capacity || checked_size > capacity - checked_offset) return false;
    *offset = checked_offset;
    *size = checked_size;
    return true;
}

static term nif_retention_size(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    const struct device *dev = get_retention_device();
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    ssize_t size = retention_size(dev);
    return size >= 0 ? term_from_int((avm_int_t) size) : retention_error(ctx, (int) size);
}

static term nif_retention_is_valid(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    const struct device *dev = get_retention_device();
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    int valid = retention_is_valid(dev);
    if (valid < 0) return retention_error(ctx, valid);
    return valid ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_retention_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    const struct device *dev = get_retention_device();
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    ssize_t capacity = retention_size(dev);
    size_t offset;
    size_t size;
    if (capacity < 0 || !get_range(argv[0], argv[1], (size_t) capacity, &offset, &size)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t needed = TUPLE_SIZE(2) + term_binary_heap_size(size);
    if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    term data = term_create_uninitialized_binary(size, &ctx->heap, ctx->global);
    if (term_is_invalid_term(data)) RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    int err = retention_read(dev, (off_t) offset, (uint8_t *) term_binary_data(data), size);
    return err == 0 ? pair(ctx, OK_ATOM, data) : retention_error(ctx, err);
}

static term nif_retention_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    const struct device *dev = get_retention_device();
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    if (!term_is_binary(argv[1])) RAISE_ERROR(BADARG_ATOM);
    ssize_t capacity = retention_size(dev);
    size_t offset;
    size_t size;
    term size_term = term_from_int((avm_int_t) term_binary_size(argv[1]));
    if (capacity < 0 || !get_range(argv[0], size_term, (size_t) capacity, &offset, &size)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int err = retention_write(dev, (off_t) offset, (const uint8_t *) term_binary_data(argv[1]), size);
    return err == 0 ? OK_ATOM : retention_error(ctx, err);
}

static term nif_retention_clear(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    const struct device *dev = get_retention_device();
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    int err = retention_clear(dev);
    return err == 0 ? OK_ATOM : retention_error(ctx, err);
}

static const struct Nif retention_size_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_retention_size };
static const struct Nif retention_is_valid_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_retention_is_valid };
static const struct Nif retention_read_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_retention_read };
static const struct Nif retention_write_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_retention_write };
static const struct Nif retention_clear_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_retention_clear };

static const struct Nif *retention_nif_get_nif(const char *name)
{
    if (strncmp("retention:", name, 10) != 0) return NULL;
    const char *rest = name + 10;
    if (strcmp("size/0", rest) == 0) return &retention_size_nif;
    if (strcmp("is_valid/0", rest) == 0) return &retention_is_valid_nif;
    if (strcmp("read/2", rest) == 0) return &retention_read_nif;
    if (strcmp("write/2", rest) == 0) return &retention_write_nif;
    if (strcmp("clear/0", rest) == 0) return &retention_clear_nif;
    return NULL;
}

REGISTER_NIF_COLLECTION(retention, NULL, NULL, retention_nif_get_nif)
