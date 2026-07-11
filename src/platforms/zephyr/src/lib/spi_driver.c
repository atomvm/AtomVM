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
#include <zephyr/drivers/spi.h>

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

static ErlNifResourceType *spi_resource_type;

struct SPIResource
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

static term spi_error(Context *ctx, int err)
{
    int normalized = err < 0 ? -err : err;
    if (normalized == ENODEV) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    if (normalized == EBUSY) return make_error(ctx, ATOM_STR("\x4", "busy"));
    if (normalized == ENOTSUP) return make_error(ctx, ATOM_STR("\x7", "enotsup"));
    return make_error(ctx, ATOM_STR("\x3", "eio"));
}

static const struct device *get_spi_device_by_index(int index)
{
    switch (index) {
#if defined(DT_N_NODELABEL_spi0) && DT_NODE_HAS_STATUS(DT_NODELABEL(spi0), okay)
        case 0: return DEVICE_DT_GET(DT_NODELABEL(spi0));
#endif
#if defined(DT_N_NODELABEL_spi1) && DT_NODE_HAS_STATUS(DT_NODELABEL(spi1), okay)
        case 1: return DEVICE_DT_GET(DT_NODELABEL(spi1));
#endif
#if defined(DT_N_NODELABEL_spi2) && DT_NODE_HAS_STATUS(DT_NODELABEL(spi2), okay)
        case 2: return DEVICE_DT_GET(DT_NODELABEL(spi2));
#endif
#if defined(DT_N_NODELABEL_spi3) && DT_NODE_HAS_STATUS(DT_NODELABEL(spi3), okay)
        case 3: return DEVICE_DT_GET(DT_NODELABEL(spi3));
#endif
        default: return NULL;
    }
}

static const struct device *get_default_spi_device(void)
{
#if DT_HAS_CHOSEN(atomvm_spi)
    return DEVICE_DT_GET(DT_CHOSEN(atomvm_spi));
#else
    return NULL;
#endif
}

static const struct device *get_spi_device(term peripheral)
{
    if (term_is_invalid_term(peripheral)) return get_default_spi_device();
    if (term_is_integer(peripheral)) {
        avm_int_t index = term_to_int(peripheral);
        return index >= 0 ? get_spi_device_by_index((int) index) : NULL;
    }
    int ok;
    char *name = interop_term_to_string(peripheral, &ok);
    if (!ok) return NULL;
    const struct device *dev = device_get_binding(name);
    free(name);
    return dev;
}

static struct SPIResource *get_resource(Context *ctx, term resource_term)
{
    void *resource;
    if (!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, spi_resource_type, &resource)) return NULL;
    struct SPIResource *spi = resource;
    return spi->closed ? NULL : spi;
}

static term nif_spi_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_list);
    term peripheral = interop_kv_get_value(argv[0], ATOM_STR("\xA", "peripheral"), ctx->global);
    const struct device *dev = get_spi_device(peripheral);
    if (IS_NULL_PTR(dev)) return make_error(ctx, ATOM_STR("\x6", "enodev"));
    if (!device_is_ready(dev)) return make_error(ctx, ATOM_STR("\x9", "not_ready"));

    struct SPIResource *resource = enif_alloc_resource(spi_resource_type, sizeof(struct SPIResource));
    if (IS_NULL_PTR(resource)) RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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

static term nif_spi_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource)) RAISE_ERROR(BADARG_ATOM);
    resource->closed = true;
    resource->dev = NULL;
    return OK_ATOM;
}

static term nif_spi_transceive(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *resource = get_resource(ctx, argv[0]);
    if (IS_NULL_PTR(resource) || !term_is_integer(argv[1]) || !term_is_integer(argv[2]) || !term_is_binary(argv[3])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t frequency = term_to_int(argv[1]);
    avm_int_t mode = term_to_int(argv[2]);
    size_t len = term_binary_size(argv[3]);
    if (frequency <= 0 || frequency > UINT32_MAX || mode < 0 || mode > 3 || len > UINT16_MAX) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term tx_term = argv[3];
    size_t needed = TUPLE_SIZE(2) + term_binary_heap_size(len);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, needed, 1, &tx_term, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term rx_term = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    if (term_is_invalid_term(rx_term)) RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    if (len == 0) return pair(ctx, OK_ATOM, rx_term);

    uint16_t operation = SPI_OP_MODE_MASTER | SPI_TRANSFER_MSB | SPI_WORD_SET(8);
    if (mode & 2) operation |= SPI_MODE_CPOL;
    if (mode & 1) operation |= SPI_MODE_CPHA;
    struct spi_config config = {
        .frequency = (uint32_t) frequency,
        .operation = operation,
        .slave = 0,
        .cs = { 0 },
    };
    const struct spi_buf tx_buf = { .buf = (void *) term_binary_data(tx_term), .len = len };
    struct spi_buf rx_buf = { .buf = (void *) term_binary_data(rx_term), .len = len };
    const struct spi_buf_set tx = { .buffers = &tx_buf, .count = 1 };
    const struct spi_buf_set rx = { .buffers = &rx_buf, .count = 1 };
    int err = spi_transceive(resource->dev, &config, &tx, &rx);
    return err == 0 ? pair(ctx, OK_ATOM, rx_term) : spi_error(ctx, err);
}

static void spi_resource_dtor(ErlNifEnv *env, void *obj)
{
    UNUSED(env);
    struct SPIResource *resource = obj;
    resource->closed = true;
    resource->dev = NULL;
}

static const ErlNifResourceTypeInit SPIResourceTypeInit = { .members = 1, .dtor = spi_resource_dtor };
static const struct Nif spi_init_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_spi_init };
static const struct Nif spi_deinit_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_spi_deinit };
static const struct Nif spi_transceive_nif = { .base.type = NIFFunctionType, .nif_ptr = nif_spi_transceive };

static void spi_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    spi_resource_type = enif_init_resource_type(&env, "spi_resource", &SPIResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *spi_nif_get_nif(const char *name)
{
    if (strncmp("spi:", name, 4) != 0) return NULL;
    const char *rest = name + 4;
    if (strcmp("init/1", rest) == 0) return &spi_init_nif;
    if (strcmp("deinit/1", rest) == 0) return &spi_deinit_nif;
    if (strcmp("transceive/4", rest) == 0) return &spi_transceive_nif;
    return NULL;
}

REGISTER_NIF_COLLECTION(spi, spi_nif_init, NULL, spi_nif_get_nif)
