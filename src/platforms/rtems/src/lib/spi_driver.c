/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Peter M. <petermm@gmail.com>
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

#include <string.h>

#ifdef RTEMS_HAS_SPI
#include <bsp.h>
#include <dev/spi/spi.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>

#if defined(RTEMS_HAS_IMX_SPI)
int spi_bus_register_imx(const char *bus_path, const char *fdt_alias);
#elif defined(RTEMS_HAS_ESP32C3_SPI)
#include <bsp/spi.h>
#endif
#endif

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
#include <utils.h>

#ifdef RTEMS_HAS_SPI
#define CLOSED_FD (-1)

static ErlNifResourceType *spi_resource_type;

struct SPIResource
{
    int fd;
    uint32_t speed_hz;
    uint8_t mode;
    uint8_t bits_per_word;
};
#endif

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static term error_atom(Context *ctx, AtomString reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, reason));
}

#ifndef RTEMS_HAS_SPI
static term nif_spi_unsupported(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return error_atom(ctx, ATOM_STR("\x7", "enotsup"));
}
#else

static term error_errno(Context *ctx, int err)
{
    AtomString reason;
    switch (err) {
        case ENODEV:
            reason = ATOM_STR("\x6", "enodev");
            break;
        case ENOENT:
            reason = ATOM_STR("\x6", "enoent");
            break;
        case EINVAL:
            reason = ATOM_STR("\x6", "einval");
            break;
        case EIO:
            reason = ATOM_STR("\x3", "eio");
            break;
        case EBUSY:
            reason = ATOM_STR("\x5", "ebusy");
            break;
        case ETIMEDOUT:
            return error_atom(ctx, ATOM_STR("\x7", "timeout"));
#ifdef ENOTSUP
        case ENOTSUP:
            reason = ATOM_STR("\x7", "enotsup");
            break;
#endif
#if defined(EOPNOTSUPP) && (!defined(ENOTSUP) || EOPNOTSUPP != ENOTSUP)
        case EOPNOTSUPP:
            reason = ATOM_STR("\x7", "enotsup");
            break;
#endif
        default:
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return create_pair(ctx, ERROR_ATOM, term_from_int(err));
    }
    return error_atom(ctx, reason);
}

static bool get_spi_resource(Context *ctx, term resource_term, struct SPIResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, spi_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct SPIResource *) rsrc_obj_ptr;
    return true;
}

static void spi_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct SPIResource *rsrc_obj = (struct SPIResource *) obj;
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
}

static int open_spi_bus(const char *path, const char *alias)
{
    int fd = open(path, O_RDWR);
    if (fd >= 0 || errno != ENOENT) {
        return fd;
    }

#if defined(RTEMS_HAS_IMX_SPI)
    int rv = spi_bus_register_imx(path, alias);
#elif defined(RTEMS_HAS_ESP32C3_SPI)
    if (strcmp(path, "/dev/spi-0") != 0 || (alias != NULL && strcmp(alias, "spi0") != 0)) {
        errno = ENOENT;
        return -1;
    }
    mkdir("/dev", 0755);
    int rv = (esp32c3_spi_register(path) == RTEMS_SUCCESSFUL) ? 0 : -1;
#else
    (void) alias;
    int rv = -1;
#endif
    if (rv != 0) {
        errno = ENOENT;
        return -1;
    }

    return open(path, O_RDWR);
}

static term nif_spi_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    GlobalContext *glb = ctx->global;
    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    static const char *const fdt_alias_str = ATOM_STR("\x9", "fdt_alias");

    term peripheral_term = interop_kv_get_value(opts, peripheral_str, glb);
    term fdt_alias_term = interop_kv_get_value(opts, fdt_alias_str, glb);

    char *path = NULL;
    char *alias = NULL;
    int ok = 0;

    if (term_is_invalid_term(peripheral_term)) {
        path = strdup("/dev/spi-0");
        ok = path != NULL;
    } else {
        path = interop_term_to_string(peripheral_term, &ok);
    }
    if (!ok || IS_NULL_PTR(path)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ok = 0;
    if (term_is_invalid_term(fdt_alias_term)) {
        alias = strdup("spi0");
        ok = alias != NULL;
    } else {
        alias = interop_term_to_string(fdt_alias_term, &ok);
    }
    if (!ok || IS_NULL_PTR(alias)) {
        free(path);
        RAISE_ERROR(BADARG_ATOM);
    }

    int fd = open_spi_bus(path, alias);
    int err = errno;
    free(alias);
    free(path);
    if (fd < 0) {
        return error_errno(ctx, err);
    }

    struct SPIResource *rsrc_obj = enif_alloc_resource(spi_resource_type, sizeof(struct SPIResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        close(fd);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->fd = fd;
    rsrc_obj->speed_hz = 1000000;
    rsrc_obj->mode = 0;
    rsrc_obj->bits_per_word = 8;

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

static term nif_spi_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
    return OK_ATOM;
}

static term nif_spi_apply_device_config(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term config = argv[1];
    VALIDATE_VALUE(config, term_is_map);

    term clock_speed_hz_term = interop_kv_get_value(config, ATOM_STR("\xE", "clock_speed_hz"), ctx->global);
    if (!term_is_invalid_term(clock_speed_hz_term)) {
        if (!term_is_integer(clock_speed_hz_term)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        avm_int_t speed_val = term_to_int(clock_speed_hz_term);
        if (speed_val <= 0) {
            RAISE_ERROR(BADARG_ATOM);
        }
        uint32_t speed = (uint32_t) speed_val;
        if (ioctl(rsrc_obj->fd, SPI_IOC_WR_MAX_SPEED_HZ, &speed) != 0) {
            return error_errno(ctx, errno);
        }
        rsrc_obj->speed_hz = speed;
    }

    term mode_term = interop_kv_get_value(config, ATOM_STR("\x4", "mode"), ctx->global);
    if (!term_is_invalid_term(mode_term)) {
        if (!term_is_integer(mode_term)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        avm_int_t mode_val = term_to_int(mode_term);
        if (mode_val < 0 || mode_val > 3) {
            RAISE_ERROR(BADARG_ATOM);
        }
        uint8_t mode = (uint8_t) mode_val;
        if (ioctl(rsrc_obj->fd, SPI_IOC_WR_MODE, &mode) != 0) {
            return error_errno(ctx, errno);
        }
        rsrc_obj->mode = mode;
    }

    term bits_term = interop_kv_get_value(config, ATOM_STR("\xD", "bits_per_word"), ctx->global);
    if (!term_is_invalid_term(bits_term)) {
        if (!term_is_integer(bits_term)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        avm_int_t bits_val = term_to_int(bits_term);
        if (bits_val < 4 || bits_val > 32) {
            RAISE_ERROR(BADARG_ATOM);
        }
        uint8_t bits = (uint8_t) bits_val;
        if (ioctl(rsrc_obj->fd, SPI_IOC_WR_BITS_PER_WORD, &bits) != 0) {
            return error_errno(ctx, errno);
        }
        rsrc_obj->bits_per_word = bits;
    }

    return OK_ATOM;
}

static term nif_spi_write_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    size_t len = term_binary_size(argv[1]);
    if (len == 0) {
        return term_from_int(0);
    }

    const uint8_t *tx = (const uint8_t *) term_binary_data(argv[1]);
    struct spi_ioc_transfer xfer;
    memset(&xfer, 0, sizeof(xfer));
    xfer.tx_buf = tx;
    xfer.rx_buf = NULL;
    xfer.len = (uint32_t) len;
    xfer.speed_hz = rsrc_obj->speed_hz;
    xfer.bits_per_word = rsrc_obj->bits_per_word;
    xfer.mode = rsrc_obj->mode;

    int rv = ioctl(rsrc_obj->fd, SPI_IOC_MESSAGE(1), &xfer);
    if (rv < 0) {
        return error_errno(ctx, errno);
    }
    return term_from_int((int) len);
}

static term nif_spi_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);

    avm_int_t repeated_val = term_to_int(argv[1]);
    avm_int_t len_val = term_to_int(argv[2]);
    if (repeated_val < 0 || repeated_val > 0xFF || len_val < 0) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t len = (size_t) len_val;
    if (len == 0) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(0)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term empty_bin = term_create_uninitialized_binary(0, &ctx->heap, ctx->global);
        return create_pair(ctx, OK_ATOM, empty_bin);
    }

    uint8_t *rx = malloc(len);
    uint8_t *tx = malloc(len);
    if (IS_NULL_PTR(rx) || IS_NULL_PTR(tx)) {
        free(rx);
        free(tx);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memset(tx, (uint8_t) repeated_val, len);

    struct spi_ioc_transfer xfer;
    memset(&xfer, 0, sizeof(xfer));
    xfer.tx_buf = tx;
    xfer.rx_buf = rx;
    xfer.len = (uint32_t) len;
    xfer.speed_hz = rsrc_obj->speed_hz;
    xfer.bits_per_word = rsrc_obj->bits_per_word;
    xfer.mode = rsrc_obj->mode;

    int rv = ioctl(rsrc_obj->fd, SPI_IOC_MESSAGE(1), &xfer);
    int err = errno;
    free(tx);
    if (rv < 0) {
        free(rx);
        return error_errno(ctx, err);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(len)) != MEMORY_GC_OK)) {
        free(rx);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term rx_bin = term_from_literal_binary(rx, len, &ctx->heap, ctx->global);
    free(rx);
    return create_pair(ctx, OK_ATOM, rx_bin);
}

static term nif_spi_write_read_blocking(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct SPIResource *rsrc_obj;
    if (UNLIKELY(!get_spi_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    size_t len = term_binary_size(argv[1]);
    if (len == 0) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(0)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term empty_bin = term_create_uninitialized_binary(0, &ctx->heap, ctx->global);
        return create_pair(ctx, OK_ATOM, empty_bin);
    }

    uint8_t *rx = malloc(len);
    if (IS_NULL_PTR(rx)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const uint8_t *tx = (const uint8_t *) term_binary_data(argv[1]);
    struct spi_ioc_transfer xfer;
    memset(&xfer, 0, sizeof(xfer));
    xfer.tx_buf = tx;
    xfer.rx_buf = rx;
    xfer.len = (uint32_t) len;
    xfer.speed_hz = rsrc_obj->speed_hz;
    xfer.bits_per_word = rsrc_obj->bits_per_word;
    xfer.mode = rsrc_obj->mode;

    int rv = ioctl(rsrc_obj->fd, SPI_IOC_MESSAGE(1), &xfer);
    int err = errno;
    if (rv < 0) {
        free(rx);
        return error_errno(ctx, err);
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(len)) != MEMORY_GC_OK)) {
        free(rx);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term rx_bin = term_from_literal_binary(rx, len, &ctx->heap, ctx->global);
    free(rx);
    return create_pair(ctx, OK_ATOM, rx_bin);
}

static const ErlNifResourceTypeInit SPIResourceTypeInit = {
    .members = 1,
    .dtor = spi_resource_dtor,
};

static const struct Nif spi_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_init
};

static const struct Nif spi_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_deinit
};

static const struct Nif spi_apply_device_config_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_apply_device_config
};

static const struct Nif spi_write_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_blocking
};

static const struct Nif spi_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_read_blocking
};

static const struct Nif spi_write_read_blocking_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_spi_write_read_blocking
};

static void spi_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    spi_resource_type = enif_init_resource_type(&env, "spi_resource", &SPIResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

#endif /* RTEMS_HAS_SPI */

static const struct Nif *spi_nif_get_nif(const char *nifname)
{
    if (strncmp("spi:", nifname, 4) != 0) {
        return NULL;
    }
#ifndef RTEMS_HAS_SPI
    static const struct Nif unsupported = {
        .base.type = NIFFunctionType,
        .nif_ptr = nif_spi_unsupported
    };
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0
        || strcmp("deinit/1", rest) == 0
        || strcmp("apply_device_config/2", rest) == 0
        || strcmp("write_blocking/2", rest) == 0
        || strcmp("read_blocking/3", rest) == 0
        || strcmp("write_read_blocking/2", rest) == 0) {
        return &unsupported;
    }
    return NULL;
#else
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0) {
        return &spi_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &spi_deinit_nif;
    }
    if (strcmp("apply_device_config/2", rest) == 0) {
        return &spi_apply_device_config_nif;
    }
    if (strcmp("write_blocking/2", rest) == 0) {
        return &spi_write_blocking_nif;
    }
    if (strcmp("read_blocking/3", rest) == 0) {
        return &spi_read_blocking_nif;
    }
    if (strcmp("write_read_blocking/2", rest) == 0) {
        return &spi_write_read_blocking_nif;
    }
    return NULL;
#endif
}

#ifdef RTEMS_HAS_SPI
REGISTER_NIF_COLLECTION(spi, spi_nif_init, NULL, spi_nif_get_nif)
#else
REGISTER_NIF_COLLECTION(spi, NULL, NULL, spi_nif_get_nif)
#endif
