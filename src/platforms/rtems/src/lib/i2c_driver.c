/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

#ifdef RTEMS_HAS_IMX_I2C
#include <bsp.h>
#include <dev/i2c/i2c.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <unistd.h>
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

#ifdef RTEMS_HAS_IMX_I2C
#define CLOSED_FD (-1)

static ErlNifResourceType *i2c_resource_type;

struct I2CResource
{
    int fd;
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

#ifndef RTEMS_HAS_IMX_I2C
static term nif_i2c_unsupported(Context *ctx, int argc, term argv[])
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

static bool get_i2c_resource(Context *ctx, term resource_term, struct I2CResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, i2c_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct I2CResource *) rsrc_obj_ptr;
    return true;
}

static bool get_timeout_ms(term timeout_term, int *out)
{
    if (term_is_atom(timeout_term)) {
        if (timeout_term == INFINITY_ATOM) {
            *out = -1;
            return true;
        }
        return false;
    }
    if (!term_is_integer(timeout_term)) {
        return false;
    }
    avm_int_t val = term_to_int(timeout_term);
    if (val < 0 || val > INT_MAX) {
        return false;
    }
    *out = (int) val;
    return true;
}

static int set_transfer_timeout(int fd, int timeout_ms)
{
    unsigned long timeout_10ms;
    if (timeout_ms < 0) {
        timeout_10ms = 0;
    } else {
        timeout_10ms = ((unsigned long) timeout_ms + 9UL) / 10UL;
        if (timeout_10ms == 0) {
            timeout_10ms = 1;
        }
    }
    return ioctl(fd, I2C_TIMEOUT, timeout_10ms);
}

static void i2c_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct I2CResource *rsrc_obj = (struct I2CResource *) obj;
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
}

static int open_imx_bus(const char *path, const char *alias)
{
    int fd = open(path, O_RDWR);
    if (fd >= 0 || errno != ENOENT) {
        return fd;
    }

    int rv = i2c_bus_register_imx(path, alias);
    if (rv != 0) {
        return -1;
    }

    return open(path, O_RDWR);
}

static bool get_i2c_address(term address_term, uint16_t *address)
{
    if (!term_is_integer(address_term)) {
        return false;
    }
    avm_int_t value = term_to_int(address_term);
    if (value < 0 || value > 0x7F) {
        return false;
    }
    *address = (uint16_t) value;
    return true;
}

static term nif_i2c_init(Context *ctx, int argc, term argv[])
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
        path = strdup("/dev/i2c-0");
        ok = path != NULL;
    } else {
        path = interop_term_to_string(peripheral_term, &ok);
    }
    if (!ok || IS_NULL_PTR(path)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ok = 0;
    if (term_is_invalid_term(fdt_alias_term)) {
        alias = strdup("i2c0");
        ok = alias != NULL;
    } else {
        alias = interop_term_to_string(fdt_alias_term, &ok);
    }
    if (!ok || IS_NULL_PTR(alias)) {
        free(path);
        RAISE_ERROR(BADARG_ATOM);
    }

    int fd = open_imx_bus(path, alias);
    int err = errno;
    free(alias);
    free(path);
    if (fd < 0) {
        return error_errno(ctx, err ? err : EIO);
    }

    struct I2CResource *rsrc_obj = enif_alloc_resource(i2c_resource_type, sizeof(struct I2CResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        close(fd);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->fd = fd;

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

static term nif_i2c_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
    return OK_ATOM;
}

static term nif_i2c_master_transmit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_binary);
    int timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[3], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(set_transfer_timeout(rsrc_obj->fd, timeout_ms) != 0)) {
        return error_errno(ctx, errno);
    }

    uint16_t addr;
    if (UNLIKELY(!get_i2c_address(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint8_t *data = (uint8_t *) term_binary_data(argv[2]);
    size_t len = term_binary_size(argv[2]);
    if (UNLIKELY(len > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct i2c_msg msg = {
        .addr = addr,
        .flags = 0,
        .len = (uint16_t) len,
        .buf = data
    };
    struct i2c_rdwr_ioctl_data xfer = {
        .msgs = &msg,
        .nmsgs = 1
    };
    if (ioctl(rsrc_obj->fd, I2C_RDWR, &xfer) != 0) {
        return error_errno(ctx, errno);
    }
    return term_from_int((int) len);
}

static term nif_i2c_master_receive(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_integer);
    int timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[3], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(set_transfer_timeout(rsrc_obj->fd, timeout_ms) != 0)) {
        return error_errno(ctx, errno);
    }

    uint16_t addr;
    if (UNLIKELY(!get_i2c_address(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t count = term_to_int(argv[2]);
    if (UNLIKELY(count < 0 || count > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size((size_t) count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary((size_t) count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    struct i2c_msg msg = {
        .addr = addr,
        .flags = I2C_M_RD,
        .len = (uint16_t) count,
        .buf = buf
    };
    struct i2c_rdwr_ioctl_data xfer = {
        .msgs = &msg,
        .nmsgs = 1
    };
    if (ioctl(rsrc_obj->fd, I2C_RDWR, &xfer) != 0) {
        return error_errno(ctx, errno);
    }
    return create_pair(ctx, OK_ATOM, data);
}

static bool encode_mem_addr(uint16_t mem_addr, uint16_t mem_addr_size, uint8_t *out, uint16_t *len)
{
    if (mem_addr_size == 16) {
        out[0] = (uint8_t) (mem_addr >> 8);
        out[1] = (uint8_t) (mem_addr & 0xFF);
        *len = 2;
        return true;
    }
    if (mem_addr_size == 8) {
        out[0] = (uint8_t) (mem_addr & 0xFF);
        *len = 1;
        return true;
    }
    return false;
}

static term nif_i2c_mem_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);
    VALIDATE_VALUE(argv[4], term_is_integer);
    int timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[5], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(set_transfer_timeout(rsrc_obj->fd, timeout_ms) != 0)) {
        return error_errno(ctx, errno);
    }

    uint16_t addr;
    if (UNLIKELY(!get_i2c_address(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t mem_addr_value = term_to_int(argv[2]);
    if (UNLIKELY(mem_addr_value < 0 || mem_addr_value > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t mem_addr = (uint16_t) mem_addr_value;
    uint16_t mem_addr_size = (uint16_t) term_to_int(argv[3]);
    avm_int_t count = term_to_int(argv[4]);
    if (UNLIKELY(count < 0 || count > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t reg_buf[2];
    uint16_t reg_len;
    if (!encode_mem_addr(mem_addr, mem_addr_size, reg_buf, &reg_len)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + term_binary_heap_size((size_t) count), MEMORY_NO_GC) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_create_uninitialized_binary((size_t) count, &ctx->heap, ctx->global);
    uint8_t *buf = (uint8_t *) term_binary_data(data);

    struct i2c_msg msgs[2] = {
        { .addr = addr, .flags = 0, .len = reg_len, .buf = reg_buf },
        { .addr = addr, .flags = I2C_M_RD, .len = (uint16_t) count, .buf = buf }
    };
    struct i2c_rdwr_ioctl_data xfer = {
        .msgs = msgs,
        .nmsgs = 2
    };
    if (ioctl(rsrc_obj->fd, I2C_RDWR, &xfer) != 0) {
        return error_errno(ctx, errno);
    }
    return create_pair(ctx, OK_ATOM, data);
}

static term nif_i2c_mem_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct I2CResource *rsrc_obj;
    if (UNLIKELY(!get_i2c_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);
    VALIDATE_VALUE(argv[4], term_is_binary);
    int timeout_ms;
    if (UNLIKELY(!get_timeout_ms(argv[5], &timeout_ms))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(set_transfer_timeout(rsrc_obj->fd, timeout_ms) != 0)) {
        return error_errno(ctx, errno);
    }

    uint16_t addr;
    if (UNLIKELY(!get_i2c_address(argv[1], &addr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t mem_addr_value = term_to_int(argv[2]);
    if (UNLIKELY(mem_addr_value < 0 || mem_addr_value > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint16_t mem_addr = (uint16_t) mem_addr_value;
    uint16_t mem_addr_size = (uint16_t) term_to_int(argv[3]);
    const uint8_t *payload = (const uint8_t *) term_binary_data(argv[4]);
    size_t payload_len = term_binary_size(argv[4]);

    uint8_t reg_buf[2];
    uint16_t reg_len;
    if (!encode_mem_addr(mem_addr, mem_addr_size, reg_buf, &reg_len)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(reg_len + payload_len > UINT16_MAX)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    uint8_t *tx = malloc(reg_len + payload_len);
    if (IS_NULL_PTR(tx)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    memcpy(tx, reg_buf, reg_len);
    memcpy(tx + reg_len, payload, payload_len);

    struct i2c_msg msg = {
        .addr = addr,
        .flags = 0,
        .len = (uint16_t) (reg_len + payload_len),
        .buf = tx
    };
    struct i2c_rdwr_ioctl_data xfer = {
        .msgs = &msg,
        .nmsgs = 1
    };
    int rv = ioctl(rsrc_obj->fd, I2C_RDWR, &xfer);
    int err = errno;
    free(tx);
    if (rv != 0) {
        return error_errno(ctx, err);
    }
    return term_from_int((int) payload_len);
}

static const ErlNifResourceTypeInit I2CResourceTypeInit = {
    .members = 1,
    .dtor = i2c_resource_dtor,
};

static const struct Nif i2c_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_init
};

static const struct Nif i2c_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_deinit
};

static const struct Nif i2c_master_transmit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_master_transmit
};

static const struct Nif i2c_master_receive_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_master_receive
};

static const struct Nif i2c_mem_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_read
};

static const struct Nif i2c_mem_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_i2c_mem_write
};

static void i2c_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    i2c_resource_type = enif_init_resource_type(&env, "i2c_resource", &I2CResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

#endif /* RTEMS_HAS_IMX_I2C */

static const struct Nif *i2c_nif_get_nif(const char *nifname)
{
    if (strncmp("i2c:", nifname, 4) != 0) {
        return NULL;
    }
#ifndef RTEMS_HAS_IMX_I2C
    static const struct Nif unsupported = {
        .base.type = NIFFunctionType,
        .nif_ptr = nif_i2c_unsupported
    };
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0
        || strcmp("deinit/1", rest) == 0
        || strcmp("master_transmit/4", rest) == 0
        || strcmp("master_receive/4", rest) == 0
        || strcmp("mem_read/6", rest) == 0
        || strcmp("mem_write/6", rest) == 0) {
        return &unsupported;
    }
    return NULL;
#else
    const char *rest = nifname + 4;
    if (strcmp("init/1", rest) == 0) {
        return &i2c_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &i2c_deinit_nif;
    }
    if (strcmp("master_transmit/4", rest) == 0) {
        return &i2c_master_transmit_nif;
    }
    if (strcmp("master_receive/4", rest) == 0) {
        return &i2c_master_receive_nif;
    }
    if (strcmp("mem_read/6", rest) == 0) {
        return &i2c_mem_read_nif;
    }
    if (strcmp("mem_write/6", rest) == 0) {
        return &i2c_mem_write_nif;
    }
    return NULL;
#endif
}

#ifdef RTEMS_HAS_IMX_I2C
REGISTER_NIF_COLLECTION(i2c, i2c_nif_init, NULL, i2c_nif_get_nif)
#else
REGISTER_NIF_COLLECTION(i2c, NULL, NULL, i2c_nif_get_nif)
#endif
