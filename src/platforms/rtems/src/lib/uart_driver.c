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

#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

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

#define CLOSED_FD (-1)

static ErlNifResourceType *uart_resource_type;

struct UARTResource
{
    int fd;
};

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

static term error_errno(Context *ctx, int err)
{
    AtomString reason;
    switch (err) {
        case EAGAIN:
            return error_atom(ctx, ATOM_STR("\x7", "timeout"));
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

static bool get_uart_resource(Context *ctx, term resource_term, struct UARTResource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, uart_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct UARTResource *) rsrc_obj_ptr;
    return true;
}

static speed_t baud_to_speed(int baud, bool *ok)
{
    *ok = true;
    switch (baud) {
        case 9600:
            return B9600;
        case 19200:
            return B19200;
        case 38400:
            return B38400;
        case 57600:
            return B57600;
        case 115200:
            return B115200;
#ifdef B230400
        case 230400:
            return B230400;
#endif
#ifdef B460800
        case 460800:
            return B460800;
#endif
        default:
            *ok = false;
            return B115200;
    }
}

static bool configure_termios(int fd, int speed, int data_bits, int stop_bits, int parity, bool raw)
{
    struct termios tio;
    if (tcgetattr(fd, &tio) != 0) {
        return false;
    }

    if (raw) {
        cfmakeraw(&tio);
    }

    bool speed_ok = false;
    speed_t spd = baud_to_speed(speed, &speed_ok);
    if (!speed_ok) {
        errno = EINVAL;
        return false;
    }
    cfsetispeed(&tio, spd);
    cfsetospeed(&tio, spd);

    tio.c_cflag |= CLOCAL | CREAD;
    tio.c_cflag &= ~CSIZE;
    switch (data_bits) {
        case 5:
            tio.c_cflag |= CS5;
            break;
        case 6:
            tio.c_cflag |= CS6;
            break;
        case 7:
            tio.c_cflag |= CS7;
            break;
        case 8:
            tio.c_cflag |= CS8;
            break;
        default:
            errno = EINVAL;
            return false;
    }

    if (stop_bits == 2) {
        tio.c_cflag |= CSTOPB;
    } else if (stop_bits == 1) {
        tio.c_cflag &= ~CSTOPB;
    } else {
        errno = EINVAL;
        return false;
    }

    switch (parity) {
        case 0:
            tio.c_cflag &= ~PARENB;
            break;
        case 1:
            tio.c_cflag |= PARENB | PARODD;
            break;
        case 2:
            tio.c_cflag |= PARENB;
            tio.c_cflag &= ~PARODD;
            break;
        default:
            errno = EINVAL;
            return false;
    }

    tio.c_cc[VMIN] = 0;
    tio.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSANOW, &tio) != 0) {
        return false;
    }
    return true;
}

static void uart_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct UARTResource *rsrc_obj = (struct UARTResource *) obj;
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
}

static term nif_uart_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term opts = argv[0];
    VALIDATE_VALUE(opts, term_is_list);

    GlobalContext *glb = ctx->global;

    static const char *const peripheral_str = ATOM_STR("\xA", "peripheral");
    static const char *const speed_str = ATOM_STR("\x5", "speed");
    static const char *const data_bits_str = ATOM_STR("\x9", "data_bits");
    static const char *const stop_bits_str = ATOM_STR("\x9", "stop_bits");
    static const char *const parity_str = ATOM_STR("\x6", "parity");
    static const char *const raw_str = ATOM_STR("\x3", "raw");

    term peripheral_term = interop_kv_get_value(opts, peripheral_str, glb);
    if (term_is_invalid_term(peripheral_term)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term speed_term = interop_kv_get_value_default(opts, speed_str, term_from_int(115200), glb);
    term data_bits_term = interop_kv_get_value_default(opts, data_bits_str, term_from_int(8), glb);
    term stop_bits_term = interop_kv_get_value_default(opts, stop_bits_str, term_from_int(1), glb);
    term parity_term = interop_kv_get_value_default(opts, parity_str, term_from_int(0), glb);
    term raw_term = interop_kv_get_value_default(opts, raw_str, TRUE_ATOM, glb);

    VALIDATE_VALUE(speed_term, term_is_integer);
    VALIDATE_VALUE(data_bits_term, term_is_integer);
    VALIDATE_VALUE(stop_bits_term, term_is_integer);
    VALIDATE_VALUE(parity_term, term_is_integer);
    if (raw_term != TRUE_ATOM && raw_term != FALSE_ATOM) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ok = 0;
    char *path = interop_term_to_string(peripheral_term, &ok);
    if (!ok || IS_NULL_PTR(path)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int fd = open(path, O_RDWR | O_NOCTTY | O_NONBLOCK);
    free(path);
    if (fd < 0) {
        return error_errno(ctx, errno);
    }

    int speed = term_to_int(speed_term);
    int data_bits = term_to_int(data_bits_term);
    int stop_bits = term_to_int(stop_bits_term);
    int parity = term_to_int(parity_term);
    bool raw = raw_term == TRUE_ATOM;

    if (!configure_termios(fd, speed, data_bits, stop_bits, parity, raw)) {
        int err = errno;
        close(fd);
        return error_errno(ctx, err);
    }

    struct UARTResource *rsrc_obj = enif_alloc_resource(uart_resource_type, sizeof(struct UARTResource));
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

static term nif_uart_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
    return OK_ATOM;
}

static term nif_uart_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_binary);

    const uint8_t *data = (const uint8_t *) term_binary_data(argv[1]);
    size_t len = term_binary_size(argv[1]);
    ssize_t written;
    do {
        written = write(rsrc_obj->fd, data, len);
    } while (written < 0 && errno == EINTR);
    if (written < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return error_atom(ctx, ATOM_STR("\x7", "timeout"));
        }
        return error_errno(ctx, errno);
    }
    return term_from_int((int) written);
}

static term nif_uart_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    struct UARTResource *rsrc_obj;
    if (UNLIKELY(!get_uart_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (rsrc_obj->fd == CLOSED_FD) {
        RAISE_ERROR(BADARG_ATOM);
    }
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t count = term_to_int(argv[1]);
    if (UNLIKELY(count < 0 || count > 4096)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (count == 0) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size(0)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term data = term_create_uninitialized_binary(0, &ctx->heap, ctx->global);
        return create_pair(ctx, OK_ATOM, data);
    }

    uint8_t *buf = malloc((size_t) count);
    if (IS_NULL_PTR(buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    ssize_t n;
    do {
        n = read(rsrc_obj->fd, buf, (size_t) count);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {
        int err = errno;
        free(buf);
        if (err == EAGAIN || err == EWOULDBLOCK) {
            return error_atom(ctx, ATOM_STR("\x7", "timeout"));
        }
        return error_errno(ctx, err);
    }
    if (n == 0) {
        free(buf);
        return error_atom(ctx, ATOM_STR("\x7", "timeout"));
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_heap_size((size_t) n)) != MEMORY_GC_OK)) {
        free(buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term data = term_from_literal_binary(buf, (size_t) n, &ctx->heap, ctx->global);
    free(buf);
    return create_pair(ctx, OK_ATOM, data);
}

static const ErlNifResourceTypeInit UARTResourceTypeInit = {
    .members = 1,
    .dtor = uart_resource_dtor,
};

static const struct Nif uart_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_init
};

static const struct Nif uart_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_deinit
};

static const struct Nif uart_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_write
};

static const struct Nif uart_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_uart_read
};

static void uart_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    uart_resource_type = enif_init_resource_type(&env, "uart_resource", &UARTResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *uart_nif_get_nif(const char *nifname)
{
    if (strncmp("uart:", nifname, 5) != 0) {
        return NULL;
    }
    const char *rest = nifname + 5;
    if (strcmp("init/1", rest) == 0) {
        return &uart_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        return &uart_deinit_nif;
    }
    if (strcmp("write_nif/2", rest) == 0) {
        return &uart_write_nif;
    }
    if (strcmp("read_nif/2", rest) == 0) {
        return &uart_read_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(uart, uart_nif_init, NULL, uart_nif_get_nif)
