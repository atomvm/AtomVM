/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

/**
 * @file posix_nifs.c
 * @brief Implementation of NIFs based on POSIX functions
 */

#if HAVE_OPEN && HAVE_CLOSE
#include <fcntl.h>
#endif
#if HAVE_OPEN && HAVE_CLOSE || defined(HAVE_GETCWD) && defined(HAVE_PATH_MAX)
#include <unistd.h>
#endif
#if HAVE_RENAME
#include <stdio.h>
#endif
#if HAVE_MKFIFO || HAVE_STAT || HAVE_FSTAT
#include <sys/stat.h>
#include <sys/types.h>
#endif

#if HAVE_CLOCK_SETTIME
#include <time.h>
#elif HAVE_SETTIMEOFDAY
#include <sys/time.h>
#endif

#include <errno.h>

#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
#include <dirent.h>
#endif

#if HAVE_POSIX_SPAWN_CLOEXEC_DEFAULT
#include <spawn.h>
#endif

#include "defaultatoms.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "interop.h"
#include "nifs.h"
#include "posix_nifs.h"

#if HAVE_EXECVE
extern char **environ;
#endif

term posix_errno_to_term(int err, GlobalContext *glb)
{
#if HAVE_OPEN && HAVE_CLOSE || defined(HAVE_CLOCK_SETTIME) || defined(HAVE_SETTIMEOFDAY)
    // These are defined in SUSv1
    switch (err) {
        case EACCES:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "eacces"));
        case EAGAIN:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "eagain"));
        case EBADF:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "ebadf"));
        case EBUSY:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "ebusy"));
        case EDQUOT:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "edquot"));
        case EEXIST:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "eexist"));
        case EFAULT:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "efault"));
        case EFBIG:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "efbig"));
        case EINTR:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "eintr"));
        case EINVAL:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "einval"));
        case EIO:
            return globalcontext_make_atom(glb, ATOM_STR("\x3", "eio"));
        case EISDIR:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "eisdir"));
        case ELOOP:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "eloop"));
        case EMFILE:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "emfile"));
        case EMLINK:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "emlink"));
        case ENAMETOOLONG:
            return globalcontext_make_atom(glb, ATOM_STR("\xC", "enametoolong"));
        case ENFILE:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "enfile"));
        case ENODEV:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "enodev"));
        case ENOENT:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "enoent"));
        case ENOMEM:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "enomem"));
        case ENOSPC:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "enospc"));
        case ENOTDIR:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "enotdir"));
        case ENXIO:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "enxio"));
        case EPERM:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "eperm"));
        case EPIPE:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "epipe"));
        case EROFS:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "erofs"));
        case ESPIPE:
            return globalcontext_make_atom(glb, ATOM_STR("\x6", "espipe"));
        case ESRCH:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "esrch"));
        case EXDEV:
            return globalcontext_make_atom(glb, ATOM_STR("\x5", "exdev"));
        case EPROTOTYPE:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "eprototype"));
        case ENOTCONN:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "enotconn"));
        case EOPNOTSUPP:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "eopnotsupp"));
    }
#else
    UNUSED(glb);
#endif
    return term_from_int(err);
}

static term error_tuple_maybe_gc(int err, Context *ctx)
{
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, ERROR_ATOM);
    term_put_tuple_element(result, 1, posix_errno_to_term(err, ctx->global));

    return result;
}

static term errno_to_error_tuple_maybe_gc(Context *ctx)
{
    return error_tuple_maybe_gc(errno, ctx);
}

#if HAVE_OPEN && HAVE_CLOSE
#define CLOSED_FD (-1)

struct PosixFd
{
    int fd;
    int32_t selecting_process_id;
    ErlNifMonitor selecting_process_monitor;
};

static void posix_fd_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct PosixFd *fd_obj = (struct PosixFd *) obj;
    if (fd_obj->fd != CLOSED_FD) {
        close(fd_obj->fd);
        fd_obj->fd = CLOSED_FD;
    }
}

static void posix_fd_stop(ErlNifEnv *caller_env, void *obj, ErlNifEvent event, int is_direct_call)
{
    UNUSED(event);
    UNUSED(is_direct_call);

    struct PosixFd *fd_obj = (struct PosixFd *) obj;
    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        enif_demonitor_process(caller_env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
}

static void posix_fd_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(pid);
    UNUSED(mon);
    struct PosixFd *fd_obj = (struct PosixFd *) obj;
    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        enif_select(caller_env, fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil());
    }
}

const ErlNifResourceTypeInit posix_fd_resource_type_init = {
    .members = 3,
    .dtor = posix_fd_dtor,
    .stop = posix_fd_stop,
    .down = posix_fd_down,
};

#define O_EXEC_ATOM_STR ATOM_STR("\x6", "o_exec")
#define O_RDONLY_ATOM_STR ATOM_STR("\x8", "o_rdonly")
#define O_RDWR_ATOM_STR ATOM_STR("\x6", "o_rdwr")
#define O_SEARCH_ATOM_STR ATOM_STR("\x8", "o_search")
#define O_WRONLY_ATOM_STR ATOM_STR("\x8", "o_wronly")

#define O_APPEND_ATOM_STR ATOM_STR("\x8", "o_append")
#define O_CLOEXEC_ATOM_STR ATOM_STR("\x9", "o_cloexec")
#define O_CREAT_ATOM_STR ATOM_STR("\x7", "o_creat")
#define O_DIRECTORY_ATOM_STR ATOM_STR("\xB", "o_directory")
#define O_DSYNC_ATOM_STR ATOM_STR("\x7", "o_dsync")
#define O_EXCL_ATOM_STR ATOM_STR("\x6", "o_excl")
#define O_NOCTTY_ATOM_STR ATOM_STR("\x8", "o_noctty")
#define O_NOFOLLOW_ATOM_STR ATOM_STR("\xA", "o_nofollow")
// #define O_NONBLOCK_ATOM_STR ATOM_STR("\xA", "o_nonblock")
#define O_RSYNC_ATOM_STR ATOM_STR("\x7", "o_rsync")
#define O_SYNC_ATOM_STR ATOM_STR("\x6", "o_sync")
#define O_TRUNC_ATOM_STR ATOM_STR("\x7", "o_trunc")
#define O_TTY_INIT_ATOM_STR ATOM_STR("\xA", "o_tty_init")

static term make_posix_fd_resource(Context *ctx, int fd)
{
    // Return a resource object
    struct PosixFd *fd_obj = enif_alloc_resource(ctx->global->posix_fd_resource_type, sizeof(struct PosixFd));
    if (IS_NULL_PTR(fd_obj)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    fd_obj->fd = fd;
    fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFERENCE_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        enif_release_resource(fd_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(fd_obj, &ctx->heap);
    enif_release_resource(fd_obj); // decrement refcount after enif_alloc_resource
    return obj;
}

static term nif_atomvm_posix_open(Context *ctx, int argc, term argv[])
{
    GlobalContext *glb = ctx->global;

    term path_term = argv[0];
    term flags = argv[1];
    VALIDATE_VALUE(flags, term_is_list);
    int posix_flags = O_NONBLOCK; // Always be non-blocking
    while (term_is_nonempty_list(flags)) {
        term flag = term_get_list_head(flags);
        VALIDATE_VALUE(flag, term_is_atom);
        // SUSv1 flags
        if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_RDONLY_ATOM_STR)) {
            posix_flags |= O_RDONLY;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_WRONLY_ATOM_STR)) {
            posix_flags |= O_WRONLY;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_RDWR_ATOM_STR)) {
            posix_flags |= O_RDWR;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_APPEND_ATOM_STR)) {
            posix_flags |= O_APPEND;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_CREAT_ATOM_STR)) {
            posix_flags |= O_CREAT;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_EXCL_ATOM_STR)) {
            posix_flags |= O_EXCL;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_NOCTTY_ATOM_STR)) {
            posix_flags |= O_NOCTTY;
            // O_NONBLOCK is not optional
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_SYNC_ATOM_STR)) {
            posix_flags |= O_SYNC;
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_TRUNC_ATOM_STR)) {
            posix_flags |= O_TRUNC;
            // SUSv4/2018 edition flags
#if HAVE_O_EXEC
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_EXEC_ATOM_STR)) {
            posix_flags |= O_EXEC;
#endif
#if HAVE_O_SEARCH
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_SEARCH_ATOM_STR)) {
            posix_flags |= O_SEARCH;
#endif
#if HAVE_O_CLOEXEC
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_CLOEXEC_ATOM_STR)) {
            posix_flags |= O_CLOEXEC;
#endif
#if HAVE_O_DIRECTORY
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_DIRECTORY_ATOM_STR)) {
            posix_flags |= O_DIRECTORY;
#endif
#if HAVE_O_DSYNC
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_DSYNC_ATOM_STR)) {
            posix_flags |= O_DSYNC;
#endif
#if HAVE_O_NOFOLLOW
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_NOFOLLOW_ATOM_STR)) {
            posix_flags |= O_NOFOLLOW;
#endif
#if HAVE_O_RSYNC
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_RSYNC_ATOM_STR)) {
            posix_flags |= O_RSYNC;
#endif
#if HAVE_O_TTY_INIT
        } else if (globalcontext_is_term_equal_to_atom_string(glb, flag, O_TTY_INIT_ATOM_STR)) {
            posix_flags |= O_TTY_INIT;
#endif
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
        flags = term_get_list_tail(flags);
    }

    int ok;
    char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term result;
    int fd;
    if (argc == 3) {
        term mode_term = argv[2];
        if (UNLIKELY(!term_is_integer(mode_term))) {
            free(path);
            RAISE_ERROR(BADARG_ATOM);
        }
        // POSIX says effect of bits other than file permissions is unspecified
        // defined, so we'll happily cast
        mode_t mode = (mode_t) term_to_int(mode_term);
        fd = open(path, posix_flags, mode);
    } else {
        if (UNLIKELY(posix_flags & O_CREAT)) {
            free(path);
            RAISE_ERROR(BADARG_ATOM);
        }
        fd = open(path, posix_flags);
    }
    free(path);
    if (UNLIKELY(fd < 0)) {
        // Return an error.
        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, posix_errno_to_term(errno, glb));
    } else {
        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + TERM_BOXED_REFERENCE_RESOURCE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term obj = make_posix_fd_resource(ctx, fd);
        if (term_is_invalid_term(obj)) {
            return obj;
        }
        result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, obj);
    }

    return result;
}

static term nif_atomvm_posix_close(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term result = OK_ATOM;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    if (fd_obj->fd != CLOSED_FD) {
        if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
            fprintf(stderr, "Calling close on a selectable posix file, missing call to posix_select_stop?\n");
        }
        if (UNLIKELY(close(fd_obj->fd) < 0)) {
            fd_obj->fd = CLOSED_FD; // even if bad things happen, do not close twice.
            return errno_to_error_tuple_maybe_gc(ctx);
        }
        fd_obj->fd = CLOSED_FD;
    }

    return result;
}

static term nif_atomvm_posix_read(Context *ctx, int argc, term argv[])
{
    GlobalContext *glb = ctx->global;
    UNUSED(argc);
    term count_term = argv[1];
    VALIDATE_VALUE(count_term, term_is_integer);
    avm_int_t count = term_to_int(count_term);

    size_t size = term_binary_data_size_in_terms(count) + BINARY_HEADER_SIZE + TERM_BOXED_SUB_BINARY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, size, argc, argv, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    void *fd_obj_ptr;

    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], glb->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    term bin_term = term_create_uninitialized_binary(count, &ctx->heap, glb);
    ssize_t res = read(fd_obj->fd, (void *) term_binary_data(bin_term), count);
    if (UNLIKELY(res < 0)) {
        // Return an error.
        return errno_to_error_tuple_maybe_gc(ctx);
    }
    if (res == 0) {
        return globalcontext_make_atom(glb, ATOM_STR("\x3", "eof"));
    }
    if (res < count) {
        bin_term = term_alloc_sub_binary(bin_term, 0, res, &ctx->heap);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, bin_term);

    return result;
}

static term nif_atomvm_posix_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term data_term = argv[1];
    VALIDATE_VALUE(data_term, term_is_binary);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    const char *data = term_binary_data(data_term);
    unsigned long n = term_binary_size(data_term);
    term result;
    ssize_t res = write(fd_obj->fd, data, n);
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    result = term_alloc_tuple(2, &ctx->heap);
    if (res < 0) {
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, posix_errno_to_term(errno, ctx->global));
    } else {
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, term_from_int(res));
    }

    return result;
}

#if HAVE_OPEN && HAVE_CLOSE && HAVE_LSEEK
static inline bool term_is_off_t(term t)
{
    if (!term_is_int64(t)) {
        return false;
    }
    if (sizeof(off_t) == 4) {
        int64_t v = term_to_int64(t);
        if (v < INT32_MIN || v > INT32_MAX) {
            return false;
        }
    }
    return true;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && (HAVE_PREAD || HAVE_PWRITE || HAVE_FTRUNCATE)
static inline bool term_is_non_neg_off_t(term t)
{
    if (!term_is_int64(t)) {
        return false;
    }
    int64_t v = term_to_int64(t);
    if (v < 0 || ((sizeof(off_t) == 4) && (v > INT32_MAX))) {
        return false;
    }
    return true;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_LSEEK
static term nif_atomvm_posix_seek(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(
            erl_nif_env_from_context(ctx), argv[0], glb->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;

    term offset_term = argv[1];
    VALIDATE_VALUE(offset_term, term_is_off_t);
    int64_t offset = term_to_int64(offset_term);

    term whence_term = argv[2];
    VALIDATE_VALUE(whence_term, term_is_atom);
    int whence;
    if (globalcontext_is_term_equal_to_atom_string(glb, whence_term, ATOM_STR("\x8", "seek_set"))) {
        whence = SEEK_SET;
    } else if (globalcontext_is_term_equal_to_atom_string(
                   glb, whence_term, ATOM_STR("\x8", "seek_cur"))) {
        whence = SEEK_CUR;
    } else if (globalcontext_is_term_equal_to_atom_string(
                   glb, whence_term, ATOM_STR("\x8", "seek_end"))) {
        whence = SEEK_END;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    off_t res = lseek(fd_obj->fd, offset, whence);
    if (res == (off_t) -1) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + BOXED_INT64_SIZE, MEMORY_CAN_SHRINK)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, term_make_maybe_boxed_int64(res, &ctx->heap));

    return result;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_PREAD
static term nif_atomvm_posix_pread(Context *ctx, int argc, term argv[])
{
    GlobalContext *glb = ctx->global;
    UNUSED(argc);

    term count_term = argv[1];
    VALIDATE_VALUE(count_term, term_is_non_neg_int);
    size_t count = term_to_int(count_term);

    term offset_term = argv[2];
    VALIDATE_VALUE(offset_term, term_is_non_neg_off_t);
    int64_t offset = term_to_int64(offset_term);

    size_t size = term_binary_data_size_in_terms(count) + BINARY_HEADER_SIZE
        + TERM_BOXED_SUB_BINARY_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, size, argc, argv, MEMORY_CAN_SHRINK)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(
            erl_nif_env_from_context(ctx), argv[0], glb->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    term bin_term = term_create_uninitialized_binary(count, &ctx->heap, glb);
    ssize_t res = pread(fd_obj->fd, (void *) term_binary_data(bin_term), count, offset);
    if (UNLIKELY(res < 0)) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }
    if (res == 0) {
        return globalcontext_make_atom(glb, ATOM_STR("\x3", "eof"));
    }
    if ((size_t) res < count) {
        bin_term = term_alloc_sub_binary(bin_term, 0, res, &ctx->heap);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, bin_term);

    return result;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_PWRITE
static term nif_atomvm_posix_pwrite(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term data_term = argv[1];
    VALIDATE_VALUE(data_term, term_is_binary);

    term offset_term = argv[2];
    VALIDATE_VALUE(offset_term, term_is_non_neg_off_t);
    int64_t offset = term_to_int64(offset_term);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    const void *data = term_binary_data(data_term);
    unsigned long n = term_binary_size(data_term);
    term result;
    ssize_t res = pwrite(fd_obj->fd, data, n, offset);
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    result = term_alloc_tuple(2, &ctx->heap);
    if (res < 0) {
        term_put_tuple_element(result, 0, ERROR_ATOM);
        term_put_tuple_element(result, 1, posix_errno_to_term(errno, ctx->global));
    } else {
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, term_from_int(res));
    }

    return result;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_FSYNC
static term nif_atomvm_posix_fsync(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    if (UNLIKELY(fsync(fd_obj->fd) < 0)) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    return OK_ATOM;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_FTRUNCATE
static term nif_atomvm_posix_ftruncate(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;

    term length_term = argv[1];
    VALIDATE_VALUE(length_term, term_is_non_neg_off_t);
    int64_t length = term_to_int64(length_term);

    if (UNLIKELY(ftruncate(fd_obj->fd, length) < 0)) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    return OK_ATOM;
}
#endif

static term nif_atomvm_posix_select(Context *ctx, term argv[], enum ErlNifSelectFlags mode)
{
    term process_pid_term = argv[1];
    VALIDATE_VALUE(process_pid_term, term_is_local_pid);
    int32_t process_pid = term_to_local_process_id(process_pid_term);
    term select_ref_term = argv[2];
    if (select_ref_term != UNDEFINED_ATOM) {
        VALIDATE_VALUE(select_ref_term, term_is_local_reference);
    }
    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (fd_obj->selecting_process_id != process_pid && fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        if (UNLIKELY(enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
    // Monitor first as select is less likely to fail and it's less expensive to demonitor
    // if select fails than to stop select if monitor fails
    if (fd_obj->selecting_process_id != process_pid) {
        if (UNLIKELY(enif_monitor_process(env, fd_obj, &process_pid, &fd_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        fd_obj->selecting_process_id = process_pid;
    }
    if (UNLIKELY(enif_select(env, fd_obj->fd, mode, fd_obj, &process_pid, select_ref_term) < 0)) {
        enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

static term nif_atomvm_posix_select_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return nif_atomvm_posix_select(ctx, argv, ERL_NIF_SELECT_READ);
}

static term nif_atomvm_posix_select_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return nif_atomvm_posix_select(ctx, argv, ERL_NIF_SELECT_WRITE);
}

static term nif_atomvm_posix_select_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;
    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil()) < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

#if HAVE_EXECVE
static void free_string_list(char **list)
{
    if (IS_NULL_PTR(list)) {
        return;
    }
    char **ptr = list;
    while (*ptr) {
        char *str = *ptr;
        free(str);
        ptr++;
    }
    free(list);
}

static char **parse_string_list(term list)
{
    if (!term_is_list(list)) {
        return NULL;
    }
    int proper;
    size_t result_len = term_list_length(list, &proper);
    if (UNLIKELY(!proper)) {
        return NULL;
    }
    // All items are initialized to NULL.
    char **result_list = calloc(result_len + 1, sizeof(char *));
    if (IS_NULL_PTR(result_list)) {
        return NULL;
    }
    term list_item = list;
    int i = 0;
    while (term_is_nonempty_list(list_item)) {
        term item = term_get_list_head(list_item);
        char *str = interop_term_to_string(item, &proper);
        if (UNLIKELY(!proper)) {
            free_string_list(result_list);
            return NULL;
        }
        result_list[i++] = str;
        list_item = term_get_list_tail(list_item);
    }
    return result_list;
}

static term nif_atomvm_subprocess(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    int ok;
    char *path = interop_term_to_string(argv[0], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char **args = parse_string_list(argv[1]);
    if (IS_NULL_PTR(args)) {
        free(path);
        RAISE_ERROR(BADARG_ATOM);
    }
    char **envp;
    char **envp_array = NULL;
    if (argv[2] == UNDEFINED_ATOM) {
        envp = environ;
    } else {
        envp_array = parse_string_list(argv[2]);
        if (IS_NULL_PTR(envp_array)) {
            free(path);
            free_string_list(args);
            RAISE_ERROR(BADARG_ATOM);
        }
        envp = envp_array;
    }

    int pstdout[2];
    int r = pipe(pstdout);
    if (r < 0) {
        free(path);
        free_string_list(args);
        free_string_list(envp_array);
        return errno_to_error_tuple_maybe_gc(ctx);
    }
    pid_t pid;
#if HAVE_POSIX_SPAWN_CLOEXEC_DEFAULT
    do {
        posix_spawn_file_actions_t file_actions;
        posix_spawnattr_t spawn_attrs;
        if (UNLIKELY((r = posix_spawn_file_actions_init(&file_actions)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawn_file_actions_adddup2(&file_actions, pstdout[1], 1)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawnattr_init(&spawn_attrs)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawnattr_setflags(&spawn_attrs, HAVE_POSIX_SPAWN_CLOEXEC_DEFAULT)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawn(&pid, path, &file_actions, &spawn_attrs, args, envp)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawnattr_destroy(&spawn_attrs)) != 0)) {
            break;
        }
        if (UNLIKELY((r = posix_spawn_file_actions_destroy(&file_actions)) != 0)) {
            break;
        }
    } while (false);
    if (UNLIKELY(r != 0)) {
        free(path);
        free_string_list(args);
        free_string_list(envp_array);
        close(pstdout[0]);
        close(pstdout[1]);
        return error_tuple_maybe_gc(r, ctx);
    }
#else
    r = fork();
    if (r < 0) {
        int err = errno;
        free(path);
        free_string_list(args);
        free_string_list(envp_array);
        close(pstdout[0]);
        close(pstdout[1]);
        return error_tuple_maybe_gc(err, ctx);
    }
    if (r == 0) {
        // child.
        close(0); // close stdin of the child
        close(pstdout[0]); // close read end of the pipe
        dup2(pstdout[1], 1); // make stdout the write-end of the pipe
#if HAVE_CLOSEFROM
        closefrom(2);
#else
        int maxfd = sysconf(_SC_OPEN_MAX);
        for (int fd = 3; fd < maxfd; fd++) {
            close(fd);
        }
#endif
        execve(path, args, envp);
        exit(1);
    }
    pid = r;
#endif
    // parent
    close(pstdout[1]); // close write-end of the pipe
    free(path);
    free_string_list(args);
    free_string_list(envp_array);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(3) + TERM_BOXED_REFERENCE_RESOURCE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term stdout_term = make_posix_fd_resource(ctx, pstdout[0]);
    if (term_is_invalid_term(stdout_term)) {
        return stdout_term;
    }
    term result = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, term_from_int(pid));
    term_put_tuple_element(result, 2, stdout_term);

    return result;
}
#endif
#endif

#if HAVE_MKFIFO
static term nif_atomvm_posix_mkfifo(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term path_term = argv[0];
    term mode_term = argv[1];
    VALIDATE_VALUE(mode_term, term_is_integer);

    int ok;
    const char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // POSIX says effects of bits other than file permissions are implementation
    // defined, so we'll happily cast
    mode_t mode = (mode_t) term_to_int(mode_term);

    int res = mkfifo(path, mode);
    free((void *) path);

    if (res < 0) {
        // Return an error.
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    return OK_ATOM;
}
#endif

#if HAVE_UNLINK
static term nif_atomvm_posix_unlink(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term path_term = argv[0];

    int ok;
    const char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int res = unlink(path);
    free((void *) path);
    if (res < 0) {
        // Return an error.
        return errno_to_error_tuple_maybe_gc(ctx);
    }
    return OK_ATOM;
}
#endif

#if HAVE_RENAME
static term nif_atomvm_posix_rename(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term old_path_term = argv[0];
    term new_path_term = argv[1];

    int ok;
    char *old_path = interop_term_to_string(old_path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char *new_path = interop_term_to_string(new_path_term, &ok);
    if (UNLIKELY(!ok)) {
        free(old_path);
        RAISE_ERROR(BADARG_ATOM);
    }

    int res = rename(old_path, new_path);
    free(old_path);
    free(new_path);
    if (res < 0) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }
    return OK_ATOM;
}
#endif

#if HAVE_STAT || HAVE_FSTAT
#define STAT_MAP_NUM_ENTRIES 10

static term stat_to_result_maybe_gc(Context *ctx, struct stat *st)
{
    GlobalContext *glb = ctx->global;

    term st_dev_atom = globalcontext_make_atom(glb, ATOM_STR("\x6", "st_dev"));
    term st_ino_atom = globalcontext_make_atom(glb, ATOM_STR("\x6", "st_ino"));
    term st_mode_atom = globalcontext_make_atom(glb, ATOM_STR("\x7", "st_mode"));
    term st_nlink_atom = globalcontext_make_atom(glb, ATOM_STR("\x8", "st_nlink"));
    term st_uid_atom = globalcontext_make_atom(glb, ATOM_STR("\x6", "st_uid"));
    term st_gid_atom = globalcontext_make_atom(glb, ATOM_STR("\x6", "st_gid"));
    term st_size_atom = globalcontext_make_atom(glb, ATOM_STR("\x7", "st_size"));
    term st_atime_s_atom = globalcontext_make_atom(glb, ATOM_STR("\xa", "st_atime_s"));
    term st_mtime_s_atom = globalcontext_make_atom(glb, ATOM_STR("\xa", "st_mtime_s"));
    term st_ctime_s_atom = globalcontext_make_atom(glb, ATOM_STR("\xa", "st_ctime_s"));

    if (UNLIKELY(term_is_invalid_term(st_dev_atom) || term_is_invalid_term(st_ino_atom)
            || term_is_invalid_term(st_mode_atom) || term_is_invalid_term(st_nlink_atom)
            || term_is_invalid_term(st_uid_atom) || term_is_invalid_term(st_gid_atom)
            || term_is_invalid_term(st_size_atom) || term_is_invalid_term(st_atime_s_atom)
            || term_is_invalid_term(st_mtime_s_atom) || term_is_invalid_term(st_ctime_s_atom))) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    // 10 fields in the map, each potentially a boxed int64
    size_t needed = TUPLE_SIZE(2) + TERM_MAP_SIZE(STAT_MAP_NUM_ENTRIES)
        + STAT_MAP_NUM_ENTRIES * BOXED_INT64_SIZE;
    if (UNLIKELY(memory_ensure_free_opt(ctx, needed, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term map = term_alloc_map(STAT_MAP_NUM_ENTRIES, &ctx->heap);

    term_set_map_assoc(map, 0, st_atime_s_atom,
        term_make_maybe_boxed_int64((avm_int64_t) st->st_atime, &ctx->heap));
    term_set_map_assoc(map, 1, st_ctime_s_atom,
        term_make_maybe_boxed_int64((avm_int64_t) st->st_ctime, &ctx->heap));
    term_set_map_assoc(
        map, 2, st_dev_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_dev, &ctx->heap));
    term_set_map_assoc(
        map, 3, st_gid_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_gid, &ctx->heap));
    term_set_map_assoc(
        map, 4, st_ino_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_ino, &ctx->heap));
    term_set_map_assoc(
        map, 5, st_mode_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_mode, &ctx->heap));
    term_set_map_assoc(map, 6, st_mtime_s_atom,
        term_make_maybe_boxed_int64((avm_int64_t) st->st_mtime, &ctx->heap));
    term_set_map_assoc(
        map, 7, st_nlink_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_nlink, &ctx->heap));
    term_set_map_assoc(
        map, 8, st_size_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_size, &ctx->heap));
    term_set_map_assoc(
        map, 9, st_uid_atom, term_make_maybe_boxed_int64((avm_int64_t) st->st_uid, &ctx->heap));

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, map);

    return result;
}
#endif

#if HAVE_STAT
static term nif_atomvm_posix_stat(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term path_term = argv[0];

    int ok;
    char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct stat st;
    int res = stat(path, &st);
    free(path);
    if (res < 0) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    return stat_to_result_maybe_gc(ctx, &st);
}
#endif

#if HAVE_OPEN && HAVE_CLOSE && HAVE_FSTAT
static term nif_atomvm_posix_fstat(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            ctx->global->posix_fd_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixFd *fd_obj = (struct PosixFd *) fd_obj_ptr;

    struct stat st;
    if (UNLIKELY(fstat(fd_obj->fd, &st) < 0)) {
        return errno_to_error_tuple_maybe_gc(ctx);
    }

    return stat_to_result_maybe_gc(ctx, &st);
}
#endif

#if defined(HAVE_CLOCK_SETTIME) || defined(HAVE_SETTIMEOFDAY)
static term nif_atomvm_posix_clock_settime(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_atom);
    if (!globalcontext_is_term_equal_to_atom_string(ctx->global, argv[0], ATOM_STR("\x8", "realtime"))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(argv[1], term_is_tuple);
    if (term_get_tuple_arity(argv[1]) != 2) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term secs = term_get_tuple_element(argv[1], 0);
    VALIDATE_VALUE(secs, term_is_any_integer);
    avm_int64_t s = term_maybe_unbox_int64(secs);

    term nsecs = term_get_tuple_element(argv[1], 1);
    VALIDATE_VALUE(nsecs, term_is_any_integer);
    avm_int64_t ns = term_maybe_unbox_int64(nsecs);

#ifdef HAVE_CLOCK_SETTIME
    struct timespec tp = {
        .tv_sec = s,
        .tv_nsec = ns
    };

    int res = clock_settime(CLOCK_REALTIME, &tp);
#else
    // Use settimeofday as a fallback
    struct timeval tv = {
        .tv_sec = s,
        .tv_usec = ns / 1000
    };

    int res = settimeofday(&tv, NULL);
#endif
    if (res != 0) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term error = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error, 0, ERROR_ATOM);
        term_put_tuple_element(error, 1, posix_errno_to_term(errno, ctx->global));
        return error;
    } else {
        return OK_ATOM;
    }
}
#endif

#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
struct PosixDir
{
    DIR *dir;
};

static void posix_dir_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct PosixDir *dir_obj = (struct PosixDir *) obj;
    if (dir_obj->dir) {
        closedir(dir_obj->dir);
        dir_obj->dir = NULL;
    }
}

const ErlNifResourceTypeInit posix_dir_resource_type_init = {
    .members = 1,
    .dtor = posix_dir_dtor
};

static term nif_atomvm_posix_opendir(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    term path_term = argv[0];

    int ok;
    char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term result;
    DIR *dir = opendir(path);
    free(path);

    if (IS_NULL_PTR(dir)) {
        return errno_to_error_tuple_maybe_gc(ctx);
    } else {
        // Return a resource object
        struct PosixDir *dir_obj
            = enif_alloc_resource(glb->posix_dir_resource_type, sizeof(struct PosixDir));
        if (IS_NULL_PTR(dir_obj)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        dir_obj->dir = dir;
        if (UNLIKELY(memory_ensure_free_opt(
                         ctx, TUPLE_SIZE(2) + TERM_BOXED_REFERENCE_RESOURCE_SIZE, MEMORY_CAN_SHRINK)
                != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term obj = term_from_resource(dir_obj, &ctx->heap);
        enif_release_resource(dir_obj); // decrement refcount after enif_alloc_resource
        result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, obj);
    }

    return result;
}

static term nif_atomvm_posix_closedir(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term result = OK_ATOM;

    void *dir_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0],
            ctx->global->posix_dir_resource_type, &dir_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixDir *dir_obj = (struct PosixDir *) dir_obj_ptr;
    if (dir_obj->dir != NULL) {
        if (UNLIKELY(closedir(dir_obj->dir) < 0)) {
            dir_obj->dir = NULL; // even if bad things happen, do not close twice.
            return errno_to_error_tuple_maybe_gc(ctx);
        }
        dir_obj->dir = NULL;
    }

    return result;
}

// This function main purpose is to avoid warnings, such as:
// warning: comparison is always true due to limited range of data type [-Wtype-limits]
static inline term to_boxed_safe(uint64_t value, Context *ctx)
{
    if (value <= INT64_MAX) {
        return term_make_maybe_boxed_int64(value, &ctx->heap);
    } else {
        return UNDEFINED_ATOM;
    }
}

static term nif_atomvm_posix_readdir(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    GlobalContext *glb = ctx->global;

    void *dir_obj_ptr;
    if (UNLIKELY(!enif_get_resource(
            erl_nif_env_from_context(ctx), argv[0], glb->posix_dir_resource_type, &dir_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct PosixDir *dir_obj = (struct PosixDir *) dir_obj_ptr;

    errno = 0;
    struct dirent *dir_result = readdir(dir_obj->dir);
    if (dir_result == NULL) {
        if (UNLIKELY(errno != 0)) {
            return errno_to_error_tuple_maybe_gc(ctx);
        }

        return globalcontext_make_atom(glb, ATOM_STR("\x3", "eof"));
    }

    size_t name_len = strlen(dir_result->d_name);
    if (UNLIKELY(
            memory_ensure_free_opt(ctx,
                BOXED_INT64_SIZE + term_binary_heap_size(name_len) + TUPLE_SIZE(3) + TUPLE_SIZE(2),
                MEMORY_CAN_SHRINK)
            != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term ino_no = to_boxed_safe(dir_result->d_ino, ctx);

    term name_term = term_create_uninitialized_binary(name_len, &ctx->heap, glb);
    memcpy((void *) term_binary_data(name_term), dir_result->d_name, name_len);

    term dirent_atom = globalcontext_make_atom(glb, ATOM_STR("\x6", "dirent"));

    // {dirent, Inode, Name}
    term dirent_term = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(dirent_term, 0, dirent_atom);
    term_put_tuple_element(dirent_term, 1, ino_no);
    term_put_tuple_element(dirent_term, 2, name_term);

    // {ok, DirentTuple}
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, dirent_term);

    return result;
}
#endif

#if defined(HAVE_GETCWD) && defined(HAVE_PATH_MAX)
static term nif_file_get_cwd_0(Context *ctx, int argc, term argv[])
{
    UNUSED(argc)
    UNUSED(argv)

    char *cwd = malloc(PATH_MAX);
    if (IS_NULL_PTR(cwd)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (IS_NULL_PTR(getcwd(cwd, PATH_MAX))) {
        free(cwd);
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term reason = posix_errno_to_term(errno, ctx->global);
        term_put_tuple_element(result_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(result_tuple, 1, reason);
        return result_tuple;
    }

    size_t cwd_length = strlen(cwd);
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TERM_STRING_SIZE(cwd_length)) != MEMORY_GC_OK)) {
        free(cwd);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term cwd_list = term_from_string((uint8_t *) cwd, cwd_length, &ctx->heap);
    free(cwd);

    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, cwd_list);
    return result_tuple;
}
#endif

#if HAVE_OPEN && HAVE_CLOSE
const struct Nif atomvm_posix_open_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_open
};
const struct Nif atomvm_posix_close_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_close
};
const struct Nif atomvm_posix_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_read
};
const struct Nif atomvm_posix_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_write
};
const struct Nif atomvm_posix_select_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_select_read
};
const struct Nif atomvm_posix_select_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_select_write
};
const struct Nif atomvm_posix_select_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_select_stop
};
#if HAVE_EXECVE
const struct Nif atomvm_subprocess_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_subprocess
};
#endif
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_LSEEK
const struct Nif atomvm_posix_seek_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_seek
};
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_PREAD
const struct Nif atomvm_posix_pread_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_pread
};
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_PWRITE
const struct Nif atomvm_posix_pwrite_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_pwrite
};
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_FSYNC
const struct Nif atomvm_posix_fsync_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_fsync
};
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_FTRUNCATE
const struct Nif atomvm_posix_ftruncate_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_ftruncate
};
#endif
#if HAVE_MKFIFO
const struct Nif atomvm_posix_mkfifo_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_mkfifo
};
#endif
#if HAVE_UNLINK
const struct Nif atomvm_posix_unlink_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_unlink
};
#endif
#if HAVE_RENAME
const struct Nif atomvm_posix_rename_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_rename
};
#endif
#if HAVE_STAT
const struct Nif atomvm_posix_stat_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_stat
};
#endif
#if HAVE_OPEN && HAVE_CLOSE && HAVE_FSTAT
const struct Nif atomvm_posix_fstat_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_fstat
};
#endif
#if defined(HAVE_CLOCK_SETTIME) || defined(HAVE_SETTIMEOFDAY)
const struct Nif atomvm_posix_clock_settime_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_clock_settime
};
#endif
#if HAVE_OPENDIR && HAVE_READDIR && HAVE_CLOSEDIR
const struct Nif atomvm_posix_opendir_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_opendir
};
const struct Nif atomvm_posix_closedir_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_closedir
};
const struct Nif atomvm_posix_readdir_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_posix_readdir
};
#endif
#if defined(HAVE_GETCWD) && defined(HAVE_PATH_MAX)
const struct Nif file_get_cwd_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_file_get_cwd_0
};
#endif
