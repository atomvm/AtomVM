/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Fred Dushin <fred@dushin.net>
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

#include <context.h>
#include <defaultatoms.h>
#include <dictionary.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <nifs.h>
#include <otp_socket.h>
#include <otp_socket_platform.h>
#include <port.h>
#include <posix_nifs.h>
#include <scheduler.h>
#include <sys.h>
#include <term.h>
#include <utils.h>

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#if HAVE_SIGNAL
#include <signal.h>
#endif

// #define ENABLE_TRACE
#include <trace.h>

struct SocketFd
{
    int fd;
    uint64_t ref_ticks;
    int32_t selecting_process_id;
    ErlNifMonitor selecting_process_monitor;
};

static const char *const addr_atom = ATOM_STR("\x4", "addr");
static const char *const any_atom = ATOM_STR("\x3", "any");
static const char *const linger_atom = ATOM_STR("\x6", "linger");
static const char *const loopback_atom = ATOM_STR("\x8", "loopback");
static const char *const onoff_atom = ATOM_STR("\x5", "onoff");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const reuseaddr_atom = ATOM_STR("\x9", "reuseaddr");

#define CLOSED_FD 0

#define ADDR_ATOM globalcontext_make_atom(global, addr_atom)
#define CLOSE_INTERNAL_ATOM globalcontext_make_atom(global, close_internal_atom)
#define ACCEPT_ATOM globalcontext_make_atom(global, accept_atom)
#define RECV_ATOM globalcontext_make_atom(global, recv_atom)

enum otp_socket_domain
{
    OtpSocketInvalidDomain = 0,
    OtpSocketInetDomain
};

static const AtomStringIntPair otp_socket_domain_table[] = {
    { ATOM_STR("\x4", "inet"), OtpSocketInetDomain },
    SELECT_INT_DEFAULT(OtpSocketInvalidDomain)
};

enum otp_socket_type
{
    OtpSocketInvalidType = 0,
    OtpSocketStreamType,
    OtpSocketDgramType
};

static const AtomStringIntPair otp_socket_type_table[] = {
    { ATOM_STR("\x6", "stream"), OtpSocketStreamType },
    { ATOM_STR("\x5", "dgram"), OtpSocketDgramType },
    SELECT_INT_DEFAULT(OtpSocketInvalidType)
};

enum otp_socket_protocol
{
    OtpSocketInvalidProtocol = 0,
    OtpSocketIpProtocol,
    OtpSocketTcpProtocol,
    OtpSocketUdpProtocol
};

static const AtomStringIntPair otp_socket_protocol_table[] = {
    { ATOM_STR("\x2", "ip"), OtpSocketIpProtocol },
    { ATOM_STR("\x3", "tcp"), OtpSocketTcpProtocol },
    { ATOM_STR("\x3", "udp"), OtpSocketUdpProtocol },
    SELECT_INT_DEFAULT(OtpSocketInvalidProtocol)
};

enum otp_socket_shutdown_direction
{
    OtpSocketInvalidShutdownDirection = 0,
    OtpSocketReadShutdownDirection,
    OtpSocketWriteShutdownDirection,
    OtpSocketReadWriteShutdownDirection
};

static const AtomStringIntPair otp_socket_shutdown_direction_table[] = {
    { ATOM_STR("\x4", "read"), OtpSocketReadShutdownDirection },
    { ATOM_STR("\x5", "write"), OtpSocketWriteShutdownDirection },
    { ATOM_STR("\xA", "read_write"), OtpSocketReadWriteShutdownDirection },
    SELECT_INT_DEFAULT(OtpSocketInvalidShutdownDirection)
};

#define DEFAULT_BUFFER_SIZE 512
#define MIN(A, B) (((A) < (B)) ? (A) : (B))

static ErlNifResourceType *socket_resource_type;

//
// resource operations
//

static void socket_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;
    if (fd_obj->fd != CLOSED_FD) {
        close(fd_obj->fd);
        fd_obj->fd = CLOSED_FD;
    }
}

static void socket_stop(ErlNifEnv *caller_env, void *obj, ErlNifEvent event, int is_direct_call)
{
    UNUSED(caller_env);
    UNUSED(event);
    UNUSED(is_direct_call);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;

    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        enif_demonitor_process(caller_env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }

    TRACE("socket_stop called on fd=%i\n", fd_obj->fd);
}

static void socket_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(caller_env);
    UNUSED(pid);
    UNUSED(mon);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;

    TRACE("socket_down called on process_id=%i fd=%i\n", *pid, fd_obj->fd);

    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        // Monitor fired, so make sure we don't try to demonitor in select_stop
        // as it could crash trying to reacquire lock on process table
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        enif_select(caller_env, fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil());
    }
}

static const ErlNifResourceTypeInit SocketResourceTypeInit = {
    .members = 3,
    .dtor = socket_dtor,
    .stop = socket_stop,
    .down = socket_down,
};

// register the socket_fd resource type
void otp_socket_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    socket_resource_type = enif_init_resource_type(&env, "socket_fd", &SocketResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
#if HAVE_SIGNAL
    // ignore pipe signals on shutdown
    signal(SIGPIPE, SIG_IGN);
#endif
}

//
// socket operations
//

static uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

static term socket_tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

static inline int get_domain(GlobalContext *global, term domain_term, bool *ok)
{
    *ok = true;

    int val = interop_atom_term_select_int(otp_socket_domain_table, domain_term, global);
    switch (val) {

        case OtpSocketInetDomain:
            return PF_INET;

        default:
            AVM_LOGW(TAG, "Unsupported domain.");
            *ok = false;
            return -1;
    }
}

static inline int get_type(GlobalContext *global, term type_term, bool *ok)
{
    *ok = true;

    int val = interop_atom_term_select_int(otp_socket_type_table, type_term, global);
    switch (val) {

        case OtpSocketStreamType:
            return SOCK_STREAM;

        case OtpSocketDgramType:
            return SOCK_DGRAM;

        default:
            AVM_LOGW(TAG, "Unsupported type.");
            *ok = false;
            return -1;
    }
}

static inline int get_protocol(GlobalContext *global, term protocol_term, bool *ok)
{
    *ok = true;

    int val = interop_atom_term_select_int(otp_socket_protocol_table, protocol_term, global);
    switch (val) {

        case OtpSocketIpProtocol:
            return IPPROTO_IP;

        case OtpSocketTcpProtocol:
            return IPPROTO_TCP;

        case OtpSocketUdpProtocol:
            return IPPROTO_UDP;

        default:
            AVM_LOGW(TAG, "Unsupported type.");
            *ok = false;
            return -1;
    }
}

static inline term make_error_tuple(term reason, Context *ctx)
{
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason);
    return error_tuple;
}

//
// open
//

static term nif_socket_open(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_open\n");

    UNUSED(argc);

    GlobalContext *global = ctx->global;

    bool ok = false;
    int domain = get_domain(global, argv[0], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int type = get_type(global, argv[1], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int protocol = get_protocol(global, argv[2], &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int fd = socket(domain, type, protocol);
    if (UNLIKELY(fd == -1 || fd == CLOSED_FD)) {

        AVM_LOGE(TAG, "Failed to initialize socket.");

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return make_error_tuple(posix_errno_to_term(errno, global), ctx);

    } else {

        struct SocketFd *fd_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketFd));

        if (IS_NULL_PTR(fd_obj)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        fd_obj->fd = fd;
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        TRACE("nif_socket_open: Created socket fd=%i\n", fd_obj->fd);

        if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term obj = enif_make_resource(erl_nif_env_from_context(ctx), fd_obj);
        enif_release_resource(fd_obj);

        size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE;
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term socket_term = term_alloc_tuple(2, &ctx->heap);
        uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
        term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
        term_put_tuple_element(socket_term, 0, obj);
        term_put_tuple_element(socket_term, 1, ref);

        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, socket_term);

        return result;
    }
}

static term get_socket(term socket_term)
{
    return term_get_tuple_element(socket_term, 0);
}

static bool term_is_socket(term socket_term)
{
    bool ret = term_is_tuple(socket_term)
        && term_get_tuple_arity(socket_term) == 2
        && term_is_binary(term_get_tuple_element(socket_term, 0))
        && term_is_reference(term_get_tuple_element(socket_term, 1));

    TRACE("term is a socket: %i\n", ret);

    return ret;
}

//
// close
//

static term nif_socket_close(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_close\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
            //
            // If we are in select, then stop selecting
            //
            if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil()) < 0)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            //
            // If there is a process (other than ourself) who is waiting on select
            // the send a {closed, Ref} message to it, so that it can break
            // out of its receive statement.
            //
            if (fd_obj->selecting_process_id != ctx->process_id) {

                // send a {closed, Ref | undefined} message to the pid
                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, CLOSED_ATOM);
                term ref = (fd_obj->ref_ticks == 0) ? UNDEFINED_ATOM : term_from_ref_ticks(fd_obj->ref_ticks, &ctx->heap);
                term_put_tuple_element(error_tuple, 1, ref);

                TRACE("nif_socket_close: Sending msg to process %i\n", fd_obj->selecting_process_id);
                globalcontext_send_message(ctx->global, fd_obj->selecting_process_id, error_tuple);
            } else {
                AVM_LOGW(TAG, "Selectable socket %i was closed but no known pid is waiting.  This shouldn't happen.", fd_obj->fd);
            }
        }

        int res = close(fd_obj->fd);
        if (UNLIKELY(res != 0)) {
            AVM_LOGW(TAG, "Failed to close socket %i", res);
        }

        TRACE("nif_socket_close: Clearing pid for socket fd=%i\n", fd_obj->fd);
        fd_obj->fd = CLOSED_FD;
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        fd_obj->ref_ticks = 0;
    } else {
        TRACE("Double close on socket fd %i", fd_obj->fd);
    }

    return OK_ATOM;
}

//
// select
//

static term nif_socket_select(Context *ctx, term argv[], enum ErlNifSelectFlags mode)
{
    TRACE("nif_socket_select\n");

    VALIDATE_VALUE(argv[0], term_is_socket);

    term process_pid_term = argv[1];
    VALIDATE_VALUE(process_pid_term, term_is_pid);
    int32_t process_pid = term_to_local_process_id(process_pid_term);
    TRACE("process_pid=%i\n", (int) process_pid);

    term select_ref_term = argv[2];
    if (select_ref_term != UNDEFINED_ATOM) {
        VALIDATE_VALUE(select_ref_term, term_is_reference);
    }
    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    TRACE("fd_obj->fd=%i\n", (int) fd_obj->fd);

    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (fd_obj->selecting_process_id != process_pid && fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        if (UNLIKELY(enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor) != 0)) {
            // RAISE_ERROR(posix_errno_to_term(EINVAL, global));
        }
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
    // Monitor first as select is less likely to fail and it's less expensive to demonitor
    // if select fails than to stop select if monitor fails
    if (fd_obj->selecting_process_id != process_pid) {
        if (UNLIKELY(enif_monitor_process(env, fd_obj, &process_pid, &fd_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(NOPROC_ATOM);
        }
        fd_obj->selecting_process_id = process_pid;
    }

    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, mode, fd_obj, &process_pid, select_ref_term) < 0)) {
        enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }

    TRACE("nif_socket_select: Setting pid for socket fd %i to %i\n", fd_obj->fd, process_pid);
    fd_obj->ref_ticks = (select_ref_term == UNDEFINED_ATOM) ? 0 : term_to_ref_ticks(select_ref_term);

    return OK_ATOM;
}

static term nif_socket_select_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return nif_socket_select(ctx, argv, ERL_NIF_SELECT_READ);
}

static term nif_socket_select_stop(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_stop\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil()) < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

//
// setopt
//

static term nif_socket_setopt(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_setopt\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        term level_tuple = argv[1];
        term value = argv[2];

        term opt = term_get_tuple_element(level_tuple, 1);
        if (globalcontext_is_term_equal_to_atom_string(global, opt, reuseaddr_atom)) {
            int option_value = (value == TRUE_ATOM);
            int res = setsockopt(fd_obj->fd, SOL_SOCKET, SO_REUSEADDR, &option_value, sizeof(int));
            if (UNLIKELY(res != 0)) {

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return make_error_tuple(posix_errno_to_term(errno, global), ctx);
            } else {
                return OK_ATOM;
            }
        } else if (globalcontext_is_term_equal_to_atom_string(global, opt, linger_atom)) {
            term onoff = interop_kv_get_value(value, onoff_atom, ctx->global);
            term linger = interop_kv_get_value(value, linger_atom, ctx->global);
            VALIDATE_VALUE(linger, term_is_integer);

            struct linger sl;
            sl.l_onoff = (onoff == TRUE_ATOM);
            sl.l_linger = term_to_int(linger);
            int res = setsockopt(fd_obj->fd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));
            if (UNLIKELY(res != 0)) {

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return make_error_tuple(posix_errno_to_term(errno, global), ctx);
            } else {
                return OK_ATOM;
            }
            // TODO add more as needed
            // int flag = 1;
            // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
            // if (UNLIKELY(res != 0)) {
            //     AVM_LOGW(TAG, "Failed to set TCP_NODELAY.");
            // }
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// sockname
//

static term nif_socket_sockname(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_sockname\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int res = getsockname(fd_obj->fd, (struct sockaddr *) &addr, &addrlen);

        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", fd_obj->fd, res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(posix_errno_to_term(errno, global), ctx);
        } else {
            // {ok, #{addr => {a,b,c,d}, port => integer()}}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            } else {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term return_value = port_create_tuple2(ctx, OK_ATOM, map);

                return return_value;
            }
        }
    } else {
        RAISE_ERROR(posix_errno_to_term(EBADF, ctx->global));
    }
}

//
// peername
//

static term nif_socket_peername(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_peername\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int res = getpeername(fd_obj->fd, (struct sockaddr *) &addr, &addrlen);

        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to getpeername: fd=%i res=%i.", fd_obj->fd, res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(posix_errno_to_term(errno, global), ctx);
        } else {
            // {ok, #{addr => {a,b,c,d}, port => integer()}}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            } else {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term return_value = port_create_tuple2(ctx, OK_ATOM, map);

                return return_value;
            }
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// bind
//

static term nif_socket_bind(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_bind\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        term sockaddr = argv[1];

        struct sockaddr_in serveraddr;
        memset(&serveraddr, 0, sizeof(serveraddr));
        serveraddr.sin_family = AF_INET;

        if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, any_atom)) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
        } else if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, loopback_atom)) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (term_is_map(sockaddr)) {
            term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
            serveraddr.sin_port = htons(term_to_int(port));
            term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
            if (globalcontext_is_term_equal_to_atom_string(global, addr, any_atom)) {
                serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
            } else if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
                serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            } else {
                serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
            }
        }

        socklen_t address_len = sizeof(serveraddr);
        int res = bind(fd_obj->fd, (struct sockaddr *) &serveraddr, address_len);
        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return make_error_tuple(posix_errno_to_term(errno, global), ctx);
        } else {
            return OK_ATOM;
        }

    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// listen
//

static term nif_socket_listen(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_listen\n");
    UNUSED(argc);

    GlobalContext *global = ctx->global;

    VALIDATE_VALUE(argv[0], term_is_socket);
    VALIDATE_VALUE(argv[1], term_is_integer);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        int backlog = term_to_int(argv[1]);
        int res = listen(fd_obj->fd, backlog);
        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(posix_errno_to_term(errno, global), ctx);
        } else {
            return OK_ATOM;
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// accept
//

static term nif_socket_accept(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_accept\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in clientaddr;
        socklen_t clientlen = sizeof(clientaddr);
        int fd = accept(fd_obj->fd, (struct sockaddr *) &clientaddr, &clientlen);
        if (UNLIKELY(fd == -1 || fd == CLOSED_FD)) {
            AVM_LOGE(TAG, "Unable to accept on socket %i.", fd_obj->fd);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            int err = errno;
            term reason = (err == ECONNABORTED) ? CLOSED_ATOM : posix_errno_to_term(err, global);
            return make_error_tuple(reason, ctx);

        } else {

            struct SocketFd *conn_fd_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketFd));
            conn_fd_obj->fd = fd;
            conn_fd_obj->selecting_process_id = INVALID_PROCESS_ID;
            TRACE("nif_socket_accept: Created socket on accept fd=%i\n", fd_obj->fd);

            term obj = enif_make_resource(erl_nif_env_from_context(ctx), conn_fd_obj);
            enif_release_resource(conn_fd_obj);

            size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE;
            if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term socket_term = term_alloc_tuple(2, &ctx->heap);
            uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
            term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
            term_put_tuple_element(socket_term, 0, obj);
            term_put_tuple_element(socket_term, 1, ref);

            term result = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result, 0, OK_ATOM);
            term_put_tuple_element(result, 1, socket_term);

            return result;
        }

    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// recv/recvfrom
//

static term nif_socket_recv_with_peek(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    TRACE("nif_socket_recv_with_peek\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        int flags = MSG_WAITALL;
        // TODO parameterize buffer size
        ssize_t res = recvfrom(fd_obj->fd, NULL, DEFAULT_BUFFER_SIZE, MSG_PEEK | flags, NULL, NULL);
        TRACE("%li bytes available.\n", (long int) res);
        if (res < 0) {
            AVM_LOGI(TAG, "Unable to receive data on fd %i.  errno=%i", fd_obj->fd, errno);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(posix_errno_to_term(errno, global), ctx);

        } else if (res == 0) {

            TRACE("Peer closed socket %i.\n", fd_obj->fd);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(CLOSED_ATOM, ctx);

        } else {

            size_t len = (size_t) res;
            ssize_t buffer_size = MIN(len, DEFAULT_BUFFER_SIZE);

            // {ok, Data :: binary()}}
            size_t ensure_packet_avail = term_binary_data_size_in_terms(buffer_size) + BINARY_HEADER_SIZE;
            size_t requested_size = ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);

            if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term data = term_create_uninitialized_binary(buffer_size, &ctx->heap, global);
            const char *buffer = term_binary_data(data);

            //
            // receive data on the socket
            //
            struct sockaddr_in addr;
            socklen_t addrlen = sizeof(addr);
            res = recvfrom(fd_obj->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

            TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", fd_obj->fd, (unsigned long) len);

            term payload = term_invalid_term();
            if (is_recvfrom) {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
                payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
            } else {
                payload = port_heap_create_ok_tuple(&ctx->heap, data);
            }

            return payload;
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

static term nif_socket_recv_without_peek(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    TRACE("nif_socket_recv_without_peek\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        // TODO plumb through buffer size
        size_t buffer_size = DEFAULT_BUFFER_SIZE;
        char *buffer = malloc(buffer_size);
        term payload = term_invalid_term();

        if (IS_NULL_PTR(buffer)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);

        } else {

            int flags = 0;
            struct sockaddr_in addr;
            socklen_t addrlen = sizeof(addr);
            ssize_t res = recvfrom(fd_obj->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

            if (res < 0) {

                int err = errno;
                term reason = (err == ECONNRESET) ? globalcontext_make_atom(global, ATOM_STR("\xA", "econnreset")) : posix_errno_to_term(err, global);

                if (err == ECONNRESET) {
                    TRACE("Peer closed connection.\n");
                } else {
                    AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", fd_obj->fd, errno);
                }

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                return make_error_tuple(reason, ctx);

            } else if (res == 0) {

                TRACE("Peer closed socket %i.\n", fd_obj->fd);

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                return make_error_tuple(CLOSED_ATOM, ctx);

            } else {

                size_t len = (size_t) res;
                TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", fd_obj->fd, (unsigned long) len);

                size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
                size_t requested_size = REF_SIZE + 2 * TUPLE_SIZE(2) + ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);

                if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term data = term_from_literal_binary(buffer, len, &ctx->heap, global);

                if (is_recvfrom) {
                    term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                    term port_number = term_from_int(ntohs(addr.sin_port));

                    term map = term_alloc_map(2, &ctx->heap);
                    term_set_map_assoc(map, 0, ADDR_ATOM, address);
                    term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                    term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
                    payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
                } else {
                    payload = port_heap_create_ok_tuple(&ctx->heap, data);
                }
            }

            free(buffer);
            return payload;
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

static term nif_socket_recv_internal(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    if (otp_socket_platform_supports_peek()) {
        return nif_socket_recv_with_peek(ctx, argc, argv, is_recvfrom);
    } else {
        return nif_socket_recv_without_peek(ctx, argc, argv, is_recvfrom);
    }
}

static term nif_socket_recv(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_recv\n");
    return nif_socket_recv_internal(ctx, argc, argv, false);
}

static term nif_socket_recvfrom(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_recvfrom\n");
    return nif_socket_recv_internal(ctx, argc, argv, true);
}

//
// send/sendto
//

static term nif_socket_send_internal(Context *ctx, int argc, term argv[], bool is_sendto)
{
    TRACE("nif_socket_send_internal\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);
    VALIDATE_VALUE(argv[1], term_is_binary);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        term data = argv[1];
        term dest = term_invalid_term();
        if (is_sendto) {
            dest = argv[2];
        }

        // TODO make non-blocking

        const char *buf = term_binary_data(data);
        size_t len = term_binary_size(data);

        ssize_t sent_data = -1;

        if (is_sendto) {

            struct sockaddr_in destaddr;
            memset(&destaddr, 0, sizeof(destaddr));
            destaddr.sin_family = AF_INET;

            term port = interop_kv_get_value_default(dest, port_atom, term_from_int(0), global);
            destaddr.sin_port = htons(term_to_int(port));
            term addr = interop_kv_get_value(dest, addr_atom, ctx->global);
            if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
                destaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            } else {
                destaddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
            }

            sent_data = sendto(fd_obj->fd, buf, len, 0, (struct sockaddr *) &destaddr, sizeof(destaddr));

        } else {
            sent_data = send(fd_obj->fd, buf, len, 0);
        }

        // {ok, RestData} | {error, Reason}

        if (sent_data > 0) {

            // assert sent_data < len
            size_t rest_len = len - sent_data;
            if (rest_len == 0) {
                return OK_ATOM;
            }

            size_t requested_size = term_sub_binary_heap_size(data, rest_len);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + requested_size) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term rest = term_maybe_create_sub_binary(data, sent_data, rest_len, &ctx->heap, ctx->global);
            return port_create_tuple2(ctx, OK_ATOM, rest);

        } else {

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            TRACE("Unable to send data: res=%zi.\n", sent_data);
            return make_error_tuple(CLOSED_ATOM, ctx);
        }

    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

static term nif_socket_send(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_send\n");
    return nif_socket_send_internal(ctx, argc, argv, false);
}

static term nif_socket_sendto(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_sendto\n");
    return nif_socket_send_internal(ctx, argc, argv, true);
}

//
// connect
//

static term nif_socket_connect(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_connect\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);
    VALIDATE_VALUE(argv[1], term_is_map);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        term sockaddr = argv[1];
        struct sockaddr_in address;
        memset(&address, 0, sizeof(struct sockaddr_in));
        address.sin_family = AF_INET;

        term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
        address.sin_port = htons(term_to_int(port));

        term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
        if (term_is_invalid_term(addr)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
            address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            // TODO more validation on addr tuple
            address.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
        }

        socklen_t addr_len = sizeof(struct sockaddr_in);
        int res = connect(fd_obj->fd, (const struct sockaddr *) &address, addr_len);
        if (res == -1) {
            if (errno == EINPROGRESS) {

                // TODO make connect non-blocking
                return UNDEFINED_ATOM;

            } else {
                AVM_LOGE(TAG, "Unable to connect: res=%i errno=%i", res, errno);

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                return make_error_tuple(CLOSED_ATOM, ctx);
            }
        } else if (res == 0) {
            return OK_ATOM;
        } else {
            // won't happen according to connect(2)
            return UNDEFINED_ATOM;
        }
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// shutdown
//

static term nif_socket_shutdown(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_shutdown\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);
    VALIDATE_VALUE(argv[1], term_is_atom);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        int how;

        int val = interop_atom_term_select_int(otp_socket_shutdown_direction_table, argv[1], global);
        switch (val) {

            case OtpSocketReadShutdownDirection:
                how = SHUT_RD;
                break;

            case OtpSocketWriteShutdownDirection:
                how = SHUT_WR;
                break;

            case OtpSocketReadWriteShutdownDirection:
                how = SHUT_RDWR;
                break;

            default:
                RAISE_ERROR(BADARG_ATOM);
        }

        int res = shutdown(fd_obj->fd, how);
        if (res < 0) {
            AVM_LOGE(TAG, "Unable to shut down socket: res=%i errno=%i", res, errno);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return make_error_tuple(posix_errno_to_term(errno, global), ctx);
        }
        return OK_ATOM;
    } else {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
}

//
// Nifs
//

static const struct Nif socket_open_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_open
};
static const struct Nif socket_close_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_close
};
static const struct Nif socket_select_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_select_stop
};
static const struct Nif socket_setopt_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_setopt
};
static const struct Nif socket_bind_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_bind
};
static const struct Nif socket_listen_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_listen
};
static const struct Nif socket_sockname_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_sockname
};
static const struct Nif socket_peername_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_peername
};
static const struct Nif socket_select_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_select_read
};
static const struct Nif socket_accept_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_accept
};
static const struct Nif socket_recv_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_recv
};
static const struct Nif socket_recvfrom_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_recvfrom
};
static const struct Nif socket_send_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_send
};
static const struct Nif socket_sendto_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_sendto
};
static const struct Nif socket_connect_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_connect
};
static const struct Nif socket_shutdown_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_shutdown
};

const struct Nif *otp_socket_nif_get_nif(const char *nifname)
{
    if (strncmp("socket:", nifname, 7) == 0) {
        const char *rest = nifname + 7;

        if (strcmp("open/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_open_nif;
        }
        if (strcmp("close/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_close_nif;
        }
        if (strcmp("nif_select_stop/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_select_stop_nif;
        }
        if (strcmp("setopt/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_setopt_nif;
        }
        if (strcmp("bind/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_bind_nif;
        }
        if (strcmp("listen/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_listen_nif;
        }
        if (strcmp("sockname/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_sockname_nif;
        }
        if (strcmp("peername/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_peername_nif;
        }
        if (strcmp("nif_select_read/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_select_read_nif;
        }
        if (strcmp("nif_accept/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_accept_nif;
        }
        if (strcmp("nif_recv/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_recv_nif;
        }
        if (strcmp("nif_recvfrom/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_recvfrom_nif;
        }
        if (strcmp("nif_send/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_send_nif;
        }
        if (strcmp("nif_sendto/3", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_sendto_nif;
        }
        if (strcmp("connect/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_connect_nif;
        }
        if (strcmp("shutdown/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_shutdown_nif;
        }
    }
    return NULL;
}
