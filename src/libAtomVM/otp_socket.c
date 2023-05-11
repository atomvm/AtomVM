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
#include <globalcontext.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <otp_socket.h>
#include <otp_socket_platform.h>
#include <port.h>
#include <scheduler.h>
#include <sys.h>
#include <term.h>
#include <utils.h>

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

// #define ENABLE_TRACE
#include <trace.h>

static const char *const accept_atom = ATOM_STR("\x6", "accept");
static const char *const addr_atom = ATOM_STR("\x4", "addr");
static const char *const any_atom = ATOM_STR("\x3", "any");
static const char *const close_internal_atom = ATOM_STR("\x14", "$atomvm_socket_close");
static const char *const closed_atom = ATOM_STR("\x6", "closed");
static const char *const dgram_atom = ATOM_STR("\x5", "dgram");
static const char *const domain_atom = ATOM_STR("\x6", "domain");
static const char *const inet_atom = ATOM_STR("\x4", "inet");
static const char *const ip_atom = ATOM_STR("\x2", "ip");
static const char *const linger_atom = ATOM_STR("\x6", "linger");
static const char *const loopback_atom = ATOM_STR("\x8", "loopback");
static const char *const onoff_atom = ATOM_STR("\x5", "onoff");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const protocol_atom = ATOM_STR("\x8", "protocol");
static const char *const recv_atom = ATOM_STR("\x4", "recv");
static const char *const reuseaddr_atom = ATOM_STR("\x9", "reuseaddr");
static const char *const stream_atom = ATOM_STR("\x6", "stream");
static const char *const tcp_atom = ATOM_STR("\x3", "tcp");
static const char *const type_atom = ATOM_STR("\x4", "type");
static const char *const udp_atom = ATOM_STR("\x3", "udp");

#define ADDR_ATOM globalcontext_make_atom(global, addr_atom)
#define CLOSE_INTERNAL_ATOM globalcontext_make_atom(global, close_internal_atom)
#define ACCEPT_ATOM globalcontext_make_atom(global, accept_atom)
#define RECV_ATOM globalcontext_make_atom(global, recv_atom)

#define DEFAULT_BUFFER_SIZE 512
#define MIN(A, B) (((A) < (B)) ? (A) : (B))

enum SocketState
{
    idle,
    waiting_accept,
    waiting_recv,
    waiting_connect
};

typedef struct AsyncEventListener
{
    EventListener base;
    int32_t process_id;
    term pid;
    uint64_t ref_ticks;
    int fd;
    size_t buffer_size;
    bool is_recvfrom;
} AsyncEventListener;

struct SocketData
{
    enum SocketState socket_state;
    AsyncEventListener *async_listener;
    int fd;
    size_t buffer_size;
};

enum otp_socket_cmd
{
    OTPSocketInvalidCmd = 0,

    OTPSocketInitCmd,
    OTPSocketSockNameCmd,
    OTPSocketPeerNameCmd,
    OTPSocketBindCmd,
    OTPSocketSetOptCmd,
    OTPSocketListenCmd,
    OTPSocketAcceptCmd,
    OTPSocketSendCmd,
    OTPSocketSendToCmd,
    OTPSocketRecvCmd,
    OTPSocketRecvFromCmd,
    OTPSocketConnectCmd,
    OTPSocketCloseCmd,
    OTPSocketInternalCloseCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "init"), OTPSocketInitCmd },
    { ATOM_STR("\x8", "sockname"), OTPSocketSockNameCmd },
    { ATOM_STR("\x8", "peername"), OTPSocketPeerNameCmd },
    { ATOM_STR("\x6", "setopt"), OTPSocketSetOptCmd },
    { ATOM_STR("\x4", "bind"), OTPSocketBindCmd },
    { ATOM_STR("\x6", "listen"), OTPSocketListenCmd },
    { ATOM_STR("\x6", "accept"), OTPSocketAcceptCmd },
    { ATOM_STR("\x4", "send"), OTPSocketSendCmd },
    { ATOM_STR("\x6", "sendto"), OTPSocketSendToCmd },
    { ATOM_STR("\x4", "recv"), OTPSocketRecvCmd },
    { ATOM_STR("\x8", "recvfrom"), OTPSocketRecvFromCmd },
    { ATOM_STR("\x7", "connect"), OTPSocketConnectCmd },
    { ATOM_STR("\x5", "close"), OTPSocketCloseCmd },
    { ATOM_STR("\x14", "$atomvm_socket_close"), OTPSocketInternalCloseCmd },
    SELECT_INT_DEFAULT(OTPSocketInvalidCmd)
};

static NativeHandlerResult consume_mailbox(Context *ctx);

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

static term map_errno(int err, GlobalContext *global)
{
    // ref https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html
    switch (err) {
        // open
        case EACCES:
            return globalcontext_make_atom(global, ATOM_STR("\x7", "eaccess"));
        case EAFNOSUPPORT:
            return globalcontext_make_atom(global, ATOM_STR("\xC", "eafnosupport"));
        case EMFILE:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "emfile"));
        case ENFILE:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "enfile"));
        case ENOBUFS:
            return globalcontext_make_atom(global, ATOM_STR("\x7", "enobufs"));
        case ENOMEM:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "enomem"));
        case EPROTONOSUPPORT:
            return globalcontext_make_atom(global, ATOM_STR("\xF", "eprotonosupport"));
        case EPROTOTYPE:
            return globalcontext_make_atom(global, ATOM_STR("\xA", "eprototype"));

        // bind
        case EADDRINUSE:
            return globalcontext_make_atom(global, ATOM_STR("\xA", "eaddrinuse"));
        case EADDRNOTAVAIL:
            return globalcontext_make_atom(global, ATOM_STR("\xD", "eaddrnotavail"));
        case EBADF:
            return globalcontext_make_atom(global, ATOM_STR("\x5", "ebadf"));
        case EDESTADDRREQ:
            return globalcontext_make_atom(global, ATOM_STR("\xC", "edestaddrreq"));
        case EFAULT:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "efault"));
        case EINVAL:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "einval"));
        case ENOTSOCK:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "enotsock"));
        case EOPNOTSUPP:
            return globalcontext_make_atom(global, ATOM_STR("\xA", "eopnotsupp"));

        // accept
        case ECONNABORTED:
            return globalcontext_make_atom(global, ATOM_STR("\xC", "econnaborted"));
        case EINTR:
            return globalcontext_make_atom(global, ATOM_STR("\x5", "eintr"));

        // send
        case EAGAIN:
            return globalcontext_make_atom(global, ATOM_STR("\x6", "eagain"));
        case ECONNRESET:
            return globalcontext_make_atom(global, ATOM_STR("\xA", "econnreset"));
        case EHOSTUNREACH:
            return globalcontext_make_atom(global, ATOM_STR("\xC", "ehostunreach"));
        case EMSGSIZE:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "emsgsize"));
        case ENETDOWN:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "enetdown"));
        case ENETUNREACH:
            return globalcontext_make_atom(global, ATOM_STR("\xB", "enetunreach"));
        case EPIPE:
            return globalcontext_make_atom(global, ATOM_STR("\x5", "epipe"));

        // recv
        case ENOTCONN:
            return globalcontext_make_atom(global, ATOM_STR("\x8", "enotconn"));
        case ETIMEDOUT:
            return globalcontext_make_atom(global, ATOM_STR("\x9", "etimedout"));

        // connect
        case ECONNREFUSED:
            return globalcontext_make_atom(global, ATOM_STR("\xC", "econnrefused"));
        case EINPROGRESS:
            return globalcontext_make_atom(global, ATOM_STR("\xB", "einprogress"));
        case EISCONN:
            return globalcontext_make_atom(global, ATOM_STR("\x7", "eisconn"));

        // close
        case EIO:
            return globalcontext_make_atom(global, ATOM_STR("\x3", "eio"));

        // TODO All posix error codes?
        default:
            return term_from_int(err);
    }
}

static void sync_send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);

    Context *target = globalcontext_get_process_lock(global, local_process_id);
    mailbox_send(target, message);
    globalcontext_get_process_unlock(global, target);
}

static void sync_send_reply(Context *ctx, term pid, uint64_t ref_ticks, term return_value)
{
    // Pid ! {Ref :: reference(), ReturnValue :: term()}
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) + REF_SIZE, 1, &return_value, MEMORY_NO_SHRINK) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        sync_send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    } else {
        term return_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(return_tuple, 0, term_from_ref_ticks(ref_ticks, &ctx->heap));
        term_put_tuple_element(return_tuple, 1, return_value);
        sync_send_message(pid, return_tuple, ctx->global);
    }
}

static void sync_send_error_tuple(Context *ctx, term pid, uint64_t ref_ticks, term reason)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &reason, MEMORY_NO_SHRINK) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        sync_send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    } else {
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, reason);

        sync_send_reply(ctx, pid, ref_ticks, error_tuple);
    }
}

//
// init
//

static Context *create_context(GlobalContext *global)
{
    struct SocketData *socket_data = malloc(sizeof(struct SocketData));
    if (IS_NULL_PTR(socket_data)) {
        AVM_LOGE(TAG, "Insufficient memory to allocate SocketData.");
        return NULL;
    }
    socket_data->fd = 0;
    socket_data->buffer_size = DEFAULT_BUFFER_SIZE;
    socket_data->socket_state = idle;
    socket_data->async_listener = NULL;

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        AVM_LOGE(TAG, "Insufficient memory to allocate Context.");
        free(socket_data);
        return NULL;
    }
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = socket_data;
    TRACE("Created Context 0x%p with process_id=%i\n", (void *) ctx, ctx->process_id);

    return ctx;
}

static inline int get_domain(GlobalContext *global, term domain_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, domain_term, inet_atom)) {
        return PF_INET;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported domain.  Defaulting to inet.");
        return PF_INET;
    }
}

static inline int get_type(GlobalContext *global, term type_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, type_term, stream_atom)) {
        return SOCK_STREAM;
    } else if (globalcontext_is_term_equal_to_atom_string(global, type_term, dgram_atom)) {
        return SOCK_DGRAM;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported type.  Defaulting to stream.");
        return SOCK_STREAM;
    }
}

static inline int get_protocol(GlobalContext *global, term protocol_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, ip_atom)) {
        return IPPROTO_IP;
    } else if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, tcp_atom)) {
        return IPPROTO_TCP;
    } else if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, udp_atom)) {
        return IPPROTO_UDP;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported protocol.  Defaulting to ip.");
        return IPPROTO_IP;
    }
}

static void init_socket_context(Context *ctx, int fd)
{
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    socket_data->fd = fd;
    otp_socket_platform_notify_add(ctx, fd);

    TRACE("Context 0x%p with process_id=%i initialized with fd=%i\n", (void *) ctx, ctx->process_id, fd);
}

static bool do_init(Context *ctx, term msg, term cmd)
{
    GlobalContext *global = ctx->global;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    // {Ref :: reference, ok | {error, badarg}}
    // NB overshoot in case of failure case
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        sync_send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }

    int arity = term_get_tuple_arity(cmd);
    if (arity != 2) {
        sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
        return false;
    }
    term map = term_get_tuple_element(cmd, 1);
    if (!term_is_map(map)) {
        sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
        return false;
    }

    term domain_term = interop_kv_get_value(map, domain_atom, global);
    if (term_is_invalid_term(domain_term)) {
        domain_term = globalcontext_make_atom(global, inet_atom);
    }
    term type_term = interop_kv_get_value(map, type_atom, global);
    if (term_is_invalid_term(type_term)) {
        type_term = globalcontext_make_atom(global, stream_atom);
    }
    term protocol_term = interop_kv_get_value(map, protocol_atom, global);
    if (term_is_invalid_term(protocol_term)) {
        protocol_term = globalcontext_make_atom(global, tcp_atom);
    }

    int fd = socket(get_domain(global, domain_term), get_type(global, type_term), get_protocol(global, protocol_term));
    if (fd == -1) {
        AVM_LOGE(TAG, "Failed to initialize socket.");
        sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
        return false;
    }

    init_socket_context(ctx, fd);

    sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);

    return true;
}

//
// close
//

static void close_socket(Context *ctx)
{
    struct SocketData *socket_data = ctx->platform_data;
    GlobalContext *global = ctx->global;

    otp_socket_platform_notify_remove(ctx, socket_data->fd);

    TRACE("close_socket: fd=%i\n", socket_data->fd);
    int res = close(socket_data->fd);
    if (UNLIKELY(res != 0)) {
        AVM_LOGW(TAG, "Unable to close socket.");
    }

    if (socket_data->async_listener) {
        sys_unregister_listener(ctx->global, &socket_data->async_listener->base);
        TRACE("EventListener 0x%p with sender=0x%p process_id=%i fd=%i removed from platform listeners\n", (void *) socket_data->async_listener, (void *) ctx, ctx->process_id, socket_data->fd);

        term pid = socket_data->async_listener->pid;
        uint64_t ref_ticks = socket_data->async_listener->ref_ticks;
        free(socket_data->async_listener);
        socket_data->async_listener = NULL;

        // {Ref :: reference(), {error, closed}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            sync_send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            TRACE("Notifying pid %i that socket has closed\n", term_to_local_process_id(pid));
            sync_send_error_tuple(ctx, pid, ref_ticks, CLOSED_ATOM);
        }
    } else {
        TRACE("No EventListener on close with sender=0x%p process_id=%i fd=%i\n", (void *) ctx, ctx->process_id, socket_data->fd);
    }
}

static void do_close(Context *ctx, term msg)
{
    TRACE("do_close\n");

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);

    close_socket(ctx);
}

//
// sockname
//

static void do_sockname(Context *ctx, term msg)
{
    TRACE("do_sockname\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getsockname(socket_data->fd, (struct sockaddr *) &addr, &addrlen);

    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", socket_data->fd, res);
        sync_send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        // {Ref :: reference(), {ok, #{addr => {a,b,c,d}, port => integer()}}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            sync_send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(2, &ctx->heap);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            sync_send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

//
// peername
//

static void do_peername(Context *ctx, term msg)
{
    TRACE("do_peername\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getpeername(socket_data->fd, (struct sockaddr *) &addr, &addrlen);

    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to getpeername: fd=%i res=%i.", socket_data->fd, res);
        sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
    } else {
        // {Ref :: reference(), {ok, #{addr => {a,b,c,d}, port => integer()}}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            sync_send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(2, &ctx->heap);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            sync_send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

//
// setopt
//

static void do_setopt(Context *ctx, term msg, term cmd)
{
    TRACE("do_setopt\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);
    term level_tuple = term_get_tuple_element(cmd, 1);
    term value = term_get_tuple_element(cmd, 2);

    term opt = term_get_tuple_element(level_tuple, 1);
    if (globalcontext_is_term_equal_to_atom_string(global, opt, reuseaddr_atom)) {
        int option_value = (value == TRUE_ATOM) ? 1 : 0;
        int res = setsockopt(socket_data->fd, SOL_SOCKET, SO_REUSEADDR, &option_value, sizeof(int));
        if (UNLIKELY(res != 0)) {
            sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
        } else {
            sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);
        }
    } else if (globalcontext_is_term_equal_to_atom_string(global, opt, linger_atom)) {
        term onoff = interop_kv_get_value(value, onoff_atom, ctx->global);
        term linger = interop_kv_get_value(value, linger_atom, ctx->global);
        struct linger sl;
        sl.l_onoff = (onoff == TRUE_ATOM) ? 1 : 0;
        sl.l_linger = term_to_int(linger);
        int res = setsockopt(socket_data->fd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));
        if (UNLIKELY(res != 0)) {
            sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
        } else {
            sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);
        }
        // TODO add more as needed
        // int flag = 1;
        // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
        // if (UNLIKELY(res != 0)) {
        //     AVM_LOGW(TAG, "Failed to set TCP_NODELAY.");
        // }
    } else {
        sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
    }
}

//
// bind
//

static void do_bind(Context *ctx, term msg, term cmd)
{
    TRACE("do_bind\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term sockaddr = term_get_tuple_element(cmd, 1);

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
    int res = bind(socket_data->fd, (struct sockaddr *) &serveraddr, address_len);
    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
        sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
    } else {
        sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

//
// listen
//

static void do_listen(Context *ctx, term msg, term cmd)
{
    TRACE("do_listen\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term backlog = term_get_tuple_element(cmd, 1);

    int res = listen(socket_data->fd, term_to_int(backlog));
    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
        sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
    } else {
        sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

//
// accept
//

static EventListener *accept_handler(struct GlobalContext *global, EventListener *base_listener)
{
    TRACE("accept_handler\n");

    AsyncEventListener *listener = GET_LIST_ENTRY(base_listener, AsyncEventListener, base);
    Context *ctx = globalcontext_get_process_lock(global, listener->process_id);

    if (IS_NULL_PTR(ctx)) {
        AVM_LOGW(TAG, "Unable to locate process %i when socket was accepted.  Listening socket was likely closed.", listener->process_id);
        free(listener);
        return NULL;
    }
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(listener->fd, (struct sockaddr *) &clientaddr, &clientlen);
    if (fd == -1) {
        AVM_LOGE(TAG, "Unable to accept on socket %i.", listener->fd);

        // {Ref :: reference() , {error, {accept, ErrNo :: integer()}}}
        BEGIN_WITH_STACK_HEAP(REF_SIZE + 3 * TUPLE_SIZE(2), heap);
        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
        term reply = port_heap_create_reply(&heap, ref, port_heap_create_sys_error_tuple(&heap, ACCEPT_ATOM, errno));
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);

    } else {

        TRACE("otp_socket.accept_handler: accepted connection.  fd: %i\n", fd);

        globalcontext_get_process_unlock(global, ctx);
        Context *new_ctx = create_context(global);
        init_socket_context(new_ctx, fd);
        ctx = globalcontext_get_process_lock(global, listener->process_id);

        // {Ref :: reference(), {ok, Socket :: pid()}}
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2), heap);

        term msg = term_alloc_tuple(2, &heap);
        term new_pid = term_from_local_process_id(new_ctx->process_id);
        term_put_tuple_element(msg, 0, OK_ATOM);
        term_put_tuple_element(msg, 1, new_pid);

        term reply = term_alloc_tuple(2, &heap);
        term_put_tuple_element(reply, 0, term_from_ref_ticks(listener->ref_ticks, &heap));
        term_put_tuple_element(reply, 1, msg);

        port_send_message_nolock(global, listener->pid, reply);

        END_WITH_STACK_HEAP(heap, global);
    }

    socket_data->socket_state = idle;
    globalcontext_get_process_unlock(global, ctx);

    socket_data->async_listener = NULL;
    free(listener);
    TRACE("Free'd listener 0x%p\n", (void *) listener);

    return NULL;
}

static void do_accept(Context *ctx, term msg)
{
    TRACE("do_accept\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    //
    // Create an event listener with request-specific data, and append to the global list
    //
    AsyncEventListener *listener = malloc(sizeof(AsyncEventListener));
    if (IS_NULL_PTR(listener)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        // {Ref :: reference, out_of_memory}
        BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2), heap);
        term reply = port_heap_create_reply(&heap, ref, OUT_OF_MEMORY_ATOM);
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);
    }

    struct EventListenerKey key = {
        .sender = (void *) ctx,
        .fd = socket_data->fd
    };
    otp_socket_platform_set_listener((void *) &listener->base, &key);
    listener->base.handler = accept_handler;
    listener->process_id = ctx->process_id;
    listener->pid = pid;
    listener->ref_ticks = ref_ticks;
    listener->fd = socket_data->fd;
    listener->buffer_size = 0;
    listener->is_recvfrom = false;

    sys_register_listener(global, &listener->base);

    socket_data->socket_state = waiting_accept;
    socket_data->async_listener = listener;

    TRACE("EventListener 0x%p with sender=0x%p process_id=%i fd=%i created\n", (void *) listener, (void *) ctx, ctx->process_id, socket_data->fd);
}

//
// recv/recvfrom
//

static EventListener *recv_handler_with_peek(GlobalContext *global, EventListener *base_listener)
{
    TRACE("recv_handler_with_peek\n");

    AsyncEventListener *listener = GET_LIST_ENTRY(base_listener, AsyncEventListener, base);
    Context *ctx = globalcontext_get_process_lock(global, listener->process_id);

    if (IS_NULL_PTR(ctx)) {
        AVM_LOGW(TAG, "Unable to locate process by id %i when data was received with peek.  fd=%i  Socket was likely closed.", listener->process_id, listener->fd);
        free(listener);
        return NULL;
    }

    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    int flags = MSG_WAITALL;
    ssize_t res = recvfrom(listener->fd, NULL, listener->buffer_size, MSG_PEEK | flags, NULL, NULL);
    TRACE("%li bytes available.\n", (long int) res);
    if (res < 0) {
        AVM_LOGI(TAG, "Unable to receive data on fd %i.  errno=%i", listener->fd, errno);

        // {Ref :: reference(), {error, {recv, Errno :: integer()}}}
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + 2 * TUPLE_SIZE(2), heap);
        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
        term reply = port_heap_create_reply(&heap, ref, port_heap_create_sys_error_tuple(&heap, RECV_ATOM, errno));
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);

        mailbox_send(ctx, CLOSE_INTERNAL_ATOM);

    } else if (res == 0) {

        AVM_LOGI(TAG, "Peer closed socket %i.", listener->fd);

        // {Ref :: reference, {error, closed}}
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2), heap);
        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
        term reply = port_heap_create_reply(&heap, ref, port_heap_create_error_tuple(&heap, globalcontext_make_atom(global, closed_atom)));
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);

        mailbox_send(ctx, CLOSE_INTERNAL_ATOM);

    } else {

        size_t len = (size_t) res;
        ssize_t buffer_size = MIN(len, listener->buffer_size);

        // {Ref :: reference(), {ok, Data :: binary()}}
        size_t ensure_packet_avail = term_binary_data_size_in_terms(buffer_size) + BINARY_HEADER_SIZE;
        size_t requested_size = REF_SIZE + 2 * TUPLE_SIZE(2) + ensure_packet_avail + (listener->is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);
        Heap heap;
        if (UNLIKELY(memory_init_heap(&heap, requested_size) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);

            // {Ref :: reference, out_of_memory}
            BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2), heap);
            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term reply = port_heap_create_reply(&heap, ref, OUT_OF_MEMORY_ATOM);
            port_send_message_nolock(global, pid, reply);
            END_WITH_STACK_HEAP(heap, global);

            mailbox_send(ctx, CLOSE_INTERNAL_ATOM);
        }
        term data = term_create_uninitialized_binary(buffer_size, &heap, global);
        const char *buffer = term_binary_data(data);

        //
        // receive data on the socket
        //
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        res = recvfrom(listener->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

        TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", listener->fd, (unsigned long) len);

        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);

        term payload = term_invalid_term();
        if (listener->is_recvfrom) {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(2, &ctx->heap);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term tuple = port_heap_create_tuple2(&heap, map, data);
            payload = port_heap_create_ok_tuple(&heap, tuple);
        } else {
            payload = port_heap_create_ok_tuple(&heap, data);
        }

        term reply = port_heap_create_reply(&heap, ref, payload);
        port_send_message_nolock(global, pid, reply);
        memory_destroy_heap(&heap, global);
    }

    //
    // remove the EventListener from the global list and clean up
    //
    socket_data->socket_state = idle;
    globalcontext_get_process_unlock(global, ctx);
    socket_data->async_listener = NULL;

    free(listener);

    return NULL;
}

static EventListener *recv_handler_without_peek(GlobalContext *global, EventListener *base_listener)
{
    TRACE("recv_handler_without_peek\n");

    AsyncEventListener *listener = GET_LIST_ENTRY(base_listener, AsyncEventListener, base);
    Context *ctx = globalcontext_get_process_lock(global, listener->process_id);

    if (IS_NULL_PTR(ctx)) {
        AVM_LOGW(TAG, "Unable to locate process by id %i when data was received without peek.  fd=%i  Socket was likely closed.", listener->process_id, listener->fd);
        free(listener);
        return NULL;
    }

    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    size_t buffer_size = listener->buffer_size;
    char *buffer = malloc(buffer_size);

    if (IS_NULL_PTR(buffer)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);

        // {Ref :: reference, out_of_memory}
        BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2), heap);
        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
        term reply = port_heap_create_reply(&heap, ref, OUT_OF_MEMORY_ATOM);
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);

        mailbox_send(ctx, CLOSE_INTERNAL_ATOM);

    } else {

        int flags = 0;
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        ssize_t res = recvfrom(listener->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

        if (res < 0) {

            if (errno == ECONNRESET) {
                AVM_LOGI(TAG, "Peer closed connection.");
            } else {
                AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", listener->fd, errno);
            }

            // {Ref :: reference(), {error, {recv, Errno :: integer()}}}
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + 2 * TUPLE_SIZE(2), heap);
            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term reply = port_heap_create_reply(&heap, ref, port_heap_create_sys_error_tuple(&heap, RECV_ATOM, errno));
            port_send_message_nolock(global, pid, reply);
            END_WITH_STACK_HEAP(heap, global);

            mailbox_send(ctx, CLOSE_INTERNAL_ATOM);

        } else if (res == 0) {

            TRACE("Peer closed socket %i.", listener->fd);
            // {Ref :: reference, {error, closed}}
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2), heap);
            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term reply = port_heap_create_reply(&heap, ref, port_heap_create_error_tuple(&heap, globalcontext_make_atom(global, closed_atom)));
            port_send_message_nolock(global, pid, reply);
            END_WITH_STACK_HEAP(heap, global);

            mailbox_send(ctx, CLOSE_INTERNAL_ATOM);

        } else {

            size_t len = (size_t) res;
            TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", listener->fd, (unsigned long) len);

            size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
            size_t requested_size = REF_SIZE + 2 * TUPLE_SIZE(2) + ensure_packet_avail + (listener->is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);
            Heap heap;
            // {Ref :: reference(), {ok, Data | {Source, Data}}}
            if (UNLIKELY(memory_init_heap(&heap, requested_size) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);

                // {Ref :: reference, out_of_memory}
                BEGIN_WITH_STACK_HEAP(REF_SIZE + TUPLE_SIZE(2), heap);
                term pid = listener->pid;
                term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
                term reply = port_heap_create_reply(&heap, ref, OUT_OF_MEMORY_ATOM);
                port_send_message_nolock(global, pid, reply);
                END_WITH_STACK_HEAP(heap, global);

                mailbox_send(ctx, CLOSE_INTERNAL_ATOM);
            }

            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term data = term_from_literal_binary(buffer, len, &heap, global);

            term payload = term_invalid_term();
            if (listener->is_recvfrom) {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term tuple = port_heap_create_tuple2(&heap, map, data);
                payload = port_heap_create_ok_tuple(&heap, tuple);
            } else {
                payload = port_heap_create_ok_tuple(&heap, data);
            }

            term reply = port_heap_create_reply(&heap, ref, payload);
            port_send_message_nolock(global, pid, reply);
            memory_destroy_heap(&heap, global);
        }

        free(buffer);
    }

    socket_data->socket_state = idle;
    globalcontext_get_process_unlock(global, ctx);
    socket_data->async_listener = NULL;

    TRACE("recv_handler_without_peek: EventListener 0x%p with sender=0x%p process_id=%i fd=%i free'd and removed from listeners\n", (void *) listener, (void *) ctx, ctx->process_id, socket_data->fd);

    free(listener);

    return NULL;
}

static EventListener *recv_handler(GlobalContext *global, EventListener *base_listener)
{
    if (otp_socket_platform_supports_peek()) {
        return recv_handler_with_peek(global, base_listener);
    } else {
        return recv_handler_without_peek(global, base_listener);
    }
}

static void do_recv_internal(Context *ctx, term msg, bool is_recvfrom)
{
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    AsyncEventListener *listener = malloc(sizeof(AsyncEventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory for listener in recv: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    struct EventListenerKey key = {
        .sender = (void *) ctx,
        .fd = socket_data->fd
    };
    otp_socket_platform_set_listener((void *) &listener->base, &key);

    listener->base.handler = recv_handler;
    listener->process_id = ctx->process_id;
    listener->pid = pid;
    listener->ref_ticks = ref_ticks;
    listener->fd = socket_data->fd;
    listener->buffer_size = socket_data->buffer_size;
    listener->is_recvfrom = is_recvfrom;

    sys_register_listener(global, &listener->base);

    socket_data->socket_state = waiting_recv;
    socket_data->async_listener = listener;

    TRACE("EventListener 0x%p with sender=0x%p process_id=%i fd=%i added to platform listeners for recv\n", (void *) listener, (void *) ctx, ctx->process_id, socket_data->fd);
}

static void do_recv(Context *ctx, term msg)
{
    TRACE("do_recv\n");
    do_recv_internal(ctx, msg, false);
}

static void do_recvfrom(Context *ctx, term msg)
{
    TRACE("do_recvfrom\n");
    do_recv_internal(ctx, msg, true);
}

//
// send/sendto
//

static void do_send_internal(Context *ctx, term msg, term cmd, bool is_sendto)
{
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);
    term data = term_get_tuple_element(cmd, 1);
    term dest = term_invalid_term();
    if (is_sendto) {
        dest = term_get_tuple_element(cmd, 2);
    }

    // TODO make non-blocking

    // {Ref :: reference(), {ok, RestData} | {error, Reason}}  (RestData currently nil)
    // TODO we should allocate space for the sub-binary here when we support returning Rest
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        sync_send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }

    char *buf = NULL;
    size_t len = 0;
    if (term_is_binary(data)) {
        buf = (char *) term_binary_data(data);
        len = term_binary_size(data);
    } else if (term_is_list(data)) {
        InteropFunctionResult result = interop_iolist_size(data, &len);
        if (UNLIKELY(result == InteropMemoryAllocFail)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            sync_send_error_tuple(ctx, pid, ref_ticks, OUT_OF_MEMORY_ATOM);
            return;
        }
        if (UNLIKELY(result == InteropBadArg)) {
            TRACE("badarg: input data is not an iolist\n");
            sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
        // TODO: send an iolist without allocating a buffer, maybe by iterative sends
        buf = malloc(len);
        if (IS_NULL_PTR(buf)) {
            AVM_LOGW(TAG, "Failed to allocate %lu bytes for buffer\n", (unsigned long) len);
            free(buf);
            sync_send_error_tuple(ctx, pid, ref_ticks, OUT_OF_MEMORY_ATOM);
            return;
        }
        if (UNLIKELY(interop_write_iolist(data, buf) != InteropOk)) {
            TRACE("badarg: unable to write iolist\n");
            free(buf);
            sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
    } else {
        TRACE("badarg: input data is not a list\n");
        sync_send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
        return;
    }

    int sent_data = -1;

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

        sent_data = sendto(socket_data->fd, buf, len, 0, (struct sockaddr *) &destaddr, sizeof(destaddr));

    } else {
        sent_data = send(socket_data->fd, buf, len, 0);
    }

    if (sent_data == -1) {
        AVM_LOGE(TAG, "Unable to send data: res=%i.", sent_data);
        sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
    } else {
        // TODO allocate substring that references the data that was not sent
        term return_value = port_create_tuple2(ctx, OK_ATOM, term_nil());
        sync_send_reply(ctx, pid, ref_ticks, return_value);
    }

    if (term_is_list(data)) {
        free(buf);
    }
}

static void do_send(Context *ctx, term msg, term cmd)
{
    TRACE("do_send\n");
    do_send_internal(ctx, msg, cmd, false);
}

static void do_sendto(Context *ctx, term msg, term cmd)
{
    TRACE("do_sendto\n");
    do_send_internal(ctx, msg, cmd, true);
}

//
// connect
//

// NB.  Not currently active
static EventListener *connect_handler(GlobalContext *global, EventListener *base_listener)
{
    TRACE("connect_handler\n");

    AsyncEventListener *listener = GET_LIST_ENTRY(base_listener, AsyncEventListener, base);
    Context *ctx = globalcontext_get_process_lock(global, listener->process_id);
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    const int flags = fcntl(listener->fd, F_GETFL, 0);
    int fctl_res = fcntl(listener->fd, F_SETFL, flags ^ O_NONBLOCK);
    if (fctl_res == -1) {
        AVM_LOGE(TAG, "Unable to set socket back to blocking: fd=%i fctl_res=%i errno=%i", listener->fd, fctl_res, errno);

        // {Ref :: reference(), {error, {recv, Errno :: integer()}}}
        BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + 2 * TUPLE_SIZE(2), heap);
        term pid = listener->pid;
        term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
        term reply = port_heap_create_reply(&heap, ref, port_heap_create_sys_error_tuple(&heap, RECV_ATOM, errno));
        port_send_message_nolock(global, pid, reply);
        END_WITH_STACK_HEAP(heap, global);

    } else {

        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int res = getpeername(listener->fd, (struct sockaddr *) &addr, &addrlen);
        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Socket connect failed: fd=%i fctl_res=%i errno=%i", listener->fd, fctl_res, errno);

            // {Ref :: reference(), {error, {recv, Errno :: integer()}}}
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + 2 * TUPLE_SIZE(2), heap);
            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term reply = port_heap_create_reply(&heap, ref, port_heap_create_sys_error_tuple(&heap, RECV_ATOM, errno));
            port_send_message_nolock(global, pid, reply);
            END_WITH_STACK_HEAP(heap, global);

        } else {

            // {Ref :: reference(), ok}
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2) + REF_SIZE + 2 * TUPLE_SIZE(2), heap);
            term pid = listener->pid;
            term ref = term_from_ref_ticks(listener->ref_ticks, &heap);
            term reply = port_heap_create_reply(&heap, ref, OK_ATOM);
            port_send_message_nolock(global, pid, reply);
            END_WITH_STACK_HEAP(heap, global);
        }
    }

    socket_data->socket_state = idle;
    globalcontext_get_process_unlock(global, ctx);
    socket_data->async_listener = NULL;

    free(listener);

    return NULL;
}

// TODO make connect non-blocking
static void do_connect(Context *ctx, term msg, term cmd)
{
    TRACE("do_connect\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term sockaddr = term_get_tuple_element(cmd, 1);

    // int fctl_res = fcntl(socket_data->fd, F_SETFL, O_NONBLOCK);
    // if (fctl_res == -1) {
    //     AVM_LOGE(TAG, "Unable to set socket to non-blocking: fd=%i fctl_res=%i errno=%i", socket_data->fd, fctl_res, errno);
    //     sync_send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    //     return;
    // }

    struct sockaddr_in address;
    memset(&address, 0, sizeof(struct sockaddr_in));
    address.sin_family = AF_INET;

    // TODO is `loopback` a legal value?
    if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, loopback_atom)) {
        address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else if (term_is_map(sockaddr)) {
        term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
        address.sin_port = htons(term_to_int(port));
        term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
        if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
            address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            address.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
        }
    }
    socklen_t addr_len = sizeof(struct sockaddr_in);
    int res = connect(socket_data->fd, (const struct sockaddr *) &address, addr_len);
    if (res == -1) {
        if (errno == EINPROGRESS) {

            AsyncEventListener *listener = malloc(sizeof(AsyncEventListener));
            if (IS_NULL_PTR(listener)) {
                fprintf(stderr, "Failed to allocate memory for listener in connect: %s:%i.\n", __FILE__, __LINE__);
                AVM_ABORT();
            }

            struct EventListenerKey key = {
                .sender = (void *) ctx,
                .fd = socket_data->fd
            };
            otp_socket_platform_set_listener((void *) &listener->base, &key);

            listener->base.handler = connect_handler;
            listener->process_id = ctx->process_id;
            listener->pid = pid;
            listener->ref_ticks = ref_ticks;
            listener->fd = socket_data->fd;
            listener->buffer_size = socket_data->buffer_size;
            listener->is_recvfrom = false;

            sys_register_listener(global, &listener->base);

            socket_data->socket_state = waiting_connect;

            TRACE("EventListener 0x%p with sender=0x%p process_id=%i fd=%i added to platform listeners for connect\n", (void *) listener, (void *) ctx, ctx->process_id, socket_data->fd);
        } else {
            AVM_LOGE(TAG, "Unable to connect: res=%i errno=%i", res, errno);

            // TODO memory allocate
            sync_send_error_tuple(ctx, pid, ref_ticks, map_errno(errno, global));
        }
    } else if (res == 0) {
        sync_send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

//
// message processing
//

static NativeHandlerResult consume_mailbox(Context *ctx)
{
    TRACE("Processing mailbox message for process_id %i\n", ctx->process_id);
    GlobalContext *global = ctx->global;
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;

#ifdef ENABLE_TRACE
    TRACE("message: ");
    term_display(stdout, msg, ctx);
    TRACE("\n");
#endif

    if (globalcontext_is_term_equal_to_atom_string(global, msg, close_internal_atom)) {
        close_socket(ctx);
        TRACE("internal close: ctx->process_id=%i\n", ctx->process_id);
        return NativeTerminate;
    }

    NativeHandlerResult result = NativeContinue;
    term cmd_term = term_get_tuple_element(msg, 2);

    if (term_is_atom(cmd_term)) {

        enum otp_socket_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {

            case OTPSocketSockNameCmd: {
                do_sockname(ctx, msg);
                break;
            }
            case OTPSocketPeerNameCmd: {
                do_peername(ctx, msg);
                break;
            }
            case OTPSocketAcceptCmd: {
                do_accept(ctx, msg);
                break;
            }
            case OTPSocketRecvCmd: {
                do_recv(ctx, msg);
                break;
            }
            case OTPSocketRecvFromCmd: {
                do_recvfrom(ctx, msg);
                break;
            }
            case OTPSocketCloseCmd: {
                TRACE("closing: ctx->process_id=%i\n", ctx->process_id);
                do_close(ctx, msg);
                result = NativeTerminate;
                break;
            }
            default:
                AVM_LOGW(TAG, "Unexpected cmd");
                break;
        }

    } else if (term_is_tuple(cmd_term)) {

        term cmd_name_term = term_get_tuple_element(cmd_term, 0);
        enum otp_socket_cmd cmd_name = interop_atom_term_select_int(cmd_table, cmd_name_term, ctx->global);
        switch (cmd_name) {

            case OTPSocketInitCmd: {
                if (!do_init(ctx, msg, cmd_term)) {
                    return NativeTerminate;
                }
                break;
            }
            case OTPSocketBindCmd: {
                do_bind(ctx, msg, cmd_term);
                break;
            }
            case OTPSocketListenCmd: {
                do_listen(ctx, msg, cmd_term);
                break;
            }
            case OTPSocketSendCmd: {
                do_send(ctx, msg, cmd_term);
                break;
            }
            case OTPSocketSendToCmd: {
                do_sendto(ctx, msg, cmd_term);
                break;
            }
            case OTPSocketSetOptCmd: {
                do_setopt(ctx, msg, cmd_term);
                break;
            }
            case OTPSocketConnectCmd: {
                do_connect(ctx, msg, cmd_term);
                break;
            }
            default:
                AVM_LOGW(TAG, "Unexpected cmd_name");
                break;
        }
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return result;
}

//
// entrypoints
//

Context *otp_socket_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    return create_context(global);
}
