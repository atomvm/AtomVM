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
#include <inet.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <memory.h>
#include <nifs.h>
#include <otp_socket.h>
#include <port.h>
#include <posix_nifs.h>
#include <scheduler.h>
#include <sys.h>
#include <term.h>
#include <utils.h>

// We use errno to report errors with both BSD Sockets and LWIP
#include <errno.h>

#if OTP_SOCKET_BSD
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#if HAVE_SIGNAL
#include <signal.h>
#endif
#elif OTP_SOCKET_LWIP
#include <lwip/tcp.h>
#include <lwip/udp.h>
#else
#error OTP Socket requires BSD Socket or lwIP
#endif

// #define ENABLE_TRACE
#include <trace.h>

// Check some LWIP options
#if OTP_SOCKET_LWIP
#if !TCP_LISTEN_BACKLOG
#error TCP_LISTEN_BACKLOG is undefined
#endif
#endif

// To factorize parsing of erlang term options, we define few constants for lwIP.
#if OTP_SOCKET_LWIP
enum ShutdownHow
{
    SHUT_RD,
    SHUT_WR,
    SHUT_RDWR,
};

enum SocketProtocol
{
    IPPROTO_IP,
    IPPROTO_TCP,
    IPPROTO_UDP
};

enum SocketDomain
{
    PF_INET
};

enum SocketType
{
    SOCK_STREAM,
    SOCK_DGRAM
};
#endif

// Additional LWIP definitions
#if OTP_SOCKET_LWIP
enum SocketState
{
    // Bits
    SocketStateClosed = 0,
    SocketStateUDP = 1 << 1,
    SocketStateTCP = 1 << 2,
    SocketStateSelectingRead = 1 << 3,
    SocketStateConnected = 1 << 4,
    SocketStateListening = 1 << 5,

    // Actual states
    SocketStateUDPIdle = SocketStateUDP,
    SocketStateUDPSelectingRead = SocketStateUDP | SocketStateSelectingRead,
    SocketStateTCPNew = SocketStateTCP,
    SocketStateTCPConnected = SocketStateTCP | SocketStateConnected,
    SocketStateTCPSelectingRead = SocketStateTCPConnected | SocketStateSelectingRead,
    SocketStateTCPListening = SocketStateTCP | SocketStateListening,
    SocketStateTCPSelectingAccept = SocketStateTCPListening | SocketStateSelectingRead,
};

struct TCPAcceptedItem
{
    struct ListHead list_head;
    struct tcp_pcb *newpcb;
};

struct TCPReceivedItem
{
    struct ListHead list_head;
    struct pbuf *buf;
    err_t err;
};

struct UDPReceivedItem
{
    struct ListHead list_head;
    struct pbuf *buf;
    uint32_t addr;
    uint16_t port;
};

static err_t tcp_recv_cb(void *arg, struct tcp_pcb *tpcb, struct pbuf *p, err_t err);
static void udp_recv_cb(void *arg, struct udp_pcb *pcb, struct pbuf *p, const ip_addr_t *addr, u16_t port);

#endif

#if OTP_SOCKET_BSD
struct SocketResource
{
    int fd;
    uint64_t ref_ticks;
    int32_t selecting_process_id;
    ErlNifMonitor selecting_process_monitor;
    size_t buf_size;
};
#elif OTP_SOCKET_LWIP
struct SocketResource
{
    enum SocketState socket_state;
    union
    {
        struct tcp_pcb *tcp_pcb;
        struct udp_pcb *udp_pcb;
    };
    uint64_t ref_ticks;
    int32_t selecting_process_id; // trapped or selecting
    ErlNifMonitor selecting_process_monitor;
    bool linger_on;
    int linger_sec;
    size_t pos;
    struct ListHead received_list;
    size_t buf_size;
};
#endif

static const char *const addr_atom = ATOM_STR("\x4", "addr");
static const char *const any_atom = ATOM_STR("\x3", "any");
static const char *const invalid_option_atom = ATOM_STR("\xE", "invalid_option");
static const char *const invalid_value_atom = ATOM_STR("\xD", "invalid_value");
static const char *const linger_atom = ATOM_STR("\x6", "linger");
static const char *const loopback_atom = ATOM_STR("\x8", "loopback");
static const char *const onoff_atom = ATOM_STR("\x5", "onoff");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const rcvbuf_atom = ATOM_STR("\x6", "rcvbuf");
static const char *const reuseaddr_atom = ATOM_STR("\x9", "reuseaddr");

#define CLOSED_FD 0

#define ADDR_ATOM globalcontext_make_atom(global, addr_atom)
#define CLOSE_INTERNAL_ATOM globalcontext_make_atom(global, close_internal_atom)
#define ACCEPT_ATOM globalcontext_make_atom(global, accept_atom)
#define RECV_ATOM globalcontext_make_atom(global, recv_atom)

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

enum otp_socket_setopt_level
{
    OtpSocketInvalidSetoptLevel = 0,
    OtpSocketSetoptLevelSocket,
    OtpSocketSetoptLevelOTP
};

static const AtomStringIntPair otp_socket_setopt_level_table[] = {
    { ATOM_STR("\x6", "socket"), OtpSocketSetoptLevelSocket },
    { ATOM_STR("\x3", "otp"), OtpSocketSetoptLevelOTP },
    SELECT_INT_DEFAULT(OtpSocketInvalidSetoptLevel)
};

#define DEFAULT_BUFFER_SIZE 512

#ifndef MIN
#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#endif

static ErlNifResourceType *socket_resource_type;

//
// resource operations
//

static void socket_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct SocketResource *rsrc_obj = (struct SocketResource *) obj;

    TRACE("socket_dtor called on resource=%p\n", (void *) rsrc_obj);
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd != CLOSED_FD) {
        close(rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
    }
#elif OTP_SOCKET_LWIP
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateUDP) {
        udp_remove(rsrc_obj->udp_pcb);
        rsrc_obj->udp_pcb = NULL;
    } else if (rsrc_obj->socket_state & SocketStateTCP) {
        // Try to nicely close the connection here
        // NOTE: we can't handle linger if the socket is gone.
        if (UNLIKELY(tcp_close(rsrc_obj->tcp_pcb) != ERR_OK)) {
            // The resource will be gone, there is not much we can do here.
            if (!(rsrc_obj->socket_state & SocketStateTCPListening)) {
                tcp_abort(rsrc_obj->tcp_pcb);
            }
        }
        rsrc_obj->tcp_pcb = NULL;
    }
    LWIP_END();
#endif
}

#if OTP_SOCKET_BSD
static void socket_stop(ErlNifEnv *caller_env, void *obj, ErlNifEvent event, int is_direct_call)
{
    UNUSED(caller_env);
    UNUSED(event);
    UNUSED(is_direct_call);

    struct SocketResource *rsrc_obj = (struct SocketResource *) obj;

    if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
        enif_demonitor_process(caller_env, rsrc_obj, &rsrc_obj->selecting_process_monitor);
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
    }

    TRACE("socket_stop called on fd=%i\n", rsrc_obj->fd);
}
#endif

static void socket_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(caller_env);
    UNUSED(pid);
    UNUSED(mon);

    struct SocketResource *rsrc_obj = (struct SocketResource *) obj;

#if OTP_SOCKET_BSD
    TRACE("socket_down called on process_id=%i fd=%i\n", (int) *pid, rsrc_obj->fd);
#else
    TRACE("socket_down called on process_id=%i\n", (int) *pid);
#endif

#if OTP_SOCKET_BSD
    if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
        // Monitor fired, so make sure we don't try to demonitor in select_stop
        // as it could crash trying to reacquire lock on process table
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        enif_select(caller_env, rsrc_obj->fd, ERL_NIF_SELECT_STOP, rsrc_obj, NULL, term_nil());
    }
#elif OTP_SOCKET_LWIP
    // Monitor can be called when we're selecting, accepting or connecting.
    if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
        LWIP_BEGIN();
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        if (rsrc_obj->socket_state & SocketStateTCP) {
            if (rsrc_obj->socket_state & SocketStateTCPListening) {
                (void) tcp_close(rsrc_obj->tcp_pcb);
            } else {
                tcp_abort(rsrc_obj->tcp_pcb);
            }
            rsrc_obj->tcp_pcb = NULL;
            rsrc_obj->socket_state = SocketStateClosed;
        } else if (rsrc_obj->socket_state & SocketStateUDP) {
            udp_remove(rsrc_obj->udp_pcb);
            rsrc_obj->udp_pcb = NULL;
            rsrc_obj->socket_state = SocketStateClosed;
        }
        LWIP_END();
    }
#endif
}

static const ErlNifResourceTypeInit SocketResourceTypeInit = {
    .members = 3,
    .dtor = socket_dtor,
#if OTP_SOCKET_BSD
    .stop = socket_stop,
#else
    .stop = NULL,
#endif
    .down = socket_down,
};

// select emulation for lwIP that doesn't have select.
#if OTP_SOCKET_LWIP
static void select_event_send_notification_from_nif(struct SocketResource *rsrc_obj, Context *locked_ctx)
{
    BEGIN_WITH_STACK_HEAP(SELECT_EVENT_NOTIFICATION_SIZE, heap)
    term notification = select_event_make_notification(rsrc_obj, rsrc_obj->ref_ticks, false, &heap);
    mailbox_send(locked_ctx, notification);
    END_WITH_STACK_HEAP(heap, locked_ctx->global)
}

static void select_event_send_notification_from_handler(struct SocketResource *rsrc_obj, int32_t process_id)
{
    struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
    GlobalContext *global = rsrc_refc->resource_type->global;
    BEGIN_WITH_STACK_HEAP(SELECT_EVENT_NOTIFICATION_SIZE, heap)
    term notification = select_event_make_notification(rsrc_obj, rsrc_obj->ref_ticks, false, &heap);
    globalcontext_send_message(global, process_id, notification);
    END_WITH_STACK_HEAP(heap, global)
}
#endif

// register the socket_fd resource type
void otp_socket_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    socket_resource_type = enif_init_resource_type(&env, "socket_fd", &SocketResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
#if OTP_SOCKET_BSD && HAVE_SIGNAL
    // ignore pipe signals on shutdown
    signal(SIGPIPE, SIG_IGN);
#endif
}

//
// socket operations
//

static inline int get_domain(GlobalContext *global, term domain_term, bool *ok)
{
    *ok = true;

    int val = inet_atom_to_domain(domain_term, global);
    switch (val) {

        case InetDomain:
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

    int val = inet_atom_to_type(type_term, global);
    switch (val) {

        case InetStreamType:
            return SOCK_STREAM;

        case InetDgramType:
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

    int val = inet_atom_to_protocol(protocol_term, global);
    switch (val) {

        case InetIpProtocol:
            return IPPROTO_IP;

        case InetTcpProtocol:
            return IPPROTO_TCP;

        case InetUdpProtocol:
            return IPPROTO_UDP;

        default:
            AVM_LOGW(TAG, "Unsupported type.");
            *ok = false;
            return -1;
    }
}

/**
 * @brief Allocate memory on ctx and make and return an error tuple from immediate
 * term reason.
 * @param reason the reason, should be an immediate (atom or integer)
 * @param ctx the current context
 * @returns a term
 * @details This function is meant to be called from a nif that should return
 * its result directly, to allow for further processing of a possible out of
 * memory exception.
 * @end
 */
static inline term make_error_tuple(term reason, Context *ctx)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason);
    return error_tuple;
}

#if OTP_SOCKET_BSD
/**
 * @brief Like make_error_tuple but using errno converted to an atom or an int
 */
static term make_errno_tuple(Context *ctx)
{
    return make_error_tuple(posix_errno_to_term(errno, ctx->global), ctx);
}
#elif OTP_SOCKET_LWIP
static term make_lwip_err_tuple(err_t err, Context *ctx)
{
    return make_error_tuple(term_from_int(err), ctx);
}
#endif

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

    struct SocketResource *rsrc_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketResource));
    if (IS_NULL_PTR(rsrc_obj)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

#if OTP_SOCKET_BSD
    rsrc_obj->fd = socket(domain, type, protocol);
    if (UNLIKELY(rsrc_obj->fd == -1 || rsrc_obj->fd == CLOSED_FD)) {
        AVM_LOGE(TAG, "Failed to initialize socket.");
        enif_release_resource(rsrc_obj);
        return make_errno_tuple(ctx);
    } else {
        TRACE("nif_socket_open: Created socket fd=%i\n", rsrc_obj->fd);
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;

#elif OTP_SOCKET_LWIP
    if (domain == PF_INET && type == SOCK_STREAM && protocol == IPPROTO_TCP) {
        LWIP_BEGIN();
        rsrc_obj->tcp_pcb = tcp_new();
        LWIP_END();
        rsrc_obj->socket_state = SocketStateTCPNew;
    } else if (domain == PF_INET && type == SOCK_DGRAM && protocol == IPPROTO_UDP) {
        LWIP_BEGIN();
        rsrc_obj->udp_pcb = udp_new();
        LWIP_END();
        rsrc_obj->socket_state = SocketStateUDPIdle;
    } else {
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(rsrc_obj->udp_pcb == NULL && rsrc_obj->tcp_pcb == NULL)) {
        rsrc_obj->socket_state = SocketStateClosed;
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    } else {
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        rsrc_obj->linger_on = false;
        rsrc_obj->linger_sec = 0;
        rsrc_obj->pos = 0;
        list_init(&rsrc_obj->received_list);
        if (rsrc_obj->socket_state & SocketStateTCP) {
            LWIP_BEGIN();
            tcp_arg(rsrc_obj->tcp_pcb, rsrc_obj);
            tcp_recv(rsrc_obj->tcp_pcb, tcp_recv_cb);
            LWIP_END();
        } else {
            LWIP_BEGIN();
            udp_recv(rsrc_obj->udp_pcb, udp_recv_cb, rsrc_obj);
            LWIP_END();
        }
#endif
        rsrc_obj->buf_size = DEFAULT_BUFFER_SIZE;

        if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term obj = enif_make_resource(erl_nif_env_from_context(ctx), rsrc_obj);
        enif_release_resource(rsrc_obj);

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

bool term_to_otp_socket(term socket_term, struct SocketResource **rsrc_obj, Context *ctx)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), term_get_tuple_element(socket_term, 0), socket_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct SocketResource *) rsrc_obj_ptr;

    return true;
}

bool term_is_otp_socket(term socket_term)
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

static int send_closed_notification(Context *ctx, struct SocketResource *rsrc_obj)
{
    // send a {closed, Ref | undefined} message to the pid
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        return -1;
    }

    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, CLOSED_ATOM);
    term ref = (rsrc_obj->ref_ticks == 0) ? UNDEFINED_ATOM : term_from_ref_ticks(rsrc_obj->ref_ticks, &ctx->heap);
    term_put_tuple_element(error_tuple, 1, ref);

    TRACE("nif_socket_close: Sending msg to process %i, rsrc_obj = %p\n", (int) rsrc_obj->selecting_process_id, (void *) rsrc_obj);
    globalcontext_send_message(ctx->global, rsrc_obj->selecting_process_id, error_tuple);

    return 0;
}

#if OTP_SOCKET_LWIP
static void finalize_close_hander(struct LWIPEvent *event)
{
    enif_release_resource(event->finalize_close.rsrc_obj);
}
#endif

static term nif_socket_close(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_close\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd) {
        if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
            //
            // If we are in select, then stop selecting
            //
            int stop_res = enif_select(erl_nif_env_from_context(ctx), rsrc_obj->fd, ERL_NIF_SELECT_STOP, rsrc_obj, NULL, term_nil());
            if (UNLIKELY(stop_res < 0)) {
                RAISE_ERROR(BADARG_ATOM);
            }
            // TODO: check if stop_res & ERL_NIF_SELECT_STOP_CALLED or ERL_NIF_SELECT_STOP_SCHEDULED
            // following what OTP does. Indeed, if we have ERL_NIF_SELECT_STOP_SCHEDULED, we should not close the socket now
            //
            // If there is a process (other than ourself) who is waiting on select
            // the send a {closed, Ref} message to it, so that it can break
            // out of its receive statement.
            //
            if (rsrc_obj->selecting_process_id != ctx->process_id) {

                // send a {closed, Ref | undefined} message to the pid
                if (UNLIKELY(send_closed_notification(ctx, rsrc_obj) < 0)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
            } else {
                AVM_LOGW(TAG, "Selectable socket %i was closed but no known pid is waiting.  This shouldn't happen.", rsrc_obj->fd);
            }
        }

        int res = close(rsrc_obj->fd);
        if (UNLIKELY(res != 0)) {
            AVM_LOGW(TAG, "Failed to close socket %i", res);
        }

        TRACE("nif_socket_close: Clearing pid for socket fd=%i\n", rsrc_obj->fd);
        rsrc_obj->fd = CLOSED_FD;
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        rsrc_obj->ref_ticks = 0;
    } else {
        TRACE("Double close on socket fd %i", rsrc_obj->fd);
    }
#elif OTP_SOCKET_LWIP
    // If the socket is being selected by another process, send a closed tuple.
    if (rsrc_obj->socket_state & SocketStateSelectingRead
        && rsrc_obj->selecting_process_id != INVALID_PROCESS_ID
        && rsrc_obj->selecting_process_id != ctx->process_id) {
        // send a {closed, Ref | undefined} message to the pid
        if (UNLIKELY(send_closed_notification(ctx, rsrc_obj) < 0)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }
    if (rsrc_obj->socket_state == SocketStateClosed) {
        TRACE("Double close on pcb");
    } else {
        if (rsrc_obj->socket_state & SocketStateTCP) {
            LWIP_BEGIN();
            tcp_arg(rsrc_obj->tcp_pcb, NULL);
            err_t err = tcp_close(rsrc_obj->tcp_pcb);
            LWIP_END();
            if (UNLIKELY(err != ERR_OK)) {
                AVM_LOGW(TAG, "tcp_close failed with err=%d: %s:%i.\n", err, __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            rsrc_obj->tcp_pcb = NULL;
        } else if (rsrc_obj->socket_state & SocketStateUDP) {
            LWIP_BEGIN();
            udp_recv(rsrc_obj->udp_pcb, udp_recv_cb, NULL);
            udp_remove(rsrc_obj->udp_pcb);
            LWIP_END();
            rsrc_obj->udp_pcb = NULL;
        }
        rsrc_obj->socket_state = SocketStateClosed;
        // The resource should not go away until all callbacks are processed.
        enif_keep_resource(rsrc_obj);
        struct LWIPEvent event;
        event.handler = finalize_close_hander;
        event.finalize_close.rsrc_obj = rsrc_obj;
        otp_socket_lwip_enqueue(&event);
    }

#endif
    rsrc_obj->buf_size = DEFAULT_BUFFER_SIZE;

    return OK_ATOM;
}

//
// select
//

#if OTP_SOCKET_LWIP
static struct SocketResource *make_accepted_socket_resource(struct tcp_pcb *newpcb)
{
    struct SocketResource *conn_rsrc_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketResource));
    if (IS_NULL_PTR(conn_rsrc_obj)) {
        return NULL;
    }
    conn_rsrc_obj->socket_state = SocketStateTCPConnected;
    conn_rsrc_obj->tcp_pcb = newpcb;
    conn_rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
    conn_rsrc_obj->pos = 0;
    conn_rsrc_obj->linger_on = false;
    conn_rsrc_obj->linger_sec = 0;
    conn_rsrc_obj->buf_size = DEFAULT_BUFFER_SIZE;
    list_init(&conn_rsrc_obj->received_list);

    tcp_arg(newpcb, conn_rsrc_obj);
    tcp_recv(newpcb, tcp_recv_cb);
    return conn_rsrc_obj;
}

static void tcp_accept_handler(struct LWIPEvent *event)
{
    struct SocketResource *rsrc_obj = event->tcp_accept.rsrc_obj;
    struct TCPAcceptedItem *new_item = malloc(sizeof(struct TCPAcceptedItem));
    list_append(&rsrc_obj->received_list, &new_item->list_head);
    new_item->newpcb = event->tcp_accept.newpcb;

    // Send notification if we are selecting.
    if (rsrc_obj->socket_state & SocketStateSelectingRead) {
        // Clear flag to avoid sending a message again.
        rsrc_obj->socket_state &= ~SocketStateSelectingRead;
        if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
            select_event_send_notification_from_handler(rsrc_obj, rsrc_obj->selecting_process_id);
        } // otherwise, selecting process died but we can just wait for monitor to handle it
    }
}

static err_t tcp_accept_cb(void *arg, struct tcp_pcb *newpcb, err_t err)
{
    UNUSED(err);

    struct SocketResource *rsrc_obj = (struct SocketResource *) arg;
    if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
        if (newpcb != NULL) {
            // Because data can come in, we need to immediately set the receive callback
            // However, we cannot allocate the resource because we can't call malloc from ISR.
            tcp_arg(newpcb, NULL);
            tcp_recv(newpcb, tcp_recv_cb);

            // Delay accepting
            tcp_backlog_delayed(newpcb);

            // Enqueue an event to further process accept
            struct LWIPEvent event;
            event.handler = tcp_accept_handler;
            event.tcp_accept.rsrc_obj = rsrc_obj;
            event.tcp_accept.newpcb = newpcb;
            otp_socket_lwip_enqueue(&event);
        } else {
            if (err == ERR_MEM) {
                AVM_LOGW(TAG, "Insufficient memory to accept connections, client will have to resend SYN.");
            }
        }
        return ERR_OK;
    } else {
        // Selecting process died
        if (newpcb != NULL) {
            if (rsrc_obj->socket_state & SocketStateTCPListening) {
                (void) tcp_close(newpcb);
            } else {
                tcp_abort(newpcb);
            }
        }
        return ERR_ABRT;
    }
}

static void tcp_recv_handler(struct LWIPEvent *event)
{
    struct tcp_pcb *tpcb = event->tcp_recv.tpcb;
    // The resource should have been set by now as make_resource was queued first
    struct SocketResource *rsrc_obj = tpcb->callback_arg;
    if (IS_NULL_PTR(rsrc_obj)) {
        TRACE("Unexpected null resource in tcp_recv_handler -- buf = %p\n", (void *) event->tcp_recv.buf);
    } else {
        struct TCPReceivedItem *new_item = malloc(sizeof(struct TCPReceivedItem));
        list_append(&rsrc_obj->received_list, &new_item->list_head);
        new_item->buf = event->tcp_recv.buf;
        new_item->err = event->tcp_recv.err;

        // Send notification if we are selecting.
        if (rsrc_obj->socket_state & SocketStateSelectingRead) {
            // Clear flag to avoid sending a message again.
            rsrc_obj->socket_state &= ~SocketStateSelectingRead;
            if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
                select_event_send_notification_from_handler(rsrc_obj, rsrc_obj->selecting_process_id);
            } // otherwise, selecting process died but we can just wait for monitor to handle it
        }
    }
}

static err_t tcp_recv_cb(void *arg, struct tcp_pcb *tpcb, struct pbuf *p, err_t err)
{
    UNUSED(arg);

    // Enqueue an event to further process recv
    // The resource may or may not have been created, so we store tpcb instead.
    struct LWIPEvent event;
    event.handler = tcp_recv_handler;
    event.tcp_recv.tpcb = tpcb;
    event.tcp_recv.buf = p;
    event.tcp_recv.err = err;
    otp_socket_lwip_enqueue(&event);
    return ERR_OK;
}

static void udp_recv_handler(struct LWIPEvent *event)
{
    struct SocketResource *rsrc_obj = event->udp_recv.rsrc_obj;
    struct UDPReceivedItem *new_item = malloc(sizeof(struct UDPReceivedItem));
    list_append(&rsrc_obj->received_list, &new_item->list_head);
    new_item->buf = event->udp_recv.buf;
    new_item->addr = event->udp_recv.addr;
    new_item->port = event->udp_recv.port;

    // Send notification if we are selecting.
    if (rsrc_obj->socket_state & SocketStateSelectingRead) {
        // Clear flag to avoid sending a message again.
        rsrc_obj->socket_state &= ~SocketStateSelectingRead;
        if (rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
            select_event_send_notification_from_handler(rsrc_obj, rsrc_obj->selecting_process_id);
        } // otherwise, selecting process died
    }
}

static void udp_recv_cb(void *arg, struct udp_pcb *pcb, struct pbuf *p, const ip_addr_t *addr, u16_t port)
{
    TRACE("udp_recv_cb\n");

    UNUSED(pcb);

    struct SocketResource *rsrc_obj = (struct SocketResource *) arg;
    if (LIKELY(rsrc_obj)) {
        struct LWIPEvent event;
        event.handler = udp_recv_handler;
        event.udp_recv.rsrc_obj = rsrc_obj;
        event.udp_recv.buf = p;
        // Convert IPv4 address as it may be invalid after this function returns
        // (lwIP documentation mentions it may be in pbuf but experience shows
        // it can be invalid even if p is not freed)
        event.udp_recv.addr = ntohl(ip_addr_get_ip4_u32(addr));
        event.udp_recv.port = port;
        otp_socket_lwip_enqueue(&event);
    } // Otherwise socket was closed.
}
#endif

static term nif_socket_select_read(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_select_read\n");

    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    term select_ref_term = argv[1];
    if (select_ref_term != UNDEFINED_ATOM) {
        VALIDATE_VALUE(select_ref_term, term_is_reference);
    }
    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (rsrc_obj->selecting_process_id != ctx->process_id && rsrc_obj->selecting_process_id != INVALID_PROCESS_ID) {
        // demonitor can fail if process is gone.
        enif_demonitor_process(env, rsrc_obj, &rsrc_obj->selecting_process_monitor);
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
    // Monitor first as select is less likely to fail and it's less expensive to demonitor
    // if select fails than to stop select if monitor fails
    if (rsrc_obj->selecting_process_id != ctx->process_id) {
        if (UNLIKELY(enif_monitor_process(env, rsrc_obj, &ctx->process_id, &rsrc_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(NOPROC_ATOM);
        }
        rsrc_obj->selecting_process_id = ctx->process_id;
    }

    rsrc_obj->ref_ticks = (select_ref_term == UNDEFINED_ATOM) ? 0 : term_to_ref_ticks(select_ref_term);

#if OTP_SOCKET_BSD
    TRACE("rsrc_obj->fd=%i\n", (int) rsrc_obj->fd);

    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), rsrc_obj->fd, ERL_NIF_SELECT_READ, rsrc_obj, &ctx->process_id, select_ref_term) < 0)) {
        enif_demonitor_process(env, rsrc_obj, &rsrc_obj->selecting_process_monitor);
        rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }

    TRACE("nif_socket_select: Setting pid for socket fd %i to %i\n", (int) rsrc_obj->fd, (int) ctx->process_id);

#elif OTP_SOCKET_LWIP
    LWIP_BEGIN();
    switch (rsrc_obj->socket_state) {
        case SocketStateTCPListening: {
            if (!list_is_empty(&rsrc_obj->received_list)) {
                // Send (or resend) notification
                select_event_send_notification_from_nif(rsrc_obj, ctx);
            } else {
                // Set flag to send it when a packet will arrive.
                rsrc_obj->socket_state = SocketStateTCPSelectingAccept;
            }
        } break;
        case SocketStateTCPSelectingAccept:
            // noop
            break;
        case SocketStateTCPConnected: {
            if (!list_is_empty(&rsrc_obj->received_list)) {
                // Send (or resend) notification
                select_event_send_notification_from_nif(rsrc_obj, ctx);
            } else {
                // Set flag to send it when a packet will arrive.
                rsrc_obj->socket_state = SocketStateTCPSelectingRead;
            }
        } break;
        case SocketStateTCPSelectingRead:
            // noop
            break;
        case SocketStateUDPIdle: {
            if (!list_is_empty(&rsrc_obj->received_list)) {
                // Send (or resend) notification
                select_event_send_notification_from_nif(rsrc_obj, ctx);
            } else {
                rsrc_obj->socket_state = SocketStateUDPSelectingRead;
            }
        } break;
        case SocketStateUDPSelectingRead:
            // noop
            break;
        default:
            enif_demonitor_process(env, rsrc_obj, &rsrc_obj->selecting_process_monitor);
            LWIP_END();
            RAISE_ERROR(BADARG_ATOM);
    }
    LWIP_END();
#endif

    return OK_ATOM;
}

static term nif_socket_select_stop(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_stop\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), rsrc_obj->fd, ERL_NIF_SELECT_STOP, rsrc_obj, NULL, term_nil()) < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
#elif OTP_SOCKET_LWIP
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateSelectingRead) {
        rsrc_obj->socket_state &= ~SocketStateSelectingRead;
    }
    LWIP_END();

    return OK_ATOM;
#endif
}

//
// setopt
//

static term nif_socket_setopt(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_setopt\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
#endif
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    term level_tuple = argv[1];
    term value = argv[2];

    term level = term_get_tuple_element(level_tuple, 0);
    int level_val = interop_atom_term_select_int(otp_socket_setopt_level_table, level, global);
    switch (level_val) {

        case OtpSocketSetoptLevelSocket: {

            term opt = term_get_tuple_element(level_tuple, 1);
            if (globalcontext_is_term_equal_to_atom_string(global, opt, reuseaddr_atom)) {
                int option_value = (value == TRUE_ATOM);
#if OTP_SOCKET_BSD
                int res = setsockopt(rsrc_obj->fd, SOL_SOCKET, SO_REUSEADDR, &option_value, sizeof(int));
                if (UNLIKELY(res != 0)) {
                    return make_errno_tuple(ctx);
                } else {
                    return OK_ATOM;
                }
#elif OTP_SOCKET_LWIP
                LWIP_BEGIN();
                if (option_value) {
                    if (rsrc_obj->socket_state & SocketStateTCP) {
                        ip_set_option(rsrc_obj->tcp_pcb, SOF_REUSEADDR);
                    } else {
                        ip_set_option(rsrc_obj->udp_pcb, SOF_REUSEADDR);
                    }
                } else {
                    if (rsrc_obj->socket_state & SocketStateTCP) {
                        ip_reset_option(rsrc_obj->tcp_pcb, SOF_REUSEADDR);
                    } else {
                        ip_reset_option(rsrc_obj->udp_pcb, SOF_REUSEADDR);
                    }
                }
                LWIP_END();
                return OK_ATOM;
#endif
            } else if (globalcontext_is_term_equal_to_atom_string(global, opt, linger_atom)) {
                term onoff = interop_kv_get_value(value, onoff_atom, ctx->global);
                term linger = interop_kv_get_value(value, linger_atom, ctx->global);
                VALIDATE_VALUE(linger, term_is_integer);

#if OTP_SOCKET_BSD
                struct linger sl;
                sl.l_onoff = (onoff == TRUE_ATOM);
                sl.l_linger = term_to_int(linger);
                int res = setsockopt(rsrc_obj->fd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));
                if (UNLIKELY(res != 0)) {
                    return make_errno_tuple(ctx);
                } else {
                    return OK_ATOM;
                }
#elif OTP_SOCKET_LWIP
                rsrc_obj->linger_on = (onoff == TRUE_ATOM);
                rsrc_obj->linger_sec = term_to_int(linger);
                return OK_ATOM;
#endif
                // TODO add more as needed
                // int flag = 1;
                // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
                // if (UNLIKELY(res != 0)) {
                //     AVM_LOGW(TAG, "Failed to set TCP_NODELAY.");
                // }
            } else {
                RAISE_ERROR(BADARG_ATOM);
            }

            case OtpSocketSetoptLevelOTP: {
                term opt = term_get_tuple_element(level_tuple, 1);
                if (globalcontext_is_term_equal_to_atom_string(global, opt, rcvbuf_atom)) {
                    // socket:setopt(Socket, {otp, rcvbuf}, BufSize :: non_neg_integer())

                    // TODO support the atom `default` as a value to roll back to the default buffer size
                    if (UNLIKELY(!term_is_integer(value))) {
                        AVM_LOGE(TAG, "socket:setopt: otp rcvbuf value must be an integer");
                        return make_error_tuple(globalcontext_make_atom(global, invalid_value_atom), ctx);
                    }

                    avm_int_t buf_size = term_to_int(value);
                    if (UNLIKELY(buf_size < 0)) {
                        AVM_LOGE(TAG, "socket:setopt: otp rcvbuf value may not be negative");
                        return make_error_tuple(globalcontext_make_atom(global, invalid_value_atom), ctx);
                    }

                    rsrc_obj->buf_size = (size_t) buf_size;

                    return OK_ATOM;
                } else {
                    AVM_LOGE(TAG, "socket:setopt: Unsupported otp option");
                    return make_error_tuple(globalcontext_make_atom(global, invalid_option_atom), ctx);
                }
            }

            default: {
                AVM_LOGE(TAG, "socket:setopt: Unsupported level");
                RAISE_ERROR(BADARG_ATOM);
            }
        }
    }
}

//
// sockname
//

static term nif_socket_sockname(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_sockname\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
#endif
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }

#if OTP_SOCKET_BSD
    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getsockname(rsrc_obj->fd, (struct sockaddr *) &addr, &addrlen);

    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", rsrc_obj->fd, res);
        return make_errno_tuple(ctx);
    }
    uint32_t ip4_u32 = ntohl(addr.sin_addr.s_addr);
    uint16_t port_u16 = ntohs(addr.sin_port);
#elif OTP_SOCKET_LWIP
    uint32_t ip4_u32;
    uint16_t port_u16;
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateTCP) {
        ip4_u32 = ntohl(ip_addr_get_ip4_u32(&rsrc_obj->tcp_pcb->local_ip));
        port_u16 = rsrc_obj->tcp_pcb->local_port;
    } else {
        ip4_u32 = ntohl(ip_addr_get_ip4_u32(&rsrc_obj->udp_pcb->local_ip));
        port_u16 = rsrc_obj->udp_pcb->local_port;
    }
    LWIP_END();
#endif

    // {ok, #{addr => {a,b,c,d}, port => integer()}}
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    } else {
        term address = inet_make_addr4(ip4_u32, &ctx->heap);
        term port_number = term_from_int(port_u16);

        term map = term_alloc_map(2, &ctx->heap);
        term_set_map_assoc(map, 0, ADDR_ATOM, address);
        term_set_map_assoc(map, 1, PORT_ATOM, port_number);
        term return_value = port_create_tuple2(ctx, OK_ATOM, map);

        return return_value;
    }
}

//
// peername
//

static term nif_socket_peername(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_peername\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    if (rsrc_obj->socket_state & SocketStateUDP) {
        // TODO: handle "connected" UDP sockets
        return make_error_tuple(posix_errno_to_term(EOPNOTSUPP, global), ctx);
    }
    if ((rsrc_obj->socket_state & SocketStateTCPListening) == SocketStateTCPListening) {
        return make_error_tuple(posix_errno_to_term(ENOTCONN, global), ctx);
    }
#endif

#if OTP_SOCKET_BSD
    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getpeername(rsrc_obj->fd, (struct sockaddr *) &addr, &addrlen);

    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to getpeername: fd=%i res=%i.", rsrc_obj->fd, res);
        return make_errno_tuple(ctx);
    }
    uint32_t ip4_u32 = ntohl(addr.sin_addr.s_addr);
    uint16_t port_u16 = ntohs(addr.sin_port);
#elif OTP_SOCKET_LWIP
    // TODO: support peername for "connected" UDP sockets
    uint32_t ip4_u32 = ntohl(ip_addr_get_ip4_u32(&rsrc_obj->tcp_pcb->remote_ip));
    uint16_t port_u16 = rsrc_obj->tcp_pcb->remote_port;
#endif

    // {ok, #{addr => {a,b,c,d}, port => integer()}}
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    } else {
        term address = inet_make_addr4(ip4_u32, &ctx->heap);
        term port_number = term_from_int(port_u16);

        term map = term_alloc_map(2, &ctx->heap);
        term_set_map_assoc(map, 0, ADDR_ATOM, address);
        term_set_map_assoc(map, 1, PORT_ATOM, port_number);
        term return_value = port_create_tuple2(ctx, OK_ATOM, map);

        return return_value;
    }
}

//
// bind
//

static term nif_socket_bind(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_bind\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    TRACE("rsrc_obj->fd=%i\n", (int) rsrc_obj->fd);
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#endif

    term sockaddr = argv[1];

#if OTP_SOCKET_BSD
    struct sockaddr_in serveraddr;
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
#elif OTP_SOCKET_LWIP
    ip_addr_t ip_addr;
#endif

    uint16_t port_u16 = 0;

    if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, any_atom)) {
#if OTP_SOCKET_BSD
        serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
#elif OTP_SOCKET_LWIP
        ip_addr_set_any(false, &ip_addr);
#endif
    } else if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, loopback_atom)) {
#if OTP_SOCKET_BSD
        serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
#elif OTP_SOCKET_LWIP
        ip_addr_set_loopback(false, &ip_addr);
#endif
    } else if (term_is_map(sockaddr)) {
        term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
        port_u16 = term_to_int(port);
        term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
        if (globalcontext_is_term_equal_to_atom_string(global, addr, any_atom)) {
#if OTP_SOCKET_BSD
            serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
#elif OTP_SOCKET_LWIP
            ip_addr_set_any(false, &ip_addr);
#endif
        } else if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
#if OTP_SOCKET_BSD
            serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
#elif OTP_SOCKET_LWIP
            ip_addr_set_loopback(false, &ip_addr);
#endif
        } else {
#if OTP_SOCKET_BSD
            serveraddr.sin_addr.s_addr = htonl(inet_addr4_to_uint32(addr));
#elif OTP_SOCKET_LWIP
            ip_addr_set_ip4_u32(&ip_addr, htonl(inet_addr4_to_uint32(addr)));
#endif
        }
    }

#if OTP_SOCKET_BSD
    serveraddr.sin_port = htons(port_u16);
    socklen_t address_len = sizeof(serveraddr);
    int res = bind(rsrc_obj->fd, (struct sockaddr *) &serveraddr, address_len);
    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
        return make_errno_tuple(ctx);
    } else {
        return OK_ATOM;
    }
#elif OTP_SOCKET_LWIP
    err_t res;
    if (rsrc_obj->socket_state & SocketStateTCP) {
        res = tcp_bind(rsrc_obj->tcp_pcb, &ip_addr, port_u16);
    } else {
        res = udp_bind(rsrc_obj->udp_pcb, &ip_addr, port_u16);
    }
    if (UNLIKELY(res != ERR_OK)) {
        AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
        return make_lwip_err_tuple(res, ctx);
    } else {
        return OK_ATOM;
    }
#endif
}

//
// listen
//

static term nif_socket_listen(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_listen\n");
    UNUSED(argc);

    GlobalContext *global = ctx->global;

    VALIDATE_VALUE(argv[0], term_is_otp_socket);
    VALIDATE_VALUE(argv[1], term_is_integer);

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    if (rsrc_obj->socket_state & SocketStateUDP) {
        return make_error_tuple(posix_errno_to_term(EPROTOTYPE, global), ctx);
    }
#endif

    int backlog = term_to_int(argv[1]);

#if OTP_SOCKET_BSD
    int res = listen(rsrc_obj->fd, backlog);
    if (UNLIKELY(res != 0)) {
        AVM_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
        return make_errno_tuple(ctx);
    } else {
        return OK_ATOM;
    }
#elif OTP_SOCKET_LWIP
    uint8_t backlog_u8;
    if (backlog > 255) {
        // POSIX says: "Implementations may impose a limit on backlog and silently reduce the specified value"
        backlog_u8 = 255;
    }
    if (backlog < 0) {
        // POSIX says: "If listen() is called with a backlog argument value that is less than 0, the function behaves as if it had been called with a backlog argument value of 0."
        backlog_u8 = 0;
    } else {
        backlog_u8 = backlog;
    }
    err_t err;
    struct tcp_pcb *new_pcb = tcp_listen_with_backlog_and_err(rsrc_obj->tcp_pcb, backlog_u8, &err);
    if (new_pcb == NULL) {
        return make_lwip_err_tuple(err, ctx);
    }
    // Define accept callback
    tcp_accept(new_pcb, tcp_accept_cb);
    rsrc_obj->tcp_pcb = new_pcb;
    rsrc_obj->socket_state = SocketStateTCPListening;
    return OK_ATOM;
#endif
}

//
// accept
//

#if OTP_SOCKET_LWIP
static term make_accepted_socket_term(struct SocketResource *conn_rsrc_obj, Heap *heap, GlobalContext *global)
{
    term obj = term_from_resource(conn_rsrc_obj, heap);

    term socket_term = term_alloc_tuple(2, heap);
    uint64_t ref_ticks = globalcontext_get_ref_ticks(global);
    term ref = term_from_ref_ticks(ref_ticks, heap);
    term_put_tuple_element(socket_term, 0, obj);
    term_put_tuple_element(socket_term, 1, ref);
    return socket_term;
}
#endif

static term nif_socket_accept(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_accept\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state & SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    if (rsrc_obj->socket_state & SocketStateUDP) {
        return make_error_tuple(posix_errno_to_term(EOPNOTSUPP, global), ctx);
    }
    // Only listening is allowed
    if ((rsrc_obj->socket_state & SocketStateTCPListening) != SocketStateTCPListening) {
        return make_error_tuple(posix_errno_to_term(EINVAL, global), ctx);
    }
#endif

#if OTP_SOCKET_BSD
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(rsrc_obj->fd, (struct sockaddr *) &clientaddr, &clientlen);
    if (UNLIKELY(fd == -1 || fd == CLOSED_FD)) {
        AVM_LOGE(TAG, "Unable to accept on socket %i.", rsrc_obj->fd);
        int err = errno;
        term reason = (err == ECONNABORTED) ? CLOSED_ATOM : posix_errno_to_term(err, global);
        return make_error_tuple(reason, ctx);
    } else {

        struct SocketResource *conn_rsrc_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketResource));
        conn_rsrc_obj->fd = fd;
        conn_rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
        conn_rsrc_obj->buf_size = DEFAULT_BUFFER_SIZE;
        TRACE("nif_socket_accept: Created socket on accept fd=%i\n", rsrc_obj->fd);

        term obj = enif_make_resource(erl_nif_env_from_context(ctx), conn_rsrc_obj);
        enif_release_resource(conn_rsrc_obj);

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
#elif OTP_SOCKET_LWIP
    term result;
    LWIP_BEGIN();
    if (!list_is_empty(&rsrc_obj->received_list)) {
        struct TCPAcceptedItem *first_item = CONTAINER_OF(list_first(&rsrc_obj->received_list), struct TCPAcceptedItem, list_head);
        struct SocketResource *new_resource = make_accepted_socket_resource(first_item->newpcb);
        if (IS_NULL_PTR(new_resource)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            LWIP_END();
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        size_t requested_size = TERM_BOXED_RESOURCE_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE;
        if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            LWIP_END();
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        tcp_backlog_accepted(first_item->newpcb);
        list_remove(&first_item->list_head);
        free(first_item);

        term socket_term = make_accepted_socket_term(new_resource, &ctx->heap, global);
        result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, socket_term);
    } else {
        // return EAGAIN
        return make_error_tuple(posix_errno_to_term(EAGAIN, ctx->global), ctx);
    }
    LWIP_END();
    return result;
#endif
}

//
// recv/recvfrom
//
#if OTP_SOCKET_LWIP
static size_t copy_pbuf_data(struct pbuf *src, size_t offset, size_t count, uint8_t *dst)
{
    size_t copied = 0;
    while (count > 0 && src != NULL) {
        if (offset > src->len) {
            offset -= src->len;
            src = src->next;
            continue;
        }
        size_t chunk_count = MIN(count, src->len - offset);
        memcpy(dst, ((const uint8_t *) src->payload) + offset, chunk_count);
        count -= chunk_count;
        copied += chunk_count;
        dst += chunk_count;
        src = src->next;
    }
    return copied;
}
#endif

ssize_t socket_recv(struct SocketResource *rsrc_obj, uint8_t *buf, size_t len, int flags, term *from, Heap *heap)
{
#if OTP_SOCKET_BSD
    //
    // receive data on the socket
    //
    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    ssize_t res;
    if (from) {
        struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
        GlobalContext *global = rsrc_refc->resource_type->global;

        res = recvfrom(rsrc_obj->fd, buf, len, flags, (struct sockaddr *) &addr, &addrlen);

        term address = inet_make_addr4(ntohl(addr.sin_addr.s_addr), heap);
        term port_number = term_from_int(ntohs(addr.sin_port));

        term map = term_alloc_map(2, heap);
        term_set_map_assoc(map, 0, ADDR_ATOM, address);
        term_set_map_assoc(map, 1, PORT_ATOM, port_number);
        *from = map;
    } else {
        res = recv(rsrc_obj->fd, buf, len, flags);
    }
    if (res == 0) {
        return SocketClosed;
    }
    if (res < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return SocketWouldBlock;
        }
        return SocketOtherError;
    }
    return res;
#elif OTP_SOCKET_LWIP
    UNUSED(flags);

    uint32_t ip4_u32;
    uint16_t port_u16;

    size_t remaining = len;
    uint8_t *ptr = buf;
    bool closed = false;
    err_t err = ERR_OK;
    // Use lwIP lock
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateTCP) {
        size_t pos = rsrc_obj->pos;
        struct ListHead *item;
        struct ListHead *tmp;
        MUTABLE_LIST_FOR_EACH (item, tmp, &rsrc_obj->received_list) {
            struct TCPReceivedItem *received_item = GET_LIST_ENTRY(item, struct TCPReceivedItem, list_head);
            if (received_item->buf == NULL || received_item->err != ERR_OK) {
                closed = received_item->buf == NULL;
                err = received_item->err;
                break;
            }
            if (pos < received_item->buf->tot_len) {
                size_t copied = copy_pbuf_data(received_item->buf, pos, remaining, ptr);
                ptr += copied;
                remaining -= copied;
                tcp_recved(rsrc_obj->tcp_pcb, copied);
                if (copied + pos == received_item->buf->tot_len) {
                    // all data was copied.
                    list_remove(item);
                    pbuf_free(received_item->buf);
                    pos = 0;
                } else {
                    pos = pos + copied;
                }
                if (remaining == 0) {
                    break;
                }
            } else {
                pos -= received_item->buf->tot_len;
            }
        }
        rsrc_obj->pos = pos;
        if (from) {
            ip4_u32 = ntohl(ip_addr_get_ip4_u32(&rsrc_obj->tcp_pcb->remote_ip));
            port_u16 = rsrc_obj->tcp_pcb->remote_port;
        }
    } else {
        struct UDPReceivedItem *first_item = CONTAINER_OF(list_first(&rsrc_obj->received_list), struct UDPReceivedItem, list_head);
        size_t copied = copy_pbuf_data(first_item->buf, 0, remaining, ptr);
        remaining -= copied;
        if (from) {
            ip4_u32 = first_item->addr;
            port_u16 = first_item->port;
        }
        list_remove(&first_item->list_head);
        pbuf_free(first_item->buf);
        free(first_item);
    }
    LWIP_END();
    if (remaining < len) {
        if (from) {
            struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
            GlobalContext *global = rsrc_refc->resource_type->global;

            term address = inet_make_addr4(ip4_u32, heap);
            term port_number = term_from_int(port_u16);

            term map = term_alloc_map(2, heap);
            term_set_map_assoc(map, 0, globalcontext_make_atom(global, addr_atom), address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);

            *from = map;
        }

        return len - remaining;
    }
    if (closed) {
        return SocketClosed;
    }
    return err == ERR_OK ? SocketWouldBlock : SocketOtherError;
#endif
}

#if OTP_SOCKET_BSD
static term nif_socket_recv_with_peek(Context *ctx, struct SocketResource *rsrc_obj, size_t len, bool is_recvfrom)
{
    TRACE("nif_socket_recv_with_peek\n");

    GlobalContext *global = ctx->global;

    int flags = MSG_WAITALL;
    ssize_t res = recvfrom(rsrc_obj->fd, NULL, rsrc_obj->buf_size, MSG_PEEK | flags, NULL, NULL);
    TRACE("%li bytes available.\n", (long int) res);
    if (res < 0) {
        AVM_LOGI(TAG, "Unable to receive data on fd %i.  errno=%i", rsrc_obj->fd, errno);
        return make_errno_tuple(ctx);
    } else if (res == 0) {
        TRACE("Peer closed socket %i.\n", rsrc_obj->fd);
        return make_error_tuple(CLOSED_ATOM, ctx);
    } else {
        // user-supplied len has higher precedence than the default buffer size, but we also
        // want the configured default buffer size to be a lower bound on anything we peek
        ssize_t buffer_size = MIN(len == 0 ? (ssize_t) rsrc_obj->buf_size : (ssize_t) len, res);

        // {ok, Data :: binary()}
        // {ok, {Source :: #{addr => Address :: {0..255, 0..255, 0..255, 0..255}, port => Port :: non_neg_integer()}, Data :: binary()}}
        size_t ensure_packet_avail = term_binary_data_size_in_terms(buffer_size) + BINARY_HEADER_SIZE;
        size_t requested_size = TUPLE_SIZE(2) + ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + INET_ADDR4_TUPLE_SIZE + TERM_MAP_SIZE(2)) : 0);

        if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term data = term_create_uninitialized_binary(buffer_size, &ctx->heap, global);
        uint8_t *buffer = (uint8_t *) term_binary_data(data);

        //
        // receive data on the socket
        //
        term map = term_invalid_term();
        res = socket_recv(rsrc_obj, buffer, buffer_size, flags, is_recvfrom ? &map : NULL, &ctx->heap);

        TRACE("otp_socket.recv_handler: received data on fd: %i available=%lu, read=%lu\n", rsrc_obj->fd, (unsigned long) res, (unsigned long) buffer_size);

        term payload;
        if (is_recvfrom) {
            term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
            payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
        } else {
            payload = port_heap_create_ok_tuple(&ctx->heap, data);
        }

        return payload;
    }
}

static term nif_socket_recv_without_peek(Context *ctx, struct SocketResource *rsrc_obj, size_t len, bool is_recvfrom)
{
    TRACE("nif_socket_recv_without_peek\n");

    GlobalContext *global = ctx->global;

    size_t buffer_size = len == 0 ? rsrc_obj->buf_size : len;
    uint8_t *buffer = (uint8_t *) malloc(buffer_size);
    term payload = term_invalid_term();

    if (IS_NULL_PTR(buffer)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);

    } else {

        term map = term_invalid_term();
        if (is_recvfrom) {
            if (UNLIKELY(memory_ensure_free(ctx, INET_ADDR4_TUPLE_SIZE + TERM_MAP_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
        }

        ssize_t res = socket_recv(rsrc_obj, buffer, buffer_size, 0, is_recvfrom ? &map : NULL, &ctx->heap);

        if (res < 0) {

            int err = errno;
            term reason = (err == ECONNRESET) ? globalcontext_make_atom(global, ATOM_STR("\xA", "econnreset")) : posix_errno_to_term(err, global);

            if (err == ECONNRESET) {
                AVM_LOGI(TAG, "Peer closed connection.");
            } else {
                AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", rsrc_obj->fd, errno);
            }

            return make_error_tuple(reason, ctx);

        } else if (res == 0) {
            TRACE("Peer closed socket %i.\n", rsrc_obj->fd);
            return make_error_tuple(CLOSED_ATOM, ctx);
        } else {

            size_t len = (size_t) res;
            TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", rsrc_obj->fd, (unsigned long) len);

            // {ok, Data :: binary()}
            // {ok, {Source :: #{addr => Address :: {0..255, 0..255, 0..255, 0..255}, port => Port :: non_neg_integer()}, Data :: binary()}}
            size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
            size_t requested_size = TUPLE_SIZE(2) + ensure_packet_avail + (is_recvfrom ? TUPLE_SIZE(2) : 0);

            if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, is_recvfrom ? 1 : 0, &map, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term data = term_from_literal_binary(buffer, len, &ctx->heap, global);

            if (is_recvfrom) {
                term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
                payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
            } else {
                payload = port_heap_create_ok_tuple(&ctx->heap, data);
            }
        }

        free(buffer);
        return payload;
    }
}

#elif OTP_SOCKET_LWIP

static term nif_socket_recv_lwip(Context *ctx, struct SocketResource *rsrc_obj, size_t len, bool is_recvfrom)
{
    TRACE("nif_socket_recv_lwip\n");

    GlobalContext *global = ctx->global;

    size_t buffer_size = 0;
    bool closed = false;
    err_t err = ERR_OK;
    // Use lwIP lock
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateTCP) {
        // TCP: we return up to len bytes or all available (if len == 0)
        struct ListHead *item;
        LIST_FOR_EACH (item, &rsrc_obj->received_list) {
            struct TCPReceivedItem *received_item = GET_LIST_ENTRY(item, struct TCPReceivedItem, list_head);
            if (received_item->buf == NULL || received_item->err != ERR_OK) {
                closed = received_item->buf == NULL;
                err = received_item->err;
                break;
            } else {
                buffer_size += received_item->buf->tot_len;
                if (len > 0 && buffer_size >= len) {
                    buffer_size = len;
                    break;
                }
            }
        }
    } else {
        // UDP: we return the first message and truncate it to len if len != 0
        if (!list_is_empty(&rsrc_obj->received_list)) {
            struct UDPReceivedItem *first_item = CONTAINER_OF(list_first(&rsrc_obj->received_list), struct UDPReceivedItem, list_head);
            buffer_size = first_item->buf->tot_len;
            if (len > 0 && buffer_size > len) {
                buffer_size = len;
            }
        }
    }
    LWIP_END();

    // If we have no data, return EAGAIN or closed or the error.
    if (buffer_size == 0) {
        if (closed) {
            return make_error_tuple(CLOSED_ATOM, ctx);
        }
        if (err != ERR_OK) {
            return make_error_tuple(term_from_int(err), ctx);
        }
        return make_error_tuple(posix_errno_to_term(EAGAIN, global), ctx);
    }

    size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
    size_t requested_size = REF_SIZE + 2 * TUPLE_SIZE(2) + ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + INET_ADDR4_TUPLE_SIZE + TERM_MAP_SIZE(2)) : 0);
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term data = term_create_uninitialized_binary(buffer_size, &ctx->heap, global);
    uint8_t *ptr = (uint8_t *) term_binary_data(data);
    size_t remaining = buffer_size;

    term map = term_invalid_term();

    ssize_t result = socket_recv(rsrc_obj, ptr, remaining, 0, is_recvfrom ? &map : NULL, &ctx->heap);
    UNUSED(result);

    term payload;

    if (is_recvfrom) {
        term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
        payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
    } else {
        payload = port_heap_create_ok_tuple(&ctx->heap, data);
    }

    return payload;
}
#endif

static term nif_socket_recv_internal(Context *ctx, term argv[], bool is_recvfrom)
{
    VALIDATE_VALUE(argv[0], term_is_otp_socket);
    VALIDATE_VALUE(argv[1], term_is_integer);
    avm_int_t len = term_to_int(argv[1]);
    // We raise badarg but return error tuples for POSIX errors
    if (UNLIKELY(len < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }
#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, ctx->global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state & SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, ctx->global), ctx);
    }
    if (rsrc_obj->socket_state & SocketStateListening) {
        return make_error_tuple(posix_errno_to_term(EOPNOTSUPP, ctx->global), ctx);
    }
#endif

#if OTP_SOCKET_BSD
    if (otp_socket_platform_supports_peek()) {
        return nif_socket_recv_with_peek(ctx, rsrc_obj, len, is_recvfrom);
    } else {
        return nif_socket_recv_without_peek(ctx, rsrc_obj, len, is_recvfrom);
    }
#elif OTP_SOCKET_LWIP
    return nif_socket_recv_lwip(ctx, rsrc_obj, len, is_recvfrom);
#endif
}

static term nif_socket_recv(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    TRACE("nif_socket_recv\n");
    return nif_socket_recv_internal(ctx, argv, false);
}

static term nif_socket_recvfrom(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    TRACE("nif_socket_recvfrom\n");
    return nif_socket_recv_internal(ctx, argv, true);
}

//
// send/sendto
//
ssize_t socket_send(struct SocketResource *rsrc_obj, const uint8_t *buf, size_t len, term dest)
{
    ssize_t sent_data = -1;
#if OTP_SOCKET_BSD
    if (!term_is_invalid_term(dest)) {
        struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
        GlobalContext *global = rsrc_refc->resource_type->global;

        struct sockaddr_in destaddr;
        memset(&destaddr, 0, sizeof(destaddr));
        destaddr.sin_family = AF_INET;

        term port = interop_kv_get_value_default(dest, port_atom, term_from_int(0), global);
        destaddr.sin_port = htons(term_to_int(port));
        term addr = interop_kv_get_value(dest, addr_atom, global);
        if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
            destaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            destaddr.sin_addr.s_addr = htonl(inet_addr4_to_uint32(addr));
        }

        sent_data = sendto(rsrc_obj->fd, buf, len, 0, (struct sockaddr *) &destaddr, sizeof(destaddr));

    } else {
        sent_data = send(rsrc_obj->fd, buf, len, 0);
    }
    if (sent_data == 0) {
        return SocketClosed;
    }
    if (sent_data < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return SocketWouldBlock;
        }
        return SocketOtherError;
    }
    return sent_data;
#elif OTP_SOCKET_LWIP
    err_t err;
    ip_addr_t ip_addr;
    uint16_t port_u16;
    if (!term_is_invalid_term(dest)) {
        struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
        GlobalContext *global = rsrc_refc->resource_type->global;

        term port_term = interop_kv_get_value_default(dest, port_atom, term_from_int(0), global);
        avm_int_t port_number = term_to_int(port_term);
        if (port_number < 0 || port_number > 65535) {
            return SocketOtherError;
        }
        port_u16 = (uint16_t) port_number;
        term addr = interop_kv_get_value(dest, addr_atom, global);
        if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
            ip_addr_set_loopback(false, &ip_addr);
        } else {
            ip_addr_set_ip4_u32(&ip_addr, htonl(inet_addr4_to_uint32(addr)));
        }
    }

    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateUDP) {
        struct pbuf *p = pbuf_alloc(PBUF_TRANSPORT, len, PBUF_RAM);
        uint8_t *bytes = (uint8_t *) p->payload;
        memcpy(bytes, buf, len);
        if (!term_is_invalid_term(dest)) {
            err = udp_sendto(rsrc_obj->udp_pcb, p, &ip_addr, port_u16);
        } else {
            err = udp_send(rsrc_obj->udp_pcb, p);
        }
        if (err == ERR_OK) {
            sent_data = len;
        }
        pbuf_free(p);
    } else {
        // If the socket is connection-mode, dest_addr shall be ignored.
        // Because we are copying, we cannot really send tcp_sndbuf(rsrc_obj->tcp_pcb) at once
        size_t buflen = MIN(len, TCP_MSS);
        int flags = TCP_WRITE_FLAG_COPY;
        if (buflen < len) {
            flags |= TCP_WRITE_FLAG_MORE;
        }
        err = tcp_write(rsrc_obj->tcp_pcb, buf, buflen, flags);
        if (err == ERR_MEM) {
            sent_data = 0;
        } else {
            if (err == ERR_OK) {
                err = tcp_output(rsrc_obj->tcp_pcb);
                if (err == ERR_OK) {
                    sent_data = buflen;
                }
            }
        }
    }
    LWIP_END();
    if (err == ERR_CLSD) {
        return SocketClosed;
    }
    if (sent_data == 0) {
        return SocketWouldBlock;
    }
    if (err != ERR_OK) {
        return SocketOtherError;
    }
    return sent_data;
#endif
}

static term nif_socket_send_internal(Context *ctx, int argc, term argv[], bool is_sendto)
{
    TRACE("nif_socket_send_internal\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);
    VALIDATE_VALUE(argv[1], term_is_binary);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    if (rsrc_obj->socket_state & SocketStateListening) {
        return make_error_tuple(posix_errno_to_term(EOPNOTSUPP, global), ctx);
    }
#endif

    term data = argv[1];
    term dest = term_invalid_term();
    if (is_sendto) {
        dest = argv[2];
    }

    const uint8_t *buf = (const uint8_t *) term_binary_data(data);
    size_t len = term_binary_size(data);

    ssize_t sent_data = socket_send(rsrc_obj, buf, len, dest);

    // {ok, RestData} | {error, Reason}

    size_t rest_len = len - sent_data;
    if (rest_len == 0) {
        return OK_ATOM;
    } else if (sent_data > 0) {

        size_t requested_size = term_sub_binary_heap_size(data, rest_len);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) + requested_size, 1, &data, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term rest = term_maybe_create_sub_binary(data, sent_data, rest_len, &ctx->heap, ctx->global);
        return port_create_tuple2(ctx, OK_ATOM, rest);

    } else if (sent_data == 0) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &data, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return port_create_tuple2(ctx, OK_ATOM, data);
    } else {
        AVM_LOGE(TAG, "Unable to send data: res=%zi.", sent_data);
        return make_error_tuple(CLOSED_ATOM, ctx);
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

#if OTP_SOCKET_LWIP
static void trap_answer_ok(struct LWIPEvent *event)
{
    Context *target_ctx = globalcontext_get_process_lock(event->trap_answer_ok.global, event->trap_answer_ok.target_pid);
    if (target_ctx) {
        mailbox_send_term_signal(target_ctx, TrapAnswerSignal, OK_ATOM);
        globalcontext_get_process_unlock(event->trap_answer_ok.global, target_ctx);
    }
}

static void trap_answer_closed(struct LWIPEvent *event)
{
    BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap);
    {
        term result = term_alloc_tuple(2, &heap);
        term_put_tuple_element(result, 0, ERROR_ATOM);
        // TODO: We may want to interpret err
        term_put_tuple_element(result, 1, CLOSED_ATOM);
        Context *target_ctx = globalcontext_get_process_lock(event->trap_answer_closed.global, event->trap_answer_closed.target_pid);
        if (target_ctx) {
            mailbox_send_term_signal(target_ctx, TrapAnswerSignal, result);
            globalcontext_get_process_unlock(event->trap_answer_closed.global, target_ctx);
        }
    }
    END_WITH_STACK_HEAP(heap, event->trap_answer_closed.global);
}

static err_t tcp_connected_cb(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    UNUSED(tpcb);

    struct SocketResource *rsrc_obj = (struct SocketResource *) arg;
    struct RefcBinary *rsrc_refc = refc_binary_from_data(rsrc_obj);
    GlobalContext *global = rsrc_refc->resource_type->global;
    int32_t target_pid = rsrc_obj->selecting_process_id;
    rsrc_obj->selecting_process_id = INVALID_PROCESS_ID;
    rsrc_obj->socket_state = SocketStateTCPConnected;
    if (target_pid != INVALID_PROCESS_ID) {
        if (err == ERR_OK) {
            struct LWIPEvent event;
            event.handler = trap_answer_ok;
            event.trap_answer_ok.global = global;
            event.trap_answer_ok.target_pid = target_pid;
            otp_socket_lwip_enqueue(&event);
        } else {
            struct LWIPEvent event;
            event.handler = trap_answer_closed;
            event.trap_answer_closed.global = global;
            event.trap_answer_closed.target_pid = target_pid;
            otp_socket_lwip_enqueue(&event);
        }
    } // else: sender died
    return ERR_OK;
}
#endif

static term nif_socket_connect(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_connect\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);
    VALIDATE_VALUE(argv[1], term_is_map);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term sockaddr = argv[1];
    term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
    term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
    if (term_is_invalid_term(addr)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    avm_int_t port_number = term_to_int(port);
    if (port_number < 0 || port_number > 65535) {
        RAISE_ERROR(BADARG_ATOM);
    }

#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }
    if (((rsrc_obj->socket_state & SocketStateTCPListening) == SocketStateTCPListening)
        || ((rsrc_obj->socket_state & SocketStateTCPConnected) == SocketStateTCPConnected)) {
        return make_error_tuple(posix_errno_to_term(EOPNOTSUPP, global), ctx);
    }
#endif

#if OTP_SOCKET_BSD
    struct sockaddr_in address;
    memset(&address, 0, sizeof(struct sockaddr_in));
    address.sin_family = AF_INET;

    address.sin_port = htons(port_number);

    if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
        address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else {
        // TODO more validation on addr tuple
        address.sin_addr.s_addr = htonl(inet_addr4_to_uint32(addr));
    }

    socklen_t addr_len = sizeof(struct sockaddr_in);
    int res = connect(rsrc_obj->fd, (const struct sockaddr *) &address, addr_len);
    if (res == -1) {
        if (errno == EINPROGRESS) {

            // TODO make connect non-blocking
            return UNDEFINED_ATOM;

        } else {
            AVM_LOGE(TAG, "Unable to connect: res=%i errno=%i", res, errno);
            return make_error_tuple(CLOSED_ATOM, ctx);
        }
    } else if (res == 0) {
        return OK_ATOM;
    } else {
        // won't happen according to connect(2)
        return UNDEFINED_ATOM;
    }
#elif OTP_SOCKET_LWIP
    ip_addr_t ip_addr;
    ip_addr_set_ip4_u32(&ip_addr, htonl(inet_addr4_to_uint32(addr)));
    err_t err;
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateUDP) {
        err = udp_connect(rsrc_obj->udp_pcb, &ip_addr, port_number);
    } else {
        err = tcp_connect(rsrc_obj->tcp_pcb, &ip_addr, port_number, tcp_connected_cb);
    }
    LWIP_END();

    if (UNLIKELY(err != ERR_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (rsrc_obj->socket_state & SocketStateUDP) {
        return OK_ATOM;
    } else {
        rsrc_obj->selecting_process_id = ctx->process_id;
        // Trap caller waiting for completion
        context_update_flags(ctx, ~NoFlags, Trap);
        return term_invalid_term();
    }
#endif
}

//
// shutdown
//

static term nif_socket_shutdown(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_shutdown\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_otp_socket);
    VALIDATE_VALUE(argv[1], term_is_atom);

    GlobalContext *global = ctx->global;

    struct SocketResource *rsrc_obj;
    if (UNLIKELY(!term_to_otp_socket(argv[0], &rsrc_obj, ctx))) {
        RAISE_ERROR(BADARG_ATOM);
    }

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

#if OTP_SOCKET_BSD
    if (rsrc_obj->fd == 0) {
#elif OTP_SOCKET_LWIP
    if (rsrc_obj->socket_state == SocketStateClosed) {
#endif
        return make_error_tuple(posix_errno_to_term(EBADF, global), ctx);
    }

    term result = OK_ATOM;

#if OTP_SOCKET_BSD
    int res = shutdown(rsrc_obj->fd, how);
    if (res < 0) {
        AVM_LOGE(TAG, "Unable to shut down socket: res=%i errno=%i", res, errno);
        return make_errno_tuple(ctx);
    }
#elif OTP_SOCKET_LWIP
    LWIP_BEGIN();
    if (rsrc_obj->socket_state & SocketStateTCP) {
        err_t res = tcp_shutdown(rsrc_obj->tcp_pcb, how != SHUT_WR, how != SHUT_RD);
        if (how == SHUT_RDWR && res == ERR_OK) {
            rsrc_obj->tcp_pcb = NULL;
        }
        if (res != ERR_OK) {
            AVM_LOGE(TAG, "Unable to shut down socket: res=%i", res);
            result = make_lwip_err_tuple(res, ctx);
        }
    }
    LWIP_END();
#endif
    return result;
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
        if (strcmp("nif_select_read/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_select_read_nif;
        }
        if (strcmp("nif_accept/1", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_accept_nif;
        }
        if (strcmp("nif_recv/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &socket_recv_nif;
        }
        if (strcmp("nif_recvfrom/2", rest) == 0) {
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
