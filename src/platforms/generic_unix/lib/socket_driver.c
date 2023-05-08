/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
 * Copyright 2019 Fred Dushin <fred@dushin.net>
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

#include "socket_driver.h"
#include "atom.h"
#include "context.h"
#include "generic_unix_sys.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "port.h"
#include "term.h"
#include "utils.h"
#include <limits.h>
#include <netinet/tcp.h>
#include <stdbool.h>
#include <string.h>

#include "platform_defaultatoms.h"
#include "scheduler.h"
#include "sys.h"
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
// FreeBSD 12 bug, sys/types must be included before netinet headers
#include <netinet/in.h>
#include <netinet/udp.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

// #define ENABLE_TRACE
#include "trace.h"

#define BUFSIZE 128

typedef struct ActiveRecvListener
{
    EventListener base;
    int32_t process_id;
    size_t buf_size;
} ActiveRecvListener;

typedef struct PassiveRecvListener
{
    EventListener base;
    int32_t process_id;
    term pid;
    size_t length;
    size_t buffer;
    term controlling_process;
    uint64_t ref_ticks;
} PassiveRecvListener;

typedef struct SocketDriverData
{
    int sockfd;
    term proto;
    uint16_t port;
    term controlling_process;
    bool binary;
    bool active;
    size_t buffer;
    ActiveRecvListener *active_listener;
    PassiveRecvListener *passive_listener;
} SocketDriverData;

// TODO define in defaultatoms
const char *const init_a = ATOM_STR("\x4", "init");
const char *const get_port_a = ATOM_STR("\x8", "get_port");
const char *const sockname_a = ATOM_STR("\x8", "sockname");
const char *const peername_a = ATOM_STR("\x8", "peername");
const char *const not_owner_a = ATOM_STR("\x9", "not_owner");

const char *const close_internal = ATOM_STR("\x14", "$atomvm_socket_close");

static EventListener *active_recv_callback(GlobalContext *glb, EventListener *listener);
static EventListener *passive_recv_callback(GlobalContext *glb, EventListener *listener);
static EventListener *active_recvfrom_callback(GlobalContext *glb, EventListener *listener);
static EventListener *passive_recvfrom_callback(GlobalContext *glb, EventListener *listener);
static NativeHandlerResult socket_driver_do_init(Context *ctx, term pid, term ref, term params);
static void socket_driver_do_send(Context *ctx, term pid, term ref, term data);
static void socket_driver_do_sendto(Context *ctx, term pid, term ref, term dest_address, term dest_port, term data);
static void socket_driver_do_recv(Context *ctx, term pid, term ref, term length, term timeout);
static void socket_driver_do_recvfrom(Context *ctx, term pid, term ref, term length, term timeout);
static void socket_driver_do_close(Context *ctx);
static void socket_driver_get_port(Context *ctx, term pid, term ref);
static void socket_driver_do_accept(Context *ctx, term pid, term ref, term timeout);
static void socket_driver_sockname(Context *ctx, term pid, term ref);
static void socket_driver_peername(Context *ctx, term pid, term ref);
static NativeHandlerResult socket_consume_mailbox(Context *ctx);

uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

#define SOCKET_INET_ADDR TUPLE_SIZE(4)

term socket_ctx_tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

term socket_heap_tuple_from_addr(term **heap_ptr, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_heap_create_tuple_n(heap_ptr, 4, terms);
}

term socket_heap_create_packet_term(term **heap_ptr, const char *buf, ssize_t len, int is_binary)
{
    if (is_binary) {
        return term_heap_from_literal_binary((void *) buf, len, heap_ptr);
    } else {
        return term_heap_from_string((const uint8_t *) buf, len, heap_ptr);
    }
}

void *socket_driver_create_data()
{
    struct SocketDriverData *data = calloc(1, sizeof(struct SocketDriverData));
    data->sockfd = -1;
    data->proto = term_invalid_term();
    data->port = 0;
    data->controlling_process = term_invalid_term();
    data->binary = false;
    data->active = true;
    data->buffer = 512;
    data->active_listener = NULL;
    data->passive_listener = NULL;
    return (void *) data;
}

void socket_driver_delete_data(void *data)
{
    free(data);
}

/**
 * @brief Bind a socket
 * @details This function binds a socket. It sends an error reply to the caller
 * if an error occurs, but doesn't send OK if it succeeds.
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param params initialization socket parameters with address and port
 * @return 0 on success
 */
static int do_bind(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    term address = interop_proplist_get_value_default(params, ADDRESS_ATOM, UNDEFINED_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    struct sockaddr_in serveraddr;
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    if (address == UNDEFINED_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (term_is_tuple(address)) {
        serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(address));
    } else {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    if (!term_is_integer(port)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    avm_int_t p = term_to_int(port);
    serveraddr.sin_port = htons(p);
    socklen_t address_len = sizeof(serveraddr);
    if (bind(socket_data->sockfd, (struct sockaddr *) &serveraddr, address_len) == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, BIND_ATOM, errno);
        return -1;
    }
    if (getsockname(socket_data->sockfd, (struct sockaddr *) &serveraddr, &address_len) == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, GETSOCKNAME_ATOM, errno);
        return -1;
    }
    socket_data->port = ntohs(serveraddr.sin_port);
    TRACE("socket_driver|do_bind: bound to %ld\n", socket_data->port);
    return 0;
}

/**
 * @brief Initialize a UDP socket
 * @details This function initializes a UDP socket and sends the reply to
 * the caller
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param socket_data current socket data, updated with fd and listener
 * @param params UDP initialization socket parameters with address and port
 * @return NativeContinue or NativeTerminate if the driver should terminate
 */
static NativeHandlerResult init_udp_socket(Context *ctx, term pid, term ref, SocketDriverData *socket_data, term params)
{
    GlobalContext *glb = ctx->global;

    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, SOCKET_ATOM, errno);
        return NativeTerminate;
    }
    socket_data->sockfd = sockfd;
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1) {
        close(sockfd);
        port_send_sys_error_tuple(ctx, pid, ref, FCNTL_ATOM, errno);
        return NativeTerminate;
    }
    if (do_bind(ctx, pid, ref, params)) {
        close(sockfd);
        return NativeTerminate;
    }
    if (socket_data->active) {
        ActiveRecvListener *listener = malloc(sizeof(ActiveRecvListener));
        if (IS_NULL_PTR(listener)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            AVM_ABORT();
        }
        listener->base.fd = socket_data->sockfd;
        listener->base.handler = active_recvfrom_callback;
        listener->buf_size = socket_data->buffer;
        listener->process_id = ctx->process_id;
        sys_register_listener(glb, &listener->base);
        socket_data->active_listener = listener;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
    return NativeContinue;
}

/**
 * @brief Connect a TCP socket
 * @details This function connects a TCP socket. It sends an error reply to the
 * caller if an error occurs, but doesn't send OK if it succeeds.
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param params UDP initialization socket parameters with address and port
 * @return 0 on success, -1 on failure
 */
static int do_connect(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    int sockfd = socket_data->sockfd;
    term address = interop_proplist_get_value(params, ADDRESS_ATOM);
    if (!term_is_list(address)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    term port = interop_proplist_get_value(params, PORT_ATOM);
    if (!term_is_integer(port)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }

    // TODO handle IP addresses
    if (!term_is_list(address)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    int ok;
    char *addr_str = interop_term_to_string(address, &ok);
    if (!ok) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    char port_str[32];
    snprintf(port_str, 32, "%u", (unsigned short) term_to_int(port));
    TRACE("socket_driver:do_connect: resolving to %s:%s over socket fd %i\n", addr_str, port_str, sockfd);

    struct addrinfo *server_info;
    int status = getaddrinfo(addr_str, port_str, &hints, &server_info);

    free(addr_str);

    if (status != 0) {
        port_send_sys_error_tuple(ctx, pid, ref, GETADDRINFO_ATOM, status);
        return -1;
    }

    struct sockaddr *addr = NULL;
    size_t addr_len = 0;
    for (struct addrinfo *p = server_info; p != NULL; p = p->ai_next) {
        addr = p->ai_addr;
        addr_len = p->ai_addrlen;
        break;
    }
    if (IS_NULL_PTR(addr)) {
        port_send_error_tuple(ctx, pid, ref, NO_SUCH_HOST_ATOM);
        return -1;
    }

    status = connect(sockfd, addr, addr_len);

    freeaddrinfo(server_info);

    if (status == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, CONNECT_ATOM, errno);
        return -1;
    }
    TRACE("socket_driver|do_connect: connected.\n");
    return 0;
}

/**
 * @brief Initialize context as a client TCP socket
 * @details This function initializes and connects the TCP socket and sends the
 * reply to the caller. Socket data is updated with fd and listener.
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param params UDP initialization socket parameters with address and port
 * @return NativeContinue or NativeTerminate if the driver should terminate
 */
static NativeHandlerResult init_client_tcp_socket(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    GlobalContext *glb = ctx->global;

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, SOCKET_ATOM, errno);
        return NativeTerminate;
    }
    socket_data->sockfd = sockfd;
    if (do_connect(ctx, pid, ref, params)) {
        close(sockfd);
        return NativeTerminate;
    }
    if (socket_data->active) {
        ActiveRecvListener *listener = malloc(sizeof(ActiveRecvListener));
        if (IS_NULL_PTR(listener)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            AVM_ABORT();
        }
        listener->base.fd = socket_data->sockfd;
        listener->base.handler = active_recv_callback;
        listener->buf_size = socket_data->buffer;
        listener->process_id = ctx->process_id;
        sys_register_listener(glb, &listener->base);
        socket_data->active_listener = listener;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
    return NativeContinue;
}

/**
 * @brief Configure a tcp socket for listening
 * @details This function configures the TCP socket for listening. It sends an
 * error reply to the caller if an error occurs, but doesn't send OK if it
 * succeeds.
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param params TCP parameters with backlog
 * @return 0 on success, -1 on failure
 */
static int do_listen(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    term backlog = interop_proplist_get_value(params, BACKLOG_ATOM);
    if (!term_is_integer(backlog)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return -1;
    }
    if (listen(socket_data->sockfd, term_from_int(backlog)) == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, LISTEN_ATOM, errno);
        return -1;
    }
    return 0;
}

/**
 * @brief Initialize context as a server TCP socket
 * @details This function initializes and binds the TCP socket and sends the
 * reply to the caller. Socket data is updated with fd.
 * @param ctx the driver context
 * @param pid the caller's pid, for the reply
 * @param ref the caller's message ref, to tag the reply
 * @param params TCP initialization socket parameters with address and port
 * @return NativeContinue or NativeTerminate if the driver should terminate
 */
static NativeHandlerResult init_server_tcp_socket(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, SOCKET_ATOM, errno);
        return NativeTerminate;
    }
    socket_data->sockfd = sockfd;

    //
    // set socket options:
    //      reuse-address
    //      disable linger
    //
    int flag = 1;
    setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char *) &flag, sizeof(int));
    struct linger sl;
    sl.l_onoff = 1;
    sl.l_linger = 0;
    setsockopt(sockfd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));

    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1) {
        close(sockfd);
        port_send_sys_error_tuple(ctx, pid, ref, FCNTL_ATOM, errno);
        return NativeTerminate;
    }
    if (do_bind(ctx, pid, ref, params)) {
        close(sockfd);
        return NativeTerminate;
    }
    if (do_listen(ctx, pid, ref, params)) {
        close(sockfd);
        return NativeTerminate;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
    TRACE("socket_driver|init_server_tcp_socket: listening on port %u\n", (unsigned) term_to_int(port));
    return NativeContinue;
}

static Context *create_accepting_socket(GlobalContext *glb, SocketDriverData *new_socket_data)
{
    Context *new_ctx = context_new(glb);
    new_ctx->native_handler = socket_consume_mailbox;
    new_ctx->platform_data = new_socket_data;
    return new_ctx;
}

static ActiveRecvListener *create_accepting_socket_listener(Context *ctx, SocketDriverData *socket_data)
{
    ActiveRecvListener *listener = malloc(sizeof(ActiveRecvListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    listener->base.fd = socket_data->sockfd;
    listener->base.handler = active_recv_callback;
    listener->buf_size = socket_data->buffer;
    listener->process_id = ctx->process_id;
    socket_data->active_listener = listener;
    return listener;
}

static NativeHandlerResult socket_driver_do_init(Context *ctx, term pid, term ref, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (!term_is_list(params)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    term proto = interop_proplist_get_value(params, PROTO_ATOM);
    if (term_is_nil(proto)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    socket_data->proto = proto;
    //
    // get the controlling process
    //
    term controlling_process = interop_proplist_get_value_default(params, CONTROLLING_PROCESS_ATOM, term_invalid_term());
    if (!(term_is_invalid_term(controlling_process) || term_is_pid(controlling_process))) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    socket_data->controlling_process = controlling_process;
    //
    // get the binary flag
    //
    term binary = interop_proplist_get_value_default(params, BINARY_ATOM, FALSE_ATOM);
    if (!(binary == TRUE_ATOM || binary == FALSE_ATOM)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    socket_data->binary = binary == TRUE_ATOM;
    //
    // get the buffer size
    //
    term buffer = interop_proplist_get_value_default(params, BUFFER_ATOM, term_from_int(BUFSIZE));
    if (!term_is_integer(buffer)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    avm_int_t buffer_val = term_to_int(buffer);
#if AVM_INT_MAX > SIZE_MAX
    if (buffer_val > SIZE_MAX) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
#endif
    socket_data->buffer = (size_t) buffer_val;
    //
    // get the active flag
    //
    term active = interop_proplist_get_value_default(params, ACTIVE_ATOM, FALSE_ATOM);
    if (!(active == TRUE_ATOM || active == FALSE_ATOM)) {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
    socket_data->active = active == TRUE_ATOM;
    //
    // initialize based on specified protocol and action
    //
    if (proto == UDP_ATOM) {
        return init_udp_socket(ctx, pid, ref, socket_data, params);
    } else if (proto == TCP_ATOM) {
        term connect = interop_proplist_get_value_default(params, CONNECT_ATOM, FALSE_ATOM);
        if (connect == TRUE_ATOM) {
            return init_client_tcp_socket(ctx, pid, ref, params);
        } else {
            term listen = interop_proplist_get_value_default(params, LISTEN_ATOM, FALSE_ATOM);
            if (listen == TRUE_ATOM) {
                return init_server_tcp_socket(ctx, pid, ref, params);
            } else {
                port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
                return NativeTerminate;
            }
        }
    } else {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NativeTerminate;
    }
}

static void socket_driver_do_close(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (close(socket_data->sockfd) == -1) {
        TRACE("socket_driver|socket_driver_do_close: close failed");
    } else {
        TRACE("socket_driver|socket_driver_do_close: closed socket\n");
    }
}

static void socket_driver_controlling_process(Context *ctx, term pid, term ref, term new_pid_term)
{
    struct SocketDriverData *socket_data = ctx->platform_data;
    term reply;

    if (UNLIKELY(!term_is_pid(new_pid_term))) {
        port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + PORT_ERROR_TUPLE_SIZE, 1, &ref, MEMORY_NO_SHRINK);
        reply = port_create_error_tuple(ctx, BADARG_ATOM);
    } else if (UNLIKELY(pid != socket_data->controlling_process)) {
        port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + PORT_ERROR_TUPLE_SIZE, 1, &ref, MEMORY_NO_SHRINK);
        reply = port_create_error_tuple(ctx, globalcontext_make_atom(ctx->global, not_owner_a));
    } else {
        port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE, 1, &ref, MEMORY_NO_SHRINK);
        socket_data->controlling_process = new_pid_term;
        reply = OK_ATOM;
    }
    port_send_reply(ctx, pid, ref, reply);
}

//
// INET API
//

void socket_driver_get_port(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    port_send_ok_tuple(ctx, pid, ref, term_from_int(socket_data->port));
}

void socket_driver_send_name_reply(Context *ctx, term pid, term ref, struct sockaddr_in *addr)
{
    port_ensure_available_with_roots(ctx, PORT_REPLY_SIZE + SOCKET_INET_ADDR + TUPLE_SIZE(2) + PORT_OK_TUPLE_SIZE, 1, &ref, MEMORY_NO_SHRINK);
    term addr_term = socket_ctx_tuple_from_addr(ctx, ntohl(addr->sin_addr.s_addr));
    term port_term = term_from_int(ntohs(addr->sin_port));
    term addr_port = port_create_tuple2(ctx, addr_term, port_term);
    port_send_ok_tuple(ctx, pid, ref, addr_port);
}

static void socket_driver_sockname(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    if (getsockname(socket_data->sockfd, (struct sockaddr *) &addr, &addrlen) == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, GETSOCKNAME_ATOM, errno);
    }
    socket_driver_send_name_reply(ctx, pid, ref, &addr);
}

static void socket_driver_peername(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    if (getpeername(socket_data->sockfd, (struct sockaddr *) &addr, &addrlen) == -1) {
        port_send_sys_error_tuple(ctx, pid, ref, GETPEERNAME_ATOM, errno);
    }
    socket_driver_send_name_reply(ctx, pid, ref, &addr);
}

//
// send operations
//

/**
 * @brief get buffer data from provided term
 * @details return or allocate buffer data. Sends an error message to the caller
 * if it fails. The buffer may be allocated, `socket_driver_send_free_data`
 * must be called to free it.
 * @param ctx the current context
 * @param pid the caller's pid
 * @param ref the message ref
 * @param data the data to get a buffer from
 * @param len on output, length of data
 * @returns NULL if it failed
 */
static char *socket_driver_send_get_data(Context *ctx, term pid, term ref, term data, size_t *len)
{
    char *buf;
    if (term_is_binary(data)) {
        buf = (char *) term_binary_data(data);
        *len = term_binary_size(data);
    } else if (term_is_list(data)) {
        switch (interop_iolist_size(data, len)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                port_send_error_tuple(ctx, pid, ref, OUT_OF_MEMORY_ATOM);
                return NULL;
            case InteropBadArg:
                port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
                return NULL;
        }
        buf = malloc(*len);
        switch (interop_write_iolist(data, buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(buf);
                port_send_error_tuple(ctx, pid, ref, OUT_OF_MEMORY_ATOM);
                return NULL;
            case InteropBadArg:
                free(buf);
                port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
                return NULL;
        }
    } else {
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
        return NULL;
    }
    return buf;
}

/**
 * @brief free the buffer data that was allocated by
 * `socket_driver_send_get_data`
 * @param data the data `socket_driver_send_get_data` got buffer from
 * @param buf the result of `socket_driver_send_get_data`
 */
static void socket_driver_send_free_data(term data, char *buf)
{
    if (term_is_list(data)) {
        free(buf);
    }
}

/**
 * @brief send data to a TCP socket
 * @details return an error tuple or `{Ref, {ok, Len}}` to the caller
 * @param ctx the driver context
 * @param pid caller's pid
 * @param ref message ref, for the reply
 * @param data iolist or binary of the data to send
 */
static void socket_driver_do_send(Context *ctx, term pid, term ref, term data)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    char *buf;
    size_t len;

    buf = socket_driver_send_get_data(ctx, pid, ref, data, &len);
    if (buf) {
        int sent_data = send(socket_data->sockfd, buf, len, 0);
        socket_driver_send_free_data(data, buf);

        if (sent_data < 0) {
            port_send_sys_error_tuple(ctx, pid, ref, SEND_ATOM, errno);
        } else {
            TRACE("socket_driver_do_send: sent data with len %li to fd %i\n", len, socket_data->sockfd);
            port_send_ok_tuple(ctx, pid, ref, term_from_int(sent_data));
        }
    }
}

/**
 * @brief send data to a UDP socket
 * @details return an error tuple or `{Ref, {ok, Len}}` to the caller
 * @param ctx the driver context
 * @param pid caller's pid
 * @param ref message ref, for the reply
 * @param data iolist or binary of the data to send
 */
static void socket_driver_do_sendto(Context *ctx, term pid, term ref, term dest_address, term dest_port, term data)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    char *buf;
    size_t len;

    buf = socket_driver_send_get_data(ctx, pid, ref, data, &len);
    if (buf) {
        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(struct sockaddr_in));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(socket_tuple_to_addr(dest_address));
        addr.sin_port = htons(term_to_int32(dest_port));
        int sent_data = sendto(socket_data->sockfd, buf, len, 0, (struct sockaddr *) &addr, sizeof(addr));
        socket_driver_send_free_data(data, buf);

        if (sent_data < 0) {
            port_send_sys_error_tuple(ctx, pid, ref, SEND_ATOM, errno);
        } else {
            TRACE("socket_driver_do_sendto: sent data with len %li to fd %i\n", len, socket_data->sockfd);
            port_send_ok_tuple(ctx, pid, ref, term_from_int(sent_data));
        }
    }
}

//
// receive operations
//

static EventListener *active_recv_callback(GlobalContext *glb, EventListener *base_listener)
{
    ActiveRecvListener *listener = GET_LIST_ENTRY(base_listener, ActiveRecvListener, base);
    //
    // allocate the receive buffer
    //
    char *buf = malloc(listener->buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    //
    // receive the data
    //
    EventListener *result = base_listener;
    ssize_t len = recvfrom(listener->base.fd, buf, listener->buf_size, 0, NULL, NULL);
    Context *ctx = globalcontext_get_process_lock(glb, listener->process_id);
    if (UNLIKELY(ctx == NULL)) {
        free(listener);
        free(buf);
        return NULL;
    }
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (len <= 0) {
        // {tcp, Socket, {error, {SysCall, Errno}}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = socket_data->controlling_process;
        term msgs[2] = { TCP_CLOSED_ATOM, term_from_local_process_id(ctx->process_id) };
        term msg = port_heap_create_tuple_n(&heap_ptr, 2, msgs);
        port_send_message_nolock(glb, pid, msg);
        socket_data->active_listener = NULL;
        mailbox_send(ctx, globalcontext_make_atom(glb, close_internal));
        free(listener);
        result = NULL;
    } else {
        TRACE("socket_driver|active_recv_callback: received data of len %li from fd %i\n", len, socket_data->sockfd);
        int ensure_packet_avail;
        if (socket_data->binary) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        // {tcp, pid, binary}
        term heap[20 + ensure_packet_avail];
        term *heap_ptr = heap;
        term pid = socket_data->controlling_process;
        term packet = socket_heap_create_packet_term(&heap_ptr, buf, len, socket_data->binary);
        term msgs[3] = { TCP_ATOM, term_from_local_process_id(ctx->process_id), packet };
        term msg = port_heap_create_tuple_n(&heap_ptr, 3, msgs);
        port_send_message_nolock(glb, pid, msg);
    }
    globalcontext_get_process_unlock(glb, ctx);
    free(buf);
    return result;
}

static EventListener *passive_recv_callback(GlobalContext *glb, EventListener *base_listener)
{
    PassiveRecvListener *listener = GET_LIST_ENTRY(base_listener, PassiveRecvListener, base);

    //
    // allocate the receive buffer
    //
    size_t buf_size = listener->length;
    int flags = MSG_WAITALL;
    if (buf_size == 0) {
        buf_size = listener->buffer;
        flags = 0;
    }
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    //
    // receive the data
    //
    ssize_t len = recvfrom(listener->base.fd, buf, buf_size, flags, NULL, NULL);
    Context *ctx = globalcontext_get_process_lock(glb, listener->process_id);
    if (UNLIKELY(ctx == NULL)) {
        free(listener);
        free(buf);
        return NULL;
    }
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (len == 0) {
        // {Ref, {error, closed}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term reply = port_heap_create_reply(&heap_ptr, ref, port_heap_create_error_tuple(&heap_ptr, CLOSED_ATOM));
        port_send_message_nolock(glb, pid, reply);
        mailbox_send(ctx, globalcontext_make_atom(glb, close_internal));
    } else if (len < 0) {
        // {Ref, {error, {SysCall, Errno}}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term reply = port_heap_create_reply(&heap_ptr, ref, port_heap_create_sys_error_tuple(&heap_ptr, RECV_ATOM, errno));
        port_send_message_nolock(glb, pid, reply);
        mailbox_send(ctx, globalcontext_make_atom(glb, close_internal));
    } else {
        TRACE("socket_driver|passive_recv_callback: passive received data of len: %li\n", len);
        int ensure_packet_avail;
        if (socket_data->binary) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        term heap[20 + ensure_packet_avail];
        term *heap_ptr = heap;
        // {Ref, {ok, Packet::binary()}}
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term packet = socket_heap_create_packet_term(&heap_ptr, buf, len, socket_data->binary);
        term payload = port_heap_create_ok_tuple(&heap_ptr, packet);
        term reply = port_heap_create_reply(&heap_ptr, ref, payload);
        port_send_message_nolock(glb, pid, reply);
    }
    socket_data->passive_listener = NULL;
    globalcontext_get_process_unlock(glb, ctx);
    //
    // remove the EventListener from the global list and clean up
    //
    free(listener);
    free(buf);
    return NULL;
}

static EventListener *active_recvfrom_callback(GlobalContext *glb, EventListener *base_listener)
{
    ActiveRecvListener *listener = GET_LIST_ENTRY(base_listener, ActiveRecvListener, base);
    //
    // allocate the receive buffer
    //
    char *buf = malloc(listener->buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    //
    // receive the data
    //
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    ssize_t len = recvfrom(listener->base.fd, buf, listener->buf_size, 0, (struct sockaddr *) &clientaddr, &clientlen);
    Context *ctx = globalcontext_get_process_lock(glb, listener->process_id);
    if (UNLIKELY(ctx == NULL)) {
        free(listener);
        free(buf);
        return NULL;
    }
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (len == -1) {
        // {udp, Socket, {error, {SysCall, Errno}}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = socket_data->controlling_process;
        term msgs[3] = { UDP_ATOM, term_from_local_process_id(ctx->process_id), port_heap_create_sys_error_tuple(&heap_ptr, RECVFROM_ATOM, errno) };
        term msg = port_heap_create_tuple_n(&heap_ptr, 3, msgs);
        port_send_message_nolock(glb, pid, msg);
        // Not closing the listener here as there is no connection to close.
    } else {
        int ensure_packet_avail;
        if (socket_data->binary) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        // {udp, pid, {int,int,int,int}, int, binary}
        term heap[20 + ensure_packet_avail];
        term *heap_ptr = heap;
        term pid = socket_data->controlling_process;
        term addr = socket_heap_tuple_from_addr(&heap_ptr, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_heap_create_packet_term(&heap_ptr, buf, len, socket_data->binary);
        term msgs[5] = { UDP_ATOM, term_from_local_process_id(ctx->process_id), addr, port, packet };
        term msg = port_heap_create_tuple_n(&heap_ptr, 5, msgs);
        port_send_message_nolock(glb, pid, msg);
    }
    globalcontext_get_process_unlock(glb, ctx);
    free(buf);
    return base_listener;
}

static EventListener *passive_recvfrom_callback(GlobalContext *glb, EventListener *base_listener)
{
    PassiveRecvListener *listener = GET_LIST_ENTRY(base_listener, PassiveRecvListener, base);

    //
    // allocate the receive buffer
    //
    size_t buf_size = listener->length;
    int flags = MSG_WAITALL;
    if (buf_size == 0) {
        buf_size = listener->buffer;
        flags = 0;
    }
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    //
    // receive the data
    //
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    ssize_t len = recvfrom(listener->base.fd, buf, buf_size, flags, (struct sockaddr *) &clientaddr, &clientlen);
    Context *ctx = globalcontext_get_process_lock(glb, listener->process_id);
    if (UNLIKELY(ctx == NULL)) {
        free(listener);
        free(buf);
        return NULL;
    }
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (len == -1) {
        // {Ref, {error, {SysCall, Errno}}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term reply = port_heap_create_reply(&heap_ptr, ref, port_heap_create_sys_error_tuple(&heap_ptr, RECVFROM_ATOM, errno));
        port_send_message_nolock(glb, pid, reply);
    } else {
        int ensure_packet_avail;
        if (socket_data->binary) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        // {Ref, {ok, {{int,int,int,int}, int, binary}}}
        term heap[20 + ensure_packet_avail];
        term *heap_ptr = heap;
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term addr = socket_heap_tuple_from_addr(&heap_ptr, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_heap_create_packet_term(&heap_ptr, buf, len, socket_data->binary);
        term addr_port_packet = port_heap_create_tuple3(&heap_ptr, addr, port, packet);
        term payload = port_heap_create_ok_tuple(&heap_ptr, addr_port_packet);
        term reply = port_heap_create_reply(&heap_ptr, ref, payload);
        port_send_message_nolock(glb, pid, reply);
    }
    socket_data->passive_listener = NULL;
    globalcontext_get_process_unlock(glb, ctx);
    //
    // remove the EventListener from the global list and clean up
    //
    free(listener);
    free(buf);
    return NULL;
}

static void do_recv(Context *ctx, term pid, term ref, term length, term timeout, event_handler_t handler)
{
    UNUSED(timeout);

    GlobalContext *glb = ctx->global;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // The socket must be in active mode
    //
    if (socket_data->active) {
        port_ensure_available(ctx, 12);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        return;
    }
    //
    // Create an event listener with request-specific data, and append to the global list
    //
    PassiveRecvListener *listener = malloc(sizeof(PassiveRecvListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    listener->base.fd = socket_data->sockfd;
    listener->base.handler = handler;
    listener->process_id = ctx->process_id;
    listener->pid = pid;
    listener->length = term_to_int(length);
    listener->buffer = socket_data->buffer;
    listener->ref_ticks = term_to_ref_ticks(ref);
    sys_register_listener(glb, &listener->base);
    socket_data->passive_listener = listener;
}

static inline void socket_driver_do_recvfrom(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recvfrom_callback);
}

static inline void socket_driver_do_recv(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recv_callback);
}

//
// accept
//

static EventListener *accept_callback(GlobalContext *glb, EventListener *base_listener)
{
    PassiveRecvListener *listener = GET_LIST_ENTRY(base_listener, PassiveRecvListener, base);

    //
    // accept the connection
    //
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(listener->base.fd, (struct sockaddr *) &clientaddr, &clientlen);
    Context *ctx = globalcontext_get_process_lock(glb, listener->process_id);
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    EventListener *result = NULL;
    if (fd == -1) {
        // {Ref, {error, {SysCall, Errno}}}
        term heap[12];
        term *heap_ptr = heap;
        term pid = listener->pid;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term reply = port_heap_create_reply(&heap_ptr, ref, port_heap_create_sys_error_tuple(&heap_ptr, ACCEPT_ATOM, errno));
        port_send_message_nolock(glb, pid, reply);
    } else {
        TRACE("socket_driver|accept_callback: accepted connection.  fd: %i\n", fd);

        term pid = listener->pid;
        SocketDriverData *new_socket_data = socket_driver_create_data();
        new_socket_data->sockfd = fd;
        new_socket_data->proto = socket_data->proto;
        new_socket_data->active = socket_data->active;
        new_socket_data->binary = socket_data->binary;
        new_socket_data->buffer = socket_data->buffer;
        new_socket_data->controlling_process = pid;

        globalcontext_get_process_unlock(glb, ctx);
        Context *new_ctx = create_accepting_socket(glb, new_socket_data);
        ctx = globalcontext_get_process_lock(glb, listener->process_id);
        if (UNLIKELY(ctx == NULL)) {
            socket_data->passive_listener = NULL;
            free(listener);
            return NULL;
        }
        if (new_socket_data->active) {
            result = &create_accepting_socket_listener(new_ctx, new_socket_data)->base;
        }

        // {Ref, Socket}
        term socket_pid = term_from_local_process_id(new_ctx->process_id);
        term heap[10];
        term *heap_ptr = heap;
        term ref = term_heap_from_ref_ticks(listener->ref_ticks, &heap_ptr);
        term payload = port_heap_create_ok_tuple(&heap_ptr, socket_pid);
        term reply = port_heap_create_reply(&heap_ptr, ref, payload);
        port_send_message_nolock(glb, pid, reply);
    }
    socket_data->passive_listener = NULL;
    globalcontext_get_process_unlock(glb, ctx);
    //
    // remove the EventListener from the global list and clean up
    //
    free(listener);
    return result;
}

static void socket_driver_do_accept(Context *ctx, term pid, term ref, term timeout)
{
    UNUSED(timeout);

    GlobalContext *glb = ctx->global;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // Create an event listener with request-specific data, and append to the global list
    //
    PassiveRecvListener *listener = malloc(sizeof(PassiveRecvListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    listener->base.fd = socket_data->sockfd;
    listener->base.handler = accept_callback;
    listener->process_id = ctx->process_id;
    listener->pid = pid;
    listener->length = 0;
    listener->buffer = 0;
    listener->ref_ticks = term_to_ref_ticks(ref);
    sys_register_listener(glb, &listener->base);
    socket_data->passive_listener = listener;
}

static NativeHandlerResult socket_consume_mailbox(Context *ctx)
{
    TRACE("START socket_consume_mailbox\n");
    if (UNLIKELY(ctx->native_handler != socket_consume_mailbox)) {
        AVM_ABORT();
    }

    GlobalContext *glb = ctx->global;

    // Socket can be closed in another thread.
    Message *message = mailbox_first(&ctx->mailbox);
    term msg = message->message;
    if (msg == globalcontext_make_atom(glb, close_internal)) {
        socket_driver_do_close(ctx);
        // We don't need to remove message.
        return NativeTerminate;
    }
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    NativeHandlerResult result = NativeContinue;

    term cmd_name = term_get_tuple_element(cmd, 0);
    if (cmd_name == globalcontext_make_atom(glb, init_a)) {
        TRACE("init\n");
        term params = term_get_tuple_element(cmd, 1);
        result = socket_driver_do_init(ctx, pid, ref, params);
    } else if (cmd_name == SENDTO_ATOM) {
        TRACE("sendto\n");
        term dest_address = term_get_tuple_element(cmd, 1);
        term dest_port = term_get_tuple_element(cmd, 2);
        term buffer = term_get_tuple_element(cmd, 3);
        socket_driver_do_sendto(ctx, pid, ref, dest_address, dest_port, buffer);
    } else if (cmd_name == SEND_ATOM) {
        TRACE("send\n");
        term buffer = term_get_tuple_element(cmd, 1);
        socket_driver_do_send(ctx, pid, ref, buffer);
    } else if (cmd_name == RECVFROM_ATOM) {
        TRACE("recvfrom\n");
        term length = term_get_tuple_element(cmd, 1);
        term timeout = term_get_tuple_element(cmd, 2);
        socket_driver_do_recvfrom(ctx, pid, ref, length, timeout);
    } else if (cmd_name == RECV_ATOM) {
        TRACE("recv\n");
        term length = term_get_tuple_element(cmd, 1);
        term timeout = term_get_tuple_element(cmd, 2);
        socket_driver_do_recv(ctx, pid, ref, length, timeout);
    } else if (cmd_name == ACCEPT_ATOM) {
        TRACE("accept\n");
        term timeout = term_get_tuple_element(cmd, 1);
        socket_driver_do_accept(ctx, pid, ref, timeout);
    } else if (cmd_name == CLOSE_ATOM) {
        TRACE("close\n");
        port_send_reply(ctx, pid, ref, OK_ATOM);
        SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
        if (socket_data->active_listener) {
            sys_unregister_listener(glb, &socket_data->active_listener->base);
            free(socket_data->active_listener);
        }
        if (socket_data->passive_listener) {
            sys_unregister_listener(glb, &socket_data->passive_listener->base);
            free(socket_data->passive_listener);
        }
        socket_driver_do_close(ctx);
        result = NativeTerminate;
    } else if (cmd_name == globalcontext_make_atom(glb, sockname_a)) {
        TRACE("sockname\n");
        socket_driver_sockname(ctx, pid, ref);
    } else if (cmd_name == globalcontext_make_atom(glb, peername_a)) {
        TRACE("peername\n");
        socket_driver_peername(ctx, pid, ref);
    } else if (cmd_name == globalcontext_make_atom(glb, get_port_a)) {
        // TODO This function is not supported in the gen_tcp or gen_udp APIs.
        // It should be removed.  (Use inet:peername and inet:sockname instead)
        TRACE("get_port\n");
        socket_driver_get_port(ctx, pid, ref);
    } else if (cmd_name == CONTROLLING_PROCESS_ATOM) {
        TRACE("controlling_process\n");
        term new_pid = term_get_tuple_element(cmd, 1);
        socket_driver_controlling_process(ctx, pid, ref, new_pid);
    } else {
        TRACE("unknown cmd\n");
        port_send_error_tuple(ctx, pid, ref, BADARG_ATOM);
    }

    mailbox_remove(&ctx->mailbox);
    TRACE("END socket_consume_mailbox\n");

    return result;
}

Context *socket_init(GlobalContext *glb, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(glb);
    void *data = socket_driver_create_data();
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = data;

    return ctx;
}
