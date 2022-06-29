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
#include <netinet/tcp.h>
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

//#define ENABLE_TRACE
#include "trace.h"

#define BUFSIZE 128

typedef struct SocketDriverData
{
    int sockfd;
    term proto;
    term port;
    term controlling_process;
    term binary;
    term active;
    term buffer;
    EventListener *active_listener;
} SocketDriverData;

// TODO define in defaultatoms
const char *const send_a = "\x4" "send";
const char *const sendto_a = "\x6" "sendto";
const char *const init_a = "\x4" "init";
const char *const bind_a = "\x4" "bind";
const char *const recvfrom_a = "\x8" "recvfrom";
const char *const recv_a = "\x4" "recv";
const char *const close_a = "\x5" "close";
const char *const closed_a = "\x6" "closed";
const char *const get_port_a = "\x8" "get_port";
const char *const accept_a = "\x6" "accept";
const char *const sockname_a = "\x8" "sockname";
const char *const peername_a = "\x8" "peername";
const char *const controlling_process_a = "\x13" "controlling_process";
const char *const not_owner_a = "\x9" "not_owner";

static void active_recv_callback(EventListener *listener);
static void passive_recv_callback(EventListener *listener);
static void active_recvfrom_callback(EventListener *listener);
static void passive_recvfrom_callback(EventListener *listener);
static void socket_consume_mailbox(Context *ctx);

uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

term socket_tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

term socket_create_packet_term(Context *ctx, const char *buf, ssize_t len, int is_binary)
{
    if (is_binary) {
        return term_from_literal_binary((void *) buf, len, ctx);
    } else {
        return term_from_string((const uint8_t *) buf, len, ctx);
    }
}

void *socket_driver_create_data()
{
    struct SocketDriverData *data = calloc(1, sizeof(struct SocketDriverData));
    data->sockfd = -1;
    data->proto = term_invalid_term();
    data->port = term_invalid_term();
    data->controlling_process = term_invalid_term();
    data->binary = term_invalid_term();
    data->active = term_invalid_term();
    data->buffer = term_invalid_term();
    data->active_listener = NULL;
    return (void *) data;
}

void socket_driver_delete_data(void *data)
{
    free(data);
}

static term do_bind(Context *ctx, term address, term port)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    struct sockaddr_in serveraddr;
    UNUSED(address);
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    if (address == UNDEFINED_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (term_is_tuple(address)) {
        serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(address));
    } else {
        term_display(stderr, address, ctx);
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    avm_int_t p = term_to_int(port);
    serveraddr.sin_port = htons(p);
    socklen_t address_len = sizeof(serveraddr);
    if (bind(socket_data->sockfd, (struct sockaddr *) &serveraddr, address_len) == -1) {
        return port_create_sys_error_tuple(ctx, BIND_ATOM, errno);
    } else {
        TRACE("socket_driver|do_bind: bound to %ld\n", p);
        if (getsockname(socket_data->sockfd, (struct sockaddr *) &serveraddr, &address_len) == -1) {
            return port_create_sys_error_tuple(ctx, GETSOCKNAME_ATOM, errno);
        } else {
            socket_data->port = ntohs(serveraddr.sin_port);
            return OK_ATOM;
        }
    }
}

static term init_udp_socket(Context *ctx, SocketDriverData *socket_data, term params, term active)
{
    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd == -1) {
        return port_create_sys_error_tuple(ctx, SOCKET_ATOM, errno);
    }
    socket_data->sockfd = sockfd;
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1) {
        close(sockfd);
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }
    term address = interop_proplist_get_value_default(params, ADDRESS_ATOM, UNDEFINED_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_bind(ctx, address, port);
    if (ret != OK_ATOM) {
        close(sockfd);
    } else {
        if (active == TRUE_ATOM) {
            EventListener *listener = malloc(sizeof(EventListener));
            if (IS_NULL_PTR(listener)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                AVM_ABORT();
            }
            listener->fd = socket_data->sockfd;
            listener->data = ctx;
            listener->handler = active_recvfrom_callback;
            list_append(&platform->listeners, &listener->listeners_list_head);
            socket_data->active_listener = listener;
        }
    }
    return ret;
}

static term do_connect(SocketDriverData *socket_data, Context *ctx, term address, term port)
{
    // TODO handle IP addresses
    if (!term_is_list(address)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    int ok;
    char *addr_str = interop_term_to_string(address, &ok);
    if (!ok) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    char port_str[32];
    snprintf(port_str, 32, "%u", (unsigned short) term_to_int(port));
    TRACE("socket_driver:do_connect: resolving to %s:%s over socket fd %i\n", addr_str, port_str, term_to_int32(socket_data->sockfd));

    struct addrinfo *server_info;
    int status = getaddrinfo(addr_str, port_str, &hints, &server_info);

    free(addr_str);

    if (status != 0) {
        return port_create_sys_error_tuple(ctx, GETADDRINFO_ATOM, status);
    }

    struct sockaddr *addr = NULL;
    size_t addr_len = 0;
    for (struct addrinfo *p = server_info; p != NULL; p = p->ai_next) {
        addr = p->ai_addr;
        addr_len = p->ai_addrlen;
        break;
    }
    if (IS_NULL_PTR(addr)) {
        return port_create_error_tuple(ctx, NO_SUCH_HOST_ATOM);
    }

    status = connect(socket_data->sockfd, addr, addr_len);

    freeaddrinfo(server_info);

    if (status == -1) {
        return port_create_sys_error_tuple(ctx, CONNECT_ATOM, errno);
    } else {
        TRACE("socket_driver|do_connect: connected.\n");
        return OK_ATOM;
    }
}

static term init_client_tcp_socket(Context *ctx, SocketDriverData *socket_data, term params, term active)
{
    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) {
        return port_create_sys_error_tuple(ctx, SOCKET_ATOM, errno);
    }
    socket_data->sockfd = sockfd;
    term address = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_connect(socket_data, ctx, address, port);
    if (ret != OK_ATOM) {
        close(sockfd);
    } else {
        if (active == TRUE_ATOM) {
            EventListener *listener = malloc(sizeof(EventListener));
            if (IS_NULL_PTR(listener)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                AVM_ABORT();
            }
            listener->fd = socket_data->sockfd;
            listener->data = ctx;
            listener->handler = active_recv_callback;
            list_append(&platform->listeners, &listener->listeners_list_head);
            socket_data->active_listener = listener;
        }
    }
    return ret;
}

static term do_listen(SocketDriverData *socket_data, Context *ctx, term params)
{
    term backlog = interop_proplist_get_value(params, BACKLOG_ATOM);
    if (!term_is_integer(backlog)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    int status = listen(socket_data->sockfd, term_from_int(backlog));
    if (status == -1) {
        return port_create_sys_error_tuple(ctx, LISTEN_ATOM, errno);
    } else {
        return OK_ATOM;
    }
}

static term init_server_tcp_socket(Context *ctx, SocketDriverData *socket_data, term params)
{
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) {
        return port_create_sys_error_tuple(ctx, SOCKET_ATOM, errno);
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
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }
    term address = interop_proplist_get_value_default(params, ADDRESS_ATOM, UNDEFINED_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_bind(ctx, address, port);
    if (ret != OK_ATOM) {
        close(sockfd);
    } else {
        ret = do_listen(socket_data, ctx, params);
        if (ret != OK_ATOM) {
            close(sockfd);
        } else {
            TRACE("socket_driver|init_server_tcp_socket: listening on port %u\n", (unsigned) term_to_int(port));
        }
    }
    return ret;
}

static Context *create_accepting_socket(GlobalContext *glb, SocketDriverData *socket_data, int fd, term controlling_process)
{
    struct GenericUnixPlatformData *platform = glb->platform_data;

    Context *new_ctx = context_new(glb);
    new_ctx->native_handler = socket_consume_mailbox;
    scheduler_make_waiting(glb, new_ctx);

    SocketDriverData *new_socket_data = socket_driver_create_data();
    new_socket_data->sockfd = fd;
    new_socket_data->proto = socket_data->proto;
    new_socket_data->active = socket_data->active;
    new_socket_data->binary = socket_data->binary;
    new_socket_data->buffer = socket_data->buffer;
    new_socket_data->controlling_process = controlling_process;

    new_ctx->platform_data = new_socket_data;

    if (new_socket_data->active == TRUE_ATOM) {
        EventListener *listener = malloc(sizeof(EventListener));
        if (IS_NULL_PTR(listener)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            AVM_ABORT();
        }
        listener->fd = new_socket_data->sockfd;
        listener->data = new_ctx;
        listener->handler = active_recv_callback;
        list_append(&platform->listeners, &listener->listeners_list_head);
        new_socket_data->active_listener = listener;
    }
    return new_ctx;
}

term socket_driver_do_init(Context *ctx, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (!term_is_list(params)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    term proto = interop_proplist_get_value(params, PROTO_ATOM);
    if (term_is_nil(proto)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    socket_data->proto = proto;
    //
    // get the controlling process
    //
    term controlling_process = interop_proplist_get_value_default(params, CONTROLLING_PROCESS_ATOM, term_invalid_term());
    if (!(term_is_invalid_term(controlling_process) || term_is_pid(controlling_process))) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    socket_data->controlling_process = controlling_process;
    //
    // get the binary flag
    //
    term binary = interop_proplist_get_value_default(params, BINARY_ATOM, FALSE_ATOM);
    if (!(binary == TRUE_ATOM || binary == FALSE_ATOM)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    socket_data->binary = binary;
    //
    // get the buffer size
    //
    term buffer = interop_proplist_get_value_default(params, BUFFER_ATOM, term_from_int(BUFSIZE));
    if (!term_is_integer(buffer)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    socket_data->buffer = buffer;
    //
    // get the active flag
    //
    term active = interop_proplist_get_value_default(params, ACTIVE_ATOM, FALSE_ATOM);
    socket_data->active = active;
    //
    // initialize based on specified protocol and action
    //
    if (proto == UDP_ATOM) {
        term ret = init_udp_socket(ctx, socket_data, params, active);
        return ret;
    } else if (proto == TCP_ATOM) {
        term connect = interop_proplist_get_value_default(params, CONNECT_ATOM, FALSE_ATOM);
        if (connect == TRUE_ATOM) {
            return init_client_tcp_socket(ctx, socket_data, params, active);
        } else {
            term listen = interop_proplist_get_value_default(params, LISTEN_ATOM, FALSE_ATOM);
            if (listen == TRUE_ATOM) {
                return init_server_tcp_socket(ctx, socket_data, params);
            } else {
                return port_create_error_tuple(ctx, BADARG_ATOM);
            }
        }
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
}

void socket_driver_do_close(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (socket_data->active == TRUE_ATOM) {
        // If this socket is a listening socket, it may already have been removed from the
        // listeners list during an accept.  Check here to make sure that the active listener
        // has not already been removed.
        if (list_length(&socket_data->active_listener->listeners_list_head) != 0) {
            list_remove(&socket_data->active_listener->listeners_list_head);
        }
    }
    if (close(socket_data->sockfd) == -1) {
        TRACE("socket_driver|socket_driver_do_close: close failed");
    } else {
        TRACE("socket_driver|socket_driver_do_close: closed socket\n");
    }
    scheduler_terminate(ctx);
}

static term socket_driver_controlling_process(Context *ctx, term pid, term new_pid_term)
{
    struct SocketDriverData *socket_data = ctx->platform_data;

    if (UNLIKELY(!term_is_pid(new_pid_term))) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term error = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error, 0, ERROR_ATOM);
        term_put_tuple_element(error, 1, BADARG_ATOM);
        return error;
    } else if (UNLIKELY(pid != socket_data->controlling_process)) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term error = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error, 0, ERROR_ATOM);
        term_put_tuple_element(error, 1, context_make_atom(ctx, not_owner_a));
        return error;
    } else {
        socket_data->controlling_process = new_pid_term;
        return OK_ATOM;
    }
}

//
// INET API
//

term socket_driver_get_port(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    port_ensure_available(ctx, 7);
    return port_create_ok_tuple(ctx, term_from_int(socket_data->port));
}

term socket_driver_sockname(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int result = getsockname(socket_data->sockfd, (struct sockaddr *) &addr, &addrlen);
    if (result != 0) {
        port_ensure_available(ctx, 3);
        return port_create_error_tuple(ctx, term_from_int(errno));
    } else {
        port_ensure_available(ctx, 11);
        term addr_term = socket_tuple_from_addr(
            ctx, ntohl(addr.sin_addr.s_addr));
        term port_term = term_from_int(ntohs(addr.sin_port));
        term addr_port = port_create_tuple2(
            ctx,
            addr_term,
            port_term);
        return port_create_tuple2(
            ctx,
            OK_ATOM,
            addr_port);
    }
}

term socket_driver_peername(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int result = getpeername(socket_data->sockfd, (struct sockaddr *) &addr, &addrlen);
    if (result != 0) {
        port_ensure_available(ctx, 3);
        return port_create_error_tuple(ctx, term_from_int(errno));
    } else {
        port_ensure_available(ctx, 11);
        term addr_term = socket_tuple_from_addr(
            ctx, ntohl(addr.sin_addr.s_addr));
        term port_term = term_from_int(ntohs(addr.sin_port));
        term addr_port = port_create_tuple2(
            ctx,
            addr_term,
            port_term);
        return port_create_tuple2(
            ctx,
            OK_ATOM,
            addr_port);
    }
}

//
// send operations
//

term socket_driver_do_send(Context *ctx, term buffer)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    char *buf;
    size_t len;
    if (term_is_binary(buffer)) {
        buf = (char *) term_binary_data(buffer);
        len = term_binary_size(buffer);
    } else if (term_is_list(buffer)) {
        switch (interop_iolist_size(buffer, &len)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                return port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        buf = malloc(len);
        switch (interop_write_iolist(buffer, buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(buf);
                return port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                free(buf);
                return port_create_error_tuple(ctx, BADARG_ATOM);
        }
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    int sent_data = send(socket_data->sockfd, buf, len, 0);
    if (term_is_list(buffer)) {
        free(buf);
    }

    if (sent_data == -1) {
        return port_create_sys_error_tuple(ctx, SEND_ATOM, errno);
    } else {
        TRACE("socket_driver_do_send: sent data with len %li to fd %i\n", len, socket_data->sockfd);
        term sent_atom = term_from_int(sent_data);
        return port_create_ok_tuple(ctx, sent_atom);
    }
}

term socket_driver_do_sendto(Context *ctx, term dest_address, term dest_port, term buffer)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(socket_tuple_to_addr(dest_address));
    addr.sin_port = htons(term_to_int32(dest_port));
    char *buf;
    size_t len;
    if (term_is_binary(buffer)) {
        buf = (char *) term_binary_data(buffer);
        len = term_binary_size(buffer);
    } else if (term_is_list(buffer)) {
        switch (interop_iolist_size(buffer, &len)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                return port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        buf = malloc(len);
        switch (interop_write_iolist(buffer, buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(buf);
                return port_create_error_tuple(ctx, OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                free(buf);
                return port_create_error_tuple(ctx, BADARG_ATOM);
        }
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    int sent_data = sendto(socket_data->sockfd, buf, len, 0, (struct sockaddr *) &addr, sizeof(addr));
    if (term_is_list(buffer)) {
        free(buf);
    }
    if (sent_data == -1) {
        return port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno);
    } else {
        TRACE("socket_driver_do_sendto: sent data with len: %li, to: %i, port: %i\n", len, ntohl(addr.sin_addr.s_addr), ntohs(addr.sin_port));
        term sent_atom = term_from_int32(sent_data);
        return port_create_ok_tuple(ctx, sent_atom);
    }
}

//
// receive operations
//

typedef struct RecvFromData
{
    Context *ctx;
    term pid;
    term length;
    term controlling_process;
    uint64_t ref_ticks;
} RecvFromData;

static void active_recv_callback(EventListener *listener)
{
    Context *ctx = (Context *) listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(socket_data->buffer);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    //
    // receive the data
    //
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, NULL, NULL);
    if (len <= 0) {
        // {tcp, Socket, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[2] = { TCP_CLOSED_ATOM, term_from_local_process_id(ctx->process_id) };
        term msg = port_create_tuple_n(ctx, 2, msgs);
        port_send_message(ctx, pid, msg);
        socket_driver_do_close(ctx);
    } else {
        TRACE("socket_driver|active_recv_callback: received data of len %li from fd %i\n", len, socket_data->sockfd);
        int ensure_packet_avail;
        int binary;
        if (socket_data->binary == TRUE_ATOM) {
            binary = 1;
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            binary = 0;
            ensure_packet_avail = len * 2;
        }
        // {tcp, pid, binary}
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        term pid = socket_data->controlling_process;
        term packet = socket_create_packet_term(ctx, buf, len, binary);
        term msgs[3] = { TCP_ATOM, term_from_local_process_id(ctx->process_id), packet };
        term msg = port_create_tuple_n(ctx, 3, msgs);
        port_send_message(ctx, pid, msg);
    }
    free(buf);
}

static void passive_recv_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(recvfrom_data->length);
    int flags = MSG_WAITALL;
    if (buf_size == 0) {
        buf_size = term_to_int(socket_data->buffer);
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
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, flags, NULL, NULL);
    if (len == 0) {
        // {Ref, {error, closed}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, context_make_atom(ctx, closed_a)));
        socket_driver_do_close(ctx);
    } else if (len < 0) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECV_ATOM, errno));
        socket_driver_do_close(ctx);
    } else {
        TRACE("socket_driver|passive_recv_callback: passive received data of len: %li\n", len);
        int ensure_packet_avail;
        if (socket_data->binary == TRUE_ATOM) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        // {Ref, {ok, Packet::binary()}}
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term packet = socket_create_packet_term(ctx, buf, len, socket_data->binary == TRUE_ATOM);
        term reply = port_create_ok_tuple(ctx, packet);
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    list_remove(&listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
}

static void active_recvfrom_callback(EventListener *listener)
{
    Context *ctx = (Context *) listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(socket_data->buffer);
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
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, (struct sockaddr *) &clientaddr, &clientlen);
    if (len == -1) {
        // {udp, Socket, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[3] = { UDP_ATOM, term_from_local_process_id(ctx->process_id), port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno) };
        term msg = port_create_tuple_n(ctx, 3, msgs);
        port_send_message(ctx, pid, msg);
    } else {
        int ensure_packet_avail;
        if (socket_data->binary == TRUE_ATOM) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        // {udp, pid, {int,int,int,int}, int, binary}
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        term pid = socket_data->controlling_process;
        term addr = socket_tuple_from_addr(ctx, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_create_packet_term(ctx, buf, len, socket_data->binary == TRUE_ATOM);
        term msgs[5] = { UDP_ATOM, term_from_local_process_id(ctx->process_id), addr, port, packet };
        term msg = port_create_tuple_n(ctx, 5, msgs);
        port_send_message(ctx, pid, msg);
    }
    free(buf);
}

static void passive_recvfrom_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(recvfrom_data->length);
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
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, (struct sockaddr *) &clientaddr, &clientlen);
    if (len == -1) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno));
    } else {
        int ensure_packet_avail;
        if (socket_data->binary == TRUE_ATOM) {
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            ensure_packet_avail = len * 2;
        }
        // {Ref, {ok, {{int,int,int,int}, int, binary}}}
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term addr = socket_tuple_from_addr(ctx, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_create_packet_term(ctx, buf, len, socket_data->binary == TRUE_ATOM);
        term addr_port_packet = port_create_tuple3(ctx, addr, port, packet);
        term reply = port_create_ok_tuple(ctx, addr_port_packet);
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    list_remove(&listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
}

static void do_recv(Context *ctx, term pid, term ref, term length, term timeout, event_handler_t handler)
{
    UNUSED(timeout);

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // The socket must be in active mode
    //
    if (socket_data->active == TRUE_ATOM) {
        port_ensure_available(ctx, 12);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        return;
    }
    //
    // Create and initialize the request-specific data
    //
    RecvFromData *data = (RecvFromData *) malloc(sizeof(RecvFromData));
    if (IS_NULL_PTR(data)) {
        fprintf(stderr, "Unable to allocate space for RecvFromData: %s:%i\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    data->ctx = ctx;
    data->pid = pid;
    data->length = length;
    data->ref_ticks = term_to_ref_ticks(ref);
    //
    // Create an event listener with this request-specific data, and append to the global list
    //
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    listener->fd = socket_data->sockfd;
    listener->handler = handler;
    listener->data = data;
    list_append(&platform->listeners, &listener->listeners_list_head);
}

void socket_driver_do_recvfrom(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recvfrom_callback);
}

void socket_driver_do_recv(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recv_callback);
}

//
// accept
//

static void accept_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    //
    // accept the connection
    //
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(socket_data->sockfd, (struct sockaddr *) &clientaddr, &clientlen);
    if (fd == -1) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, ACCEPT_ATOM, errno));
    } else {
        TRACE("socket_driver|accept_callback: accepted connection.  fd: %i\n", fd);

        term pid = recvfrom_data->pid;
        Context *new_ctx = create_accepting_socket(glb, socket_data, fd, pid);

        // {Ref, Socket}
        term socket_pid = term_from_local_process_id(new_ctx->process_id);
        port_ensure_available(ctx, 10);
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term reply = port_create_ok_tuple(ctx, socket_pid);
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    list_remove(&listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
}

void socket_driver_do_accept(Context *ctx, term pid, term ref, term timeout)
{
    UNUSED(timeout);

    GlobalContext *glb = ctx->global;
    struct GenericUnixPlatformData *platform = glb->platform_data;

    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // Create and initialize the request-specific data
    //
    RecvFromData *data = (RecvFromData *) malloc(sizeof(RecvFromData));
    if (IS_NULL_PTR(data)) {
        fprintf(stderr, "Unable to allocate space for RecvFromData: %s:%i\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    data->ctx = ctx;
    data->pid = pid;
    data->length = 0;
    data->ref_ticks = term_to_ref_ticks(ref);
    //
    // Create an event listener with this request-specific data, and append to the global list
    //
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }
    listener->fd = socket_data->sockfd;
    listener->handler = accept_callback;
    listener->data = data;
    list_append(&platform->listeners, &listener->listeners_list_head);
}

static void socket_consume_mailbox(Context *ctx)
{
    TRACE("START socket_consume_mailbox\n");
    if (UNLIKELY(ctx->native_handler != socket_consume_mailbox)) {
        AVM_ABORT();
    }

    port_ensure_available(ctx, 16);

    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    term cmd_name = term_get_tuple_element(cmd, 0);
    if (cmd_name == context_make_atom(ctx, init_a)) {
        TRACE("init\n");
        term params = term_get_tuple_element(cmd, 1);
        term reply = socket_driver_do_init(ctx, params);
        port_send_reply(ctx, pid, ref, reply);
        if (reply != OK_ATOM) {
            // TODO handle shutdown
            // socket_driver_delete_data(ctx->platform_data);
            // context_destroy(ctx);
        }
    } else if (cmd_name == context_make_atom(ctx, sendto_a)) {
        TRACE("sendto\n");
        term dest_address = term_get_tuple_element(cmd, 1);
        term dest_port = term_get_tuple_element(cmd, 2);
        term buffer = term_get_tuple_element(cmd, 3);
        term reply = socket_driver_do_sendto(ctx, dest_address, dest_port, buffer);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, send_a)) {
        TRACE("send\n");
        term buffer = term_get_tuple_element(cmd, 1);
        term reply = socket_driver_do_send(ctx, buffer);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, recvfrom_a)) {
        TRACE("recvfrom\n");
        term length = term_get_tuple_element(cmd, 1);
        term timeout = term_get_tuple_element(cmd, 2);
        socket_driver_do_recvfrom(ctx, pid, ref, length, timeout);
    } else if (cmd_name == context_make_atom(ctx, recv_a)) {
        TRACE("recv\n");
        term length = term_get_tuple_element(cmd, 1);
        term timeout = term_get_tuple_element(cmd, 2);
        socket_driver_do_recv(ctx, pid, ref, length, timeout);
    } else if (cmd_name == context_make_atom(ctx, accept_a)) {
        TRACE("accept\n");
        term timeout = term_get_tuple_element(cmd, 1);
        socket_driver_do_accept(ctx, pid, ref, timeout);
    } else if (cmd_name == context_make_atom(ctx, close_a)) {
        TRACE("close\n");
        port_send_reply(ctx, pid, ref, OK_ATOM);
        socket_driver_do_close(ctx);
        // TODO handle shutdown
        // socket_driver_delete_data(ctx->platform_data);
        // context_destroy(ctx);
    } else if (cmd_name == context_make_atom(ctx, sockname_a)) {
        TRACE("sockname\n");
        term reply = socket_driver_sockname(ctx);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, peername_a)) {
        TRACE("peername\n");
        term reply = socket_driver_peername(ctx);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, get_port_a)) {
        // TODO This function is not supported in the gen_tcp or gen_udp APIs.
        // It should be removed.  (Use inet:peername and inet:sockname instead)
        TRACE("get_port\n");
        term reply = socket_driver_get_port(ctx);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, controlling_process_a)) {
        TRACE("controlling_process\n");
        term new_pid = term_get_tuple_element(cmd, 1);
        term reply = socket_driver_controlling_process(ctx, pid, new_pid);
        port_send_reply(ctx, pid, ref, reply);
    } else {
        TRACE("unknown cmd\n");
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
    }

    mailbox_destroy_message(message);
    TRACE("END socket_consume_mailbox\n");
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
