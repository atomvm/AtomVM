/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 by Fred Dushin <fred@dushin.net>
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
#include <errno.h>
#include <fcntl.h>
#include <globalcontext.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <otp_socket_platform.h>
#include <port.h>
#include <scheduler.h>
#include <sys/socket.h>
#include <term.h>
#include <utils.h>

// #define ENABLE_TRACE
#include <trace.h>

static const char *const domain_atom = ATOM_STR("\x6", "domain");
static const char *const inet_atom = ATOM_STR("\x4", "inet");
static const char *const protocol_atom = ATOM_STR("\x8", "protocol");
static const char *const type_atom = ATOM_STR("\x4", "type");
static const char *const ip_atom = ATOM_STR("\x2", "ip");
static const char *const tcp_atom = ATOM_STR("\x3", "tcp");
static const char *const stream_atom = ATOM_STR("\x6", "stream");
static const char *const dgram_atom = ATOM_STR("\x5", "dgram");
static const char *const close_atom = ATOM_STR("\x5", "close");
static const char *const bind_atom = ATOM_STR("\x4", "bind");
static const char *const any_atom = ATOM_STR("\x3", "any");
static const char *const loopback_atom = ATOM_STR("\x8", "loopback");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const addr_atom = ATOM_STR("\x4", "addr");
static const char *const listen_atom = ATOM_STR("\x6", "listen");
static const char *const accept_atom = ATOM_STR("\x6", "accept");
static const char *const sockname_atom = ATOM_STR("\x8", "sockname");
static const char *const peername_atom = ATOM_STR("\x8", "peername");
static const char *const recv_atom = ATOM_STR("\x4", "recv");
static const char *const send_atom = ATOM_STR("\x4", "send");
// static const char *const otp_socket_atom = ATOM_STR("\xA", "otp_socket");
static const char *const closed_atom = ATOM_STR("\x6", "closed");
static const char *const setopt_atom = ATOM_STR("\x6", "setopt");
static const char *const reuseaddr_atom = ATOM_STR("\x9", "reuseaddr");
static const char *const linger_atom = ATOM_STR("\x6", "linger");
static const char *const onoff_atom = ATOM_STR("\x5", "onoff");
static const char *const connect_atom = ATOM_STR("\x7", "connect");

#define DOMAIN_ATOM make_atom(global, domain_atom)
#define INET_ATOM make_atom(global, inet_atom)
#define TYPE_ATOM make_atom(global, type_atom)
#define PROTOCOL_ATOM make_atom(global, protocol_atom)
#define IP_ATOM make_atom(global, ip_atom)
#define TCP_ATOM make_atom(global, tcp_atom)
#define STREAM_ATOM make_atom(global, stream_atom)
#define DGRAM_ATOM make_atom(global, dgram_atom)
#define CLOSE_ATOM make_atom(global, close_atom)
#define BIND_ATOM make_atom(global, bind_atom)
#define ANY_ATOM make_atom(global, any_atom)
#define LOOPBACK_ATOM make_atom(global, loopback_atom)
#define PORT_ATOM make_atom(global, port_atom)
#define ADDR_ATOM make_atom(global, addr_atom)
#define LISTEN_ATOM make_atom(global, listen_atom)
#define ACCEPT_ATOM make_atom(global, accept_atom)
#define SOCKNAME_ATOM make_atom(global, sockname_atom)
#define PEERNAME_ATOM make_atom(global, peername_atom)
#define RECV_ATOM make_atom(global, recv_atom)
#define SEND_ATOM make_atom(global, send_atom)
#define OTP_SOCKET_ATOM make_atom(global, otp_socket_atom)
#define CLOSED_ATOM make_atom(global, closed_atom)
#define SETOPT_ATOM make_atom(global, setopt_atom)
#define REUSEADDR_ATOM make_atom(global, reuseaddr_atom)
#define LINGER_ATOM make_atom(global, linger_atom)
#define ONOFF_ATOM make_atom(global, onoff_atom)
#define CONNECT_ATOM make_atom(global, connect_atom)

#define DEFAULT_BUFFER_SIZE 512
#define MIN(A, B) (((A) < (B)) ? (A) : (B))

enum SocketState
{
    idle,
    waiting_accept,
    waiting_recv,
    waiting_connect
};

struct AsyncResponseData
{
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
};

struct SocketData
{
    EventListener event_listener;
    enum SocketState socket_state;
    struct AsyncResponseData async_response_data;
    int fd;
    ssize_t buffer_size;
};

static void consume_mailbox(Context *ctx);
static Context *create_context(GlobalContext *global, int fd);

//
// socket operations
//

static inline term make_atom(GlobalContext *global, AtomString string)
{
    int global_atom_index = globalcontext_insert_atom(global, string);
    return term_from_atom_index(global_atom_index);
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    mailbox_send(target, message);
}

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

static void send_reply(Context *ctx, term pid, uint64_t ref_ticks, term return_value)
{
    // Pid ! {Ref, ReturnValue}
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) + REF_SIZE, 1, &return_value, MEMORY_NO_SHRINK) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    } else {
        term return_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_tuple, 0, term_from_ref_ticks(ref_ticks, ctx));
        term_put_tuple_element(return_tuple, 1, return_value);
        send_message(pid, return_tuple, ctx->global);
    }
    // force a shrinking GC
    if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
        // ignore
    }
}

static void send_error_tuple(Context *ctx, term pid, uint64_t ref_ticks, term reason)
{
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    } else {
        term error_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, reason);

        send_reply(ctx, pid, ref_ticks, error_tuple);
    }
}

static void close_socket(Context *ctx)
{
    struct SocketData *socket_data = ctx->platform_data;
    EventListener *listener = &socket_data->event_listener;

    otp_socket_platform_notify_remove(ctx, socket_data->fd);

    list_remove(&listener->listeners_list_head);

    int sres = close(socket_data->fd);
    if (sres != 0) {
        AVM_LOGW(TAG, "Unable to close socket.");
    }
    TRACE("Destroying context 0x%p with (removed) EventListener 0x%p\n", ctx, listener);

    free(socket_data);
    scheduler_terminate(ctx);
}

static void do_close(Context *ctx, term msg)
{
    TRACE("do_close\n");
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    send_reply(ctx, pid, ref_ticks, OK_ATOM);

    // notify any procs that might be stuck in accept or recv if the parent socket was closed
    if (socket_data->socket_state != idle) {
        GlobalContext *global = ctx->global;
        EventListener *listener = &socket_data->event_listener;
        struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
        TRACE("Notifying pid %i that socket has closed\n", async_response_data->pid);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
    }

    close_socket(ctx);
}

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

    if (sockaddr == ANY_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (sockaddr == LOOPBACK_ATOM) {
        serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else if (term_is_map(sockaddr)) {
        term port = term_get_map_assoc_default(ctx, sockaddr, PORT_ATOM, term_from_int(0));
        serveraddr.sin_port = htons(term_to_int(port));
        term addr = term_get_map_assoc(ctx, sockaddr, ADDR_ATOM);
        if (addr == ANY_ATOM) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
        } else if (addr == LOOPBACK_ATOM) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
        }
    }

    socklen_t address_len = sizeof(serveraddr);
    int res = bind(socket_data->fd, (struct sockaddr *) &serveraddr, address_len);
    if (res != 0) {
        AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

static void do_listen(Context *ctx, term msg, term cmd)
{
    TRACE("do_listen\n");
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term backlog = term_get_tuple_element(cmd, 1);

    int res = listen(socket_data->fd, term_to_int(backlog));
    if (res != 0) {
        AVM_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }
}

static void accept_handler(EventListener *listener)
{
    TRACE("accept_handler\n");
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
    Context *ctx = async_response_data->ctx;
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    int fd = accept(socket_data->fd, (struct sockaddr *) &clientaddr, &clientlen);
    if (fd == -1) {
        AVM_LOGE(TAG, "Unable to accept on socket %i.", socket_data->fd);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
    } else {

        TRACE("otp_socket.accept_handler: accepted connection.  fd: %i\n", fd);

        Context *new_ctx = create_context(global, fd);
        scheduler_make_waiting(global, new_ctx);

        // {ok, Socket}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, ctx->global);
        }
        term return_value = term_alloc_tuple(2, ctx);
        term new_pid = term_from_local_process_id(new_ctx->process_id);
        term_put_tuple_element(return_value, 0, OK_ATOM);
        term_put_tuple_element(return_value, 1, new_pid);

        send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, return_value);
    }
    socket_data->socket_state = idle;
    list_remove(&listener->listeners_list_head);
}

static void do_accept(Context *ctx, term msg)
{
    TRACE("do_accept\n");
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    EventListener *listener = &socket_data->event_listener;
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;

    async_response_data->ctx = ctx;
    async_response_data->pid = pid;
    async_response_data->ref_ticks = ref_ticks;
    socket_data->socket_state = waiting_accept;

    list_init(&listener->listeners_list_head);
    listener->handler = accept_handler;
    struct ListHead *listeners = otp_socket_platform_get_listeners(global->platform_data);
    list_append(listeners, &listener->listeners_list_head);

    TRACE("EventListener 0x%p with sender=0x%p added to platform listeners for accept\n", listener, ctx);
}

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

    if (res != 0) {
        AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", socket_data->fd, res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        // {ok, #{addr => {a,b,c,d}, port => integer()}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(ctx, 2);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

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

    if (res != 0) {
        AVM_LOGE(TAG, "Unable to getpeername: fd=%i res=%i.", socket_data->fd, res);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        // {ok, #{addr => {a,b,c,d}, port => integer()}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
            send_message(pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
            term port_number = term_from_int(ntohs(addr.sin_port));

            term map = term_alloc_map(ctx, 2);
            term_set_map_assoc(map, 0, ADDR_ATOM, address);
            term_set_map_assoc(map, 1, PORT_ATOM, port_number);
            term return_value = port_create_tuple2(ctx, OK_ATOM, map);

            send_reply(ctx, pid, ref_ticks, return_value);
        }
    }
}

static void recv_handler(EventListener *listener)
{
    TRACE("recv_handler\n");
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
    Context *ctx = async_response_data->ctx;
    GlobalContext *global = ctx->global;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    if (otp_socket_platform_supports_peek()) {
        int flags = MSG_WAITALL;
        ssize_t num_available_bytes = recvfrom(socket_data->fd, NULL, socket_data->buffer_size, MSG_PEEK | flags, NULL, NULL);
        TRACE("%i bytes available.\n", num_available_bytes);
        if (num_available_bytes < 0) {
            AVM_LOGI(TAG, "Unable to receive data on fd %i.  errno=%i", socket_data->fd, errno);
            send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
            close_socket(ctx);
        } else if (num_available_bytes == 0) {
            AVM_LOGI(TAG, "Peer closed socket %i.", socket_data->fd);
            send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
            close_socket(ctx);
        } else {
            ssize_t buffer_size = MIN(num_available_bytes, socket_data->buffer_size);
            // {ok, Data}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_data_size_in_terms(buffer_size)) != MEMORY_GC_OK)) {
                AVM_LOGE(TAG, "Failed to allocate memory for recv buffer.");
                send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, global);
            } else {
                term data = term_create_uninitialized_binary(buffer_size, ctx);
                const char *buffer = term_binary_data(data);
                //
                // receive data on the socket
                //
                ssize_t len = recvfrom(socket_data->fd, (char *) buffer, buffer_size, flags, NULL, NULL);
                if (len < 0) {
                    AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", socket_data->fd, errno);
                    send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
                    close_socket(ctx);
                } else {
                    TRACE("otp_socket.recv_handler: received data on fd: %i len=%u\n", socket_data->fd, len);

                    term return_value = term_alloc_tuple(2, ctx);
                    term_put_tuple_element(return_value, 0, OK_ATOM);
                    term_put_tuple_element(return_value, 1, data);

                    send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, return_value);
                    list_remove(&listener->listeners_list_head);
                }
            }
        }
    } else {
        int flags = 0;
        size_t buffer_size = socket_data->buffer_size;
        // {ok, Data}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_binary_data_size_in_terms(buffer_size)) != MEMORY_GC_OK)) {
            AVM_LOGE(TAG, "Failed to allocate memory for recv buffer.");
            send_message(async_response_data->pid, OUT_OF_MEMORY_ATOM, global);
        } else {
            term data = term_create_uninitialized_binary(buffer_size, ctx);
            const char *buffer = term_binary_data(data);
            //
            // receive data on the socket
            //
            ssize_t len = recvfrom(socket_data->fd, (char *) buffer, buffer_size, flags, NULL, NULL);
            if (len < 0) {
                if (errno == ECONNRESET) {
                    AVM_LOGI(TAG, "Peer closed connection.");
                } else {
                    AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", socket_data->fd, errno);
                }
                send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
                close_socket(ctx);
            } else if (len == 0) {
                TRACE("Peer closed socket %i.", socket_data->fd);
                send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, CLOSED_ATOM);
                close_socket(ctx);
            } else {
                TRACE("otp_socket.recv_handler: received data on fd: %i len=%u\n", socket_data->fd, len);

                term return_value = term_alloc_tuple(2, ctx);
                term_put_tuple_element(return_value, 0, OK_ATOM);
                term bin = term_from_literal_binary(buffer, len, ctx);
                term_put_tuple_element(return_value, 1, bin);

                send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, return_value);
                list_remove(&listener->listeners_list_head);
            }
        }
    }

    socket_data->socket_state = idle;
}

static void do_recv(Context *ctx, term msg)
{
    TRACE("do_recv\n");
    GlobalContext *global = ctx->global;
    // struct ESP32PlatformData *platform = global->platform_data;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    EventListener *listener = &socket_data->event_listener;
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;

    async_response_data->ctx = ctx;
    async_response_data->pid = pid;
    async_response_data->ref_ticks = ref_ticks;
    socket_data->socket_state = waiting_recv;

    list_init(&listener->listeners_list_head);
    listener->handler = recv_handler;
    struct ListHead *listeners = otp_socket_platform_get_listeners(global->platform_data);
    list_append(listeners, &listener->listeners_list_head);

    TRACE("EventListener 0x%p with sender=0x%p added to platform listeners for recv\n", listener, ctx);
}

static void do_send(Context *ctx, term msg, term cmd)
{
    TRACE("do_send\n");
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);
    term data = term_get_tuple_element(cmd, 1);

    // {ok, RestData} | {error, Reason}
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        send_message(pid, OUT_OF_MEMORY_ATOM, ctx->global);
    }

    char *buf = NULL;
    size_t len = 0;
    if (term_is_binary(data)) {
        buf = (char *) term_binary_data(data);
        len = term_binary_size(data);
    } else if (term_is_list(data)) {
        InteropFunctionResult result = interop_iolist_size(data, &len);
        if (UNLIKELY(result == InteropMemoryAllocFail)) {
            send_error_tuple(ctx, pid, ref_ticks, OUT_OF_MEMORY_ATOM);
            return;
        }
        if (result == InteropBadArg) {
            send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
        buf = malloc(len);
        if (UNLIKELY(!interop_write_iolist(data, buf))) {
            free(buf);
            send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
            return;
        }
    } else {
        send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
        return;
    }

    int sent_data = send(socket_data->fd, buf, len, 0);
    if (term_is_list(data)) {
        free(buf);
    }

    if (sent_data == -1) {
        AVM_LOGE(TAG, "Unable to send data: res=%i.", sent_data);
        send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    } else {
        term return_value = port_create_tuple2(ctx, OK_ATOM, term_nil());
        send_reply(ctx, pid, ref_ticks, return_value);
    }
}

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
    if (opt == REUSEADDR_ATOM) {
        int option_value = (value == TRUE_ATOM) ? 1 : 0;
        int res = setsockopt(socket_data->fd, SOL_SOCKET, SO_REUSEADDR, &option_value, sizeof(int));
        if (res != 0) {
            send_error_tuple(ctx, pid, ref_ticks, term_from_int(res));
        } else {
            send_reply(ctx, pid, ref_ticks, OK_ATOM);
        }
    } else if (opt == LINGER_ATOM) {
        term onoff = term_get_map_assoc(ctx, value, ONOFF_ATOM);
        term linger = term_get_map_assoc(ctx, value, LINGER_ATOM);
        struct linger sl;
        sl.l_onoff = (onoff == TRUE_ATOM) ? 1 : 0;
        sl.l_linger = term_to_int(linger);
        int res = setsockopt(socket_data->fd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));
        if (res != 0) {
            send_error_tuple(ctx, pid, ref_ticks, term_from_int(res));
        } else {
            send_reply(ctx, pid, ref_ticks, OK_ATOM);
        }
        // TODO add more as needed
        // int flag = 1;
        // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
        // if (res != 0) {
        //     AVM_LOGW(TAG, "Failed to set TCP_NODELAY.");
        // }
    } else {
        send_error_tuple(ctx, pid, ref_ticks, BADARG_ATOM);
    }
}

static void connect_handler(EventListener *listener)
{
    TRACE("connect_handler\n");
    struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;
    Context *ctx = async_response_data->ctx;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    const int flags = fcntl(socket_data->fd, F_GETFL, 0);
    int fctl_res = fcntl(socket_data->fd, F_SETFL, flags ^ O_NONBLOCK);
    if (fctl_res == -1) {
        AVM_LOGE(TAG, "Unable to set socket back to blocking: fd=%i fctl_res=%i errno=%i", socket_data->fd, fctl_res, errno);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
        return;
    }
    list_remove(&listener->listeners_list_head);

    struct sockaddr_in addr;
    socklen_t addrlen = sizeof(addr);
    int res = getpeername(socket_data->fd, (struct sockaddr *) &addr, &addrlen);
    if (res != 0) {
        AVM_LOGE(TAG, "Socket connect failed: fd=%i fctl_res=%i errno=%i", socket_data->fd, fctl_res, errno);
        send_error_tuple(ctx, async_response_data->pid, async_response_data->ref_ticks, term_from_int(errno));
    }
    send_reply(ctx, async_response_data->pid, async_response_data->ref_ticks, OK_ATOM);
}

static void do_connect(Context *ctx, term msg, term cmd)
{
    TRACE("do_connect\n");
    GlobalContext *global = ctx->global;
    // struct ESP32PlatformData *platform = global->platform_data;
    struct SocketData *socket_data = (struct SocketData *) ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    term sockaddr = term_get_tuple_element(cmd, 1);

    // TODO make connect non-blocking
    // int fctl_res = fcntl(socket_data->fd, F_SETFL, O_NONBLOCK);
    // if (fctl_res == -1) {
    //     AVM_LOGE(TAG, "Unable to set socket to non-blocking: fd=%i fctl_res=%i errno=%i", socket_data->fd, fctl_res, errno);
    //     send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
    //     return;
    // }

    struct sockaddr_in address;
    memset(&address, 0, sizeof(struct sockaddr_in));
    address.sin_family = AF_INET;

    if (sockaddr == LOOPBACK_ATOM) {
        address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else if (term_is_map(sockaddr)) {
        term port = term_get_map_assoc_default(ctx, sockaddr, PORT_ATOM, term_from_int(0));
        address.sin_port = htons(term_to_int(port));
        term addr = term_get_map_assoc(ctx, sockaddr, ADDR_ATOM);
        if (addr == LOOPBACK_ATOM) {
            address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else {
            address.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
        }
    }
    socklen_t addr_len = sizeof(struct sockaddr_in);
    int res = connect(socket_data->fd, (const struct sockaddr *) &address, addr_len);
    if (res == -1) {
        if (errno == EINPROGRESS) {
            EventListener *listener = &socket_data->event_listener;
            struct AsyncResponseData *async_response_data = (struct AsyncResponseData *) listener->data;

            async_response_data->ctx = ctx;
            async_response_data->pid = pid;
            async_response_data->ref_ticks = ref_ticks;
            socket_data->socket_state = waiting_connect;

            list_init(&listener->listeners_list_head);
            listener->handler = connect_handler;
            struct ListHead *listeners = otp_socket_platform_get_listeners(global->platform_data);
            list_append(listeners, &listener->listeners_list_head);
            TRACE("EventListener 0x%p with sender=0x%p added to platform listeners for connect\n", listener, ctx);
        } else {
            AVM_LOGE(TAG, "Unable to connect: res=%i errno=%i", res, errno);
            send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
        }
    } else if (res == 0) {
        send_reply(ctx, pid, ref_ticks, OK_ATOM);
    }

}

//
// message processing
//

static void consume_mailbox(Context *ctx)
{
    TRACE("Processing mailbox message for context 0x%p\n", ctx);
    GlobalContext *global = ctx->global;
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    // TRACE("message: ");
    // #ifdef ENABLE_TRACE
    //     term_display(stdout, msg, ctx);
    // #endif
    // TRACE("\n");

    term cmd = term_get_tuple_element(msg, 2);

    if (cmd == CLOSE_ATOM) {
        do_close(ctx, msg);
    } else if (cmd == SOCKNAME_ATOM) {
        do_sockname(ctx, msg);
    } else if (cmd == PEERNAME_ATOM) {
        do_peername(ctx, msg);
    } else if (cmd == ACCEPT_ATOM) {
        do_accept(ctx, msg);
    } else if (cmd == RECV_ATOM) {
        do_recv(ctx, msg);
    } else if (term_is_tuple(cmd)) {
        term cmd_name = term_get_tuple_element(cmd, 0);
        if (cmd_name == BIND_ATOM) {
            do_bind(ctx, msg, cmd);
        } else if (cmd_name == LISTEN_ATOM) {
            do_listen(ctx, msg, cmd);
        } else if (cmd_name == SEND_ATOM) {
            do_send(ctx, msg, cmd);
        } else if (cmd_name == SETOPT_ATOM) {
            do_setopt(ctx, msg, cmd);
        } else if (cmd_name == CONNECT_ATOM) {
            do_connect(ctx, msg, cmd);
        } else {
            TRACE("badarg\n");
        }
    }

    mailbox_destroy_message(message);
}

//
// entrypoints
//

static inline int get_domain(GlobalContext *global, term domain_term)
{
    if (domain_term == INET_ATOM) {
        return PF_INET;
    } else {
        AVM_LOGW(TAG, "Unsupported domain.  Defaulting to inet.");
        return PF_INET;
    }
}

static inline int get_type(GlobalContext *global, term type_term)
{
    if (type_term == STREAM_ATOM) {
        return SOCK_STREAM;
    } else if (type_term == DGRAM_ATOM) {
        return SOCK_DGRAM;
    } else {
        AVM_LOGW(TAG, "Unsupported type.  Defaulting to stream.");
        return SOCK_STREAM;
    }
}

static inline int get_protocol(GlobalContext *global, term protocol_term)
{
    if (protocol_term == IP_ATOM) {
        return IPPROTO_IP;
    } else if (protocol_term == TCP_ATOM) {
        return IPPROTO_TCP;
    } else {
        AVM_LOGW(TAG, "Unsupported protocol.  Defaulting to ip.");
        return IPPROTO_IP;
    }
}

static Context *create_context(GlobalContext *global, int fd)
{
    struct SocketData *socket_data = malloc(sizeof(struct SocketData));
    if (IS_NULL_PTR(socket_data)) {
        AVM_LOGE(TAG, "Insufficient memory to allocate SocketData.");
        return NULL;
    }
    socket_data->fd = fd;
    socket_data->buffer_size = DEFAULT_BUFFER_SIZE;
    socket_data->event_listener.data = &socket_data->async_response_data;
    socket_data->socket_state = idle;

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
        AVM_LOGE(TAG, "Insufficient memory to allocate Context.");
        free(socket_data);
        return NULL;
    }
    ctx->native_handler = consume_mailbox;
    ctx->platform_data = socket_data;
    otp_socket_platform_set_listener(&socket_data->event_listener, ctx, fd);
    TRACE("Created Context 0x%p with EventListener 0x%p\n", ctx, &socket_data->event_listener);

    otp_socket_platform_notify_add(ctx, fd);

    return ctx;
}

Context *otp_socket_create_port(GlobalContext *global, term opts)
{
    term domain_term = interop_proplist_get_value(opts, DOMAIN_ATOM);
    term type_term = interop_proplist_get_value(opts, TYPE_ATOM);
    term protocol_term = interop_proplist_get_value(opts, PROTOCOL_ATOM);

    int fd = socket(get_domain(global, domain_term), get_type(global, type_term), get_protocol(global, protocol_term));
    if (fd == -1) {
        AVM_LOGE(TAG, "Failed to initialize socket.");
        return NULL;
    }

    return create_context(global, fd);
}
