/*
 * This file is part of AtomVM.
 *
 * Copyright 2018,2019 Davide Bettio <davide@uninstall.it>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_SOCKET_PORT_DRIVER

#include "port.h"

#include <stdbool.h>
#include <string.h>

#include "atom.h"
#include "context.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "scheduler.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"
#include "platform_defaultatoms.h"

#include <esp_log.h>

#include <lwip/api.h>
#include <lwip/inet.h>
#include <lwip/ip_addr.h>
#include <tcpip_adapter.h>

//#define ENABLE_TRACE 1
#include "trace.h"

typedef struct SocketListener
{
    EventListener base;
    int32_t process_id;
    avm_int_t buf_size;
} ActiveRecvListener;

// To make thread model explicit, functions that are passed Context *ctx are
// called from this context.
static void socket_driver_init(GlobalContext *global);
static Context *socket_driver_create_port(GlobalContext *global, term opts);

static NativeHandlerResult socket_consume_mailbox(Context *ctx);

static const char *const tcp_error_atom = "\x9" "tcp_error";

static const char *const netconn_event_internal = "\x14" "$atomvm_netconn_event_internal";

uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

static void tuple_to_ip_addr(term address_tuple, ip_addr_t *out_addr)
{
    out_addr->type = IPADDR_TYPE_V4;
    out_addr->u_addr.ip4.addr = htonl(socket_tuple_to_addr(address_tuple));
}

static void socket_fill_ipv4_addr_tuple(term addr_tuple, ip_addr_t *addr)
{
    uint8_t ad1 = ip4_addr1(&(addr->u_addr.ip4));
    uint8_t ad2 = ip4_addr2(&(addr->u_addr.ip4));
    uint8_t ad3 = ip4_addr3(&(addr->u_addr.ip4));
    uint8_t ad4 = ip4_addr4(&(addr->u_addr.ip4));
    term_put_tuple_element(addr_tuple, 0, term_from_int11(ad1));
    term_put_tuple_element(addr_tuple, 1, term_from_int11(ad2));
    term_put_tuple_element(addr_tuple, 2, term_from_int11(ad3));
    term_put_tuple_element(addr_tuple, 3, term_from_int11(ad4));
}

static term socket_addr_to_tuple(Heap *heap, ip_addr_t *addr)
{
    term addr_tuple;
    switch (IP_GET_TYPE(addr)) {
        case IPADDR_TYPE_V4: {
            term addr_tuple = term_alloc_tuple(4, heap);
            socket_fill_ipv4_addr_tuple(addr_tuple, addr);
            break;
        }
        case IPADDR_TYPE_V6:
            //TODO: implement IPv6
            addr_tuple = term_invalid_term();
            break;

        default:
            addr_tuple = term_invalid_term();
    }

    return addr_tuple;
}

enum socket_type
{
    TCPServerSocket,
    TCPClientSocket,
    UDPSocket
};

struct SocketData
{
    struct ListHead sockets_head;
    struct netconn *conn;
    int32_t process_id;
    enum socket_type type;
    int32_t controlling_process_pid;

    int32_t passive_receiver_process_pid;
    uint64_t passive_ref_ticks;

    uint16_t port;

    size_t buffer;
    bool active : 1;
    bool binary : 1;
    bool not_blocking : 1;
};

struct TCPClientSocketData
{
    struct SocketData socket_data;
};

struct TCPServerSocketData
{
    struct SocketData socket_data;
    int ready_connections;

    struct ListHead accepters_list_head;
};

struct TCPServerAccepter
{
    struct ListHead accepter_head;
    int32_t accepting_process_pid;
    uint64_t ref_ticks;
};

struct UDPSocketData
{
    struct SocketData socket_data;
};

// `socket_callback` is called from from lwip thread and enqueues the event
// into an event queue, `netconn_events`. It then posts a pointer to
// `netconn_events` as a message in the `event_queue` queue of the ESP32
// platform (declared in sys.c).
// When `sys_poll_events` is called by the scheduler, `receive_events` links
// the event in `event_queue` with `socket_events_handler` as the message is
// a pointer to `netconn_events`, which was registered as the `sender`.
// `socket_events_handler` is passed the `GlobalContext` and performs a lookup
// in the list of sockets to find the proper driver context and sends the event
// to it as an Erlang term using `globalcontext_get_process_lock`.
// The event message is then processed when the scheduler calls the
// native_handler.
//
// This pattern makes sure all accesses to netconn API are serialized. They
// may be called in different scheduler threads, though.

xQueueHandle netconn_events = NULL;

void ESP_IRAM_ATTR socket_callback(struct netconn *netconn, enum netconn_evt evt, u16_t len)
{
    // We only listen to NETCONN_EVT_RCVPLUS events
    if (evt == NETCONN_EVT_RCVPLUS) {
        BaseType_t xHigherPriorityTaskWoken;
        int result = xQueueSendFromISR(netconn_events, &netconn, &xHigherPriorityTaskWoken);
        if (result != pdTRUE) {
            fprintf(stderr, "socket: failed to enqueue: %i to netconn_events.\n", result);
        }

        result = xQueueSendFromISR(event_queue, &netconn_events, &xHigherPriorityTaskWoken);
        if (result != pdTRUE) {
            fprintf(stderr, "socket: failed to enqueue: %i to event_queue.\n", result);
        }
    }
}

// The scheduler is built in such a way that only one scheduler thread executes
// this function. So we don't need to protect the list of sockets.
EventListener *socket_events_handler(GlobalContext *glb, EventListener *listener)
{
    TRACE("socket_events_handler\n");

    struct ESP32PlatformData *platform = glb->platform_data;

    struct netconn *netconn;
    while (xQueueReceive(netconn_events, &netconn, 1) == pdTRUE) {
        TRACE("Got netconn: %p\n", (void *) netconn);
        struct SocketData *socket = NULL;
        struct ListHead *socket_head;
        LIST_FOR_EACH (socket_head, &platform->sockets_list_head) {
            struct SocketData *current_socket = GET_LIST_ENTRY(socket_head, struct SocketData, sockets_head);
            if (current_socket->conn == netconn) {
                socket = current_socket;
                break;
            }
        }

        if (socket == NULL) {
            // The socket may already be gone
            TRACE("Got event for unknown conn: %p\n", (void *) netconn);
            continue;
        }

        term message = globalcontext_make_atom(glb, netconn_event_internal);
        globalcontext_send_message(glb, socket->process_id, message);
    }
    return listener;
}

void socket_driver_init(GlobalContext *glb)
{
    TRACE("Initializing socket driver\n");

    netconn_events = xQueueCreate(32, sizeof(struct netconn *));
    EventListener *socket_listener = malloc(sizeof(EventListener));

    struct ESP32PlatformData *platform = glb->platform_data;
    socket_listener->sender = netconn_events;
    socket_listener->handler = socket_events_handler;
    synclist_append(&platform->listeners, &socket_listener->listeners_list_head);

    list_init(&platform->sockets_list_head);

    TRACE("Socket driver init: done\n");
}

static void socket_data_init(struct SocketData *data, Context *ctx, struct netconn *conn,
    enum socket_type type, struct ESP32PlatformData *platform)
{
    data->type = type;
    data->conn = conn;
    data->process_id = ctx->process_id;
    data->controlling_process_pid = 0;
    data->port = 0;
    data->active = true;
    data->binary = true;
    data->buffer = 512;

    list_append(&platform->sockets_list_head, &data->sockets_head);

    data->passive_receiver_process_pid = 0;
    data->passive_ref_ticks = 0;

    ctx->platform_data = data;
}

static struct TCPServerSocketData *tcp_server_socket_data_new(Context *ctx, struct netconn *conn,
    struct ESP32PlatformData *platform)
{
    struct TCPServerSocketData *tcp_data = malloc(sizeof(struct TCPServerSocketData));
    if (IS_NULL_PTR(tcp_data)) {
        return NULL;
    }
    socket_data_init(&tcp_data->socket_data, ctx, conn, TCPServerSocket, platform);
    tcp_data->ready_connections = 0;
    list_init(&tcp_data->accepters_list_head);

    return tcp_data;
}

static struct TCPClientSocketData *tcp_client_socket_data_new(Context *ctx, struct netconn *conn,
    struct ESP32PlatformData *platform, int32_t controlling_process_pid)
{
    struct TCPClientSocketData *tcp_data = malloc(sizeof(struct TCPClientSocketData));
    if (IS_NULL_PTR(tcp_data)) {
        return NULL;
    }
    socket_data_init(&tcp_data->socket_data, ctx, conn, TCPClientSocket, platform);
    tcp_data->socket_data.controlling_process_pid = controlling_process_pid;

    return tcp_data;
}

static struct UDPSocketData *udp_socket_data_new(Context *ctx, struct netconn *conn,
    struct ESP32PlatformData *platform, int32_t controlling_process_pid)
{
    struct UDPSocketData *udp_data = malloc(sizeof(struct UDPSocketData));
    if (IS_NULL_PTR(udp_data)) {
        return NULL;
    }
    socket_data_init(&udp_data->socket_data, ctx, conn, UDPSocket, platform);
    udp_data->socket_data.controlling_process_pid = controlling_process_pid;

    return udp_data;
}

static term accept_conn(Context *ctx, struct TCPServerSocketData *tcp_data, struct TCPServerAccepter *accepter)
{
    TRACE("Going to accept a TCP connection\n");
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = ctx->global->platform_data;

    struct netconn *accepted_conn;
    err_t status = netconn_accept(tcp_data->socket_data.conn, &accepted_conn);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "accept error: %i on %p\n", status, (void *) tcp_data->socket_data.conn);
        return term_invalid_term();
    }

    int32_t pid = accepter->accepting_process_pid;

    TRACE("accepted conn: %p\n", (void *) accepted_conn);

    Context *new_ctx = context_new(glb);
    new_ctx->native_handler = socket_consume_mailbox;

    term socket_pid = term_from_local_process_id(new_ctx->process_id);

    struct TCPClientSocketData *new_tcp_data = tcp_client_socket_data_new(new_ctx, accepted_conn, platform, pid);
    if (IS_NULL_PTR(new_tcp_data)) {
        AVM_ABORT();
    }
    new_tcp_data->socket_data.active = tcp_data->socket_data.active;
    new_tcp_data->socket_data.binary = tcp_data->socket_data.binary;
    new_tcp_data->socket_data.buffer = tcp_data->socket_data.buffer;

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    term ref = term_from_ref_ticks(accepter->ref_ticks, &ctx->heap);
    term return_tuple = term_alloc_tuple(2, &ctx->heap);

    free(accepter);

    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, socket_pid);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, result_tuple);

    return return_tuple;
}

static void do_accept(Context *ctx, term msg)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    term ref = term_get_tuple_element(msg, 1);

    struct TCPServerAccepter *accepter = malloc(sizeof(struct TCPServerAccepter));
    accepter->accepting_process_pid = pid;
    accepter->ref_ticks = term_to_ref_ticks(ref);
    list_append(&tcp_data->accepters_list_head, &accepter->accepter_head);

    if (tcp_data->ready_connections) {
        TRACE("accepting existing connections.\n");

        struct ListHead *accepter_head;
        struct ListHead *tmp;
        struct TCPServerAccepter *accepter = NULL;
        MUTABLE_LIST_FOR_EACH (accepter_head, tmp, &tcp_data->accepters_list_head) {
            //TODO: check if is alive here
            if (1) {
                accepter = GET_LIST_ENTRY(accepter_head, struct TCPServerAccepter, accepter_head);
                list_remove(accepter_head);
            }
        }

        if (accepter) {
            term return_tuple = accept_conn(ctx, tcp_data, accepter);
            if (!term_is_invalid_term(return_tuple)) {
                globalcontext_send_message(ctx->global, accepter->accepting_process_pid, return_tuple);
            }
            tcp_data->ready_connections--;
        }
    }
}

// When this method is called, ensure free was called with REPLY_SIZE
#define REPLY_SIZE (TUPLE_SIZE(2) + REF_SIZE)
static void do_send_reply(Context *ctx, term reply, uint64_t ref_ticks, int32_t pid)
{
    GlobalContext *glb = ctx->global;
    term reply_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(reply_tuple, 0, term_from_ref_ticks(ref_ticks, &ctx->heap));
    term_put_tuple_element(reply_tuple, 1, reply);
    globalcontext_send_message(glb, pid, reply_tuple);
}

static void do_send_passive_reply(Context *ctx, struct SocketData *socket_data, term reply)
{
    do_send_reply(ctx, reply, socket_data->passive_ref_ticks, socket_data->passive_receiver_process_pid);
    socket_data->passive_receiver_process_pid = 0;
    socket_data->passive_ref_ticks = 0;
}

// Encode both LWIP errors and some internal errors
// (for example ealready if caller tries to call a passive socket twice)
static term lwip_error_atom(GlobalContext *glb, err_t status)
{
    switch (status) {
        case ERR_MEM:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "err_mem"));
        case ERR_BUF:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "err_buf"));
        case ERR_TIMEOUT:
            return globalcontext_make_atom(glb, ATOM_STR("\xb", "err_timeout"));
        case ERR_USE:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "err_use"));
        case ERR_ALREADY:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "ealready"));
        case ERR_ARG:
            return BADARG_ATOM;
        default:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "unknown"));
    }
}

static void do_send_error_reply(Context *ctx, err_t status, uint64_t ref_ticks, int32_t pid)
{
    GlobalContext *glb = ctx->global;
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    term reason_atom = lwip_error_atom(glb, status);
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason_atom);
    do_send_reply(ctx, error_tuple, ref_ticks, pid);
}

static void do_send_socket_error(Context *ctx, err_t status)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;
    if (socket_data->active) {
        // udp active sockets do not send errors
        if (socket_data->type != UDPSocket) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
                AVM_ABORT();
            }
            term reason_atom = lwip_error_atom(glb, status);
            term result_tuple = term_alloc_tuple(3, &ctx->heap);
            term_put_tuple_element(result_tuple, 0, globalcontext_make_atom(glb, tcp_error_atom));
            term_put_tuple_element(result_tuple, 1, term_from_local_process_id(ctx->process_id));
            term_put_tuple_element(result_tuple, 2, reason_atom);
            globalcontext_send_message(glb, socket_data->controlling_process_pid, result_tuple);
        }
    } else {
        do_send_error_reply(ctx, status, socket_data->passive_ref_ticks, socket_data->passive_receiver_process_pid);
        socket_data->passive_receiver_process_pid = 0;
        socket_data->passive_ref_ticks = 0;
    }
}

static void do_send_tcp_closed(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;
    if (socket_data->active) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, TCP_CLOSED_ATOM);
        term_put_tuple_element(result_tuple, 1, term_from_local_process_id(ctx->process_id));
        globalcontext_send_message(glb, socket_data->controlling_process_pid, result_tuple);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, CLOSED_ATOM);
        do_send_passive_reply(ctx, socket_data, error_tuple);
    }
}

static void do_tcp_server_netconn_event(Context *ctx)
{
    TRACE("do_tcp_server_netconn_event\n");
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    struct ListHead *accepter_head;
    struct ListHead *tmp;
    struct TCPServerAccepter *accepter = NULL;
    MUTABLE_LIST_FOR_EACH (accepter_head, tmp, &tcp_data->accepters_list_head) {
        //TODO: is alive here
        accepter = GET_LIST_ENTRY(accepter_head, struct TCPServerAccepter, accepter_head);
        list_remove(accepter_head);
    }

    if (accepter) {
        term return_tuple = accept_conn(ctx, tcp_data, accepter);
        globalcontext_send_message(ctx->global, accepter->accepting_process_pid, return_tuple);
    } else {
        tcp_data->ready_connections++;
    }
}

static NativeHandlerResult do_receive_data(Context *ctx)
{
    // Common case where socket is active or passive receiver is waiting
    struct netbuf *buf = NULL;
    struct SocketData *socket_data = ctx->platform_data;
    err_t status = netconn_recv(socket_data->conn, &buf);
    socket_data->not_blocking = false;
    if (status != ERR_OK) {
        if (socket_data->type == TCPClientSocket) {
            // Close socket in case of errors or finish closing if it's closed
            // on the other end.
            list_remove(&socket_data->sockets_head);
            if (UNLIKELY(netconn_close(socket_data->conn) != ERR_OK)) {
                TRACE("do_receive_data: netconn_close failed");
            }
            if (UNLIKELY(netconn_delete(socket_data->conn) != ERR_OK)) {
                TRACE("do_receive_data: netconn_delete failed");
            }
            socket_data->conn = NULL;
        }
        if (socket_data->type == TCPClientSocket && (status == ERR_CLSD || status == ERR_CONN)) {
            do_send_tcp_closed(ctx);
        } else {
            TRACE("do_receive_data: netconn_recv error: %i\n", status);
            // Send an error packet.
            do_send_socket_error(ctx, status);
        }
        return NativeTerminate;
    }

    // This only retrieves the first chunk of buf
    netbuf_first(buf);
    void *data;
    u16_t data_len;
    status = netbuf_data(buf, &data, &data_len);
    if (UNLIKELY(status != ERR_OK)) {
        TRACE("do_receive_data: netbuf_data error: %i\n", status);
        do_send_socket_error(ctx, status);
        return NativeContinue;
    }

    TRACE("%*s\n", (int) data_len, (char *) data);

    int recv_terms_size;
    if (socket_data->binary) {
        recv_terms_size = term_binary_data_size_in_terms(data_len) + BINARY_HEADER_SIZE;
    } else {
        recv_terms_size = data_len * 2;
    }

    int tuples_size;
    if (socket_data->active) {
        // tuples_size = 4 (result_tuple size)
        tuples_size = TUPLE_SIZE(3);
    } else {
        // tuples_size = 3 (ok_tuple size)
        tuples_size = TUPLE_SIZE(2) + REPLY_SIZE;
    }
    if (UNLIKELY(memory_ensure_free(ctx, tuples_size + recv_terms_size) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }

    term recv_data;
    if (socket_data->binary) {
        recv_data = term_create_uninitialized_binary(data_len, &ctx->heap, ctx->global);
        memcpy((void *) term_binary_data(recv_data), data, data_len);
    } else {
        recv_data = term_from_string((const uint8_t *) data, data_len, &ctx->heap);
    }

    if (netbuf_next(buf) == 0) {
        TRACE("do_receive_data: netbuf error : got more parts\n");
        do_send_socket_error(ctx, ERR_BUF);
        return NativeContinue;
    }

    netbuf_delete(buf);

    if (socket_data->active) {
        term active_tuple = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(active_tuple, 0, socket_data->type == TCPClientSocket ? TCP_ATOM : UDP_ATOM);
        term_put_tuple_element(active_tuple, 1, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(active_tuple, 2, recv_data);
        globalcontext_send_message(ctx->global, socket_data->controlling_process_pid, active_tuple);
        TRACE("sent received to active process (pid=%d): ", socket_data->controlling_process_pid);
        #ifdef ENABLE_TRACE
            term_display(stdout, active_tuple, ctx);
        #endif
        TRACE("\n");
    } else {
        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, recv_data);
        do_send_passive_reply(ctx, socket_data, ok_tuple);
        TRACE("sent received to passive caller (pid=%d): ", socket_data->passive_receiver_process_pid);
        #ifdef ENABLE_TRACE
            term_display(stdout, ok_tuple, ctx);
        #endif
        TRACE("\n");
    }

    return NativeContinue;
}

static NativeHandlerResult do_data_netconn_event(Context *ctx)
{
    TRACE("do_data_netconn_event\n");
    struct SocketData *socket_data = ctx->platform_data;

    if (!socket_data->active && socket_data->passive_receiver_process_pid == 0) {
        // netconn_recv will not block
        socket_data->not_blocking = true;
        return NativeContinue;
    }
    return do_receive_data(ctx);
}

static NativeHandlerResult do_netconn_event(Context *ctx)
{
    NativeHandlerResult result = NativeContinue;
    struct SocketData *socket_data = ctx->platform_data;
    if (socket_data->type == TCPServerSocket) {
        do_tcp_server_netconn_event(ctx);
    } else {
        result = do_data_netconn_event(ctx);
    }
    return result;
}

static bool bool_term_to_bool(term b, bool *ok)
{
    switch (b) {
        case TRUE_ATOM:
            *ok = true;
            return true;

        case FALSE_ATOM:
            *ok = true;
            return false;

        default:
            return false;
            *ok = false;
    }
}

static void do_connect(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term address_term = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term binary_term = interop_proplist_get_value(params, BINARY_ATOM);
    term active_term = interop_proplist_get_value(params, ACTIVE_ATOM);
    term controlling_process_term = interop_proplist_get_value(params, CONTROLLING_PROCESS_ATOM);

    bool ok = term_is_pid(controlling_process_term);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    int32_t controlling_process_pid = term_to_local_process_id(controlling_process_term);
    int ok_int;
    char *address_string = interop_term_to_string(address_term, &ok_int);
    if (UNLIKELY(!ok_int)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    avm_int_t port = term_to_int(port_term);
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }

    if (UNLIKELY(memory_ensure_free(ctx, REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }

    TRACE("tcp: connecting to: %s\n", address_string);

    struct ip_addr remote_ip;
    //TODO: use dns_gethostbyname instead
    err_t status = netconn_gethostbyname(address_string, &remote_ip);
    if (UNLIKELY(status != ERR_OK)) {
        free(address_string);
        TRACE("tcp: host resolution failed.\n");
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    TRACE("tcp: host resolved.\n");

    free(address_string);

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        AVM_ABORT();
    }

    status = netconn_connect(conn, &remote_ip, port);
    if (UNLIKELY(status != ERR_OK)) {
        TRACE("tcp: failed connect: %i\n", status);
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    TRACE("tcp: connected.\n");

    struct TCPClientSocketData *tcp_data = tcp_client_socket_data_new(ctx, conn, platform, controlling_process_pid);
    if (IS_NULL_PTR(tcp_data)) {
        AVM_ABORT();
    }
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;

    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

static void do_listen(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term backlog_term = interop_proplist_get_value(params, BACKLOG_ATOM);
    term binary_term = interop_proplist_get_value(params, BINARY_ATOM);
    term active_term = interop_proplist_get_value(params, ACTIVE_ATOM);
    term buffer_term = interop_proplist_get_value(params, BUFFER_ATOM);

    avm_int_t port = term_to_int(port_term);
    avm_int_t backlog = term_to_int(backlog_term);
    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }

    avm_int_t buffer = term_to_int(buffer_term);

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);

    err_t status = netconn_bind(conn, IP_ADDR_ANY, port);
    if (UNLIKELY(status != ERR_OK)) {
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    ip_addr_t naddr;
    u16_t nport;
    status = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(status != ERR_OK)) {
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    status = netconn_listen_with_backlog(conn, backlog);
    if (UNLIKELY(status != ERR_OK)) {
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    struct TCPServerSocketData *tcp_data = tcp_server_socket_data_new(ctx, conn, platform);
    if (IS_NULL_PTR(tcp_data)) {
        AVM_ABORT();
    }
    tcp_data->socket_data.port = nport;
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;
    tcp_data->socket_data.buffer = buffer;

    if (UNLIKELY(memory_ensure_free(ctx, REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

void do_udp_open(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term binary_term = interop_proplist_get_value(params, BINARY_ATOM);
    term active_term = interop_proplist_get_value(params, ACTIVE_ATOM);
    term controlling_process = interop_proplist_get_value(params, CONTROLLING_PROCESS_ATOM);

    avm_int_t port = term_to_int(port_term);
    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_UDP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        fprintf(stderr, "failed to open conn\n");
        AVM_ABORT();
    }

    struct UDPSocketData *udp_data = udp_socket_data_new(ctx, conn, platform, controlling_process);
    if (IS_NULL_PTR(udp_data)) {
        AVM_ABORT();
    }
    udp_data->socket_data.active = active;
    udp_data->socket_data.binary = binary;

    if (port != 0) {
        err_t status = netconn_bind(conn, IP_ADDR_ANY, port);
        if (UNLIKELY(status != ERR_OK)) {
            do_send_error_reply(ctx, status, ref_ticks, pid);
            return;
        }
    }

    ip_addr_t naddr;
    u16_t nport;
    err_t status = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(status != ERR_OK)) {
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }
    udp_data->socket_data.port = nport;

    if (UNLIKELY(memory_ensure_free(ctx, REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

// Required for compatibility with existing erlang libraries
// TODO: remove this when not required anymore
static void do_init(Context *ctx, term msg)
{
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    if (interop_proplist_get_value_default(params, LISTEN_ATOM, FALSE_ATOM) == TRUE_ATOM) {
        TRACE("listen\n");
        do_listen(ctx, msg);

    } else if (interop_proplist_get_value_default(params, CONNECT_ATOM, FALSE_ATOM) == TRUE_ATOM) {
        TRACE("connect\n");
        do_connect(ctx, msg);

    } else if (interop_proplist_get_value_default(params, PROTO_ATOM, FALSE_ATOM) == UDP_ATOM) {
        TRACE("udp_open\n");
        do_udp_open(ctx, msg);
    }
}

static void do_send(Context *ctx, term msg)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));
    term cmd = term_get_tuple_element(msg, 2);

    term data = term_get_tuple_element(cmd, 1);

    size_t buffer_size;
    switch (interop_iolist_size(data, &buffer_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            fprintf(stderr, "error: failed alloc.\n");
            return;
        case InteropBadArg:
            fprintf(stderr, "error: invalid iolist.\n");
            return;
    }
    void *buffer = malloc(buffer_size);
    switch (interop_write_iolist(data, buffer)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(buffer);
            fprintf(stderr, "error: failed alloc.\n");
            return;
        case InteropBadArg:
            free(buffer);
            fprintf(stderr, "error: invalid iolist.\n");
            return;
    }
    err_t status = netconn_write(tcp_data->socket_data.conn, buffer, buffer_size, NETCONN_COPY);
    if (UNLIKELY(status != ERR_OK)) {
        fprintf(stderr, "write error: %i\n", status);
        return;
    }

    free(buffer);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

static void do_sendto(Context *ctx, term msg)
{
    struct UDPSocketData *udp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));
    term cmd = term_get_tuple_element(msg, 2);

    term dest_addr_term = term_get_tuple_element(cmd, 1);
    term dest_port_term = term_get_tuple_element(cmd, 2);
    term data = term_get_tuple_element(cmd, 3);

    size_t buffer_size;
    switch (interop_iolist_size(data, &buffer_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            do_send_error_reply(ctx, ERR_MEM, ref_ticks, pid);
            return;
        case InteropBadArg:
            do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
            return;
    }
    void *buffer = malloc(buffer_size);
    switch (interop_write_iolist(data, buffer)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(buffer);
            do_send_error_reply(ctx, ERR_MEM, ref_ticks, pid);
            return;
        case InteropBadArg:
            free(buffer);
            do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
            return;
    }

    ip_addr_t ip4addr;
    tuple_to_ip_addr(dest_addr_term, &ip4addr);
    uint16_t destport = term_to_int32(dest_port_term);

    struct netbuf *sendbuf = netbuf_new();

    err_t status = netbuf_ref(sendbuf, buffer, buffer_size);
    if (UNLIKELY(status != ERR_OK)) {
        netbuf_delete(sendbuf);
        free(buffer);
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    status = netconn_sendto(udp_data->socket_data.conn, sendbuf, &ip4addr, destport);
    netbuf_delete(sendbuf);
    free(buffer);

    if (UNLIKELY(status != ERR_OK)) {
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    if (UNLIKELY(memory_ensure_free(ctx, REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

static void do_close(Context *ctx, term msg)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    err_t close_res = netconn_close(tcp_data->socket_data.conn);
    err_t delete_res = netconn_delete(tcp_data->socket_data.conn);

    tcp_data->socket_data.conn = NULL;
    list_remove(&tcp_data->socket_data.sockets_head);

    if (UNLIKELY(close_res != ERR_OK)) {
        do_send_error_reply(ctx, close_res, ref_ticks, pid);
    } else if (UNLIKELY(delete_res != ERR_OK)) {
        do_send_error_reply(ctx, delete_res, ref_ticks, pid);
    } else {
        do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
    }
}

static NativeHandlerResult do_recvfrom(Context *ctx, term msg)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    // We cannot stack blocked queries
    if (socket_data->passive_receiver_process_pid != 0) {
        do_send_error_reply(ctx, ERR_ALREADY, ref_ticks, pid);
        return NativeContinue;
    }

    socket_data->passive_receiver_process_pid = pid;
    socket_data->passive_ref_ticks = ref_ticks;

    if (socket_data->not_blocking) {
        return do_receive_data(ctx);
    }

    return NativeContinue;
}

static void do_get_port(Context *ctx, term msg)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    if (socket_data->port == 0) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
    } else {
        // 3 (error_ok_tuple) + 3 (result_tuple)
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, term_from_int(socket_data->port));
        do_send_reply(ctx, ok_tuple, ref_ticks, pid);
    }
}

static void do_sockname(Context *ctx, term msg)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_addr(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        do_send_error_reply(ctx, result, ref_ticks, pid);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + 8 + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        return_msg = term_alloc_tuple(2, &ctx->heap);
        term addr_term = socket_addr_to_tuple(&ctx->heap, &addr);
        term port_term = term_from_int(port);
        term address_port_term = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(address_port_term, 0, addr_term);
        term_put_tuple_element(address_port_term, 1, port_term);
        term_put_tuple_element(return_msg, 0, OK_ATOM);
        term_put_tuple_element(return_msg, 1, address_port_term);
        do_send_reply(ctx, return_msg, ref_ticks, pid);
    }
}

static void do_peername(Context *ctx, term msg)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_peer(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        do_send_error_reply(ctx, result, ref_ticks, pid);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + 8 + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        return_msg = term_alloc_tuple(2, &ctx->heap);
        term addr_term = socket_addr_to_tuple(&ctx->heap, &addr);
        term port_term = term_from_int(port);
        term address_port_term = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(address_port_term, 0, addr_term);
        term_put_tuple_element(address_port_term, 1, port_term);
        term_put_tuple_element(return_msg, 0, OK_ATOM);
        term_put_tuple_element(return_msg, 1, address_port_term);
        do_send_reply(ctx, return_msg, ref_ticks, pid);
    }
}

static void do_controlling_process(Context *ctx, term msg)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(term_get_tuple_element(msg, 0));
    uint64_t ref_ticks = term_to_ref_ticks(term_get_tuple_element(msg, 1));

    term cmd = term_get_tuple_element(msg, 2);

    term new_pid_term = term_get_tuple_element(cmd, 1);
    if (UNLIKELY(!term_is_pid(new_pid_term))) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
    } else {
        term return_msg;

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }

        if (UNLIKELY(pid != socket_data->controlling_process_pid)) {
            return_msg = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(return_msg, 0, ERROR_ATOM);
            term_put_tuple_element(return_msg, 1, NOT_OWNER_ATOM);
        } else {
            int32_t new_pid = term_to_local_process_id(new_pid_term);
            socket_data->controlling_process_pid = new_pid;
            return_msg = OK_ATOM;
        }
        do_send_reply(ctx, return_msg, ref_ticks, pid);
    }
}

static NativeHandlerResult socket_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;

    while (mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        term msg = message->message;

        TRACE("message: ");
        #ifdef ENABLE_TRACE
            term_display(stdout, msg, ctx);
        #endif
        TRACE("\n");

        if (msg == globalcontext_make_atom(glb, netconn_event_internal)) {
            NativeHandlerResult result = do_netconn_event(ctx);
            if (result == NativeTerminate) {
                // We don't need to remove message.
                return NativeTerminate;
            }
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        term cmd = term_get_tuple_element(msg, 2);
        term cmd_name = term_get_tuple_element(cmd, 0);

        switch (cmd_name) {
            //TODO: remove this
            case INIT_ATOM:
                TRACE("init\n");
                do_init(ctx, msg);
                break;

            case SENDTO_ATOM:
                TRACE("sendto\n");
                do_sendto(ctx, msg);
                break;

            case SEND_ATOM:
                TRACE("send\n");
                do_send(ctx, msg);
                break;

            case RECVFROM_ATOM:
                TRACE("recvfrom\n");
                if (do_recvfrom(ctx, msg) == NativeTerminate) {
                    return NativeTerminate;
                }
                break;

            case RECV_ATOM:
                TRACE("recv\n");
                if (do_recvfrom(ctx, msg) == NativeTerminate) {
                    return NativeTerminate;
                }
                break;

            case ACCEPT_ATOM:
                TRACE("accept\n");
                do_accept(ctx, msg);
                break;

            case CLOSE_ATOM:
                TRACE("close\n");
                do_close(ctx, msg);
                // We don't need to remove message.
                return NativeTerminate;

            case GET_PORT_ATOM:
                TRACE("get_port\n");
                do_get_port(ctx, msg);
                break;

            case SOCKNAME_ATOM:
                TRACE("sockname\n");
                do_sockname(ctx, msg);
                break;

            case PEERNAME_ATOM:
                TRACE("peername\n");
                do_peername(ctx, msg);
                break;

            case CONTROLLING_PROCESS_ATOM:
                TRACE("controlling_process\n");
                do_controlling_process(ctx, msg);
                break;

            default:
                TRACE("badarg\n");
                break;
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return NativeContinue;
}

Context *socket_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

REGISTER_PORT_DRIVER(socket, socket_driver_init, socket_driver_create_port)

#endif
