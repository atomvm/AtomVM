/*
 * This file is part of AtomVM.
 *
 * Copyright 2018,2019 Davide Bettio <davide@uninstall.it>
 * Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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

#include <stdbool.h>
#include <string.h>

#include <atom.h>
#include <context.h>
#include <esp32_sys.h>
#include <globalcontext.h>
#include <interop.h>
#include <mailbox.h>
#include <port.h>
#include <scheduler.h>
#include <sdkconfig.h>
#include <sys.h>
#include <term.h>
#include <utils.h>

#include <esp_log.h>
#include <lwip/api.h>
#include <lwip/inet.h>
#include <lwip/ip_addr.h>
#include <tcpip_adapter.h>

//#define ENABLE_TRACE 1
#include "trace.h"

#define TAG "socket_driver"

static void tcp_server_handler(Context *ctx);
static void tcp_client_handler(Context *ctx);
static void udp_handler(Context *ctx);
static void socket_consume_mailbox(Context *ctx);

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
    Context *ctx;
    enum socket_type type;
    term controlling_process_pid;

    term passive_receiver_process_pid;
    uint64_t passive_ref_ticks;

    int avail_bytes;

    uint16_t port;

    size_t buffer;
    bool active : 1;
    bool binary : 1;
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
    term accepting_process_pid;
    uint64_t ref_ticks;
};

struct UDPSocketData
{
    struct SocketData socket_data;
};

struct NetconnEvent
{
    struct netconn *netconn;
    enum netconn_evt evt;
    u16_t len;
};

static const char *const active_atom = ATOM_STR("\x6", "active");
static const char *const address_atom = ATOM_STR("\x7", "address");
static const char *const backlog_atom = ATOM_STR("\x7", "backlog");
static const char *const binary_atom = ATOM_STR("\x6", "binary");
static const char *const buffer_atom = ATOM_STR("\x6", "buffer");
static const char *const connect_atom = ATOM_STR("\x7", "connect");
static const char *const controlling_process_atom = ATOM_STR("\x13", "controlling_process");
static const char *const ealready_atom = ATOM_STR("\x8", "ealready");
static const char *const listen_atom = ATOM_STR("\x6", "listen");
static const char *const not_owner_atom = ATOM_STR("\x9", "not_owner");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const proto_atom = ATOM_STR("\x5", "proto");
static const char *const tcp_atom = ATOM_STR("\x3", "tcp");
static const char *const tcp_closed_atom = ATOM_STR("\xA", "tcp_closed");
static const char *const udp_atom = ATOM_STR("\x3", "udp");

enum socket_cmd
{
    SocketInvalidCmd = 0,
    SocketInitCmd,
    SocketSendToCmd,
    SocketSendCmd,
    SocketRecvFromCmd,
    SocketRecvCmd,
    SocketAcceptCmd,
    SocketCloseCmd,
    SocketGetPortCmd,
    SocketSockNameCmd,
    SocketPeerNameCmd,
    SocketControllingProcessCmd
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "init"), SocketInitCmd },
    { ATOM_STR("\x6", "sendto"), SocketSendToCmd },
    { ATOM_STR("\x4", "send"), SocketSendCmd },
    { ATOM_STR("\x8", "recvfrom"), SocketRecvFromCmd },
    { ATOM_STR("\x4", "recv"), SocketRecvCmd },
    { ATOM_STR("\x6", "accept"), SocketAcceptCmd },
    { ATOM_STR("\x5", "close"), SocketCloseCmd },
    { ATOM_STR("\x8", "get_port"), SocketGetPortCmd },
    { ATOM_STR("\x8", "sockname"), SocketSockNameCmd },
    { ATOM_STR("\x8", "peername"), SocketPeerNameCmd },
    { ATOM_STR("\x13", "controlling_process"), SocketControllingProcessCmd },
    SELECT_INT_DEFAULT(SocketInvalidCmd)
};

xQueueHandle netconn_events = NULL;

// TODO use globalcontext_make_atom as part of merge into SMP branch
static inline term make_atom(GlobalContext *global, AtomString atom_str)
{
    int global_atom_index = globalcontext_insert_atom(global, atom_str);
    return term_from_atom_index(global_atom_index);
}

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

static term socket_addr_to_tuple(Context *ctx, ip_addr_t *addr)
{
    term addr_tuple;

    switch (IP_GET_TYPE(addr)) {
        case IPADDR_TYPE_V4: {
            uint8_t ad1 = ip4_addr1(&(addr->u_addr.ip4));
            uint8_t ad2 = ip4_addr2(&(addr->u_addr.ip4));
            uint8_t ad3 = ip4_addr3(&(addr->u_addr.ip4));
            uint8_t ad4 = ip4_addr4(&(addr->u_addr.ip4));
            addr_tuple = term_alloc_tuple(4, ctx);
            term_put_tuple_element(addr_tuple, 0, term_from_int11(ad1));
            term_put_tuple_element(addr_tuple, 1, term_from_int11(ad2));
            term_put_tuple_element(addr_tuple, 2, term_from_int11(ad3));
            term_put_tuple_element(addr_tuple, 3, term_from_int11(ad4));
            break;
        }

        case IPADDR_TYPE_V6:
            // TODO: implement IPv6
            addr_tuple = term_invalid_term();
            break;

        default:
            addr_tuple = term_invalid_term();
    }

    return addr_tuple;
}

void socket_events_handler(EventListener *listener)
{
    TRACE("socket_events_handler\n");

    GlobalContext *glb = listener->data;
    struct ESP32PlatformData *platform = glb->platform_data;

    struct NetconnEvent event;
    while (xQueueReceive(netconn_events, &event, 1) == pdTRUE) {
        TRACE("Got netconn event: %p %i %i\n", event.netconn, event.evt, event.len);

        struct netconn *netconn = event.netconn;
        enum netconn_evt evt = event.evt;
        u16_t len = event.len;

        struct SocketData *socket = NULL;
        struct ListHead *socket_head;
        LIST_FOR_EACH (socket_head, &platform->sockets_list_head) {
            struct SocketData *current_socket = GET_LIST_ENTRY(socket_head, struct SocketData, sockets_head);
            if (current_socket->conn == netconn) {
                socket = current_socket;
            }
        }

        if (socket) {
            if ((evt == NETCONN_EVT_RCVMINUS) /*&& (len != 0)*/) {
                TRACE("Ignoring RCVMINUS event\n");
                continue;
            }

            if ((evt == NETCONN_EVT_SENDMINUS) || (evt == NETCONN_EVT_SENDPLUS)) {
                TRACE("Ignoring SENDMINUS/SENDPLUS event\n");
                continue;
            }

            if (evt == NETCONN_EVT_ERROR) {
                TRACE("Ignoring ERROR event\n");
                continue;
            }

            socket->avail_bytes += len;

            switch (socket->type) {
                case TCPServerSocket:
                    tcp_server_handler(socket->ctx);
                    break;

                case TCPClientSocket:
                    tcp_client_handler(socket->ctx);
                    break;

                case UDPSocket:
                    udp_handler(socket->ctx);
                    break;

                default:
                    ESP_LOGE(TAG, "bug: unknown socket type.");
            }

        } else {
            TRACE("Got event for unknown conn: %p, evt: %i, len: %i\n", netconn, (int) evt, (int) len);
        }
    }
}

static void socket_data_init(struct SocketData *data, Context *ctx, struct netconn *conn,
    enum socket_type type, struct ESP32PlatformData *platform)
{
    data->type = type;
    data->conn = conn;
    data->ctx = ctx;
    data->controlling_process_pid = term_invalid_term();
    data->port = 0;
    data->active = true;
    data->binary = true;
    data->buffer = 512;

    list_append(&platform->sockets_list_head, &data->sockets_head);

    data->passive_receiver_process_pid = term_invalid_term();
    data->passive_ref_ticks = 0;

    data->avail_bytes = 0;

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
    struct ESP32PlatformData *platform, term controlling_process_pid)
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
    struct ESP32PlatformData *platform, term controlling_process_pid)
{
    struct UDPSocketData *udp_data = malloc(sizeof(struct UDPSocketData));
    if (IS_NULL_PTR(udp_data)) {
        return NULL;
    }
    socket_data_init(&udp_data->socket_data, ctx, conn, UDPSocket, platform);
    udp_data->socket_data.controlling_process_pid = controlling_process_pid;

    return udp_data;
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(global, local_process_id);
    mailbox_send(target, message);
}

void ESP_IRAM_ATTR socket_callback(struct netconn *netconn, enum netconn_evt evt, u16_t len)
{
    struct NetconnEvent event = {
        .netconn = netconn,
        .evt = evt,
        .len = len
    };

    BaseType_t xHigherPriorityTaskWoken;
    int result = xQueueSendFromISR(netconn_events, &event, &xHigherPriorityTaskWoken);
    if (result != pdTRUE) {
        ESP_LOGE(TAG, "socket: failed to enqueue: %i to netconn_events.", result);
    }

    void *netconn_events_ptr = &netconn_events;
    result = xQueueSendFromISR(event_queue, &netconn_events_ptr, &xHigherPriorityTaskWoken);
    if (result != pdTRUE) {
        ESP_LOGE(TAG, "socket: failed to enqueue: %i to event_queue.", result);
    }
}

void accept_conn(struct TCPServerAccepter *accepter, Context *ctx)
{
    TRACE("Going to accept a TCP connection\n");

    struct TCPServerSocketData *tcp_data = ctx->platform_data;
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;
    term pid = accepter->accepting_process_pid;

    // {Ref, {ok, pid} | {error, int()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }
    term ref = term_from_ref_ticks(accepter->ref_ticks, ctx);

    struct netconn *accepted_conn;
    err_t err = netconn_accept(tcp_data->socket_data.conn, &accepted_conn);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_accept failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
    }

    TRACE("accepted conn: %p\n", accepted_conn);

    Context *new_ctx = context_new(glb);
    new_ctx->native_handler = socket_consume_mailbox;
    scheduler_make_waiting(glb, new_ctx);

    term socket_pid = term_from_local_process_id(new_ctx->process_id);

    struct TCPClientSocketData *new_tcp_data = tcp_client_socket_data_new(new_ctx, accepted_conn, platform, pid);
    if (IS_NULL_PTR(new_tcp_data)) {
        ESP_LOGE(TAG, "Unable to create new tcp client socket data");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    new_tcp_data->socket_data.active = tcp_data->socket_data.active;
    new_tcp_data->socket_data.binary = tcp_data->socket_data.binary;
    new_tcp_data->socket_data.buffer = tcp_data->socket_data.buffer;

    free(accepter);

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, socket_pid);

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, result_tuple);

    send_message(pid, return_tuple, glb);
}

static void do_accept(Context *ctx, term msg)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
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
            accept_conn(accepter, ctx);
            tcp_data->ready_connections--;
        }
    }
}

static void close_tcp_socket(Context *ctx, struct TCPClientSocketData *tcp_data)
{
    // {tcp_closed, pid()}
    size_t heap_size = TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    term pid = tcp_data->socket_data.controlling_process_pid;
    term msg = term_alloc_tuple(2, ctx);
    term_put_tuple_element(msg, 0, make_atom(ctx->global, tcp_closed_atom));
    term_put_tuple_element(msg, 1, term_from_local_process_id(pid));

    err_t err = netconn_delete(tcp_data->socket_data.conn);
    if (err != ERR_OK) {
        ESP_LOGW(TAG, "close_tcp_socket: netconn_delete failed");
    }
    tcp_data->socket_data.conn = NULL;
    list_remove(&tcp_data->socket_data.sockets_head);

    free(tcp_data);

    send_message(pid, msg, ctx->global);
    scheduler_terminate(ctx);

    return;
}

static void tcp_client_handler(Context *ctx)
{
    TRACE("tcp_client_handler\n");

    struct TCPClientSocketData *tcp_data = ctx->platform_data;
    GlobalContext *glb = ctx->global;

    if (!tcp_data->socket_data.active) {
        TRACE("tcp_client_handler: Not active socket.  Ignoring.\n");
        return;
    }

    if (!tcp_data->socket_data.avail_bytes) {
        TRACE("tcp_client_handler: No bytes to receive.\n");
        // NB. When the connection peer closes a connection, then no avail_bytes
        // are reported.  We verify that this is the expected behavior.
        close_tcp_socket(ctx, tcp_data);
        return;
    }

    struct netbuf *buf = NULL;
    err_t err = netconn_recv(tcp_data->socket_data.conn, &buf);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "tcp_client_handler: netconn_recv error: %i\n", err);
        close_tcp_socket(ctx, tcp_data);
        return;
    }

    void *data;
    u16_t data_len;
    err = netbuf_data(buf, &data, &data_len);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "tcp_client_handler: netbuf_data error: %i\n", err);
        close_tcp_socket(ctx, tcp_data);
        return;
    }

    tcp_data->socket_data.avail_bytes -= data_len;

    //HANDLE fragments here?

    TRACE("%*s\n", (int) data_len, (char *) data);

    int recv_terms_size;
    if (tcp_data->socket_data.binary) {
        recv_terms_size = term_binary_data_size_in_terms(data_len) + BINARY_HEADER_SIZE;
    } else {
        recv_terms_size = data_len * 2;
    }

    int tuples_size;
    if (tcp_data->socket_data.active) {
        // tuples_size = 5 (result_tuple size)
        tuples_size = 4;
    } else {
        // tuples_size = 3 (ok_tuple size) + 3 (result_tuple size)
        tuples_size = 3 + 3;
    }
    if (UNLIKELY(memory_ensure_free(ctx, tuples_size + recv_terms_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    term recv_data;
    if (tcp_data->socket_data.binary) {
        recv_data = term_create_uninitialized_binary(data_len, ctx);
        memcpy((void *) term_binary_data(recv_data), data, data_len);
    } else {
        recv_data = term_from_string((const uint8_t *) data, data_len, ctx);
    }

    netbuf_delete(buf);

    term pid = tcp_data->socket_data.controlling_process_pid;

    term result_tuple;
    if (tcp_data->socket_data.active) {
        result_tuple = term_alloc_tuple(3, ctx);
        term_put_tuple_element(result_tuple, 0, make_atom(ctx->global, tcp_atom));
        term_put_tuple_element(result_tuple, 1, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(result_tuple, 2, recv_data);

    } else {
        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, recv_data);

        result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(tcp_data->socket_data.passive_ref_ticks, ctx));
        term_put_tuple_element(result_tuple, 1, ok_tuple);

        pid = tcp_data->socket_data.passive_receiver_process_pid;

        tcp_data->socket_data.passive_receiver_process_pid = term_invalid_term();
        tcp_data->socket_data.passive_ref_ticks = 0;
    }

    TRACE("sending received: ");
    #ifdef ENABLE_TRACE
        term_display(stdout, result_tuple, ctx);
    #endif
    TRACE(" to ");
    #ifdef ENABLE_TRACE
        term_display(stdout, pid, ctx);
    #endif
    TRACE("\n");

    send_message(pid, result_tuple, glb);
}

static void tcp_server_handler(Context *ctx)
{
    TRACE("tcp_server_handler\n");

    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    struct ListHead *accepter_head;
    struct ListHead *tmp;
    struct TCPServerAccepter *accepter = NULL;
    MUTABLE_LIST_FOR_EACH (accepter_head, tmp, &tcp_data->accepters_list_head) {
        //TODO: is alive here
        if (1) {
            accepter = GET_LIST_ENTRY(accepter_head, struct TCPServerAccepter, accepter_head);
            list_remove(accepter_head);
        }
    }

    if (accepter) {
        accept_conn(accepter, ctx);

    } else {
        tcp_data->ready_connections++;
    }
}

static void udp_handler(Context *ctx)
{
    TRACE("udp_client_handler\n");

    struct UDPSocketData *udp_data = ctx->platform_data;
    GlobalContext *glb = ctx->global;

    struct SocketData *socket_data = &udp_data->socket_data;
    if (!socket_data->active && (socket_data->passive_receiver_process_pid == term_invalid_term())) {
        return;
    }

    if (!udp_data->socket_data.avail_bytes) {
        TRACE("No bytes to receive.\n");
        return;
    }

    struct netbuf *buf = NULL;
    err_t err = netconn_recv(udp_data->socket_data.conn, &buf);
    if (UNLIKELY(err != ERR_OK)) {
        //TODO
        ESP_LOGE(TAG, "tcp_client_handler error: %i", err);
        return;
    }

    void *data;
    u16_t data_len;
    err = netbuf_data(buf, &data, &data_len);
    if (UNLIKELY(err != ERR_OK)) {
        //TODO
        ESP_LOGE(TAG, "netbuf_data error: %i", err);
        return;
    }

    udp_data->socket_data.avail_bytes -= data_len;

    //HANDLE fragments here?

    TRACE("%*s\n", (int) data_len, (char *) data);

    int recv_terms_size;
    if (udp_data->socket_data.binary) {
        recv_terms_size = term_binary_data_size_in_terms(data_len) + BINARY_HEADER_SIZE;
    } else {
        recv_terms_size = data_len * 2;
    }

    int tuples_size;
    if (socket_data->active) {
        // tuples_size = 5 (addr size) + 6 (result_tuple size)
        tuples_size = 5 + 6;
    } else {
        // tuples_size = 4 (recv_ret size) + 5 (addr size) + 3 (ok_tuple size) + 3 (result_tuple size)
        tuples_size = 4 + 5 + 3 + 3;
    }
    if (UNLIKELY(memory_ensure_free(ctx, tuples_size + recv_terms_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    term recv_data;
    if (udp_data->socket_data.binary) {
        recv_data = term_create_uninitialized_binary(data_len, ctx);
        memcpy((void *) term_binary_data(recv_data), data, data_len);
    } else {
        recv_data = term_from_string((const uint8_t *) data, data_len, ctx);
    }

    term addr = socket_addr_to_tuple(ctx, netbuf_fromaddr(buf));
    term port = term_from_int32(netbuf_fromport(buf));

    netbuf_delete(buf);

    term pid = udp_data->socket_data.controlling_process_pid;

    term result_tuple;
    if (socket_data->active) {
        result_tuple = term_alloc_tuple(5, ctx);
        term_put_tuple_element(result_tuple, 0, make_atom(ctx->global, udp_atom));
        term_put_tuple_element(result_tuple, 1, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(result_tuple, 2, addr);
        term_put_tuple_element(result_tuple, 3, port);
        term_put_tuple_element(result_tuple, 4, recv_data);

    } else {
        term recv_ret = term_alloc_tuple(3, ctx);
        term_put_tuple_element(recv_ret, 0, addr);
        term_put_tuple_element(recv_ret, 1, port);
        term_put_tuple_element(recv_ret, 2, recv_data);

        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, recv_ret);

        result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(socket_data->passive_ref_ticks, ctx));
        term_put_tuple_element(result_tuple, 1, ok_tuple);

        pid = socket_data->passive_receiver_process_pid;

        socket_data->passive_receiver_process_pid = term_invalid_term();
        socket_data->passive_ref_ticks = 0;
    }

    TRACE("sending received: ");
    #ifdef ENABLE_TRACE
        term_display(stdout, result_tuple, ctx);
    #endif
    TRACE(" to ");
     #ifdef ENABLE_TRACE
        term_display(stdout, pid, ctx);
    #endif
    TRACE("\n");

    send_message(pid, result_tuple, glb);
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

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term address_term = interop_kv_get_value(params, address_atom, ctx->global);
    term port_term = interop_kv_get_value(params, port_atom, ctx->global);
    term binary_term = interop_kv_get_value(params, binary_atom, ctx->global);
    term active_term = interop_kv_get_value(params, active_atom, ctx->global);
    term controlling_process_term = interop_kv_get_value(params, controlling_process_atom, ctx->global);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    int ok_int;
    char *address_string = interop_term_to_string(address_term, &ok_int);
    if (UNLIKELY(!ok_int)) {
        ESP_LOGE(TAG, "Bad address parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    if (!term_is_integer(port_term)) {
        ESP_LOGE(TAG, "Bad port parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    avm_int_t port = term_to_int(port_term);

    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad active parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad binary parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    TRACE("tcp: connecting to: %s\n", address_string);

    struct ip_addr remote_ip;
    //TODO: use dns_gethostbyname instead
    err_t err = netconn_gethostbyname(address_string, &remote_ip);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "Host resolution failed for address %s with err %i", address_string, err);
        free(address_string);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    TRACE("tcp: host resolved.\n");

    free(address_string);

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        ESP_LOGE(TAG, "Unable to create new netconn instance");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    err = netconn_connect(conn, &remote_ip, port);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_connect failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    TRACE("tcp: connected.\n");

    struct TCPClientSocketData *tcp_data = tcp_client_socket_data_new(ctx, conn, platform, controlling_process_term);
    if (IS_NULL_PTR(tcp_data)) {
        ESP_LOGE(TAG, "Unable to create new tcp client socket data");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

static void do_listen(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term port_term = interop_kv_get_value(params, port_atom, ctx->global);
    term backlog_term = interop_kv_get_value(params, backlog_atom, ctx->global);
    term binary_term = interop_kv_get_value(params, binary_atom, ctx->global);
    term active_term = interop_kv_get_value(params, active_atom, ctx->global);
    term buffer_term = interop_kv_get_value(params, buffer_atom, ctx->global);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    if (!term_is_integer(port_term)) {
        ESP_LOGE(TAG, "Bad port parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    avm_int_t port = term_to_int(port_term);

    if (!term_is_integer(backlog_term)) {
        ESP_LOGE(TAG, "Bad backlog parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    avm_int_t backlog = term_to_int(backlog_term);

    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad active parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad binary parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    if (!term_is_integer(buffer_term)) {
        ESP_LOGE(TAG, "Bad buffer parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    avm_int_t buffer = term_to_int(buffer_term);

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        ESP_LOGE(TAG, "Unable to create new netconn instance");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    err_t err = netconn_bind(conn, IP_ADDR_ANY, port);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_bind failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    ip_addr_t naddr;
    u16_t nport;
    err = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_getaddr failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    err = netconn_listen_with_backlog(conn, backlog);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_listen_with_backlog failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    struct TCPServerSocketData *tcp_data = tcp_server_socket_data_new(ctx, conn, platform);
    if (IS_NULL_PTR(tcp_data)) {
        ESP_LOGE(TAG, "Unable to create new tcp server socket data");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    tcp_data->socket_data.port = nport;
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;
    tcp_data->socket_data.buffer = buffer;

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

void do_udp_open(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term port_term = interop_kv_get_value(params, port_atom, ctx->global);
    term binary_term = interop_kv_get_value(params, binary_atom, ctx->global);
    term active_term = interop_kv_get_value(params, active_atom, ctx->global);
    term controlling_process_term = interop_kv_get_value(params, controlling_process_atom, ctx->global);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    if (!term_is_integer(port_term)) {
        ESP_LOGE(TAG, "Bad port parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    avm_int_t port = term_to_int(port_term);

    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad active parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Bad binary parameter");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_UDP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        ESP_LOGE(TAG, "Unable to create new netconn instance");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    struct UDPSocketData *udp_data = udp_socket_data_new(ctx, conn, platform, controlling_process_term);
    if (IS_NULL_PTR(udp_data)) {
        ESP_LOGE(TAG, "Unable to create new udp socket data");
        term error = port_create_error_tuple(ctx, UNDEFINED_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    udp_data->socket_data.active = active;
    udp_data->socket_data.binary = binary;

    if (port != 0) {
        err_t err = netconn_bind(conn, IP_ADDR_ANY, port);
        if (UNLIKELY(err != ERR_OK)) {
            ESP_LOGE(TAG, "netconn_bind failed with error %d", err);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
            return;
        }
    }

    ip_addr_t naddr;
    u16_t nport;
    err_t err = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_getaddr failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    udp_data->socket_data.port = nport;

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

// Required for compatibility with existing erlang libraries
// TODO: remove this when not required anymore
static void do_init(Context *ctx, term msg)
{
    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    if (interop_kv_get_value_default(params, listen_atom, FALSE_ATOM, ctx->global) == TRUE_ATOM) {
        TRACE("listen\n");
        do_listen(ctx, msg);

    } else if (interop_kv_get_value_default(params, connect_atom, FALSE_ATOM, ctx->global) == TRUE_ATOM) {
        TRACE("connect\n");
        do_connect(ctx, msg);

    } else if (interop_kv_get_value_default(params, proto_atom, FALSE_ATOM, ctx->global) == make_atom(ctx->global, udp_atom)) {
        TRACE("udp_open\n");
        do_udp_open(ctx, msg);
    }
}

static void do_send(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term data = term_get_tuple_element(cmd, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    int ok;
    int buffer_size = interop_iolist_size(data, &ok);
    if (UNLIKELY(!ok)) {
        ESP_LOGE(TAG, "Invalid iolist");
        term error = port_create_error_tuple(ctx, BADARG_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    void *buffer = malloc(buffer_size);
    if (IS_NULL_PTR(buffer)) {
        ESP_LOGE(TAG, "Insufficient space to allocate buffer of size %i", buffer_size);
        term error = port_create_error_tuple(ctx, MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    interop_write_iolist(data, buffer);

    err_t err = netconn_write(tcp_data->socket_data.conn, buffer, buffer_size, NETCONN_NOCOPY);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_write error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    free(buffer);

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

static void do_sendto(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct UDPSocketData *udp_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    term dest_addr_term = term_get_tuple_element(cmd, 1);
    term dest_port_term = term_get_tuple_element(cmd, 2);
    term data = term_get_tuple_element(cmd, 3);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    int ok;
    int buffer_size = interop_iolist_size(data, &ok);
    void *buffer = malloc(buffer_size);
    if (IS_NULL_PTR(buffer)) {
        ESP_LOGE(TAG, "Insufficient space to allocate buffer of size %i", buffer_size);
        term error = port_create_error_tuple(ctx, MEMORY_ATOM);
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    interop_write_iolist(data, buffer);

    ip_addr_t ip4addr;
    tuple_to_ip_addr(dest_addr_term, &ip4addr);
    uint16_t destport = term_to_int32(dest_port_term);

    struct netbuf *sendbuf = netbuf_new();

    err_t err = netbuf_ref(sendbuf, buffer, buffer_size);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netbuf_ref error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        netbuf_delete(sendbuf);
        return;
    }

    err = netconn_sendto(udp_data->socket_data.conn, sendbuf, &ip4addr, destport);
    if (UNLIKELY(err != ERR_OK)) {
        ESP_LOGE(TAG, "netconn_sendto error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        netbuf_delete(sendbuf);
        return;
    }
    netbuf_delete(sendbuf);
    free(buffer);

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

static void do_close(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, ok | {error, atom()}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    err_t err = netconn_delete(tcp_data->socket_data.conn);
    if (err != ERR_OK) {
        ESP_LOGE(TAG, "netconn_delete failed with error %d", err);
        term error = port_create_error_tuple(ctx, term_from_int(err));
        port_send_reply(ctx, pid, ref, error);
        return;
    }
    //TODO
    tcp_data->socket_data.conn = NULL;
    list_remove(&tcp_data->socket_data.sockets_head);

    term return_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);

    free(tcp_data);
    scheduler_terminate(ctx);
}

static void do_recvfrom(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    if (socket_data->passive_receiver_process_pid != term_invalid_term()) {
        // {Ref, {error, ealready}}
        size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
            return;
        }

        term error = port_create_error_tuple(ctx, make_atom(ctx->global, ealready_atom));
        port_send_reply(ctx, pid, ref, error);
        return;
    }

    if (socket_data->avail_bytes) {
        TRACE(stderr, "do_recvfrom: have already ready bytes.\n");

        struct netbuf *buf = NULL;
        err_t err = netconn_recv(socket_data->conn, &buf);
        if (UNLIKELY(err != ERR_OK)) {
            ESP_LOGE(TAG, "netconn_recv failed with error %d", err);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
        }

        void *data;
        u16_t data_len;
        err = netbuf_data(buf, &data, &data_len);
        if (UNLIKELY(err != ERR_OK)) {
            ESP_LOGE(TAG, "netbuf_data failed with error %d", err);
            term error = port_create_error_tuple(ctx, term_from_int(err));
            port_send_reply(ctx, pid, ref, error);
        }

        socket_data->avail_bytes -= data_len;

        //HANDLE fragments here?

        TRACE("%*s\n", (int) data_len, (char *) data);

        int recv_terms_size;
        if (socket_data->binary) {
            recv_terms_size = term_binary_data_size_in_terms(data_len) + BINARY_HEADER_SIZE;
        } else {
            recv_terms_size = data_len * 2;
        }

        // {Ref, {ok, {{192, 166, 1, 4}, Port, ReceiveData}}}
        size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(3) + TUPLE_SIZE(4) + recv_terms_size;
        if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
            return;
        }

        term recv_data;
        if (socket_data->binary) {
            recv_data = term_create_uninitialized_binary(data_len, ctx);
            memcpy((void *) term_binary_data(recv_data), data, data_len);
        } else {
            recv_data = term_from_string((const uint8_t *) data, data_len, ctx);
        }

        term addr = socket_addr_to_tuple(ctx, netbuf_fromaddr(buf));
        term port = term_from_int32(netbuf_fromport(buf));

        netbuf_delete(buf);

        term recv_ret = term_alloc_tuple(3, ctx);
        term_put_tuple_element(recv_ret, 0, addr);
        term_put_tuple_element(recv_ret, 1, port);
        term_put_tuple_element(recv_ret, 2, recv_data);

        term ok_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, recv_ret);

        term result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, ref);
        term_put_tuple_element(result_tuple, 1, ok_tuple);

        send_message(pid, result_tuple, glb);

    } else {
        socket_data->passive_receiver_process_pid = pid;
        socket_data->passive_ref_ticks = term_to_ref_ticks(ref);
    }
}

static void do_get_port(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, {ok, int()} | {error, badarg}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    term error_ok_tuple = term_alloc_tuple(2, ctx);
    if (socket_data->port != 0) {
        term_put_tuple_element(error_ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(error_ok_tuple, 1, term_from_int(socket_data->port));
    } else {
        term_put_tuple_element(error_ok_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_ok_tuple, 1, BADARG_ATOM);
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, error_ok_tuple);

    send_message(pid, result_tuple, glb);
}

static void do_sockname(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, {ok, {192, 168, 1, 3}} | {error, badarg}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(4);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    ip_addr_t addr;
    u16_t port;
    err_t err = netconn_addr(socket_data->conn, &addr, &port);
    term return_msg;
    if (err != ERR_OK) {
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, term_from_int(err));
    } else {
        return_msg = term_alloc_tuple(2, ctx);
        term addr_term = socket_addr_to_tuple(ctx, &addr);
        term port_term = term_from_int(port);
        term address_port_term = term_alloc_tuple(2, ctx);
        term_put_tuple_element(address_port_term, 0, addr_term);
        term_put_tuple_element(address_port_term, 1, port_term);
        term_put_tuple_element(return_msg, 0, OK_ATOM);
        term_put_tuple_element(return_msg, 1, address_port_term);
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, return_msg);
    send_message(pid, result_tuple, glb);
}

static void do_peername(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, {ok, {192, 168, 1, 3}} | {error, badarg}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2) + TUPLE_SIZE(4);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_peer(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, term_from_int(result));
    } else {
        return_msg = term_alloc_tuple(2, ctx);
        term addr_term = socket_addr_to_tuple(ctx, &addr);
        term port_term = term_from_int(port);
        term address_port_term = term_alloc_tuple(2, ctx);
        term_put_tuple_element(address_port_term, 0, addr_term);
        term_put_tuple_element(address_port_term, 1, port_term);
        term_put_tuple_element(return_msg, 0, OK_ATOM);
        term_put_tuple_element(return_msg, 1, address_port_term);
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, return_msg);
    send_message(pid, result_tuple, glb);
}

static void do_controlling_process(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);
    term new_pid_term = term_get_tuple_element(cmd, 1);

    // NB. GC is safe when msg is referenced from mailbox
    // {Ref, {ok | {error, badarg | not_owner}}
    size_t heap_size = TUPLE_SIZE(2) + REF_SIZE + TUPLE_SIZE(2);
    if (UNLIKELY(memory_ensure_free(ctx, heap_size) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "Unable to allocate space for return message.  No reply will be sent!");
        return;
    }

    term return_msg;
    if (UNLIKELY(!term_is_pid(new_pid_term))) {
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, BADARG_ATOM);
    } else if (UNLIKELY(pid != socket_data->controlling_process_pid)) {
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, make_atom(ctx->global, not_owner_atom));
    } else {
        socket_data->controlling_process_pid = new_pid_term;
        return_msg = OK_ATOM;
    }

    term result_tuple = term_alloc_tuple(2, ctx);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, return_msg);
    send_message(pid, result_tuple, glb);
}

static void socket_consume_mailbox(Context *ctx)
{
    while (!list_is_empty(&ctx->mailbox)) {
        Message *message = mailbox_dequeue(ctx);
        term msg = message->message;

        TRACE("message: ");
        #ifdef ENABLE_TRACE
            term_display(stdout, msg, ctx);
        #endif
        TRACE("\n");

        term cmd_term = term_get_tuple_element(msg, 2);
        term cmd_name = term_get_tuple_element(cmd_term, 0);

        enum socket_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_name, ctx->global);
        switch (cmd) {
            //TODO: remove this
            case SocketInitCmd:
                TRACE("init\n");
                do_init(ctx, msg);
                break;

            case SocketSendToCmd:
                TRACE("sendto\n");
                do_sendto(ctx, msg);
                break;

            case SocketSendCmd:
                TRACE("send\n");
                do_send(ctx, msg);
                break;

            case SocketRecvFromCmd:
                TRACE("recvfrom\n");
                do_recvfrom(ctx, msg);
                break;

            case SocketRecvCmd:
                TRACE("recv\n");
                do_recvfrom(ctx, msg);
                break;

            case SocketAcceptCmd:
                TRACE("accept\n");
                do_accept(ctx, msg);
                break;

            case SocketCloseCmd:
                TRACE("close\n");
                do_close(ctx, msg);
                break;

            case SocketGetPortCmd:
                TRACE("get_port\n");
                do_get_port(ctx, msg);
                break;

            case SocketSockNameCmd:
                TRACE("sockname\n");
                do_sockname(ctx, msg);
                break;

            case SocketPeerNameCmd:
                TRACE("peername\n");
                do_peername(ctx, msg);
                break;

            case SocketControllingProcessCmd:
                TRACE("controlling_process\n");
                do_controlling_process(ctx, msg);
                break;

            case SocketInvalidCmd:
            default:
                ESP_LOGE(TAG, "Unknown command");
                break;
        }

        mailbox_destroy_message(message);
    }
}

void socket_driver_init(GlobalContext * glb)
{
    TRACE("Initializing socket driver\n");

    netconn_events = xQueueCreate(32, sizeof(struct NetconnEvent));
    EventListener *socket_listener = malloc(sizeof(EventListener));

    struct ESP32PlatformData *platform = glb->platform_data;
    sys_event_listener_init(socket_listener, &netconn_events, socket_events_handler, glb);
    list_append(&platform->listeners, &socket_listener->listeners_list_head);

    list_init(&platform->sockets_list_head);

    TRACE("Socket driver init: done\n");
}

Context *socket_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

#ifdef CONFIG_AVM_ENABLE_SOCKET_PORT_DRIVER

REGISTER_PORT_DRIVER(socket, socket_driver_init, socket_driver_create_port)

#endif
