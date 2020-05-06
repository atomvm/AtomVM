/***************************************************************************
 *   Copyright 2018,2019 by Davide Bettio <davide@uninstall.it>            *
 *   Copyright 2019 by Fred Dushin <fred@dushin.net>                       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License as        *
 *   published by the Free Software Foundation; either version 2 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        *
 ***************************************************************************/

#include "socket_driver.h"
#include "port.h"

#include <string.h>
#include <stdbool.h>

#include "atom.h"
#include "context.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "interop.h"
#include "utils.h"
#include "scheduler.h"
#include "sys.h"
#include "term.h"

#include "esp32_sys.h"
#include "platform_defaultatoms.h"

#include <esp_log.h>

#include <lwip/ip_addr.h>
#include <lwip/inet.h>
#include <lwip/api.h>

//#define ENABLE_TRACE 1
#include "trace.h"

static void tcp_server_handler(Context *ctx);
static void tcp_client_handler(Context *ctx);
static void udp_handler(Context *ctx);
static void socket_consume_mailbox(Context *ctx);

static const char *const ealready_atom = "\x8" "ealready";

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
    terms[2] = term_from_int32((addr >>  8) & 0xFF);
    terms[3] = term_from_int32( addr        & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
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
    Context *ctx;
    enum socket_type type;
    term controlling_process_pid;

    term passive_receiver_process_pid;
    uint64_t passive_ref_ticks;

    int avail_bytes;

    uint16_t port;

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

xQueueHandle netconn_events = NULL;

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
        LIST_FOR_EACH(socket_head, &platform->sockets_list_head) {
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
                    fprintf(stderr, "bug: unknown socket type.\n");
            }

        } else {
            TRACE("Got event for unknown conn: %p, evt: %i, len: %i\n", netconn, (int) evt, (int) len);
        }
    }
}

void socket_driver_init(GlobalContext *glb)
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

static void socket_data_init(struct SocketData *data, Context *ctx, struct netconn *conn,
        enum socket_type type, struct ESP32PlatformData *platform)
{
    data->type = type;
    data->conn = conn;
    data->ctx = ctx;
    data->controlling_process_pid = term_invalid_term();
    data->port = 0;
    data->active = true;
    data->binary = false;

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
        fprintf(stderr, "socket: failed to enqueue: %i to netconn_events.\n", result);
    }

    void *netconn_events_ptr = &netconn_events;
    result = xQueueSendFromISR(event_queue, &netconn_events_ptr, &xHigherPriorityTaskWoken);
    if (result != pdTRUE) {
        fprintf(stderr, "socket: failed to enqueue: %i to event_queue.\n", result);
    }
}

void accept_conn(struct TCPServerAccepter *accepter, Context *ctx)
{
        TRACE("Going to accept a TCP connection\n");

        struct TCPServerSocketData *tcp_data = ctx->platform_data;
        GlobalContext *glb = ctx->global;
        struct ESP32PlatformData *platform = glb->platform_data;

        struct netconn *accepted_conn;
        err_t status = netconn_accept(tcp_data->socket_data.conn, &accepted_conn);
        if (UNLIKELY(status != ERR_OK)) {
            //TODO
            fprintf(stderr, "accept error: %i on %p\n", status, (void *) tcp_data->socket_data.conn);
            return;
        }

        term pid = accepter->accepting_process_pid;

        TRACE("accepted conn: %p\n", accepted_conn);

        Context *new_ctx = context_new(glb);
        new_ctx->native_handler = socket_consume_mailbox;
        scheduler_make_waiting(glb, new_ctx);

        term socket_pid = term_from_local_process_id(new_ctx->process_id);

        struct TCPClientSocketData *new_tcp_data = tcp_client_socket_data_new(new_ctx, accepted_conn, platform, pid);
        if (IS_NULL_PTR(new_tcp_data)) {
            abort();
        }

        //TODO
        if (UNLIKELY(memory_ensure_free(ctx, 128) != MEMORY_GC_OK)) {
            abort();
        }
        term ref = term_from_ref_ticks(accepter->ref_ticks, ctx);
        term return_tuple = term_alloc_tuple(2, ctx);

        free(accepter);

        term result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, OK_ATOM);
        term_put_tuple_element(result_tuple, 1, socket_pid);

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
        MUTABLE_LIST_FOR_EACH(accepter_head, tmp, &tcp_data->accepters_list_head) {
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

static void tcp_client_handler(Context *ctx)
{
    TRACE("tcp_client_handler\n");

    struct TCPClientSocketData *tcp_data = ctx->platform_data;
    GlobalContext *glb = ctx->global;

    if (!tcp_data->socket_data.active) {
        return;
    }

    if (!tcp_data->socket_data.avail_bytes) {
        TRACE("No bytes to receive.\n");
        return;
    }

    struct netbuf *buf = NULL;
    err_t status = netconn_recv(tcp_data->socket_data.conn, &buf);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "tcp_client_handler error: %i\n", status);
        return;
    }

    void *data;
    u16_t data_len;
    status = netbuf_data(buf, &data, &data_len);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "netbuf_data error: %i\n", status);
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
        abort();
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
        term_put_tuple_element(result_tuple, 0, TCP_ATOM);
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
    MUTABLE_LIST_FOR_EACH(accepter_head, tmp, &tcp_data->accepters_list_head) {
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
    err_t status = netconn_recv(udp_data->socket_data.conn, &buf);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "tcp_client_handler error: %i\n", status);
        return;
    }

    void *data;
    u16_t data_len;
    status = netbuf_data(buf, &data, &data_len);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "netbuf_data error: %i\n", status);
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
        abort();
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
        term_put_tuple_element(result_tuple, 0, UDP_ATOM);
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

    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term address_term = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term binary_term = interop_proplist_get_value(params, BINARY_ATOM);
    term active_term = interop_proplist_get_value(params, ACTIVE_ATOM);
    term controlling_process_term = interop_proplist_get_value(params, CONTROLLING_PROCESS_ATOM);

    int ok_int;
    char *address_string = interop_term_to_string(address_term, &ok_int);
    if (UNLIKELY(!ok_int)) {
        abort();
    }

    avm_int_t port = term_to_int(port_term);
    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        abort();
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        abort();
    }

    TRACE("tcp: connecting to: %s\n", address_string);

    struct ip_addr remote_ip;
    //TODO: use dns_gethostbyname instead
    err_t status = netconn_gethostbyname(address_string, &remote_ip);
    if (UNLIKELY(status != ERR_OK)) {
        free(address_string);
        TRACE("tcp: host resolution failed.\n");
        return;
    }

    TRACE("tcp: host resolved.\n");

    free(address_string);

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        abort();
    }

    status = netconn_connect(conn, &remote_ip, port);
    if (UNLIKELY(status != ERR_OK)) {
        TRACE("tcp: failed connect: %i\n", status);
        return;
    }

    TRACE("tcp: connected.\n");

    struct TCPClientSocketData *tcp_data = tcp_client_socket_data_new(ctx, conn, platform, controlling_process_term);
    if (IS_NULL_PTR(tcp_data)) {
        abort();
    }
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
    term return_tuple = term_alloc_tuple(2, ctx);

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

static void do_listen(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    term cmd = term_get_tuple_element(msg, 2);
    term params = term_get_tuple_element(cmd, 1);

    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term backlog_term = interop_proplist_get_value(params, BACKLOG_ATOM);
    term binary_term = interop_proplist_get_value(params, BINARY_ATOM);
    term active_term = interop_proplist_get_value(params, ACTIVE_ATOM);

    avm_int_t port = term_to_int(port_term);
    avm_int_t backlog = term_to_int(backlog_term);
    bool ok;
    bool active = bool_term_to_bool(active_term, &ok);
    if (UNLIKELY(!ok)) {
        abort();
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        abort();
    }

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);

    err_t status = netconn_bind(conn, IP_ADDR_ANY, port);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "bind error: %i\n", status);
        return;
    }

    ip_addr_t naddr;
    u16_t nport;
    status = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "getaddr error: %i\n", status);
        return;
    }

    status = netconn_listen_with_backlog(conn, backlog);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "listen error: %i\n", status);
        return;
    }

    struct TCPServerSocketData *tcp_data = tcp_server_socket_data_new(ctx, conn, platform);
    if (IS_NULL_PTR(tcp_data)) {
        abort();
    }
    tcp_data->socket_data.port = nport;
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
    term return_tuple = term_alloc_tuple(2, ctx);

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

    term_put_tuple_element(return_tuple, 0, ref);
    term_put_tuple_element(return_tuple, 1, OK_ATOM);

    send_message(pid, return_tuple, glb);
}

void do_udp_open(Context *ctx, term msg)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

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
        abort();
    }
    bool binary = bool_term_to_bool(binary_term, &ok);
    if (UNLIKELY(!ok)) {
        abort();
    }

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_UDP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        fprintf(stderr, "failed to open conn\n");
        abort();
    }

    struct UDPSocketData *udp_data = udp_socket_data_new(ctx, conn, platform, controlling_process);
    if (IS_NULL_PTR(udp_data)) {
        abort();
    }
    udp_data->socket_data.active = active;
    udp_data->socket_data.binary = binary;

    if (port != 0) {
        err_t status = netconn_bind(conn, IP_ADDR_ANY, port);
        if (UNLIKELY(status != ERR_OK)) {
            fprintf(stderr, "bind error: %i\n", status);
            return;
        }
    }

    ip_addr_t naddr;
    u16_t nport;
    err_t status = netconn_getaddr(conn, &naddr, &nport, 1);
    if (UNLIKELY(status != ERR_OK)) {
        //TODO
        fprintf(stderr, "getaddr error: %i\n", status);
        return;
    }
    udp_data->socket_data.port = nport;

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
    term return_tuple = term_alloc_tuple(2, ctx);

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);

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
    GlobalContext *glb = ctx->global;
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    term pid = term_get_tuple_element(msg, 0);
    term ref = term_get_tuple_element(msg, 1);
    term cmd = term_get_tuple_element(msg, 2);

    term data = term_get_tuple_element(cmd, 1);

    int ok;
    int buffer_size = interop_iolist_size(data, &ok);
    if (UNLIKELY(!ok)) {
        fprintf(stderr, "error: invalid iolist.\n");
        return;
    }
    void *buffer = malloc(buffer_size);
    interop_write_iolist(data, buffer);
    err_t status = netconn_write(tcp_data->socket_data.conn, buffer, buffer_size, NETCONN_NOCOPY);
    if (UNLIKELY(status != ERR_OK)) {
        fprintf(stderr, "write error: %i\n", status);
        return;
    }

    free(buffer);


    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
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

    int ok;
    int buffer_size = interop_iolist_size(data, &ok);
    void *buffer = malloc(buffer_size);
    interop_write_iolist(data, buffer);

    ip_addr_t ip4addr;
    tuple_to_ip_addr(dest_addr_term, &ip4addr);
    uint16_t destport = term_to_int32(dest_port_term);

    struct netbuf *sendbuf = netbuf_new();

    err_t status = netbuf_ref(sendbuf, buffer, buffer_size);
    if (UNLIKELY(status != ERR_OK)) {
        fprintf(stderr, "netbuf_ref error: %i\n", status);
        netbuf_delete(sendbuf);
        return;
    }

    status = netconn_sendto(udp_data->socket_data.conn, sendbuf, &ip4addr, destport);
    if (UNLIKELY(status != ERR_OK)) {
        fprintf(stderr, "netbuf_ref error: %i\n", status);
        netbuf_delete(sendbuf);
        return;
    }

    netbuf_delete(sendbuf);
    free(buffer);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
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

    err_t res = netconn_delete(tcp_data->socket_data.conn);
    if (res != ERR_OK) {
        TRACE("socket: close failed");
        return;
    }
    //TODO
    tcp_data->socket_data.conn = NULL;
    list_remove(&tcp_data->socket_data.sockets_head);

    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        abort();
    }
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
        // 3 (error_tuple) + 3 (result_tuple)
        if (UNLIKELY(memory_ensure_free(ctx, 3 + 3) != MEMORY_GC_OK)) {
            abort();
        }

        term ealready = context_make_atom(ctx, ealready_atom);

        term error_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, ealready);

        term result_tuple = term_alloc_tuple(2, ctx);
        term_put_tuple_element(result_tuple, 0, ref);
        term_put_tuple_element(result_tuple, 1, error_tuple);

        send_message(pid, result_tuple, glb);
    }

    if (socket_data->avail_bytes) {
        TRACE(stderr, "do_recvfrom: have already ready bytes.\n");

        struct netbuf *buf = NULL;
        err_t status = netconn_recv(socket_data->conn, &buf);
        if (UNLIKELY(status != ERR_OK)) {
            //TODO
            fprintf(stderr, "tcp_client_handler error: %i\n", status);
            return;
        }

        void *data;
        u16_t data_len;
        status = netbuf_data(buf, &data, &data_len);
        if (UNLIKELY(status != ERR_OK)) {
            //TODO
            fprintf(stderr, "netbuf_data error: %i\n", status);
            return;
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

        // 4 (recv_ret size) + 3 (ok_tuple size) + 3 (result_tuple size) + recv_terms_size
        if (UNLIKELY(memory_ensure_free(ctx, 4 + 3 + 3 + recv_terms_size) != MEMORY_GC_OK)) {
            abort();
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

    // 3 (error_ok_tuple) + 3 (result_tuple)
    if (UNLIKELY(memory_ensure_free(ctx, 3 + 3) != MEMORY_GC_OK)) {
        abort();
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

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_addr(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        if (UNLIKELY(memory_ensure_free(ctx, 3 + 3) != MEMORY_GC_OK)) {
            abort();
        }
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, term_from_int(result));
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, 3 + 3 + 8) != MEMORY_GC_OK)) {
            abort();
        }
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

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_peer(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        if (UNLIKELY(memory_ensure_free(ctx, 3 + 3) != MEMORY_GC_OK)) {
            abort();
        }
        return_msg = term_alloc_tuple(2, ctx);
        term_put_tuple_element(return_msg, 0, ERROR_ATOM);
        term_put_tuple_element(return_msg, 1, term_from_int(result));
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, 3 + 3 + 8) != MEMORY_GC_OK)) {
            abort();
        }
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
                do_recvfrom(ctx, msg);
                break;

            case RECV_ATOM:
                TRACE("recv\n");
                do_recvfrom(ctx, msg);
                break;

            case ACCEPT_ATOM:
                TRACE("accept\n");
                do_accept(ctx, msg);
                break;

            case CLOSE_ATOM:
                TRACE("close\n");
                do_close(ctx, msg);
                break;

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

            default:
                TRACE("badarg\n");
                break;
        }

        free(message);
    }
}

void socket_init(Context *ctx, term opts)
{
    UNUSED(opts);
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = NULL;
}
