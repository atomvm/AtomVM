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
#include "port.h"
#include "scheduler.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#include "esp32_sys.h"
#include "platform_defaultatoms.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <esp_log.h>
#include <lwip/api.h>
#include <lwip/inet.h>
#include <lwip/ip_addr.h>
#pragma GCC diagnostic pop

//#define ENABLE_TRACE 1
#include "trace.h"

#define TAG "socket_driver"

typedef struct SocketListener
{
    EventListener base;
    int32_t process_id;
    avm_int_t buf_size;
} ActiveRecvListener;

// To make thread model explicit, functions that are passed Context *ctx are
// called from this context.
static void socket_driver_init(GlobalContext *global);
static void socket_driver_destroy(GlobalContext *global);
static Context *socket_driver_create_port(GlobalContext *global, term opts);

static NativeHandlerResult socket_consume_mailbox(Context *ctx);

static const char *const tcp_error_atom = "\x9" "tcp_error";

static const char *const netconn_event_internal = ATOM_STR("\x14", "$atomvm_netconn_event_internal");
static const char *gen_tcp_moniker_atom = ATOM_STR("\xC", "$avm_gen_tcp");
static const char *native_tcp_module_atom = ATOM_STR("\xC", "gen_tcp_inet");
static const char *gen_udp_moniker_atom = ATOM_STR("\xC", "$avm_gen_udp");
static const char *native_udp_module_atom = ATOM_STR("\xC", "gen_udp_inet");

static inline term create_socket_wrapper(term pid, const char *moniker_atom, const char *module_atom, Heap *heap, GlobalContext *global)
{
    term tuple = term_alloc_tuple(3, heap);
    term_put_tuple_element(tuple, 0, globalcontext_make_atom(global, moniker_atom));
    term_put_tuple_element(tuple, 1, pid);
    term_put_tuple_element(tuple, 2, globalcontext_make_atom(global, module_atom));

    return tuple;
}

static inline term create_tcp_socket_wrapper(term pid, Heap *heap, GlobalContext *global)
{
    return create_socket_wrapper(pid, gen_tcp_moniker_atom, native_tcp_module_atom, heap, global);
}

static inline term create_udp_socket_wrapper(term pid, Heap *heap, GlobalContext *global)
{
    return create_socket_wrapper(pid, gen_udp_moniker_atom, native_udp_module_atom, heap, global);
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

#define SOCKET_INET_ADDR TUPLE_SIZE(4)
static term socket_addr_to_tuple(Heap *heap, ip_addr_t *addr)
{
    term addr_tuple;
    switch (IP_GET_TYPE(addr)) {
        case IPADDR_TYPE_V4: {
            addr_tuple = term_alloc_tuple(4, heap);
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
    int32_t accepting_process_pid;
    uint64_t ref_ticks;
};

struct UDPSocketData
{
    struct SocketData socket_data;
};

// We are queueing netconn with the length because 0 length packets can
// be dropped but > 0 packets cannot because of the race condition with
// accept.
struct NetconnEvent
{
    struct netconn *netconn;
    u16_t len;
};


struct ReadyConnection
{
    struct ListHead ready_connection_head;
    struct netconn *netconn;
    u16_t len;
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

QueueHandle_t netconn_events = NULL;

void IRAM_ATTR socket_callback(struct netconn *netconn, enum netconn_evt evt, u16_t len)
{
    TRACE("socket_callback netconn=%p, evt=%d, len=%d\n", (void *) netconn, evt, len);

    // We only listen to NETCONN_EVT_RCVPLUS events
    if (evt == NETCONN_EVT_RCVPLUS) {
        struct NetconnEvent event;
        event.netconn = netconn;
        event.len = len;

        BaseType_t xHigherPriorityTaskWoken;
        int result = xQueueSendFromISR(netconn_events, &event, &xHigherPriorityTaskWoken);
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

    struct NetconnEvent event;
    while (xQueueReceive(netconn_events, &event, 1) == pdTRUE) {
        TRACE("Got netconn: %p, len = %d\n", (void *) event.netconn, event.len);
        struct SocketData *socket = NULL;
        struct ListHead *socket_head;
        struct ListHead *socket_list = synclist_rdlock(&platform->sockets);
        LIST_FOR_EACH (socket_head, socket_list) {
            struct SocketData *current_socket = GET_LIST_ENTRY(socket_head, struct SocketData, sockets_head);
            if (current_socket->conn == event.netconn) {
                socket = current_socket;
                break;
            }
        }

        if (socket == NULL) {
            if (event.len == 0) {
                // The socket may already be gone
                TRACE("Got event for unknown conn: %p, dropping it as len is 0\n", (void *) event.netconn);
            } else {
                // Add it to ready_connections
                TRACE("Got event for unknown conn: %p, len = %d adding to ready connections list\n", (void *) event.netconn, event.len);
                struct ReadyConnection *ready = (struct ReadyConnection *) malloc(sizeof (struct ReadyConnection));
                ready->netconn = event.netconn;
                ready->len = event.len;
                list_append(&platform->ready_connections, &ready->ready_connection_head);
            }
        }
        synclist_unlock(&platform->sockets);

        if (socket != NULL) {
            BEGIN_WITH_STACK_HEAP(TUPLE_SIZE(2), heap)
            term message = term_alloc_tuple(2, &heap);
            term_put_tuple_element(message, 0, globalcontext_make_atom(glb, netconn_event_internal));
            term_put_tuple_element(message, 1, term_from_int(event.len));
            globalcontext_send_message(glb, socket->process_id, message);
            END_WITH_STACK_HEAP(heap, glb)
        }
    }
    return listener;
}

void socket_driver_init(GlobalContext *glb)
{
    TRACE("Initializing socket driver\n");

    netconn_events = xQueueCreate(32, sizeof(struct NetconnEvent));
    EventListener *socket_listener = malloc(sizeof(EventListener));

    struct ESP32PlatformData *platform = glb->platform_data;
    platform->socket_listener = socket_listener;
    socket_listener->sender = netconn_events;
    socket_listener->handler = socket_events_handler;
    sys_register_listener(glb, socket_listener);

    synclist_init(&platform->sockets);
    list_init(&platform->ready_connections);

    TRACE("Socket driver init: done\n");
}

void socket_driver_destroy(GlobalContext *glb)
{
    TRACE("Destroying socket driver\n");

    struct ESP32PlatformData *platform = glb->platform_data;
    EventListener *socket_listener = platform->socket_listener;
    sys_unregister_listener(glb, socket_listener);
    vQueueDelete(socket_listener->sender);
    free((void *) socket_listener);

    synclist_destroy(&platform->sockets);

    TRACE("Socket driver destroy: done\n");
}

static struct ListHead *socket_data_preinit(struct ESP32PlatformData *platform)
{
    TRACE("socket_data_preinit\n");

    return synclist_wrlock(&platform->sockets);
}

static void socket_data_postinit(struct ESP32PlatformData *platform)
{
    TRACE("socket_data_postinit\n");

    synclist_unlock(&platform->sockets);
}

static void socket_data_init(struct SocketData *data, Context *ctx, struct netconn *conn,
    enum socket_type type, struct ListHead *sockets)
{
    TRACE("socket_data_init\n");

    data->type = type;
    data->conn = conn;
    data->process_id = ctx->process_id;
    data->controlling_process_pid = 0;
    data->port = 0;
    data->active = true;
    data->binary = true;
    data->buffer = 512;

    list_append(sockets, &data->sockets_head);
    data->passive_receiver_process_pid = 0;
    data->passive_ref_ticks = 0;

    ctx->platform_data = data;
}

static struct TCPServerSocketData *tcp_server_socket_data_new(Context *ctx, struct netconn *conn,
    struct ListHead *sockets)
{
    struct TCPServerSocketData *tcp_data = malloc(sizeof(struct TCPServerSocketData));
    if (IS_NULL_PTR(tcp_data)) {
        return NULL;
    }
    socket_data_init(&tcp_data->socket_data, ctx, conn, TCPServerSocket, sockets);
    tcp_data->ready_connections = 0;
    list_init(&tcp_data->accepters_list_head);

    return tcp_data;
}

static struct TCPClientSocketData *tcp_client_socket_data_new(Context *ctx, struct netconn *conn,
    struct ListHead *sockets, int32_t controlling_process_pid)
{
    struct TCPClientSocketData *tcp_data = malloc(sizeof(struct TCPClientSocketData));
    if (IS_NULL_PTR(tcp_data)) {
        return NULL;
    }
    socket_data_init(&tcp_data->socket_data, ctx, conn, TCPClientSocket, sockets);
    tcp_data->socket_data.controlling_process_pid = controlling_process_pid;

    return tcp_data;
}

static struct UDPSocketData *udp_socket_data_new(Context *ctx, struct netconn *conn,
    struct ListHead *sockets, int32_t controlling_process_pid)
{
    struct UDPSocketData *udp_data = malloc(sizeof(struct UDPSocketData));
    if (IS_NULL_PTR(udp_data)) {
        return NULL;
    }
    socket_data_init(&udp_data->socket_data, ctx, conn, UDPSocket, sockets);
    udp_data->socket_data.controlling_process_pid = controlling_process_pid;

    return udp_data;
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
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "etimedout"));
        case ERR_WOULDBLOCK:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "ewouldblock"));
        case ERR_USE:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "eaddrinuse"));
        case ERR_ALREADY:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "ealready"));
        case ERR_CONN:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "enotconn"));
        case ERR_ABRT:
            return globalcontext_make_atom(glb, ATOM_STR("\xC", "econnaborted"));
        case ERR_RST:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "econnreset"));
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

static void accept_conn(Context *ctx, struct TCPServerSocketData *tcp_data, uint64_t ref_ticks, int32_t pid)
{
    TRACE("Going to accept a TCP connection\n");
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = ctx->global->platform_data;

    struct netconn *accepted_conn;

    // There is a bug in lwIP: the callbacks may be called before
    // netconn_accept is called. Locking the list of sockets is not going
    // to help in this case.
    struct ListHead *sockets = socket_data_preinit(platform);
    err_t status = netconn_accept(tcp_data->socket_data.conn, &accepted_conn);
    if (UNLIKELY(status != ERR_OK)) {
        socket_data_postinit(platform);
        do_send_error_reply(ctx, status, ref_ticks, pid);
        return;
    }

    TRACE("accepted conn: %p\n", (void *) accepted_conn);
    // Check if it's in the list of ready_connections

    struct ListHead *ready_connections_head;
    struct ListHead *tmp;
    bool is_ready = false;
    u16_t ready_len = 0;
    MUTABLE_LIST_FOR_EACH (ready_connections_head, tmp, &platform->ready_connections) {
        struct ReadyConnection *ready_connection;
        ready_connection = GET_LIST_ENTRY(ready_connections_head, struct ReadyConnection, ready_connection_head);
        if (ready_connection->netconn == accepted_conn) {
            TRACE("found accepted conn in list of ready connections\n");
            is_ready = true;
            ready_len += ready_connection->len;
            list_remove(ready_connections_head);
            free(ready_connection);
        }
    }

    Context *new_ctx = context_new(glb);
    new_ctx->native_handler = socket_consume_mailbox;

    term socket_pid = term_from_local_process_id(new_ctx->process_id);

    struct TCPClientSocketData *new_tcp_data = tcp_client_socket_data_new(new_ctx, accepted_conn, sockets, pid);
    socket_data_postinit(platform);
    if (IS_NULL_PTR(new_tcp_data)) {
        AVM_ABORT();
    }
    new_tcp_data->socket_data.active = tcp_data->socket_data.active;
    new_tcp_data->socket_data.binary = tcp_data->socket_data.binary;
    new_tcp_data->socket_data.buffer = tcp_data->socket_data.buffer;

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE + TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    term result_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, OK_ATOM);
    term_put_tuple_element(result_tuple, 1, socket_pid);

    if (is_ready) {
        term message = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(message, 0, globalcontext_make_atom(glb, netconn_event_internal));
        term_put_tuple_element(message, 1, term_from_int(ready_len));
        globalcontext_send_message(glb, new_ctx->process_id, message);
    }

    do_send_reply(ctx, result_tuple, ref_ticks, pid);

}

static void do_accept(Context *ctx, const GenMessage *gen_message)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (tcp_data->ready_connections) {
        TRACE("accepting existing connections.\n");

        accept_conn(ctx, tcp_data, ref_ticks, pid);
        tcp_data->ready_connections--;
    } else {
        struct TCPServerAccepter *accepter = malloc(sizeof(struct TCPServerAccepter));
        accepter->accepting_process_pid = pid;
        accepter->ref_ticks = ref_ticks;
        list_append(&tcp_data->accepters_list_head, &accepter->accepter_head);
    }
}

static void do_send_passive_reply(Context *ctx, struct SocketData *socket_data, term reply)
{
    do_send_reply(ctx, reply, socket_data->passive_ref_ticks, socket_data->passive_receiver_process_pid);
    socket_data->passive_receiver_process_pid = 0;
    socket_data->passive_ref_ticks = 0;
}

static void do_send_socket_error(Context *ctx, err_t status)
{
    GlobalContext *glb = ctx->global;
    struct SocketData *socket_data = ctx->platform_data;
    if (socket_data->active) {
        // udp active sockets do not send errors
        if (socket_data->type != UDPSocket) {
            // {tcp_error, {Moniker :: atom(), Socket :: pid(), Module :: module()}, Reason :: atom()}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3) + TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
                AVM_ABORT();
            }
            term reason_atom = lwip_error_atom(glb, status);
            term result_tuple = term_alloc_tuple(3, &ctx->heap);
            term_put_tuple_element(result_tuple, 0, globalcontext_make_atom(glb, tcp_error_atom));
            term socket_pid = term_from_local_process_id(ctx->process_id);
            term socket_wrapper = create_tcp_socket_wrapper(socket_pid, &ctx->heap, glb);
            term_put_tuple_element(result_tuple, 1, socket_wrapper);
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
        // {tcp_closed, {Moniker :: atom(), Socket :: pid(), Module :: module()}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term result_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result_tuple, 0, TCP_CLOSED_ATOM);
        term socket_pid = term_from_local_process_id(ctx->process_id);
        term socket_wrapper = create_tcp_socket_wrapper(socket_pid, &ctx->heap, glb);
        term_put_tuple_element(result_tuple, 1, socket_wrapper);
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
        accepter = GET_LIST_ENTRY(accepter_head, struct TCPServerAccepter, accepter_head);
        list_remove(accepter_head);
    }

    if (accepter) {
        accept_conn(ctx, tcp_data, accepter->ref_ticks, accepter->accepting_process_pid);
        free(accepter);
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
    if (status != ERR_OK) {
        if (socket_data->type == TCPClientSocket) {
            // Close socket in case of errors or finish closing if it's closed
            // on the other end.
            struct ESP32PlatformData *platform = ctx->global->platform_data;
            synclist_remove(&platform->sockets, &socket_data->sockets_head);
            if (UNLIKELY(netconn_close(socket_data->conn) != ERR_OK)) {
                TRACE("do_receive_data: netconn_close failed\n");
            }
            if (UNLIKELY(netconn_delete(socket_data->conn) != ERR_OK)) {
                TRACE("do_receive_data: netconn_delete failed\n");
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
    socket_data->avail_bytes -= data_len;

    TRACE("%*s\n", (int) data_len, (char *) data);

    int recv_terms_size;
    if (socket_data->binary) {
        recv_terms_size = term_binary_heap_size(data_len);
    } else {
        recv_terms_size = data_len * 2;
    }

    int tuples_size;
    if (socket_data->active) {
        // tuples_size = 4 (result_tuple size)
        // add 3-tuple for {Moniker :: atom(), Socket :: pid(), Module :: module()}
        tuples_size = TUPLE_SIZE(3) + TUPLE_SIZE(3);
    } else {
        // tuples_size = 3 (ok_tuple size)
        tuples_size = TUPLE_SIZE(2) + REPLY_SIZE;
    }
    if (socket_data->type == UDPSocket) {
        // {<ip_addr>, port, packet}
        tuples_size += SOCKET_INET_ADDR + TUPLE_SIZE(3);
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
    term recv_term;
    if (socket_data->type == UDPSocket) {
        term addr_term = socket_addr_to_tuple(&ctx->heap, netbuf_fromaddr(buf));
        if (addr_term == term_invalid_term()) {
            TRACE("do_receive_data: socket_addr_to_tuple error\n");
            do_send_socket_error(ctx, ERR_BUF);
            return NativeContinue;
        }
        term port = term_from_int32(netbuf_fromport(buf));
        recv_term = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(recv_term, 0, addr_term);
        term_put_tuple_element(recv_term, 1, port);
        term_put_tuple_element(recv_term, 2, recv_data);
    } else {
        recv_term = recv_data;
    }

    if (netbuf_next(buf) == 0) {
        TRACE("do_receive_data: netbuf error : got more parts\n");
        do_send_socket_error(ctx, ERR_BUF);
        return NativeContinue;
    }

    netbuf_delete(buf);


    if (socket_data->active) {
        term active_tuple = term_alloc_tuple(socket_data->type == TCPClientSocket ? 3 : 5, &ctx->heap);
        term_put_tuple_element(active_tuple, 0, socket_data->type == TCPClientSocket ? TCP_ATOM : UDP_ATOM);
        term socket_pid = term_from_local_process_id(ctx->process_id);
        term socket_wrapper =
            socket_data->type == UDPSocket ?
                create_udp_socket_wrapper(socket_pid, &ctx->heap, ctx->global) :
                create_tcp_socket_wrapper(socket_pid, &ctx->heap, ctx->global);
        term_put_tuple_element(active_tuple, 1, socket_wrapper);
        if (socket_data->type == TCPClientSocket) {
            term_put_tuple_element(active_tuple, 2, recv_term);
        } else {
            term_put_tuple_element(active_tuple, 2, term_get_tuple_element(recv_term, 0));
            term_put_tuple_element(active_tuple, 3, term_get_tuple_element(recv_term, 1));
            term_put_tuple_element(active_tuple, 4, recv_data);
        }
        globalcontext_send_message(ctx->global, socket_data->controlling_process_pid, active_tuple);
        TRACE("sent received to active process (pid=%d): ", socket_data->controlling_process_pid);
        #ifdef ENABLE_TRACE
            term_display(stdout, active_tuple, ctx);
        #endif
        TRACE("\n");
    } else {
        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, recv_term);
        do_send_passive_reply(ctx, socket_data, ok_tuple);
        TRACE("sent received to passive caller (pid=%d): ", socket_data->passive_receiver_process_pid);
        #ifdef ENABLE_TRACE
            term_display(stdout, ok_tuple, ctx);
        #endif
        TRACE("\n");
    }

    return NativeContinue;
}

static NativeHandlerResult do_data_netconn_event(Context *ctx, int len)
{
    TRACE("do_data_netconn_event\n");
    struct SocketData *socket_data = ctx->platform_data;

    if (!socket_data->active && socket_data->passive_receiver_process_pid == 0) {
        // netconn_recv will not block
        socket_data->avail_bytes += len;
        return NativeContinue;
    }
    return do_receive_data(ctx);
}

static NativeHandlerResult do_netconn_event(Context *ctx, int len)
{
    NativeHandlerResult result = NativeContinue;
    struct SocketData *socket_data = ctx->platform_data;
    if (socket_data->type == TCPServerSocket) {
        do_tcp_server_netconn_event(ctx);
    } else {
        result = do_data_netconn_event(ctx, len);
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
            *ok = false;
            return false;
    }
}

static void do_connect(Context *ctx, const GenMessage *gen_message)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);
    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term params = term_get_tuple_element(gen_message->req, 1);

    term address_term = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term binary_term = interop_proplist_get_value_default(params, BINARY_ATOM, FALSE_ATOM);
    term active_term = interop_proplist_get_value_default(params, ACTIVE_ATOM, TRUE_ATOM);
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

    // Lock list of sockets before the event callback is called
    struct ListHead *sockets = socket_data_preinit(platform);
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

    struct TCPClientSocketData *tcp_data = tcp_client_socket_data_new(ctx, conn, sockets, controlling_process_pid);
    socket_data_postinit(platform);
    if (IS_NULL_PTR(tcp_data)) {
        AVM_ABORT();
    }
    tcp_data->socket_data.active = active;
    tcp_data->socket_data.binary = binary;

    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

static void do_listen(Context *ctx, const GenMessage *gen_message)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);
    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term params = term_get_tuple_element(gen_message->req, 1);

    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term backlog_term = interop_proplist_get_value(params, BACKLOG_ATOM);
    term binary_term = interop_proplist_get_value_default(params, BINARY_ATOM, FALSE_ATOM);
    term active_term = interop_proplist_get_value_default(params, ACTIVE_ATOM, TRUE_ATOM);
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

    // Lock list of sockets before the event callback is called
    struct ListHead *sockets = socket_data_preinit(platform);
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

    struct TCPServerSocketData *tcp_data = tcp_server_socket_data_new(ctx, conn, sockets);
    socket_data_postinit(platform);
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

void do_udp_open(Context *ctx, const GenMessage *gen_message)
{
    GlobalContext *glb = ctx->global;
    struct ESP32PlatformData *platform = glb->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);
    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term params = term_get_tuple_element(gen_message->req, 1);

    term port_term = interop_proplist_get_value(params, PORT_ATOM);
    term binary_term = interop_proplist_get_value_default(params, BINARY_ATOM, FALSE_ATOM);
    term active_term = interop_proplist_get_value_default(params, ACTIVE_ATOM, TRUE_ATOM);
    term controlling_process_term = interop_proplist_get_value(params, CONTROLLING_PROCESS_ATOM);

    bool ok = term_is_pid(controlling_process_term);
    if (UNLIKELY(!ok)) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
        return;
    }
    int32_t controlling_process_pid = term_to_local_process_id(controlling_process_term);
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

    // Lock list of sockets before the event callback is called
    struct ListHead *sockets = socket_data_preinit(platform);
    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_UDP, 0, socket_callback);
    if (IS_NULL_PTR(conn)) {
        fprintf(stderr, "failed to open conn\n");
        AVM_ABORT();
    }

    struct UDPSocketData *udp_data = udp_socket_data_new(ctx, conn, sockets, controlling_process_pid);
    socket_data_postinit(platform);
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
static void do_init(Context *ctx, const GenMessage *gen_message)
{
    if (UNLIKELY(term_get_tuple_arity(gen_message->req) < 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term params = term_get_tuple_element(gen_message->req, 1);

    if (interop_proplist_get_value_default(params, LISTEN_ATOM, FALSE_ATOM) == TRUE_ATOM) {
        TRACE("listen\n");
        do_listen(ctx, gen_message);

    } else if (interop_proplist_get_value_default(params, CONNECT_ATOM, FALSE_ATOM) == TRUE_ATOM) {
        TRACE("connect\n");
        do_connect(ctx, gen_message);

    } else if (interop_proplist_get_value_default(params, PROTO_ATOM, FALSE_ATOM) == UDP_ATOM) {
        TRACE("udp_open\n");
        do_udp_open(ctx, gen_message);
    }
}

static void do_send(Context *ctx, const GenMessage *gen_message)
{
    struct TCPServerSocketData *tcp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term data = term_get_tuple_element(gen_message->req, 1);

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
    free(buffer);
    if (UNLIKELY(status != ERR_OK)) {
        fprintf(stderr, "write error: %i\n", status);
        return;
    }

    if (UNLIKELY(memory_ensure_free(ctx, REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
}

static void do_sendto(Context *ctx, const GenMessage *gen_message)
{
    struct UDPSocketData *udp_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 4)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term cmd = gen_message->req;
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

static void do_close(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
        AVM_ABORT();
    }
    err_t close_disconnect_res = 0;
    if (socket_data->type == UDPSocket) {
        close_disconnect_res = netconn_disconnect(socket_data->conn);
    } else if (socket_data->type == TCPClientSocket) {
        close_disconnect_res = netconn_close(socket_data->conn);
    }
    err_t delete_res = netconn_delete(socket_data->conn);

    socket_data->conn = NULL;
    struct ESP32PlatformData *platform = ctx->global->platform_data;
    synclist_remove(&platform->sockets, &socket_data->sockets_head);

    if (UNLIKELY(close_disconnect_res != ERR_OK)) {
        do_send_error_reply(ctx, close_disconnect_res, ref_ticks, pid);
    } else if (UNLIKELY(delete_res != ERR_OK)) {
        do_send_error_reply(ctx, delete_res, ref_ticks, pid);
    } else {
        do_send_reply(ctx, OK_ATOM, ref_ticks, pid);
    }
}

static NativeHandlerResult do_recvfrom(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    // We cannot stack blocked queries
    if (socket_data->passive_receiver_process_pid != 0) {
        do_send_error_reply(ctx, ERR_ALREADY, ref_ticks, pid);
        return NativeContinue;
    }

    socket_data->passive_receiver_process_pid = pid;
    socket_data->passive_ref_ticks = ref_ticks;

    if (socket_data->avail_bytes > 0) {
        return do_receive_data(ctx);
    }

    return NativeContinue;
}

static void do_get_port(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (socket_data->port == 0) {
        do_send_error_reply(ctx, ERR_ARG, ref_ticks, pid);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REPLY_SIZE) != MEMORY_GC_OK)) {
            AVM_ABORT();
        }
        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, term_from_int(socket_data->port));
        do_send_reply(ctx, ok_tuple, ref_ticks, pid);
    }
}

static void do_sockname(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_addr(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        do_send_error_reply(ctx, result, ref_ticks, pid);
    } else {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + SOCKET_INET_ADDR + REPLY_SIZE) != MEMORY_GC_OK)) {
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

static void do_peername(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    ip_addr_t addr;
    u16_t port;
    err_t result = netconn_peer(socket_data->conn, &addr, &port);
    term return_msg;
    if (result != ERR_OK) {
        do_send_error_reply(ctx, result, ref_ticks, pid);
    } else {
        // {ok, {{A,B,C,D}, Port}}
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + SOCKET_INET_ADDR + REPLY_SIZE) != MEMORY_GC_OK)) {
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

static void do_controlling_process(Context *ctx, const GenMessage *gen_message)
{
    struct SocketData *socket_data = ctx->platform_data;

    int32_t pid = term_to_local_process_id(gen_message->pid);
    uint64_t ref_ticks = term_to_ref_ticks(gen_message->ref);

    if (UNLIKELY(term_get_tuple_arity(gen_message->req) != 2)) {
        ESP_LOGW(TAG, "Received invalid message.");
        return;
    }
    term new_pid_term = term_get_tuple_element(gen_message->req, 1);
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
    TRACE("START socket_consume_mailbox\n");
    GlobalContext *glb = ctx->global;

    while (mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        term msg = message->message;

        TRACE("message: ");
        #ifdef ENABLE_TRACE
            term_display(stdout, msg, ctx);
        #endif
        TRACE("\n");

        if (term_is_tuple(msg) && term_get_tuple_element(msg, 0) == globalcontext_make_atom(glb, netconn_event_internal)) {
            int len = term_to_int32(term_get_tuple_element(msg, 1));
            NativeHandlerResult result = do_netconn_event(ctx, len);
            if (result == NativeTerminate) {
                // We don't need to remove message.
                return NativeTerminate;
            }
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        GenMessage gen_message;
        if (UNLIKELY((port_parse_gen_message(msg, &gen_message) != GenCallMessage)
                || !term_is_tuple(gen_message.req) || term_get_tuple_arity(gen_message.req) < 1)) {
            ESP_LOGW(TAG, "Received invalid message.");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            return NativeContinue;
        }

        term cmd_name = term_get_tuple_element(gen_message.req, 0);

        switch (cmd_name) {
            //TODO: remove this
            case INIT_ATOM:
                TRACE("init\n");
                do_init(ctx, &gen_message);
                break;

            case SENDTO_ATOM:
                TRACE("sendto\n");
                do_sendto(ctx, &gen_message);
                break;

            case SEND_ATOM:
                TRACE("send\n");
                do_send(ctx, &gen_message);
                break;

            case RECVFROM_ATOM:
                TRACE("recvfrom\n");
                if (do_recvfrom(ctx, &gen_message) == NativeTerminate) {
                    return NativeTerminate;
                }
                break;

            case RECV_ATOM:
                TRACE("recv\n");
                if (do_recvfrom(ctx, &gen_message) == NativeTerminate) {
                    return NativeTerminate;
                }
                break;

            case ACCEPT_ATOM:
                TRACE("accept\n");
                do_accept(ctx, &gen_message);
                break;

            case CLOSE_ATOM:
                TRACE("close\n");
                do_close(ctx, &gen_message);
                // We don't need to remove message.
                return NativeTerminate;

            case GET_PORT_ATOM:
                TRACE("get_port\n");
                do_get_port(ctx, &gen_message);
                break;

            case SOCKNAME_ATOM:
                TRACE("sockname\n");
                do_sockname(ctx, &gen_message);
                break;

            case PEERNAME_ATOM:
                TRACE("peername\n");
                do_peername(ctx, &gen_message);
                break;

            case CONTROLLING_PROCESS_ATOM:
                TRACE("controlling_process\n");
                do_controlling_process(ctx, &gen_message);
                break;

            default:
                TRACE("badarg\n");
                break;
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    TRACE("END socket_consume_mailbox\n");
    return NativeContinue;
}

static Context *socket_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    Context *ctx = context_new(global);
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = NULL;
    return ctx;
}

REGISTER_PORT_DRIVER(socket, socket_driver_init, socket_driver_destroy, socket_driver_create_port)

#endif
