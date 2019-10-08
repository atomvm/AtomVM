/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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

#include "socket.h"
#include "socket_driver.h"
#include "port.h"

#include <string.h>

#include "atom.h"
#include "context.h"
#include "globalcontext.h"
#include "interop.h"
#include "utils.h"
#include "term.h"

#include "platform_defaultatoms.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <unistd.h>

#include <esp_log.h>
#include <lwip/ip_addr.h>
#include <lwip/inet.h>
#include <lwip/api.h>

#include "esp32_sys.h"
#include "sys.h"
#include "term.h"

// #define ENABLE_TRACE 1
#include "trace.h"

#define BUFSIZE 128

static void active_recv_callback(EventListener *listener);
static void passive_recv_callback(EventListener *listener);
static void active_recvfrom_callback(EventListener *listener);
static void passive_recvfrom_callback(EventListener *listener);

typedef struct SocketDriverData
{
    struct netconn *conn;
    uint64_t ref_ticks;
    term proto;
    term port;
    term controlling_process;
    term binary;
    term active;
    term buffer;
    EventListener *active_listener;
} SocketDriverData;

void *socket_driver_create_data()
{
    struct SocketDriverData *data = calloc(1, sizeof(struct SocketDriverData));
    data->conn = NULL;
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

void socket_callback(struct netconn *netconn, enum netconn_evt evt, u16_t len)
{
    TRACE("socket: netconn callback, evt: %i, len: %i\n", evt, len);

    if (evt != NETCONN_EVT_RCVPLUS) {
        return;
    }

    int event_descriptor = find_event_descriptor(netconn);
    if (UNLIKELY(event_descriptor < 0)) {
        abort();
    }

    int result = xQueueSend(event_queue, &event_descriptor, 0);
    if (result != pdTRUE) {
        fprintf(stderr, "socket: failed to enqueue: %i.\n", result);
    }
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

static void tuple_to_ip_addr(term address_tuple, ip_addr_t *out_addr)
{
    out_addr->type = IPADDR_TYPE_V4;
    out_addr->u_addr.ip4.addr = htonl(socket_tuple_to_addr(address_tuple));
}


static term do_bind(Context *ctx, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    term address_term = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port_term = interop_proplist_get_value(params, PORT_ATOM);

    UNUSED(address_term);

    u16_t port = term_to_int32(port_term);

    TRACE("socket: binding to IP_ADDR_ANY on port %i\n", (int) port);

    //TODO: replace IP_ADDR_ANY
    if (UNLIKELY(netconn_bind(socket_data->conn, IP_ADDR_ANY, port) != ERR_OK)) {
        TRACE("socket: Failed to bin");
        return port_create_sys_error_tuple(ctx, BIND_ATOM, errno);
    }

    ip_addr_t naddr;

    if (UNLIKELY(netconn_getaddr(socket_data->conn, &naddr, &port, 1) != ERR_OK)) {
        TRACE("socket: Failed to getaddr");
        return port_create_sys_error_tuple(ctx, GETSOCKNAME_ATOM, errno);
    } else {
        TRACE("socket: binded");
        socket_data->port = term_from_int32(port);
        return OK_ATOM;
    }
}

static term init_udp_socket(Context *ctx, SocketDriverData *socket_data, term params, term active)
{
    TRACE("socket: creating udp netconn\n");

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_UDP, 0, socket_callback);
    // TODO check for invalid return
    socket_data->conn = conn;

    int event_descriptor = open_event_descriptor(conn);

    term ret = do_bind(ctx, params);

    socket_data->active = active;
    if (ret == OK_ATOM && active == TRUE_ATOM) {
        EventListener *listener = malloc(sizeof(EventListener));
        if (IS_NULL_PTR(listener)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            abort();
        }
        listener->fd = event_descriptor;
        listener->expires = 0;
        listener->expiral_timestamp.tv_sec = INT_MAX;
        listener->expiral_timestamp.tv_nsec = INT_MAX;
        listener->one_shot = 0;
        listener->data = ctx;
        listener->handler = active_recvfrom_callback;
        linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
        socket_data->active_listener = listener;
        TRACE("socket: initialized\n");
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    return ret;
}

static term do_connect(SocketDriverData *socket_data, Context *ctx, term address, term port)
{
    // TODO handle IP addresses
    if (!term_is_list(address)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    int ok;
    char *addr_str = interop_term_to_string(address, &ok);
    if (!ok) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    TRACE("socket_driver: resolving %s ...\n", addr_str);

    struct ip_addr remote_ip; 
    err_t status = netconn_gethostbyname(addr_str, &remote_ip);

    free(addr_str);

    if (status != ERR_OK) {
        // TODO atom
        return port_create_sys_error_tuple(ctx, UNDEFINED_ATOM, status);
    }

    int port_val = term_to_int(port);
    
    TRACE("socket_driver: connecting to port %i ...\n", port_val);
    status = netconn_connect(socket_data->conn, &remote_ip, port_val);

    if (status != ERR_OK) {
        // TODO atom
        return port_create_sys_error_tuple(ctx, CONNECT_ATOM, status);
    } else {
        TRACE("socket_driver: connected.\n");
        return OK_ATOM;
    }
}

static term init_client_tcp_socket(Context *ctx, SocketDriverData *socket_data, term params, term active)
{
    TRACE("socket: creating tcp netconn\n");

    struct netconn *conn = netconn_new_with_proto_and_callback(NETCONN_TCP, 0, socket_callback);
    // TODO check for invalid return
    socket_data->conn = conn;

    int event_descriptor = open_event_descriptor(conn);

    term address = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_connect(socket_data, ctx, address, port);
    if (ret != OK_ATOM) {
        // TODO
        // close(sockfd);
    } else {
        if (active == TRUE_ATOM) {
            EventListener *listener = malloc(sizeof(EventListener));
            if (IS_NULL_PTR(listener)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                abort();
            }
            listener->fd = event_descriptor;
            listener->expires = 0;
            listener->expiral_timestamp.tv_sec = INT_MAX;
            listener->expiral_timestamp.tv_nsec = INT_MAX;
            listener->one_shot = 0;
            listener->data = ctx;
            listener->handler = active_recv_callback;
            linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
            socket_data->active_listener = listener;
        }
    }
    return ret;
}

term socket_driver_do_init(Context *ctx, term params)
{
    TRACE("socket: init\n");

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
    term binary = interop_proplist_get_value_default(params, BINARY_ATOM, TRUE_ATOM);
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
        return init_udp_socket(ctx, socket_data, params, active);
    } else if (proto == TCP_ATOM) {
        term connect = interop_proplist_get_value_default(params, CONNECT_ATOM, FALSE_ATOM);
        if (connect == TRUE_ATOM) {
            return init_client_tcp_socket(ctx, socket_data, params, active);
        } else {
            // TODO add TCP server support
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
}

void socket_driver_do_close(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (socket_data->active == TRUE_ATOM) {
        linkedlist_remove(&ctx->global->listeners, &socket_data->active_listener->listeners_list_head);
    }
    if (netconn_close(socket_data->conn) != ERR_OK) {
        TRACE("socket: close failed");
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

//
// send operations
//

term socket_driver_do_send(Context *ctx, term buffer)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    
    const char *buf = NULL;
    size_t len = 0;
    if (term_is_binary(buffer)) {
        buf = term_binary_data(buffer);
        len = term_binary_size(buffer);
    } else if (term_is_list(buffer)) {
        int ok;
        buf = interop_list_to_string(buffer, &ok);
        if (!ok) {
            // TODO error
        }
        len = strlen(buf);
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    
    err_t status = netconn_write(socket_data->conn, buf, len, NETCONN_NOCOPY);
    if (status != ERR_OK) {
        return port_create_sys_error_tuple(ctx, SEND_ATOM, status);
    } else {
        TRACE("socket_driver: sent data with len: %i\n", len);
        term sent_atom = term_from_int(len);
        return port_create_ok_tuple(ctx, sent_atom);
    }
}

term socket_driver_do_sendto(Context *ctx, term dest_address, term dest_port, term buffer)
{
    TRACE("socket: Going to send data\n");

    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    const char *buf = NULL;
    size_t len = 0;
    if (term_is_binary(buffer)) {
        buf = term_binary_data(buffer);
        len = term_binary_size(buffer);
    } else if (term_is_list(buffer)) {
        int ok;
        buf = interop_list_to_string(buffer, &ok);
        if (UNLIKELY(!ok)) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        len = strlen(buf);
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    struct netbuf *sendbuf = netbuf_new();
    if (IS_NULL_PTR(sendbuf)) {
        TRACE("socket: netbuf alloc failed\n");
        return port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno);
    }

    ip_addr_t ip4addr;
    tuple_to_ip_addr(dest_address, &ip4addr);
    uint16_t destport = term_to_int32(dest_port);

    TRACE("socket: send: data with len: %i, to: %x, port: %i\n", len, ip4addr.u_addr.ip4.addr, destport);

    if (UNLIKELY(netbuf_ref(sendbuf, buf, len) != ERR_OK)) {
        TRACE("socket: netbuf_ref fail\n");
        netbuf_delete(sendbuf);
        return port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno);
    }

    if (UNLIKELY(netconn_sendto(socket_data->conn, sendbuf, &ip4addr, destport) != ERR_OK)) {
        TRACE("socket: send failed\n");
        netbuf_delete(sendbuf);
        return port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno);
    }

    netbuf_delete(sendbuf);

    return port_create_ok_tuple(ctx, OK_ATOM);
}

//
// receive operations
//

typedef struct RecvFromData {
    Context *ctx;
    term pid;
    term length;
    uint64_t ref_ticks;
} RecvFromData;

static void active_recv_callback(EventListener *listener)
{
    Context *ctx = listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct netbuf *buf = NULL;

    if (UNLIKELY(netconn_recv(socket_data->conn, &buf) != ERR_OK)) {
        // {tcp, Socket, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[2] = {TCP_CLOSED_ATOM, term_from_local_process_id(ctx->process_id)};
        term msg = port_create_tuple_n(ctx, 2, msgs);
        port_send_message(ctx, pid, msg);
        socket_driver_do_close(ctx);
    } else {
        void *data;
        u16_t data_len;
        netbuf_data(buf, &data, &data_len);

        TRACE("socket_driver: active received data of len: %i\n", data_len);
        // {tcp, pid, binary}
        port_ensure_available(ctx, 20 + data_len/(TERM_BITS/8) + 1);
        term pid = socket_data->controlling_process;
        term packet = socket_create_packet_term(ctx, data, data_len, socket_data->binary == TRUE_ATOM);
        term msgs[3] = {TCP_ATOM, term_from_local_process_id(ctx->process_id), packet};
        term msg = port_create_tuple_n(ctx, 3, msgs);
        port_send_message(ctx, pid, msg);
    }
    netbuf_delete(buf);
}

static void passive_recv_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // receive the data
    //
    struct netbuf *buf = NULL;
    if (UNLIKELY(netconn_recv(socket_data->conn, &buf) != ERR_OK)) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECV_ATOM, errno));
    } else {
        void *data;
        u16_t data_len;
        netbuf_data(buf, &data, &data_len);

        TRACE("socket_driver: passive received data of len: %i\n", data_len);
        port_ensure_available(ctx, 20 + data_len/(TERM_BITS/8) + 1);
        // {Ref, {ok, Packet::binary()}}
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term packet = socket_create_packet_term(ctx, data, data_len, socket_data->binary == TRUE_ATOM);
        term reply = port_create_ok_tuple(ctx, packet);
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    netbuf_delete(buf);
}

static void active_recvfrom_callback(EventListener *listener)
{
    Context *ctx = listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct netbuf *buf = NULL;

    if (UNLIKELY(netconn_recv(socket_data->conn, &buf) != ERR_OK)) {
        // {udp, Socket, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[5] = {UDP_ATOM, term_from_local_process_id(ctx->process_id), port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno)};
        term msg = port_create_tuple_n(ctx, 5, msgs);
        port_send_message(ctx, pid, msg);

    } else {
        void *data;
        uint16_t datalen;
        netbuf_data(buf, &data, &datalen);
        // {udp, pid, {int,int,int,int}, int, binary}
        port_ensure_available(ctx, 20 + datalen/(TERM_BITS/8) + 1);
        term pid = socket_data->controlling_process;
        term addr = socket_addr_to_tuple(ctx, netbuf_fromaddr(buf));
        term port = term_from_int32(netbuf_fromport(buf));
        term packet = socket_create_packet_term(ctx, data, datalen, socket_data->binary == TRUE_ATOM);
        term msgs[5] = {UDP_ATOM, term_from_local_process_id(ctx->process_id), addr, port, packet};
        term msg = port_create_tuple_n(ctx, 5, msgs);
        port_send_message(ctx, pid, msg);
    }
    netbuf_delete(buf);
}

static void passive_recvfrom_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct netbuf *buf = NULL;

    if (UNLIKELY(netconn_recv(socket_data->conn, &buf) != ERR_OK)) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term ref = term_from_ref_ticks(socket_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno));

    } else {
        void *data;
        uint16_t datalen;
        netbuf_data(buf, &data, &datalen);
        // {Ref, {ok, {{int,int,int,int}, int, binary}}}
        port_ensure_available(ctx, 20 + datalen/(TERM_BITS/8) + 1);

        term ref = term_from_ref_ticks(socket_data->ref_ticks, ctx);
        term addr = socket_addr_to_tuple(ctx, netbuf_fromaddr(buf));
        term port = term_from_int32(netbuf_fromport(buf));
        term packet = term_from_literal_binary(data, datalen, ctx);
        term addr_port_packet = port_create_tuple3(ctx, addr, port, packet);
        term reply = port_create_ok_tuple(ctx, addr_port_packet);
        port_send_reply(ctx, socket_data->controlling_process, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    netbuf_delete(buf);
}

static void do_recv(Context *ctx, term pid, term ref, term length, term timeout, event_handler_t handler)
{
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
        abort();
    }
    data->ctx = ctx;
    data->pid = pid;
    data->length = length;
    data->ref_ticks = term_to_ref_ticks(ref);
    //
    // Look up the event descriptor for our socket
    //
    int event_descriptor = find_event_descriptor(socket_data->conn);
    if (UNLIKELY(event_descriptor < 0)) {
        abort();
    }
    //
    // Create an event listener with this request-specific data, and append to the global list
    //
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    listener->fd = event_descriptor;
    listener->expires = 0;
    listener->expiral_timestamp.tv_sec = 60*60*24; // TODO handle timeout
    listener->expiral_timestamp.tv_nsec = 0;
    listener->one_shot = 1;
    listener->handler = handler;
    listener->data = data;
    linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
}

void socket_driver_do_recvfrom(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recvfrom_callback);
}

void socket_driver_do_recv(Context *ctx, term pid, term ref, term length, term timeout)
{
    do_recv(ctx, pid, ref, length, timeout, passive_recv_callback);
}
