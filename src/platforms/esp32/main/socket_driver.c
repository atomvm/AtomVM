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
#include "trace.h"
#include "sys.h"
#include "term.h"

static void recvfrom_callback(Context *ctx);

typedef struct SocketDriverData
{
    struct netconn *conn;
    uint64_t ref_ticks;
    term listener_pid;
} SocketDriverData;

void *socket_driver_create_data()
{
    struct SocketDriverData *data = calloc(1, sizeof(struct SocketDriverData));
    data->conn = NULL;
    data->ref_ticks = 0;
    data->listener_pid = term_invalid_term();

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

void socket_handling_callback(EventListener *listener)
{
    Context *socket_ctx = listener->data;

    SocketDriverData *socket_data = (SocketDriverData *) socket_ctx->platform_data;
    if (!term_is_invalid_term(socket_data->listener_pid)) {
        recvfrom_callback(socket_ctx);
    } else {
        TRACE("socket: no listening context, discarding data.\n");
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

    enum netconn_type conn_type;

    if (proto == UDP_ATOM) {
        conn_type = NETCONN_UDP;

    } else if (proto == TCP_ATOM) {
        conn_type = NETCONN_TCP;

    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    TRACE("socket: creating netconn\n");

    struct netconn *conn = netconn_new_with_proto_and_callback(conn_type, 0, socket_callback);
    socket_data->conn = conn;

    int event_descriptor = open_event_descriptor(conn);

    GlobalContext *global = ctx->global;

    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    linkedlist_append(&global->listeners, &listener->listeners_list_head);
    listener->fd = event_descriptor;

    listener->expires = 0;
    listener->expiral_timestamp.tv_sec = INT_MAX;
    listener->expiral_timestamp.tv_nsec = INT_MAX;
    listener->one_shot = 0;
    listener->data = ctx;
    listener->handler = socket_handling_callback;

    TRACE("socket: initialized\n");

    return OK_ATOM;
}


term socket_driver_do_bind(Context *ctx, term address_term, term port_term)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    UNUSED(address_term);

    u16_t port = term_to_int32(port_term);

    TRACE("socket: binding to IP_ADDR_ANY on port %i\n", (int) port);

    //TODO: replace IP_ADDR_ANY
    if (UNLIKELY(netconn_bind(socket_data->conn, IP_ADDR_ANY, port) != ERR_OK)) {
        return port_create_sys_error_tuple(ctx, BIND_ATOM, errno);
    }

    ip_addr_t naddr;

    if (UNLIKELY(netconn_getaddr(socket_data->conn, &naddr, &port, 1) != ERR_OK)) {
        return port_create_sys_error_tuple(ctx, GETSOCKNAME_ATOM, errno);
    } else {
        term port_atom = term_from_int32(port);
        return port_create_ok_tuple(ctx, port_atom);
    }

    TRACE("socket: binded");
}

term socket_driver_do_send(Context *ctx, term dest_address, term dest_port, term buffer)
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

static void recvfrom_callback(Context *ctx)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct netbuf *buf = NULL;

    if (UNLIKELY(netconn_recv(socket_data->conn, &buf) != ERR_OK)) {
        // {Ref, {error, {SysCall, Errno}}}
        // tuple arity 2:       3
        // tuple arity 2:       3
        // tuple arity 2:       3
        // ref:                 3 (max)
        port_ensure_available(ctx, 12);
        term pid = socket_data->listener_pid;
        term ref = term_from_ref_ticks(socket_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno));

    } else {
        void *data;
        uint16_t datalen;
        netbuf_data(buf, &data, &datalen);

        // {Ref, {ok, {{int,int,int,int}, int, binary}}}
        // tuple arity 2:       3
        // tuple arity 3:       4
        // tuple arity 4:       5
        // tuple arity 2:       3
        // ref:                 3 (max)
        // binary:              2 + len(binary)/WORD_SIZE + 1
        port_ensure_available(ctx, 20 + datalen/(TERM_BITS/8) + 1);

        term ref = term_from_ref_ticks(socket_data->ref_ticks, ctx);
        term addr = socket_addr_to_tuple(ctx, netbuf_fromaddr(buf));
        term port = term_from_int32(netbuf_fromport(buf));
        term packet = term_from_literal_binary(data, datalen, ctx);
        term addr_port_packet = port_create_tuple3(ctx, addr, port, packet);
        term reply = port_create_ok_tuple(ctx, addr_port_packet);
        port_send_reply(ctx, socket_data->listener_pid, ref, reply);
    }

    netbuf_delete(buf);
}

void socket_driver_do_recvfrom(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    socket_data->listener_pid = pid;
    socket_data->ref_ticks = term_to_ref_ticks(ref);
}
