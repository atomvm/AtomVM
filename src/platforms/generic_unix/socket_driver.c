/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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
#include "socket.h"
#include "socket_driver.h"
#include "port.h"
#include <string.h>
#include "atom.h"
#include "context.h"
#include "generic_unix_sys.h"
#include "globalcontext.h"
#include "interop.h"
#include "utils.h"
#include "term.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/udp.h>
#include <netdb.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include "sys.h"
#include "platform_defaultatoms.h"

// #define ENABLE_TRACE
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

static void active_recv_callback(EventListener *listener);
static void passive_recv_callback(EventListener *listener);
static void active_recvfrom_callback(EventListener *listener);
static void passive_recvfrom_callback(EventListener *listener);

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
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY); // TODO
    avm_int_t p = term_to_int(port);
    serveraddr.sin_port = htons(p);
    socklen_t address_len = sizeof(serveraddr);
    if (bind(socket_data->sockfd, (struct sockaddr *) &serveraddr, address_len) == -1) {
        return port_create_sys_error_tuple(ctx, BIND_ATOM, errno);
    } else {
        TRACE("socket_driver: bound to %ld\n", p);
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
    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd == -1) {
        return port_create_sys_error_tuple(ctx, SOCKET_ATOM, errno);
    }
    socket_data->sockfd = sockfd;
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1) {
        close(sockfd);
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }
    term address = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_bind(ctx, address, port);
    if (ret != OK_ATOM) {
        close(sockfd);
    } else {
        if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1){
            return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
        }
        if (active == TRUE_ATOM) {
            EventListener *listener = malloc(sizeof(EventListener));
            if (IS_NULL_PTR(listener)) {
                fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                abort();
            }
            listener->fd = socket_data->sockfd;
            listener->expires = 0;
            listener->expiral_timestamp.tv_sec = INT_MAX;
            listener->expiral_timestamp.tv_nsec = INT_MAX;
            listener->one_shot = 0;
            listener->data = ctx;
            listener->handler = active_recvfrom_callback;
            linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
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
    TRACE("socket_driver: resolving to %s:%s over socket fd %i\n", addr_str, port_str, term_to_int32(socket_data->sockfd));

    struct addrinfo *server_info;
    int status = getaddrinfo(addr_str, port_str, &hints, &server_info);

    free(addr_str);

    if (status != 0) {
        return port_create_sys_error_tuple(ctx, GETADDRINFO_ATOM, status);
    }

    struct sockaddr *addr = NULL;
    size_t addr_len = 0;
    for (struct addrinfo *p = server_info; p != NULL;  p = p->ai_next) {
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
        TRACE("socket_driver: connected.\n");
        return OK_ATOM;
    }
}

static term init_client_tcp_socket(Context *ctx, SocketDriverData *socket_data, term params, term active)
{
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
                abort();
            }
            listener->fd = socket_data->sockfd;
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

    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1) {
        close(sockfd);
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }
    term address = interop_proplist_get_value(params, ADDRESS_ATOM);
    term port = interop_proplist_get_value(params, PORT_ATOM);
    term ret = do_bind(ctx, address, port);
    if (ret != OK_ATOM) {
        close(sockfd);
    } else {
        ret = do_listen(socket_data, ctx, params);
        if (ret != OK_ATOM) {
            close(sockfd);
        } else {
            TRACE("socket_driver: listening on port %u\n", (unsigned) term_to_int(port));
        }
    }
    return ret;
}

static term init_accepting_socket(Context *ctx, SocketDriverData *socket_data, term fd, term active)
{
    socket_data->sockfd = term_to_int(fd);

    if (active == TRUE_ATOM) {
        EventListener *listener = malloc(sizeof(EventListener));
        if (IS_NULL_PTR(listener)) {
            fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            abort();
        }
        listener->fd = socket_data->sockfd;
        listener->expires = 0;
        listener->expiral_timestamp.tv_sec = INT_MAX;
        listener->expiral_timestamp.tv_nsec = INT_MAX;
        listener->one_shot = 0;
        listener->data = ctx;
        listener->handler = active_recv_callback;
        linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
        socket_data->active_listener = listener;
    }
    return OK_ATOM;
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
                term accept = interop_proplist_get_value_default(params, ACCEPT_ATOM, FALSE_ATOM);
                if (accept == TRUE_ATOM) {
                    term fd = interop_proplist_get_value(params, FD_ATOM);
                    if (!term_is_integer(fd)) {
                        return port_create_error_tuple(ctx, BADARG_ATOM);
                    } else {
                        return init_accepting_socket(ctx, socket_data, fd, active);
                    }
                } else {
                    return port_create_error_tuple(ctx, BADARG_ATOM);
                }
            }
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
    if (close(socket_data->sockfd) == -1) {
        TRACE("socket: close failed");
    } else {
        TRACE("socket_driver: closed socket\n");
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

    char *buf;
    size_t len;
    if (term_is_binary(buffer)) {
        buf = (char *) term_binary_data(buffer);
        len = term_binary_size(buffer);
    } else if (term_is_list(buffer)) {
        int ok;
        len = interop_iolist_size(buffer, &ok);
        if (UNLIKELY(!ok)) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        buf = malloc(len);
        if (UNLIKELY(!interop_write_iolist(buffer, buf))) {
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
        TRACE("socket_driver: sent data with len: %li\n", len);
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
        int ok;
        len = interop_iolist_size(buffer, &ok);
        if (UNLIKELY(!ok)) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        buf = malloc(len);
        if (UNLIKELY(!interop_write_iolist(buffer, buf))) {
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
        TRACE("socket_driver: sent data with len: %li, to: %i, port: %i\n", len, ntohl(addr.sin_addr.s_addr), ntohs(addr.sin_port));
        term sent_atom = term_from_int32(sent_data);
        return port_create_ok_tuple(ctx, sent_atom);
    }
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
    Context *ctx = (Context *) listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(socket_data->buffer);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    //
    // receive the data
    //
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, NULL, NULL);
    if (len <= 0) {
        // {tcp, Socket, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[2] = {TCP_CLOSED_ATOM, term_from_local_process_id(ctx->process_id)};
        term msg = port_create_tuple_n(ctx, 2, msgs);
        port_send_message(ctx, pid, msg);
        socket_driver_do_close(ctx);
    } else {
        TRACE("socket_driver: received data of len: %li\n", len);
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
        term msgs[3] = {TCP_ATOM, term_from_local_process_id(ctx->process_id), packet};
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
    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(recvfrom_data->length);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    //
    // receive the data
    //
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, NULL, NULL);
    if (len <= 0) {
        // {Ref, {error, {SysCall, Errno}}}
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, RECV_ATOM, errno));
    } else {
        TRACE("socket_driver: passive received data of len: %li\n", len);
        int ensure_packet_avail;
        int binary;
        if (socket_data->binary == TRUE_ATOM) {
            binary = 1;
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            binary = 0;
            ensure_packet_avail = len * 2;
        }
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        // {Ref, {ok, Packet::binary()}}
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term packet = socket_create_packet_term(ctx, buf, len, ensure_packet_avail);
        term reply = port_create_ok_tuple(ctx, packet);
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
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
        abort();
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
        term msgs[3] = {UDP_ATOM, term_from_local_process_id(ctx->process_id), port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno)};
        term msg = port_create_tuple_n(ctx, 3, msgs);
        port_send_message(ctx, pid, msg);
    } else {
        int ensure_packet_avail;
        int binary;
        if (socket_data->binary == TRUE_ATOM) {
            binary = 1;
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            binary = 0;
            ensure_packet_avail = len * 2;
        }
        // {udp, pid, {int,int,int,int}, int, binary}
        port_ensure_available(ctx, 20 + ensure_packet_avail);
        term pid = socket_data->controlling_process;
        term addr = socket_tuple_from_addr(ctx, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_create_packet_term(ctx, buf, len, socket_data->binary == TRUE_ATOM);
        term msgs[5] = {UDP_ATOM, term_from_local_process_id(ctx->process_id), addr, port, packet};
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
    //
    // allocate the receive buffer
    //
    avm_int_t buf_size = term_to_int(recvfrom_data->length);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
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
        int binary;
        if (socket_data->binary == TRUE_ATOM) {
            binary = 1;
            ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
        } else {
            binary = 0;
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
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
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
    // Create an event listener with this request-specific data, and append to the global list
    //
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    listener->fd = socket_data->sockfd;
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

//
// accept
//

static void accept_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
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
        // {Ref, {ok, Fd::int()}}
        port_ensure_available(ctx, 10);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term reply = port_create_ok_tuple(ctx, term_from_int(fd));
        port_send_reply(ctx, pid, ref, reply);
    }
    //
    // remove the EventListener from the global list and clean up
    //
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
}

void socket_driver_do_accept(Context *ctx, term pid, term ref, term timeout)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
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
    data->length = 0;
    data->ref_ticks = term_to_ref_ticks(ref);
    //
    // Create an event listener with this request-specific data, and append to the global list
    //
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    listener->fd = socket_data->sockfd;
    listener->expires = 0;
    listener->expiral_timestamp.tv_sec = 60*60*24; // TODO handle timeout
    listener->expiral_timestamp.tv_nsec = 0;
    listener->one_shot = 1;
    listener->handler = accept_callback;
    listener->data = data;
    linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);

}
