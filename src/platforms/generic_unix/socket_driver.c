/***************************************************************************
 *   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
 *   Copyright 2018 by Fred Dushin <fred@dushin.net>                       *
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
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/udp.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include "trace.h"
#include "sys.h"
#include "platform_defaultatoms.h"
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
} SocketDriverData;
static void passive_recvfrom_callback(EventListener *listener);
static void active_recvfrom_callback(EventListener *listener);
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
        if (getsockname(socket_data->sockfd, (struct sockaddr *) &serveraddr, &address_len) == -1) {
            return port_create_sys_error_tuple(ctx, GETSOCKNAME_ATOM, errno);
        } else {
            socket_data->port = ntohs(serveraddr.sin_port);
            return OK_ATOM;
        }
    }
}
static term init_udp_socket(SocketDriverData *socket_data, Context *ctx, term params)
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
    }
    return ret;
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
    if (proto == UDP_ATOM) {
        socket_data->proto = UDP_ATOM;
        term controlling_process = interop_proplist_get_value_default(params, CONTROLLING_PROCESS_ATOM, term_invalid_term());
        if (!(term_is_invalid_term(controlling_process) || term_is_pid(controlling_process))) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        socket_data->controlling_process = controlling_process;
        term binary = interop_proplist_get_value_default(params, BINARY_ATOM, TRUE_ATOM);
        if (!(binary == TRUE_ATOM || binary == FALSE_ATOM)) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        socket_data->binary = binary;
        term buffer = interop_proplist_get_value_default(params, BUFFER_ATOM, term_from_int(BUFSIZE));
        if (!term_is_integer(buffer)) {
            return port_create_error_tuple(ctx, BADARG_ATOM);
        }
        socket_data->buffer = buffer;
        term ret = init_udp_socket(socket_data, ctx, params);
        term active = interop_proplist_get_value_default(params, ACTIVE_ATOM, FALSE_ATOM);
        socket_data->active = active;
        if (ret == OK_ATOM && active == TRUE_ATOM) {
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
        }
        return ret;
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1){
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }
}
term socket_driver_do_send(Context *ctx, term dest_address, term dest_port, term buffer)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(struct sockaddr_in));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(socket_tuple_to_addr(dest_address));
    addr.sin_port = htons(term_to_int32(dest_port));
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
    TRACE("send: data with len: %i, to: %i, port: %i\n", len, ntohl(addr.sin_addr.s_addr), ntohs(addr.sin_port));
    int sent_data = sendto(socket_data->sockfd, buf, len, 0, (struct sockaddr *) &addr, sizeof(addr));
    if (sent_data == -1) {
        return port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno);
    } else {
        term sent_atom = term_from_int32(sent_data);
        return port_create_ok_tuple(ctx, sent_atom);
    }
}
typedef struct RecvFromData {
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
} RecvFromData;
static void passive_recvfrom_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    GlobalContext *global = ctx->global;
    linkedlist_remove(&global->listeners, &listener->listeners_list_head);
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    avm_int_t buf_size = term_to_int(socket_data->buffer);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, (struct sockaddr *) &clientaddr, &clientlen);
    if (len == -1) {
        // {Ref, {error, {SysCall, Errno}}}
        // tuple arity 2:       3
        // tuple arity 2:       3
        // tuple arity 2:       3
        // ref:                 3 (max)
        port_ensure_available(ctx, 12);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        port_send_reply(ctx, pid, ref, port_create_sys_error_tuple(ctx, SENDTO_ATOM, errno));
    } else {
        // {Ref, {ok, {{int,int,int,int}, int, binary}}}
        // tuple arity 2:       3
        // tuple arity 3:       4
        // tuple arity 4:       5
        // tuple arity 2:       3
        // ref:                 3 (max)
        // binary:              2 + len(binary)/WORD_SIZE + 1
        port_ensure_available(ctx, 20 + len/(TERM_BITS/8) + 1);
        term pid = recvfrom_data->pid;
        term ref = term_from_ref_ticks(recvfrom_data->ref_ticks, ctx);
        term addr = socket_tuple_from_addr(ctx, htonl(clientaddr.sin_addr.s_addr));
        term port = term_from_int32(htons(clientaddr.sin_port));
        term packet = socket_create_packet_term(ctx, buf, len, socket_data->binary == TRUE_ATOM);
        term addr_port_packet = port_create_tuple3(ctx, addr, port, packet);
        term reply = port_create_ok_tuple(ctx, addr_port_packet);
        port_send_reply(ctx, pid, ref, reply);
    }
    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
}
static void active_recvfrom_callback(EventListener *listener)
{
    Context *ctx = (Context *) listener->data;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    avm_int_t buf_size = term_to_int(socket_data->buffer);
    char *buf = malloc(buf_size);
    if (IS_NULL_PTR(buf)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    ssize_t len = recvfrom(socket_data->sockfd, buf, buf_size, 0, (struct sockaddr *) &clientaddr, &clientlen);
    if (len == -1) {
        // {udp, Socket, {error, {SysCall, Errno}}}
        // tuple arity 2:       3
        // tuple arity 2:       3
        // tuple arity 4:       5
        port_ensure_available(ctx, 12);
        term pid = socket_data->controlling_process;
        term msgs[5] = {UDP_ATOM, term_from_local_process_id(ctx->process_id), port_create_sys_error_tuple(ctx, RECVFROM_ATOM, errno)};
        term msg = port_create_tuple_n(ctx, 5, msgs);
        port_send_message(ctx, pid, msg);
    } else {
        // {udp, pid, {int,int,int,int}, int, binary}
        // tuple arity 5:       6
        // atom:                1
        // pid:                 1
        // tuple arity 4:       5
        // port:                1
        // binary:              2 + len(binary)/WORD_SIZE + 1
        port_ensure_available(ctx, 20 + len/(TERM_BITS/8) + 1);
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
void socket_driver_do_recvfrom(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;
    if (socket_data->active == TRUE_ATOM) {
        port_ensure_available(ctx, 12);
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
    }
    EventListener *listener = malloc(sizeof(EventListener));
    if (IS_NULL_PTR(listener)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        abort();
    }
    RecvFromData *data = (RecvFromData *) malloc(sizeof(RecvFromData));
    if (IS_NULL_PTR(data)) {
        fprintf(stderr, "Unable to allocate space for RecvFromData: %s:%i\n", __FILE__, __LINE__);
        abort();
    }
    data->ctx = ctx;
    data->pid = pid;
    data->ref_ticks = term_to_ref_ticks(ref);
    linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);
    listener->fd = socket_data->sockfd;
    listener->expires = 0;
    listener->expiral_timestamp.tv_sec = 60*60*24; // TODO
    listener->expiral_timestamp.tv_nsec = 0;
    listener->one_shot = 1;
    listener->data = data;
    listener->handler = passive_recvfrom_callback;
}
