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
} SocketDriverData;


void *socket_driver_create_data()
{
    struct SocketDriverData *data = calloc(1, sizeof(struct SocketDriverData));
    return (void *) data;
}


void socket_driver_delete_data(void *data)
{
    free(data);
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
        int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
        if (sockfd == -1) {
            return port_create_sys_error_tuple(ctx, SOCKET_ATOM, errno);
        }
        socket_data->sockfd = sockfd;
    } else if (proto == TCP_ATOM) {
        socket_data->sockfd = socket(AF_INET, SOCK_STREAM, 0);
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1){
        return port_create_sys_error_tuple(ctx, FCNTL_ATOM, errno);
    }

    return OK_ATOM;
}


term socket_driver_do_bind(Context *ctx, term address, term port)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in serveraddr;

    UNUSED(address);
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY); // TODO
    serveraddr.sin_port = htons(term_to_int32(port));

    socklen_t address_len = sizeof(serveraddr);
    if (bind(socket_data->sockfd, (struct sockaddr *) &serveraddr, address_len) == -1) {
        return port_create_sys_error_tuple(ctx, BIND_ATOM, errno);
    } else {
        if (getsockname(socket_data->sockfd, (struct sockaddr *) &serveraddr, &address_len) == -1) {
            return port_create_sys_error_tuple(ctx, GETSOCKNAME_ATOM, errno);
        } else {
            term port_atom = term_from_int32(ntohs(serveraddr.sin_port));
            return port_create_ok_tuple(ctx, port_atom);
        }
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

static void recvfrom_callback(EventListener *listener)
{
    RecvFromData *recvfrom_data = (RecvFromData *) listener->data;
    Context *ctx = recvfrom_data->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    GlobalContext *global = ctx->global;
    linkedlist_remove(&global->listeners, &listener->listeners_list_head);

    struct sockaddr_in clientaddr;
    socklen_t clientlen = sizeof(clientaddr);
    char *buf = malloc(256*sizeof(char));
    if (UNLIKELY(!buf)) {
        fprintf(stderr, "malloc %s:%d", __FILE__, __LINE__);
        abort();
    }

    ssize_t len = recvfrom(socket_data->sockfd, buf, BUFSIZE, 0, (struct sockaddr *) &clientaddr, &clientlen);
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
        term packet = socket_create_packet_term(ctx, buf, len);
        term addr_port_packet = port_create_tuple3(ctx, addr, port, packet);
        term reply = port_create_ok_tuple(ctx, addr_port_packet);
        port_send_reply(ctx, pid, ref, reply);
    }

    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
}

void socket_driver_do_recvfrom(Context *ctx, term pid, term ref)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

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
    listener->handler = recvfrom_callback;
}
