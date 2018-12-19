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
#include "ccontext.h"
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

#define BUFSIZE 128

static const char *const tag_proto_a = "\x5" "proto";
static const char *const proto_udp_a = "\x3" "udp";
static const char *const proto_tcp_a = "\x3" "tcp";

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


term_ref socket_driver_do_init(CContext *cc, term params)
{
    Context *ctx = cc->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    if (!term_is_list(params)) {
        return port_create_error_tuple(cc, "badarg: params is not a list");
    }
    term proto = interop_proplist_get_value(params, context_make_atom(ctx, tag_proto_a));

    if (term_is_nil(proto)) {
        return port_create_error_tuple(cc, "badarg: no proto in params");
    }

    if (proto == context_make_atom(ctx, proto_udp_a)) {
        int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
        if (sockfd == -1) {
            const char *error_string = strerror(errno);
            return port_create_error_tuple(cc, error_string);
        }
        socket_data->sockfd = sockfd;
    } else if (proto == context_make_atom(ctx, proto_tcp_a)) {
        socket_data->sockfd = socket(AF_INET, SOCK_STREAM, 0);
    } else {
        return port_create_error_tuple(cc, "badarg: unsupported protocol");
    }
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1){
        const char *error_string = strerror(errno);
        return port_create_error_tuple(cc, error_string);
    }

    return ccontext_make_term_ref(cc, context_make_atom(ctx, port_ok_a));
}


term_ref socket_driver_do_bind(CContext *cc, term address, term port)
{
    Context *ctx = cc->ctx;
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    struct sockaddr_in serveraddr;

    UNUSED(address);
    memset(&serveraddr, 0, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY); // TODO
    serveraddr.sin_port = htons(term_to_int32(port));

    socklen_t address_len = sizeof(serveraddr);
    if (bind(socket_data->sockfd, (struct sockaddr *) &serveraddr, address_len) == -1) {
        const char *error_string = strerror(errno);
        return port_create_error_tuple(cc, error_string);
    } else {
        if (getsockname(socket_data->sockfd, (struct sockaddr *) &serveraddr, &address_len) == -1) {
            const char *error_string = strerror(errno);
            return port_create_error_tuple(cc, error_string);
        } else {
            term port_atom = term_from_int32(ntohs(serveraddr.sin_port));
            return port_create_ok_tuple(cc, ccontext_make_term_ref(cc, port_atom));
        }
    }
}

term_ref socket_driver_do_send(CContext *cc, term dest_address, term dest_port, term buffer)
{
    Context *ctx = cc->ctx;
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
        buf = interop_list_to_string(buffer);
        len = strlen(buf);
    } else {
        return port_create_error_tuple(cc, "unsupported type for send");
    }

    TRACE("send: data with len: %i, to: %i, port: %i\n", len, ntohl(addr.sin_addr.s_addr), ntohs(addr.sin_port));

    int sent_data = sendto(socket_data->sockfd, buf, len, 0, (struct sockaddr *) &addr, sizeof(addr));
    if (sent_data == -1) {
        const char *error_string = strerror(errno);
        return port_create_error_tuple(cc, error_string);
    } else {
        term sent_atom = term_from_int32(sent_data);
        return port_create_ok_tuple(cc, ccontext_make_term_ref(cc, sent_atom));
    }
}

typedef struct RecvFromData {
    Context *ctx;
    term pid;
    uint64_t ref_ticks;
} RecvFromData;

static void recvfrom_callback(void *data)
{
    EventListener *listener = (EventListener *) data;
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

    struct CContext *cc = malloc(sizeof(struct CContext));
    if (!cc) {
        fprintf(stderr, "malloc %s:%d", __FILE__, __LINE__);
        abort();
    }
    ccontext_init(cc, ctx);

    // temporary workaround
    port_ensure_available(cc->ctx, BUFSIZE*2 + 5 + 4 + 3);

    term_ref pid = ccontext_make_term_ref(cc, recvfrom_data->pid);
    term_ref ref = ccontext_make_term_ref(cc, term_from_ref_ticks(recvfrom_data->ref_ticks, cc->ctx));

    ssize_t len = recvfrom(socket_data->sockfd, buf, BUFSIZE, 0, (struct sockaddr *) &clientaddr, &clientlen);
    if (len == -1) {
        const char *error_string = strerror(errno);
        port_send_reply(cc, pid, ref, port_create_error_tuple(cc, error_string));
    } else {
        term_ref addr = socket_tuple_from_addr(cc, htonl(clientaddr.sin_addr.s_addr));
        term_ref port = ccontext_make_term_ref(cc, term_from_int32(htons(clientaddr.sin_port)));
        term_ref packet = socket_create_packet_term(cc, buf, len);
        term_ref addr_port_packet = port_create_tuple3(cc, addr, port, packet);
        term_ref reply = port_create_ok_tuple(cc, addr_port_packet);
        port_send_reply(cc, pid, ref, reply);
    }

    ccontext_release_all_refs(cc);
    free(cc);

    linkedlist_remove(&ctx->global->listeners, &listener->listeners_list_head);
    free(listener);
    free(recvfrom_data);
    free(buf);
}

void socket_driver_do_recvfrom(CContext *cc, term_ref pid, term_ref ref)
{
    Context *ctx = cc->ctx;
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
    data->pid = ccontext_get_term(cc, pid);
    data->ref_ticks = term_to_ref_ticks(ccontext_get_term(cc, ref));

    linkedlist_append(&ctx->global->listeners, &listener->listeners_list_head);

    listener->fd = socket_data->sockfd;
    listener->expires = 0;
    listener->expiral_timestamp.tv_sec = 60*60*24; // TODO
    listener->expiral_timestamp.tv_nsec = 0;
    listener->one_shot = 1;
    listener->data = data;
    listener->handler = recvfrom_callback;
}
