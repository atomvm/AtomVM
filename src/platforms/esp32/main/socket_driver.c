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

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <errno.h>
#include <unistd.h>

#include <esp_log.h>
#include <lwip/inet.h>

#include "trace.h"
#include "sys.h"

static const char *const tag_proto_a = "\x5" "proto";
static const char *const proto_udp_a = "\x3" "udp";
static const char *const proto_tcp_a = "\x3" "tcp";

// TODO use net_conn instead of BSD Sockets

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
    return port_create_error_tuple(cc, "unimplemented");
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

void socket_driver_do_recvfrom(CContext *cc, term_ref pid, term_ref ref)
{
    port_send_reply(
        cc, pid, ref,
        port_create_error_tuple(cc, "unimplemented")
    );
}
