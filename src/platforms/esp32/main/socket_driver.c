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
static const char *const socket_a    = "\x6" "socket";
static const char *const fcntl_a     = "\x5" "fcntl";

// 3 unused variables -> won't compile using esp-idf v 3.2
// static const char *const bind_a      = "\x4" "bind";
// static const char *const getsockname_a = "\xB" "getsockname";
// static const char *const recvfrom_a    = "\x8" "recvfrom";
static const char *const sendto_a      = "\x6" "sendto";

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


term socket_driver_do_init(Context *ctx, term params)
{
    SocketDriverData *socket_data = (SocketDriverData *) ctx->platform_data;

    if (!term_is_list(params)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    term proto = interop_proplist_get_value(params, context_make_atom(ctx, tag_proto_a));

    if (term_is_nil(proto)) {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    if (proto == context_make_atom(ctx, proto_udp_a)) {
        int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
        if (sockfd == -1) {
            return port_create_sys_error_tuple(ctx, socket_a, errno);
        }
        socket_data->sockfd = sockfd;
    } else if (proto == context_make_atom(ctx, proto_tcp_a)) {
        socket_data->sockfd = socket(AF_INET, SOCK_STREAM, 0);
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }
    if (fcntl(socket_data->sockfd, F_SETFL, O_NONBLOCK) == -1){
        return port_create_sys_error_tuple(ctx, fcntl_a, errno);
    }

    return OK_ATOM;
}


term socket_driver_do_bind(Context *ctx, term address, term port)
{
    return port_create_error_tuple(ctx, UNDEFINED_ATOM);
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
        buf = interop_list_to_string(buffer);
        len = strlen(buf);
    } else {
        return port_create_error_tuple(ctx, BADARG_ATOM);
    }

    TRACE("send: data with len: %i, to: %i, port: %i\n", len, ntohl(addr.sin_addr.s_addr), ntohs(addr.sin_port));

    int sent_data = sendto(socket_data->sockfd, buf, len, 0, (struct sockaddr *) &addr, sizeof(addr));
    if (sent_data == -1) {
        return port_create_sys_error_tuple(ctx, sendto_a, errno);
    } else {
        term sent_atom = term_from_int32(sent_data);
        return port_create_ok_tuple(ctx, sent_atom);
    }
}

void socket_driver_do_recvfrom(Context *ctx, term pid, term ref)
{
    port_send_reply(
        ctx, pid, ref,
        port_create_error_tuple(ctx, UNDEFINED_ATOM)
    );
}
