/***************************************************************************
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
#include "mailbox.h"
#include "term.h"

#include "trace.h"
#include "sys.h"

const char *const send_a = "\x4" "send";
const char *const init_a = "\x4" "init";
const char *const bind_a = "\x4" "bind";
const char *const recvfrom_a = "\x8" "recvfrom";


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

term socket_create_packet_term(Context *ctx, const char *buf, ssize_t len)
{
    return term_from_literal_binary((void *)buf, len, ctx);
}

static void socket_consume_mailbox(Context *ctx)
{
    TRACE("START socket_consume_mailbox\n");
    if (UNLIKELY(ctx->native_handler != socket_consume_mailbox)) {
        abort();
    }

    port_ensure_available(ctx, 16);

    Message *message = mailbox_dequeue(ctx);
    term     msg = message->message;
    term     pid = term_get_tuple_element(msg, 0);
    term     ref = term_get_tuple_element(msg, 1);
    term     cmd = term_get_tuple_element(msg, 2);

    term cmd_name = term_get_tuple_element(cmd, 0);
    if (cmd_name == context_make_atom(ctx, init_a)) {
        term params = term_get_tuple_element(cmd, 1);
        term reply = socket_driver_do_init(ctx, params);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, bind_a)) {
        term address = term_get_tuple_element(cmd, 1);
        term port = term_get_tuple_element(cmd, 2);
        term reply = socket_driver_do_bind(ctx, address, port);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, send_a)) {
        term dest_address = term_get_tuple_element(cmd, 1);
        term dest_port = term_get_tuple_element(cmd, 2);
        term buffer = term_get_tuple_element(cmd, 3);
        term reply = socket_driver_do_send(ctx, dest_address, dest_port, buffer);
        port_send_reply(ctx, pid, ref, reply);
    } else if (cmd_name == context_make_atom(ctx, recvfrom_a)) {
        socket_driver_do_recvfrom(ctx, pid, ref);
    } else {
        port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, "unrecognized command"));
    }

    free(message);
    TRACE("END socket_consume_mailbox\n");
}

void socket_init(Context *ctx, term opts)
{
    UNUSED(opts);
    void *data = socket_driver_create_data();
    ctx->native_handler = socket_consume_mailbox;
    ctx->platform_data = data;
}
