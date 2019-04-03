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

#include "context.h"
#include "port.h"
#include "network.h"
#include "network_driver.h"

#include "context.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "utils.h"
#include "term.h"

static const char *const setup_a = "\x5" "setup";
static const char *const ifconfig_a = "\x8" "ifconfig";

static void network_consume_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    if (port_is_standard_port_command(msg)) {
        port_ensure_available(ctx, 32);

        term pid = term_get_tuple_element(msg, 0);
        term ref = term_get_tuple_element(msg, 1);
        term cmd = term_get_tuple_element(msg, 2);

        if (term_is_atom(cmd) && cmd == context_make_atom(ctx, ifconfig_a)) {
            term reply = network_driver_ifconfig(ctx);
            port_send_reply(ctx, pid, ref, reply);
        } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
            term cmd_name = term_get_tuple_element(cmd, 0);
            term config = term_get_tuple_element(cmd, 1);
            if (cmd_name == context_make_atom(ctx, setup_a)) {
                network_driver_setup(ctx, pid, ref, config);
            } else {
                port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, "unrecognized tuple command"));
            }
        } else {
            port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, "unrecognized command"));
        }
    } else {
        fprintf(stderr, "WARNING: Invalid port command.  Unable to send reply");
    }

    free(message);
}


void network_init(Context *ctx, term opts)
{
    UNUSED(opts);
    ctx->native_handler = network_consume_mailbox;
    ctx->platform_data = NULL;
}

