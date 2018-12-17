/***************************************************************************
 *   Copyright 2018 by Riccardo Binetti <rbino@gmx.com>                    *
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

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "globalcontext.h"
#include "mailbox.h"
#include "term.h"
#include "trace.h"

static void consume_network_mailbox(Context *ctx);
static term setup_network(Context *ctx, term config);

static const char *const error_a = "\x5" "error";

void networkdriver_init(Context *ctx)
{
    ctx->native_handler = consume_network_mailbox;
    ctx->platform_data = NULL;
}

static void consume_network_mailbox(Context *ctx)
{
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;
    term pid = term_get_tuple_element(msg, 0);
    free(message);

	// TODO: unimplemented, we just return error for now
    TRACE("network: unrecognized command\n");
    term ret = context_make_atom(ctx, error_a);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, ret);
}
