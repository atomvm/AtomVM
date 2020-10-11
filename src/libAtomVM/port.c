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

#include "port.h"
#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "mailbox.h"

term port_create_tuple2(Context *ctx, term a, term b)
{
    term terms[2];
    terms[0] = a;
    terms[1] = b;

    return port_create_tuple_n(ctx, 2, terms);
}

term port_create_tuple3(Context *ctx, term a, term b, term c)
{
    term terms[3];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;

    return port_create_tuple_n(ctx, 3, terms);
}

term port_create_tuple_n(Context *ctx, size_t num_terms, term *terms)
{
    term ret = term_alloc_tuple(num_terms, ctx);

    for (size_t i = 0; i < num_terms; ++i) {
        term_put_tuple_element(ret, i, terms[i]);
    }

    return ret;
}

term port_create_error_tuple(Context *ctx, term reason)
{
    return port_create_tuple2(ctx, ERROR_ATOM, reason);
}

term port_create_sys_error_tuple(Context *ctx, term syscall, int errno)
{
    term reason = port_create_tuple2(ctx, syscall, term_from_int32(errno));
    return port_create_error_tuple(ctx, reason);
}

term port_create_ok_tuple(Context *ctx, term t)
{
    return port_create_tuple2(ctx, OK_ATOM, t);
}

void port_send_reply(Context *ctx, term pid, term ref, term reply)
{
    term msg = port_create_tuple2(ctx, ref, reply);
    port_send_message(ctx, pid, msg);
}

void port_send_message(Context *ctx, term pid, term msg)
{
    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, msg);
}

void port_ensure_available(Context *ctx, size_t size)
{
    if (context_avail_free_memory(ctx) < size) {
        switch (memory_ensure_free(ctx, size)) {
            case MEMORY_GC_OK:
                break;
            case MEMORY_GC_ERROR_FAILED_ALLOCATION:
                // TODO Improve error handling
                fprintf(stderr, "Failed to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
                abort();
            case MEMORY_GC_DENIED_ALLOCATION:
                // TODO Improve error handling
                fprintf(stderr, "Not permitted to allocate additional heap storage: [%s:%i]\n", __FILE__, __LINE__);
                abort();
        }
    }
}

int port_is_standard_port_command(term t)
{
    if (!term_is_tuple(t)) {
        return 0;
    } else if (term_get_tuple_arity(t) != 3) {
        return 0;
    } else {
        term pid = term_get_tuple_element(t, 0);
        term ref = term_get_tuple_element(t, 1);
        if (!term_is_pid(pid)) {
            return 0;
        } else if (!term_is_reference(ref)) {
            return 0;
        } else {
            return 1;
        }
    }
}
