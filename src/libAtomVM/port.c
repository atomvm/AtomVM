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
#include "ccontext.h"
#include "globalcontext.h"
#include "mailbox.h"

const char *const port_ok_a = "\x2" "ok";
const char *const port_error_a = "\x5" "error";


term_ref port_create_tuple2(CContext *cc, term_ref a, term_ref b)
{
    term_ref terms[2];
    terms[0] = a;
    terms[1] = b;

    return port_create_tuple_n(cc, 2, terms);
}

term_ref port_create_tuple3(CContext *cc, term_ref a, term_ref b, term_ref c)
{
    term_ref terms[3];
    terms[0] = a;
    terms[1] = b;
    terms[2] = c;

    return port_create_tuple_n(cc, 3, terms);
}

term_ref port_create_tuple_n(CContext *cc, size_t num_terms, term_ref *terms)
{
    term ret = term_alloc_tuple(num_terms, cc->ctx);

    for (size_t i = 0; i < num_terms;  ++i) {
        term_put_tuple_element(ret, i, ccontext_get_term(cc, terms[i]));
    }

    return ccontext_make_term_ref(cc, ret);
}

term_ref port_create_error_tuple(CContext *cc, const char *reason)
{
    int len = strnlen(reason, 424);
    term error_atom = context_make_atom(cc->ctx, port_error_a);
    term error_reason = term_from_string((const uint8_t*) reason, len, cc->ctx);
    return port_create_tuple2(cc,
        ccontext_make_term_ref(cc, error_atom),
        ccontext_make_term_ref(cc, error_reason)
    );
}

term_ref port_create_ok_tuple(CContext *cc, term_ref t)
{
    term_ref ok_atom = port_make_atom(cc, port_ok_a);
    return port_create_tuple2(cc, ok_atom, t);
}

void port_send_reply(CContext *cc, term_ref pid, term_ref ref, term_ref reply)
{
    int local_process_id = term_to_local_process_id(ccontext_get_term(cc, pid));
    Context *target = globalcontext_get_process(cc->ctx->global, local_process_id);
    term_ref msg = port_create_tuple2(cc, ref, reply);
    mailbox_send(target, ccontext_get_term(cc, msg));
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
