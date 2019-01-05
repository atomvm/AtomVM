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

#ifndef _PORT_H_
#define _PORT_H_

#include "globalcontext.h"
#include "ccontext.h"
#include "term.h"

extern const char *const port_ok_a;
extern const char *const port_error_a;

static inline term_ref port_make_atom(CContext *cc, AtomString atom)
{
    return ccontext_make_term_ref(cc, context_make_atom(cc->ctx, atom));
}

static inline term_ref port_make_ok_atom(CContext *cc)
{
    return ccontext_make_term_ref(cc, context_make_atom(cc->ctx, port_ok_a));
}

term_ref port_create_tuple2(CContext *cc, term_ref a, term_ref b);
term_ref port_create_tuple3(CContext *cc, term_ref a, term_ref b, term_ref c);
term_ref port_create_tuple_n(CContext *cc, size_t num_terms, term_ref *terms);
term_ref port_create_error_tuple(CContext *cc, const char *reason);
term_ref port_create_ok_tuple(CContext *cc, term_ref t);
void port_send_reply(CContext *cc, term_ref pid, term_ref ref, term_ref reply);
void port_ensure_available(Context *ctx, size_t size);
int port_is_standard_port_command(term msg);

#endif
