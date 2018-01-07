/***************************************************************************
 *   Copyright 2017 by Davide Bettio <davide@uninstall.it>                 *
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

#include "nifs.h"

#include "context.h"
#include "mailbox.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"

#include <stdio.h>
#include <string.h>

#define MAX_NIF_NAME_LEN 32

static char *list_to_string(term list);
static void process_echo_mailbox(Context *ctx);
static void process_console_mailbox(Context *ctx);

static const struct Nif open_port_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_open_port_2
};

static const struct Nif register_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_register_2
};

static const struct Nif whereis_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_whereis_1
};

const struct Nif *nifs_get(AtomString module, AtomString function, int arity)
{
    char nifname[MAX_NIF_NAME_LEN];

    int module_name_len = atom_string_len(module);
    memcpy(nifname, atom_string_data(module), module_name_len);

    nifname[module_name_len] = ':';

    int function_name_len = atom_string_len(function);
    memcpy(nifname + module_name_len + 1, atom_string_data(function), function_name_len);

    nifname[module_name_len + function_name_len + 1] = '\\';
    nifname[module_name_len + function_name_len + 2] = '0' + arity;
    nifname[module_name_len + function_name_len + 3] = 0;

    if (!strcmp("erlang:open_port\\2", nifname)) {
        return &open_port_nif;
    } else if (!strcmp("erlang:register\\2", nifname)) {
        return &register_nif;
    } else if (!strcmp("erlang:whereis\\1", nifname)) {
        return &whereis_nif;
    }

    return NULL;
}

term nif_erlang_open_port_2(Context *ctx, int argc, term argv[])
{
    if (argc != 2) {
        fprintf(stderr, "wrong arity\n");
        abort();
    }

    Context *new_ctx = context_new(ctx->global);

    term t = term_get_tuple_element(argv[0], 1);
    char *driver_name = list_to_string(t);

    if (!strcmp("echo", driver_name)) {
        new_ctx->native_handler = process_echo_mailbox;
    } else if (!strcmp("console", driver_name)) {
        new_ctx->native_handler = process_console_mailbox;
    }

    if (!new_ctx->native_handler) {
        new_ctx->native_handler = platform_open_port(driver_name);
    }

    free(driver_name);

    scheduler_make_waiting(ctx->global, new_ctx);

    return term_from_local_process_id(new_ctx->process_id);
}


term nif_erlang_register_2(Context *ctx, int argc, term argv[])
{
    if ((argc != 2) || !term_is_atom(argv[0]) || !term_is_pid(argv[1])) {
        fprintf(stderr, "bad match\n");
        abort();
    }

    int atom_index = term_to_atom_index(argv[0]);
    int pid = term_to_local_process_id(argv[1]);

    globalcontext_register_process(ctx->global, atom_index, pid);

    return term_nil();
}

term nif_erlang_whereis_1(Context *ctx, int argc, term argv[])
{
    if ((argc != 1) || !term_is_atom(argv[0])) {
        fprintf(stderr, "bad match\n");
        abort();
    }

    int atom_index = term_to_atom_index(argv[0]);

    int local_process_id = globalcontext_get_registered_process(ctx->global, atom_index);
    if (local_process_id) {
        return term_from_local_process_id(local_process_id);
    } else {
        return term_nil();
    }
}

static char *list_to_string(term list)
{
    int len = 0;

    term t = list;

    while (!term_is_nil(t)) {
        len++;
        term *t_ptr = term_get_list_ptr(t);
        t = *t_ptr;
    }

    t = list;
    char *str = malloc(len + 1);

    for (int i = 0; i < len; i++) {
        term *t_ptr = term_get_list_ptr(t);
        str[i] = (char) term_to_int32(t_ptr[1]);
        t = *t_ptr;
    }
    str[len] = 0;

    return str;
}

static void process_echo_mailbox(Context *ctx)
{
    term msg = mailbox_receive(ctx);
    term pid = term_get_tuple_element(msg, 0);
    term val = term_get_tuple_element(msg, 1);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, val);
}

static void process_console_mailbox(Context *ctx)
{
    term msg = mailbox_receive(ctx);
    term pid = term_get_tuple_element(msg, 0);
    term val = term_get_tuple_element(msg, 1);

    char *str = list_to_string(val);
    int len = strlen(str);
    printf("%s", str);
    free(str);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, term_from_int32(len));
}
