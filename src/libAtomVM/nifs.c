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

#include "atomshashtable.h"
#include "context.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"
#include "sys.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MAX_NIF_NAME_LEN 260

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = context_make_atom(ctx, error_atom); \
        argv[1] = context_make_atom(ctx, badarg_atom); \
        return term_invalid_term(); \
    } \

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = context_make_atom(ctx, error_atom); \
    ctx->x[1] = context_make_atom(ctx, (error_type_atom)); \
    return term_invalid_term();

static const char *const latin1_atom = "\x6" "latin1";
static const char *const ok_atom = "\x2" "ok";
static const char *const error_atom = "\x5" "error";
static const char *const undefined_atom = "\x9" "undefined";
static const char *const true_atom = "\x4" "true";
static const char *const false_atom = "\x5" "false";
static const char *const badarg_atom = "\x6" "badarg";

static void process_echo_mailbox(Context *ctx);
static void process_console_mailbox(Context *ctx);

static void display_term(term t, Context *ctx);
static term binary_to_atom(Context *ctx, int argc, term argv[], int create_new);
static term list_to_atom(Context *ctx, int argc, term argv[], int create_new);

static term nif_erlang_delete_element_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_atom_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_atom_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_existing_atom_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_concat_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_display_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_ref_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_insert_element_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_integer_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_existing_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_register_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_send_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_setelement_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_spawn_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_time_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_tuple_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[]);
static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[]);

static const struct Nif make_ref_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_make_ref_0
};

static const struct Nif atom_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_atom_to_list_1
};

static const struct Nif binary_to_atom_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_atom_2
};

static const struct Nif binary_to_existing_atom_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_existing_atom_2
};

static const struct Nif delete_element_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_delete_element_2
};

static const struct Nif display_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_display_1
};

static const struct Nif insert_element_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_insert_element_3
};

static const struct Nif integer_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_integer_to_list_1
};

static const struct Nif is_process_alive_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_is_process_alive_1
};

static const struct Nif list_to_atom_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_atom_1
};

static const struct Nif list_to_existing_atom_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_existing_atom_1
};

static const struct Nif list_to_integer_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_integer_1
};

static const struct Nif open_port_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_open_port_2
};

static const struct Nif make_tuple_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_make_tuple_2
};

static const struct Nif register_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_register_2
};

static const struct Nif spawn_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn_3
};

static const struct Nif send_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_send_2
};

static const struct Nif setelement_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_setelement_3
};

static const struct Nif whereis_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_whereis_1
};

static const struct Nif concat_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_concat_2
};

static const struct Nif system_time_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_system_time_1
};

static const struct Nif universaltime_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_universaltime_0
};

static const struct Nif tuple_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_tuple_to_list_1
};

static const struct Nif flat_size_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erts_debug_flat_size
};

//Ignore warning caused by gperf generated code
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#include "nifs_hash.h"
#pragma GCC diagnostic pop


const struct Nif *nifs_get(AtomString module, AtomString function, int arity)
{
    char nifname[MAX_NIF_NAME_LEN];

    int module_name_len = atom_string_len(module);
    memcpy(nifname, atom_string_data(module), module_name_len);

    nifname[module_name_len] = ':';

    int function_name_len = atom_string_len(function);
    if (UNLIKELY((arity > 9) || (module_name_len + function_name_len + 4 > MAX_NIF_NAME_LEN))) {
        abort();
    }
    memcpy(nifname + module_name_len + 1, atom_string_data(function), function_name_len);

    //TODO: handle NIFs with more than 9 parameters
    nifname[module_name_len + function_name_len + 1] = '/';
    nifname[module_name_len + function_name_len + 2] = '0' + arity;
    nifname[module_name_len + function_name_len + 3] = 0;

    const NifNameAndNifPtr *nameAndPtr = nif_in_word_set(nifname, strlen(nifname));
    if (!nameAndPtr) {
        return NULL;
    }

    return nameAndPtr->nif;
}

static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY(argc != 2)) {
        fprintf(stderr, "wrong arity\n");
        abort();
    }

    term port_name = argv[0];
    term opts = argv[1];

    if (!(term_is_tuple(port_name) && term_get_tuple_arity(port_name) == 2) && !term_is_nonempty_list(opts)) {
        fprintf(stderr, "bad args\n");
        abort();
    }

    term t = term_get_tuple_element(port_name, 1);
    char *driver_name = interop_term_to_string(t);
    if (IS_NULL_PTR(driver_name)) {
        int error_index = globalcontext_insert_atom(ctx->global, error_atom);
        if (error_index < 0) {
            abort();
        }
        return term_from_atom_index(error_index);
    }

    Context *new_ctx = NULL;

    if (!strcmp("echo", driver_name)) {
        new_ctx = context_new(ctx->global);
        new_ctx->native_handler = process_echo_mailbox;

    } else if (!strcmp("console", driver_name)) {
        new_ctx = context_new(ctx->global);
        new_ctx->native_handler = process_console_mailbox;
    }

    if (!new_ctx) {
        new_ctx = platform_open_port(ctx->global, driver_name, opts);
    }

    free(driver_name);

    scheduler_make_waiting(ctx->global, new_ctx);

    return term_from_local_process_id(new_ctx->process_id);
}


static term nif_erlang_register_2(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY((argc != 2) || !term_is_atom(argv[0]) || !term_is_pid(argv[1]))) {
        fprintf(stderr, "bad match\n");
        abort();
    }

    int atom_index = term_to_atom_index(argv[0]);
    int pid = term_to_local_process_id(argv[1]);

    globalcontext_register_process(ctx->global, atom_index, pid);

    return term_nil();
}

static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY((argc != 1) || !term_is_atom(argv[0]))) {
        fprintf(stderr, "bad match\n");
        abort();
    }

    int atom_index = term_to_atom_index(argv[0]);

    int local_process_id = globalcontext_get_registered_process(ctx->global, atom_index);
    if (local_process_id) {
        return term_from_local_process_id(local_process_id);
    } else {
        int undefined_index = globalcontext_insert_atom(ctx->global, undefined_atom);
        return term_from_atom_index(undefined_index);
    }
}

static void process_echo_mailbox(Context *ctx)
{
    Message *msg = mailbox_dequeue(ctx);
    term pid = term_get_tuple_element(msg->message, 0);
    term val = term_get_tuple_element(msg->message, 1);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    mailbox_send(target, val);

    free(msg);
}

static void process_console_mailbox(Context *ctx)
{

    Message *msg = mailbox_dequeue(ctx);
    term pid = term_get_tuple_element(msg->message, 0);
    term val = term_get_tuple_element(msg->message, 1);

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    char *str = interop_term_to_string(val);
    if (IS_NULL_PTR(str)) {
        free(msg);
        return;
    }

    printf("%s", str);
    free(str);

    //TODO: use globalcontext_get_atom_id(ctx->global, ok_atom);
    int ok_index = globalcontext_insert_atom(ctx->global, ok_atom);
    mailbox_send(target, term_from_atom_index(ok_index));

    free(msg);
}

static term nif_erlang_spawn_3(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY(argc != 3)) {
        fprintf(stderr, "spawn: wrong args count\n");
        abort();
    }

    if (UNLIKELY(!term_is_atom(argv[0]) || !term_is_atom(argv[1]) || !term_is_list(argv[2]))) {
        fprintf(stderr, "spawn: invalid arguments\n");
        abort();
    }

    Context *new_ctx = context_new(ctx->global);

    int mod_atom_index = term_to_atom_index(argv[0]);
    AtomString module_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, mod_atom_index, (unsigned long) NULL);
    int func_atom_index = term_to_atom_index(argv[1]);
    AtomString function_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, func_atom_index, (unsigned long) NULL);

    Module *found_module = globalcontext_get_module(ctx->global, module_string);
    if (UNLIKELY(!found_module)) {
        int undefined_index = globalcontext_insert_atom(ctx->global, undefined_atom);
        if (undefined_index < 0) {
            abort();
        }
        return term_from_atom_index(undefined_index);
    }

    int label = module_search_exported_function(found_module, function_string, term_list_length(argv[2]));
    //TODO: fail here if no function has been found
    new_ctx->saved_module = found_module;
    new_ctx->saved_ip = found_module->labels[label];
    new_ctx->cp = module_address(found_module->module_index, found_module->end_instruction_ii);

    //TODO: check available registers count
    int reg_index = 0;
    term t = argv[2];
    while (!term_is_nil(t)) {
        term *t_ptr = term_get_list_ptr(t);
        new_ctx->x[reg_index] = memory_copy_term_tree(&new_ctx->heap_ptr, &new_ctx->e, t_ptr[1], 0);
        t = *t_ptr;
        reg_index++;
    }

    return term_from_local_process_id(new_ctx->process_id);
}
static term nif_erlang_send_2(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY(argc != 2)) {
        fprintf(stderr, "send: wrong args count\n");
        abort();
    }

    if (UNLIKELY(!term_is_pid(argv[0]))) {
        fprintf(stderr, "send: invalid arguments\n");
        abort();
    }

    int local_process_id = term_to_local_process_id(argv[0]);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    mailbox_send(target, argv[1]);

    return argv[1];
}

static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY(argc != 1)) {
        fprintf(stderr, "is_process_alive: wrong args count\n");
        abort();
    }

    int local_process_id = term_to_local_process_id(argv[0]);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);
    if (target) {
        int true_i = globalcontext_insert_atom(ctx->global, true_atom);
        return term_from_atom_index(true_i);
    } else {
        int false_i = globalcontext_insert_atom(ctx->global, false_atom);
        return term_from_atom_index(false_i);
    }
}

static term nif_erlang_concat_2(Context *ctx, int argc, term argv[])
{
    if (UNLIKELY(argc != 2)) {
        fprintf(stderr, "++: wrong args count\n");
        abort();
    }

    term prepend_list = argv[0];

    if (UNLIKELY(!term_is_nonempty_list(prepend_list))) {
        if (term_is_nil(prepend_list)) {
            return argv[1];

        } else {
            fprintf(stderr, "Argument error\n");
            abort();
        }
    }

    int len = term_list_length(prepend_list);
    memory_ensure_free(ctx, len * 2);

    // GC might have changed all pointers
    prepend_list = argv[0];
    term append_list = argv[1];

    term t = prepend_list;
    term list_begin = term_nil();
    term *prev_term = NULL;

    // TODO: handle impropers list
    while (!term_is_nil(t)) {
        term head = term_get_list_head(t);

        term *new_list_item = term_list_alloc(ctx);

        if (prev_term) {
            prev_term[0] = term_list_from_list_ptr(new_list_item);
        } else {
            list_begin = term_list_from_list_ptr(new_list_item);
        }

        prev_term = new_list_item;
        new_list_item[1] = head;

        t = term_get_list_tail(t);
    }

    if (prev_term) {
        prev_term[0] = append_list;
    }

    return list_begin;
}

term nif_erlang_make_ref_0(Context *ctx, int argc, term argv[])
{
    UNUSED(argv);

    if (UNLIKELY(argc != 0)) {
        fprintf(stderr, "erlang:make_ref: wrong args count\n");
        abort();
    }

    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);

    return term_from_ref_ticks(ref_ticks, ctx);
}

term nif_erlang_system_time_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);

    if (argc != 1) {
        fprintf(stderr, "erlang:system_time: wrong args count\n");
        abort();
    }

    struct timespec ts;
    sys_time(&ts);

    term minute_atom = context_make_atom(ctx, "\x6" "minute");
    if (argv[0] == minute_atom) {
        // FIXME: This is not standard, however we cannot hold seconds since 1970 in just 27 bits.
        return term_from_int32(ts.tv_sec / 60);

    } else {
        fprintf(stderr, "nif_erlang_system_time: error, got: %lx\n", argv[0]);
        abort();
    }
}

term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argv);

    if (argc != 0) {
        fprintf(stderr, "system_time_to_universal_time(): wrong args count\n");
        abort();
    }

    // 4 = size of date/time tuple, 3 size of date time tuple
    memory_ensure_free(ctx, 3 + 4 + 4);
    term date_tuple = term_alloc_tuple(3, ctx);
    term time_tuple = term_alloc_tuple(3, ctx);
    term date_time_tuple = term_alloc_tuple(2, ctx);

    struct timespec ts;
    sys_time(&ts);

    struct tm broken_down_time;
    gmtime_r(&ts.tv_sec, &broken_down_time);

    term_put_tuple_element(date_tuple, 0, term_from_int32(1900 + broken_down_time.tm_year));
    term_put_tuple_element(date_tuple, 1, term_from_int32(broken_down_time.tm_mon + 1));
    term_put_tuple_element(date_tuple, 2, term_from_int32(broken_down_time.tm_mday));

    term_put_tuple_element(time_tuple, 0, term_from_int32(broken_down_time.tm_hour));
    term_put_tuple_element(time_tuple, 1, term_from_int32(broken_down_time.tm_min));
    term_put_tuple_element(time_tuple, 2, term_from_int32(broken_down_time.tm_sec));

    term_put_tuple_element(date_time_tuple, 0, date_tuple);
    term_put_tuple_element(date_time_tuple, 1, time_tuple);

    return date_time_tuple;
}

static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[])
{
    if (argc != 2) {
        fprintf(stderr, "make_tuple: wrong args count\n");
        abort();
    }

    int count_elem = term_to_int32(argv[0]);

    if (UNLIKELY(count_elem < 0)) {
        fprintf(stderr, "make_tuple: bad argument: %i\n", count_elem);
        abort();
    }

    memory_ensure_free(ctx, count_elem + 1);
    term new_tuple = term_alloc_tuple(count_elem, ctx);

    term element = argv[1];

    for (int i = 0; i < count_elem; i++) {
        term_put_tuple_element(new_tuple, i, element);
    }

    return new_tuple;
}

static term nif_erlang_insert_element_3(Context *ctx, int argc, term argv[])
{
    if (argc != 3) {
        fprintf(stderr, "insert_element: wrong args count\n");
        abort();
    }

    // indexes are 1 based
    int insert_index = term_to_int32(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((insert_index > old_tuple_size) || (insert_index < 0))) {
        fprintf(stderr, "insert_element: bad argument: %i\n", insert_index);
        abort();
    }

    int new_tuple_size = old_tuple_size + 1;
    memory_ensure_free(ctx, new_tuple_size + 1);
    term new_tuple = term_alloc_tuple(new_tuple_size, ctx);

    term old_tuple = argv[1];
    term new_element = argv[2];

    int src_elements_shift = 0;
    for (int i = 0; i < new_tuple_size; i++) {
        if (i == insert_index) {
            src_elements_shift = 1;
            term_put_tuple_element(new_tuple, i, new_element);
        } else {
            term_put_tuple_element(new_tuple, i, term_get_tuple_element(old_tuple, i - src_elements_shift));
        }
    }

    return new_tuple;
}


static term nif_erlang_delete_element_2(Context *ctx, int argc, term argv[])
{
    if (argc != 2) {
        fprintf(stderr, "delete_element: wrong args count\n");
        abort();
    }

    // indexes are 1 based
    int delete_index = term_to_int32(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((delete_index > old_tuple_size) || (delete_index < 0))) {
        fprintf(stderr, "insert_element: bad argument: %i\n", delete_index);
        abort();
    }

    int new_tuple_size = old_tuple_size - 1;
    memory_ensure_free(ctx, new_tuple_size + 1);
    term new_tuple = term_alloc_tuple(new_tuple_size, ctx);

    term old_tuple = argv[1];

    int src_elements_shift = 0;
    for (int i = 0; i < new_tuple_size; i++) {
        if (i == delete_index) {
            src_elements_shift = 1;
        }
        term_put_tuple_element(new_tuple, i, term_get_tuple_element(old_tuple, i + src_elements_shift));
    }

    return new_tuple;
}

static term nif_erlang_setelement_3(Context *ctx, int argc, term argv[])
{
    if (argc != 3) {
        fprintf(stderr, "setelement: wrong args count\n");
        abort();
    }

    // indexes are 1 based
    int replace_index = term_to_int32(argv[0]) - 1;

    int tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((replace_index >= tuple_size) || (replace_index < 0))) {
        fprintf(stderr, "setelement: bad argument: %i\n", replace_index);
        abort();
    }

    memory_ensure_free(ctx, tuple_size + 1);
    term new_tuple = term_alloc_tuple(tuple_size, ctx);

    term old_tuple = argv[1];
    for (int i = 0; i < tuple_size; i++) {
        term_put_tuple_element(new_tuple, i, term_get_tuple_element(old_tuple, i));
    }

    term value = argv[2];
    term_put_tuple_element(new_tuple, replace_index, value);

    return new_tuple;
}

static term nif_erlang_tuple_to_list_1(Context *ctx, int argc, term argv[])
{
    if (argc != 1) {
        fprintf(stderr, "tuple_to_list: wrong args count\n");
        abort();
    }

    int tuple_size = term_get_tuple_arity(argv[0]);

    memory_ensure_free(ctx, tuple_size * 2);

    term tuple = argv[0];
    term prev = term_nil();

    for (int i = tuple_size - 1; i >= 0; i--) {
        prev = term_list_prepend(term_get_tuple_element(tuple, i), prev, ctx);
    }

    return prev;
}

static term nif_erlang_binary_to_atom_2(Context *ctx, int argc, term argv[])
{
    return binary_to_atom(ctx, argc, argv, 1);
}

static term nif_erlang_binary_to_existing_atom_2(Context *ctx, int argc, term argv[])
{
    return binary_to_atom(ctx, argc, argv, 0);
}

static term binary_to_atom(Context *ctx, int argc, term argv[], int create_new)
{
    if (argc != 2) {
        fprintf(stderr, "binary_to_atom: wrong args count\n");
        abort();
    }
    term a_binary = argv[0];
    VALIDATE_VALUE(a_binary, term_is_binary);

    if (UNLIKELY(argv[1] != context_make_atom(ctx, latin1_atom))) {
        fprintf(stderr, "binary_to_atom: only latin1 is supported.\n");
        abort();
    }

    char *atom_string = interop_binary_to_string(a_binary);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        fprintf(stderr, "Too long atom.\n");
        free(atom_string);
        abort();
    }

    AtomString atom = malloc(atom_string_len + 1);
    ((uint8_t *) atom)[0] = atom_string_len;
    memcpy(((char *) atom) + 1, atom_string, atom_string_len);

    unsigned long global_atom_index = atomshashtable_get_value(ctx->global->atoms_table, atom, ULONG_MAX);
    int has_atom = (global_atom_index != ULONG_MAX);

    if (create_new || has_atom) {
        if (!has_atom) {
            global_atom_index = globalcontext_insert_atom(ctx->global, atom);
        } else {
            free((void *) atom);
        }
        return term_from_atom_index(global_atom_index);

    } else {
        free((void *) atom);
        RAISE_ERROR(badarg_atom);
    }
}

term nif_erlang_list_to_atom_1(Context *ctx, int argc, term argv[])
{
    return list_to_atom(ctx, argc, argv, 1);
}

term nif_erlang_list_to_existing_atom_1(Context *ctx, int argc, term argv[])
{
    return list_to_atom(ctx, argc, argv, 0);
}

term list_to_atom(Context *ctx, int argc, term argv[], int create_new)
{
    if (argc != 1) {
        fprintf(stderr, "list_to_existing_atom: wrong args count\n");
        abort();
    }
    term a_list = argv[0];
    VALIDATE_VALUE(a_list, term_is_list);

    char *atom_string = interop_list_to_string(a_list);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        fprintf(stderr, "Too long atom.\n");
        free(atom_string);
        abort();
    }

    AtomString atom = malloc(atom_string_len + 1);
    ((uint8_t *) atom)[0] = atom_string_len;
    memcpy(((char *) atom) + 1, atom_string, atom_string_len);

    unsigned long global_atom_index = atomshashtable_get_value(ctx->global->atoms_table, atom, ULONG_MAX);
    int has_atom = (global_atom_index != ULONG_MAX);

    if (create_new || has_atom) {
        if (!has_atom) {
            global_atom_index = globalcontext_insert_atom(ctx->global, atom);
        } else {
            free((void *) atom);
        }
        return term_from_atom_index(global_atom_index);

    } else {
        free((void *) atom);
        RAISE_ERROR(badarg_atom);
    }
}

static term nif_erlang_atom_to_list_1(Context *ctx, int argc, term argv[])
{
    if (argc != 1) {
        fprintf(stderr, "list_to_atom: wrong args count\n");
        abort();
    }

    term atom_term = argv[0];
    VALIDATE_VALUE(atom_term, term_is_atom);

    int atom_index = term_to_atom_index(atom_term);
    AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);

    int atom_len = atom_string_len(atom_string);

    memory_ensure_free(ctx, atom_len * 2);

    term prev = term_nil();
    for (int i = atom_len - 1; i >= 0; i--) {
        char c = ((const char *) atom_string_data(atom_string))[i];
        prev = term_list_prepend(term_from_int11(c), prev, ctx);
    }

    return prev;
}

static term nif_erlang_integer_to_list_1(Context *ctx, int argc, term argv[])
{
    if (argc != 1) {
        fprintf(stderr, "integer_to_list: wrong args count\n");
        abort();
    }

    int32_t int_value = term_to_int32(argv[0]);
    char integer_string[24];

    snprintf(integer_string, 24, "%i", int_value);
    int integer_string_len = strlen(integer_string);

    memory_ensure_free(ctx, integer_string_len * 2);

    term prev = term_nil();
    for (int i = integer_string_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(integer_string[i]), prev, ctx);
    }

    return prev;
}

static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);

    if (argc != 1) {
        fprintf(stderr, "list_to_integer: wrong args count\n");
        abort();
    }

    term t = argv[0];
    int32_t acc = 0;
    int digits = 0;

    int negative = 0;
    term first_digit = term_get_list_head(t);
    if (first_digit == term_from_int11('-')) {
        negative = 1;
        t = term_get_list_tail(t);
    } else if (first_digit == term_from_int11('+')) {
        t = term_get_list_tail(t);
    }

    while (!term_is_nil(t)) {
        term head = term_get_list_head(t);

        if (UNLIKELY(!term_is_integer(head))) {
            fprintf(stderr, "list_to_integer: bad argument.");
            abort();
        }

        int32_t c = term_to_int32(head);

        if (UNLIKELY((c < '0') || (c > '9'))) {
            fprintf(stderr, "list_to_integer: bad argument.");
            abort();
        }

        if (acc > INT32_MAX / 10) {
            fprintf(stderr, "list_to_integer: overflow.");
            abort();
        }

        acc = (acc * 10) + (c - '0');
        digits++;
        t = term_get_list_tail(t);
    }

    if (negative) {
        acc = -acc;
    }

    if (UNLIKELY(digits == 0)) {
        fprintf(stderr, "list_to_integer: bad argument.");
        abort();
    }

    return term_from_int32(acc);
}

static term nif_erlang_display_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);

    if (argc != 1) {
        fprintf(stderr, "display: wrong args count\n");
        abort();
    }

    display_term(argv[0], ctx);
    printf("\n");

    return term_nil();
}

static void display_term(term t, Context *ctx)
{
    if (term_is_atom(t)) {
        int atom_index = term_to_atom_index(t);
        AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);
        printf("%.*s", (int) atom_string_len(atom_string), (char *) atom_string_data(atom_string));

    } else if (term_is_integer(t)) {
        long iv = term_to_int32(t);
        printf("%li", iv);

    } else if (term_is_nil(t)) {
        printf("[]");

    } else if (term_is_nonempty_list(t)) {
        int is_printable = 1;
        term list_item = t;
        while (!term_is_nil(list_item)) {
            term head = term_get_list_head(list_item);
            if (!term_is_integer(head) || !isprint(term_to_int32(head))) {
                is_printable = 0;
            }
            list_item = term_get_list_tail(list_item);
        }

        if (is_printable) {
            char *printable = interop_list_to_string(t);
            printf("\"%s\"", printable);
            free(printable);

        } else {
            putchar('[');
            int display_separator = 0;
            while (!term_is_nil(t)) {
                if (display_separator) {
                    putchar(',');
                } else {
                    display_separator = 1;
                }

                display_term(term_get_list_head(t), ctx);
                t = term_get_list_tail(t);
            }
            putchar(']');
        }
    } else if (term_is_pid(t)) {
        printf("<0.%i.0>", term_to_local_process_id(t));

    } else if (term_is_tuple(t)) {
        putchar('{');

        int tuple_size = term_get_tuple_arity(t);
        for (int i = 0; i < tuple_size; i++) {
            if (i != 0) {
                putchar(',');
            }
            display_term(term_get_tuple_element(t, i), ctx);
        }

        putchar('}');

    } else if (term_is_binary(t)) {
        int len = term_binary_size(t);
        const char *binary_data = term_binary_data(t);

        int is_printable = 1;
        for (int i = 0; i < len; i++) {
            if (!isprint(binary_data[i])) {
                is_printable = 0;
            }
        }

        if (is_printable) {
            printf("<<\"%.*s\">>", len, binary_data);

        } else {
            int display_separator = 0;
            for (int i = 0; i < len; i++) {
                if (display_separator) {
                    putchar(',');
                } else {
                    display_separator = 1;
                }

                printf("%i", binary_data[i]);
            }
        }

    } else if (term_is_reference(t)) {
        const char *format =
#ifdef __clang__
        "#Ref<0.0.0.%llu>";
#else
        "#Ref<0.0.0.%lu>";
#endif
        printf(format, term_to_ref_ticks(t));
    }
}

static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);

    if (UNLIKELY(argc != 1)) {
        fprintf(stderr, "erts_debug:flat_size: wrong args count\n");
        abort();
    }

    unsigned long terms_count;

    terms_count = memory_estimate_usage(argv[0]);

    return term_from_int32(terms_count);
}
