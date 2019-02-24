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
#include "ccontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "port.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"
#include "sys.h"

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
static const char *const error_atom = "\x5" "error";
static const char *const undefined_atom = "\x9" "undefined";
static const char *const true_atom = "\x4" "true";
static const char *const false_atom = "\x5" "false";
static const char *const badarg_atom = "\x6" "badarg";
static const char *const overflow_atom = "\x8" "overflow";
static const char *const system_limit_atom = "\xC" "system_limit";
static const char *const puts_a = "\x4" "puts";
static const char *const flush_a = "\x5" "flush";
static const char *const max_heap_size_a ="\xD" "max_heap_size";

static const char *const out_of_memory_atom = "\xD" "out_of_memory";

#ifdef ENABLE_ADVANCED_TRACE
static const char *const ok_atom = "\x2" "ok";

static const char *const trace_calls_atom = "\xB" "trace_calls";
static const char *const trace_call_args_atom = "\xF" "trace_call_args";
static const char *const trace_returns_atom = "\xD" "trace_returns";
static const char *const trace_send_atom = "\xA" "trace_send";
static const char *const trace_receive_atom = "\xD" "trace_receive";
#endif


static void process_echo_mailbox(Context *ctx);
static void process_console_mailbox(Context *ctx);

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
static term nif_erlang_spawn(Context *ctx, int argc, term argv[]);
static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_time_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_tuple_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_timestamp_0(Context *ctx, int argc, term argv[]);
static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[]);
static term nifs_erlang_process_flag(Context *ctx, int argc, term argv[]);

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
    .nif_ptr = nif_erlang_spawn
};

static const struct Nif spawn_opt_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn
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

static const struct Nif timestamp_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_timestamp_0
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

static const struct Nif process_flag_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nifs_erlang_process_flag
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
    UNUSED(argc);

    term port_name_tuple = argv[0];
    VALIDATE_VALUE(port_name_tuple, term_is_tuple);
    term opts = argv[1];
    VALIDATE_VALUE(opts, term_is_list);

    if (UNLIKELY(term_get_tuple_arity(port_name_tuple) != 2)) {
        RAISE_ERROR(badarg_atom);
    }

    term t = term_get_tuple_element(port_name_tuple, 1);
    //TODO: validate port name
    char *driver_name = interop_term_to_string(t);
    if (IS_NULL_PTR(driver_name)) {
        //TODO: handle atoms here
        RAISE_ERROR(badarg_atom);
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
        new_ctx = sys_create_port(ctx->global, driver_name, opts);
    }

    free(driver_name);

    if (!new_ctx) {
        RAISE_ERROR(badarg_atom);
    } else {
        scheduler_make_waiting(ctx->global, new_ctx);
        return term_from_local_process_id(new_ctx->process_id);

    }
}

static term nif_erlang_register_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term reg_name_term = argv[0];
    VALIDATE_VALUE(reg_name_term, term_is_atom);
    term pid_or_port_term = argv[1];
    VALIDATE_VALUE(pid_or_port_term, term_is_pid);

    int atom_index = term_to_atom_index(reg_name_term);
    int pid = term_to_local_process_id(pid_or_port_term);

    // TODO: pid must be existing, not already registered.
    // TODO: reg_name_term must not be the atom undefined and not already registered.

    globalcontext_register_process(ctx->global, atom_index, pid);

    return term_nil();
}

static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term reg_name_term = argv[0];
    VALIDATE_VALUE(reg_name_term, term_is_atom);

    int atom_index = term_to_atom_index(reg_name_term);

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
    Message *message = mailbox_dequeue(ctx);
    term msg = message->message;

    if (port_is_standard_port_command(msg)) {
        CContext ccontext;
        CContext *cc = &ccontext;
        ccontext_init(cc, ctx);

        term_ref pid = ccontext_make_term_ref(cc, term_get_tuple_element(msg, 0));
        term_ref ref = ccontext_make_term_ref(cc, term_get_tuple_element(msg, 1));
        term cmd = term_get_tuple_element(msg, 2);

        if (term_is_atom(cmd) && cmd == context_make_atom(ctx, flush_a)) {
            fflush(stdout);
            port_send_reply(cc, pid, ref, port_make_ok_atom(cc));
        } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
            term cmd_name = term_get_tuple_element(cmd, 0);
            if (cmd_name == context_make_atom(ctx, puts_a)) {
                char *str = interop_term_to_string(term_get_tuple_element(cmd, 1));
                if (IS_NULL_PTR(str)) {
                    term_ref error = port_create_error_tuple(cc, "Unable to convert term to string");
                    port_send_reply(cc, pid, ref, error);
                } else {
                    printf("%s", str);
                    port_send_reply(cc, pid, ref, port_make_ok_atom(cc));
                }
                free(str);
            } else {
                term_ref error = port_create_error_tuple(cc, "Expected puts command");
                port_send_reply(cc, pid, ref, error);
            }
        } else {
            port_send_reply(cc, pid, ref, port_create_error_tuple(cc, "unrecognized command"));
        }

        ccontext_release_all_refs(cc);
    } else {
        fprintf(stderr, "WARNING: Invalid port command.  Unable to send reply");
    }

    free(message);
}

static term nif_erlang_spawn(Context *ctx, int argc, term argv[])
{
    term module_term = argv[0];
    term function_term = argv[1];
    term args_term = argv[2];
    term opts_term = argv[3];
    VALIDATE_VALUE(module_term, term_is_atom);
    VALIDATE_VALUE(function_term, term_is_atom);
    VALIDATE_VALUE(args_term, term_is_list);

    if (argc == 4) {
        // spawn_opt has been called
        VALIDATE_VALUE(opts_term, term_is_list);
    } else {
        // regular spawn
        opts_term = term_nil();
    }

    Context *new_ctx = context_new(ctx->global);

    AtomString module_string = globalcontext_atomstring_from_term(ctx->global, argv[0]);
    AtomString function_string = globalcontext_atomstring_from_term(ctx->global, argv[1]);

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

    term max_heap_size_term = interop_proplist_get_value(opts_term, context_make_atom(ctx, max_heap_size_a));
    if (max_heap_size_term != term_nil()) {
        new_ctx->has_max_heap_size = 1;
        new_ctx->max_heap_size = term_to_int32(max_heap_size_term);
    }

    //TODO: check available registers count
    int reg_index = 0;
    term t = argv[2];
    memory_ensure_free(new_ctx, memory_estimate_usage(t));
    while (!term_is_nil(t)) {
        term *t_ptr = term_get_list_ptr(t);
        new_ctx->x[reg_index] = memory_copy_term_tree(&new_ctx->heap_ptr, t_ptr[1]);
        t = *t_ptr;
        reg_index++;
    }

    return term_from_local_process_id(new_ctx->process_id);
}
static term nif_erlang_send_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term pid_term = argv[0];
    VALIDATE_VALUE(pid_term, term_is_pid);

    int local_process_id = term_to_local_process_id(pid_term);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    mailbox_send(target, argv[1]);

    return argv[1];
}

static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

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
    UNUSED(argc);

    term prepend_list = argv[0];

    if (UNLIKELY(!term_is_nonempty_list(prepend_list))) {
        if (term_is_nil(prepend_list)) {
            return argv[1];

        } else {
            RAISE_ERROR(badarg_atom);
        }
    }

    int len = term_list_length(prepend_list);
    if (UNLIKELY(memory_ensure_free(ctx, len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }

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
    UNUSED(argc);
    UNUSED(argv);

    // a ref is 64 bits, hence 8 bytes
    if (UNLIKELY(memory_ensure_free(ctx, (8 / TERM_BYTES) + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }

    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);

    return term_from_ref_ticks(ref_ticks, ctx);
}

term nif_erlang_system_time_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    struct timespec ts;
    sys_time(&ts);

    term minute_atom = context_make_atom(ctx, "\x6" "minute");
    if (argv[0] == minute_atom) {
        // FIXME: This is not standard, however we cannot hold seconds since 1970 in just 27 bits.
        return term_from_int32(ts.tv_sec / 60);

    } else {
        RAISE_ERROR(badarg_atom);
    }
}

term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    // 4 = size of date/time tuple, 3 size of date time tuple
    if (UNLIKELY(memory_ensure_free(ctx, 3 + 4 + 4) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
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

term nif_erlang_timestamp_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free(ctx, 4) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
    term timestamp_tuple = term_alloc_tuple(3, ctx);

    struct timespec ts;
    sys_time(&ts);

    term_put_tuple_element(timestamp_tuple, 0, term_from_int32(ts.tv_sec / 1000000));
    term_put_tuple_element(timestamp_tuple, 1, term_from_int32(ts.tv_sec % 1000000));
    term_put_tuple_element(timestamp_tuple, 2, term_from_int32(ts.tv_nsec / 1000));

    return timestamp_tuple;
}

static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);

    int count_elem = term_to_int32(argv[0]);

    if (UNLIKELY(count_elem < 0)) {
        RAISE_ERROR(badarg_atom);
    }

    if (UNLIKELY(memory_ensure_free(ctx, count_elem + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
    term new_tuple = term_alloc_tuple(count_elem, ctx);

    term element = argv[1];

    for (int i = 0; i < count_elem; i++) {
        term_put_tuple_element(new_tuple, i, element);
    }

    return new_tuple;
}

static term nif_erlang_insert_element_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_tuple);

    // indexes are 1 based
    int insert_index = term_to_int32(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((insert_index > old_tuple_size) || (insert_index < 0))) {
        RAISE_ERROR(badarg_atom);
    }

    int new_tuple_size = old_tuple_size + 1;
    if (UNLIKELY(memory_ensure_free(ctx, new_tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
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
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_tuple);

    // indexes are 1 based
    int delete_index = term_to_int32(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((delete_index > old_tuple_size) || (delete_index < 0))) {
        RAISE_ERROR(badarg_atom);
    }

    int new_tuple_size = old_tuple_size - 1;
    if (UNLIKELY(memory_ensure_free(ctx, new_tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
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
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_tuple);

    // indexes are 1 based
    int replace_index = term_to_int32(argv[0]) - 1;

    int tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((replace_index >= tuple_size) || (replace_index < 0))) {
        RAISE_ERROR(badarg_atom);
    }

    if (UNLIKELY(memory_ensure_free(ctx, tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }
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
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_tuple);

    int tuple_size = term_get_tuple_arity(argv[0]);

    if (UNLIKELY(memory_ensure_free(ctx, tuple_size * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }

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
    UNUSED(argc);

    term a_binary = argv[0];
    VALIDATE_VALUE(a_binary, term_is_binary);

    if (UNLIKELY(argv[1] != context_make_atom(ctx, latin1_atom))) {
        RAISE_ERROR(badarg_atom);
    }

    char *atom_string = interop_binary_to_string(a_binary);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        RAISE_ERROR(system_limit_atom);
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
    UNUSED(argc);

    term a_list = argv[0];
    VALIDATE_VALUE(a_list, term_is_list);

    char *atom_string = interop_list_to_string(a_list);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        RAISE_ERROR(system_limit_atom);
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
    UNUSED(argc);

    term atom_term = argv[0];
    VALIDATE_VALUE(atom_term, term_is_atom);

    int atom_index = term_to_atom_index(atom_term);
    AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);

    int atom_len = atom_string_len(atom_string);

    if (UNLIKELY(memory_ensure_free(ctx, atom_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }

    term prev = term_nil();
    for (int i = atom_len - 1; i >= 0; i--) {
        char c = ((const char *) atom_string_data(atom_string))[i];
        prev = term_list_prepend(term_from_int11(c), prev, ctx);
    }

    return prev;
}

static term nif_erlang_integer_to_list_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term value = argv[0];
    VALIDATE_VALUE(value, term_is_integer);

    int32_t int_value = term_to_int32(value);
    char integer_string[24];

    snprintf(integer_string, 24, "%i", int_value);
    int integer_string_len = strlen(integer_string);

    if (UNLIKELY(memory_ensure_free(ctx, integer_string_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(out_of_memory_atom);
    }

    term prev = term_nil();
    for (int i = integer_string_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(integer_string[i]), prev, ctx);
    }

    return prev;
}

static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    int32_t acc = 0;
    int digits = 0;

    VALIDATE_VALUE(t, term_is_nonempty_list);

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

        VALIDATE_VALUE(head, term_is_integer);

        int32_t c = term_to_int32(head);

        if (UNLIKELY((c < '0') || (c > '9'))) {
            RAISE_ERROR(badarg_atom);
        }

        if (acc > INT32_MAX / 10) {
            // overflow error is not standard, but we need it since we are running on an embedded device
            RAISE_ERROR(overflow_atom);
        }

        acc = (acc * 10) + (c - '0');
        digits++;
        t = term_get_list_tail(t);
    }

    if (negative) {
        acc = -acc;
    }

    if (UNLIKELY(digits == 0)) {
        RAISE_ERROR(badarg_atom);
    }

    return term_from_int32(acc);
}

static term nif_erlang_display_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    term_display(stdout, argv[0], ctx);
    printf("\n");

    return term_nil();
}

static term nifs_erlang_process_flag(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifdef ENABLE_ADVANCED_TRACE
    term pid = argv[0];
    term flag = argv[1];
    term value = argv[2];

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    term true_term = context_make_atom(ctx, true_atom);
    term false_term = context_make_atom(ctx, false_atom);
    term ok_term = context_make_atom(ctx, ok_atom);

    if (flag == context_make_atom(target, trace_calls_atom)) {
        if (value == true_term) {
            target->trace_calls = 1;
            return ok_term;
        } else if (value == false_term) {
            target->trace_calls = 0;
            return ok_term;
        }
    } else if (flag == context_make_atom(target, trace_call_args_atom)) {
        if (value == true_term) {
            target->trace_call_args = 1;
            return ok_term;
        } else if (value == false_term) {
            target->trace_call_args = 0;
            return ok_term;
        }
    } else if (flag == context_make_atom(target, trace_returns_atom)) {
        if (value == true_term) {
            target->trace_returns = 1;
            return ok_term;
        } else if (value == false_term) {
            target->trace_returns = 0;
            return ok_term;
        }
    } else if (flag == context_make_atom(target, trace_send_atom)) {
        if (value == true_term) {
            target->trace_send = 1;
            return ok_term;
        } else if (value == false_term) {
            target->trace_send = 0;
            return ok_term;
        }
    } else if (flag == context_make_atom(target, trace_receive_atom)) {
        if (value == true_term) {
            target->trace_receive = 1;
            return ok_term;
        } else if (value == false_term) {
            target->trace_receive = 0;
            return ok_term;
        }
    }
#else
    UNUSED(ctx);
    UNUSED(argv);
#endif

    RAISE_ERROR(badarg_atom);
}

static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    unsigned long terms_count;

    terms_count = memory_estimate_usage(argv[0]);

    return term_from_int32(terms_count);
}
