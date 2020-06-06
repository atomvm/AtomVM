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

#define _GNU_SOURCE

#include "nifs.h"

#include "atomshashtable.h"
#include "avmpack.h"
#include "context.h"
#include "defaultatoms.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "port.h"
#include "platform_nifs.h"
#include "scheduler.h"
#include "term.h"
#include "utils.h"
#include "sys.h"
#include "version.h"
#include "externalterm.h"

#include <stdio.h>
#include <string.h>
#include <time.h>

#define MAX_NIF_NAME_LEN 260
#define FLOAT_BUF_SIZE 64

#define VALIDATE_VALUE(value, verify_function) \
    if (UNLIKELY(!verify_function((value)))) { \
        argv[0] = ERROR_ATOM; \
        argv[1] = BADARG_ATOM; \
        return term_invalid_term(); \
    } \

#define RAISE_ERROR(error_type_atom) \
    ctx->x[0] = ERROR_ATOM; \
    ctx->x[1] = (error_type_atom); \
    return term_invalid_term();

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define NOT_FOUND (0xFF)

#ifdef ENABLE_ADVANCED_TRACE
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

static term nif_binary_at_2(Context *ctx, int argc, term argv[]);
static term nif_binary_first_1(Context *ctx, int argc, term argv[]);
static term nif_binary_last_1(Context *ctx, int argc, term argv[]);
static term nif_binary_part_3(Context *ctx, int argc, term argv[]);
static term nif_binary_split_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_delete_element_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_atom_to_binary_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_atom_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_atom_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_float_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_integer_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_existing_atom_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_concat_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_display_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_error(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_fun_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_ref_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_insert_element_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_integer_to_binary_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_integer_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_float_to_binary(Context *ctx, int argc, term argv[]);
static term nif_erlang_float_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_binary_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_float_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_existing_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_iolist_size_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_iolist_to_binary_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_register_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_send_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_setelement_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_spawn(Context *ctx, int argc, term argv[]);
static term nif_erlang_spawn_fun(Context *ctx, int argc, term argv[]);
static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_time_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_tuple_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_timestamp_0(Context *ctx, int argc, term argv[]);
static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[]);
static term nif_erlang_process_flag(Context *ctx, int argc, term argv[]);
static term nif_erlang_processes(Context *ctx, int argc, term argv[]);
static term nif_erlang_process_info(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_info(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_term(Context *ctx, int argc, term argv[]);
static term nif_erlang_term_to_binary(Context *ctx, int argc, term argv[]);
static term nif_erlang_throw(Context *ctx, int argc, term argv[]);
static term nif_erlang_pid_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_ref_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_fun_to_list(Context *ctx, int argc, term argv[]);
static term nif_atomvm_read_priv(Context *ctx, int argc, term argv[]);
static term nif_console_print(Context *ctx, int argc, term argv[]);
static term nif_base64_encode(Context *ctx, int argc, term argv[]);
static term nif_base64_decode(Context *ctx, int argc, term argv[]);
static term nif_base64_encode_to_string(Context *ctx, int argc, term argv[]);
static term nif_base64_decode_to_string(Context *ctx, int argc, term argv[]);

static const struct Nif binary_at_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_binary_at_2
};

static const struct Nif binary_first_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_binary_first_1
};

static const struct Nif binary_last_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_binary_last_1
};

static const struct Nif binary_part_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_binary_part_3
};

static const struct Nif binary_split_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_binary_split_2
};

static const struct Nif make_ref_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_make_ref_0
};

static const struct Nif atom_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_atom_to_binary_2
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

static const struct Nif binary_to_float_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_float_1
};

static const struct Nif binary_to_integer_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_integer_1
};

static const struct Nif binary_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_list_1
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

static const struct Nif error_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_error
};

static const struct Nif insert_element_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_insert_element_3
};

static const struct Nif integer_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_integer_to_binary_1
};

static const struct Nif integer_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_integer_to_list_1
};

static const struct Nif float_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_float_to_binary
};

static const struct Nif float_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_float_to_list
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

static const struct Nif list_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_binary_1
};

static const struct Nif list_to_integer_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_integer_1
};

static const struct Nif list_to_float_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_float_1
};

static const struct Nif iolist_size_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_iolist_size_1
};

static const struct Nif iolist_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_iolist_to_binary_1
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

static const struct Nif spawn_fun_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn_fun
};

static const struct Nif spawn_fun_opt_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn_fun
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
    .nif_ptr = nif_erlang_process_flag
};

static const struct Nif processes_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_processes
};

static const struct Nif process_info_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_process_info
};

static const struct Nif system_info_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_system_info
};

static const struct Nif binary_to_term_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_binary_to_term
};

static const struct Nif term_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_term_to_binary
};

static const struct Nif throw_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_throw
};

static const struct Nif pid_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_pid_to_list
};

static const struct Nif ref_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_ref_to_list
};

static const struct Nif fun_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_fun_to_list
};

static const struct Nif make_fun_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_make_fun_3
};

static const struct Nif atomvm_read_priv_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_read_priv
};
static const struct Nif console_print_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_console_print
};
static const struct Nif base64_encode_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_base64_encode
};
static const struct Nif base64_decode_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_base64_decode
};
static const struct Nif base64_encode_to_string_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_base64_encode_to_string
};
static const struct Nif base64_decode_to_string_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_base64_decode_to_string
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
        return platform_nifs_get_nif(nifname);
    }

    return nameAndPtr->nif;
}

static inline term make_maybe_boxed_int64(Context *ctx, avm_int64_t value)
{
    #if BOXED_TERMS_REQUIRED_FOR_INT64 == 2
        if ((value < AVM_INT_MIN) || (value > AVM_INT_MAX)) {
            if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT64_SIZE) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            return term_make_boxed_int64(value, ctx);

        }
    #endif

    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        if (UNLIKELY(memory_ensure_free(ctx, BOXED_INT_SIZE) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_make_boxed_int(value, ctx);

    } else {
        return term_from_int(value);
    }
}

static term nif_erlang_iolist_size_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    int ok;
    avm_int_t size = interop_iolist_size(argv[0], &ok);

    if (ok) {
        return term_from_int(size);
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_erlang_iolist_to_binary_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];

    if (term_is_binary(t)) {
        return t;
    }

    int ok;
    int bin_size = interop_iolist_size(t, &ok);
    if (!ok) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char *bin_buf = malloc(bin_size);
    if (IS_NULL_PTR(bin_buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(!interop_write_iolist(t, bin_buf))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(bin_size) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term bin_res = term_from_literal_binary(bin_buf, bin_size, ctx);

    free(bin_buf);

    return bin_res;
}

static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term port_name_tuple = argv[0];
    VALIDATE_VALUE(port_name_tuple, term_is_tuple);
    term opts = argv[1];
    VALIDATE_VALUE(opts, term_is_list);

    if (UNLIKELY(term_get_tuple_arity(port_name_tuple) != 2)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term t = term_get_tuple_element(port_name_tuple, 1);
    //TODO: validate port name
    int ok;
    char *driver_name = interop_term_to_string(t, &ok);
    if (UNLIKELY(!ok)) {
        //TODO: handle atoms here
        RAISE_ERROR(BADARG_ATOM);
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
        RAISE_ERROR(BADARG_ATOM);
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
        return UNDEFINED_ATOM;
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

    port_ensure_available(ctx, 12);

    if (port_is_standard_port_command(msg)) {

        term pid = term_get_tuple_element(msg, 0);
        term ref = term_get_tuple_element(msg, 1);
        term cmd = term_get_tuple_element(msg, 2);

        if (term_is_atom(cmd) && cmd == FLUSH_ATOM) {
            fflush(stdout);
            port_send_reply(ctx, pid, ref, OK_ATOM);
        } else if (term_is_tuple(cmd) && term_get_tuple_arity(cmd) == 2) {
            term cmd_name = term_get_tuple_element(cmd, 0);
            if (cmd_name == PUTS_ATOM) {
                int ok;
                char *str = interop_term_to_string(term_get_tuple_element(cmd, 1), &ok);
                if (UNLIKELY(!ok)) {
                    term error = port_create_error_tuple(ctx, BADARG_ATOM);
                    port_send_reply(ctx, pid, ref, error);
                } else {
                    printf("%s", str);
                    port_send_reply(ctx, pid, ref, OK_ATOM);
                }
                free(str);
            } else {
                term error = port_create_error_tuple(ctx, BADARG_ATOM);
                port_send_reply(ctx, pid, ref, error);
            }
        } else {
            port_send_reply(ctx, pid, ref, port_create_error_tuple(ctx, BADARG_ATOM));
        }
    } else {
        fprintf(stderr, "WARNING: Invalid port command.  Unable to send reply");
    }

    free(message);
}

static term nif_erlang_spawn_fun(Context *ctx, int argc, term argv[])
{
    term fun_term = argv[0];
    term opts_term = argv[1];
    VALIDATE_VALUE(fun_term, term_is_function);

    if (argc == 2) {
        // spawn_opt has been called
        VALIDATE_VALUE(opts_term, term_is_list);
    } else {
        // regular spawn
        opts_term = term_nil();
    }

    Context *new_ctx = context_new(ctx->global);

    const term *boxed_value = term_to_const_term_ptr(fun_term);

    Module *fun_module = (Module *) boxed_value[1];
    term index_or_module = boxed_value[2];
    if (term_is_atom(index_or_module)) {
        // it is not possible to spawn a function reference except for those having
        // 0 arity, however right now they are not supported.
        // TODO: implement for funs having arity 0.
        abort();
    }
    uint32_t fun_index = term_to_int32(index_or_module);

    uint32_t label;
    uint32_t arity;
    uint32_t n_freeze;
    module_get_fun(fun_module, fun_index, &label, &arity, &n_freeze);

    // TODO: new process should fail with badarity if arity != 0

    int size = 0;
    for (uint32_t i = 0; i < n_freeze; i++) {
        size += memory_estimate_usage(boxed_value[i + 3]);
    }
    if (UNLIKELY(memory_ensure_free(new_ctx, size) != MEMORY_GC_OK)) {
        //TODO: new process should be terminated, however a new pid is returned anyway
        fprintf(stderr, "Unable to allocate sufficient memory to spawn process.\n");
        abort();
    }
    for (uint32_t i = 0; i < n_freeze; i++) {
        new_ctx->x[i + arity - n_freeze] = memory_copy_term_tree(&new_ctx->heap_ptr, boxed_value[i + 3]);
    }

    new_ctx->saved_module = fun_module;
    new_ctx->saved_ip = fun_module->labels[label];
    new_ctx->cp = module_address(fun_module->module_index, fun_module->end_instruction_ii);

    term max_heap_size_term = interop_proplist_get_value(opts_term, MAX_HEAP_SIZE_ATOM);
    if (max_heap_size_term != term_nil()) {
        new_ctx->has_max_heap_size = 1;
        //TODO: check type, make sure max_heap_size_term is an int32
        new_ctx->max_heap_size = term_to_int(max_heap_size_term);
    }

    return term_from_local_process_id(new_ctx->process_id);
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
        return UNDEFINED_ATOM;
    }

    int proper;
    int args_len = term_list_length(argv[2], &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    int label = module_search_exported_function(found_module, function_string, args_len);
    //TODO: fail here if no function has been found
    new_ctx->saved_module = found_module;
    new_ctx->saved_ip = found_module->labels[label];
    new_ctx->cp = module_address(found_module->module_index, found_module->end_instruction_ii);

    term min_heap_size_term = interop_proplist_get_value(opts_term, MIN_HEAP_SIZE_ATOM);
    term max_heap_size_term = interop_proplist_get_value(opts_term, MAX_HEAP_SIZE_ATOM);

    if (min_heap_size_term != term_nil()) {
        if (UNLIKELY(!term_is_integer(min_heap_size_term))) {
            //TODO: gracefully handle this error
            abort();
        }
        new_ctx->has_min_heap_size = 1;
        new_ctx->min_heap_size = term_to_int(min_heap_size_term);
    } else {
        min_heap_size_term = term_from_int(0);
    }
    if (max_heap_size_term != term_nil()) {
        if (UNLIKELY(!term_is_integer(max_heap_size_term))) {
            //TODO: gracefully handle this error
            abort();
        }
        new_ctx->has_max_heap_size = 1;
        new_ctx->max_heap_size = term_to_int(max_heap_size_term);
    }

    if (new_ctx->has_min_heap_size && new_ctx->has_max_heap_size) {
        if (term_to_int(min_heap_size_term) > term_to_int(max_heap_size_term)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    //TODO: check available registers count
    int reg_index = 0;
    term t = argv[2];
    avm_int_t size = MAX((unsigned long) term_to_int(min_heap_size_term), memory_estimate_usage(t));
    if (UNLIKELY(memory_ensure_free(new_ctx, size) != MEMORY_GC_OK)) {
        //TODO: new process should be terminated, however a new pid is returned anyway
        fprintf(stderr, "Unable to allocate sufficient memory to spawn process.\n");
        abort();
    }
    while (term_is_nonempty_list(t)) {
        new_ctx->x[reg_index] = memory_copy_term_tree(&new_ctx->heap_ptr, term_get_list_head(t));
        reg_index++;

        t = term_get_list_tail(t);
        if (!term_is_list(t)) {
            RAISE_ERROR(BADARG_ATOM);
        }
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

    return target ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_erlang_concat_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term prepend_list = argv[0];

    if (UNLIKELY(!term_is_nonempty_list(prepend_list))) {
        if (term_is_nil(prepend_list)) {
            return argv[1];

        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    int proper;
    int len = term_list_length(prepend_list, &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    // GC might have changed all pointers
    prepend_list = argv[0];
    term append_list = argv[1];

    term t = prepend_list;
    term list_begin = term_nil();
    term *prev_term = NULL;

    while (term_is_nonempty_list(t)) {
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
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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

    term second_atom = context_make_atom(ctx, "\x6" "second");
    if (argv[0] == second_atom) {
        return make_maybe_boxed_int64(ctx, ts.tv_sec);

    } else if (argv[0] == context_make_atom(ctx, "\xB" "millisecond")) {
        return make_maybe_boxed_int64(ctx, ((int64_t) ts.tv_sec) * 1000 + ts.tv_nsec / 1000000);

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    // 4 = size of date/time tuple, 3 size of date time tuple
    if (UNLIKELY(memory_ensure_free(ctx, 3 + 4 + 4) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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

    avm_int_t count_elem = term_to_int(argv[0]);

    if (UNLIKELY(count_elem < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, count_elem + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
    avm_int_t insert_index = term_to_int(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((insert_index > old_tuple_size) || (insert_index < 0))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int new_tuple_size = old_tuple_size + 1;
    if (UNLIKELY(memory_ensure_free(ctx, new_tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
    avm_int_t delete_index = term_to_int(argv[0]) - 1;

    int old_tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((delete_index > old_tuple_size) || (delete_index < 0))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int new_tuple_size = old_tuple_size - 1;
    if (UNLIKELY(memory_ensure_free(ctx, new_tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
    avm_int_t replace_index = term_to_int(argv[0]) - 1;

    int tuple_size = term_get_tuple_arity(argv[1]);

    if (UNLIKELY((replace_index >= tuple_size) || (replace_index < 0))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, tuple_size + 1) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
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

static term nif_erlang_binary_to_integer_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];
    VALIDATE_VALUE(bin_term, term_is_binary);

    const char *bin_data = term_binary_data(bin_term);
    int bin_data_size = term_binary_size(bin_term);

    if (UNLIKELY((bin_data_size == 0) || (bin_data_size >= 24))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char null_terminated_buf[24];
    memcpy(null_terminated_buf, bin_data, bin_data_size);
    null_terminated_buf[bin_data_size] = '\0';

    //TODO: handle 64 bits numbers
    //TODO: handle errors
    char *endptr;
    uint64_t value = strtoll(null_terminated_buf, &endptr, 10);
    if (*endptr != '\0') {
        RAISE_ERROR(BADARG_ATOM);
    }

    return make_maybe_boxed_int64(ctx, value);
}

#ifndef AVM_NO_FP
static int is_valid_float_string(const char *str, int len)
{
    int has_point = 0;
    int scientific = 0;
    for (int i = 0; i < len; i++) {
        switch (str[i]) {
            case '.':
                if (!scientific) {
                    has_point = 1;
                } else {
                    return 0;
                }
                break;

            case 'e':
                if (!scientific) {
                    scientific = 1;
                } else {
                    return 0;
                }
                break;

            default:
                continue;
        }
    }
    return has_point;
}

static term parse_float(Context *ctx, const char *buf, int len)
{
    if (UNLIKELY((len == 0) || (len >= FLOAT_BUF_SIZE - 1))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char null_terminated_buf[FLOAT_BUF_SIZE];
    memcpy(null_terminated_buf, buf, len);
    null_terminated_buf[len] = '\0';

    avm_float_t fvalue;
    if (UNLIKELY(sscanf(null_terminated_buf, AVM_FLOAT_FMT, &fvalue) != 1)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // *_to_float requires that given input is a float
    if (UNLIKELY(!is_valid_float_string(null_terminated_buf, len))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, FLOAT_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_float(fvalue, ctx);
}
#endif

static term nif_erlang_binary_to_float_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifndef AVM_NO_FP
    term bin_term = argv[0];
    VALIDATE_VALUE(bin_term, term_is_binary);

    const char *bin_data = term_binary_data(bin_term);
    int bin_data_size = term_binary_size(bin_term);

    return parse_float(ctx, bin_data, bin_data_size);

#else
    UNUSED(ctx);
    UNUSED(argv);
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_erlang_list_to_float_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifndef AVM_NO_FP
    term t = argv[0];
    VALIDATE_VALUE(t, term_is_list);

    int proper;
    int len = term_list_length(t, &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ok;
    char *string = interop_list_to_string(argv[0], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term res_term = parse_float(ctx, string, len);

    free(string);

    return res_term;
#else
    UNUSED(ctx);
    UNUSED(argv);
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_erlang_binary_to_list_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term value = argv[0];
    VALIDATE_VALUE(value, term_is_binary);

    int bin_size = term_binary_size(value);
    if (UNLIKELY(memory_ensure_free(ctx, bin_size * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const char *bin_data = term_binary_data(argv[0]);

    term prev = term_nil();
    for (int i = bin_size - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(bin_data[i]), prev, ctx);
    }

    return prev;
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

    if (UNLIKELY(argv[1] != LATIN1_ATOM)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    char *atom_string = interop_binary_to_string(a_binary);
    if (IS_NULL_PTR(atom_string)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        RAISE_ERROR(SYSTEM_LIMIT_ATOM);
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
        RAISE_ERROR(BADARG_ATOM);
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

    int ok;
    char *atom_string = interop_list_to_string(a_list, &ok);
    if (UNLIKELY(!ok)) {
        fprintf(stderr, "Failed to alloc temporary string\n");
        abort();
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        RAISE_ERROR(SYSTEM_LIMIT_ATOM);
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
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_erlang_atom_to_binary_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term atom_term = argv[0];
    VALIDATE_VALUE(atom_term, term_is_atom);

    if (UNLIKELY(argv[1] != LATIN1_ATOM)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int atom_index = term_to_atom_index(atom_term);
    AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);

    int atom_len = atom_string_len(atom_string);

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(atom_len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const char *atom_data = (const char *) atom_string_data(atom_string);
    return term_from_literal_binary(atom_data, atom_len, ctx);
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
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = atom_len - 1; i >= 0; i--) {
        char c = ((const char *) atom_string_data(atom_string))[i];
        prev = term_list_prepend(term_from_int11(c), prev, ctx);
    }

    return prev;
}

static term nif_erlang_integer_to_binary_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term value = argv[0];
    VALIDATE_VALUE(value, term_is_any_integer);

    avm_int64_t int_value = term_maybe_unbox_int64(value);
    char integer_string[21];

    //TODO: just copy data to the binary instead of using the stack
    snprintf(integer_string, 21, AVM_INT64_FMT, int_value);
    int len = strlen(integer_string);

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(integer_string, len, ctx);
}

static term nif_erlang_integer_to_list_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term value = argv[0];
    VALIDATE_VALUE(value, term_is_any_integer);

    avm_int64_t int_value = term_maybe_unbox_int64(value);
    char integer_string[21];

    snprintf(integer_string, 21, AVM_INT64_FMT, int_value);
    int integer_string_len = strlen(integer_string);

    if (UNLIKELY(memory_ensure_free(ctx, integer_string_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = integer_string_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(integer_string[i]), prev, ctx);
    }

    return prev;
}

#ifndef AVM_NO_FP
static int format_float(term value, int scientific, int decimals, int compact, char *out_buf, int outbuf_len)
{
    // %lf and %f are the same since C99 due to double promotion.
    const char *format;
    if (scientific) {
        format = "%.*e";
    } else {
        format = "%.*f";
    }

    avm_float_t float_value = term_to_float(value);

    snprintf(out_buf, outbuf_len, format, decimals, float_value);

    if (compact && !scientific) {
        int start = 0;
        int len = strlen(out_buf);
        for (int i = 0; i < len; i++) {
            if (out_buf[i] == '.') {
                start = i + 2;
                break;
            }
        }
        if (start > 1) {
            int zero_seq_len = 0;
            for (int i = start; i < len; i++) {
                if (out_buf[i] == '0') {
                    if (zero_seq_len == 0) {
                        start = i;
                    }
                    zero_seq_len++;
                } else {
                    zero_seq_len = 0;
                }
            }
            if (zero_seq_len) {
                out_buf[start] = 0;
            }
        }
    }

    return strlen(out_buf);
}

int get_float_format_opts(term opts, int *scientific, int *decimals, int *compact)
{
    term t = opts;

    while (term_is_nonempty_list(t)) {
        term head = term_get_list_head(t);

        if (term_is_tuple(head) && term_get_tuple_arity(head) == 2) {
            term val_term = term_get_tuple_element(head, 1);
            if (!term_is_integer(val_term)) {
                return 0;
            }
            *decimals = term_to_int(val_term);
            if ((*decimals < 0) || (*decimals > FLOAT_BUF_SIZE - 7)) {
                return 0;
            }

            switch (term_get_tuple_element(head, 0)) {
                case DECIMALS_ATOM:
                    *scientific = 0;
                    break;
                case SCIENTIFIC_ATOM:
                    *scientific = 1;
                    break;
                default:
                    return 0;
            }

        } else if (head == DEFAULTATOMS_COMPACT_ATOM) {
            *compact = 1;

        } else {
            return 0;
        }

        t = term_get_list_tail(t);
        if (!term_is_list(t)) {
            return 0;
        }
    }

    return 1;
}
#endif

static term nif_erlang_float_to_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifndef AVM_NO_FP
    term float_term = argv[0];
    VALIDATE_VALUE(float_term, term_is_float);

    int scientific = 1;
    int decimals = 20;
    int compact = 0;

    term opts = argv[1];
    if (argc == 2) {
        VALIDATE_VALUE(opts, term_is_list);
        if (UNLIKELY(!get_float_format_opts(opts, &scientific, &decimals, &compact))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    char float_buf[FLOAT_BUF_SIZE];
    int len = format_float(float_term, scientific, decimals, compact, float_buf, FLOAT_BUF_SIZE);
    if (len > FLOAT_BUF_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(float_buf, len, ctx);
#else
    UNUSED(ctx);
    UNUSED(argv);
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_erlang_float_to_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifndef AVM_NO_FP
    term float_term = argv[0];
    VALIDATE_VALUE(float_term, term_is_float);

    int scientific = 1;
    int decimals = 20;
    int compact = 0;

    term opts = argv[1];
    if (argc == 2) {
        VALIDATE_VALUE(opts, term_is_list);
        if (UNLIKELY(!get_float_format_opts(opts, &scientific, &decimals, &compact))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    char float_buf[FLOAT_BUF_SIZE];
    int len = format_float(float_term, scientific, decimals, compact, float_buf, FLOAT_BUF_SIZE);
    if (len > FLOAT_BUF_SIZE) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(float_buf[i]), prev, ctx);
    }

    return prev;
#else
    UNUSED(ctx);
    UNUSED(argv);
    RAISE_ERROR(BADARG_ATOM);
#endif
}

static term nif_erlang_list_to_binary_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    VALIDATE_VALUE(t, term_is_list);

    int proper;
    int len = term_list_length(t, &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    //TODO: avoid this copy: just write to binary memory
    int ok;
    char *string = interop_list_to_string(argv[0], &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term bin_term = term_from_literal_binary(string, len, ctx);

    free(string);

    return bin_term;
}

static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    int64_t acc = 0;
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

    while (term_is_nonempty_list(t)) {
        term head = term_get_list_head(t);

        VALIDATE_VALUE(head, term_is_integer);

        avm_int_t c = term_to_int(head);

        if (UNLIKELY((c < '0') || (c > '9'))) {
            RAISE_ERROR(BADARG_ATOM);
        }

        //TODO: fix this
        if (acc > INT64_MAX / 10) {
            // overflow error is not standard, but we need it since we are running on an embedded device
            RAISE_ERROR(OVERFLOW_ATOM);
        }

        acc = (acc * 10) + (c - '0');
        digits++;
        t = term_get_list_tail(t);
        if (!term_is_list(t)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    if (negative) {
        acc = -acc;
    }

    if (UNLIKELY(digits == 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return make_maybe_boxed_int64(ctx, acc);
}

static term nif_erlang_display_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    term_display(stdout, argv[0], ctx);
    printf("\n");

    return term_nil();
}

static term nif_erlang_process_flag(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

#ifdef ENABLE_ADVANCED_TRACE
    term pid = argv[0];
    term flag = argv[1];
    term value = argv[2];

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (flag == context_make_atom(target, trace_calls_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_calls = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_calls = 0;
            return OK_ATOM;
        }
    } else if (flag == context_make_atom(target, trace_call_args_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_call_args = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_call_args = 0;
            return OK_ATOM;
        }
    } else if (flag == context_make_atom(target, trace_returns_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_returns = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_returns = 0;
            return OK_ATOM;
        }
    } else if (flag == context_make_atom(target, trace_send_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_send = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_send = 0;
            return OK_ATOM;
        }
    } else if (flag == context_make_atom(target, trace_receive_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_receive = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_receive = 0;
            return OK_ATOM;
        }
    }
#else
    UNUSED(ctx);
    UNUSED(argv);
#endif

    RAISE_ERROR(BADARG_ATOM);
}

typedef void *(*context_iterator)(Context *ctx, void *accum);

static void *nif_increment_context_count(Context *ctx, void *accum)
{
    UNUSED(ctx);

    return (void *) ((size_t) accum + 1);
}

static void *nif_increment_port_count(Context *ctx, void *accum)
{
    if (ctx->native_handler) {
        return (void *) ((size_t) accum + 1);
    } else {
        return accum;
    }
}

struct ContextAccumulator {
    Context *ctx;
    term result;
};

static void *nif_cons_context(Context *ctx, void *p)
{
    struct ContextAccumulator *accum = (struct ContextAccumulator *) p;
    accum->result = term_list_prepend(term_from_local_process_id(ctx->process_id), accum->result, accum->ctx);
    return (void *) accum;
}

static void *nif_iterate_processes(GlobalContext *glb, context_iterator fun, void *accum)
{
    Context *processes = GET_LIST_ENTRY(glb->processes_table, Context, processes_table_head);
    Context *p = processes;
    do {
        accum = fun(p, accum);
        p = GET_LIST_ENTRY(p->processes_table_head.next, Context, processes_table_head);
    } while (processes != p);
    return accum;
}

static size_t nif_num_processes(GlobalContext *glb)
{
    return (size_t) nif_iterate_processes(glb, nif_increment_context_count, NULL);
}

static size_t nif_num_ports(GlobalContext *glb)
{
    return (size_t) nif_iterate_processes(glb, nif_increment_port_count, NULL);
}

static term nif_list_processes(Context *ctx)
{
    struct ContextAccumulator accum;
    accum.ctx = ctx;
    accum.result = term_nil();
    nif_iterate_processes(ctx->global, nif_cons_context, (void *) &accum);
    return accum.result;
}

static term nif_erlang_processes(Context *ctx, int argc, term argv[])
{
    UNUSED(argv);
    UNUSED(argc);

    size_t num_processes = nif_num_processes(ctx->global);
    if (memory_ensure_free(ctx, 2 * num_processes) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return nif_list_processes(ctx);
}

static term nif_erlang_process_info(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term pid = argv[0];
    term item_or_item_info = argv[1];

    if (!term_is_atom(item_or_item_info)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    // TODO add support for process_info/1
    // and process_info/2 when second argument is a list
    term item = item_or_item_info;

    int local_process_id = term_to_local_process_id(pid);
    Context *target = globalcontext_get_process(ctx->global, local_process_id);

    if (memory_ensure_free(ctx, 3) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term ret = term_alloc_tuple(2, ctx);
    // heap_size size in words of the heap of the process
    if (item == HEAP_SIZE_ATOM) {
        term_put_tuple_element(ret, 0, HEAP_SIZE_ATOM);
        term_put_tuple_element(ret, 1, term_from_int32(context_heap_size(target)));

    // stack_size stack size, in words, of the process
    } else if (item == STACK_SIZE_ATOM) {
        term_put_tuple_element(ret, 0, STACK_SIZE_ATOM);
        term_put_tuple_element(ret, 1, term_from_int32(context_stack_size(target)));

    // message_queue_len number of messages currently in the message queue of the process
    } else if (item == MESSAGE_QUEUE_LEN_ATOM) {
        term_put_tuple_element(ret, 0, MESSAGE_QUEUE_LEN_ATOM);
        term_put_tuple_element(ret, 1, term_from_int32(context_message_queue_len(target)));

    // memory size in bytes of the process. This includes call stack, heap, and internal structures.
    } else if (item == MEMORY_ATOM) {
        term_put_tuple_element(ret, 0, MEMORY_ATOM);
        term_put_tuple_element(ret, 1, term_from_int32(context_size(target)));

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    return ret;
}

static term nif_erlang_system_info(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term key = argv[0];

    if (!term_is_atom(key)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (key == PROCESS_COUNT_ATOM) {
        return term_from_int32(nif_num_processes(ctx->global));
    }
    if (key == PORT_COUNT_ATOM) {
        return term_from_int32(nif_num_ports(ctx->global));
    }
    if (key == ATOM_COUNT_ATOM) {
        return term_from_int32(ctx->global->atoms_table->count);
    }
    if (key == WORDSIZE_ATOM) {
        return term_from_int32(TERM_BYTES);
    }
    if (key == SYSTEM_ARCHITECTURE_ATOM) {
        char buf[128];
        snprintf(buf, 128, "%s-%s-%s", SYSTEM_NAME, SYSTEM_VERSION, SYSTEM_ARCHITECTURE);
        size_t len = strnlen(buf, 128);
        if (memory_ensure_free(ctx, term_binary_data_size_in_terms(len)) != MEMORY_GC_OK) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return term_from_literal_binary((const uint8_t *) buf, len, ctx);
    }
    return sys_get_info(ctx, key);
}

static term nif_erlang_binary_to_term(Context *ctx, int argc, term argv[])
{
    if (argc < 1 || 2 < argc) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (argc == 2 && !term_is_list(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term binary = argv[0];
    if (!term_is_binary(binary)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    uint8_t return_used = 0;
    size_t num_extra_terms = 0;
    if (argc == 2 && term_list_member(argv[1], USED_ATOM, ctx)) {
        return_used = 1;
        num_extra_terms = 3;
    }
    term dst = term_invalid_term();
    size_t bytes_read = 0;
    enum ExternalTermResult result = externalterm_from_binary(ctx, &dst, binary, &bytes_read, num_extra_terms);
    switch (result) {
        case EXTERNAL_TERM_BAD_ARG:
            RAISE_ERROR(BADARG_ATOM);
        case EXTERNAL_TERM_MALLOC:
        case EXTERNAL_TERM_HEAP_ALLOC:
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        case EXTERNAL_TERM_OK:
        default:
            break;
    }
    if (term_is_invalid_term(dst)) {
        RAISE_ERROR(BADARG_ATOM)
    }
    if (return_used) {
        term ret = term_alloc_tuple(2, ctx);
        term_put_tuple_element(ret, 0, dst);
        term_put_tuple_element(ret, 1, term_from_int(bytes_read));
        return ret;
    } else {
        return dst;
    }
}

static term nif_erlang_term_to_binary(Context *ctx, int argc, term argv[])
{
    if (argc != 1) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term t = argv[0];
    term ret = externalterm_to_binary(ctx, t);
    if (term_is_invalid_term(ret)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    return ret;
}

static term nif_binary_at_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];
    term pos_term = argv[1];

    VALIDATE_VALUE(bin_term, term_is_binary);
    VALIDATE_VALUE(pos_term, term_is_integer);

    int32_t size = term_binary_size(bin_term);
    avm_int_t pos = term_to_int(pos_term);

    if (UNLIKELY((pos < 0) || (pos >= size))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_int11(term_binary_data(bin_term)[pos]);
}

static term nif_binary_first_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];

    VALIDATE_VALUE(bin_term, term_is_binary);

    if (UNLIKELY(term_binary_size(bin_term) == 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_int11(term_binary_data(bin_term)[0]);
}

static term nif_binary_last_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];

    VALIDATE_VALUE(bin_term, term_is_binary);

    int size = term_binary_size(bin_term);

    if (UNLIKELY(size == 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_int11(term_binary_data(bin_term)[size - 1]);
}

static term nif_binary_part_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];
    term pos_term = argv[1];
    term len_term = argv[2];

    VALIDATE_VALUE(bin_term, term_is_binary);
    VALIDATE_VALUE(pos_term, term_is_integer);
    VALIDATE_VALUE(len_term, term_is_integer);

    int bin_size = term_binary_size(bin_term);
    avm_int_t pos = term_to_int(pos_term);
    avm_int_t len = term_to_int(len_term);

    if (len < 0) {
        pos += len;
        len = -len;
    }

    if (UNLIKELY((pos < 0) || (pos > bin_size) || (pos + len > bin_size))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // + 2, which is the binary header size
    if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const char *bin_data = term_binary_data(argv[0]);
    return term_from_literal_binary(bin_data + pos, len, ctx);
}

static term nif_binary_split_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];
    term pattern_term = argv[1];

    VALIDATE_VALUE(bin_term, term_is_binary);
    VALIDATE_VALUE(pattern_term, term_is_binary);

    int bin_size = term_binary_size(bin_term);
    int pattern_size = term_binary_size(pattern_term);

    if (UNLIKELY(pattern_size == 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const char *bin_data = term_binary_data(bin_term);
    const char *pattern_data = term_binary_data(pattern_term);

    const char *found = (const char *) memmem(bin_data, bin_size, pattern_data, pattern_size);

    int offset = found - bin_data;

    if (found) {
        int tok_size = offset;
        // + 2, which is the binary header size
        int tok_size_in_terms = term_binary_data_size_in_terms(tok_size) + BINARY_HEADER_SIZE;

        int rest_size = bin_size - offset - pattern_size;
        // + 2, which is the binary header size
        int rest_size_in_terms = term_binary_data_size_in_terms(rest_size) + BINARY_HEADER_SIZE;

        // + 2 which is the result cons
        if (UNLIKELY(memory_ensure_free(ctx, tok_size_in_terms + rest_size_in_terms + 2) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        const char *bin_data = term_binary_data(argv[0]);

        term tok = term_from_literal_binary(bin_data, tok_size, ctx);
        term rest = term_from_literal_binary(bin_data + offset + pattern_size, rest_size, ctx);

        term result_list = term_list_prepend(rest, term_nil(), ctx);
        result_list = term_list_prepend(tok, result_list, ctx);

        return result_list;

    } else {
        if (UNLIKELY(memory_ensure_free(ctx, 2) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_list_prepend(argv[0], term_nil(), ctx);
    }
}

static term nif_erlang_throw(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];

    ctx->x[0] = THROW_ATOM;
    ctx->x[1] = t;
    return term_invalid_term();
}

static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    unsigned long terms_count;

    terms_count = memory_estimate_usage(argv[0]);

    return term_from_int32(terms_count);
}

static term nif_erlang_pid_to_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    VALIDATE_VALUE(t, term_is_pid);

    // 2^32 = 4294967296 (10 chars)
    // 6 chars of static text + '\0'
    char buf[17];
    snprintf(buf, 17, "<0.%i.0>", term_to_local_process_id(t));

    int str_len = strnlen(buf, 17);

    if (UNLIKELY(memory_ensure_free(ctx, str_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, ctx);
    }
    return prev;
}

static term nif_erlang_ref_to_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    VALIDATE_VALUE(t, term_is_reference);

    const char *format =
    #ifdef __clang__
            "#Ref<0.0.0.%llu>";
    #else
            "#Ref<0.0.0.%lu>";
    #endif
    // 2^64 = 18446744073709551616 (20 chars)
    // 12 chars of static text + '\0'
    char buf[33];
    snprintf(buf, 33, format, term_to_ref_ticks(t));

    int str_len = strnlen(buf, 33);

    if (UNLIKELY(memory_ensure_free(ctx, str_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, ctx);
    }
    return prev;
}

static term nif_erlang_fun_to_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    VALIDATE_VALUE(t, term_is_function);

    const term *boxed_value = term_to_const_term_ptr(t);
    Module *fun_module = (Module *) boxed_value[1];
    uint32_t fun_index = boxed_value[2];

    // char-len(address) + char-len(32-bit-num) + 16 + '\0' = 47
    // 20                  10
    const char *format =
    #ifdef __clang__
            "#Fun<erl_eval.%lu.%llu>";
    #else
            "#Fun<erl_eval.%lu.%llu>";
    #endif
    char buf[47];
    snprintf(buf, 47, format, fun_index, (unsigned long) fun_module);

    int str_len = strnlen(buf, 47);

    if (UNLIKELY(memory_ensure_free(ctx, str_len * 2) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, ctx);
    }
    return prev;
}

static term nif_erlang_error(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term r = argv[0];

    RAISE_ERROR(r);
}

static term nif_erlang_make_fun_3(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term module_term = argv[0];
    term function_term = argv[1];
    term arity_term = argv[2];

    VALIDATE_VALUE(module_term, term_is_atom);
    VALIDATE_VALUE(function_term, term_is_atom);
    VALIDATE_VALUE(arity_term, term_is_integer);

    return term_make_function_reference(module_term, function_term, arity_term, ctx);
}

// AtomVM extension
static term nif_atomvm_read_priv(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term app_term = argv[0];
    term path_term = argv[1];
    VALIDATE_VALUE(app_term, term_is_atom);

    GlobalContext *glb = ctx->global;

    if (UNLIKELY(list_is_empty(&glb->avmpack_data))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int atom_index = term_to_atom_index(app_term);
    AtomString atom_string = (AtomString) valueshashtable_get_value(glb->atoms_ids_table,
            atom_index, (unsigned long) NULL);

    int app_len = atom_string_len(atom_string);
    char *app = malloc(app_len + 1);
    memcpy(app, (const char *) atom_string_data(atom_string), app_len);
    app[app_len] = '\0';

    int ok;
    char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!path)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    int complete_path_len = app_len + strlen("/priv/") + strlen(path) + 1;
    char *complete_path = malloc(complete_path_len);
    snprintf(complete_path, complete_path_len, "%s/priv/%s", app, path);
    free(app);
    free(path);

    const void *bin_data;
    uint32_t size;
    struct ListHead *item;
    LIST_FOR_EACH(item, &glb->avmpack_data) {
        struct AVMPackData *avmpack_data = (struct AVMPackData *) item;
        if (avmpack_find_section_by_name(avmpack_data->data, complete_path, &bin_data, &size)) {
            uint32_t file_size = READ_32_ALIGNED((uint32_t *) bin_data);
            free(complete_path);
            if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_REFC_BINARY_SIZE) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return term_from_const_binary(((uint8_t *) bin_data) + sizeof(uint32_t), file_size, ctx);
        }
    }

    free(complete_path);
    return UNDEFINED_ATOM;
}

static term nif_console_print(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    if (term_is_binary(t)) {
        const char *data = term_binary_data(t);
        unsigned long n = term_binary_size(t);
        fprintf(stdout, "%.*s", (int) n, data);
    } else {
        VALIDATE_VALUE(t, term_is_list);
        int ok;
        int size = interop_iolist_size(t, &ok);
        if (!ok) {
            RAISE_ERROR(BADARG_ATOM);
        }
        char *buf = malloc(size);
        if (IS_NULL_PTR(buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        if (UNLIKELY(!interop_write_iolist(t, buf))) {
            free(buf);
            RAISE_ERROR(BADARG_ATOM);
        }
        fprintf(stdout, "%.*s", size, buf);
        fflush(stdout);
        free(buf);
    }
    return OK_ATOM;
}

static char b64_table[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// per https://tools.ietf.org/rfc/rfc4648.txt

static term base64_encode(Context *ctx, int argc, term argv[], bool return_binary)
{
    UNUSED(argc);
    term src = argv[0];

    size_t src_size;
    uint8_t *src_pos = NULL, *src_buf = NULL;
    if (term_is_binary(src)) {
        src_size = term_binary_size(src);
        if (src_size == 0) {
            return return_binary ? src : term_nil();
        }
    } else if (term_is_list(src)) {
        int ok;
        src_size = interop_iolist_size(src, &ok);
        if (UNLIKELY(!ok)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        if (src_size == 0) {
            if (return_binary) {
                if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(0) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return term_create_empty_binary(0, ctx);
            } else {
                return term_nil();
            }
        }
        src_buf = malloc(src_size);
        if (IS_NULL_PTR(src_buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        if (UNLIKELY(!interop_write_iolist(src, (char *) src_buf))) {
            free(src_buf);
            RAISE_ERROR(BADARG_ATOM);
        }
        src_pos = src_buf;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t dst_size = (4 * src_size) / 3;
    size_t pad = 0;
    switch (src_size % 3) {
        case 0:
            break;
        case 1:
            pad = 2;
            dst_size++;
            break;
        case 2:
            pad = 1;
            dst_size++;
            break;
    }
    size_t dst_size_with_pad = dst_size + pad;
    size_t heap_free = return_binary ?
        term_binary_data_size_in_terms(dst_size_with_pad) + BINARY_HEADER_SIZE
        : 2*dst_size;
    if (UNLIKELY(memory_ensure_free(ctx, heap_free) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (term_is_binary(argv[0])) {
        src_pos = (uint8_t *) term_binary_data(argv[0]);
    }
    term dst;
    uint8_t *dst_pos;
    if (return_binary) {
        dst = term_create_empty_binary(dst_size_with_pad, ctx);
        dst_pos = (uint8_t *) term_binary_data(dst);
    } else {
        dst_pos = malloc(dst_size_with_pad);
        if (IS_NULL_PTR(dst_pos)) {
            free(src_buf);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }
    for (size_t i = 0;  i < dst_size;  ++i) {
        uint8_t accum = 0;
        switch (i & 0x03) {
            case 0:
                dst_pos[i] = b64_table[(*src_pos) >> 2];
                break;
            case 1:
                accum = ((*src_pos) & 0x03) << 4;
                src_pos++;
                if (i < dst_size - 1) {
                    accum |= ((*src_pos) & 0xF0) >> 4;
                }
                dst_pos[i] = b64_table[accum];
                break;
            case 2:
                accum = ((*src_pos) & 0x0F) << 2;
                src_pos++;
                if (i < dst_size - 1) {
                    accum |= ((*src_pos) & 0xC0) >> 6;
                }
                dst_pos[i] = b64_table[accum];
                break;
            case 3:
                dst_pos[i] = b64_table[(*src_pos) & 0x3F];
                src_pos++;
                break;
        }
    }
    free(src_buf);
    for (size_t i = 0;  i < pad;  ++i) {
        dst_pos[dst_size + i] = '=';
    }
    if (!return_binary) {
        dst = term_from_string(dst_pos, dst_size_with_pad, ctx);
        free(dst_pos);
    }
    return dst;
}

static inline uint8_t find_index(uint8_t c)
{
    if ('A' <= c && c <= 'Z') {
        return c - 'A';
    } else if ('a' <= c && c <= 'z') {
        return 26 + (c - 'a');
    } else if ('0' <= c && c <= '9') {
           return 52 + (c - '0');
    } else if (c == '+') {
        return 62;
    } else if (c == '/') {
        return 63;
    } else {
        return NOT_FOUND;
    }
}

static term base64_decode(Context *ctx, int argc, term argv[], bool return_binary)
{
    UNUSED(argc);
    term src = argv[0];

    size_t src_size;
    uint8_t *src_pos, *src_buf = NULL;
    if (term_is_binary(src)) {
        src_size = term_binary_size(src);
        if (src_size == 0) {
            return return_binary ? src : term_nil();
        }
        // for now, we only accept valid encodings (no whitespace)
        if (src_size % 4 != 0) {
            RAISE_ERROR(BADARG_ATOM);
        }
        src_pos = (uint8_t *) term_binary_data(src);
    } else if (term_is_list(src)) {
        int ok;
        src_size = interop_iolist_size(src, &ok);
        if (UNLIKELY(!ok)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        if (src_size == 0) {
            if (return_binary) {
                if (UNLIKELY(memory_ensure_free(ctx, term_binary_data_size_in_terms(0) + BINARY_HEADER_SIZE) != MEMORY_GC_OK)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return term_create_empty_binary(0, ctx);
            } else {
                return term_nil();
            }
        }
        // for now, we only accept valid encodings (no whitespace)
        if (src_size % 4 != 0) {
            RAISE_ERROR(BADARG_ATOM);
        }
        src_buf = malloc(src_size);
        if (IS_NULL_PTR(src_buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        if (UNLIKELY(!interop_write_iolist(src, (char *) src_buf))) {
            free(src_buf);
            RAISE_ERROR(BADARG_ATOM);
        }
        src_pos = src_buf;
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t dst_size = (3 * src_size) / 4;
    size_t pad = 0;
    if (src_pos[src_size - 1] == '=') {
        if (src_pos[src_size - 2] == '=') {
            pad = 2;
        } else {
            pad = 1;
        }
    }
    dst_size -= pad;
    size_t heap_free = return_binary ?
        term_binary_data_size_in_terms(dst_size) + BINARY_HEADER_SIZE
        : 2*dst_size;
    if (UNLIKELY(memory_ensure_free(ctx, heap_free) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term dst;
    uint8_t *dst_pos, *dst_buf = NULL;
    if (return_binary) {
        dst = term_create_empty_binary(dst_size, ctx);
        dst_pos = (uint8_t *) term_binary_data(dst);
    } else {
        dst_buf = malloc(dst_size);
        if (IS_NULL_PTR(dst_buf)) {
            free(src_buf);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        dst_pos = dst_buf;
    }
    if (term_is_binary(argv[0])) {
        src_pos = (uint8_t *) term_binary_data(argv[0]);
    }
    size_t n = src_size - pad;
    for (size_t i = 0;  i < n;  ++i) {
        uint8_t octet = find_index(src_pos[i]);
        if (octet == NOT_FOUND) {
            RAISE_ERROR(BADARG_ATOM);
        }
        switch (i & 0x03) {
            case 0:
                *dst_pos = octet << 2;
                break;
            case 1:
                *dst_pos |= octet >> 4;
                if ((pad != 2) || i < n - 1) {
                    dst_pos++;
                    *dst_pos = (octet & 0x0F) << 4;
                }
                break;
            case 2:
                *dst_pos |= (octet & 0xFC) >> 2;
                if ((pad != 1) || i < n - 1) {
                    dst_pos++;
                    *dst_pos = (octet & 0x03) << 6;
                }
                break;
            case 3:
                *dst_pos |= octet;
                dst_pos++;
                break;
        }
    }
    free(src_buf);
    if (!return_binary) {
        dst = term_from_string(dst_buf, dst_size, ctx);
        free(dst_buf);
    }
    return dst;
}

static term nif_base64_encode(Context *ctx, int argc, term argv[])
{
    return base64_encode(ctx, argc, argv, true);
}

static term nif_base64_decode(Context *ctx, int argc, term argv[])
{
    return base64_decode(ctx, argc, argv, true);
}

static term nif_base64_encode_to_string(Context *ctx, int argc, term argv[])
{
    return base64_encode(ctx, argc, argv, false);
}

static term nif_base64_decode_to_string(Context *ctx, int argc, term argv[])
{
    return base64_decode(ctx, argc, argv, false);
}
