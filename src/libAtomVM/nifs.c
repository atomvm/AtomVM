/*
 * This file is part of AtomVM.
 *
 * Copyright 2017 Davide Bettio <davide@uninstall.it>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include "nifs.h"

#include <errno.h>
#include <fenv.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "atom_table.h"
#include "avm_version.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "defaultatoms.h"
#include "dictionary.h"
#include "externalterm.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_nifs.h"
#include "port.h"
#include "posix_nifs.h"
#include "scheduler.h"
#include "smp.h"
#include "synclist.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#define MAX_NIF_NAME_LEN 260
#define FLOAT_BUF_SIZE 64

#define RAISE(a, b)  \
    ctx->x[0] = (a); \
    ctx->x[1] = (b); \
    return term_invalid_term();

#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#define NOT_FOUND (0xFF)

#ifdef ENABLE_ADVANCED_TRACE
static const char *const trace_calls_atom = "\xB" "trace_calls";
static const char *const trace_call_args_atom = "\xF" "trace_call_args";
static const char *const trace_returns_atom = "\xD" "trace_returns";
static const char *const trace_send_atom = "\xA" "trace_send";
static const char *const trace_receive_atom = "\xD" "trace_receive";
#endif

static NativeHandlerResult process_echo_mailbox(Context *ctx);
static NativeHandlerResult process_console_mailbox(Context *ctx);

static term binary_to_atom(Context *ctx, int argc, term argv[], int create_new);
static term list_to_atom(Context *ctx, int argc, term argv[], int create_new);

static term nif_binary_at_2(Context *ctx, int argc, term argv[]);
static term nif_binary_first_1(Context *ctx, int argc, term argv[]);
static term nif_binary_last_1(Context *ctx, int argc, term argv[]);
static term nif_binary_part_3(Context *ctx, int argc, term argv[]);
static term nif_binary_split_2(Context *ctx, int argc, term argv[]);
static term nif_calendar_system_time_to_universal_time_2(Context *ctx, int argc, term argv[]);
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
static term nif_erlang_erase_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_error(Context *ctx, int argc, term argv[]);
static term nif_erlang_exit(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_fun_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_ref_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_insert_element_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_integer_to_binary_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_integer_to_list_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_link(Context *ctx, int argc, term argv[]);
static term nif_erlang_float_to_binary(Context *ctx, int argc, term argv[]);
static term nif_erlang_float_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_binary_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_integer_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_float_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_existing_atom_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_monotonic_time_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_iolist_size_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_iolist_to_binary_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_register_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_unregister_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_send_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_setelement_3(Context *ctx, int argc, term argv[]);
static term nif_erlang_spawn_opt(Context *ctx, int argc, term argv[]);
static term nif_erlang_spawn_fun_opt(Context *ctx, int argc, term argv[]);
static term nif_erlang_whereis_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_time_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_tuple_to_list_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_list_to_tuple_1(Context *ctx, int argc, term argv[]);
static term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[]);
static term nif_erlang_localtime(Context *ctx, int argc, term argv[]);
static term nif_erlang_timestamp_0(Context *ctx, int argc, term argv[]);
static term nif_erts_debug_flat_size(Context *ctx, int argc, term argv[]);
static term nif_erlang_process_flag(Context *ctx, int argc, term argv[]);
static term nif_erlang_processes(Context *ctx, int argc, term argv[]);
static term nif_erlang_process_info(Context *ctx, int argc, term argv[]);
static term nif_erlang_put_2(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_info(Context *ctx, int argc, term argv[]);
static term nif_erlang_system_flag(Context *ctx, int argc, term argv[]);
static term nif_erlang_binary_to_term(Context *ctx, int argc, term argv[]);
static term nif_erlang_term_to_binary(Context *ctx, int argc, term argv[]);
static term nif_erlang_throw(Context *ctx, int argc, term argv[]);
static term nif_erlang_raise(Context *ctx, int argc, term argv[]);
static term nif_erlang_pid_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_ref_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_fun_to_list(Context *ctx, int argc, term argv[]);
static term nif_erlang_function_exported(Context *ctx, int argc, term argv[]);
static term nif_erlang_garbage_collect(Context *ctx, int argc, term argv[]);
static term nif_erlang_group_leader(Context *ctx, int argc, term argv[]);
static term nif_erlang_get_module_info(Context *ctx, int argc, term argv[]);
static term nif_erlang_memory(Context *ctx, int argc, term argv[]);
static term nif_erlang_monitor(Context *ctx, int argc, term argv[]);
static term nif_erlang_demonitor(Context *ctx, int argc, term argv[]);
static term nif_erlang_unlink(Context *ctx, int argc, term argv[]);
static term nif_atomvm_add_avm_pack_binary(Context *ctx, int argc, term argv[]);
static term nif_atomvm_add_avm_pack_file(Context *ctx, int argc, term argv[]);
static term nif_atomvm_close_avm_pack(Context *ctx, int argc, term argv[]);
static term nif_atomvm_get_start_beam(Context *ctx, int argc, term argv[]);
static term nif_atomvm_read_priv(Context *ctx, int argc, term argv[]);
static term nif_console_print(Context *ctx, int argc, term argv[]);
static term nif_base64_encode(Context *ctx, int argc, term argv[]);
static term nif_base64_decode(Context *ctx, int argc, term argv[]);
static term nif_base64_encode_to_string(Context *ctx, int argc, term argv[]);
static term nif_base64_decode_to_string(Context *ctx, int argc, term argv[]);
static term nif_code_load_abs(Context *ctx, int argc, term argv[]);
static term nif_code_load_binary(Context *ctx, int argc, term argv[]);
static term nif_lists_reverse(Context *ctx, int argc, term argv[]);
static term nif_maps_next(Context *ctx, int argc, term argv[]);
static term nif_unicode_characters_to_list(Context *ctx, int argc, term argv[]);
static term nif_unicode_characters_to_binary(Context *ctx, int argc, term argv[]);

#define DECLARE_MATH_NIF_FUN(moniker) \
    static term nif_math_##moniker(Context *ctx, int argc, term argv[]);

DECLARE_MATH_NIF_FUN(cos)
DECLARE_MATH_NIF_FUN(acos)
DECLARE_MATH_NIF_FUN(acosh)
DECLARE_MATH_NIF_FUN(asin)
DECLARE_MATH_NIF_FUN(asinh)
DECLARE_MATH_NIF_FUN(atan)
DECLARE_MATH_NIF_FUN(atan2)
DECLARE_MATH_NIF_FUN(atanh)
DECLARE_MATH_NIF_FUN(ceil)
DECLARE_MATH_NIF_FUN(cosh)
DECLARE_MATH_NIF_FUN(exp)
DECLARE_MATH_NIF_FUN(floor)
DECLARE_MATH_NIF_FUN(fmod)
DECLARE_MATH_NIF_FUN(log)
DECLARE_MATH_NIF_FUN(log10)
DECLARE_MATH_NIF_FUN(log2)
DECLARE_MATH_NIF_FUN(pow)
DECLARE_MATH_NIF_FUN(sin)
DECLARE_MATH_NIF_FUN(sinh)
DECLARE_MATH_NIF_FUN(sqrt)
DECLARE_MATH_NIF_FUN(tan)
DECLARE_MATH_NIF_FUN(tanh)

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

static const struct Nif erase_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_erase_1
};

static const struct Nif error_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_error
};

static const struct Nif exit_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_exit
};

static const struct Nif insert_element_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_insert_element_3
};

static const struct Nif integer_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_integer_to_binary_2
};

static const struct Nif integer_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_integer_to_list_2
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

static const struct Nif list_to_tuple_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_list_to_tuple_1
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

static const struct Nif unregister_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_unregister_1
};

static const struct Nif spawn_opt_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn_opt
};

static const struct Nif spawn_fun_opt_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_spawn_fun_opt
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

static const struct Nif monotonic_time_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_monotonic_time_1
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

static const struct Nif localtime_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_localtime
};

static const struct Nif timestamp_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_timestamp_0
};

static const struct Nif system_time_to_universal_time_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_calendar_system_time_to_universal_time_2
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

static const struct Nif put_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_put_2
};

static const struct Nif system_info_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_system_info
};

static const struct Nif system_flag_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_system_flag
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

static const struct Nif function_exported_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_function_exported
};

static const struct Nif garbage_collect_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_garbage_collect
};

static const struct Nif make_fun_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_make_fun_3
};

static const struct Nif memory_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_memory
};

static const struct Nif monitor_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_monitor
};

static const struct Nif demonitor_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_demonitor
};

static const struct Nif link_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_link
};

static const struct Nif unlink_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_unlink
};

static const struct Nif group_leader_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_group_leader
};

static const struct Nif get_module_info_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_get_module_info
};

static const struct Nif raise_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_erlang_raise
};

static const struct Nif atomvm_add_avm_pack_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_add_avm_pack_binary
};
static const struct Nif atomvm_add_avm_pack_file_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_add_avm_pack_file
};
static const struct Nif atomvm_close_avm_pack_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_close_avm_pack
};
static const struct Nif atomvm_get_start_beam_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_get_start_beam
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
static const struct Nif code_load_abs_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_code_load_abs
};
static const struct Nif code_load_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_code_load_binary
};
static const struct Nif lists_reverse_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_lists_reverse
};
static const struct Nif maps_next_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_maps_next
};
static const struct Nif unicode_characters_to_list_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_unicode_characters_to_list
};
static const struct Nif unicode_characters_to_binary_nif =
{
    .base.type = NIFFunctionType,
    .nif_ptr = nif_unicode_characters_to_binary
};

#define DEFINE_MATH_NIF(moniker)                    \
    static const struct Nif math_##moniker##_nif =  \
    {                                               \
        .base.type = NIFFunctionType,               \
        .nif_ptr = nif_math_##moniker               \
    };

DEFINE_MATH_NIF(cos)
DEFINE_MATH_NIF(acos)
DEFINE_MATH_NIF(acosh)
DEFINE_MATH_NIF(asin)
DEFINE_MATH_NIF(asinh)
DEFINE_MATH_NIF(atan)
DEFINE_MATH_NIF(atan2)
DEFINE_MATH_NIF(atanh)
DEFINE_MATH_NIF(ceil)
DEFINE_MATH_NIF(cosh)
DEFINE_MATH_NIF(exp)
DEFINE_MATH_NIF(floor)
DEFINE_MATH_NIF(fmod)
DEFINE_MATH_NIF(log)
DEFINE_MATH_NIF(log10)
DEFINE_MATH_NIF(log2)
DEFINE_MATH_NIF(pow)
DEFINE_MATH_NIF(sin)
DEFINE_MATH_NIF(sinh)
DEFINE_MATH_NIF(sqrt)
DEFINE_MATH_NIF(tan)
DEFINE_MATH_NIF(tanh)

//Handle optional nifs
#if HAVE_OPEN && HAVE_CLOSE
#define IF_HAVE_OPEN_CLOSE(expr) (expr)
#else
#define IF_HAVE_OPEN_CLOSE(expr) NULL
#endif
#if HAVE_MKFIFO
#define IF_HAVE_MKFIFO(expr) (expr)
#else
#define IF_HAVE_MKFIFO(expr) NULL
#endif
#if HAVE_UNLINK
#define IF_HAVE_UNLINK(expr) (expr)
#else
#define IF_HAVE_UNLINK(expr) NULL
#endif
#if HAVE_CLOCK_SETTIME
#define IF_HAVE_CLOCK_SETTIME_OR_SETTIMEOFDAY(expr) (expr)
#elif HAVE_SETTIMEOFDAY
#define IF_HAVE_CLOCK_SETTIME_OR_SETTIMEOFDAY(expr) (expr)
#else
#define IF_HAVE_CLOCK_SETTIME_OR_SETTIMEOFDAY(expr) NULL
#endif

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
        AVM_ABORT();
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
            if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT64_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            return term_make_boxed_int64(value, &ctx->heap);
        }
    #endif

    if ((value < MIN_NOT_BOXED_INT) || (value > MAX_NOT_BOXED_INT)) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, BOXED_INT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return term_make_boxed_int(value, &ctx->heap);
    } else {
        return term_from_int(value);
    }
}

static term nif_erlang_iolist_size_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    size_t size;
    switch (interop_iolist_size(argv[0], &size)) {
        case InteropOk:
            return term_from_int(size);
        case InteropMemoryAllocFail:
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        case InteropBadArg:
            RAISE_ERROR(BADARG_ATOM);
        default:
            RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_erlang_iolist_to_binary_1(Context *ctx, int argc, term argv[])
{
    term t = argv[0];
    if (term_is_binary(t)) {
        return t;
    }

    return nif_erlang_list_to_binary_1(ctx, argc, argv);
}

static term nif_erlang_open_port_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term port_name_tuple = argv[0];
    VALIDATE_VALUE(port_name_tuple, term_is_tuple);
    term opts = argv[1];
    if (UNLIKELY(!term_is_list(opts) && !term_is_map(opts))) {
        RAISE_ERROR(BADARG_ATOM);
    }

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
    int32_t pid = term_to_local_process_id(pid_or_port_term);

    // pid must be existing, not already registered, and not the atom undefined.
    if (UNLIKELY(!globalcontext_process_exists(ctx->global, pid)) ||
        globalcontext_get_registered_process(ctx->global, atom_index) != 0 ||
        reg_name_term == UNDEFINED_ATOM){
        RAISE_ERROR(BADARG_ATOM);
    }

    globalcontext_register_process(ctx->global, atom_index, pid);

    return TRUE_ATOM;
}

static term nif_erlang_unregister_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term reg_name_term = argv[0];
    VALIDATE_VALUE(reg_name_term, term_is_atom);

    int atom_index = term_to_atom_index(reg_name_term);

    bool unregistered = globalcontext_unregister_process(ctx->global, atom_index);
    if (UNLIKELY(!unregistered)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return TRUE_ATOM;
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

static NativeHandlerResult process_echo_mailbox(Context *ctx)
{
    NativeHandlerResult result = NativeContinue;

    Message *msg = mailbox_first(&ctx->mailbox);
    term pid = term_get_tuple_element(msg->message, 0);
    term val = term_get_tuple_element(msg->message, 1);

    if (val == CLOSE_ATOM) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &pid, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            // Not much we can do.
            return NativeTerminate;
        }
        result = NativeTerminate;
        term reply = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(reply, 0, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(reply, 1, CLOSED_ATOM);
        port_send_message(ctx->global, pid, reply);
    } else {
        port_send_message(ctx->global, pid, val);
    }

    mailbox_remove_message(&ctx->mailbox, &ctx->heap);

    return result;
}

static bool is_tagged_tuple(term t, term tag, int size)
{
    return term_is_tuple(t) && term_get_tuple_arity(t) == size && term_get_tuple_element(t, 0) == tag;
}

static NativeHandlerResult process_console_message(Context *ctx, term msg)
{
    // msg is not in the port's heap
    NativeHandlerResult result = NativeContinue;
    if (UNLIKELY(memory_ensure_free_opt(ctx, 12, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        fprintf(stderr, "Unable to allocate sufficient memory for console driver.\n");
        AVM_ABORT();
    }

    GenMessage gen_message;

    if (term_is_tuple(msg) && term_get_tuple_arity(msg) == 2 && term_get_tuple_element(msg, 1) == CLOSE_ATOM) {
        result = NativeTerminate;
        term pid = term_get_tuple_element(msg, 0);
        term reply = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(reply, 0, term_from_local_process_id(ctx->process_id));
        term_put_tuple_element(reply, 1, CLOSED_ATOM);
        port_send_message(ctx->global, pid, reply);
    } else if (is_tagged_tuple(msg, IO_REQUEST_ATOM, 4)) {
        term pid = term_get_tuple_element(msg, 1);
        term ref = term_get_tuple_element(msg, 2);
        term req = term_get_tuple_element(msg, 3);
        uint64_t ref_ticks = term_to_ref_ticks(ref);

        if (is_tagged_tuple(req, PUT_CHARS_ATOM, 3)) {
            term chars = term_get_tuple_element(req, 2);
            int ok;
            char *str = interop_term_to_string(chars, &ok);
            if (ok) {
                printf("%s", str);
                free(str);

                term refcopy = term_from_ref_ticks(ref_ticks, &ctx->heap);

                term reply = term_alloc_tuple(3, &ctx->heap);
                term_put_tuple_element(reply, 0, IO_REPLY_ATOM);
                term_put_tuple_element(reply, 1, refcopy);
                term_put_tuple_element(reply, 2, OK_ATOM);

                port_send_message(ctx->global, pid, reply);
            }
        }

    } else if (port_parse_gen_message(msg, &gen_message) == GenCallMessage) {
        term pid = gen_message.pid;
        term ref = gen_message.ref;
        term cmd = gen_message.req;

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

    return result;
}

static NativeHandlerResult process_console_mailbox(Context *ctx)
{
    NativeHandlerResult result = NativeContinue;
    while (result == NativeContinue) {
        Message *message = mailbox_first(&ctx->mailbox);
        if (message == NULL) break;
        term msg = message->message;

        result = process_console_message(ctx, msg);

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }

    return result;
}

// Common handling of spawn/1, spawn/3, spawn_opt/2, spawn_opt/4
// opts_term is [] for spawn/1,3
static term do_spawn(Context *ctx, Context *new_ctx, term opts_term)
{
    term min_heap_size_term = interop_proplist_get_value(opts_term, MIN_HEAP_SIZE_ATOM);
    term max_heap_size_term = interop_proplist_get_value(opts_term, MAX_HEAP_SIZE_ATOM);
    term link_term = interop_proplist_get_value(opts_term, LINK_ATOM);
    term monitor_term = interop_proplist_get_value(opts_term, MONITOR_ATOM);
    term heap_growth_strategy = interop_proplist_get_value_default(opts_term, ATOMVM_HEAP_GROWTH_ATOM, BOUNDED_FREE_ATOM);

    if (min_heap_size_term != term_nil()) {
        if (UNLIKELY(!term_is_integer(min_heap_size_term))) {
            // Context was not scheduled yet, we can destroy it.
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
        }
        new_ctx->has_min_heap_size = 1;
        new_ctx->min_heap_size = term_to_int(min_heap_size_term);
    }
    if (max_heap_size_term != term_nil()) {
        if (UNLIKELY(!term_is_integer(max_heap_size_term))) {
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
        }
        new_ctx->has_max_heap_size = 1;
        new_ctx->max_heap_size = term_to_int(max_heap_size_term);
    }

    if (new_ctx->has_min_heap_size && new_ctx->has_max_heap_size) {
        if (new_ctx->min_heap_size > new_ctx->max_heap_size) {
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    switch (heap_growth_strategy) {
        case BOUNDED_FREE_ATOM:
            new_ctx->heap_growth_strategy = BoundedFreeHeapGrowth;
            break;
        case MINIMUM_ATOM:
            new_ctx->heap_growth_strategy = MinimumHeapGrowth;
            break;
        case FIBONACCI_ATOM:
            new_ctx->heap_growth_strategy = FibonacciHeapGrowth;
            break;
        default:
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
    }
    uint64_t ref_ticks = 0;

    if (link_term == TRUE_ATOM) {
        if (UNLIKELY(context_link(new_ctx, term_from_local_process_id(ctx->process_id)) < 0)) {
            context_destroy(new_ctx);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        // This is a really simple hack to get the parent - child linking
        // I don't really like how it is implemented but it works nicely.
        // I think it should be implemented adding a parent field to Context.
        if (UNLIKELY(context_link(ctx, term_from_local_process_id(new_ctx->process_id)) < 0)) {
            context_destroy(new_ctx);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }
    if (monitor_term == TRUE_ATOM) {
        ref_ticks = context_monitor(new_ctx, term_from_local_process_id(ctx->process_id));
        if (UNLIKELY(ref_ticks == 0)) {
            context_destroy(new_ctx);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }

    term new_pid = term_from_local_process_id(new_ctx->process_id);

    if (ref_ticks) {
        int res_size = REF_SIZE + TUPLE_SIZE(2);
        if (UNLIKELY(memory_ensure_free_opt(ctx, res_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            context_destroy(new_ctx);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        scheduler_init_ready(new_ctx);

        term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);

        term pid_ref_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(pid_ref_tuple, 0, new_pid);
        term_put_tuple_element(pid_ref_tuple, 1, ref);

        return pid_ref_tuple;
    } else {
        scheduler_init_ready(new_ctx);
        return new_pid;
    }
}

static term nif_erlang_spawn_fun_opt(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term fun_term = argv[0];
    term opts_term = argv[1];
    VALIDATE_VALUE(fun_term, term_is_function);
    VALIDATE_VALUE(opts_term, term_is_list);

    Context *new_ctx = context_new(ctx->global);
    new_ctx->group_leader = ctx->group_leader;

    const term *boxed_value = term_to_const_term_ptr(fun_term);

    Module *fun_module = (Module *) boxed_value[1];
    term index_or_module = boxed_value[2];
    if (term_is_atom(index_or_module)) {
        // it is not possible to spawn a function reference except for those having
        // 0 arity, however right now they are not supported.
        // TODO: implement for funs having arity 0.
        AVM_ABORT();
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
    if (UNLIKELY(memory_ensure_free_opt(new_ctx, size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        //TODO: new process should be terminated, however a new pid is returned anyway
        fprintf(stderr, "Unable to allocate sufficient memory to spawn process.\n");
        AVM_ABORT();
    }
    for (uint32_t i = 0; i < n_freeze; i++) {
        new_ctx->x[i + arity - n_freeze] = memory_copy_term_tree(&new_ctx->heap, boxed_value[i + 3]);
    }

    new_ctx->saved_module = fun_module;
    new_ctx->saved_ip = fun_module->labels[label];
    new_ctx->cp = module_address(fun_module->module_index, fun_module->end_instruction_ii);

    return do_spawn(ctx, new_ctx, opts_term);
}

static term nif_erlang_spawn_opt(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term module_term = argv[0];
    term function_term = argv[1];
    term args_term = argv[2];
    term opts_term = argv[3];
    VALIDATE_VALUE(module_term, term_is_atom);
    VALIDATE_VALUE(function_term, term_is_atom);
    VALIDATE_VALUE(args_term, term_is_list);
    VALIDATE_VALUE(opts_term, term_is_list);

    Context *new_ctx = context_new(ctx->global);
    new_ctx->group_leader = ctx->group_leader;

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
    int label = module_search_exported_function(found_module, function_string, args_len, ctx->global);
    //TODO: fail here if no function has been found
    if (UNLIKELY(label == 0)) {
        AVM_ABORT();
    }
    new_ctx->saved_module = found_module;
    new_ctx->saved_ip = found_module->labels[label];
    new_ctx->cp = module_address(found_module->module_index, found_module->end_instruction_ii);

    //TODO: check available registers count
    int reg_index = 0;

    size_t min_heap_size = 0;
    term min_heap_size_term = interop_proplist_get_value(opts_term, MIN_HEAP_SIZE_ATOM);
    if (min_heap_size_term != term_nil()) {
        if (UNLIKELY(!term_is_integer(min_heap_size_term))) {
            // Context was not scheduled yet, we can destroy it.
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
        }
        min_heap_size = term_to_int(min_heap_size_term);
        new_ctx->has_min_heap_size = true;
        new_ctx->min_heap_size = min_heap_size;
    }

    avm_int_t size = memory_estimate_usage(args_term);
    if (UNLIKELY(memory_ensure_free_opt(new_ctx, size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        // Context was not scheduled yet, we can destroy it.
        context_destroy(new_ctx);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    while (term_is_nonempty_list(args_term)) {
        new_ctx->x[reg_index] = memory_copy_term_tree(&new_ctx->heap, term_get_list_head(args_term));
        reg_index++;

        args_term = term_get_list_tail(args_term);
        if (!term_is_list(args_term)) {
            context_destroy(new_ctx);
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    return do_spawn(ctx, new_ctx, opts_term);
}

static term nif_erlang_send_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term target = argv[0];
    GlobalContext *glb = ctx->global;

    if (term_is_pid(target)) {
        int32_t local_process_id = term_to_local_process_id(target);

        globalcontext_send_message(glb, local_process_id, argv[1]);

    } else if (term_is_atom(target)) {
        // We need to hold a lock on the processes_table until the message is sent to avoid a race condition,
        // otherwise the receiving process could be killed at any point between checking it is registered,
        // obtaining its process_id, and sending the message, any of which would lead to crashes or incorrect
        // behavior that does not match OTP.
        struct ListHead *dummy = synclist_rdlock(&glb->processes_table);
        UNUSED(dummy);
        int atom_index = term_to_atom_index(target);

        int local_process_id = globalcontext_get_registered_process(glb, atom_index);
        if (UNLIKELY(local_process_id == 0)) {
            synclist_unlock(&glb->processes_table);
            RAISE_ERROR(BADARG_ATOM);
        }

        Context *p = globalcontext_get_process_nolock(glb, local_process_id);
        if (IS_NULL_PTR(p)) {
            synclist_unlock(&glb->processes_table);
            RAISE_ERROR(BADARG_ATOM);
        }

        globalcontext_send_message_nolock(glb, local_process_id, argv[1]);
        synclist_unlock(&glb->processes_table);
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    return argv[1];
}

static term nif_erlang_is_process_alive_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    int local_process_id = term_to_local_process_id(argv[0]);
    bool process_exists = globalcontext_process_exists(ctx->global, local_process_id);

    return process_exists ? TRUE_ATOM : FALSE_ATOM;
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
    if (UNLIKELY(memory_ensure_free_opt(ctx, len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
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

        term *new_list_item = term_list_alloc(&ctx->heap);

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
    if (UNLIKELY(memory_ensure_free_opt(ctx, REF_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);

    return term_from_ref_ticks(ref_ticks, &ctx->heap);
}

term nif_erlang_monotonic_time_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    struct timespec ts;
    sys_monotonic_time(&ts);

    if (argv[0] == SECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ts.tv_sec);

    } else if (argv[0] == MILLISECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ((int64_t) ts.tv_sec) * 1000UL + ts.tv_nsec / 1000000UL);

    } else if (argv[0] == MICROSECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ((int64_t) ts.tv_sec) * 1000000UL + ts.tv_nsec / 1000UL);

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

term nif_erlang_system_time_1(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    struct timespec ts;
    sys_time(&ts);

    if (argv[0] == SECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ts.tv_sec);

    } else if (argv[0] == MILLISECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ((int64_t) ts.tv_sec) * 1000UL + ts.tv_nsec / 1000000UL);

    } else if (argv[0] == MICROSECOND_ATOM) {
        return make_maybe_boxed_int64(ctx, ((int64_t) ts.tv_sec) * 1000000UL + ts.tv_nsec / 1000UL);

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term build_datetime_from_tm(Context *ctx, struct tm *broken_down_time)
{
    // 4 = size of date/time tuple, 3 size of date time tuple
    if (UNLIKELY(memory_ensure_free_opt(ctx, 3 + 4 + 4, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term date_tuple = term_alloc_tuple(3, &ctx->heap);
    term time_tuple = term_alloc_tuple(3, &ctx->heap);
    term date_time_tuple = term_alloc_tuple(2, &ctx->heap);

    term_put_tuple_element(date_tuple, 0, term_from_int32(1900 + broken_down_time->tm_year));
    term_put_tuple_element(date_tuple, 1, term_from_int32(broken_down_time->tm_mon + 1));
    term_put_tuple_element(date_tuple, 2, term_from_int32(broken_down_time->tm_mday));

    term_put_tuple_element(time_tuple, 0, term_from_int32(broken_down_time->tm_hour));
    term_put_tuple_element(time_tuple, 1, term_from_int32(broken_down_time->tm_min));
    term_put_tuple_element(time_tuple, 2, term_from_int32(broken_down_time->tm_sec));

    term_put_tuple_element(date_time_tuple, 0, date_tuple);
    term_put_tuple_element(date_time_tuple, 1, time_tuple);

    return date_time_tuple;
}

term nif_erlang_universaltime_0(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    struct timespec ts;
    sys_time(&ts);

    struct tm broken_down_time;
    return build_datetime_from_tm(ctx, gmtime_r(&ts.tv_sec, &broken_down_time));
}

term nif_erlang_localtime(Context *ctx, int argc, term argv[])
{
    char *tz;
    if (argc == 1) {
        int ok;
        tz = interop_term_to_string(argv[0], &ok);
        if (UNLIKELY(!ok)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        tz = NULL;
    }

    struct timespec ts;
    sys_time(&ts);

    struct tm storage;
    struct tm *localtime;

#ifndef AVM_NO_SMP
    smp_spinlock_lock(&ctx->global->env_spinlock);
#endif
    if (tz) {
        char *oldtz = getenv("TZ");
        setenv("TZ", tz, 1);
        tzset();
        localtime = localtime_r(&ts.tv_sec, &storage);
        if (oldtz) {
            setenv("TZ", oldtz, 1);
        } else {
            unsetenv("TZ");
        }
    } else {
        // Call tzset to handle DST changes
        tzset();
        localtime = localtime_r(&ts.tv_sec, &storage);
    }
#ifndef AVM_NO_SMP
    smp_spinlock_unlock(&ctx->global->env_spinlock);
#endif

    free(tz);
    return build_datetime_from_tm(ctx, localtime);
}

term nif_erlang_timestamp_0(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);
    UNUSED(argv);

    if (UNLIKELY(memory_ensure_free_opt(ctx, 4, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term timestamp_tuple = term_alloc_tuple(3, &ctx->heap);

    struct timespec ts;
    sys_time(&ts);

    term_put_tuple_element(timestamp_tuple, 0, term_from_int32(ts.tv_sec / 1000000));
    term_put_tuple_element(timestamp_tuple, 1, term_from_int32(ts.tv_sec % 1000000));
    term_put_tuple_element(timestamp_tuple, 2, term_from_int32(ts.tv_nsec / 1000));

    return timestamp_tuple;
}

term nif_calendar_system_time_to_universal_time_2(Context *ctx, int argc, term argv[])
{
    UNUSED(ctx);
    UNUSED(argc);

    struct timespec ts;

    avm_int64_t value = term_maybe_unbox_int64(argv[0]);

    if (argv[1] == SECOND_ATOM) {
        ts.tv_sec = (time_t) value;
        ts.tv_nsec = 0;

    } else if (argv[1] == MILLISECOND_ATOM) {
        ts.tv_sec = (time_t) (value / 1000);
        ts.tv_nsec = (value % 1000) * 1000000;

    } else if (argv[1] == MICROSECOND_ATOM) {
        ts.tv_sec = (time_t) (value / 1000000);
        ts.tv_nsec = (value % 1000000) * 1000;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct tm broken_down_time;
    return build_datetime_from_tm(ctx, gmtime_r(&ts.tv_sec, &broken_down_time));
}

static term nif_erlang_make_tuple_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);

    avm_int_t count_elem = term_to_int(argv[0]);

    if (UNLIKELY(count_elem < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, count_elem + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term new_tuple = term_alloc_tuple(count_elem, &ctx->heap);

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
    if (UNLIKELY(memory_ensure_free_opt(ctx, new_tuple_size + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term new_tuple = term_alloc_tuple(new_tuple_size, &ctx->heap);

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
    if (UNLIKELY(memory_ensure_free_opt(ctx, new_tuple_size + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term new_tuple = term_alloc_tuple(new_tuple_size, &ctx->heap);

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

    if (UNLIKELY(memory_ensure_free_opt(ctx, tuple_size + 1, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term new_tuple = term_alloc_tuple(tuple_size, &ctx->heap);

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

    if (UNLIKELY(memory_ensure_free_opt(ctx, tuple_size * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term tuple = argv[0];
    term prev = term_nil();

    for (int i = tuple_size - 1; i >= 0; i--) {
        prev = term_list_prepend(term_get_tuple_element(tuple, i), prev, &ctx->heap);
    }

    return prev;
}

static term nif_erlang_list_to_tuple_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_list);

    int proper;
    avm_int_t len = term_list_length(argv[0], &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term tuple = term_alloc_tuple(len, &ctx->heap);

    term l = argv[0];
    for (int i = 0; i < len; i++) {
        term element = term_get_list_head(l);
        term_put_tuple_element(tuple, i, element);
        l = term_get_list_tail(l);
    }

    return tuple;
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

    if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_float(fvalue, &ctx->heap);
}

static term nif_erlang_binary_to_float_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term bin_term = argv[0];
    VALIDATE_VALUE(bin_term, term_is_binary);

    const char *bin_data = term_binary_data(bin_term);
    int bin_data_size = term_binary_size(bin_term);

    return parse_float(ctx, bin_data, bin_data_size);
}

static term nif_erlang_list_to_float_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

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
}

static term nif_erlang_binary_to_list_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term value = argv[0];
    VALIDATE_VALUE(value, term_is_binary);

    int bin_size = term_binary_size(value);
    if (UNLIKELY(memory_ensure_free_opt(ctx, bin_size * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    const uint8_t *bin_data = (const uint8_t *) term_binary_data(argv[0]);

    term prev = term_nil();
    for (int i = bin_size - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(bin_data[i]), prev, &ctx->heap);
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

    const char *atom_string = term_binary_data(a_binary);
    size_t atom_string_len = term_binary_size(a_binary);
    if (UNLIKELY(atom_string_len > 255)) {
        RAISE_ERROR(SYSTEM_LIMIT_ATOM);
    }

    AtomString atom = malloc(atom_string_len + 1);
    ((uint8_t *) atom)[0] = atom_string_len;
    memcpy(((char *) atom) + 1, atom_string, atom_string_len);

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    long global_atom_index = atom_table_ensure_atom(ctx->global->atom_table, atom, atom_opts);
    free((void *) atom);
    if (UNLIKELY(global_atom_index == ATOM_TABLE_NOT_FOUND)) {
        RAISE_ERROR(BADARG_ATOM);
    } else if (UNLIKELY(global_atom_index == ATOM_TABLE_ALLOC_FAIL)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_atom_index(global_atom_index);
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
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    int atom_string_len = strlen(atom_string);
    if (UNLIKELY(atom_string_len > 255)) {
        free(atom_string);
        RAISE_ERROR(SYSTEM_LIMIT_ATOM);
    }

    AtomString atom = malloc(atom_string_len + 1);
    if (IS_NULL_PTR(atom)) {
        free(atom_string);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    ((uint8_t *) atom)[0] = atom_string_len;
    memcpy(((char *) atom) + 1, atom_string, atom_string_len);
    free(atom_string);

    enum AtomTableCopyOpt atom_opts = AtomTableCopyAtom;
    if (!create_new) {
        atom_opts |= AtomTableAlreadyExisting;
    }
    long global_atom_index = atom_table_ensure_atom(ctx->global->atom_table, atom, atom_opts);
    free((void *) atom);
    if (UNLIKELY(global_atom_index == ATOM_TABLE_NOT_FOUND)) {
        RAISE_ERROR(BADARG_ATOM);
    } else if (UNLIKELY(global_atom_index == ATOM_TABLE_ALLOC_FAIL)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_from_atom_index(global_atom_index);
}

static term nif_erlang_atom_to_binary_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term atom_term = argv[0];
    VALIDATE_VALUE(atom_term, term_is_atom);

    if (UNLIKELY(argv[1] != LATIN1_ATOM)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    GlobalContext *glb = ctx->global;

    int atom_index = term_to_atom_index(atom_term);
    size_t atom_len;
    atom_ref_t atom_ref = atom_table_get_atom_ptr_and_len(glb->atom_table, atom_index, &atom_len);

    if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(atom_len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term binary = term_create_uninitialized_binary(atom_len, &ctx->heap, glb);
    atom_table_write_bytes(glb->atom_table, atom_ref, atom_len, (char *) term_binary_data(binary));
    return binary;
}

static term nif_erlang_atom_to_list_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term atom_term = argv[0];
    VALIDATE_VALUE(atom_term, term_is_atom);

    int atom_index = term_to_atom_index(atom_term);
    size_t atom_len;

    atom_ref_t atom_ref
        = atom_table_get_atom_ptr_and_len(ctx->global->atom_table, atom_index, &atom_len);

    // TODO: use stack for smaller atoms
    char *atom_buf = malloc(atom_len);
    if (IS_NULL_PTR(atom_buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, atom_len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    atom_table_write_bytes(ctx->global->atom_table, atom_ref, atom_len, atom_buf);

    term prev = term_nil();
    for (int i = atom_len - 1; i >= 0; i--) {
        char c = atom_buf[i];
        prev = term_list_prepend(term_from_int11(c), prev, &ctx->heap);
    }

    free(atom_buf);

    return prev;
}

static size_t lltoa(avm_int64_t int_value, unsigned base, char *integer_string)
{
    int integer_string_len = 0;
    bool neg = int_value < 0;
    if (neg) {
        integer_string_len++;
        if (integer_string) {
            integer_string[0] = '-';
        }
    }
    avm_int64_t v = int_value;
    do {
        v = v / base;
        integer_string_len++;
    } while (v != 0);
    if (integer_string) {
        int ix = 1;
        do {
            avm_int_t digit = int_value % base;
            if (digit < 0) {
                digit = -digit;
            }
            if (digit < 10) {
                integer_string[integer_string_len - ix] = '0' + digit;
            } else {
                integer_string[integer_string_len - ix] = 'A' + digit - 10;
            }
            int_value = int_value / base;
            ix++;
        } while (int_value != 0);
    }
    return integer_string_len;
}

static term nif_erlang_integer_to_binary_2(Context *ctx, int argc, term argv[])
{
    term value = argv[0];
    avm_int_t base = 10;
    VALIDATE_VALUE(value, term_is_any_integer);
    if (argc > 1) {
        VALIDATE_VALUE(argv[1], term_is_integer);
        base = term_to_int(argv[1]);
        if (UNLIKELY(base < 2 || base > 36)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    avm_int64_t int_value = term_maybe_unbox_int64(value);
    size_t len = lltoa(int_value, base, NULL);

    if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_create_empty_binary(len, &ctx->heap, ctx->global);
    lltoa(int_value, base, (char *) term_binary_data(result));
    return result;
}

static term nif_erlang_integer_to_list_2(Context *ctx, int argc, term argv[])
{
    term value = argv[0];
    unsigned base = 10;
    VALIDATE_VALUE(value, term_is_any_integer);
    if (argc > 1) {
        VALIDATE_VALUE(argv[1], term_is_integer);
        base = term_to_int(argv[1]);
        if (UNLIKELY(base < 2 || base > 36)) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }

    avm_int64_t int_value = term_maybe_unbox_int64(value);
    size_t integer_string_len = lltoa(int_value, base, NULL);
    char integer_string[integer_string_len];
    lltoa(int_value, base, integer_string);

    if (UNLIKELY(memory_ensure_free_opt(ctx, integer_string_len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = integer_string_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(integer_string[i]), prev, &ctx->heap);
    }

    return prev;
}

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

static term nif_erlang_float_to_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

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

    if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_literal_binary(float_buf, len, &ctx->heap, ctx->global);
}

static term nif_erlang_float_to_list(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

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

    if (UNLIKELY(memory_ensure_free_opt(ctx, len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(float_buf[i]), prev, &ctx->heap);
    }

    return prev;
}

static term nif_erlang_list_to_binary_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term t = argv[0];
    VALIDATE_VALUE(t, term_is_list);

    size_t bin_size;
    switch (interop_iolist_size(t, &bin_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        case InteropBadArg:
            RAISE_ERROR(BADARG_ATOM);
    }

    char *bin_buf = NULL;
    bool buf_allocated = true;
    if (bin_size > 0) {
        bin_buf = malloc(bin_size);
        if (IS_NULL_PTR(bin_buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        switch (interop_write_iolist(t, bin_buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(bin_buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                free(bin_buf);
                RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        bin_buf = "";
        buf_allocated = false;
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(bin_size), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        if (buf_allocated) {
            free(bin_buf);
        }
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term bin_res = term_from_literal_binary(bin_buf, bin_size, &ctx->heap, ctx->global);

    if (buf_allocated) {
        free(bin_buf);
    }

    return bin_res;
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

    return TRUE_ATOM;
}

// process_flag/3 work on a subset of flags, target is locked.
static term nif_erlang_process_flag_3(Context *ctx, Context *target, term flag, term value)
{
#ifdef ENABLE_ADVANCED_TRACE
    if (flag == globalcontext_make_atom(ctx->global, trace_calls_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_calls = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_calls = 0;
            return OK_ATOM;
        }
    } else if (flag == globalcontext_make_atom(ctx->global, trace_call_args_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_call_args = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_call_args = 0;
            return OK_ATOM;
        }
    } else if (flag == globalcontext_make_atom(ctx->global, trace_returns_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_returns = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_returns = 0;
            return OK_ATOM;
        }
    } else if (flag == globalcontext_make_atom(ctx->global, trace_send_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_send = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_send = 0;
            return OK_ATOM;
        }
    } else if (flag == globalcontext_make_atom(ctx->global, trace_receive_atom)) {
        if (value == TRUE_ATOM) {
            target->trace_receive = 1;
            return OK_ATOM;
        } else if (value == FALSE_ATOM) {
            target->trace_receive = 0;
            return OK_ATOM;
        }
    }
#else
    UNUSED(target);
    UNUSED(flag);
    UNUSED(value);
#endif

    RAISE_ERROR(BADARG_ATOM);
}

static term nif_erlang_process_flag(Context *ctx, int argc, term argv[])
{
    term flag;
    term value;

    if (argc == 2) {
        flag = argv[0];
        value = argv[1];

        // flags that only work with process_flag/2
        switch (flag) {
            case TRAP_EXIT_ATOM: {
                term prev = ctx->trap_exit ? TRUE_ATOM : FALSE_ATOM;
                switch (value) {
                    case FALSE_ATOM:
                        ctx->trap_exit = false;
                        break;
                    case TRUE_ATOM:
                        ctx->trap_exit = true;
                        break;
                    default:
                        RAISE_ERROR(BADARG_ATOM);
                }
                return prev;
            }
        }

        // TODO: check erlang:process_flag/3 implementation
        return nif_erlang_process_flag_3(ctx, ctx, flag, value);
    } else if (argc == 3) {
        term pid = argv[0];
        flag = argv[1];
        value = argv[2];

        VALIDATE_VALUE(pid, term_is_pid);
        int local_process_id = term_to_local_process_id(pid);
        Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
        if (IS_NULL_PTR(target)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        term result = nif_erlang_process_flag_3(ctx, target, flag, value);
        globalcontext_get_process_unlock(ctx->global, target);
        return result;
    } else {
        AVM_ABORT();
    }
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

struct ContextAccumulator
{
    Context *ctx;
    term result;
};

static void *nif_cons_context(Context *ctx, void *p)
{
    struct ContextAccumulator *accum = (struct ContextAccumulator *) p;
    accum->result = term_list_prepend(term_from_local_process_id(ctx->process_id), accum->result, &accum->ctx->heap);
    return (void *) accum;
}

static void *nif_iterate_processes(GlobalContext *glb, context_iterator fun, void *accum)
{
    struct ListHead *item;
    struct ListHead *processes_table = synclist_rdlock(&glb->processes_table);
    LIST_FOR_EACH (item, processes_table) {
        Context *p = GET_LIST_ENTRY(item, Context, processes_table_head);
        accum = fun(p, accum);
    }
    synclist_unlock(&glb->processes_table);
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
    if (memory_ensure_free_opt(ctx, 2 * num_processes, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
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
    Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
    if (IS_NULL_PTR(target)) {
        return UNDEFINED_ATOM;
    }

    term ret = term_invalid_term();
    if (ctx == target) {
        if (!context_get_process_info(ctx, &ret, item)) {
            globalcontext_get_process_unlock(ctx->global, target);
            RAISE_ERROR(ret);
        }
    } else {
        // Currently, all items require a signal. We could nevertheless filter
        // items that do not exist.
        mailbox_send_built_in_atom_request_signal(target, ProcessInfoRequestSignal, ctx->process_id, item);
        context_update_flags(ctx, ~NoFlags, Trap);
    }
    globalcontext_get_process_unlock(ctx->global, target);

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
        return term_from_int32(atom_table_count(ctx->global->atom_table));
    }
    if (key == WORDSIZE_ATOM) {
        return term_from_int32(TERM_BYTES);
    }
    if (key == MACHINE_ATOM) {
        if (memory_ensure_free_opt(ctx, (sizeof("ATOM") - 1) * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return term_from_string((const uint8_t *) "ATOM", sizeof("ATOM") - 1, &ctx->heap);
    }
    if (key == AVM_FLOATSIZE_ATOM) {
        return term_from_int32(sizeof(avm_float_t));
    }
    if (key == SYSTEM_ARCHITECTURE_ATOM) {
        char buf[128];
        snprintf(buf, 128, "%s-%s-%s", SYSTEM_NAME, SYSTEM_VERSION, SYSTEM_ARCHITECTURE);
        size_t len = strnlen(buf, 128);
        if (memory_ensure_free_opt(ctx, term_binary_heap_size(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return term_from_literal_binary((const uint8_t *) buf, len, &ctx->heap, ctx->global);
    }
    if (key == ATOMVM_VERSION_ATOM) {
        return term_from_literal_binary((const uint8_t *) ATOMVM_VERSION, strlen(ATOMVM_VERSION), &ctx->heap, ctx->global);
    }
    if (key == REFC_BINARY_INFO_ATOM) {
        fprintf(stderr, "WARNING: The refc_binary_info system info tag is deprecated.  Use erlang:memory(binary) instead.\n");
        term ret = refc_binary_create_binary_info(ctx);
        if (term_is_invalid_term(ret)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return ret;
    }
    if (key == SCHEDULERS_ATOM) {
#ifndef AVM_NO_SMP
        return term_from_int32(smp_get_online_processors());
#else
        return term_from_int32(1);
#endif
    }
    if (key == SCHEDULERS_ONLINE_ATOM) {
#ifndef AVM_NO_SMP
        return term_from_int32(ctx->global->online_schedulers);
#else
        return term_from_int32(1);
#endif
    }
    return sys_get_info(ctx, key);
}

static term nif_erlang_system_flag(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    term key = argv[0];
    term value = argv[1];

    if (!term_is_atom(key)) {
        RAISE_ERROR(BADARG_ATOM);
    }

#ifndef AVM_NO_SMP
    if (key == SCHEDULERS_ONLINE_ATOM) {
        VALIDATE_VALUE(value, term_is_integer);
        int old_value = 0;
        int new_value = term_to_int(value);
        int nb_processors = smp_get_online_processors();
        if (UNLIKELY(new_value < 1) || UNLIKELY(new_value > nb_processors)) {
            argv[0] = ERROR_ATOM;
            argv[1] = BADARG_ATOM;
            return term_invalid_term();
        }
        while (!ATOMIC_COMPARE_EXCHANGE_WEAK(&ctx->global->online_schedulers, &old_value, new_value)) {};
        return term_from_int32(old_value);
    }
#else
    UNUSED(value);
#endif
    RAISE_ERROR(BADARG_ATOM);
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
    if (argc == 2
        && interop_proplist_get_value_default(argv[1], USED_ATOM, FALSE_ATOM) == TRUE_ATOM) {
        return_used = 1;
    }
    term dst = term_invalid_term();
    size_t bytes_read = 0;
    enum ExternalTermResult result = externalterm_from_binary(ctx, &dst, binary, &bytes_read);
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
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &dst, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        term ret = term_alloc_tuple(2, &ctx->heap);
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

    size_t size = term_sub_binary_heap_size(bin_term, len);
    if (UNLIKELY(memory_ensure_free_opt(ctx, size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_maybe_create_sub_binary(argv[0], pos, len, &ctx->heap, ctx->global);
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
        size_t tok_size_in_terms = term_sub_binary_heap_size(bin_term, tok_size);

        int rest_size = bin_size - offset - pattern_size;
        size_t rest_size_in_terms = term_sub_binary_heap_size(bin_term, rest_size);

        // + 4 which is the result cons
        if (UNLIKELY(memory_ensure_free_opt(ctx, tok_size_in_terms + rest_size_in_terms + 4, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        bin_term = argv[0];
        term tok = term_maybe_create_sub_binary(bin_term, 0, tok_size, &ctx->heap, ctx->global);
        term rest = term_maybe_create_sub_binary(bin_term, offset + pattern_size, rest_size, &ctx->heap, ctx->global);

        term result_list = term_list_prepend(rest, term_nil(), &ctx->heap);
        result_list = term_list_prepend(tok, result_list, &ctx->heap);

        return result_list;

    } else {
        if (UNLIKELY(memory_ensure_free_opt(ctx, 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        return term_list_prepend(argv[0], term_nil(), &ctx->heap);
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

static term nif_erlang_raise(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term ex_class = argv[0];
    if (UNLIKELY(ex_class != ERROR_ATOM && ex_class != LOWERCASE_EXIT_ATOM && ex_class != THROW_ATOM)) {
        return BADARG_ATOM;
    }
    ctx->x[0] = ex_class;
    ctx->x[1] = argv[1];
    ctx->x[2] = term_nil();
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
    snprintf(buf, 17, "<0.%" PRIu32 ".0>", term_to_local_process_id(t));

    int str_len = strnlen(buf, 17);

    if (UNLIKELY(memory_ensure_free_opt(ctx, str_len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, &ctx->heap);
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

    if (UNLIKELY(memory_ensure_free_opt(ctx, str_len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, &ctx->heap);
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

    if (UNLIKELY(memory_ensure_free_opt(ctx, str_len * 2, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term prev = term_nil();
    for (int i = str_len - 1; i >= 0; i--) {
        prev = term_list_prepend(term_from_int11(buf[i]), prev, &ctx->heap);
    }
    return prev;
}

static term nif_erlang_function_exported(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term module = argv[0];
    term function = argv[1];
    term arity_term = argv[2];

    VALIDATE_VALUE(module, term_is_atom);
    VALIDATE_VALUE(function, term_is_atom);
    VALIDATE_VALUE(arity_term, term_is_integer);

    AtomString module_name = globalcontext_atomstring_from_term(ctx->global, module);
    AtomString function_name = globalcontext_atomstring_from_term(ctx->global, function);
    avm_int_t arity = term_to_int(arity_term);

    const struct ExportedFunction *bif = bif_registry_get_handler(module_name, function_name, arity);
    if (bif) {
        return TRUE_ATOM;
    }

    struct Nif *nif = (struct Nif *) nifs_get(module_name, function_name, arity);
    if (nif) {
        return TRUE_ATOM;
    }

    Module *target_module = globalcontext_get_module(ctx->global, module_name);
    if (IS_NULL_PTR(target_module)) {
        return FALSE_ATOM;
    }

    int target_label = module_search_exported_function(target_module, function_name, arity, ctx->global);
    if (target_label == 0) {
        return FALSE_ATOM;
    }

    return TRUE_ATOM;
}

static term nif_erlang_garbage_collect(Context *ctx, int argc, term argv[])
{
    if (argc == 0) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    } else {
        // argc == 1
        term t = argv[0];
        VALIDATE_VALUE(t, term_is_pid);

        int local_id = term_to_local_process_id(t);
        Context *target = globalcontext_get_process_lock(ctx->global, local_id);

        if (IS_NULL_PTR(target)) {
            return FALSE_ATOM;
        }

        if (target == ctx) {
            globalcontext_get_process_unlock(ctx->global, target);
            if (UNLIKELY(memory_ensure_free_opt(ctx, 0, MEMORY_FORCE_SHRINK) != MEMORY_GC_OK)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
        } else {
            // We cannot garbage collect another message, yet we will return
            // true anyway.
            mailbox_send_empty_body_signal(target, GCSignal);
            globalcontext_get_process_unlock(ctx->global, target);
        }
    }

    return TRUE_ATOM;
}

static term nif_erlang_error(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term r = argv[0];

    RAISE_ERROR(r);
}

static term nif_erlang_exit(Context *ctx, int argc, term argv[])
{
    if (argc == 1) {
        term reason = argv[0];
        RAISE(LOWERCASE_EXIT_ATOM, reason);
    } else {
        term target_process = argv[0];
        VALIDATE_VALUE(target_process, term_is_pid);
        term reason = argv[1];
        GlobalContext *glb = ctx->global;
        Context *target = globalcontext_get_process_lock(glb, term_to_local_process_id(target_process));
        bool self_is_signaled = false;
        if (LIKELY(target)) {
            if (reason == KILL_ATOM) {
                mailbox_send_term_signal(target, KillSignal, KILLED_ATOM);
                self_is_signaled = target == ctx;
            } else {
                if (target->trap_exit) {
                    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(3)) != MEMORY_GC_OK)) {
                        globalcontext_get_process_unlock(glb, target);
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    term info_tuple = term_alloc_tuple(3, &ctx->heap);
                    term_put_tuple_element(info_tuple, 0, EXIT_ATOM);
                    term_put_tuple_element(info_tuple, 1, term_from_local_process_id(ctx->process_id));
                    term_put_tuple_element(info_tuple, 2, reason);
                    mailbox_send(target, info_tuple);
                } else if (ctx == target) {
                    mailbox_send_term_signal(target, KillSignal, reason);
                    self_is_signaled = target == ctx;
                } else if (reason != NORMAL_ATOM){
                    mailbox_send_term_signal(target, KillSignal, reason);
                    self_is_signaled = target == ctx;
                } // else there is no effect
            }
            globalcontext_get_process_unlock(glb, target);
        }
        if (self_is_signaled) {
            context_update_flags(ctx, ~NoFlags, Trap);
            return term_invalid_term();
        }
        return TRUE_ATOM;
    }
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

    if (UNLIKELY(memory_ensure_free_opt(ctx, FUNCTION_REFERENCE_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return term_make_function_reference(module_term, function_term, arity_term, &ctx->heap);
}

static term nif_erlang_put_2(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term old;
    DictionaryFunctionResult result = dictionary_put(&ctx->dictionary, argv[0], argv[1], &old, ctx->global);
    if (UNLIKELY(result != DictionaryOk)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return old;
}

static term nif_erlang_erase_1(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term old;
    DictionaryFunctionResult result = dictionary_erase(&ctx->dictionary, argv[0], &old, ctx->global);
    if (UNLIKELY(result != DictionaryOk)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return old;
}

static term nif_erlang_memory(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term type = argv[0];
    VALIDATE_VALUE(type, term_is_atom);

    if (type == BINARY_ATOM) {
        size_t size = refc_binary_total_size(ctx);
        size_t term_size = term_boxed_integer_size(size);
        if (UNLIKELY(memory_ensure_free_opt(ctx, term_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return term_make_maybe_boxed_int64(size, &ctx->heap);
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_erlang_monitor(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term target_pid = argv[1];

    if (argv[0] != PROCESS_ATOM && argv[0] != PORT_ATOM) {
        RAISE_ERROR(BADARG_ATOM);
    }

    VALIDATE_VALUE(target_pid, term_is_pid);

    int local_process_id = term_to_local_process_id(target_pid);
    Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
    if (IS_NULL_PTR(target)) {
        int res_size = REF_SIZE + TUPLE_SIZE(5);
        if (UNLIKELY(memory_ensure_free_opt(ctx, res_size, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
        term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
        term down_message_tuple = term_alloc_tuple(5, &ctx->heap);
        term_put_tuple_element(down_message_tuple, 0, DOWN_ATOM);
        term_put_tuple_element(down_message_tuple, 1, ref);
        term_put_tuple_element(down_message_tuple, 2, argv[0]);
        term_put_tuple_element(down_message_tuple, 3, argv[1]);
        term_put_tuple_element(down_message_tuple, 4, NOPROC_ATOM);
        mailbox_send(ctx, down_message_tuple);
        return ref;
    }

    if ((argv[0] == PROCESS_ATOM && target->native_handler != NULL) || (argv[0] == PORT_ATOM && target->native_handler == NULL)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term callee_pid = term_from_local_process_id(ctx->process_id);

    uint64_t ref_ticks = context_monitor(target, callee_pid);
    globalcontext_get_process_unlock(ctx->global, target);

    if (UNLIKELY(memory_ensure_free_opt(ctx, REF_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    return term_from_ref_ticks(ref_ticks, &ctx->heap);
}

static term nif_erlang_demonitor(Context *ctx, int argc, term argv[])
{
    term ref = argv[0];
    bool flush = false;
    bool info = false;

    if (argc == 2) {
        term options = argv[1];
        VALIDATE_VALUE(options, term_is_list);
        flush = interop_proplist_get_value_default(options, FLUSH_ATOM, FALSE_ATOM) == TRUE_ATOM;
        info = interop_proplist_get_value_default(options, INFO_ATOM, FALSE_ATOM) == TRUE_ATOM;
    }

    VALIDATE_VALUE(ref, term_is_reference);
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    bool result = globalcontext_demonitor(ctx->global, ref_ticks);
    if (flush) {
        mailbox_send_ref_signal(ctx, info ? FlushInfoMonitorSignal : FlushMonitorSignal, ref_ticks);
        context_update_flags(ctx, ~NoFlags, Trap);
        return term_invalid_term();
    }

    return !info || result ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_erlang_link(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term target_pid = argv[0];

    VALIDATE_VALUE(target_pid, term_is_pid);

    int local_process_id = term_to_local_process_id(target_pid);
    Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
    if (IS_NULL_PTR(target)) {
        RAISE_ERROR(NOPROC_ATOM);
    }

    term callee_pid = term_from_local_process_id(ctx->process_id);

    if (UNLIKELY(context_link(target, callee_pid) < 0)) {
        globalcontext_get_process_unlock(ctx->global, target);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(context_link(ctx, term_from_local_process_id(target->process_id)) < 0)) {
        context_unlink(target, callee_pid);
        globalcontext_get_process_unlock(ctx->global, target);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    globalcontext_get_process_unlock(ctx->global, target);

    return TRUE_ATOM;
}

static term nif_erlang_unlink(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term target_pid = argv[0];

    VALIDATE_VALUE(target_pid, term_is_pid);

    int local_process_id = term_to_local_process_id(target_pid);
    Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
    if (IS_NULL_PTR(target)) {
        return TRUE_ATOM;
    }

    term callee_pid = term_from_local_process_id(ctx->process_id);

    context_unlink(target, callee_pid);
    context_unlink(ctx, term_from_local_process_id(target->process_id));
    globalcontext_get_process_unlock(ctx->global, target);

    return TRUE_ATOM;
}

static term nif_erlang_group_leader(Context *ctx, int argc, term argv[])
{
    if (argc == 0) {
        // Hack: group leader is not mandatory in AtomVM, so self is returned when it is not set.
        // self PID is recognized from libraries as "no group leader set".
        if (ctx->group_leader == term_from_local_process_id(INVALID_PROCESS_ID)) {
            return term_from_local_process_id(ctx->process_id);
        } else {
            return ctx->group_leader;
        }

    } else {
        term leader = argv[0];
        term pid = argv[1];
        VALIDATE_VALUE(pid, term_is_pid);
        VALIDATE_VALUE(leader, term_is_pid);

        int local_process_id = term_to_local_process_id(pid);
        Context *target = globalcontext_get_process_lock(ctx->global, local_process_id);
        if (IS_NULL_PTR(target)) {
            RAISE_ERROR(BADARG_ATOM);
        }

        target->group_leader = leader;
        globalcontext_get_process_unlock(ctx->global, target);
        return TRUE_ATOM;
    }
}

static term nif_erlang_get_module_info(Context *ctx, int argc, term argv[])
{
    VALIDATE_VALUE(argv[0], term_is_atom);
    if (argc == 2) {
        VALIDATE_VALUE(argv[1], term_is_atom);
    }
    term module = argv[0];
    AtomString module_name = globalcontext_atomstring_from_term(ctx->global, module);
    Module *target_module = globalcontext_get_module(ctx->global, module_name);
    if (IS_NULL_PTR(target_module)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (argc == 2) {
        term key = argv[1];
        if (key == MODULE_ATOM) {
            return argv[0];
        }
        if (key == ATTRIBUTES_ATOM || key == COMPILE_ATOM) {
            return term_nil();
        }
        if (key != EXPORTS_ATOM) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }
    size_t info_size = module_get_exported_functions_list_size(target_module);
    if (argc == 1) {
        info_size += 4 * (TUPLE_SIZE(2) + CONS_SIZE);
    }
    if (UNLIKELY(memory_ensure_free(ctx, info_size) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term exports = module_get_exported_functions(target_module, &ctx->heap, ctx->global);
    if (argc == 2) {
        return exports;
    }
    term result = term_nil();

    term compile_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(compile_tuple, 0, COMPILE_ATOM);
    term_put_tuple_element(compile_tuple, 1, term_nil());
    result = term_list_prepend(compile_tuple, result, &ctx->heap);

    term attributes_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(attributes_tuple, 0, ATTRIBUTES_ATOM);
    term_put_tuple_element(attributes_tuple, 1, term_nil());
    result = term_list_prepend(attributes_tuple, result, &ctx->heap);

    term exports_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(exports_tuple, 0, EXPORTS_ATOM);
    term_put_tuple_element(exports_tuple, 1, exports);
    result = term_list_prepend(exports_tuple, result, &ctx->heap);

    term module_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(module_tuple, 0, MODULE_ATOM);
    term_put_tuple_element(module_tuple, 1, module);
    result = term_list_prepend(module_tuple, result, &ctx->heap);

    return result;
}

struct RefcBinaryAVMPack
{
    struct AVMPackData base;
    struct RefcBinary *refc;
};

static void refc_binary_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global);

static const struct AVMPackInfo refc_binary_avm_pack_info = {
    .destructor = refc_binary_avm_pack_destructor
};

static void refc_binary_avm_pack_destructor(struct AVMPackData *obj, GlobalContext *global)
{
    struct RefcBinaryAVMPack *refc_bin_avm = CONTAINER_OF(obj, struct RefcBinaryAVMPack, base);
    refc_binary_decrement_refcount(refc_bin_avm->refc, global);
    free(obj);
}

// AtomVM extension
static term nif_atomvm_add_avm_pack_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term binary = argv[0];
    if (!term_is_binary(binary)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term opts = argv[1];
    if (!term_is_list(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t bin_size = term_binary_size(binary);

    if (UNLIKELY(!avmpack_is_valid(term_binary_data(binary), bin_size))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term name = interop_kv_get_value_default(opts, ATOM_STR("\x4", "name"), UNDEFINED_ATOM, ctx->global);
    if (!term_is_atom(name)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct AVMPackData *avmpack_data = NULL;
    if (term_is_refc_binary(binary)) {
        if (!term_refc_binary_is_const(binary)) {
            struct RefcBinaryAVMPack *refc_bin_avm = malloc(sizeof(struct RefcBinaryAVMPack));
            if (IS_NULL_PTR(refc_bin_avm)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            avmpack_data_init(&refc_bin_avm->base, &refc_binary_avm_pack_info);
            refc_bin_avm->base.data = (const uint8_t *) term_binary_data(binary);
            struct RefcBinary *refc_bin = (struct RefcBinary *) term_refc_binary_ptr(binary);
            refc_binary_increment_refcount(refc_bin);
            refc_bin_avm->refc = refc_bin;

            avmpack_data = &refc_bin_avm->base;

        } else {
            struct ConstAVMPack *const_avm = malloc(sizeof(struct ConstAVMPack));
            if (IS_NULL_PTR(const_avm)) {
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            avmpack_data_init(&const_avm->base, &const_avm_pack_info);
            const_avm->base.data = (const uint8_t *) term_binary_data(binary);

            avmpack_data = &const_avm->base;
        }

    } else {
        uint8_t *allocated_data = malloc(bin_size);
        if (IS_NULL_PTR(allocated_data)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        memcpy(allocated_data, term_binary_data(binary), bin_size);

        struct InMemoryAVMPack *in_memory_avm = malloc(sizeof(struct InMemoryAVMPack));
        if (IS_NULL_PTR(in_memory_avm)) {
            free(allocated_data);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        avmpack_data_init(&in_memory_avm->base, &in_memory_avm_pack_info);
        in_memory_avm->base.data = (const uint8_t *) allocated_data;

        avmpack_data = &in_memory_avm->base;
    }

    if (name != UNDEFINED_ATOM) {
        avmpack_data->name_atom_id = term_to_atom_index(name);
    }

    synclist_append(&ctx->global->avmpack_data, &avmpack_data->avmpack_head);

    return OK_ATOM;
}

static term open_avm_error_tuple(Context *ctx, enum OpenAVMResult result)
{
    term reason = UNDEFINED_ATOM;
    switch (result) {
        case AVM_OPEN_FAILED_ALLOC:
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            break;
        case AVM_OPEN_INVALID:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "invalid_avm"));
            break;
        case AVM_OPEN_CANNOT_OPEN:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "cannot_open"));
            break;
        case AVM_OPEN_CANNOT_READ:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "cannot_read"));
            break;
        case AVM_OPEN_NOT_SUPPORTED:
            reason = globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "not_supported"));
            break;
        case AVM_OPEN_OK:
            UNREACHABLE();
    }
    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason);

    return error_tuple;
}

// AtomVM extension
static term nif_atomvm_add_avm_pack_file(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term abs_term = argv[0];

    term opts = argv[1];
    if (!term_is_list(argv[1])) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int ok;
    char *abs = interop_list_to_string(abs_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    struct AVMPackData *avmpack_data;
    enum OpenAVMResult result = sys_open_avm_from_file(ctx->global, abs, &avmpack_data);
    if (UNLIKELY(result != AVM_OPEN_OK)) {
        free(abs);
        return open_avm_error_tuple(ctx, result);
    }

    term name = interop_kv_get_value_default(opts, ATOM_STR("\x4", "name"), UNDEFINED_ATOM, ctx->global);
    if (!term_is_atom(name)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (name != UNDEFINED_ATOM) {
        avmpack_data->name_atom_id = term_to_atom_index(name);
    }
    synclist_prepend(&ctx->global->avmpack_data, &avmpack_data->avmpack_head);

    return OK_ATOM;
}

static term nif_atomvm_close_avm_pack(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term name = argv[0];
    if (UNLIKELY(!term_is_atom(name))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int name_atom_id = term_to_atom_index(name);

    struct ListHead *open_avm_packs = synclist_wrlock(&ctx->global->avmpack_data);
    struct ListHead *item;
    struct ListHead *tmp;
    bool found = false;
    MUTABLE_LIST_FOR_EACH (item, tmp, open_avm_packs) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        if (avmpack_data->name_atom_id == name_atom_id) {
            if (UNLIKELY(avmpack_data->in_use)) {
                return ERROR_ATOM;
            }
            found = true;
            list_remove(&avmpack_data->avmpack_head);
            avmpack_data_destroy(avmpack_data, ctx->global);
        }
    }
    synclist_unlock(&ctx->global->avmpack_data);

    if (UNLIKELY(!found)) {
        return ERROR_ATOM;
    }

    return OK_ATOM;
}

// AtomVM extension
static term nif_atomvm_get_start_beam(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term name = argv[0];
    if (UNLIKELY(!term_is_atom(name))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int name_atom_id = term_to_atom_index(name);

    struct ListHead *open_avm_packs = synclist_wrlock(&ctx->global->avmpack_data);
    struct ListHead *item;
    LIST_FOR_EACH (item, open_avm_packs) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        if (avmpack_data->name_atom_id == name_atom_id) {
            uint32_t size;
            const void *beam;
            const char *module_name;
            if (!avmpack_find_section_by_flag(avmpack_data->data, BEAM_START_FLAG, &beam, &size, &module_name)) {
                synclist_unlock(&ctx->global->avmpack_data);
                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                term no_start = globalcontext_make_atom(ctx->global, ATOM_STR("\xD", "no_start_beam"));
                term result = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(result, 0, ERROR_ATOM);
                term_put_tuple_element(result, 1, no_start);
                return result;
            }

            int module_name_len = strlen(module_name);
            int needed = TUPLE_SIZE(2) + term_binary_heap_size(module_name_len);
            if (UNLIKELY(memory_ensure_free(ctx, needed) != MEMORY_GC_OK)) {
                synclist_unlock(&ctx->global->avmpack_data);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term start_bin = term_from_literal_binary(module_name, module_name_len, &ctx->heap, ctx->global);
            term result = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result, 0, OK_ATOM);
            term_put_tuple_element(result, 1, start_bin);
            synclist_unlock(&ctx->global->avmpack_data);
            return result;
        }
    }
    synclist_unlock(&ctx->global->avmpack_data);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term not_found = globalcontext_make_atom(ctx->global, ATOM_STR("\x12", "avm_pack_not_found"));
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, ERROR_ATOM);
    term_put_tuple_element(result, 1, not_found);
    return result;
}

// AtomVM extension
static term nif_atomvm_read_priv(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term app_term = argv[0];
    term path_term = argv[1];
    VALIDATE_VALUE(app_term, term_is_atom);

    GlobalContext *glb = ctx->global;

    if (UNLIKELY(synclist_is_empty(&glb->avmpack_data))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    int atom_index = term_to_atom_index(app_term);
    size_t app_len;
    atom_ref_t atom_ref = atom_table_get_atom_ptr_and_len(glb->atom_table, atom_index, &app_len);
    char *app = malloc(app_len + 1);
    if (IS_NULL_PTR(app)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    atom_table_write_cstring(glb->atom_table, atom_ref, app_len + 1, app);

    int ok;
    char *path = interop_term_to_string(path_term, &ok);
    if (UNLIKELY(!ok)) {
        free(app);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(!path)) {
        free(app);
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
    term result = UNDEFINED_ATOM;
    struct ListHead *avmpack_data = synclist_rdlock(&glb->avmpack_data);
    LIST_FOR_EACH (item, avmpack_data) {
        struct AVMPackData *avmpack_data = GET_LIST_ENTRY(item, struct AVMPackData, avmpack_head);
        bool prev_in_use = avmpack_data->in_use;
        avmpack_data->in_use = true;
        if (avmpack_find_section_by_name(avmpack_data->data, complete_path, &bin_data, &size)) {
            uint32_t file_size = READ_32_ALIGNED((uint32_t *) bin_data);
            free(complete_path);
            complete_path = NULL;
            if (UNLIKELY(memory_ensure_free_opt(ctx, TERM_BOXED_REFC_BINARY_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                avmpack_data->in_use = prev_in_use;
                synclist_unlock(&glb->avmpack_data);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            result = term_from_const_binary(((uint8_t *) bin_data) + sizeof(uint32_t), file_size, &ctx->heap, ctx->global);
            break;
        } else {
            avmpack_data->in_use = prev_in_use;
        }
    }
    synclist_unlock(&glb->avmpack_data);

    free(complete_path);
    return result;
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
        size_t size;
        switch (interop_iolist_size(t, &size)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                RAISE_ERROR(BADARG_ATOM);
        }
        char *buf = malloc(size);
        if (IS_NULL_PTR(buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        switch (interop_write_iolist(t, buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                free(buf);
                RAISE_ERROR(BADARG_ATOM);
        }
        fprintf(stdout, "%.*s", (int) size, buf);
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
        switch (interop_iolist_size(src, &src_size)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                RAISE_ERROR(BADARG_ATOM);
        }
        if (src_size == 0) {
            if (return_binary) {
                if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(0), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return term_create_empty_binary(0, &ctx->heap, ctx->global);
            } else {
                return term_nil();
            }
        }
        src_buf = malloc(src_size);
        if (IS_NULL_PTR(src_buf)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        switch (interop_write_iolist(src, (char *) src_buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(src_buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
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
        term_binary_heap_size(dst_size_with_pad)
        : 2*dst_size_with_pad;
    if (UNLIKELY(memory_ensure_free_opt(ctx, heap_free, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    // src may have been invalidated by GC
    if (term_is_binary(argv[0])) {
        src_pos = (uint8_t *) term_binary_data(argv[0]);
    }
    term dst;
    uint8_t *dst_pos;
    if (return_binary) {
        dst = term_create_empty_binary(dst_size_with_pad, &ctx->heap, ctx->global);
        dst_pos = (uint8_t *) term_binary_data(dst);
    } else {
        dst = term_invalid_term();
        dst_pos = malloc(dst_size_with_pad);
        if (IS_NULL_PTR(dst_pos)) {
            free(src_buf);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
    }
    for (size_t i = 0; i < dst_size; ++i) {
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
    for (size_t i = 0; i < pad; ++i) {
        dst_pos[dst_size + i] = '=';
    }
    if (!return_binary) {
        dst = term_from_string(dst_pos, dst_size_with_pad, &ctx->heap);
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
        switch (interop_iolist_size(src, &src_size)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
                RAISE_ERROR(BADARG_ATOM);
        }
        if (src_size == 0) {
            if (return_binary) {
                if (UNLIKELY(memory_ensure_free_opt(ctx, term_binary_heap_size(0), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                return term_create_empty_binary(0, &ctx->heap, ctx->global);
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
        switch (interop_write_iolist(src, (char *) src_buf)) {
            case InteropOk:
                break;
            case InteropMemoryAllocFail:
                free(src_buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            case InteropBadArg:
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
        term_binary_heap_size(dst_size)
        : 2*dst_size;
    if (UNLIKELY(memory_ensure_free_opt(ctx, heap_free, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term dst = term_invalid_term();
    uint8_t *dst_pos, *dst_buf = NULL;
    if (return_binary) {
        dst = term_create_empty_binary(dst_size, &ctx->heap, ctx->global);
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
    for (size_t i = 0; i < n; ++i) {
        uint8_t octet = find_index(src_pos[i]);
        if (octet == NOT_FOUND) {
            free(src_buf);
            free(dst_buf);
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
        dst = term_from_string(dst_buf, dst_size, &ctx->heap);
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

static term nif_code_load_abs(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term abs_term = argv[0];

    int ok;
    char *abs = interop_list_to_string(abs_term, &ok);
    if (UNLIKELY(!ok)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    char *path = malloc(strlen(abs) + strlen(".beam") + 1);
    if (IS_NULL_PTR(path)) {
        free(abs);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    strcpy(path, abs);
    strcat(path, ".beam");

    Module *new_module = sys_load_module_from_file(ctx->global, path);
    free(abs);
    free(path);
    if (IS_NULL_PTR(new_module)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(globalcontext_insert_module(ctx->global, new_module) < 0)) {
        return ERROR_ATOM;
    }

    term module_name = module_get_atom_term_by_id(new_module, 1);

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, MODULE_ATOM);
    term_put_tuple_element(result, 1, module_name);

    return result;
}

static term nif_code_load_binary(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    term module_name = argv[0];
    if (UNLIKELY(!term_is_atom(module_name))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    term file_name = argv[1];
    UNUSED(file_name);

    term binary = argv[2];
    if (UNLIKELY(!term_is_binary(binary))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    const uint8_t *data;
    size_t bin_size = term_binary_size(binary);
    if (term_is_refc_binary(binary)) {
        if (!term_refc_binary_is_const(binary)) {
            // TODO: track this and decrement when we free the Module
            refc_binary_increment_refcount((struct RefcBinary *) term_refc_binary_ptr(binary));
        }
        data = (const uint8_t *) term_binary_data(binary);
    } else {
        uint8_t *allocated_data = malloc(bin_size);
        if (IS_NULL_PTR(allocated_data)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        memcpy(allocated_data, term_binary_data(binary), bin_size);
        data = allocated_data;
    }

    Module *new_module = module_new_from_iff_binary(ctx->global, data, bin_size);
    if (IS_NULL_PTR(new_module)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    new_module->module_platform_data = NULL;

    if (UNLIKELY(globalcontext_insert_module(ctx->global, new_module) < 0)) {
        return ERROR_ATOM;
    }

    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, MODULE_ATOM);
    term_put_tuple_element(result, 1, module_name);

    return result;
}

static term nif_lists_reverse(Context *ctx, int argc, term argv[])
{
    // Compared to erlang version, compute the length of the list and allocate
    // at once the space for the reverse.
    int proper;
    size_t len = term_list_length(argv[0], &proper);
    if (UNLIKELY(!proper)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, len * CONS_SIZE, 2, argv, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_nil();
    if (argc == 2) {
        result = argv[1];
    }
    term list_crsr = argv[0];
    while (!term_is_nil(list_crsr)) {
        // term is a proper list as verified above
        term *list_ptr = term_get_list_ptr(list_crsr);
        result = term_list_prepend(list_ptr[LIST_HEAD_INDEX], result, &ctx->heap);
        list_crsr = list_ptr[LIST_TAIL_INDEX];
    }
    return result;
}

static term nif_maps_next(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    if (argv[0] == NONE_ATOM) {
        return NONE_ATOM;
    }

    term iterator = argv[0];
    VALIDATE_VALUE(iterator, term_is_nonempty_list);

    term post = term_get_list_head(iterator);
    VALIDATE_VALUE(post, term_is_integer);

    term map = term_get_list_tail(iterator);
    VALIDATE_VALUE(map, term_is_map);

    int size = term_get_map_size(map);
    int pos = term_to_int(post);
    if (pos >= size) {
        return NONE_ATOM;
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, 6, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    // recompute all the terms we need (after possible GC)
    iterator = argv[0];
    map = term_get_list_tail(iterator);
    term key = term_get_map_key(map, pos);
    term value = term_get_map_value(map, pos);

    term next_iterator = term_list_prepend(term_from_int(pos + 1), map, &ctx->heap);
    term ret = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(ret, 0, key);
    term_put_tuple_element(ret, 1, value);
    term_put_tuple_element(ret, 2, next_iterator);

    return ret;
}

static term nif_unicode_characters_to_list(Context *ctx, int argc, term argv[])
{
    enum CharDataEncoding in_encoding = UTF8Encoding;
    if (argc == 2) {
        if (argv[1] == LATIN1_ATOM) {
            in_encoding = Latin1Encoding;
        } else if (UNLIKELY((argv[1] != UTF8_ATOM))) {
            RAISE_ERROR(BADARG_ATOM);
        }
    }
    size_t size;
    size_t rest_size;
    enum UnicodeConversionResult conv_result = interop_chardata_to_bytes_size(argv[0], &size, &rest_size, in_encoding, UCS4NativeEncoding);
    if (UNLIKELY(conv_result == UnicodeMemoryAllocFail)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (UNLIKELY(conv_result == UnicodeBadArg)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t len = size / sizeof(uint32_t);
    uint32_t *chars = malloc(size);
    if (IS_NULL_PTR(chars)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    size_t needed_terms = CONS_SIZE * len;
    if (UNLIKELY(conv_result == UnicodeError || conv_result == UnicodeIncompleteTransform)) {
        needed_terms += rest_size + TUPLE_SIZE(3);
    }
    if (UNLIKELY(conv_result == UnicodeBadArg)) {
        free(chars);
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, needed_terms) != MEMORY_GC_OK)) {
        free(chars);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term rest;
    conv_result = interop_chardata_to_bytes(argv[0], (uint8_t *) chars, &rest, in_encoding, UCS4NativeEncoding, &ctx->heap);
    if (UNLIKELY(conv_result == UnicodeMemoryAllocFail)) {
        free(chars);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_nil();
    uint32_t *crsr = chars + len - 1;
    for (size_t index_list = len; index_list > 0; index_list--) {
        result = term_list_prepend(term_from_int(*crsr--), result, &ctx->heap);
    }
    free(chars);
    if (LIKELY(conv_result == UnicodeOk)) {
        return result;
    }
    term result_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, conv_result == UnicodeError ? ERROR_ATOM : INCOMPLETE_ATOM);
    term_put_tuple_element(result_tuple, 1, result);
    term_put_tuple_element(result_tuple, 2, rest);
    return result_tuple;
}

static term nif_unicode_characters_to_binary(Context *ctx, int argc, term argv[])
{
    enum CharDataEncoding in_encoding = UTF8Encoding;
    enum CharDataEncoding out_encoding = UTF8Encoding;
    if (argc > 1) {
        if (argv[1] == LATIN1_ATOM) {
            in_encoding = Latin1Encoding;
        } else if (UNLIKELY((argv[1] != UTF8_ATOM))) {
            RAISE_ERROR(BADARG_ATOM);
        }
        if (argc == 3) {
            if (argv[2] == LATIN1_ATOM) {
                out_encoding = Latin1Encoding;
            } else if (UNLIKELY((argv[2] != UTF8_ATOM))) {
                RAISE_ERROR(BADARG_ATOM);
            }
        }
    }
    size_t len;
    size_t rest_size;
    enum UnicodeConversionResult conv_result = interop_chardata_to_bytes_size(argv[0], &len, &rest_size, in_encoding, out_encoding);
    if (UNLIKELY(conv_result == UnicodeMemoryAllocFail)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (UNLIKELY(conv_result == UnicodeBadArg)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    size_t needed_terms = term_binary_data_size_in_terms(len);
    if (UNLIKELY(conv_result == UnicodeError || conv_result == UnicodeIncompleteTransform)) {
        needed_terms += TUPLE_SIZE(3) + rest_size;
    }
    if (UNLIKELY(conv_result == UnicodeBadArg)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (UNLIKELY(memory_ensure_free(ctx, needed_terms) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_create_uninitialized_binary(len, &ctx->heap, ctx->global);
    uint8_t *binary_data = (uint8_t *) term_binary_data(result);
    term rest;
    conv_result = interop_chardata_to_bytes(argv[0], binary_data, &rest, in_encoding, out_encoding, &ctx->heap);
    if (UNLIKELY(conv_result == UnicodeMemoryAllocFail)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    if (LIKELY(conv_result == UnicodeOk)) {
        return result;
    }
    term result_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(result_tuple, 0, conv_result == UnicodeError ? ERROR_ATOM : INCOMPLETE_ATOM);
    term_put_tuple_element(result_tuple, 1, result);
    term_put_tuple_element(result_tuple, 2, rest);
    return result_tuple;
}

//
// MAINTENANCE NOTE: Exception handling for fp operations using math
// error handling is designed to be thread-safe, as errors are specified
// to be stored as thread-local data.  The maybe_clear_exception and
// clear_exception functions must ONLY be called within the execution
// extent of a single Nif call, in order to guarantee thread-safety.
//

typedef avm_float_t (*unary_math_f)(avm_float_t x);
typedef avm_float_t (*binary_math_f)(avm_float_t x, avm_float_t y);

static void maybe_clear_exceptions()
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    feclearexcept(FE_DIVBYZERO | FE_INVALID);
#endif
}

static term get_exception(avm_float_t f)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    #pragma STDC FENV_ACCESS ON
    UNUSED(f)
    if (fetestexcept(FE_DIVBYZERO | FE_INVALID)) {
        return BADARITH_ATOM;
    } else {
        return OK_ATOM;
    }
#else
    return !isfinite(f) ? BADARITH_ATOM : OK_ATOM;
#endif
}

static term math_unary_op(Context *ctx, term x_term, unary_math_f f)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    #pragma STDC FENV_ACCESS ON
#endif
    avm_float_t x = term_conv_to_float(x_term);
    maybe_clear_exceptions();
    avm_float_t y = f(x);
    term exception = get_exception(y);
    if (exception != OK_ATOM) {
        return exception;
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return term_from_float(y, &ctx->heap);
}

static term math_binary_op(Context *ctx, term x_term, term y_term, binary_math_f f)
{
#ifdef HAVE_PRAGMA_STDC_FENV_ACCESS
    #pragma STDC FENV_ACCESS ON
#endif
    avm_float_t x = term_conv_to_float(x_term);
    avm_float_t y = term_conv_to_float(y_term);
    maybe_clear_exceptions();
    avm_float_t z = f(x, y);
    term exception = get_exception(z);
    if (exception != OK_ATOM) {
        return exception;
    }

    if (UNLIKELY(memory_ensure_free_opt(ctx, FLOAT_SIZE, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        return OUT_OF_MEMORY_ATOM;
    }
    return term_from_float(z, &ctx->heap);
}

#define DEFINE_UNARY_MATH_OP(moniker)                                   \
    static avm_float_t math_##moniker(avm_float_t x)                    \
    {                                                                   \
        return moniker(x);                                              \
    }                                                                   \
                                                                        \
    static term nif_math_##moniker(Context *ctx, int argc, term argv[]) \
    {                                                                   \
        UNUSED(argc);                                                   \
        VALIDATE_VALUE(argv[0], term_is_number);                        \
        term t = math_unary_op(ctx, argv[0], math_##moniker);           \
        if (term_is_atom(t)) {                                          \
            RAISE_ERROR(t);                                             \
        } else {                                                        \
            return t;                                                   \
        }                                                               \
    }

#define DEFINE_BINARY_MATH_OP(moniker)                                  \
    static avm_float_t math_##moniker(avm_float_t x, avm_float_t y)     \
    {                                                                   \
        return moniker(x, y);                                           \
    }                                                                   \
                                                                        \
    static term nif_math_##moniker(Context *ctx, int argc, term argv[]) \
    {                                                                   \
        UNUSED(argc);                                                   \
        VALIDATE_VALUE(argv[0], term_is_number);                        \
        VALIDATE_VALUE(argv[1], term_is_number);                        \
        term t = math_binary_op(ctx, argv[0], argv[1], math_##moniker); \
        if (term_is_atom(t)) {                                          \
            RAISE_ERROR(t);                                             \
        } else {                                                        \
            return t;                                                   \
        }                                                               \
    }

DEFINE_UNARY_MATH_OP(cos)
DEFINE_UNARY_MATH_OP(acos)
DEFINE_UNARY_MATH_OP(acosh)
DEFINE_UNARY_MATH_OP(asin)
DEFINE_UNARY_MATH_OP(asinh)
DEFINE_UNARY_MATH_OP(atan)
DEFINE_BINARY_MATH_OP(atan2)
DEFINE_UNARY_MATH_OP(atanh)
DEFINE_UNARY_MATH_OP(ceil)
DEFINE_UNARY_MATH_OP(cosh)
DEFINE_UNARY_MATH_OP(exp)
DEFINE_UNARY_MATH_OP(floor)
DEFINE_BINARY_MATH_OP(fmod)
DEFINE_UNARY_MATH_OP(log)
DEFINE_UNARY_MATH_OP(log10)
DEFINE_UNARY_MATH_OP(log2)
DEFINE_BINARY_MATH_OP(pow)
DEFINE_UNARY_MATH_OP(sin)
DEFINE_UNARY_MATH_OP(sinh)
DEFINE_UNARY_MATH_OP(sqrt)
DEFINE_UNARY_MATH_OP(tan)
DEFINE_UNARY_MATH_OP(tanh)
