/*
 * This file is part of AtomVM.
 *
 * Copyright 2019 Davide Bettio <davide@uninstall.it>
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

#include "defaultatoms.h"
#include <stdio.h>

static const char *const false_atom = "\x05" "false";
static const char *const true_atom = "\x04" "true";

static const char *const ok_atom = "\x2" "ok";
static const char *const error_atom = "\x5" "error";

static const char *const undefined_atom = "\x9" "undefined";

static const char *const badarg_atom = "\x6" "badarg";
static const char *const badarith_atom = "\x08" "badarith";
static const char *const badarity_atom = "\x08" "badarity";
static const char *const badfun_atom = "\x06" "badfun";
static const char *const system_limit_atom = "\xC" "system_limit";
static const char *const function_clause_atom = "\x0F" "function_clause";
static const char *const try_clause_atom = "\xA" "try_clause";
static const char *const out_of_memory_atom = "\xD" "out_of_memory";
static const char *const overflow_atom = "\x8" "overflow";

static const char *const flush_atom = "\x5" "flush";
static const char *const heap_size_atom  = "\x9" "heap_size";
static const char *const latin1_atom = "\x6" "latin1";
static const char *const max_heap_size_atom ="\xD" "max_heap_size";
static const char *const memory_atom = "\x6" "memory";
static const char *const message_queue_len_atom = "\x11" "message_queue_len";
static const char *const puts_atom = "\x4" "puts";
static const char *const stack_size_atom = "\xA" "stack_size";
static const char *const min_heap_size_atom ="\xD" "min_heap_size";
static const char *const process_count_atom = "\xD" "process_count";
static const char *const port_count_atom = "\xA" "port_count";
static const char *const atom_count_atom = "\xA" "atom_count";
static const char *const system_architecture_atom = "\x13" "system_architecture";
static const char *const wordsize_atom = "\x8" "wordsize";

static const char *const decimals_atom = "\x8" "decimals";
static const char *const scientific_atom = "\xA" "scientific";
static const char *const compact_atom = "\x7" "compact";

static const char *const badmatch_atom = "\x8" "badmatch";
static const char *const case_clause_atom = "\xB" "case_clause";
static const char *const if_clause_atom = "\x9" "if_clause";
static const char *const throw_atom = "\x5" "throw";
static const char *const low_entropy_atom = "\xB" "low_entropy";
static const char *const unsupported_atom = "\xB" "unsupported";
static const char *const used_atom = "\x4" "used";
static const char *const all_atom = "\x3" "all";
static const char *const start_atom = "\x5" "start";

static const char *const undef_atom = "\x5" "undef";
static const char *const vm_abort_atom = "\x8" "vm_abort";

static const char *const link_atom = "\x4" "link";
static const char *const monitor_atom = "\x7" "monitor";
static const char *const normal_atom = "\x6" "normal";
static const char *const down_atom = "\x4" "DOWN";
static const char *const process_atom = "\x7" "process";
static const char *const nocatch_atom = "\x7" "nocatch";
static const char *const refc_binary_info_atom = "\x10" "refc_binary_info";
static const char *const noproc_atom = "\x6" "noproc";
static const char *const trap_exit_atom = "\x9" "trap_exit";
static const char *const exit_atom = "\x4" "EXIT";

static const char *const badmap_atom = "\x6" "badmap";
static const char *const badkey_atom = "\x6" "badkey";
static const char *const none_atom = "\x4" "none";

static const char *const io_request_atom = "\xA" "io_request";
static const char *const io_reply_atom = "\x8" "io_reply";
static const char *const put_chars_atom = "\x9" "put_chars";

static const char *const lowercase_exit_atom = "\x4" "exit";
static const char *const atomvm_version_atom = "\xE" "atomvm_version";

static const char *const second_atom = "\x6" "second";
static const char *const millisecond_atom = "\xB" "millisecond";
static const char *const microsecond_atom = "\xB" "microsecond";

static const char *const infinity_atom = "\x8" "infinity";
static const char *const timeout_value_atom = "\xD" "timeout_value";

static const char *const schedulers_atom = "\xA" "schedulers";
static const char *const schedulers_online_atom = "\x11" "schedulers_online";

static const char *const append_atom = "\x6" "append";
static const char *const private_append_atom = "\xE" "private_append";
static const char *const binary_atom = "\x6" "binary";
static const char *const integer_atom = "\x7" "integer";
static const char *const little_atom = "\x6" "little";
static const char *const native_atom = "\x6" "native";
static const char *const string_atom = "\x6" "string";
static const char *const utf8_atom = "\x4" "utf8";
static const char *const utf16_atom = "\x5" "utf16";
static const char *const utf32_atom = "\x5" "utf32";
static const char *const badrecord_atom = "\x9" "badrecord";

static const char *const copy_atom = "\x4" "copy";
static const char *const reuse_atom = "\x5" "reuse";
static const char *const ensure_at_least_atom = "\xF" "ensure_at_least";
static const char *const ensure_exactly_atom = "\xE" "ensure_exactly";
static const char *const skip_atom = "\x4" "skip";
static const char *const get_tail_atom = "\x8" "get_tail";
static const char *const equal_colon_equal_atom = "\x3" "=:=";
static const char *const signed_atom = "\x6" "signed";

static const char *const machine_atom = "\x7" "machine";
static const char *const avm_floatsize_atom = "\xD" "avm_floatsize";

static const char *const close_atom = "\x5" "close";
static const char *const closed_atom = "\x6" "closed";
static const char *const port_atom = "\x4" "port";

static const char *const info_atom = "\x4" "info";

static const char *const module_atom = "\x06" "module";

static const char *const select_atom = "\x6" "select";
static const char *const ready_input_atom = "\xB" "ready_input";
static const char *const ready_output_atom = "\xC" "ready_output";

static const char *const attributes_atom = "\xA" "attributes";
static const char *const compile_atom = "\x7" "compile";
static const char *const exports_atom = "\x7" "exports";

static const char *const incomplete_atom = "\xA" "incomplete";

static const char *const kill_atom = "\x4" "kill";
static const char *const killed_atom = "\x6" "killed";
static const char *const links_atom = "\x5" "links";

static const char *const total_heap_size_atom = "\xF" "total_heap_size";
static const char *const atomvm_heap_growth_atom = "\x12" "atomvm_heap_growth";
static const char *const bounded_free_atom = "\xC" "bounded_free";
static const char *const minimum_atom = "\x7" "minimum";
static const char *const fibonacci_atom = "\x9" "fibonacci";

static const char *const call_atom = "\x5" "$call";
static const char *const cast_atom = "\x5" "$cast";

static const char *const unicode_atom = "\x7" "unicode";

static const char *const global_atom = "\x6" "global";
static const char *const type_atom = "\x4" "type";
static const char *const name_atom = "\x4" "name";
static const char *const arity_atom = "\x5" "arity";
static const char *const external_atom = "\x8" "external";
static const char *const local_atom = "\x5" "local";

void defaultatoms_init(GlobalContext *glb)
{
    int ok = 1;

    ok &= globalcontext_insert_atom(glb, false_atom) == FALSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, true_atom) == TRUE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, ok_atom) == OK_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, error_atom) == ERROR_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, undefined_atom) == UNDEFINED_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, badarg_atom) == BADARG_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badarith_atom) == BADARITH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badarity_atom) == BADARITY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badfun_atom) == BADFUN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, function_clause_atom) == FUNCTION_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, try_clause_atom) == TRY_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, out_of_memory_atom) == OUT_OF_MEMORY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, overflow_atom) == OVERFLOW_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, system_limit_atom) == SYSTEM_LIMIT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, flush_atom) == FLUSH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, heap_size_atom) == HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, latin1_atom) == LATIN1_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, max_heap_size_atom) == MAX_HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, memory_atom) == MEMORY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, message_queue_len_atom) == MESSAGE_QUEUE_LEN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, puts_atom) == PUTS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, stack_size_atom) == STACK_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, min_heap_size_atom) == MIN_HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, process_count_atom) == PROCESS_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, port_count_atom) == PORT_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, atom_count_atom) == ATOM_COUNT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, system_architecture_atom) == SYSTEM_ARCHITECTURE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, wordsize_atom) == WORDSIZE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, decimals_atom) == DECIMALS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, scientific_atom) == SCIENTIFIC_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, compact_atom) == COMPACT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, badmatch_atom) == BADMATCH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, case_clause_atom) == CASE_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, if_clause_atom) == IF_CLAUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, throw_atom) == THROW_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, low_entropy_atom) == LOW_ENTROPY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, unsupported_atom) == UNSUPPORTED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, used_atom) == USED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, all_atom) == ALL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, start_atom) == START_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, undef_atom) == UNDEF_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, vm_abort_atom) == VM_ABORT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, link_atom) == LINK_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, monitor_atom) == MONITOR_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, normal_atom) == NORMAL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, down_atom) == DOWN_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, process_atom) == PROCESS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, nocatch_atom) == NOCATCH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, refc_binary_info_atom) == REFC_BINARY_INFO_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, noproc_atom) == NOPROC_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, trap_exit_atom) == TRAP_EXIT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, exit_atom) == EXIT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, badmap_atom) == BADMAP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badkey_atom) == BADKEY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, none_atom) == NONE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, io_request_atom) == IO_REQUEST_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, io_reply_atom) == IO_REPLY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, put_chars_atom) == PUT_CHARS_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, lowercase_exit_atom) == LOWERCASE_EXIT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, atomvm_version_atom) == ATOMVM_VERSION_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, second_atom) == SECOND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, millisecond_atom) == MILLISECOND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, microsecond_atom) == MICROSECOND_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, infinity_atom) == INFINITY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, timeout_value_atom) == TIMEOUT_VALUE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, schedulers_atom) == SCHEDULERS_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, schedulers_online_atom) == SCHEDULERS_ONLINE_ATOM_INDEX;
    
    ok &= globalcontext_insert_atom(glb, append_atom) == APPEND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, private_append_atom) == PRIVATE_APPEND_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, binary_atom) == BINARY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, integer_atom) == INTEGER_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, little_atom) == LITTLE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, native_atom) == NATIVE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, string_atom) == STRING_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, utf8_atom) == UTF8_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, utf16_atom) == UTF16_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, utf32_atom) == UTF32_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, badrecord_atom) == BADRECORD_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, copy_atom) == COPY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, reuse_atom) == REUSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ensure_at_least_atom) == ENSURE_AT_LEAST_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ensure_exactly_atom) == ENSURE_EXACTLY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, skip_atom) == SKIP_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, get_tail_atom) == GET_TAIL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, equal_colon_equal_atom) == EQUAL_COLON_EQUAL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, signed_atom) == SIGNED_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, machine_atom) == MACHINE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, avm_floatsize_atom) == AVM_FLOATSIZE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, close_atom) == CLOSE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, closed_atom) == CLOSED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, port_atom) == PORT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, info_atom) == INFO_ATOM_INDEX;
    
    ok &= globalcontext_insert_atom(glb, module_atom) == MODULE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, select_atom) == SELECT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ready_input_atom) == READY_INPUT_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, ready_output_atom) == READY_OUTPUT_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, attributes_atom) == ATTRIBUTES_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, compile_atom) == COMPILE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, exports_atom) == EXPORTS_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, incomplete_atom) == INCOMPLETE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, kill_atom) == KILL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, killed_atom) == KILLED_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, links_atom) == LINKS_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, total_heap_size_atom) == TOTAL_HEAP_SIZE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, atomvm_heap_growth_atom) == ATOMVM_HEAP_GROWTH_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, bounded_free_atom) == BOUNDED_FREE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, minimum_atom) == MINIMUM_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, fibonacci_atom) == FIBONACCI_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, call_atom) == CALL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, cast_atom) == CAST_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, unicode_atom) == UNICODE_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, global_atom) == GLOBAL_ATOM_INDEX;

    ok &= globalcontext_insert_atom(glb, type_atom) == TYPE_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, name_atom) == NAME_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, arity_atom) == ARITY_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, external_atom) == EXTERNAL_ATOM_INDEX;
    ok &= globalcontext_insert_atom(glb, local_atom) == LOCAL_ATOM_INDEX;

    if (!ok) {
        AVM_ABORT();
    }

    platform_defaultatoms_init(glb);
}
