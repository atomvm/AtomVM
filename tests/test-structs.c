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

#include <assert.h>
#include <stdlib.h>

#include "atom_table.h"
#include "utils.h"
#include "valueshashtable.h"

// clang-format off
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
static const char *const heap_size_atom = "\x9" "heap_size";
static const char *const latin1_atom = "\x6" "latin1";
static const char *const max_heap_size_atom = "\xD" "max_heap_size";
static const char *const memory_atom = "\x6" "memory";
static const char *const message_queue_len_atom = "\x11" "message_queue_len";
static const char *const puts_atom = "\x4" "puts";
static const char *const stack_size_atom = "\xA" "stack_size";
static const char *const min_heap_size_atom = "\xD" "min_heap_size";
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
// clang-format on

void test_valueshashtable(void)
{
    struct ValuesHashTable *htable = valueshashtable_new();
    assert(valueshashtable_insert(htable, 0xABCDEF01, 0x12345678) == 1);
    assert(valueshashtable_get_value(htable, 0xBBCDEF01, 0xCAFEBABE) == 0xCAFEBABE);
    assert(valueshashtable_get_value(htable, 0xABCDEF01, 0xCAFEBABE) == 0x12345678);

    assert(valueshashtable_insert(htable, 0xBBCDEF01, 0x11223344) == 1);
    assert(valueshashtable_get_value(htable, 0xBBCDEF01, 0xCAFEBABE) == 0x11223344);
    assert(valueshashtable_get_value(htable, 0xABCDEF01, 0xCAFEBABE) == 0x12345678);

    for (unsigned long i = 0; i < 2000; i++) {
        assert(valueshashtable_insert(htable, 0xBBDDBBDD + i, 0xEEFFEEFF + i) == 1);
    }

    for (unsigned long i = 0; i < 2000; i++) {
        assert(valueshashtable_get_value(htable, 0xBBDDBBDD + i, 0xCAFEBABE) == 0xEEFFEEFFL + i);
        assert(valueshashtable_get_value(htable, 0xABDDBBDD + i, 0xCAFEBABE) == 0xCAFEBABE);
    }
}

atom_index_t insert_atoms_into_atom_table(struct AtomTable *table)
{
    atom_index_t global_atom_index;
    atom_index_t decimals_index;
    enum AtomTableEnsureAtomResult r;

    r = atom_table_ensure_atom(table, (const uint8_t *) false_atom + 1, false_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) true_atom + 1, true_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) ok_atom + 1, ok_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) error_atom + 1, error_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) undefined_atom + 1, undefined_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badarg_atom + 1, badarg_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badarith_atom + 1, badarith_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badarity_atom + 1, badarity_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badfun_atom + 1, badfun_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) function_clause_atom + 1, function_clause_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) try_clause_atom + 1, try_clause_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) out_of_memory_atom + 1, out_of_memory_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) overflow_atom + 1, overflow_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) system_limit_atom + 1, system_limit_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) flush_atom + 1, flush_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) heap_size_atom + 1, heap_size_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) latin1_atom + 1, latin1_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) max_heap_size_atom + 1, max_heap_size_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) memory_atom + 1, memory_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) message_queue_len_atom + 1, message_queue_len_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) puts_atom + 1, puts_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) stack_size_atom + 1, stack_size_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) min_heap_size_atom + 1, min_heap_size_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) process_count_atom + 1, process_count_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) port_count_atom + 1, port_count_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) atom_count_atom + 1, atom_count_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) system_architecture_atom + 1, system_architecture_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) wordsize_atom + 1, wordsize_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) decimals_atom + 1, decimals_atom[0], AtomTableNoOpts, &decimals_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) scientific_atom + 1, scientific_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) compact_atom + 1, compact_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badmatch_atom + 1, badmatch_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) case_clause_atom + 1, case_clause_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) if_clause_atom + 1, if_clause_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) throw_atom + 1, throw_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) low_entropy_atom + 1, low_entropy_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) unsupported_atom + 1, unsupported_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) used_atom + 1, used_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) all_atom + 1, all_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) start_atom + 1, start_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) undef_atom + 1, undef_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) vm_abort_atom + 1, vm_abort_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) link_atom + 1, link_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) monitor_atom + 1, monitor_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) normal_atom + 1, normal_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) down_atom + 1, down_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) process_atom + 1, process_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) nocatch_atom + 1, nocatch_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) refc_binary_info_atom + 1, refc_binary_info_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) noproc_atom + 1, noproc_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) trap_exit_atom + 1, trap_exit_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) exit_atom + 1, exit_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badmap_atom + 1, badmap_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badkey_atom + 1, badkey_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) none_atom + 1, none_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) io_request_atom + 1, io_request_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) io_reply_atom + 1, io_reply_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) put_chars_atom + 1, put_chars_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) lowercase_exit_atom + 1, lowercase_exit_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) atomvm_version_atom + 1, atomvm_version_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) second_atom + 1, second_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) millisecond_atom + 1, millisecond_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) microsecond_atom + 1, microsecond_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) infinity_atom + 1, infinity_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) timeout_value_atom + 1, timeout_value_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) schedulers_atom + 1, schedulers_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) schedulers_online_atom + 1, schedulers_online_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) append_atom + 1, append_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) private_append_atom + 1, private_append_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) binary_atom + 1, binary_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) integer_atom + 1, integer_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) little_atom + 1, little_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) native_atom + 1, native_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) string_atom + 1, string_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) utf8_atom + 1, utf8_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) utf16_atom + 1, utf16_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) utf32_atom + 1, utf32_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) badrecord_atom + 1, badrecord_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) copy_atom + 1, copy_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) reuse_atom + 1, reuse_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) ensure_at_least_atom + 1, ensure_at_least_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) ensure_exactly_atom + 1, ensure_exactly_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) skip_atom + 1, skip_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) get_tail_atom + 1, get_tail_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) equal_colon_equal_atom + 1, equal_colon_equal_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) signed_atom + 1, signed_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) machine_atom + 1, machine_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) avm_floatsize_atom + 1, avm_floatsize_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) close_atom + 1, close_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) closed_atom + 1, closed_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) port_atom + 1, port_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) info_atom + 1, info_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) module_atom + 1, module_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) select_atom + 1, select_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) ready_input_atom + 1, ready_input_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) ready_output_atom + 1, ready_output_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) attributes_atom + 1, attributes_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) compile_atom + 1, compile_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) exports_atom + 1, exports_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) incomplete_atom + 1, incomplete_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) kill_atom + 1, kill_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) killed_atom + 1, killed_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) links_atom + 1, links_atom[0], AtomTableNoOpts, &global_atom_index);

    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) total_heap_size_atom + 1, total_heap_size_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) atomvm_heap_growth_atom + 1, atomvm_heap_growth_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) bounded_free_atom + 1, bounded_free_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) minimum_atom + 1, minimum_atom[0], AtomTableNoOpts, &global_atom_index);
    assert(r == AtomTableEnsureAtomOk);
    r = atom_table_ensure_atom(table, (const uint8_t *) fibonacci_atom + 1, fibonacci_atom[0], AtomTableNoOpts, &global_atom_index);

    return decimals_index;
}

void test_atom_table(void)
{
    struct AtomTable *table = atom_table_new();

    enum AtomTableEnsureAtomResult r;
    atom_index_t new_index;
    r = atom_table_ensure_atom(table, (const uint8_t *) "ciao", 4, AtomTableAlreadyExisting, &new_index);
    assert(r == AtomTableEnsureAtomNotFound);

    r = atom_table_ensure_atom(table, (const uint8_t *) "bar", 3, AtomTableNoOpts, &new_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(new_index == 0);

    atom_index_t ciao_index;
    r = atom_table_ensure_atom(table, (const uint8_t *) "ciao", 4, AtomTableNoOpts, &ciao_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(ciao_index == 1);

    atom_index_t foo_index;
    r = atom_table_ensure_atom(table, (const uint8_t *) "foo", 3, AtomTableNoOpts, &foo_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(foo_index == 2);

    r = atom_table_ensure_atom(table, (const uint8_t *) "foo", 3, AtomTableNoOpts, &foo_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(foo_index == 2);

    r = atom_table_ensure_atom(table, (const uint8_t *) "ciao", 4, AtomTableAlreadyExisting, &ciao_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(ciao_index == 1);

    size_t atom_len_0;
    const uint8_t *atom_data_0 = atom_table_get_atom_string(table, 0, &atom_len_0);
    UNUSED(atom_data_0);
    assert(atom_len_0 == 3);

    atom_index_t decimals_index = insert_atoms_into_atom_table(table);

    r = atom_table_ensure_atom(table, (const uint8_t *) "decimals", 8, AtomTableNoOpts, &new_index);
    assert(r == AtomTableEnsureAtomOk);
    assert(new_index == decimals_index);

    atom_table_destroy(table);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_valueshashtable();
    test_atom_table();

    return EXIT_SUCCESS;
}
