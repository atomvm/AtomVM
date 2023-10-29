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

#include "atomshashtable.h"
#include "atom_table.h"
#include "utils.h"
#include "valueshashtable.h"

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

void test_atomshashtable()
{
    char atom_hello[] = {5, 'h', 'e', 'l', 'l', 'o'};
    char atom_ciao[] = {4, 'c', 'i', 'a', 'o'};
    char atom_a[] = {1, 'a'};
    char atom_b[] = {1, 'b'};
    char atom_c[] = {1, 'c'};
    char atom_d[] = {1, 'd'};
    char atom_e[] = {1, 'e'};
    char atom_f[] = {1, 'f'};
    char atom_0[] = {1, '0'};
    char atom_1[] = {1, '1'};
    char atom_2[] = {1, '2'};
    char atom_3[] = {1, '3'};
    char atom_4[] = {1, '4'};
    char atom_5[] = {1, '5'};
    char atom_6[] = {1, '6'};
    char atom_7[] = {1, '7'};
    char atom_8[] = {1, '8'};
    char atom_9[] = {1, '9'};

    struct AtomsHashTable *htable = atomshashtable_new();

    assert(atomshashtable_insert(htable, atom_hello, 0xABCD1234) == 1);
    assert(atomshashtable_get_value(htable, atom_hello, 0xCAFECAFE) == 0xABCD1234);
    assert(atomshashtable_get_value(htable, atom_ciao, 0xCAFECAFE) == 0xCAFECAFE);
    assert(atomshashtable_has_key(htable, atom_hello) == 1);
    assert(atomshashtable_has_key(htable, atom_ciao) == 0);

    assert(atomshashtable_insert(htable, atom_hello, 0x11112222) == 1);
    assert(atomshashtable_get_value(htable, atom_hello, 0xCAFECAFE) == 0x11112222);
    assert(atomshashtable_get_value(htable, atom_ciao, 0xCAFECAFE) == 0xCAFECAFE);

    assert(atomshashtable_insert(htable, atom_ciao, 0x99887766) == 1);
    assert(atomshashtable_get_value(htable, atom_ciao, 0xCAFECAFE) == 0x99887766);
    assert(atomshashtable_get_value(htable, atom_hello, 0xCAFECAFE) == 0x11112222);

    assert(atomshashtable_insert(htable, atom_a, 0xA) == 1);
    assert(atomshashtable_insert(htable, atom_b, 0xB) == 1);
    assert(atomshashtable_insert(htable, atom_c, 0xC) == 1);
    assert(atomshashtable_insert(htable, atom_d, 0xD) == 1);
    assert(atomshashtable_insert(htable, atom_e, 0xE) == 1);
    assert(atomshashtable_insert(htable, atom_f, 0xF) == 1);
    assert(atomshashtable_insert(htable, atom_0, 0x0) == 1);
    assert(atomshashtable_insert(htable, atom_1, 0x1) == 1);
    assert(atomshashtable_insert(htable, atom_2, 0x2) == 1);
    assert(atomshashtable_insert(htable, atom_3, 0x3) == 1);
    assert(atomshashtable_insert(htable, atom_4, 0x4) == 1);
    assert(atomshashtable_insert(htable, atom_5, 0x5) == 1);
    assert(atomshashtable_insert(htable, atom_6, 0x6) == 1);
    assert(atomshashtable_insert(htable, atom_7, 0x7) == 1);
    assert(atomshashtable_insert(htable, atom_8, 0x8) == 1);
    assert(atomshashtable_insert(htable, atom_9, 0x9) == 1);

    assert(atomshashtable_get_value(htable, atom_hello, 0xCAFECAFE) == 0x11112222);
    assert(atomshashtable_get_value(htable, atom_ciao, 0xCAFECAFE) == 0x99887766);
    assert(atomshashtable_get_value(htable, atom_a, 0xCAFEBABE) == 0xA);
    assert(atomshashtable_get_value(htable, atom_b, 0xCAFEBABE) == 0xB);
    assert(atomshashtable_get_value(htable, atom_c, 0xCAFEBABE) == 0xC);
    assert(atomshashtable_get_value(htable, atom_d, 0xCAFEBABE) == 0xD);
    assert(atomshashtable_get_value(htable, atom_e, 0xCAFEBABE) == 0xE);
    assert(atomshashtable_get_value(htable, atom_f, 0xCAFEBABE) == 0xF);
    assert(atomshashtable_get_value(htable, atom_0, 0xCAFEBABE) == 0x0);
    assert(atomshashtable_get_value(htable, atom_1, 0xCAFEBABE) == 0x1);
    assert(atomshashtable_get_value(htable, atom_2, 0xCAFEBABE) == 0x2);
    assert(atomshashtable_get_value(htable, atom_3, 0xCAFEBABE) == 0x3);
    assert(atomshashtable_get_value(htable, atom_4, 0xCAFEBABE) == 0x4);
    assert(atomshashtable_get_value(htable, atom_5, 0xCAFEBABE) == 0x5);
    assert(atomshashtable_get_value(htable, atom_6, 0xCAFEBABE) == 0x6);
    assert(atomshashtable_get_value(htable, atom_7, 0xCAFEBABE) == 0x7);
    assert(atomshashtable_get_value(htable, atom_8, 0xCAFEBABE) == 0x8);
    assert(atomshashtable_get_value(htable, atom_9, 0xCAFEBABE) == 0x9);

    assert(atomshashtable_has_key(htable, atom_hello) == 1);
    assert(atomshashtable_has_key(htable, atom_ciao) == 1);
    assert(atomshashtable_has_key(htable, atom_a) == 1);
    assert(atomshashtable_has_key(htable, atom_b) == 1);
    assert(atomshashtable_has_key(htable, atom_c) == 1);
    assert(atomshashtable_has_key(htable, atom_d) == 1);
    assert(atomshashtable_has_key(htable, atom_e) == 1);
    assert(atomshashtable_has_key(htable, atom_f) == 1);
    assert(atomshashtable_has_key(htable, atom_0) == 1);
    assert(atomshashtable_has_key(htable, atom_1) == 1);
    assert(atomshashtable_has_key(htable, atom_2) == 1);
    assert(atomshashtable_has_key(htable, atom_3) == 1);
    assert(atomshashtable_has_key(htable, atom_4) == 1);
    assert(atomshashtable_has_key(htable, atom_5) == 1);
    assert(atomshashtable_has_key(htable, atom_6) == 1);
    assert(atomshashtable_has_key(htable, atom_7) == 1);
    assert(atomshashtable_has_key(htable, atom_8) == 1);
    assert(atomshashtable_has_key(htable, atom_9) == 1);
}

void test_valueshashtable()
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

int insert_atoms_into_atom_table(struct AtomTable *table)
{
    int decimals_index;

    atom_table_ensure_atom(table, false_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, true_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, ok_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, error_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, undefined_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, badarg_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, badarith_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, badarity_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, badfun_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, function_clause_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, try_clause_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, out_of_memory_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, overflow_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, system_limit_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, flush_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, heap_size_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, latin1_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, max_heap_size_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, memory_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, message_queue_len_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, puts_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, stack_size_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, min_heap_size_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, process_count_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, port_count_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, atom_count_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, system_architecture_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, wordsize_atom, AtomTableNoOpts);

    decimals_index = atom_table_ensure_atom(table, decimals_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, scientific_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, compact_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, badmatch_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, case_clause_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, if_clause_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, throw_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, low_entropy_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, unsupported_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, used_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, all_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, start_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, undef_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, vm_abort_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, link_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, monitor_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, normal_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, down_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, process_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, nocatch_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, refc_binary_info_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, noproc_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, trap_exit_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, exit_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, badmap_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, badkey_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, none_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, io_request_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, io_reply_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, put_chars_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, lowercase_exit_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, atomvm_version_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, second_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, millisecond_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, microsecond_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, infinity_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, timeout_value_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, schedulers_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, schedulers_online_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, append_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, private_append_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, binary_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, integer_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, little_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, native_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, string_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, utf8_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, utf16_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, utf32_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, badrecord_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, copy_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, reuse_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, ensure_at_least_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, ensure_exactly_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, skip_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, get_tail_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, equal_colon_equal_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, signed_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, machine_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, avm_floatsize_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, close_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, closed_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, port_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, info_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, module_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, select_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, ready_input_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, ready_output_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, attributes_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, compile_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, exports_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, incomplete_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, kill_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, killed_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, links_atom, AtomTableNoOpts);

    atom_table_ensure_atom(table, total_heap_size_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, atomvm_heap_growth_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, bounded_free_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, minimum_atom, AtomTableNoOpts);
    atom_table_ensure_atom(table, fibonacci_atom, AtomTableNoOpts);

    return decimals_index;
}

void test_atom_table()
{
    struct AtomTable *table = atom_table_new();

    assert(atom_table_get_index(table, "\x4" "ciao") == ATOM_TABLE_NOT_FOUND);

    assert(atom_table_ensure_atom(table, "\x3" "bar", AtomTableNoOpts) == 0);
    assert(atom_table_ensure_atom(table, "\x4" "ciao", AtomTableNoOpts) == 1);
    assert(atom_table_ensure_atom(table, "\x3" "foo", AtomTableNoOpts) == 2);
    assert(atom_table_ensure_atom(table, "\x3" "foo", AtomTableNoOpts) == 2);

    assert(atom_table_get_index(table, "\x4" "ciao") == 1);

    assert(((char *) atom_table_get_atom_string(table, 0))[0] == 3);

    int decimals_index = insert_atoms_into_atom_table(table);

    assert(atom_table_get_index(table, "\x8" "decimals") == decimals_index);
    assert(atom_table_ensure_atom(table, "\x8" "decimals", AtomTableNoOpts) == decimals_index);

    atom_table_destroy(table);
}

int main(int argc, char **argv)
{
    UNUSED(argc);
    UNUSED(argv);

    test_atomshashtable();
    test_valueshashtable();
    test_atom_table();

    return EXIT_SUCCESS;
}
