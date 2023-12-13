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
#include <libgen.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "term.h"
#include "utils.h"

struct Test
{
    const char *test_module;
    int32_t expected_value;
    bool skip_atom;
    bool skip_beam;
};

// Favor modules that return 0
#define TEST_CASE(module)        \
    {                            \
        #module, 0, false, false \
    }
#define TEST_CASE_EXPECTED(module, expected) \
    {                                        \
        #module, expected, false, false      \
    }
#define TEST_CASE_ATOMVM_ONLY(module, expected) \
    {                                           \
        #module, expected, false, true          \
    }
#define TEST_CASE_COND(module, expected, skip) \
    {                                          \
        #module, expected, skip, false         \
    }

#ifndef AVM_NO_SMP
#define SKIP_SMP false
#else
#define SKIP_SMP true
#endif

#ifndef AVM_CREATE_STACKTRACES
#define SKIP_STACKTRACES true
#else
#define SKIP_STACKTRACES false
#endif

struct Test tests[] = {
    TEST_CASE_EXPECTED(add, 17),
    TEST_CASE_EXPECTED(fact, 120),
    TEST_CASE_EXPECTED(mutrec, 6),
    TEST_CASE_EXPECTED(morelabels, 6),
    TEST_CASE_EXPECTED(biggerintegers, 550),
    TEST_CASE_EXPECTED(biggerdifference, 250),
    TEST_CASE_EXPECTED(moreintegertests, 32),
    TEST_CASE_EXPECTED(send_receive, 18),
    TEST_CASE_EXPECTED(send_to_dead_process, 20),
    TEST_CASE_EXPECTED(byte_size_test, 10),
    TEST_CASE_EXPECTED(tuple, 6),
    TEST_CASE_EXPECTED(len_test, 5),
    TEST_CASE_EXPECTED(count_char, 2),
    TEST_CASE_EXPECTED(makelist_test, 532),
    TEST_CASE_EXPECTED(state_test, 3),
    TEST_CASE_EXPECTED(booleans_test, 4),
    TEST_CASE_EXPECTED(booleans2_test, 2),
    TEST_CASE_EXPECTED(rem_and_comp_test, 4),
    TEST_CASE_EXPECTED(lowercase, 15),
    TEST_CASE_EXPECTED(huge, 31),
    TEST_CASE_EXPECTED(patternmatchfunc, 102),
    TEST_CASE_EXPECTED(moda, 44),
    TEST_CASE_EXPECTED(state_test2, 3),
    TEST_CASE_EXPECTED(state_test3, 3),
    TEST_CASE_EXPECTED(guards1, 261),
    TEST_CASE_EXPECTED(guards2, 36),
    // This tests depends on echo port, but it works sufficiently on Unix
    // as BEAM will start echo(1)
    TEST_CASE_EXPECTED(guards3, 405),
    TEST_CASE_EXPECTED(guards4, 16),
    TEST_CASE_EXPECTED(guards5, 3),
    TEST_CASE(test_guards_do_not_raise),
    TEST_CASE_EXPECTED(prime, 1999),
    TEST_CASE_COND(prime_smp, 0, SKIP_SMP),
    TEST_CASE_EXPECTED(match, 5),
    TEST_CASE_EXPECTED(if_test, 5),
    TEST_CASE(sleep),
    TEST_CASE(whereis_dead_process),
    TEST_CASE_EXPECTED(whereis_fail, 2),
    TEST_CASE_EXPECTED(try_noerror, 1),
    TEST_CASE_EXPECTED(catch_badmatch, 1),
    TEST_CASE_EXPECTED(catch_nocasematch, 1),
    TEST_CASE_EXPECTED(catch_noifmatch, 1),
    TEST_CASE_EXPECTED(try_catch_test, 109),
    TEST_CASE_EXPECTED(list_concat, 2270),
    TEST_CASE_EXPECTED(make_ref_test, 130),
    TEST_CASE_EXPECTED(is_ref_test, 3),
    TEST_CASE_EXPECTED(tagged_tuple_test, 48),
    TEST_CASE_EXPECTED(call_with_ref_test, 3),
    TEST_CASE_EXPECTED(just_receive_test, 11),
    TEST_CASE_EXPECTED(gen_server_like_test, 3),
    TEST_CASE_EXPECTED(external_proplist_test, 3),
    TEST_CASE_EXPECTED(compact15bitsinteger, 1567888),
    TEST_CASE_EXPECTED(negatives, -55996),
    TEST_CASE_EXPECTED(compact23bitsinteger, 47769328),
    TEST_CASE_EXPECTED(compact27bitsinteger, 61837935),
    TEST_CASE_EXPECTED(compact23bitsneginteger, -47376112),
    TEST_CASE_EXPECTED(negatives2, -500),
    TEST_CASE_EXPECTED(datetime, 3),
    TEST_CASE(test_system_time),
    TEST_CASE_EXPECTED(is_type, 255),
    TEST_CASE(test_bitshift),
    TEST_CASE_EXPECTED(test_bitwise, -4),
    TEST_CASE(test_bitwise2),
    TEST_CASE(test_boolean),
    TEST_CASE_EXPECTED(test_gt_and_le, 255),
    TEST_CASE_EXPECTED(test_tuple_size, 6),
    TEST_CASE_EXPECTED(test_element, 7),
    TEST_CASE_EXPECTED(test_setelement, 121),
    TEST_CASE_EXPECTED(test_insert_element, 121),
    TEST_CASE_EXPECTED(test_delete_element, 421),
    TEST_CASE_EXPECTED(test_tuple_to_list, 300),
    TEST_CASE_EXPECTED(test_make_tuple, 4),
    TEST_CASE_EXPECTED(test_make_list, 5),
    TEST_CASE_EXPECTED(test_list_gc, 2),
    TEST_CASE_EXPECTED(test_list_processes, 3),
    TEST_CASE_EXPECTED(test_tl, 5),
    TEST_CASE_EXPECTED(test_list_to_atom, 9),
    TEST_CASE_EXPECTED(test_list_to_existing_atom, 9),
    TEST_CASE(test_lists_reverse),
    TEST_CASE_EXPECTED(test_binary_to_atom, 9),
    TEST_CASE_EXPECTED(test_binary_to_existing_atom, 9),
    TEST_CASE_EXPECTED(test_atom_to_list, 1),
    TEST_CASE(test_display),
    TEST_CASE(test_integer_to_list),
    TEST_CASE_EXPECTED(test_list_to_integer, 99),
    TEST_CASE_EXPECTED(test_abs, 5),
    TEST_CASE_EXPECTED(test_is_process_alive, 121),
    TEST_CASE_EXPECTED(test_is_not_type, 255),
    TEST_CASE_EXPECTED(test_badarith, -87381),
    TEST_CASE_EXPECTED(test_badarith2, -87381),
    TEST_CASE_EXPECTED(test_badarith3, -1365),
    TEST_CASE_EXPECTED(test_badarith4, -1365),
    TEST_CASE_EXPECTED(test_bif_badargument, -5592405),
    TEST_CASE_EXPECTED(test_bif_badargument2, -85),
    TEST_CASE_EXPECTED(test_bif_badargument3, -85),
    TEST_CASE_EXPECTED(test_tuple_nifs_badargs, -1398101),
    TEST_CASE_EXPECTED(long_atoms, 4),
    TEST_CASE_EXPECTED(test_concat_badarg, 4),
    TEST_CASE_EXPECTED(register_and_whereis_badarg, 333),
    TEST_CASE(test_send),
    TEST_CASE_EXPECTED(test_open_port_badargs, -21),
    TEST_CASE_EXPECTED(prime_ext, 1999),
    TEST_CASE_EXPECTED(test_try_case_end, 256),
    TEST_CASE(test_exception_classes),
    TEST_CASE_EXPECTED(test_recursion_and_try_catch, 3628800),
    TEST_CASE_EXPECTED(test_func_info, 89),
    TEST_CASE_EXPECTED(test_func_info2, 1),
    TEST_CASE_EXPECTED(test_func_info3, 120),
    TEST_CASE(test_process_info),
    TEST_CASE(test_min_heap_size),
    TEST_CASE_ATOMVM_ONLY(test_heap_growth, 0),
    TEST_CASE(test_system_flag),
    TEST_CASE(test_system_info),
    TEST_CASE_EXPECTED(test_funs0, 20),
    TEST_CASE_EXPECTED(test_funs1, 517),
    TEST_CASE_EXPECTED(test_funs2, 52),
    TEST_CASE_EXPECTED(test_funs3, 40),
    TEST_CASE_EXPECTED(test_funs4, 6),
    TEST_CASE_EXPECTED(test_funs5, 1000),
    TEST_CASE_EXPECTED(test_funs6, 16),
    TEST_CASE_EXPECTED(test_funs7, 9416),
    TEST_CASE_EXPECTED(test_funs8, 22000),
    TEST_CASE_EXPECTED(test_funs9, 3555),
    TEST_CASE_EXPECTED(test_funs10, 6817),
    TEST_CASE_EXPECTED(test_funs11, 817),
    TEST_CASE(test_make_fun3),

    TEST_CASE(nested_list_size0),
    TEST_CASE_EXPECTED(nested_list_size1, 2),
    TEST_CASE_EXPECTED(nested_list_size2, 8),
    TEST_CASE_EXPECTED(nested_list_size3, 68),
    TEST_CASE_EXPECTED(nested_list_size4, 408),

    TEST_CASE_EXPECTED(simple_list_size0, 2),
    TEST_CASE_EXPECTED(simple_list_size1, 10),

    TEST_CASE(tuple_size0),
    TEST_CASE_EXPECTED(tuple_size1, 2),
    TEST_CASE_EXPECTED(tuple_size2, 3),
    TEST_CASE_EXPECTED(tuple_size3, 4),
    TEST_CASE_EXPECTED(tuple_size4, 13),
    TEST_CASE(tuple_size5),
    TEST_CASE(tuple_size6),

    TEST_CASE(tuples_and_list_size0),
    TEST_CASE_EXPECTED(tuples_and_list_size1, 5),
    TEST_CASE_EXPECTED(tuples_and_list_size2, 10),

    TEST_CASE_EXPECTED(nested_tuple_size0, 12),
    TEST_CASE_EXPECTED(nested_tuple_size1, 44),
    TEST_CASE_EXPECTED(nested_tuple_size2, 76),
    TEST_CASE_EXPECTED(nested_tuple_size3, 80),
    TEST_CASE_EXPECTED(nested_tuple_size4, 80),

    TEST_CASE_EXPECTED(complex_struct_size0, 43),
    TEST_CASE_EXPECTED(complex_struct_size1, 37),
    TEST_CASE_EXPECTED(complex_struct_size2, 105),
    TEST_CASE_EXPECTED(complex_struct_size3, 23),
    TEST_CASE_EXPECTED(complex_struct_size4, 126),

    TEST_CASE_EXPECTED(make_garbage0, -19),
    TEST_CASE_EXPECTED(make_garbage1, -18),
    TEST_CASE_EXPECTED(make_garbage2, -56),
    TEST_CASE_EXPECTED(make_garbage3, -7),
    TEST_CASE_EXPECTED(make_garbage4, -438),
    TEST_CASE_EXPECTED(make_garbage5, 6),
    TEST_CASE_EXPECTED(make_garbage6, -210),
    TEST_CASE_EXPECTED(make_garbage7, -210),

    TEST_CASE(copy_terms0),
    TEST_CASE_EXPECTED(copy_terms1, 1),
    TEST_CASE_EXPECTED(copy_terms2, 2),
    TEST_CASE_EXPECTED(copy_terms3, 5),
    TEST_CASE_EXPECTED(copy_terms4, 2465),
    TEST_CASE_EXPECTED(copy_terms5, 32),
    TEST_CASE_EXPECTED(copy_terms6, 2),
    TEST_CASE_EXPECTED(copy_terms7, 10),
    TEST_CASE_EXPECTED(copy_terms8, 4),
    TEST_CASE_EXPECTED(copy_terms9, -19),
    TEST_CASE_EXPECTED(copy_terms10, -18),
    TEST_CASE_EXPECTED(copy_terms11, 36757),
    TEST_CASE_EXPECTED(copy_terms12, 36757),
    TEST_CASE_EXPECTED(copy_terms13, 37037),
    TEST_CASE_EXPECTED(copy_terms14, -210),
    TEST_CASE_EXPECTED(copy_terms15, -438),
    TEST_CASE_EXPECTED(copy_terms16, 6),
    TEST_CASE_EXPECTED(copy_terms17, 11),
    TEST_CASE_EXPECTED(copy_terms18, -19),

    TEST_CASE_EXPECTED(test_apply, 17),
    TEST_CASE_EXPECTED(test_apply_last, 17),
    TEST_CASE(test_monitor),
    TEST_CASE_EXPECTED(test_timestamp, 1),
    TEST_CASE(test_set_tuple_element),

    TEST_CASE_EXPECTED(spawn_fun1, 42),
    TEST_CASE_EXPECTED(spawn_fun2, 33),
    TEST_CASE_EXPECTED(spawn_fun3, 10),

    TEST_CASE_EXPECTED(binary_at_test, 121),
    TEST_CASE_EXPECTED(binary_first_test, 82),
    TEST_CASE_EXPECTED(binary_last_test, 110),

    TEST_CASE(test_integer_to_binary),
    TEST_CASE(test_list_to_binary),
    TEST_CASE_EXPECTED(test_binary_to_list, 0),
    TEST_CASE_EXPECTED(test_atom_to_binary, 1),
    TEST_CASE(test_unicode),

    TEST_CASE_EXPECTED(test_binary_part, 12),
    TEST_CASE_EXPECTED(test_binary_split, 16),

    TEST_CASE_COND(plusone, 134217728, LONG_MAX != 9223372036854775807),

    TEST_CASE_EXPECTED(plusone2, 1),
    TEST_CASE_EXPECTED(minusone, -134217729),
    TEST_CASE_EXPECTED(minusone2, -16),
    TEST_CASE_EXPECTED(int28mul, 134217728),
    TEST_CASE_EXPECTED(int28mulneg, -268435456),
    TEST_CASE_EXPECTED(int28mulneg2, 268435448),
    TEST_CASE_EXPECTED(negdiv, 134217728),
    TEST_CASE_EXPECTED(absovf, 134217728),
    TEST_CASE_EXPECTED(negovf, 134217728),

    TEST_CASE_EXPECTED(plusone3, 134217726),
    TEST_CASE_EXPECTED(plusone4, 134217728),
    TEST_CASE_EXPECTED(bigfact, 1860480),
    TEST_CASE_EXPECTED(bigfact2, 189907211),
    TEST_CASE_EXPECTED(bigfact3, 3078559),
    TEST_CASE_EXPECTED(boxedabs, 15),
    TEST_CASE_EXPECTED(boxedneg, 15),
    TEST_CASE_EXPECTED(boxedmul, 15),
    TEST_CASE_EXPECTED(boxedlit, 1073741824),
    TEST_CASE_EXPECTED(pow32, 528),
    TEST_CASE_EXPECTED(pow64, 1794),
    TEST_CASE_EXPECTED(pow32_is_integer, 528),
    TEST_CASE_EXPECTED(pow64_is_integer, 1794),
    TEST_CASE_EXPECTED(addovf32, 791),
    TEST_CASE_EXPECTED(subovf32, 282),
    TEST_CASE_EXPECTED(negovf32, -1935),
    TEST_CASE_EXPECTED(addovf64, 2781),
    TEST_CASE_EXPECTED(subovf64, 282),
    TEST_CASE_EXPECTED(negovf64, -6865),
    TEST_CASE_EXPECTED(powsquare, 1572),
    TEST_CASE_EXPECTED(minuspow31minusone, -566),
    TEST_CASE_EXPECTED(pow31plusone, 547),
    TEST_CASE_EXPECTED(minuspow31divminusone, 547),
    TEST_CASE_EXPECTED(pow31abs, 547),
    TEST_CASE_EXPECTED(minuspow31abs, 547),
    TEST_CASE_EXPECTED(pow31minusoneabs, 528),
    TEST_CASE_EXPECTED(minuspow31plusoneabs, 528),
    TEST_CASE_EXPECTED(minuspow31plustwoabs, 509),
    TEST_CASE_EXPECTED(minuspow63plusoneabs, 1794),
    TEST_CASE_EXPECTED(minuspow63plustwoabs, 1757),

    TEST_CASE_EXPECTED(literal_test0, 333575620),
    TEST_CASE_EXPECTED(literal_test1, 1680),

    TEST_CASE_EXPECTED(test_list_eq, 1),
    TEST_CASE_EXPECTED(test_tuple_eq, 1),
    TEST_CASE_EXPECTED(test_tuple_list_eq, 1),
    TEST_CASE_EXPECTED(test_list_tuple_eq, 1),
    TEST_CASE_EXPECTED(test_ref_eq, 1),
    TEST_CASE_EXPECTED(test_binary_eq, 1),
    TEST_CASE_EXPECTED(test_bigint_eq, 1),

    TEST_CASE_EXPECTED(test_binaries_ordering, 15),
    TEST_CASE_EXPECTED(test_lists_ordering, 7),
    TEST_CASE_EXPECTED(test_tuples_ordering, 7),
    TEST_CASE_EXPECTED(test_types_ordering, 1),
    TEST_CASE_EXPECTED(test_bigintegers_ordering, 7),
    TEST_CASE_EXPECTED(test_refs_ordering, 7),
    TEST_CASE_EXPECTED(test_atom_ordering, 7),
    TEST_CASE_EXPECTED(test_pids_ordering, 7),
    TEST_CASE_EXPECTED(test_list_match, 31),
    TEST_CASE(test_match),
    TEST_CASE_EXPECTED(test_ordering_0, 1),
    TEST_CASE_EXPECTED(test_ordering_1, 1),
    TEST_CASE(test_binary_to_term),
    TEST_CASE(test_selective_receive),
    TEST_CASE(test_timeout_not_integer),
    TEST_CASE(test_bs),
    TEST_CASE(test_bs_int),
    TEST_CASE(test_bs_int_unaligned),
    TEST_CASE(test_bs_utf),
    TEST_CASE(test_catch),
    TEST_CASE(test_gc),
    TEST_CASE_EXPECTED(test_raise, 7),
    TEST_CASE(test_map),
    TEST_CASE_ATOMVM_ONLY(test_refc_binaries, 0),
    TEST_CASE(test_sub_binaries),
    TEST_CASE_ATOMVM_ONLY(test_throw_call_ext_last, 0),

    TEST_CASE_EXPECTED(ceilint, 1),
    TEST_CASE_EXPECTED(ceilbadarg, -1),
    TEST_CASE_EXPECTED(floorint, 1),
    TEST_CASE_EXPECTED(floorbadarg, -1),
    TEST_CASE_EXPECTED(roundint, 1),
    TEST_CASE_EXPECTED(roundbadarg, -1),
    TEST_CASE_EXPECTED(truncint, 1),
    TEST_CASE_EXPECTED(truncbadarg, -1),

    TEST_CASE_EXPECTED(ceilfloat, -2),
    TEST_CASE(ceilfloatovf),
    TEST_CASE_EXPECTED(floorfloat, -3),
    TEST_CASE(floorfloatovf),
    TEST_CASE_EXPECTED(roundfloat, -3),
    TEST_CASE(roundfloatovf),
    TEST_CASE_EXPECTED(truncfloat, -2),
    TEST_CASE(truncfloatovf),

    TEST_CASE(floataddovf),
    TEST_CASE(floatadd),
    TEST_CASE(floatsubovf),
    TEST_CASE(floatsub),
    TEST_CASE(floatmulovf),
    TEST_CASE(floatmul),
    TEST_CASE(floatneg),
    TEST_CASE_EXPECTED(floatabs, 3),
    TEST_CASE(floatdiv),

    TEST_CASE_EXPECTED(boxed_is_not_float, 16),
    TEST_CASE_EXPECTED(float_is_float, 32),
    TEST_CASE_EXPECTED(float_is_number, 32),

    TEST_CASE_EXPECTED(float2bin, 31),
    TEST_CASE_EXPECTED(float2list, 31),
    TEST_CASE_EXPECTED(float2bin2scientific, 31),
    TEST_CASE_EXPECTED(float2bin2decimals, 255),
    TEST_CASE_EXPECTED(float2bin2, 31),
    TEST_CASE_EXPECTED(float2list2scientific, 31),
    TEST_CASE_EXPECTED(float2list2decimals, 255),
    TEST_CASE_EXPECTED(float2list2, 31),
    TEST_CASE_EXPECTED(bin2float, 511),
    TEST_CASE_EXPECTED(list2float, 511),
    TEST_CASE(floatmath),
    TEST_CASE(floatext),

    TEST_CASE(test_fp_allocate_heap_zero),

    TEST_CASE(test_bs_init2_heap_allocation),

    TEST_CASE_EXPECTED(improper_concat, 7),
    TEST_CASE_EXPECTED(improper_cmp, 3),
    TEST_CASE_EXPECTED(improper_literal, 3),
    TEST_CASE_EXPECTED(improper_length, 3),

    TEST_CASE_EXPECTED(jsonish_encode, 1058),

    TEST_CASE_EXPECTED(iolist_concat_bin, 71006),
    TEST_CASE_EXPECTED(binary_is_iolist, 1006),

    TEST_CASE_EXPECTED(catch_from_other_module, 7),
    TEST_CASE_EXPECTED(throwtest, -10),

    TEST_CASE_EXPECTED(test_tuple_is_not_map, 16),

    TEST_CASE_EXPECTED(try_error_nif, 13),
    TEST_CASE_EXPECTED(try_error2_nif, 13),

    TEST_CASE_EXPECTED(is_fun_2_with_frozen, 24),
    TEST_CASE_EXPECTED(is_fun_2_with_frozen2, 24),

    TEST_CASE_EXPECTED(function_reference_decode, 5),
    TEST_CASE_EXPECTED(makefunref, 3),
    TEST_CASE_EXPECTED(fail_apply, 17),
    TEST_CASE_EXPECTED(fail_apply_last, 17),

    TEST_CASE_EXPECTED(pid_to_list_test, 63),
    TEST_CASE_EXPECTED(ref_to_list_test, 386),
    TEST_CASE_EXPECTED(test_binary_to_integer, 99),

    TEST_CASE_EXPECTED(count_char_bs, 2),
    TEST_CASE_EXPECTED(count_char2_bs, 1002),
    TEST_CASE_EXPECTED(count_char3_bs, 1),
    TEST_CASE_EXPECTED(count_pairs, 3),
    TEST_CASE_EXPECTED(decode_mqtt, 120948),
    TEST_CASE_EXPECTED(decode_int24, 725246),
    TEST_CASE_EXPECTED(decode_int32, 289273),
    TEST_CASE_EXPECTED(decode_int48, 858867),

    TEST_CASE_EXPECTED(large_int_literal, 5953),

    TEST_CASE(test_base64),
    TEST_CASE_EXPECTED(test_dict, 2020),

    TEST_CASE_EXPECTED(alisp, 42),
    TEST_CASE_EXPECTED(test_function_exported, 7),
    TEST_CASE_EXPECTED(test_list_to_tuple, 69),

    TEST_CASE_EXPECTED(bs_context_to_binary_with_offset, 42),
    TEST_CASE_EXPECTED(bs_restore2_start_offset, 823),

    TEST_CASE_EXPECTED(test_monotonic_time, 1),

    TEST_CASE_EXPECTED(exactly_eq, 7),
    TEST_CASE_EXPECTED(map_comparisons, 67108863),
    TEST_CASE_EXPECTED(tuple_comparisons, 6),

    // Tests relying on echo driver
    TEST_CASE_ATOMVM_ONLY(pingpong, 1),

    // Tests relying on console driver
    TEST_CASE_ATOMVM_ONLY(hello_world, 10),
    TEST_CASE_ATOMVM_ONLY(test_echo_driver, 84),
    TEST_CASE_ATOMVM_ONLY(test_close_console_driver, 0),
    TEST_CASE_ATOMVM_ONLY(test_close_echo_driver, 0),
    TEST_CASE_ATOMVM_ONLY(test_regecho_driver, 11),
    TEST_CASE_ATOMVM_ONLY(test_send_nif_and_echo, 22),

    TEST_CASE_EXPECTED(test_code_load_binary, 24),
    TEST_CASE_EXPECTED(test_code_load_abs, 24),
    TEST_CASE_ATOMVM_ONLY(test_add_avm_pack_binary, 24),
    TEST_CASE_ATOMVM_ONLY(test_add_avm_pack_file, 24),
    TEST_CASE_ATOMVM_ONLY(test_close_avm_pack, 0),

    TEST_CASE(test_module_info),

    // noisy tests, keep them at the end
    TEST_CASE_EXPECTED(spawn_opt_monitor_normal, 1),
    TEST_CASE_EXPECTED(spawn_opt_link_normal, 1),
    TEST_CASE_EXPECTED(spawn_opt_monitor_throw, 1),
    TEST_CASE_EXPECTED(spawn_opt_demonitor_normal, 1),
    TEST_CASE_EXPECTED(spawn_opt_link_throw, 1),
    TEST_CASE_EXPECTED(spawn_opt_monitor_error, 1),
    TEST_CASE_EXPECTED(link_kill_parent, 1),
    TEST_CASE_EXPECTED(link_throw, 1),
    TEST_CASE_EXPECTED(unlink_error, 1),
    TEST_CASE(trap_exit_flag),
    TEST_CASE(test_exit1),
    TEST_CASE(test_exit2),
    TEST_CASE_COND(test_stacktrace, 0, SKIP_STACKTRACES),
    TEST_CASE(small_big_ext),
    TEST_CASE(test_crypto),
    TEST_CASE(test_min_max_guard),
    TEST_CASE(int64_build_binary),

#if defined ATOMVM_HAS_MBEDTLS
    TEST_CASE(test_crypto_strong_rand_bytes),
    TEST_CASE(test_atomvm_random),
#endif

    TEST_CASE(float_decode),
    // TEST CRASHES HERE: TEST_CASE(memlimit),

    { NULL, 0, false, false }
};

static int test_atom(struct Test *test)
{
    int result = 0;
    char module_file[128];
    snprintf(module_file, sizeof(module_file), "%s.beam", test->test_module);
    MappedFile *beam_file = mapped_file_open_beam(module_file);
    assert(beam_file != NULL);

    GlobalContext *glb = globalcontext_new();
    Module *mod = module_new_from_iff_binary(glb, beam_file->mapped, beam_file->size);
    if (IS_NULL_PTR(mod)) {
        fprintf(stderr, "Cannot load startup module: %s\n", test->test_module);
        return -1;
    }
    globalcontext_insert_module(glb, mod);
    Context *ctx = context_new(glb);
    ctx->leader = 1;

    context_execute_loop(ctx, mod, "start", 0);

    if (!term_is_any_integer(ctx->x[0])) {
        fprintf(stderr, "\x1b[1;31mExpected %i but result is not an integer\x1b[0m\n", test->expected_value);
        result = -1;
    } else {
        int32_t value = (int32_t) term_maybe_unbox_int(ctx->x[0]);
        if (value != test->expected_value) {
            fprintf(stderr, "\x1b[1;31mExpected %i, got: %i\x1b[0m\n", test->expected_value, value);
            result = -1;
        }
    }

    context_destroy(ctx);
    globalcontext_destroy(glb);
    module_destroy(mod);
    mapped_file_close(beam_file);
    return result;
}

static int test_beam(struct Test *test)
{
    char command[512];
    snprintf(command, sizeof(command),
        "erl -pa . -eval '"
        "erlang:process_flag(trap_exit, false), " /* init(3) traps exists */
        "R = %s:start(), "
        "S = if"
        " R =:= %i -> 0;"
        " true -> io:format(\"Expected ~B, got ~p\n\", [%i, R]) "
        "end, "
        "erlang:halt(S).' -noshell",
        test->test_module,
        test->expected_value,
        test->expected_value);
    return system(command);
}

int test_module_execution(bool beam, struct Test *test)
{
    if (beam ? test->skip_beam : test->skip_atom) {
        fprintf(stderr, "%s:\x1b[34GSKIPPED\n", test->test_module);
        return 0;
    }
    fprintf(stderr, "%s:\r", test->test_module);
    int result = beam ? test_beam(test) : test_atom(test);
    if (result) {
        fprintf(stderr, "\x1b[2K\x1b[1;31m%s:\x1b[34GFAILED\x1b[0m\n", test->test_module);
        return 1;
    }
    fprintf(stderr, "\x1b[2K%s:\x1b[34GOK\n", test->test_module);
    return 0;
}

int test_modules_execution(bool beam, bool skip, int count, char **item)
{
    if (chdir("erlang_tests") != 0) {
        perror("Error: ");
        return EXIT_FAILURE;
    }

    int failed_tests = 0;

    if (count) {
        for (int ix = 0; ix < count; ix++) {
            struct Test *test = tests;
            do {
                if (strcmp(test->test_module, item[ix]) == 0) {
                    if (skip) {
                        test->skip_beam = true;
                        test->skip_atom = true;
                    } else {
                        failed_tests += test_module_execution(beam, test);
                    }
                    break;
                }
                test++;
            } while (test->test_module);
            if (test->test_module == NULL) {
                fprintf(stderr, "Unknown test module %s\n", item[ix]);
                return EXIT_FAILURE;
            }
        }
    }

    if (count == 0 || skip) {
        struct Test *test = tests;
        do {
            failed_tests += test_module_execution(beam, test);
            test++;
        } while (test->test_module);
    }

    if (failed_tests == 0) {
        fprintf(stderr, "Success.\n");
        return EXIT_SUCCESS;
    } else {
        fprintf(stderr, "Failed: %i tests.\n", failed_tests);
        return EXIT_FAILURE;
    }
}

static void usage(const char *name)
{
    fprintf(stdout, "%s: run AtomVM tests\n", name);
    fprintf(stdout, "%s [-h] [-s test1,test2] [-b] [test1 test2...]\n", name);
    fprintf(stdout, "  -h: display this message\n");
    fprintf(stdout, "  -s test1,test2: skip these tests\n");
    fprintf(stdout, "  -b: run tests against BEAM instead of AtomVM (erl in the PATH)\n");
    fprintf(stdout, "  test1 .. test2: specify tests to run (default to all)\n");
}

static void syntax_error(const char *name, const char *message)
{
    fprintf(stderr, "%s: syntax error\n%s\nTry %s -h for help\n", name, message, name);
}

int main(int argc, char **argv)
{
    char *name = argv[0];
    time_t seed = time(NULL);
    fprintf(stderr, "Seed is %li\n", seed);
    srand(seed);

    if (chdir(dirname(name)) != 0) {
        perror("Error: ");
        return EXIT_FAILURE;
    }

    int opt;
    bool beam = false;
    bool skip = false;
    char *skip_list = NULL;
    while ((opt = getopt(argc, argv, "hbs:")) != -1) {
        switch (opt) {
            case 'b':
                beam = true;
                break;
            case 's':
                skip_list = optarg;
                break;
            case 'h':
                usage(name);
                return 0;
            case ':':
            case '?':
                syntax_error(name, "Unknown option");
                return 1;
        }
    }
    int count = argc - optind;
    char **list = argv + optind;
    char **allocated_skip_list = NULL;
    if (count > 1 && skip_list != NULL) {
        syntax_error(name, "Option -s is incompatible with module names");
    }
    if (skip_list) {
        skip = true;
        // tokenize the list, first counting elements.
        char *skip_list_c = skip_list;
        count = 1;
        while (*skip_list_c) {
            if (*skip_list_c == ',') {
                count++;
            }
            skip_list_c++;
        }
        allocated_skip_list = malloc(count * sizeof(char *));
        int ix = 0;
        char *token = strtok(skip_list, ",");
        while (token && ix < count) {
            allocated_skip_list[ix++] = token;
            token = strtok(NULL, ",");
        }
        list = allocated_skip_list;
    }

    int result = test_modules_execution(beam, skip, count, list);
    if (allocated_skip_list) {
        free(allocated_skip_list);
    }
    return result;
}
