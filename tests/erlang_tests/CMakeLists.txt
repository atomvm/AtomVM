#
# This file is part of AtomVM.
#
# Copyright 2017-2021 Davide Bettio <davide@uninstall.it>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

cmake_minimum_required (VERSION 3.13)
project (erlang_tests)

function(compile_erlang module_name)
    add_custom_command(
        OUTPUT ${module_name}.beam
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        COMMENT "Compiling ${module_name}.erl"
    )
endfunction()

set(TO_HRL_PATH ${CMAKE_CURRENT_LIST_DIR})

function(generate_hrl out_file def_name in_file)
    add_custom_command(
        OUTPUT ${out_file}
        COMMAND escript ${TO_HRL_PATH}/to_hrl.erl ${in_file} ${def_name} ${out_file}
        DEPENDS ${in_file}
        COMMENT "Generating ${out_file}"
    )
endfunction()

add_subdirectory(code_load)

compile_erlang(add)
compile_erlang(fact)
compile_erlang(mutrec)
compile_erlang(morelabels)
compile_erlang(biggerintegers)
compile_erlang(biggerdifference)
compile_erlang(moreintegertests)
compile_erlang(send_receive)
compile_erlang(send_to_dead_process)
compile_erlang(selval)
compile_erlang(byte_size_test)
compile_erlang(tuple)
compile_erlang(count_char)
compile_erlang(len_test)
compile_erlang(makelist_test)
compile_erlang(test_echo_driver)
compile_erlang(test_close_console_driver)
compile_erlang(test_close_echo_driver)
compile_erlang(test_regecho_driver)
compile_erlang(test_send_nif_and_echo)
compile_erlang(state_test)
compile_erlang(booleans_test)
compile_erlang(booleans2_test)
compile_erlang(rem_and_comp_test)
compile_erlang(lowercase)
compile_erlang(huge)
compile_erlang(patternmatchfunc)
compile_erlang(moda)
compile_erlang(modb)
compile_erlang(modc)
compile_erlang(state_test2)
compile_erlang(state_test2_sender)
compile_erlang(state_test3)
compile_erlang(state_test3_server)
compile_erlang(guards1)
compile_erlang(guards2)
compile_erlang(guards3)
compile_erlang(guards4)
compile_erlang(guards5)
compile_erlang(test_guards_do_not_raise)
compile_erlang(test_min_max_guard)
compile_erlang(prime)
compile_erlang(match)
compile_erlang(if_test)
compile_erlang(sleep)
compile_erlang(hello_world)
compile_erlang(whereis_dead_process)
compile_erlang(whereis_fail)
compile_erlang(register_unregister)
compile_erlang(try_noerror)
compile_erlang(catch_badmatch)
compile_erlang(catch_nocasematch)
compile_erlang(catch_noifmatch)
compile_erlang(try_catch_test)
compile_erlang(list_concat)
compile_erlang(make_ref_test)
compile_erlang(is_ref_test)
compile_erlang(tagged_tuple_test)
compile_erlang(call_with_ref_test)
compile_erlang(just_receive_test)
compile_erlang(gen_server_like_test)
compile_erlang(external_proplist_test)
compile_erlang(compact15bitsinteger)
compile_erlang(negatives)
compile_erlang(compact23bitsinteger)
compile_erlang(compact27bitsinteger)
compile_erlang(compact23bitsneginteger)
compile_erlang(negatives2)
compile_erlang(datetime)
compile_erlang(test_system_time)
compile_erlang(is_type)
compile_erlang(test_bitshift)
compile_erlang(test_bitwise)
compile_erlang(test_bitwise2)
compile_erlang(test_boolean)
compile_erlang(test_gt_and_le)
compile_erlang(test_tuple_size)
compile_erlang(test_element)
compile_erlang(test_setelement)
compile_erlang(test_insert_element)
compile_erlang(test_delete_element)
compile_erlang(test_tuple_to_list)
compile_erlang(test_make_tuple)
compile_erlang(test_make_list)
compile_erlang(test_list_gc)
compile_erlang(test_list_processes)
compile_erlang(test_tl)
compile_erlang(test_list_to_atom)
compile_erlang(test_list_to_existing_atom)
compile_erlang(test_lists_reverse)
compile_erlang(test_binary_to_atom)
compile_erlang(test_binary_to_existing_atom)
compile_erlang(test_atom_to_list)
compile_erlang(test_display)
compile_erlang(test_integer_to_list)
compile_erlang(test_list_to_integer)
compile_erlang(test_abs)
compile_erlang(test_is_process_alive)
compile_erlang(test_is_not_type)
compile_erlang(test_badarith)
compile_erlang(test_badarith2)
compile_erlang(test_badarith3)
compile_erlang(test_badarith4)
compile_erlang(test_bif_badargument)
compile_erlang(test_bif_badargument2)
compile_erlang(test_bif_badargument3)
compile_erlang(test_tuple_nifs_badargs)
compile_erlang(test_apply)
compile_erlang(test_apply_last)
compile_erlang(test_monitor)
compile_erlang(test_set_tuple_element)
compile_erlang(test_timestamp)
compile_erlang(long_atoms)
compile_erlang(test_concat_badarg)
compile_erlang(register_and_whereis_badarg)
compile_erlang(test_send)
compile_erlang(test_open_port_badargs)
compile_erlang(echo)
compile_erlang(pingpong)
compile_erlang(prime_ext)
compile_erlang(prime_smp)
compile_erlang(test_try_case_end)
compile_erlang(test_exception_classes)
compile_erlang(test_recursion_and_try_catch)
compile_erlang(test_func_info)
compile_erlang(test_func_info2)
compile_erlang(test_func_info3)
compile_erlang(test_process_info)
compile_erlang(test_min_heap_size)
compile_erlang(test_heap_growth)
compile_erlang(test_system_flag)
compile_erlang(test_system_info)
compile_erlang(test_binary_to_term)
compile_erlang(test_selective_receive)
compile_erlang(test_timeout_not_integer)

compile_erlang(test_funs0)
compile_erlang(test_funs1)
compile_erlang(test_funs2)
compile_erlang(test_funs3)
compile_erlang(test_funs4)
compile_erlang(test_funs5)
compile_erlang(test_funs6)
compile_erlang(test_funs7)
compile_erlang(test_funs8)
compile_erlang(test_funs9)
compile_erlang(test_funs10)
compile_erlang(test_funs11)

compile_erlang(test_make_fun3)

compile_erlang(complex_struct_size0)
compile_erlang(complex_struct_size1)
compile_erlang(complex_struct_size2)
compile_erlang(complex_struct_size3)
compile_erlang(complex_struct_size4)

compile_erlang(nested_list_size0)
compile_erlang(nested_list_size1)
compile_erlang(nested_list_size2)
compile_erlang(nested_list_size3)
compile_erlang(nested_list_size4)
compile_erlang(nested_tuple_size0)
compile_erlang(nested_tuple_size1)
compile_erlang(nested_tuple_size2)
compile_erlang(nested_tuple_size3)
compile_erlang(nested_tuple_size4)

compile_erlang(simple_list_size0)
compile_erlang(simple_list_size1)

compile_erlang(tuple_size0)
compile_erlang(tuple_size1)
compile_erlang(tuple_size2)
compile_erlang(tuple_size3)
compile_erlang(tuple_size4)
compile_erlang(tuple_size5)
compile_erlang(tuple_size6)

compile_erlang(tuples_and_list_size0)
compile_erlang(tuples_and_list_size1)
compile_erlang(tuples_and_list_size2)

compile_erlang(make_garbage0)
compile_erlang(make_garbage1)
compile_erlang(make_garbage2)
compile_erlang(make_garbage3)
compile_erlang(make_garbage4)
compile_erlang(make_garbage5)
compile_erlang(make_garbage6)
compile_erlang(make_garbage7)

compile_erlang(copy_terms0)
compile_erlang(copy_terms1)
compile_erlang(copy_terms2)
compile_erlang(copy_terms3)
compile_erlang(copy_terms4)
compile_erlang(copy_terms5)
compile_erlang(copy_terms6)
compile_erlang(copy_terms7)
compile_erlang(copy_terms8)
compile_erlang(copy_terms9)
compile_erlang(copy_terms10)
compile_erlang(copy_terms11)
compile_erlang(copy_terms12)
compile_erlang(copy_terms13)
compile_erlang(copy_terms14)
compile_erlang(copy_terms15)
compile_erlang(copy_terms16)
compile_erlang(copy_terms17)
compile_erlang(copy_terms18)

compile_erlang(memlimit)

compile_erlang(spawn_fun1)
compile_erlang(spawn_fun2)
compile_erlang(spawn_fun3)

compile_erlang(binary_at_test)
compile_erlang(binary_first_test)
compile_erlang(binary_last_test)

compile_erlang(test_integer_to_binary)
compile_erlang(test_list_to_binary)
compile_erlang(test_binary_to_list)
compile_erlang(test_atom_to_binary)
compile_erlang(test_unicode)

compile_erlang(test_binary_part)
compile_erlang(test_binary_split)

compile_erlang(plusone)
compile_erlang(plusone2)
compile_erlang(minusone)
compile_erlang(minusone2)
compile_erlang(int28mul)
compile_erlang(int28mulneg)
compile_erlang(int28mulneg2)
compile_erlang(int28mulneg2)
compile_erlang(negdiv)
compile_erlang(absovf)
compile_erlang(negovf)

compile_erlang(plusone3)
compile_erlang(plusone4)
compile_erlang(bigfact)
compile_erlang(bigfact2)
compile_erlang(bigfact3)
compile_erlang(boxedabs)
compile_erlang(boxedneg)
compile_erlang(boxedmul)
compile_erlang(boxedlit)
compile_erlang(pow32)
compile_erlang(pow64)
compile_erlang(pow32_is_integer)
compile_erlang(pow64_is_integer)
compile_erlang(addovf32)
compile_erlang(subovf32)
compile_erlang(negovf32)
compile_erlang(addovf64)
compile_erlang(subovf64)
compile_erlang(negovf64)
compile_erlang(powsquare)
compile_erlang(minuspow31minusone)
compile_erlang(pow31plusone)
compile_erlang(minuspow31divminusone)
compile_erlang(pow31abs)
compile_erlang(minuspow31abs)
compile_erlang(pow31minusoneabs)
compile_erlang(minuspow31plusoneabs)
compile_erlang(minuspow31plustwoabs)
compile_erlang(minuspow63plusoneabs)
compile_erlang(minuspow63plustwoabs)
compile_erlang(literal_test0)
compile_erlang(literal_test1)

compile_erlang(test_list_eq)
compile_erlang(test_tuple_eq)
compile_erlang(test_tuple_list_eq)
compile_erlang(test_list_tuple_eq)
compile_erlang(test_ref_eq)
compile_erlang(test_binary_eq)
compile_erlang(test_bigint_eq)

compile_erlang(test_binaries_ordering)
compile_erlang(test_lists_ordering)
compile_erlang(test_tuples_ordering)
compile_erlang(test_types_ordering)
compile_erlang(test_bigintegers_ordering)
compile_erlang(test_refs_ordering)
compile_erlang(test_atom_ordering)
compile_erlang(test_pids_ordering)
compile_erlang(test_list_match)
compile_erlang(test_match)
compile_erlang(test_ordering_0)
compile_erlang(test_ordering_1)
compile_erlang(test_bs)
compile_erlang(test_bs_int)
compile_erlang(test_bs_int_unaligned)
compile_erlang(test_bs_utf)
compile_erlang(test_catch)
compile_erlang(test_gc)
compile_erlang(test_raise)
compile_erlang(test_map)

compile_erlang(ceilint)
compile_erlang(ceilbadarg)
compile_erlang(floorint)
compile_erlang(floorbadarg)
compile_erlang(roundint)
compile_erlang(roundbadarg)
compile_erlang(truncint)
compile_erlang(truncbadarg)
compile_erlang(ceilfloat)
compile_erlang(ceilfloatovf)
compile_erlang(floorfloat)
compile_erlang(floorfloatovf)
compile_erlang(roundfloat)
compile_erlang(roundfloatovf)
compile_erlang(truncfloat)
compile_erlang(truncfloatovf)
compile_erlang(floatadd)
compile_erlang(floataddovf)
compile_erlang(floatsub)
compile_erlang(floatsubovf)
compile_erlang(floatmul)
compile_erlang(floatmulovf)
compile_erlang(floatneg)
compile_erlang(floatabs)
compile_erlang(floatdiv)
compile_erlang(floatmath)
compile_erlang(floatext)

compile_erlang(boxed_is_not_float)
compile_erlang(float_is_float)
compile_erlang(float_is_number)

compile_erlang(float2list)
compile_erlang(float2bin)
compile_erlang(float2bin2decimals)
compile_erlang(float2bin2scientific)
compile_erlang(float2bin2)
compile_erlang(float2list2decimals)
compile_erlang(float2list2scientific)
compile_erlang(float2list2)
compile_erlang(bin2float)
compile_erlang(list2float)

compile_erlang(test_fp_allocate_heap_zero)
compile_erlang(test_bs_init2_heap_allocation)

compile_erlang(improper_concat)
compile_erlang(improper_cmp)
compile_erlang(improper_literal)
compile_erlang(improper_length)

compile_erlang(jsonish_encode)
compile_erlang(iolist_concat_bin)
compile_erlang(binary_is_iolist)

compile_erlang(raise_badmatch)
compile_erlang(raise_case_end)
compile_erlang(raise_if_end)
compile_erlang(catch_from_other_module)
compile_erlang(throwtest)

compile_erlang(test_tuple_is_not_map)

compile_erlang(try_error_nif)
compile_erlang(try_error2_nif)

compile_erlang(is_fun_2_with_frozen)
compile_erlang(is_fun_2_with_frozen2)

compile_erlang(function_reference_decode)
compile_erlang(makefunref)
compile_erlang(fail_apply)
compile_erlang(fail_apply_last)

compile_erlang(pid_to_list_test)
compile_erlang(ref_to_list_test)
compile_erlang(test_binary_to_integer)

compile_erlang(count_char_bs)
compile_erlang(count_char2_bs)
compile_erlang(count_char3_bs)
compile_erlang(count_pairs)
compile_erlang(decode_mqtt)
compile_erlang(decode_int24)
compile_erlang(decode_int32)
compile_erlang(decode_int48)

compile_erlang(large_int_literal)

compile_erlang(test_base64)
compile_erlang(test_dict)

compile_erlang(alisp)
compile_erlang(sexp_parser)
compile_erlang(sexp_lexer)
compile_erlang(test_function_exported)
compile_erlang(test_list_to_tuple)

compile_erlang(bs_context_to_binary_with_offset)
compile_erlang(bs_restore2_start_offset)
compile_erlang(test_refc_binaries)
compile_erlang(test_sub_binaries)
compile_erlang(test_throw_call_ext_last)
compile_erlang(bs_append_extra_words)

compile_erlang(test_monotonic_time)

compile_erlang(exactly_eq)
compile_erlang(map_comparisons)
compile_erlang(tuple_comparisons)

compile_erlang(spawn_opt_monitor_normal)
compile_erlang(spawn_opt_monitor_throw)
compile_erlang(spawn_opt_demonitor_normal)
compile_erlang(spawn_opt_link_normal)
compile_erlang(spawn_opt_link_throw)
compile_erlang(spawn_opt_monitor_error)
compile_erlang(link_kill_parent)
compile_erlang(link_throw)
compile_erlang(unlink_error)
compile_erlang(trap_exit_flag)
compile_erlang(test_exit1)
compile_erlang(test_exit2)

compile_erlang(test_stacktrace)
compile_erlang(small_big_ext)
compile_erlang(test_crypto)

compile_erlang(test_code_load_binary)
compile_erlang(test_code_load_abs)
compile_erlang(test_add_avm_pack_binary)
compile_erlang(test_add_avm_pack_file)
compile_erlang(test_close_avm_pack)

compile_erlang(test_module_info)

compile_erlang(int64_build_binary)

compile_erlang(test_crypto_strong_rand_bytes)
compile_erlang(test_atomvm_random)

compile_erlang(float_decode)

add_custom_target(erlang_test_modules DEPENDS
    code_load_files

    add.beam
    fact.beam
    mutrec.beam
    morelabels.beam
    biggerintegers.beam
    biggerdifference.beam
    moreintegertests.beam
    send_receive.beam
    send_to_dead_process.beam
    selval.beam
    byte_size_test.beam
    tuple.beam
    count_char.beam
    len_test.beam
    makelist_test.beam
    test_echo_driver.beam
    test_close_console_driver.beam
    test_close_echo_driver.beam
    test_regecho_driver.beam
    test_send_nif_and_echo.beam
    state_test.beam
    booleans_test.beam
    booleans2_test.beam
    rem_and_comp_test.beam
    lowercase.beam
    huge.beam
    patternmatchfunc.beam
    moda.beam
    modb.beam
    modc.beam
    state_test2.beam
    state_test2_sender.beam
    state_test3.beam
    state_test3_server.beam
    guards1.beam
    guards2.beam
    guards3.beam
    guards4.beam
    guards5.beam
    test_guards_do_not_raise.beam
    test_min_max_guard.beam
    prime.beam
    match.beam
    if_test.beam
    sleep.beam
    hello_world.beam
    whereis_dead_process.beam
    whereis_fail.beam
    register_unregister.beam
    try_noerror.beam
    catch_badmatch.beam
    catch_nocasematch.beam
    catch_noifmatch.beam
    try_catch_test.beam
    list_concat.beam
    make_ref_test.beam
    is_ref_test.beam
    tagged_tuple_test.beam
    call_with_ref_test.beam
    just_receive_test.beam
    gen_server_like_test.beam
    external_proplist_test.beam
    compact15bitsinteger.beam
    negatives.beam
    compact23bitsinteger.beam
    compact27bitsinteger.beam
    compact23bitsneginteger.beam
    negatives2.beam
    datetime.beam
    test_system_time.beam
    is_type.beam
    test_bitshift.beam
    test_bitwise.beam
    test_bitwise2.beam
    test_boolean.beam
    test_gt_and_le.beam
    test_tuple_size.beam
    test_element.beam
    test_setelement.beam
    test_insert_element.beam
    test_delete_element.beam
    test_tuple_to_list.beam
    test_make_tuple.beam
    test_make_list.beam
    test_list_gc.beam
    test_list_processes.beam
    test_tl.beam
    test_list_to_atom.beam
    test_list_to_existing_atom.beam
    test_lists_reverse.beam
    test_binary_to_atom.beam
    test_binary_to_existing_atom.beam
    test_atom_to_list.beam
    test_display.beam
    test_integer_to_list.beam
    test_list_to_integer.beam
    test_abs.beam
    test_is_process_alive.beam
    test_is_not_type.beam
    test_badarith.beam
    test_badarith2.beam
    test_badarith3.beam
    test_badarith4.beam
    test_bif_badargument.beam
    test_bif_badargument2.beam
    test_bif_badargument3.beam
    test_tuple_nifs_badargs.beam
    test_apply.beam
    test_apply_last.beam
    test_monitor.beam
    test_set_tuple_element.beam
    test_timestamp.beam
    long_atoms.beam
    test_concat_badarg.beam
    register_and_whereis_badarg.beam
    test_send.beam
    test_open_port_badargs.beam
    echo.beam
    pingpong.beam
    prime_ext.beam
    prime_smp.beam
    test_try_case_end.beam
    test_exception_classes.beam
    test_recursion_and_try_catch.beam
    test_func_info.beam
    test_func_info2.beam
    test_func_info3.beam
    test_process_info.beam
    test_min_heap_size.beam
    test_heap_growth.beam
    test_system_flag.beam
    test_system_info.beam
    test_binary_to_term.beam
    test_selective_receive.beam
    test_timeout_not_integer.beam

    test_funs0.beam
    test_funs1.beam
    test_funs2.beam
    test_funs3.beam
    test_funs4.beam
    test_funs5.beam
    test_funs6.beam
    test_funs7.beam
    test_funs8.beam
    test_funs9.beam
    test_funs10.beam
    test_funs11.beam

    test_make_fun3.beam

    complex_struct_size0.beam
    complex_struct_size1.beam
    complex_struct_size2.beam
    complex_struct_size3.beam
    complex_struct_size4.beam

    nested_list_size0.beam
    nested_list_size1.beam
    nested_list_size2.beam
    nested_list_size3.beam
    nested_list_size4.beam

    nested_tuple_size0.beam
    nested_tuple_size1.beam
    nested_tuple_size2.beam
    nested_tuple_size3.beam
    nested_tuple_size4.beam

    simple_list_size0.beam
    simple_list_size1.beam

    tuple_size0.beam
    tuple_size1.beam
    tuple_size2.beam
    tuple_size3.beam
    tuple_size4.beam
    tuple_size5.beam
    tuple_size6.beam

    tuples_and_list_size0.beam
    tuples_and_list_size1.beam
    tuples_and_list_size2.beam

    make_garbage0.beam
    make_garbage1.beam
    make_garbage2.beam
    make_garbage3.beam
    make_garbage4.beam
    make_garbage5.beam
    make_garbage6.beam
    make_garbage7.beam

    copy_terms0.beam
    copy_terms1.beam
    copy_terms2.beam
    copy_terms3.beam
    copy_terms4.beam
    copy_terms5.beam
    copy_terms6.beam
    copy_terms7.beam
    copy_terms8.beam
    copy_terms9.beam
    copy_terms10.beam
    copy_terms11.beam
    copy_terms12.beam
    copy_terms13.beam
    copy_terms14.beam
    copy_terms15.beam
    copy_terms16.beam
    copy_terms17.beam
    copy_terms18.beam

    memlimit.beam

    spawn_fun1.beam
    spawn_fun2.beam
    spawn_fun3.beam

    binary_at_test.beam
    binary_first_test.beam
    binary_last_test.beam

    test_integer_to_binary.beam
    test_list_to_binary.beam
    test_binary_to_list.beam
    test_atom_to_binary.beam
    test_unicode.beam

    test_binary_part.beam
    test_binary_split.beam

    plusone.beam
    plusone2.beam
    minusone.beam
    minusone2.beam
    int28mul.beam
    int28mulneg.beam
    int28mulneg2.beam
    negdiv.beam
    absovf.beam
    negovf.beam

    plusone3.beam
    plusone4.beam
    bigfact.beam
    bigfact2.beam
    bigfact3.beam
    boxedabs.beam
    boxedneg.beam
    boxedmul.beam
    boxedlit.beam
    pow32.beam
    pow64.beam

    pow32_is_integer.beam
    pow64_is_integer.beam
    addovf32.beam
    subovf32.beam
    negovf32.beam
    addovf64.beam
    subovf64.beam
    negovf64.beam
    powsquare.beam
    minuspow31minusone.beam
    pow31plusone.beam
    minuspow31divminusone.beam
    pow31abs.beam
    minuspow31abs.beam
    pow31minusoneabs.beam
    minuspow31plusoneabs.beam
    minuspow31plustwoabs.beam
    minuspow63plusoneabs.beam
    minuspow63plustwoabs.beam

    literal_test0.beam
    literal_test1.beam

    test_list_eq.beam
    test_tuple_eq.beam
    test_tuple_list_eq.beam
    test_list_tuple_eq.beam
    test_ref_eq.beam
    test_binary_eq.beam
    test_bigint_eq.beam

    test_binaries_ordering.beam
    test_lists_ordering.beam
    test_tuples_ordering.beam
    test_types_ordering.beam
    test_bigintegers_ordering.beam
    test_refs_ordering.beam
    test_atom_ordering.beam
    test_pids_ordering.beam
    test_list_match.beam
    test_match.beam
    test_ordering_0.beam
    test_ordering_1.beam
    test_bs.beam
    test_bs_int.beam
    test_bs_int_unaligned.beam
    test_bs_utf.beam
    test_catch.beam
    test_gc.beam
    test_raise.beam
    test_map.beam

    ceilint.beam
    ceilbadarg.beam
    floorint.beam
    floorbadarg.beam
    roundint.beam
    roundbadarg.beam
    truncint.beam
    truncbadarg.beam
    ceilfloat.beam
    ceilfloatovf.beam
    floorfloat.beam
    floorfloatovf.beam
    roundfloat.beam
    roundfloatovf.beam
    truncfloat.beam
    truncfloatovf.beam
    floatadd.beam
    floataddovf.beam
    floatsub.beam
    floatsubovf.beam
    floatmul.beam
    floatmulovf.beam
    floatneg.beam
    floatabs.beam
    floatdiv.beam
    floatmath.beam
    floatext.beam

    boxed_is_not_float.beam
    float_is_float.beam
    float_is_number.beam

    float2list.beam
    float2bin.beam
    float2bin2decimals.beam
    float2bin2scientific.beam
    float2bin2.beam
    float2list2decimals.beam
    float2list2scientific.beam
    float2list2.beam
    bin2float.beam
    list2float.beam

    test_fp_allocate_heap_zero.beam
    test_bs_init2_heap_allocation.beam

    improper_concat.beam
    improper_cmp.beam
    improper_literal.beam
    improper_length.beam

    jsonish_encode.beam
    iolist_concat_bin.beam
    binary_is_iolist.beam

    raise_badmatch.beam
    raise_case_end.beam
    raise_if_end.beam
    catch_from_other_module.beam
    throwtest.beam

    test_tuple_is_not_map.beam

    try_error_nif.beam
    try_error2_nif.beam

    is_fun_2_with_frozen.beam
    is_fun_2_with_frozen2.beam

    function_reference_decode.beam
    makefunref.beam
    fail_apply.beam
    fail_apply_last.beam

    pid_to_list_test.beam
    ref_to_list_test.beam
    test_binary_to_integer.beam

    count_char_bs.beam
    count_char2_bs.beam
    count_char3_bs.beam
    count_pairs.beam
    decode_mqtt.beam
    decode_int24.beam
    decode_int32.beam
    decode_int48.beam

    large_int_literal.beam

    test_base64.beam
    test_dict.beam

    alisp.beam
    sexp_parser.beam
    sexp_lexer.beam

    test_refc_binaries.beam
    test_sub_binaries.beam
    test_throw_call_ext_last.beam
    test_function_exported.beam
    test_list_to_tuple.beam

    bs_context_to_binary_with_offset.beam
    bs_restore2_start_offset.beam
    bs_append_extra_words.beam

    test_monotonic_time.beam

    exactly_eq.beam
    map_comparisons.beam
    tuple_comparisons.beam

    spawn_opt_monitor_normal.beam
    spawn_opt_monitor_throw.beam
    spawn_opt_demonitor_normal.beam
    spawn_opt_link_normal.beam
    spawn_opt_link_throw.beam
    spawn_opt_monitor_error.beam
    link_kill_parent.beam
    link_throw.beam
    unlink_error.beam
    trap_exit_flag.beam
    test_exit1.beam
    test_exit2.beam

    test_stacktrace.beam

    small_big_ext.beam
    test_crypto.beam

    test_code_load_binary.beam
    test_code_load_abs.beam
    test_add_avm_pack_binary.beam
    test_add_avm_pack_file.beam
    test_close_avm_pack.beam

    test_module_info.beam

    int64_build_binary.beam

    test_crypto_strong_rand_bytes.beam
    test_atomvm_random.beam

    float_decode.beam
)
