cmake_minimum_required (VERSION 2.6)
project (erlang_tests)

function(compile_erlang module_name)
    add_custom_command(
        OUTPUT ${module_name}.beam
        COMMAND erlc ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${module_name}.erl
        COMMENT "Compiling ${module_name}.erl"
    )
endfunction()

compile_erlang(add)
compile_erlang(fact)
compile_erlang(mutrec)
compile_erlang(morelabels)
compile_erlang(biggerintegers)
compile_erlang(biggerdifference)
compile_erlang(moreintegertests)
compile_erlang(send_receive)
compile_erlang(selval)
compile_erlang(byte_size_test)
compile_erlang(tuple)
compile_erlang(count_char)
compile_erlang(len_test)
compile_erlang(makelist_test)
compile_erlang(test_echo_driver)
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
compile_erlang(prime)
compile_erlang(match)
compile_erlang(if_test)
compile_erlang(sleep)
compile_erlang(hello_world)
compile_erlang(whereis_fail)
compile_erlang(try_noerror)
compile_erlang(catch_badmatch)
compile_erlang(catch_nocasematch)
compile_erlang(catch_noifmatch)
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
compile_erlang(timestamp)
compile_erlang(is_type)
compile_erlang(test_bitwise)
compile_erlang(test_bitwise2)
compile_erlang(test_gt_and_le)
compile_erlang(test_tuple_size)
compile_erlang(test_element)
compile_erlang(test_setelement)
compile_erlang(test_insert_element)


compile_erlang(complex_struct_size0)
compile_erlang(complex_struct_size1)
compile_erlang(complex_struct_size2)
compile_erlang(complex_struct_size3)

compile_erlang(nested_list_size0)
compile_erlang(nested_list_size1)
compile_erlang(nested_list_size2)
compile_erlang(nested_list_size3)
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


add_custom_target(erlang_test_modules DEPENDS
    add.beam
    fact.beam
    mutrec.beam
    morelabels.beam
    biggerintegers.beam
    biggerdifference.beam
    moreintegertests.beam
    send_receive.beam
    selval.beam
    byte_size_test.beam
    tuple.beam
    count_char.beam
    len_test.beam
    makelist_test.beam
    test_echo_driver.beam
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
    prime.beam
    match.beam
    if_test.beam
    sleep.beam
    hello_world.beam
    whereis_fail.beam
    try_noerror.beam
    catch_badmatch.beam
    catch_nocasematch.beam
    catch_noifmatch.beam
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
    timestamp.beam
    is_type.beam
    test_bitwise.beam
    test_bitwise2.beam
    test_gt_and_le.beam
    test_tuple_size.beam
    test_element.beam
    test_setelement.beam
    test_insert_element.beam

    complex_struct_size0.beam
    complex_struct_size1.beam
    complex_struct_size2.beam
    complex_struct_size3.beam

    nested_list_size0.beam
    nested_list_size1.beam
    nested_list_size2.beam
    nested_list_size3.beam

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
)
