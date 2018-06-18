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
)
