cmake_minimum_required (VERSION 2.6)
project (erlang_tests)

function(compile_erlang module_name)
    add_custom_command(
        OUTPUT ${module_name}.beam
        COMMAND erlc ${module_name}.erl
        DEPENDS ${module_name}.erl
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
compile_erlang(state_test)
compile_erlang(booleans_test)
compile_erlang(booleans2_test)
compile_erlang(rem_and_comp_test)
compile_erlang(lowercase)
compile_erlang(huge)

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
    state_test.beam
    booleans_test.beam
    booleans2_test.beam
    rem_and_comp_test.beam
    lowercase.beam
    huge.beam
)
