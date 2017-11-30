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

add_custom_target(erlang_test_modules DEPENDS
    add.beam
    fact.beam
    mutrec.beam
    morelabels.beam
    biggerintegers.beam
    biggerdifference.beam
    moreintegertests.beam
    send_receive.beam
)
