##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(test_estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    test_gen_server
    test_gen_statem
    test_gen_udp
    test_io_lib
    test_lists
    test_string
    test_proplists
    test_timer
)

pack_archive(test_estdlib_lib ${ERLANG_MODULES})
pack_test(test_estdlib estdlib eavmlib etest)
