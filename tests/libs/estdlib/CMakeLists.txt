##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(test_estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    test_timer
    test_lists
)

pack_archive(test_estdlib_lib ${ERLANG_MODULES})
pack_test(test_estdlib estdlib etest)
