##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(test_eavmlib)

include(BuildErlang)

set(ERLANG_MODULES
    test_logger
    test_timer_manager
)

pack_archive(test_eavmlib_lib ${ERLANG_MODULES})
pack_test(test_eavmlib eavmlib estdlib etest)
