##
## Copyright (c) 2021 Davide Bettio
##

project(test_alisp)

include(BuildErlang)

set(ERLANG_MODULES
    test_alisp
)

pack_archive(test_alisp_lib ${ERLANG_MODULES})
pack_test(test_alisp estdlib eavmlib alisp etest)
