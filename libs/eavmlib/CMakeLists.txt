##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(eavmlib)

include(BuildErlang)

set(ERLANG_MODULES
    console
    gpio
    logger
    network
)

pack_archive(eavmlib ${ERLANG_MODULES})
