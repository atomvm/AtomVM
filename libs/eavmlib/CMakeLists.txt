##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(eavmlib)

include(BuildErlang)

set(ERLANG_MODULES
    console
    gpio
    logger
    network_fsm
    spi
    timer_manager
    timestamp_util
)

pack_archive(eavmlib ${ERLANG_MODULES})
