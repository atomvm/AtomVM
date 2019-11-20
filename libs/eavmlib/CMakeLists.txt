##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(eavmlib)

include(BuildErlang)

set(ERLANG_MODULES
    console
    esp
    gpio
    i2c
    http_server
    json_encoder
    logger
    network_fsm
    spi
    timer_manager
    timestamp_util
)

pack_archive(eavmlib ${ERLANG_MODULES})
