##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    base64
    calendar
    gen_server
    gen_statem
    gen_udp
    gen_tcp
    supervisor
    inet
    io_lib
    io
    lists
    proplists
    string
    timer
    erlang
)

pack_archive(estdlib ${ERLANG_MODULES})

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/estdlib.avm DESTINATION lib/AtomVM/ebin)
