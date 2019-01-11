##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    calendar
    erlang
    gen_server
    gen_statem
    gen_udp
    lists
    proplists
    timer
)

pack_archive(estdlib ${ERLANG_MODULES})
