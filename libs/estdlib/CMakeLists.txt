##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    calendar
    console
    gen_udp
    lists
    timer
    erlang
)

pack_archive(estdlib ${ERLANG_MODULES})
