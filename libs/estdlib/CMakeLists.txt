##
## Copyright (c) 2018 Fred Dushin <fred@dushin.net>
##

project(estdlib)

include(BuildErlang)

set(ERLANG_MODULES
    avm_calendar
    avm_gen_server
    avm_gen_statem
    avm_gen_udp
    avm_lists
    avm_proplists
    avm_timer
        erlang
)

pack_archive(estdlib ${ERLANG_MODULES})
