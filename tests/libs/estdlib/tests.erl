-module(tests).

-export([start/0]).

start() ->
    etest:test([
        test_lists
        , test_gen_server
        , test_gen_statem
        , test_gen_udp
        , test_io_lib
        , test_proplists
        , test_timer
        , test_supervisor
    ]).
