-module(tests).

-export([start/0]).

start() ->
    etest:test([
        test_lists
        , test_proplists
        , test_gen_server
        , test_timer
    ]).
