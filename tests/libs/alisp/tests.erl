-module(tests).

-export([start/0]).

start() ->
    etest:test([test_alisp]).
