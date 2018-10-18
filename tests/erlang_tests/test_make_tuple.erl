-module(test_make_tuple).
-export([start/0, f/1, g/1]).

start() ->
    g(f(2)) + g(f(0)).

f(N) ->
    erlang:make_tuple(N, hello).

g({}) ->
    1;

g({hello}) ->
    2;

g({hello, hello}) ->
    3;

g(_) ->
    100.
