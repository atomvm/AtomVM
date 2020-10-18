-module(test_display).
-export([start/0]).

start() ->
    true = erlang:display([1, 2, 3, 4]),
    0.
