-module(guards2).

-export([start/0, do_something/1]).

start() ->
    do_something([]) + do_something(<<1, 2, 3>>) + do_something(2).

do_something(S) when is_pid(S) ->
    1;
do_something(S) when is_port(S) ->
    2;
do_something(S) when is_integer(S) ->
    4;
do_something(S) when is_number(S) ->
    8;
do_something(S) when is_list(S) ->
    16;
do_something(S) when is_binary(S) ->
    16;
do_something(S) when is_atom(S) ->
    32.
