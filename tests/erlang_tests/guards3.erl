-module(guards3).

-export([start/0, do_something/1, loop/1]).

start() ->
    Port = do_open_port("echo", []),
    Pid = spawn(guards3, loop, [initial_state()]),
    do_something(Port) + do_something(Pid)*3 + do_something(2)*100.

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

initial_state() ->
    [].

loop(State) ->
    State.

do_something(S) when is_port(S) ->
    2;
do_something(S) when is_pid(S) ->
    1;
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
