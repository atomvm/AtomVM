-module(test_system_info).

-export([start/0, loop/1]).

start() ->
    assert(erlang:system_info(process_count) =:= 1),
    assert(erlang:system_info(port_count) =:= 0),
    assert(erlang:system_info(atom_count) > 0),
    assert(erlang:system_info(wordsize) > 0),
    assert(is_binary(erlang:system_info(system_architecture))),
    assert(erlang:system_info(some_wierd_unused_key) =:= undefined),

    Self = self(),
    Pid = spawn(?MODULE, loop, [Self]),
    receive
        ok -> ok
    end,

    assert(erlang:system_info(process_count) =:= 2),

    Pid ! {Self, stop},
    receive
        ok -> 0
    end.

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

assert(true) -> ok.
