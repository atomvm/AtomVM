-module(test_list_processes).
-export([start/0, loop/1]).

start() ->
    Self = self(),
    IdList = [1,2,3],
    Pids = [spawn(?MODULE, loop, [Self]) || _ <- IdList],
    [receive ok -> ok end || _ <- IdList],
    test_list_processes(Pids, 0, Self).

test_list_processes([], Accum, _Self) ->
    Accum;
test_list_processes([Self | T], Accum, Self) ->
    test_list_processes(T, Accum, Self);
test_list_processes([Pid | T] = _PidList, Accum, Self) ->
    assert(contains(erlang:processes(), Pid)),
    Pid ! {Self, stop}, receive ok -> ok end,
    assert(not contains(erlang:processes(), Pid)),
    test_list_processes(T, Accum + 1, Self).

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok;
        _ ->
            loop(undefined)
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).


contains([], _E) ->
    false;
contains([E|_T], E) ->
    true;
contains([_|T], E) ->
    contains(T, E).

assert(true) -> ok.
