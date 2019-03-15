-module(test_min_heap_size).
-export([start/0, loop/1]).

start() ->
    Self = self(),
    Pid1 = spawn(?MODULE, loop, [Self]), receive ok -> ok end,
    {memory, Pid1MemorySize} = process_info(Pid1, memory),
    assert(Pid1MemorySize < 1024),
    Pid2 = spawn_opt(?MODULE, loop, [Self], [{min_heap_size, 1024}]), receive ok -> ok end,
    {memory, Pid2MemorySize} = process_info(Pid2, memory),
    assert(1024 =< Pid2MemorySize),
    Pid1 ! {Self, stop}, receive ok -> ok end,
    Pid2 ! {Self, stop}, receive ok -> ok end,
    0.

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

assert(true) -> ok.
