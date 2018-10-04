-module(memlimit).
-export([start/0, loop/1]).

start() ->
    Pid = spawn_opt(?MODULE, loop, [[]], [{max_heap_size, 1024}]),
    grow(Pid, 0).

grow(Pid, LastSize) ->
    Pid ! {self(), LastSize},
    receive
        NewSize ->
            grow(Pid, NewSize)

    after
        1000 ->
            LastSize
    end.

loop(Data) ->
    case handle_request(Data) of
        terminate ->
            terminate;

        NewData ->
            loop(NewData)
    end.

handle_request(Data) ->
    receive
        terminate ->
            terminate;

        {Pid, Item} ->
            NewData = [Item | Data],
            Pid ! erts_debug:flat_size(NewData),
            NewData
    end.
