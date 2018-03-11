-module (state_test3_server).
-export([loop/1]).

loop(State) ->
    case handle_request(State) of
        nil ->
            ok;

        Value ->
            loop(Value)
    end.

handle_request(State) ->
    receive
        {put, Item} ->
            [Item] ++ State;

        {get, Pid} ->
            Pid ! State,
            State;

        terminate ->
            nil;

        _Any ->
            State
    end.
