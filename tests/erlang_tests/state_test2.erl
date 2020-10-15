-module(state_test2).

-export([start/0, loop/1]).

start() ->
    Pid = spawn(state_test2, loop, [initial_state()]),
    state_test2_sender:send_msgs(Pid).

initial_state() ->
    [].

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
