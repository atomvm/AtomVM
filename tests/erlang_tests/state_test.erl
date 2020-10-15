-module(state_test).

-export([start/0, loop/1]).

start() ->
    Pid = spawn(state_test, loop, [initial_state()]),
    send_integer(Pid, 1),
    send_integer(Pid, 2),
    send_integer(Pid, 3),
    Pid ! {get, self()},
    Value =
        receive
            Any -> hd(Any)
        end,
    Pid ! terminate,
    Value.

initial_state() ->
    [].

send_integer(Pid, Index) ->
    Pid ! {put, Index}.

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
