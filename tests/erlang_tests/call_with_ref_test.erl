-module(call_with_ref_test).

-export([start/0, loop/1]).

start() ->
    Pid = spawn(call_with_ref_test, loop, [initial_state()]),
    send_integer(Pid, 1),
    send_integer(Pid, 2),
    send_integer(Pid, 3),
    Value = get_integer(Pid),
    terminate(Pid),
    Value.

initial_state() ->
    [].

send_integer(Pid, Index) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, put, Index},
    receive
        {Ref, ReturnCode} -> ReturnCode;
        _Any -> error
    end.

get_integer(Pid) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, get},
    receive
        {Ref, Any} -> hd(Any)
    end.

terminate(Pid) ->
    Pid ! terminate.

loop(State) ->
    case handle_request(State) of
        nil ->
            ok;
        Value ->
            loop(Value)
    end.

handle_request(State) ->
    receive
        {Sender, Ref, put, Item} ->
            NextState = [Item] ++ State,
            Sender ! {Ref, ok},
            NextState;
        {Sender, Ref, get} ->
            Sender ! {Ref, State},
            State;
        terminate ->
            nil;
        _Any ->
            State
    end.
