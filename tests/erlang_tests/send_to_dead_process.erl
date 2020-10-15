-module(send_to_dead_process).

-export([start/0, double_and_terminate/0]).

start() ->
    Pid = spawn(?MODULE, double_and_terminate, []),
    Pid ! {self(), 10},
    Result =
        receive
            Any -> Any
        end,
    case Pid ! ping of
        ping ->
            Result;
        _Other ->
            0
    end.

double_and_terminate() ->
    receive
        {Pid, N} -> Pid ! N * 2
    end.
