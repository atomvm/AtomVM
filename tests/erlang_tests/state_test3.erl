-module(state_test3).

-export([start/0]).

start() ->
    Pid = spawn(state_test3_server, loop, [initial_state()]),
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
