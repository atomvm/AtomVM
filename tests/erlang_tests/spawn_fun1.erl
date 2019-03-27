-module(spawn_fun1).
-export([start/0, loop/0]).

start() ->
    Pid = spawn(fun loop/0),
    Pid ! {self(), 21},
    Double =
        receive
            Any -> Any
        end,
    Pid ! terminate,
    Double.

loop() ->
    case handle_request() of
        ok ->
            loop();

        terminate ->
            terminate
    end.

handle_request() ->
    receive
        {Pid, N} ->
            Pid ! N * 2,
            ok;

        terminate ->
            terminate
    end.
