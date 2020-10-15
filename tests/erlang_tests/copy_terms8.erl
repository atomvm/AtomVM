-module(copy_terms8).

-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {test, self(), 2},
    Res =
        receive
            Any -> Any
        end,
    Pid ! terminate,
    Res.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {test, Pid, N} ->
            Pid ! N * 2,
            ok;
        terminate ->
            terminate
    end.
