-module(copy_terms7).
-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {self(), test, 5},
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
        {Pid, test, N} ->
            Pid ! N * 2,
            ok;

        terminate ->
            terminate
    end.
