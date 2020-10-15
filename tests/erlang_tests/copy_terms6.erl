-module(copy_terms6).

-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {self(), [1, 2, 3]},
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
        {Pid, ListA} ->
            Pid ! dot(ListA, [0, 1, 0], 0),
            ok;
        terminate ->
            terminate
    end.

dot([], [], Acc) ->
    Acc;
dot([HA | TA], [HB | TB], Acc) ->
    dot(TA, TB, Acc + HA * HB).
