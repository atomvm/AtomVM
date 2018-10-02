-module(copy_terms5).
-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {self(), {[1, 2, 3], [4, 5, 6]}},
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
        {Pid, {ListA, ListB}} ->
            Pid ! dot(ListA, ListB, 0),
            ok;

        terminate ->
            terminate
    end.

dot([], [], Acc) ->
    Acc;

dot([HA|TA], [HB|TB], Acc) ->
    dot(TA, TB, Acc + HA * HB).
