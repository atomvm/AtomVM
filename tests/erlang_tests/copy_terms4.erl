-module(copy_terms4).

-export([start/0, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {self(), {[{1, 0}, {2, 1}, {3, 4}], [{4, 2}, {5, 5}, {6, 1}]}},
    Res =
        receive
            {A, B} -> A * A + B * B
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
            Pid ! dot(ListA, ListB, {0, 0}),
            ok;
        terminate ->
            terminate
    end.

dot([], [], Acc) ->
    Acc;
dot([{HA_1, HA_2} | TA], [{HB_1, HB_2} | TB], Acc) ->
    dot(TA, TB, sum(Acc, {HA_1 * HB_1 - HA_2 * HB_2, HA_1 * HB_2 + HA_2 * HB_1})).

sum({A_1, A_2}, {B_1, B_2}) ->
    {A_1 + B_1, A_2 + B_2}.
