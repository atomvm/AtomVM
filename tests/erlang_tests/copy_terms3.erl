-module(copy_terms3).

-export([start/0, loop/0, count_nestings/1]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {self(), {{{{{{}}}}}}},
    Count =
        receive
            Any -> Any
        end,
    Pid ! terminate,
    Count.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {Pid, ATuple} when is_tuple(ATuple) ->
            Pid ! count_nestings(ATuple),
            ok;
        terminate ->
            terminate
    end.

count_nestings(T) ->
    count_nestings(T, 0).

count_nestings({}, Acc) ->
    Acc;
count_nestings({T}, Acc) ->
    count_nestings(T, Acc + 1).
