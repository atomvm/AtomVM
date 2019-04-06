-module(binary_first_test).

-export([start/0, id/1, firstp10/1]).

start() ->
    firstp10(id(<<"HelloWorld">>)) + firstp10safe(<<>>) + firstp10safe(42) + firstp10safe({<<>>}).

firstp10(Bin) ->
    binary:first(Bin) + 10.


firstp10safe(Bin) ->
    try firstp10(Bin) of
        _Any -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

id(X) ->
    X.
