-module(binary_at_test).

-export([start/0, id/1, atp10/1]).

start() ->
    atp10(id(<<"HelloWorld">>)) + atp10safe(id(<<"">>)) + atp10safe(42).

atp10(Bin) ->
    binary:at(Bin, 4) + 10.

atp10safe(Bin) ->
    try atp10(Bin) of
        _Any -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

id(X) ->
    X.
