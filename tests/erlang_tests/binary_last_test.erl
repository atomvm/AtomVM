-module(binary_last_test).

-export([start/0, id/1, lastp10/1]).

start() ->
    lastp10(id(<<"HelloWorld">>)) + lastp10safe(<<>>) + lastp10safe(42) + lastp10safe({<<>>}).

lastp10(Bin) ->
    binary:last(Bin) + 10.

lastp10safe(Bin) ->
    try lastp10(Bin) of
        _Any -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

id(X) ->
    X.
