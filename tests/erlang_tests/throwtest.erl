-module(throwtest).
-export([start/0, t/2, id/1]).

start() ->
    t(5, 0) * 10.

t(A, B) ->
    try id(A) + id(B) of
        Value -> Value
    catch
        throw:test_throw -> -1;
        _:_ -> 0
    end.

id(0) ->
    throw(test_throw);

id(I) ->
    I.
