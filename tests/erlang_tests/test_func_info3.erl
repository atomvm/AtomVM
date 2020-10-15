-module(test_func_info3).

-export([start/0, id/1, f/1]).

start() ->
    id(f(id(5))).

id(X) ->
    X.

f(N) when N > 0 ->
    try f(N - 1) of
        AnyVal -> N * AnyVal
    catch
        error:function_clause -> 1;
        _:_ -> 0
    end.
