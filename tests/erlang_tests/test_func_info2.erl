-module(test_func_info2).
-export([start/0, id/1, f/1]).

start() ->
    try f(5) of
        AnyVal -> AnyVal
    catch
        error:function_clause -> 1;
        _:_ -> 0
    end.

id(X) ->
    X.

f(N) when N > 0 ->
    N * f(N - 1).
