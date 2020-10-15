-module(test_func_info).

-export([start/0, id/1, f/3]).

start() ->
    try id(f(id(10), id(4), id(sub))) of
        AnyVal -> AnyVal
    catch
        error:function_clause -> 89;
        _:_ -> 0
    end.

id(X) ->
    X.

f(A, B, sum) when is_integer(A) andalso is_integer(B) ->
    A + B;
f(A, B, mul) when is_integer(A) andalso is_integer(B) ->
    A * B.
