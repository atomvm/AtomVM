-module(test_recursion_and_try_catch).

-export([start/0, id/1, f/1, h/1, g/1]).

start() ->
    try f(10) of
        AnyNum -> AnyNum
    catch
        _:_ -> 0
    end.

id(X) ->
    X.

f(A) when A < 1 ->
    10 div A;
f(A) when A > 3 andalso A < 6 ->
    f(A - 1) * A;
f(A) ->
    Tmp = g(A),
    try id(f(id(A - 1))) * A + h(Tmp) of
        AnyNum -> AnyNum
    catch
        error:badarith -> Tmp;
        _:_ -> -1024
    end.

g(A) ->
    A rem 4.

h(A) when A < 4 ->
    0;
h(A) when A > 4 ->
    A.
