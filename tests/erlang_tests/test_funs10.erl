-module(test_funs10).

-export([start/0, f/4]).

start() ->
    A = val("1"),
    Op = op(mul),
    L1 = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [8, 9, 10, 11]
    ],
    L2 = [
        [20, 30, 40, 50],
        [60, 70, 80, 90],
        [100, 110, 120, 130]
    ],
    f(L1, L2, 0, fun(P, Q) -> f(P, Q, A, fun(X, Y) -> Op(X, Y) end) end).

f([H1 | T1], [H2 | T2], Acc, F) when is_function(F) ->
    R = F(H1, H2),
    f(T1, T2, R + Acc, F);
f([], [], Acc, _F) ->
    Acc.

val(A) when is_list(A) ->
    -list_to_integer(A).

op(mul) ->
    fun(A, B) -> A * B end;
op(add) ->
    fun(A, B) -> A + B end;
op(_X) ->
    fun(_A, _B) -> 0 end.
