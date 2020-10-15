-module(float2list2scientific).

-export([start/0, add/2, compare_list/2, id/1, float_to_list_badarg/2]).

start() ->
    F1 = id((add(id(2.5), id(-1.0)))),
    L1 = id(erlang:float_to_list(id(F1), [{scientific, 2}, compact])),
    F2 = id(id(F1) + id(id(0.5) * id(-1.0))),
    L2 = id(erlang:float_to_list(id(F2), [compact, {scientific, 1}])),
    F3 = id(id(F2) * id(-1.0)),
    L3 = id(erlang:float_to_list(id(F3), [{scientific, 3}, compact])),
    F4 = id(add(id(F2), id(F3))),
    L4 = id(erlang:float_to_list(id(F4), [{scientific, 0}, compact])),
    compare_list(L1, id("1.50e+00")) +
        compare_list(L2, id("1.0e+00")) * 2 +
        compare_list(L3, id("-1.000e+00")) * 4 +
        compare_list(L4, id("0e+00")) * 8 +
        float_to_list_badarg([1], [{scientific, 0}, compact]) * 16.

add(A, B) when is_float(A) and is_float(B) ->
    id(id(A) + id(B)).

compare_list([], []) ->
    1;
compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);
compare_list(_A, _B) ->
    0.

id(I) when is_float(I) ->
    I;
id(I) when is_list(I) ->
    I.

float_to_list_badarg(F, O) ->
    try erlang:float_to_list(F, O) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -1
    end.
