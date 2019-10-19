-module(float2list2).
-export([start/0, add/2, compare_list/2, id/1, float_to_bin_badarg/1]).

start() ->
    F1 = id((add(id(2.5), id(-1.0)))),
    Bin1 = id(erlang:float_to_list(id(F1), [{decimals, 2}, compact])),
    F2 = id(id(F1) + id(id(0.5) * id(-1.0))),
    Bin2 = id(erlang:float_to_list(id(F2), [compact, {decimals, 1}])),
    F3 = id(id(F2) * id(-1.0)),
    Bin3 = id(erlang:float_to_list(id(F3), [{decimals, 3}, compact])),
    F4 = id(add(id(F2), id(F3))),
    Bin4 = id(erlang:float_to_list(id(F4), [{decimals, 0}, compact])),
    compare_list(Bin1, id("1.5")) +
    compare_list(Bin2, id("1.0")) * 2 +
    compare_list(Bin3, id("-1.0")) * 4 +
    compare_list(Bin4, id("0")) * 8 +
    float_to_bin_badarg({1}) * 16.

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

float_to_bin_badarg(F) ->
    try erlang:float_to_binary(F) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -1
    end.
