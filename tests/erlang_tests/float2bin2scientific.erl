-module(float2bin2scientific).
-export([start/0, add/2, compare_bin/2, id/1, float_to_bin_badarg/2]).

start() ->
    F1 = id((add(id(2.5), id(-1.0)))),
    Bin1 = id(erlang:float_to_binary(id(F1), [{scientific, 2}, compact])),
    F2 = id(id(F1) + id(id(0.5) * id(-1.0))),
    Bin2 = id(erlang:float_to_binary(id(F2), [compact, {scientific, 1}])),
    F3 = id(id(F2) * id(-1.0)),
    Bin3 = id(erlang:float_to_binary(id(F3), [{scientific, 3}, compact])),
    F4 = id(add(id(F2), id(F3))),
    Bin4 = id(erlang:float_to_binary(id(F4), [{scientific, 0}, compact])),
    compare_bin(Bin1, id(<<"1.50e+00">>)) +
    compare_bin(Bin2, id(<<"1.0e+00">>)) * 2 +
    compare_bin(Bin3, id(<<"-1.000e+00">>)) * 4 +
    compare_bin(Bin4, id(<<"0e+00">>)) * 8 +
    float_to_bin_badarg({1}, [{scientific, 0}, compact]) * 16.

add(A, B) when is_float(A) and is_float(B) ->
    id(id(A) + id(B)).

compare_bin(Bin1, Bin2) when byte_size(Bin1) == byte_size(Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1);
compare_bin(_Bin1, _Bin2) ->
    0.

compare_bin(_Bin1, _Bin2, -1) ->
    1;

compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            compare_bin(Bin1, Bin2, Index - 1);
        _Any ->
            0
    end.

id(I) when is_float(I) ->
    I;
id(I) when is_binary(I) ->
    I.

float_to_bin_badarg(F, O) ->
    try erlang:float_to_binary(F, O) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -1
    end.
