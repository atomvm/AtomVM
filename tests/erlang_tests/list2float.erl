-module(list2float).

-export([start/0, id/1, bin2float/1, float_cmp/2]).

start() ->
    F1 = bin2float(id("1.5e+03")),
    F2 = bin2float(id("1.5")),
    F3 = bin2float(id("2.0")),
    F4 = bin2float(id("-1.0")),
    F5 = bin2float(id("0.1")),
    F6 = bin2float(id("1.0e3")),
    F7 = bin2float(id("1.0e-3")),
    F8 = bin2float(id("0.0")),
    float_cmp(F1, id(1500.0)) +
        float_cmp(F2, id(1.5)) * 2 +
        float_cmp(F3, id(2.0)) * 4 +
        float_cmp(F4, id(1.0)) * 8 +
        float_cmp(F5, id(0.1)) * 16 +
        float_cmp(F6, id(1000.0)) * 32 +
        float_cmp(F7, id(0.001)) * 64 +
        float_cmp(F8, id(0.0)) * 128 +
        bin2float({}).

bin2float(F) ->
    try id(erlang:list_to_float(F)) of
        Res -> id(Res)
    catch
        error:badarg -> 256;
        _:_ -> -45
    end.

float_cmp(F1, F2) ->
    case abs(id(abs(id(F1)) - id(F2))) < id(0.00000000001) of
        true -> 1;
        false -> 0
    end.

id(I) when is_float(I) ->
    I;
id(I) when is_list(I) ->
    I.
