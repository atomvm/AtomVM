-module(test_integer_to_binary).

-export([start/0, some_calculation/2, compare_bin/3]).

start() ->
    NewBin1 = integer_to_binary(some_calculation(20, -2)),
    NewBin2 = integer_to_binary(some_calculation(1780, 0)),
    compare_bin(NewBin1, <<"-1">>) + compare_bin(NewBin2, <<"89">>) -
        compare_bin(NewBin1, <<"+1">>) * 10 - compare_bin(NewBin2, <<"88">>) * 100.

some_calculation(N, A) when is_integer(N) and is_integer(A) ->
    N div 20 + A;
some_calculation(_N, _A) ->
    -1.

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

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
