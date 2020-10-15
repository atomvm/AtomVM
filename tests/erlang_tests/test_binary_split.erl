-module(test_binary_split).

-export([start/0, split_compare/3, split_compare/2, compare_bin/2, fail_split/1]).

start() ->
    split_compare(<<"Hello:World">>, <<"Hello">>, <<"World">>) +
        split_compare(<<"Hello:::World:">>, <<"Hello">>, <<"::World:">>) +
        split_compare(<<"Test:">>, <<"Test">>, <<>>) +
        split_compare(<<":">>, <<>>, <<>>) +
        split_compare(<<>>, <<>>) +
        split_compare(<<"Test">>, <<>>) +
        split_compare2(<<"Test">>, <<>>) +
        split_compare2(<<"helloSEPARATORworld">>, <<"hello">>, <<"world">>) +
        fail_split(<<>>) +
        fail_split({1, 2}) +
        fail_split2({1, 2}).

split_compare(Bin, Part1) ->
    [A] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A).

split_compare(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

split_compare2(Bin, Part1) ->
    [A] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A).

split_compare2(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

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

fail_split(Separator) ->
    try binary:split(<<"TESTBIN">>, Separator) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.

fail_split2(Bin) ->
    try binary:split(Bin, <<"TESTSEPARATOR">>) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.
