-module(test_binary_to_list).

-export([start/0, id/1, concat_bin/2, safelen/1, compare_list/2]).

start() ->
    L1 = concat_bin(<<"This">>, <<" is">>),
    L2 = concat_bin(<<" a ">>, <<"list">>),
    L3 = concat_bin(<<".">>, <<"">>),
    TheList = L1 ++ L2 ++ L3,
    compare_list("This is a list.", TheList) + safelen({1, 2, 3}).

safelen(Bin) ->
    try binary_to_list(id(Bin)) of
        L -> length(L) + 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

concat_bin(Bin1, Bin2) ->
    binary_to_list(Bin1) ++ binary_to_list(Bin2).

compare_list([], []) ->
    1;
compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);
compare_list(_A, _B) ->
    0.

id(X) ->
    X.
