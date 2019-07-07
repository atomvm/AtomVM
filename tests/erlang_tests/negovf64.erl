-module(negovf64).

-export([start/0, pow/2, neg_id/1, id/1]).

start() ->
    AlwaysBoxed1 = neg_id(pow(-2, 59)),
    L1 = to_list(AlwaysBoxed1, []),
    I1 = make_int(L1, 1, 0),
    AlwaysBoxed2 = abs(pow(-2, 59)),
    L2 = to_list(AlwaysBoxed2, []),
    I2 = make_int(L2, 1, 0),
    NotAlwaysBoxed1 = neg_id(AlwaysBoxed1),
    L3 = to_list(NotAlwaysBoxed1, []),
    I3 = make_int(L3, 1, 0),
    NotAlwaysBoxed2 = neg_id(AlwaysBoxed2),
    L4 = to_list(NotAlwaysBoxed2, []),
    I4 = make_int(L4, 1, 0),
    I1 + I2 * 2 + I3 * 3 + I4 * 5.

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

to_list(0, Acc) ->
    Acc;
to_list(N, Acc) ->
    to_list(N div 10, [N rem 10 | Acc]).

make_int([], _N, Acc) ->
    Acc;
make_int([H | T], N, Acc) ->
    make_int(T, N + 2, H * N + Acc).

neg_id(0) ->
    test;
neg_id(N) when is_integer(N) ->
    -id(N).

id(N) when is_integer(N) ->
    N.
