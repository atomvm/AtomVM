-module(pow31plusone).

-export([start/0, pow/2, powdiff/3]).

start() ->
    Res = powdiff(2, 31, 0) + 1,
    L = to_list(Res, []),
    make_int(L, 1, 0).

powdiff(B, M, N) ->
    pow(B, M) - pow(B, N).

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
