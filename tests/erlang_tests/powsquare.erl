-module(powsquare).

-export([start/0, pow/2]).

start() ->
    Res = pow(-2, 27) * pow(-2, 27),
    L = to_list(Res, []),
    make_int(L, 1, 0).

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
