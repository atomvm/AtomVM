-module(pow32_is_integer).

-export([start/0, pow/2]).

start() ->
    Res = (pow(-2, 31) + 1) * (pow(2, 1) - 3),
    L = to_list(Res, []),
    make_int(L, 1, 0).

pow(N, 0) when is_integer(N) ->
    1;
pow(N, M) when is_integer(N) and is_integer(M) ->
    N * pow(N, M - 1).

to_list(0, Acc) ->
    Acc;
to_list(N, Acc) when is_integer(N) ->
    to_list(N div 10, [N rem 10 | Acc]).

make_int([], N, Acc) when is_integer(N) ->
    Acc;
make_int([H | T], N, Acc) when is_integer(N) ->
    make_int(T, N + 2, H * N + Acc).
