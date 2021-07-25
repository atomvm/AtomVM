-module(boxedlit).

-export([start/0, pow/2, get_k/1]).

start() ->
    %% test to ensure 2^30 can be parsed accurately out of a literal term
    K = get_k(#{k => 1073741824}),
    K = pow(2, 30).

pow(N, 0) when is_number(N) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

get_k(#{k := K}) ->
    K.
