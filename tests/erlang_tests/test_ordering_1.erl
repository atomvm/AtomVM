-module(test_ordering_1).
-export([start/0, id/1, x/2]).

start() ->
    R = make_ref(),
    L1 = {[{[{R, [], "foo", 0, {<<"bar">>, [1, 2, 3]}, [[a], b], {{{{}}}}, [[[R], 1], 2], {pow(-2, 63), <<"foo">>}}]}]},
    L2 = {[{[{id(R), id([]), binary_to_list(id(<<"foo">>)), id(pow(5, 0)) - 1, {id(<<"bar">>), [pow(id(1), 0), id(pow(2, 1)), id(pow(3, 1))]}, id([[id(list_to_atom("a"))], b]), {{id({{}})}}, [[[id(id(R))], pow(6, id(0))], -pow(id(-2), 1)], {id(pow(-2, 63) + 1), list_to_binary(id("foo"))}}]}]},
    bool_to_n(L1 < L2).

pow(_N, 0) ->
    1;
pow(N, M) ->
    N * pow(N, M - 1).

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.

id(I) when is_binary(I) ->
    x(I, byte_size(I));
id(I) ->
    x(I, 5).

x(V, K) when K div 2 /= 0 ->
    x(V, K div 2);
x(V, 1) ->
    V.
