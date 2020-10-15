-module(improper_cmp).

-export([start/0, id/1, add/2]).

start() ->
    L1 = id([id(add(id(1), id(0))), id(add(id(1), id(1))) | id(add(id(1), id(2)))]),
    L2 = id([id(add(id(1), id(0))), id(add(id(1), id(1))) | id(add(id(1), id(3)))]),
    L3 = id([id(add(id(1), id(0))), id(add(id(1), id(1))) | id(<<"test">>)]),
    cmp(L1, L2) + cmp(L1, L3) * 2.

id(I) ->
    I.

add(A, B) ->
    id(A) + id(B).

cmp(L1, L2) when is_list(L1) and is_list(L2) ->
    if
        L1 == L2 -> 0;
        L1 > L2 -> -1;
        L1 < L2 -> 1
    end.
