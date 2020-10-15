-module(improper_length).

-export([start/0, id/1, add/2]).

start() ->
    L1 = id([id(add(id(1), id(0))), id(add(id(1), id(1))) | id(add(id(1), id(2)))]),
    L2 = id([id(add(id(1), id(0))) | id(<<"test">>)]),
    try_len(L1) * 1 + try_len(L2) * 2.

id(I) ->
    I.

add(A, B) ->
    id(A) + id(B).

try_len(L) when is_list(L) ->
    try length(L) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> 1024
    end.
