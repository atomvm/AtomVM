-module(function_reference_decode).

-export([start/0, g/1, h/1, id/1]).

start() ->
    A = [131, 113, 100, 0, 6, 101, 114, 108, 97, 110, 103, 100, 0, 15, 105, 110, 116,
      101, 103, 101, 114, 95, 116, 111, 95, 108, 105, 115, 116, 97, 1],
    Bin = id(g(A)),
    T = id(h(id(Bin))),
    L = id(T(12345)),
    length(L).

g([_H | _T] = L) when is_list(L) ->
    erlang:list_to_binary(id(L)).

h(B) when not is_binary(B) ->
    fun(_X) -> [] end;
h(B) ->
    id(erlang:binary_to_term(id(B))).

id(X) ->
    X.
