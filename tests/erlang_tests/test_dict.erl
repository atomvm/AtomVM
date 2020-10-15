-module(test_dict).

-export([start/0, put_int/1, stringize/1, factorial/1, id/1, the_get/1, the_erase/1]).

start() ->
    put_int(0),
    X = put_int(1),
    put_int(2),
    put(6, X),
    put_int(3),
    put_int(4),
    put_int(5),
    Y = stringize(factorial(8)),
    Z = stringize(factorial(9)),
    [H | _T] = stringize(erlang:list_to_integer(Z) - erlang:list_to_integer(Y)),
    _ = stringize(factorial(6)),
    _ = stringize(factorial(7)),
    _ = stringize(123456),
    _ = stringize(876543),
    W = erlang:list_to_integer(the_get(stringize((H - $0) - 2)) ++ the_erase(id(6))),
    _ = stringize(111222),
    _ = stringize(333222),
    "undefined" = erlang:atom_to_list(the_erase(6)),
    W.

put_int(N) ->
    put(stringize(id(factorial(id(N)))), stringize(id(factorial(id(N))) * 2)).

stringize(I) when is_integer(I) ->
    erlang:integer_to_list(I) ++ "0";
stringize(_I) ->
    "".

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

id(X) when is_integer(X) ->
    X;
id(_X) ->
    0.

the_get(L) when is_list(L) ->
    get(L);
the_get(_X) ->
    "-1".

the_erase(N) when is_integer(N) ->
    erase(N);
the_erase(_X) ->
    "0".
