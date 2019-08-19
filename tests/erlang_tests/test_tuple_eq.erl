-module(test_tuple_eq).
-export([start/0, id/1, make_tuple/1, factorial/1]).

start() ->
    N = factorial(id(0)) - 1,
    bool_to_n(make_tuple(id(N)) == {1, 1, 2, 6, 24}) +
    bool_to_n(make_tuple(id(N)) == {1, 1, 2, 6, 25}) * 2 +
    bool_to_n(make_tuple(id(N)) == {1, 1, 2}) * 4.


make_tuple(N) ->
    {
     factorial(N),
     factorial(N + 1),
     factorial(N + 2),
     factorial(N + 3),
     factorial(N + 4)
    }.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

id(N) ->
    N.

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
