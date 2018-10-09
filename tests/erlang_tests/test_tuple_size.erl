-module(test_tuple_size).
-export([start/0]).

start() ->
    tuple_size(test_t(0, a)) + tuple_size(test_t(1, b)) + tuple_size(test_t(2, c)) + tuple_size(test_t(3, d)).

test_t(0, _V) ->
    {};

test_t(1, V) ->
    {V};

test_t(2, V) ->
    {V, V};

test_t(3, V) ->
    {V, V, V}.
