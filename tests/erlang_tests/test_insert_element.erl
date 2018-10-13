-module(test_insert_element).
-export([start/0, f/1, t/3]).

start() ->
    f(t({world}, 1, hello)) + f(t({hello, world}, 2, test)) * 20 + f(t({hello, test}, 3, world)) * 40.

t(T, I, V) ->
    erlang:insert_element(I, T, V).

f({hello, world}) ->
    1;

f({hello, test, world}) ->
    2;

f({test, world}) ->
    10;

f({hello, test}) ->
    100;

f(T) when is_tuple(T) ->
    3;

f(_T) ->
    4.
