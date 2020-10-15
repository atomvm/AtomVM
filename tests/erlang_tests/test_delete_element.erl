-module(test_delete_element).

-export([start/0, f/1, t/2]).

start() ->
    f(t({hello, test, world}, 2)) + f(t({hello, test, world}, 1)) * 2 +
        f(t({hello, test, world}, 3)) * 4.

t(T, I) ->
    erlang:delete_element(I, T).

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
