-module (len_test).
-export([start/0,len/2]).

start() ->
    len("hello", 0).

len(A, N) ->
    length(A) + N.
