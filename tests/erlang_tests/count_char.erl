-module (count_char).
-export([start/0,count/2]).

start() ->
    count("hello", hd("l")).

count([A | T], C) ->
    one_if(A, C) + count(T, C);
count([], _C) ->
    0.

one_if(A, B) ->
    if
        A == B -> 1;
        true -> 0
    end.
