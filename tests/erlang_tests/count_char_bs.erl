-module(count_char_bs).

-export([start/0, count/3]).

start() ->
    count(<<"Hello">>, $l, 0).

count(<<A, Rest/binary>>, A, N) ->
    count(Rest, A, N + 1);
count(<<_Byte, Rest/binary>>, A, N) ->
    count(Rest, A, N);
count(<<>>, _A, N) ->
    N.
