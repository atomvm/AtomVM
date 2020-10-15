-module(count_pairs).

-export([start/0, count/3]).

start() ->
    count(<<"Hello">>, <<"ll">>, 0) + count(<<"zll">>, <<"ll">>, 0) * 2 +
        count(<<"Hello">>, <<"zz">>, 0).

count(<<A:2/binary, Rest/binary>>, Bin, N) when A == Bin ->
    count(Rest, Bin, N + 1);
count(<<_Byte, Rest/binary>>, Bin, N) ->
    count(Rest, Bin, N);
count(<<>>, _Bin, N) ->
    N.
