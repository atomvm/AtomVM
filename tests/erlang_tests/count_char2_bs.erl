-module(count_char2_bs).

-export([start/0, count/3]).

start() ->
    count(<<"oHelloo">>, $l, 0).

count(<<A, Rest/binary>>, A, N) ->
    count(Rest, A, N + 1);
count(<<_Byte, Rest/binary>>, A, N) ->
    case count(Rest, A, N) of
        2 -> count(Rest, $o, 0) * 1000 + N;
        3 -> count(Rest, $z, 0);
        4 -> count(Rest, $o, 100);
        5 -> count(Rest, $q, -100);
        M -> M
    end;
count(<<>>, _A, N) ->
    N.
