-module(count_char3_bs).

-export([start/0, count/3, id/1, is_o/1]).

start() ->
    is_o(id(count(id(<<"oHello.o">>), $l, 0))).

count(<<A, Rest/binary>>, A, N) ->
    count(Rest, A, N + 1);
count(<<$., Rest/binary>>, _A, _N) ->
    Rest;
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

id(X) ->
    X.

is_o(X) ->
    [H] = erlang:binary_to_list(X),
    case H == $o of
        true -> 1;
        false -> 0
    end.
