-module(decode_int24).

-export([start/0, decode24/1, bin/3, g/1]).

start() ->
    ((decode24(bin(b, u, [16#CA, 16#FE, 16#BA])) * 11) bxor
        (decode24(bin(b, s, [16#CA, 16#FE, 16#BA])) * -3) bxor
        (decode24(bin(l, u, [16#CA, 16#FE, 16#BA])) * 5) bxor
        (decode24(bin(l, s, [16#CA, 16#FE, 16#BA])) * -7)) rem 876113.

decode24(<<0, U24B:24/integer-unsigned-big, 0>>) ->
    U24B;
decode24(<<1, S24B:24/integer-signed-big, 0>>) ->
    S24B;
decode24(<<0, U24L:24/integer-unsigned-little, 1>>) ->
    U24L;
decode24(<<1, S24L:24/integer-signed-little, 1>>) ->
    S24L;
decode24(B) ->
    erlang:binary_to_integer(B).

bin(b, u, L) ->
    g([0 | L ++ [0]]);
bin(b, s, L) ->
    g([1 | L ++ [0]]);
bin(l, u, L) ->
    g([0 | L ++ [1]]);
bin(l, s, L) ->
    g([1 | L ++ [1]]).

g(X) ->
    erlang:list_to_binary(X).
