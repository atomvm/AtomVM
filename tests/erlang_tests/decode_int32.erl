-module(decode_int32).

-export([start/0, decode32/1, bin/3, g/1]).

start() ->
    ((decode32(bin(b, u, [16#CA, 16#FE, 16#BA, 16#BE])) * 11) bxor
     (decode32(bin(b, s, [16#CA, 16#FE, 16#BA, 16#BE])) * -3) bxor
    (decode32(bin(l, u, [16#CA, 16#FE, 16#BA, 16#BE])) * 5) bxor
    (decode32(bin(l, s, [16#CA, 16#FE, 16#BA, 16#BE])) * -7)) rem 876113.

decode32(<<0, U32B:32/integer-unsigned-big, 0>>) ->
    U32B;
decode32(<<1, S32B:32/integer-signed-big, 0>>) ->
    S32B;
decode32(<<0, U32L:32/integer-unsigned-little, 1>>) ->
    U32L;
decode32(<<1, S32L:32/integer-signed-little, 1>>) ->
    S32L;
decode32(B) ->
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
