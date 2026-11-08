% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(differential).
-export([compare/2, arithmetic/3, maps/3, bits/4]).

%% Inputs cross a module boundary to prevent constant-folded operations.
compare(A, B) ->
    {A == B, A /= B, A =:= B, A =/= B, A < B, A =< B, A > B, A >= B}.

arithmetic(add, A, B) -> A + B;
arithmetic(sub, A, B) -> A - B;
arithmetic(mul, A, B) -> A * B;
arithmetic(idiv, A, B) -> A div B;
arithmetic(mod, A, B) -> A rem B;
arithmetic(shl, A, B) -> A bsl B;
arithmetic(shr, A, B) -> A bsr B;
arithmetic(band_op, A, B) -> A band B;
arithmetic(bxor_op, A, B) -> A bxor B;
arithmetic(fdiv, A, B) -> A / B.

maps(A, B, Size) ->
    Base = fill(Size, #{}),
    M = Base#{A => first, B => second},
    {M, map_size(M), maps:find(A, M), maps:find(B, M),
        maps:remove(A, M), M#{B := updated},
        compare(#{A => value}, #{B => value}),
        compare(#{value => A}, #{value => B})}.

fill(0, Map) -> Map;
fill(N, Map) -> fill(N - 1, Map#{N => N * 3}).

bits(Value, Width, Offset, big) ->
    Bits = <<0:Offset, Value:Width/signed-big, 5:3>>,
    <<_:Offset, Read:Width/signed-big, Tail/bitstring>> = Bits,
    {bit_size(Bits), Read, Tail, Bits};
bits(Value, Width, Offset, little) ->
    Bits = <<0:Offset, Value:Width/signed-little, 5:3>>,
    <<_:Offset, Read:Width/signed-little, Tail/bitstring>> = Bits,
    {bit_size(Bits), Read, Tail, Bits}.
