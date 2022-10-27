%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_binary_to_term).

-export([start/0]).

start() ->
    test_reverse(foo, <<131, 100, 0, 3, 102, 111, 111>>),
    test_reverse(bar, <<131, 100, 0, 3, 98, 97, 114>>),
    test_reverse(128, <<131, 97, 128>>),
    test_reverse(257, <<131, 98, 0, 0, 1, 1>>),
    test_reverse(0, <<131, 97, 0>>),
    test_reverse(-1, <<131, 98, 255, 255, 255, 255>>),
    test_reverse(32768, <<131, 98, 0, 0, 128, 0>>),
    test_reverse(-32768, <<131, 98, 255, 255, 128, 0>>),
    test_reverse({foo, bar}, <<131, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114>>),
    test_reverse({foo, 0}, <<131, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0>>),
    test_reverse([], <<131, 106>>),
    test_reverse(
        [{foo, 0}, {bar, 1}],
        <<131, 108, 0, 0, 0, 2, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0, 104, 2, 100, 0, 3, 98, 97,
            114, 97, 1, 106>>
    ),
    test_reverse(
        [improper | list],
        <<131, 108, 0, 0, 0, 1, 100, 0, 8, 105, 109, 112, 114, 111, 112, 101, 114, 100, 0, 4, 108,
            105, 115, 116>>
    ),
    test_reverse(<<"foobar">>, <<131, 109, 0, 0, 0, 6, 102, 111, 111, 98, 97, 114>>),
    test_reverse(<<":アトムＶＭ">>, <<131, 109, 0, 0, 0, 6, 58, 162, 200, 224, 54, 45>>),
    test_reverse("", <<131, 106>>),
    test_reverse("foobar", <<131, 107, 0, 6, 102, 111, 111, 98, 97, 114>>),
    test_reverse(
        ":アトムＶＭ",
        <<131, 108, 0, 0, 0, 6, 97, 58, 98, 0, 0, 48, 162, 98, 0, 0, 48, 200, 98, 0, 0, 48, 224, 98,
            0, 0, 255, 54, 98, 0, 0, 255, 45, 106>>
    ),
    test_reverse(<<"foo">>, <<131, 109, 0, 0, 0, 3, 102, 111, 111>>),
    test_reverse(
        <<"012345678901234567890123456789012345678901234567890123456789012345">>,
        <<131, 109, 0, 0, 0, 66, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 49, 50, 51, 52, 53, 54,
            55, 56, 57, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 49, 50, 51, 52, 53, 54, 55, 56,
            57, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 48,
            49, 50, 51, 52, 53>>
    ),

    {32768, 6} = erlang:binary_to_term(<<131, 98, 0, 0, 128, 0, 127>>, [used]),
    test_catentate_and_split([foo, bar, 128, {foo, bar}, [a, b, c, {d}]]),
    ok = test_invalid_term_encoding(),
    ok = test_mutate_encodings(),
    0.

test_reverse(T, Interop) ->
    Bin = erlang:term_to_binary(T),
    %% erlang:display(Bin),
    Bin = Interop,
    {X, Used} = erlang:binary_to_term(Bin, [used]),
    Used = erlang:byte_size(Bin),
    X = erlang:binary_to_term(Bin),
    %% erlang:display(X),
    X = T.

test_catentate_and_split(L) ->
    Bins = [erlang:term_to_binary(E) || E <- L],
    Bin = erlang:iolist_to_binary(Bins),
    L2 = split(Bin, []),
    L = L2.

split(<<"">>, Accum) ->
    reverse(Accum, []);
split(Bin, Accum) ->
    {T, Used} = erlang:binary_to_term(Bin, [used]),
    Bin2 = binary:part(Bin, Used, erlang:byte_size(Bin) - Used),
    split(Bin2, [T | Accum]).

reverse([], Accum) -> Accum;
reverse([H | T], Accum) -> reverse(T, [H | Accum]).

test_invalid_term_encoding() ->
    ok = expect_badarg(
        fun() ->
            binary_to_term(<<"garbage">>)
        end
    ),
    ok.

test_mutate_encodings() ->
    Terms = [
        0,
        1234,
        foo,
        {foo, bar},
        [],
        [gnu, gnat],
        [gnu | gnat],
        seq(1, 100),
        "Haddock's Eyes",
        <<"A Singing on a Gate">>,
        #{foo => [{bar, tapas}]}
    ],
    [test_mutate_encoding(Term) || Term <- Terms],
    ok.

test_mutate_encoding(Term) ->
    Bin = term_to_binary(Term),
    Mutations = [mutate_bin(Bin, I) || I <- seq(erlang:byte_size(Bin) - 1)],
    %% verify the VM does not crash, even if the term can't be decoded
    [catch(binary_to_term(Mutation)) || Mutation <- Mutations],
    ok.

seq(N) -> seq(0, N).

seq(N, N) ->
    [N];
seq(I, N) ->
    [I | seq(I + 1, N)].

mutate_bin(Bin, I) ->
    <<Prefix:I/binary, Ith:8/integer-unsigned, Rest/binary>> = Bin,
    I2 = Ith bxor 16#FF,
    <<Prefix/binary, I2:8/integer-unsigned, Rest/binary>>.

expect_badarg(Fun) ->
    try
        Fun(),
        fail
    catch
        _:badarg ->
            ok
    end.
