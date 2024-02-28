%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-export([
    start/0,
    apply/2, apply/3,
    test_atom_decoding/0,
    get_atom/1,
    get_binary/1,
    test_atom_decoding_checks/0,
    id/1
]).

start() ->
    % Starting from OTP-26, atoms are encoded as UTF-8 by default.
    test_reverse(foo, {<<131, 119, 3, 102, 111, 111>>, <<131, 100, 0, 3, 102, 111, 111>>}),
    test_reverse(bar, {<<131, 119, 3, 98, 97, 114>>, <<131, 100, 0, 3, 98, 97, 114>>}),
    test_reverse(
        '∀x∃y.f(x,y)',
        <<131, 119, 15, 226, 136, 128, 120, 226, 136, 131, 121, 46, 102, 40, 120, 44, 121, 41>>,
        []
    ),
    test_reverse(
        ':アトムＶＭ',
        <<131, 119, 16, 58, 227, 130, 162, 227, 131, 136, 227, 131, 160, 239, 188, 182, 239, 188,
            173>>,
        []
    ),
    test_reverse(128, <<131, 97, 128>>),
    test_reverse(257, <<131, 98, 0, 0, 1, 1>>),
    test_reverse(0, <<131, 97, 0>>),
    test_reverse(-1, <<131, 98, 255, 255, 255, 255>>),
    test_reverse(32768, <<131, 98, 0, 0, 128, 0>>),
    test_reverse(-32768, <<131, 98, 255, 255, 128, 0>>),
    test_reverse(
        {foo, bar}, {
            <<131, 104, 2, 119, 3, 102, 111, 111, 119, 3, 98, 97, 114>>,
            <<131, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114>>
        }
    ),
    test_reverse({foo, 0}, {
        <<131, 104, 2, 119, 3, 102, 111, 111, 97, 0>>,
        <<131, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0>>
    }),
    test_reverse([], <<131, 106>>),
    test_reverse(
        [{foo, 0}, {bar, 1}], {
            <<131, 108, 0, 0, 0, 2, 104, 2, 119, 3, 102, 111, 111, 97, 0, 104, 2, 119, 3, 98, 97,
                114, 97, 1, 106>>,
            <<131, 108, 0, 0, 0, 2, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0, 104, 2, 100, 0, 3, 98,
                97, 114, 97, 1, 106>>
        }
    ),
    test_reverse(
        [improper | list],
        {
            <<131, 108, 0, 0, 0, 1, 119, 8, 105, 109, 112, 114, 111, 112, 101, 114, 119, 4, 108,
                105, 115, 116>>,
            <<131, 108, 0, 0, 0, 1, 100, 0, 8, 105, 109, 112, 114, 111, 112, 101, 114, 100, 0, 4,
                108, 105, 115, 116>>
        }
    ),
    test_reverse({foo, bar}, {
        <<131, 104, 2, 119, 3, 102, 111, 111, 119, 3, 98, 97, 114>>,
        <<131, 104, 2, 100, 0, 3, 102, 111, 111, 100, 0, 3, 98, 97, 114>>
    }),
    test_reverse({foo, 0}, {
        <<131, 104, 2, 119, 3, 102, 111, 111, 97, 0>>,
        <<131, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0>>
    }),
    test_reverse([], <<131, 106>>),
    test_reverse(
        [{foo, 0}, {bar, 1}], {
            <<131, 108, 0, 0, 0, 2, 104, 2, 119, 3, 102, 111, 111, 97, 0, 104, 2, 119, 3, 98, 97,
                114, 97, 1, 106>>,
            <<131, 108, 0, 0, 0, 2, 104, 2, 100, 0, 3, 102, 111, 111, 97, 0, 104, 2, 100, 0, 3, 98,
                97, 114, 97, 1, 106>>
        }
    ),
    test_reverse(
        [improper | list], {
            <<131, 108, 0, 0, 0, 1, 119, 8, 105, 109, 112, 114, 111, 112, 101, 114, 119, 4, 108,
                105, 115, 116>>,
            <<131, 108, 0, 0, 0, 1, 100, 0, 8, 105, 109, 112, 114, 111, 112, 101, 114, 100, 0, 4,
                108, 105, 115, 116>>
        }
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
    ok = test_external_function(),

    {32768, 6} = erlang:binary_to_term(<<131, 98, 0, 0, 128, 0, 127>>, [used]),
    test_catenate_and_split([foo, bar, 128, {foo, bar}, [a, b, c, {d}]]),
    ok = test_invalid_term_encoding(),
    ok = test_mutate_encodings(),
    ok = test_atom_decoding(),
    ok = test_atom_decoding_checks(),
    0.

test_reverse(T, Interop) ->
    test_reverse(T, Interop, []).

test_reverse(T, {Utf8Interop, Latin1Interop}, Options) ->
    case get_otp_version() of
        X when is_integer(X) andalso X >= 26 ->
            test_reverse(T, Utf8Interop, Options);
        atomvm ->
            test_reverse(T, Utf8Interop, Options);
        _ ->
            test_reverse(T, Latin1Interop, Options)
    end;
test_reverse(T, Interop, Options) when is_binary(Interop) andalso is_list(Options) ->
    Bin =
        case Options of
            [] -> erlang:term_to_binary(T);
            _ -> erlang:term_to_binary(T, Options)
        end,
    %% erlang:display(Bin),
    Bin = Interop,
    {X, Used} = erlang:binary_to_term(Bin, [used]),
    Used = erlang:byte_size(Bin),
    X = erlang:binary_to_term(Bin),
    %% erlang:display(X),
    X = T.

test_catenate_and_split(L) ->
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
    [catch (binary_to_term(Mutation)) || Mutation <- Mutations],
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

test_external_function() ->
    T = T = [?MODULE:id(fun ?MODULE:apply/2), ?MODULE:id(fun ?MODULE:apply/3)],
    Bin =
        case get_otp_version() of
            X when is_integer(X) andalso X >= 26 orelse X == atomvm ->
                %% expect SMALL_ATOM_UTF8_EXT encoding
                <<131, 108, 0, 0, 0, 2, 113, 119, 19, 116, 101, 115, 116, 95, 98, 105, 110, 97, 114,
                    121, 95, 116, 111, 95, 116, 101, 114, 109, 119, 5, 97, 112, 112, 108, 121, 97,
                    2, 113, 119, 19, 116, 101, 115, 116, 95, 98, 105, 110, 97, 114, 121, 95, 116,
                    111, 95, 116, 101, 114, 109, 119, 5, 97, 112, 112, 108, 121, 97, 3, 106>>;
            _ ->
                %% expect ATOM_EXT encoding
                <<131, 108, 0, 0, 0, 2, 113, 100, 0, 19, 116, 101, 115, 116, 95, 98, 105, 110, 97,
                    114, 121, 95, 116, 111, 95, 116, 101, 114, 109, 100, 0, 5, 97, 112, 112, 108,
                    121, 97, 2, 113, 100, 0, 19, 116, 101, 115, 116, 95, 98, 105, 110, 97, 114, 121,
                    95, 116, 111, 95, 116, 101, 114, 109, 100, 0, 5, 97, 112, 112, 108, 121, 97, 3,
                    106>>
        end,

    Bin = erlang:term_to_binary(T),

    [Fun2, Fun3] = binary_to_term(Bin),
    true = is_function(Fun2),
    true = is_function(Fun3),
    42 = Fun2(fun() -> 42 end, []),
    42 = Fun3(?MODULE, apply, [fun() -> 42 end, []]),
    42 = Fun3(?MODULE, apply, [Fun2, [fun() -> 42 end, []]]),
    ok.

get_binary(Id) ->
    case Id of
        % 'buondì'
        latin1_1 ->
            <<131, 100, 0, 6, 98, 117, 111, 110, 100, 236>>;
        % 'ðñ÷aþbÿ'
        latin1_2 ->
            <<131, 100, 0, 7, 240, 241, 247, 97, 254, 98, 255>>;
        % 'hello'
        latin1_3 ->
            <<131, 100, 0, 5, 104, 101, 108, 108, 111>>;
        % invalid length
        invalid_latin1_1 ->
            <<131, 100, 1, 6, 98, 117, 111, 110, 100, 236>>;
        % invalid length
        invalid_latin1_2 ->
            <<131, 100, 0, 6, 240, 241, 247, 97, 254, 98, 255>>;
        % invalid length
        invalid_latin1_3 ->
            <<131, 100, 0, 10, 240, 241, 247, 97, 254, 98, 255>>;
        % invalid length
        invalid_latin1_4 ->
            <<131, 100, 0, 6, 104, 101, 108, 108, 111>>;
        % invalid length
        invalid_latin1_5 ->
            <<131, 100, 0, 4, 104, 101, 108, 108, 111>>;
        % invalid length
        invalid_latin1_6 ->
            <<131, 100, 1, 5, 104, 101, 108, 108, 111>>;
        % '漢字'
        jp_1 ->
            <<131, 119, 6, 230, 188, 162, 229, 173, 151>>;
        % 'Unicodeで使える8ビット'
        jp_2 ->
            <<131, 119, 29, 85, 110, 105, 99, 111, 100, 101, 227, 129, 167, 228, 189, 191, 227, 129,
                136, 227, 130, 139, 56, 227, 131, 147, 227, 131, 131, 227, 131, 136>>;
        % 'buondì'
        latin1_as_utf8_1 ->
            <<131, 119, 7, 98, 117, 111, 110, 100, 195, 172>>;
        % 'ðñ÷aþbÿ'
        latin1_as_utf8_2 ->
            <<131, 119, 12, 195, 176, 195, 177, 195, 183, 97, 195, 190, 98, 195, 191>>;
        % 'hello'
        latin1_as_utf8_3 ->
            <<131, 119, 5, 104, 101, 108, 108, 111>>;
        % valid length, but truncated utf8
        invalid_utf8_1 ->
            <<131, 119, 5, 230, 188, 162, 229, 173>>;
        % invalid length and truncated utf8
        invalid_utf8_2 ->
            <<131, 119, 6, 230, 188, 162, 229, 173>>;
        % invalid length
        invalid_utf8_len_1 ->
            <<131, 119, 4, 104, 101, 108, 108, 111>>;
        % invalid length
        invalid_utf8_len_2 ->
            <<131, 119, 6, 104, 101, 108, 108, 111>>;
        % invalid utf8 sequence
        invalid_utf8_seq_1 ->
            <<131, 119, 1, 230>>;
        % invalid utf8 sequence
        invalid_utf8_seq_2 ->
            <<131, 119, 4, 16#f0, 16#90, 16#28, 16#bc>>;
        % invalid utf8 sequence
        invalid_utf8_seq_3 ->
            <<131, 119, 6, 16#fc, 16#a1, 16#a1, 16#a1, 16#a1, 16#a1>>
    end.

get_atom(Id) ->
    case Id of
        latin1_1 -> 'buondì';
        latin1_2 -> 'ðñ÷aþbÿ';
        latin1_3 -> 'hello';
        jp_1 -> '漢字';
        jp_2 -> 'Unicodeで使える8ビット';
        latin1_as_utf8_1 -> 'buondì';
        latin1_as_utf8_2 -> 'ðñ÷aþbÿ';
        latin1_as_utf8_3 -> 'hello'
    end.

test_atom_decoding() ->
    true = compare_pair(latin1_1),
    true = compare_pair(latin1_2),
    true = compare_pair(latin1_3),
    true = compare_pair(jp_1),
    true = compare_pair(jp_2),
    true = compare_pair(latin1_as_utf8_1),
    true = compare_pair(latin1_as_utf8_2),
    true = compare_pair(latin1_as_utf8_3),
    ok.

test_atom_decoding_checks() ->
    % if there are some bytes at the end of the binary no error is raised
    % I was expecting some kind of error, but it is not raised
    % I will keep them anyway here but commented
    ok = expect_badarg(make_binterm_fun(invalid_latin1_1)),
    %ok = expect_badarg(make_binterm_fun(invalid_latin1_2)),
    ok = expect_badarg(make_binterm_fun(invalid_latin1_3)),
    ok = expect_badarg(make_binterm_fun(invalid_latin1_4)),
    %ok = expect_badarg(make_binterm_fun(invalid_latin1_5)),
    ok = expect_badarg(make_binterm_fun(invalid_latin1_6)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_1)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_2)),
    %ok = expect_badarg(make_binterm_fun(invalid_utf8_len_1)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_len_2)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_seq_1)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_seq_2)),
    ok = expect_badarg(make_binterm_fun(invalid_utf8_seq_3)),
    ok.

make_binterm_fun(Id) ->
    fun() ->
        Bin = ?MODULE:get_binary(Id),
        erlang:binary_to_term(Bin)
    end.

compare_pair(Id) ->
    A = ?MODULE:get_atom(Id),
    B = ?MODULE:get_binary(Id),
    A == erlang:binary_to_term(B).

% We don't have access to erlang module in tests.
apply(F, []) ->
    F();
apply(F, [X]) ->
    F(X);
apply(F, [X, Y]) ->
    F(X, Y).

apply(M, F, []) ->
    M:F();
apply(M, F, [X]) ->
    M:F(X);
apply(M, F, [X, Y]) ->
    M:F(X, Y).

expect_badarg(Fun) ->
    try
        erlang:display(Fun()),
        fail
    catch
        _:badarg ->
            ok
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.

id(X) ->
    X.
