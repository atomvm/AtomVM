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
    test_encode_pid/0,
    test_encode_port/0,
    id/1
]).

start() ->
    test_reverse(foo, <<131, 119, 3, 102, 111, 111>>),
    test_reverse(bar, <<131, 119, 3, 98, 97, 114>>),
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
        {foo, bar}, <<131, 104, 2, 119, 3, 102, 111, 111, 119, 3, 98, 97, 114>>
    ),
    test_reverse({foo, 0}, <<131, 104, 2, 119, 3, 102, 111, 111, 97, 0>>),
    test_reverse([], <<131, 106>>),
    test_reverse(
        [{foo, 0}, {bar, 1}],
        <<131, 108, 0, 0, 0, 2, 104, 2, 119, 3, 102, 111, 111, 97, 0, 104, 2, 119, 3, 98, 97, 114,
            97, 1, 106>>
    ),
    test_reverse(
        [improper | list],
        <<131, 108, 0, 0, 0, 1, 119, 8, 105, 109, 112, 114, 111, 112, 101, 114, 119, 4, 108, 105,
            115, 116>>
    ),
    test_reverse({foo, bar}, <<131, 104, 2, 119, 3, 102, 111, 111, 119, 3, 98, 97, 114>>),
    test_reverse({foo, 0}, <<131, 104, 2, 119, 3, 102, 111, 111, 97, 0>>),
    test_reverse([], <<131, 106>>),
    test_reverse(
        [{foo, 0}, {bar, 1}],
        <<131, 108, 0, 0, 0, 2, 104, 2, 119, 3, 102, 111, 111, 97, 0, 104, 2, 119, 3, 98, 97, 114,
            97, 1, 106>>
    ),
    test_reverse(
        [improper | list],
        <<131, 108, 0, 0, 0, 1, 119, 8, 105, 109, 112, 114, 111, 112, 101, 114, 119, 4, 108, 105,
            115, 116>>
    ),
    test_reverse(<<"foobar">>, <<131, 109, 0, 0, 0, 6, 102, 111, 111, 98, 97, 114>>),
    test_reverse(<<1:1>>, <<131, 77, 0, 0, 0, 1, 1, 128>>),
    test_reverse(<<5:31>>, <<131, 77, 0, 0, 0, 4, 7, 0, 0, 0, 10>>),
    test_reverse(<<255, 5:7>>, <<131, 77, 0, 0, 0, 2, 7, 255, 10>>),
    % BIT_BINARY_EXT: insignificant padding bits in the last byte are accepted
    % on decode (as OTP does) and canonicalized (zeroed) on re-encode.
    DirtyBitstring = erlang:binary_to_term(id(<<131, 77, 0, 0, 0, 1, 1, 255>>)),
    true = DirtyBitstring =:= <<1:1>>,
    <<131, 77, 0, 0, 0, 1, 1, 128>> = erlang:term_to_binary(DirtyBitstring),
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
    test_reverse(
        {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
            26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
            48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
            70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
            92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110,
            111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127,
            128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144,
            145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161,
            162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178,
            179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195,
            196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212,
            213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229,
            230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246,
            247, 248, 249, 250, 251, 252, 253, 254, 255, 256},
        <<131, 105, 0, 0, 1, 0, 97, 1, 97, 2, 97, 3, 97, 4, 97, 5, 97, 6, 97, 7, 97, 8, 97, 9, 97,
            10, 97, 11, 97, 12, 97, 13, 97, 14, 97, 15, 97, 16, 97, 17, 97, 18, 97, 19, 97, 20, 97,
            21, 97, 22, 97, 23, 97, 24, 97, 25, 97, 26, 97, 27, 97, 28, 97, 29, 97, 30, 97, 31, 97,
            32, 97, 33, 97, 34, 97, 35, 97, 36, 97, 37, 97, 38, 97, 39, 97, 40, 97, 41, 97, 42, 97,
            43, 97, 44, 97, 45, 97, 46, 97, 47, 97, 48, 97, 49, 97, 50, 97, 51, 97, 52, 97, 53, 97,
            54, 97, 55, 97, 56, 97, 57, 97, 58, 97, 59, 97, 60, 97, 61, 97, 62, 97, 63, 97, 64, 97,
            65, 97, 66, 97, 67, 97, 68, 97, 69, 97, 70, 97, 71, 97, 72, 97, 73, 97, 74, 97, 75, 97,
            76, 97, 77, 97, 78, 97, 79, 97, 80, 97, 81, 97, 82, 97, 83, 97, 84, 97, 85, 97, 86, 97,
            87, 97, 88, 97, 89, 97, 90, 97, 91, 97, 92, 97, 93, 97, 94, 97, 95, 97, 96, 97, 97, 97,
            98, 97, 99, 97, 100, 97, 101, 97, 102, 97, 103, 97, 104, 97, 105, 97, 106, 97, 107, 97,
            108, 97, 109, 97, 110, 97, 111, 97, 112, 97, 113, 97, 114, 97, 115, 97, 116, 97, 117,
            97, 118, 97, 119, 97, 120, 97, 121, 97, 122, 97, 123, 97, 124, 97, 125, 97, 126, 97,
            127, 97, 128, 97, 129, 97, 130, 97, 131, 97, 132, 97, 133, 97, 134, 97, 135, 97, 136,
            97, 137, 97, 138, 97, 139, 97, 140, 97, 141, 97, 142, 97, 143, 97, 144, 97, 145, 97,
            146, 97, 147, 97, 148, 97, 149, 97, 150, 97, 151, 97, 152, 97, 153, 97, 154, 97, 155,
            97, 156, 97, 157, 97, 158, 97, 159, 97, 160, 97, 161, 97, 162, 97, 163, 97, 164, 97,
            165, 97, 166, 97, 167, 97, 168, 97, 169, 97, 170, 97, 171, 97, 172, 97, 173, 97, 174,
            97, 175, 97, 176, 97, 177, 97, 178, 97, 179, 97, 180, 97, 181, 97, 182, 97, 183, 97,
            184, 97, 185, 97, 186, 97, 187, 97, 188, 97, 189, 97, 190, 97, 191, 97, 192, 97, 193,
            97, 194, 97, 195, 97, 196, 97, 197, 97, 198, 97, 199, 97, 200, 97, 201, 97, 202, 97,
            203, 97, 204, 97, 205, 97, 206, 97, 207, 97, 208, 97, 209, 97, 210, 97, 211, 97, 212,
            97, 213, 97, 214, 97, 215, 97, 216, 97, 217, 97, 218, 97, 219, 97, 220, 97, 221, 97,
            222, 97, 223, 97, 224, 97, 225, 97, 226, 97, 227, 97, 228, 97, 229, 97, 230, 97, 231,
            97, 232, 97, 233, 97, 234, 97, 235, 97, 236, 97, 237, 97, 238, 97, 239, 97, 240, 97,
            241, 97, 242, 97, 243, 97, 244, 97, 245, 97, 246, 97, 247, 97, 248, 97, 249, 97, 250,
            97, 251, 97, 252, 97, 253, 97, 254, 97, 255, 98, 0, 0, 1, 0>>
    ),
    ok = test_external_function(),
    ok = test_function(),

    {32768, 6} = erlang:binary_to_term(<<131, 98, 0, 0, 128, 0, 127>>, [used]),
    test_catenate_and_split([foo, bar, 128, {foo, bar}, [a, b, c, {d}]]),
    ok = test_invalid_term_encoding(),
    ok = test_mutate_encodings(),
    ok = test_atom_decoding(),
    ok = test_atom_decoding_checks(),
    ok = test_encode_pid(),
    ok = test_encode_reference(),
    ok = test_encode_port(),
    ok = test_atom_encoding(),
    ok = test_encode_resource(),
    ok = test_safe_option(),
    ok = test_invalid_export_fun_encoding(),
    ok = test_atom_utf8_ext_node(),
    ok = test_encode_process_ref(),
    ok = test_term_to_binary_options(),
    ok = test_invalid_bit_binary(),
    ok = test_bitstring_used_offset(),
    0.

% A bitstring embedded in a compound term: the decoder must advance past the
% BIT_BINARY_EXT by exactly base + nbytes so the following elements decode at
% the right offset (regression for a wrap in the reported consumed size).
test_bitstring_used_offset() ->
    T = ?MODULE:id({<<5:3>>, <<255, 3:2>>, [<<1:1>>, done], <<1, 2, 3>>}),
    Bin = erlang:term_to_binary(T),
    T = erlang:binary_to_term(Bin),
    {T, Used} = erlang:binary_to_term(Bin, [used]),
    Used = byte_size(Bin),
    ok.

test_term_to_binary_options() ->
    T = ?MODULE:id({foo, [1, 2, 3], #{a => <<"b">>}, 3.14}),
    Plain = erlang:term_to_binary(T),
    Plain = erlang:term_to_binary(T, []),
    Plain = erlang:term_to_binary(T, [deterministic]),
    Plain = erlang:term_to_binary(T, [{minor_version, 2}, deterministic]),
    T = erlang:binary_to_term(erlang:term_to_binary(T, [{minor_version, 1}])),
    % compressed output may use a different encoding but must round-trip
    T = erlang:binary_to_term(erlang:term_to_binary(T, [compressed])),
    Big = duplicate(200, T, []),
    Big = erlang:binary_to_term(erlang:term_to_binary(Big, [compressed, deterministic])),
    Big = erlang:binary_to_term(erlang:term_to_binary(Big, [{compressed, 6}])),
    ok = expect_badarg(fun() -> erlang:term_to_binary(T, [bad_option]) end),
    ok = expect_badarg(fun() -> erlang:term_to_binary(T, not_a_list) end),
    ok = expect_badarg(fun() -> erlang:term_to_binary(T, [{minor_version, 17}]) end),
    ok = expect_badarg(fun() -> erlang:term_to_binary(T, [{compressed, 42}]) end),
    ok.

duplicate(0, _T, Acc) -> Acc;
duplicate(N, T, Acc) -> duplicate(N - 1, T, [T | Acc]).

test_reverse(T, Interop) ->
    test_reverse(T, Interop, []).

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
        <<131, 108, 0, 0, 0, 2, 113, 119, 19, 116, 101, 115, 116, 95, 98, 105, 110, 97, 114, 121,
            95, 116, 111, 95, 116, 101, 114, 109, 119, 5, 97, 112, 112, 108, 121, 97, 2, 113, 119,
            19, 116, 101, 115, 116, 95, 98, 105, 110, 97, 114, 121, 95, 116, 111, 95, 116, 101, 114,
            109, 119, 5, 97, 112, 112, 108, 121, 97, 3, 106>>,

    Bin = erlang:term_to_binary(T),

    [Fun2, Fun3] = binary_to_term(Bin),
    true = is_function(Fun2),
    true = is_function(Fun3),
    42 = Fun2(fun() -> 42 end, []),
    42 = Fun3(?MODULE, apply, [fun() -> 42 end, []]),
    42 = Fun3(?MODULE, apply, [Fun2, [fun() -> 42 end, []]]),
    ok.

test_function() ->
    X = id(2),
    T = [fun(A) -> A * 2 end, fun(A) -> A * X end],

    Bin = erlang:term_to_binary(T),

    <<131, ModuleAtom/binary>> = term_to_binary(?MODULE:id(?MODULE)),
    ModuleAtomSize = byte_size(ModuleAtom),

    <<131, 108, 2:32, Funs/binary>> = Bin,
    <<112, Size2:32, Rest1/binary>> = Funs,
    % need to use split_binary for OTP 21/22
    {Fun2Bin, <<112, Size3:32, Rest2/binary>>} = split_binary(Rest1, Size2 - 4),
    {Fun3Bin, <<106>>} = split_binary(Rest2, Size3 - 4),
    <<1, MD5:16/binary, Index2:32, 0:32, ModuleAtom:ModuleAtomSize/binary, 97, Index2, 98,
        OldUniq:32, Rest3/binary>> = Fun2Bin,
    <<1, MD5:16/binary, Index3:32, 1:32, ModuleAtom:ModuleAtomSize/binary, 97, Index3, 98,
        OldUniq:32, _Rest4/binary>> = Fun3Bin,

    [Fun2, Fun3] = binary_to_term(Bin),
    true = is_function(Fun2),
    true = is_function(Fun3),
    42 = Fun2(21),
    42 = Fun3(21),

    B1 =
        <<131, 112, Size2:32, 1, 0:(16 * 8), Index2:32, 0:32, ModuleAtom:ModuleAtomSize/binary, 97,
            (Index2 bxor 42), 98, (OldUniq bxor 42):32, Rest3/binary>>,
    Fun4 = binary_to_term(B1),
    ok =
        try
            42 = Fun4(21),
            unexpected
        catch
            error:{badfun, Fun4} ->
                % BEAM
                ok;
            error:undef ->
                % AtomVM
                ok
        end,
    B1 = term_to_binary(Fun4),
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
        % 'hello'
        short_with_two_bytes_length ->
            <<131, 118, 5:16, "hello">>;
        % 'hello'
        long_with_two_bytes_length ->
            <<131, 118, 256:16,
                "🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘"/utf8>>;
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
        latin1_1 ->
            'buondì';
        latin1_2 ->
            'ðñ÷aþbÿ';
        latin1_3 ->
            'hello';
        jp_1 ->
            '漢字';
        jp_2 ->
            'Unicodeで使える8ビット';
        latin1_as_utf8_1 ->
            'buondì';
        latin1_as_utf8_2 ->
            'ðñ÷aþbÿ';
        latin1_as_utf8_3 ->
            'hello';
        short_with_two_bytes_length ->
            'hello';
        % literal only works with OTP >= 28
        long_with_two_bytes_length ->
            list_to_atom(
                ?MODULE:id("🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘🌑🌒🌓🌔🌕🌖🌗🌘")
            )
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
    true = compare_pair(short_with_two_bytes_length),
    true = compare_pair(long_with_two_bytes_length),
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

test_encode_pid() ->
    Bin = term_to_binary(self()),
    Pid = binary_to_term(Bin),
    Pid ! hello,
    Pid1 = binary_to_term(
        <<131, 88, 119, 13, "nonode@nohost", 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0>>
    ),
    true = is_pid(Pid1),
    true = is_process_alive(Pid1),
    ok =
        receive
            hello -> ok
        after 500 -> error
        end,
    29 = byte_size(Bin),

    % Creation is not displayed in list representation
    FalsePid1 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 1:32, 0:32, 0:32>>, "26"
    ),
    true = is_pid(FalsePid1),
    "<0.1.0>" = pid_to_list(FalsePid1),
    FalsePid1Cr = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 1:32, 0:32, 1:32>>, "26"
    ),
    "<0.1.0>" = pid_to_list(FalsePid1Cr),
    false = FalsePid1 =:= FalsePid1Cr,
    true = FalsePid1 < FalsePid1Cr,

    % Order is done by pid on a given node
    FalsePid2 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 2:32, 0:32, 0:32>>, "26"
    ),
    "<0.2.0>" = pid_to_list(FalsePid2),
    false = FalsePid1 =:= FalsePid2,
    true = FalsePid1 < FalsePid2,
    true = FalsePid1Cr < FalsePid2,

    TruePid1 = binary_to_term_idempotent(
        <<131, 88, 119, 4, "true", 1:32, 0:32, 0:32>>, "26"
    ),
    true = is_pid(TruePid1),
    "<1.1.0>" = pid_to_list(TruePid1),
    TruePid1Again = binary_to_term(term_to_binary(TruePid1)),
    true = TruePid1Again =:= TruePid1,

    % Order is done by node atom, with local pid as nonode@nohost
    true = Pid1 > binary_to_term_idempotent(<<131, 88, 119, 5, "false", 1:32, 0:32, 0:32>>, "26"),
    true = Pid1 > binary_to_term_idempotent(<<131, 88, 119, 6, "nonode", 1:32, 0:32, 0:32>>, "26"),
    true = Pid1 < binary_to_term_idempotent(<<131, 88, 119, 6, "nonodf", 1:32, 0:32, 0:32>>, "26"),

    % Order
    FalsePid_42_43_44 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 42:32, 43:32, 44:32>>, "26"
    ),
    FalsePid_42_44_43 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 42:32, 44:32, 43:32>>, "26"
    ),
    TruePid_42_43_44 = binary_to_term_idempotent(
        <<131, 88, 119, 4, "true", 42:32, 43:32, 44:32>>, "26"
    ),
    FalsePid_44_43_42 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 44:32, 43:32, 42:32>>, "26"
    ),
    TruePid_44_43_42 = binary_to_term_idempotent(
        <<131, 88, 119, 4, "true", 44:32, 43:32, 42:32>>, "26"
    ),
    FalsePid_43_43_43 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 43:32, 43:32, 43:32>>, "26"
    ),
    FalsePid_42_43_45 = binary_to_term_idempotent(
        <<131, 88, 119, 5, "false", 42:32, 43:32, 45:32>>, "26"
    ),

    % Pid first, serial second, atom third and creation fourth
    true = FalsePid_42_43_44 < FalsePid_43_43_43,
    true = FalsePid_42_43_44 < FalsePid_42_44_43,
    true = FalsePid_43_43_43 < FalsePid_44_43_42,
    true = FalsePid_42_43_44 < TruePid_42_43_44,
    true = FalsePid_42_43_44 < TruePid_44_43_42,
    true = FalsePid_44_43_42 > TruePid_42_43_44,
    true = FalsePid_42_43_45 < TruePid_42_43_44,
    true = FalsePid_42_44_43 > TruePid_42_43_44,

    % Not enough data
    ok =
        try
            binary_to_term(<<131, 88, 119, 4, "true", 1:32, 2:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 88, 119, 4, "true", 1:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 88, 119, 4, "true">>)
        catch
            error:badarg -> ok
        end,

    % Node must be an atom
    ok =
        try
            binary_to_term(<<131, 88, 106, 1:32, 2:32, 3:32>>)
        catch
            error:badarg -> ok
        end,

    % Test distributed pid
    Ref42 = do_setnode(test@test_node, 42),
    DistributedPid1 = binary_to_term(
        <<131, 88, 119, 14, "test@test_node", 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 42>>
    ),
    true = is_pid(DistributedPid1),
    true = is_process_alive(DistributedPid1),

    DistributedBin42 = term_to_binary(self()),
    true = DistributedBin42 =/= Bin,
    DistributedPid42 = binary_to_term(DistributedBin42),
    true = DistributedPid42 =:= Pid,
    30 = byte_size(DistributedBin42),

    ok = do_unsetnode(Ref42),
    Bin = term_to_binary(self()),

    Ref43 = do_setnode(test@test_node, 43),
    DistributedBin43 = term_to_binary(self()),
    true = DistributedBin43 =/= DistributedBin42,
    DistributedPid43 = binary_to_term(DistributedBin43),
    true = DistributedPid43 =:= Pid,

    ok = do_unsetnode(Ref43),
    Bin = term_to_binary(self()),
    ok.

do_setnode(Node, Creation) ->
    {NetKernelPid, MonitorRef} = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        [monitor]
    ),
    register(net_kernel, NetKernelPid),
    true = erlang:setnode(Node, Creation),
    Node = node(),
    {NetKernelPid, MonitorRef}.

do_unsetnode({NetKernelPid, MonitorRef}) ->
    NetKernelPid ! quit,
    ok =
        receive
            {'DOWN', MonitorRef, process, NetKernelPid, normal} -> ok
        after 1000 -> timeout
        end,
    case node() of
        nonode@nohost ->
            ok;
        _Other ->
            % On BEAM, node may not be reset immediately
            "BEAM" = erlang:system_info(machine),
            sleep(100),
            nonode@nohost = node()
    end,
    ok.

test_encode_port() ->
    TestPort = open_port({spawn, "echo"}, []),
    Bin = term_to_binary(TestPort),
    TestPort = binary_to_term(Bin),
    true = is_port(TestPort),
    29 = byte_size(Bin),
    LocalPort1 = binary_to_term(
        <<131, 120, 119, 13, "nonode@nohost", 1:64, 0:32>>
    ),
    true = is_port(LocalPort1),
    "#Port<0.1>" = port_to_list(LocalPort1),
    Port1 = binary_to_term(<<131, 120, 119, 4, "true", 43:64, 0:32>>),
    Port2 = binary_to_term(<<131, 120, 119, 4, "true", 43:64, 1:32>>),
    false = Port1 =:= Port2,
    true = Port1 < Port2,
    "#Port<1.43>" = port_to_list(Port1),
    "#Port<1.43>" = port_to_list(Port2),

    % Order
    FalsePort_42_43 = binary_to_term_idempotent(
        <<131, 120, 119, 5, "false", 42:64, 43:32>>, "26"
    ),
    FalsePort_43_42 = binary_to_term_idempotent(
        <<131, 120, 119, 5, "false", 43:64, 42:32>>, "26"
    ),
    FalsePort_43_43 = binary_to_term_idempotent(
        <<131, 120, 119, 5, "false", 43:64, 43:32>>, "26"
    ),
    FalsfPort_42_41 = binary_to_term_idempotent(
        <<131, 120, 119, 5, "falsf", 42:64, 41:32>>, "26"
    ),

    % Node first, creation second, number third
    true = FalsePort_42_43 > FalsePort_43_42,
    true = FalsfPort_42_41 > FalsePort_42_43,
    true = FalsfPort_42_41 > FalsePort_43_42,
    true = FalsePort_43_42 < FalsePort_43_43,

    % Order is done by node atom, with local pid as nonode@nohost
    true =
        TestPort >
            binary_to_term_idempotent(<<131, 120, 119, 5, "false", 1:64, 0:32>>, "26"),
    true =
        TestPort >
            binary_to_term_idempotent(<<131, 120, 119, 6, "nonode", 1:64, 0:32>>, "26"),
    true =
        TestPort <
            binary_to_term_idempotent(<<131, 120, 119, 6, "nonodf", 1:64, 0:32>>, "26"),
    % Test distributed ports
    % Test doesn't pass on BEAM if we use 42 and 43 like for refs,
    % as there probably is a side-effect we don't have
    Ref42 = do_setnode(test@test_node, 1042),
    DistributedBin42 = term_to_binary(TestPort),
    true = DistributedBin42 =/= Bin,
    TestRef42 = binary_to_term(DistributedBin42),
    true = TestRef42 =:= TestPort,
    ExpectedSize = byte_size(DistributedBin42) - 1,

    ok = do_unsetnode(Ref42),

    Ref43 = do_setnode(test@test_node, 1043),
    DistributedBin43 = term_to_binary(TestPort),
    true = DistributedBin43 =/= DistributedBin42,
    TestRef43 = binary_to_term(DistributedBin43),
    true = TestRef43 =:= TestPort,
    ExpectedSize = byte_size(DistributedBin43) - 1,

    % If our creation is 1043, encoded binary with creation 1042 is a different port
    TestRef42_43 = binary_to_term(DistributedBin42),
    false = TestRef42_43 =:= TestPort,

    ok = do_unsetnode(Ref43),
    ok.

test_encode_reference() ->
    TestRef = make_ref(),
    Bin = term_to_binary(TestRef),
    Ref = binary_to_term(Bin),
    true = is_reference(Ref),
    Ref123 = binary_to_term(
        <<131, 90, 0, 2, 119, 13, "nonode@nohost", 0:32, 1:32, 2:32>>
    ),
    true = is_reference(Ref123),
    ExpectedSize =
        case erlang:system_info(machine) of
            "ATOM" ->
                % small utf8 atom & reference with 2 words
                31;
            "BEAM" ->
                % small utf8 atom
                35
        end,
    ExpectedSize = byte_size(Bin),

    % Creation is not displayed in list representation
    Ref1 = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 1:32, 43:32>>, "26"),
    Ref2 = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 2:32, 43:32>>, "26"),
    false = Ref1 =:= Ref2,
    true = Ref1 < Ref2,
    "#Ref<1.43>" = ref_to_list(Ref1),
    "#Ref<1.43>" = ref_to_list(Ref2),

    % Test order
    Ref3 = binary_to_term_idempotent(<<131, 90, 0, 2, 119, 4, "true", 1:32, 43:32, 44:32>>, "26"),
    Ref4 = binary_to_term_idempotent(<<131, 90, 0, 2, 119, 4, "true", 1:32, 44:32, 43:32>>, "26"),
    "#Ref<1.44.43>" = ref_to_list(Ref3),
    "#Ref<1.43.44>" = ref_to_list(Ref4),
    true = Ref3 > Ref4,
    true = Ref4 > Ref1,

    Ref5 = binary_to_term_idempotent(
        <<131, 90, 0, 3, 119, 4, "true", 1:32, 42:32, 43:32, 44:32>>, "26"
    ),
    Ref6 = binary_to_term_idempotent(
        <<131, 90, 0, 3, 119, 4, "true", 1:32, 44:32, 43:32, 42:32>>, "26"
    ),
    "#Ref<1.44.43.42>" = ref_to_list(Ref5),
    "#Ref<1.42.43.44>" = ref_to_list(Ref6),
    true = Ref5 > Ref6,
    true = Ref6 > Ref3,

    % Starting from OTP-24 that introduced DFLAG_V4_NC, references can have 4 or 5 words
    Ref7 = binary_to_term_idempotent(
        <<131, 90, 0, 4, 119, 4, "true", 1:32, 41:32, 42:32, 43:32, 44:32>>, "26"
    ),
    Ref8 = binary_to_term_idempotent(
        <<131, 90, 0, 4, 119, 4, "true", 1:32, 44:32, 43:32, 42:32, 41:32>>, "26"
    ),
    "#Ref<1.44.43.42.41>" = ref_to_list(Ref7),
    "#Ref<1.41.42.43.44>" = ref_to_list(Ref8),
    true = Ref7 > Ref8,
    true = Ref8 > Ref5,

    Ref9 = binary_to_term_idempotent(
        <<131, 90, 0, 5, 119, 4, "true", 1:32, 40:32, 41:32, 42:32, 43:32, 44:32>>, "26"
    ),
    RefA = binary_to_term_idempotent(
        <<131, 90, 0, 5, 119, 4, "true", 1:32, 44:32, 43:32, 42:32, 41:32, 40:32>>, "26"
    ),
    "#Ref<1.44.43.42.41.40>" = ref_to_list(Ref9),
    "#Ref<1.40.41.42.43.44>" = ref_to_list(RefA),
    true = Ref9 > RefA,
    true = RefA > Ref7,

    % Zero-length is tolerated
    RefB = binary_to_term_idempotent(<<131, 90, 0, 0, 119, 4, "true", 1:32>>, "26"),
    "#Ref<1>" = ref_to_list(RefB),
    true = RefB < Ref1,

    % Creation comes before first word in comparison
    % Ref1 = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 1:32, 43:32>>, "26"),
    % Ref2 = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 2:32, 43:32>>, "26"),
    RefC = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 2:32, 42:32>>, "26"),
    true = RefC < Ref2,
    true = RefC > Ref1,

    % Node atom comes first
    RefD = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 5, "false", 1:32, 44:32>>, "26"),
    "#Ref<0.44>" = ref_to_list(RefD),
    true = RefD < Ref1,

    % Local references count like references from node "false" (atom index 0)
    LocalRef = make_ref(),
    RefE = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 5, "false", 1:32, 0:32>>, "26"),
    RefF = binary_to_term_idempotent(<<131, 90, 0, 1, 119, 4, "true", 1:32, 0:32>>, "26"),
    true = RefE < LocalRef,
    true = RefF > LocalRef,

    % Local ref is sorted at nonode@nohost
    case term_to_binary(LocalRef) of
        <<131, 90, 0, Count, 119, 13, "nonode@nohost", Rest/binary>> ->
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 5, "false", Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 4, "true", Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "foobar", Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonode", Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonodf", Rest/binary>>, "26"
                    );
        <<131, 90, 0, Count, 100, 0, 13, "nonode@nohost", Rest/binary>> ->
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 5, "false", Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 4, "true", Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "foobar", Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonode", Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonodf", Rest/binary>>, "26"
                    );
        <<131, 114, 0, Count, 100, 0, 13, "nonode@nohost", Creation, Rest/binary>> ->
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 5, "false", Creation:32, Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 4, "true", Creation:32, Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "foobar", Creation:32, Rest/binary>>, "26"
                    ),
            true =
                LocalRef >
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonode", Creation:32, Rest/binary>>, "26"
                    ),
            true =
                LocalRef <
                    binary_to_term_idempotent(
                        <<131, 90, 0, Count, 119, 6, "nonodf", Creation:32, Rest/binary>>, "26"
                    )
    end,

    % More than 5 words is not tolerated
    ok =
        try
            binary_to_term(
                <<131, 90, 0, 6, 119, 4, "true", 1:32, 2:32, 3:32, 4:32, 5:32, 6:32, 7:32>>
            )
        catch
            error:badarg -> ok
        end,
    % Not enough data
    ok =
        try
            binary_to_term(<<131, 90, 0, 5, 119, 4, "true", 1:32, 2:32, 3:32, 4:32, 5:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 90, 0, 4, 119, 4, "true", 1:32, 2:32, 3:32, 4:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 90, 0, 3, 119, 4, "true", 1:32, 2:32, 3:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 90, 0, 2, 119, 4, "true", 1:32, 2:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 90, 0, 1, 119, 4, "true", 1:32>>)
        catch
            error:badarg -> ok
        end,
    ok =
        try
            binary_to_term(<<131, 90, 0, 0, 119, 4, "true">>)
        catch
            error:badarg -> ok
        end,

    % Node must be an atom
    ok =
        try
            binary_to_term(<<131, 90, 0, 0, 106, 1:32>>)
        catch
            error:badarg -> ok
        end,

    % Test distributed pid
    Ref42 = do_setnode(test@test_node, 42),
    DistributedRef123_42 = binary_to_term(
        <<131, 90, 0, 2, 119, 14, "test@test_node", 42:32, 1:32, 2:32>>
    ),
    true = is_reference(DistributedRef123_42),
    true = DistributedRef123_42 =:= Ref123,

    DistributedBin42 = term_to_binary(TestRef),
    true = DistributedBin42 =/= Bin,
    TestRef42 = binary_to_term(DistributedBin42),
    true = TestRef42 =:= TestRef,
    ExpectedSize = byte_size(DistributedBin42) - 1,

    ok = do_unsetnode(Ref42),

    Ref43 = do_setnode(test@test_node, 43),
    DistributedRef123_43 = binary_to_term(
        <<131, 90, 0, 2, 119, 14, "test@test_node", 43:32, 1:32, 2:32>>
    ),
    true = is_reference(DistributedRef123_43),
    true = DistributedRef123_43 =:= Ref123,

    DistributedRef123_42_43 = binary_to_term(
        <<131, 90, 0, 2, 119, 14, "test@test_node", 42:32, 1:32, 2:32>>
    ),
    true = is_reference(DistributedRef123_42_43),
    false = DistributedRef123_42_43 =:= Ref123,

    DistributedBin43 = term_to_binary(TestRef),
    true = DistributedBin43 =/= Bin,
    TestRef43 = binary_to_term(DistributedBin43),
    true = TestRef43 =:= TestRef,

    ok = do_unsetnode(Ref43),
    ok.

test_encode_resource() ->
    OTPVersion = get_otp_version(),
    test_encode_resource(OTPVersion).

test_encode_resource(OTPVersion) ->
    R = socket:open(inet, stream, tcp),
    case OTPVersion of
        atomvm ->
            {ok, {Resource, _Ref}} = R;
        _ ->
            {ok, {'$socket', Resource}} = R
    end,
    true = is_reference(Resource),
    "#Ref<0." ++ _Tail = ref_to_list(Resource),
    ResourceBin = term_to_binary(Resource),
    Resource2 = binary_to_term(ResourceBin),
    true = Resource2 =:= Resource,
    % Alter the binary a little bit so that it is a different resoruce.
    if
        OTPVersion =:= atomvm ->
            <<131, 90, 0, 4, 119, 13, "nonode@nohost", 0:32, A:32, B:32, C:32, D:32>> = ResourceBin,
            AlteredResourceBin1 =
                <<131, 90, 0, 4, 119, 13, "nonode@nohost", 0:32, A:32, B:32, C:32, (D + 4):32>>,
            AlteredResourceBin2 =
                <<131, 90, 0, 4, 119, 13, "nonode@nohost", 0:32, A:32, B:32, (C + 4):32, D:32>>,
            AlteredResourceBin3 =
                <<131, 90, 0, 4, 119, 13, "nonode@nohost", 0:32, A:32, (B + 4):32, C:32, D:32>>,
            AlteredResourceBin4 =
                <<131, 90, 0, 4, 119, 13, "nonode@nohost", 0:32, (A + 4):32, B:32, C:32, D:32>>;
        true ->
            <<131, 90, 0, 3, 119, 13, "nonode@nohost", 0:32, A:32, B:32, C:32>> = ResourceBin,
            AlteredResourceBin1 =
                <<131, 90, 0, 3, 119, 13, "nonode@nohost", 0:32, A:32, B:32, (C + 1):32>>,
            AlteredResourceBin2 =
                <<131, 90, 0, 3, 119, 13, "nonode@nohost", 0:32, A:32, B:32, (C + 4):32>>,
            AlteredResourceBin3 =
                <<131, 90, 0, 3, 119, 13, "nonode@nohost", 0:32, A:32, (B + 4):32, C:32>>,
            AlteredResourceBin4 =
                <<131, 90, 0, 3, 119, 13, "nonode@nohost", 0:32, (A + 4):32, B:32, C:32>>
    end,
    AlteredResource1 = binary_to_term(AlteredResourceBin1),
    false = AlteredResource1 =:= Resource,
    AlteredResource2 = binary_to_term(AlteredResourceBin2),
    false = AlteredResource2 =:= Resource,
    AlteredResource3 = binary_to_term(AlteredResourceBin3),
    false = AlteredResource3 =:= Resource,
    AlteredResource4 = binary_to_term(AlteredResourceBin4),
    false = AlteredResource4 =:= Resource,
    ok.

test_encode_process_ref() ->
    ProcessRef = erlang:alias(),
    Bin = term_to_binary(ProcessRef),
    <<131, 90, _Len:16, 119, 13, "nonode@nohost", 0:32, _/binary>> = Bin,
    ProcessRef = binary_to_term(?MODULE:id(Bin)),
    ok.

% Verify term_to_binary(binary_to_term(Bin)) is idempotent.
binary_to_term_idempotent(Binary, _OTPVersion) ->
    Term = binary_to_term(Binary),
    Binary = term_to_binary(Term),
    Term.

test_atom_encoding() ->
    true = compare_pair_encoding(latin1_as_utf8_1),
    true = compare_pair_encoding(latin1_as_utf8_2),
    true = compare_pair_encoding(latin1_as_utf8_3),
    true = compare_pair_encoding(long_with_two_bytes_length),
    ok.

% Build the SMALL_ATOM_UTF8_EXT (tag 119) encoding for a fresh atom name,
% without writing the atom as a literal anywhere (which would add it to the
% atom table at module load).
make_fresh_small_atom_bin(Name) when is_list(Name) ->
    Bytes = list_to_binary(Name),
    Len = byte_size(Bytes),
    true = Len =< 255,
    <<131, 119, Len, Bytes/binary>>.

make_fresh_atom_utf8_bin(Name) when is_list(Name) ->
    Bytes = list_to_binary(Name),
    Len = byte_size(Bytes),
    <<131, 118, Len:16, Bytes/binary>>.

% ATOM_EXT (tag 100): latin1-encoded
make_fresh_atom_ext_bin(Bytes) when is_binary(Bytes) ->
    Len = byte_size(Bytes),
    <<131, 100, Len:16, Bytes/binary>>.

unique_atom_name(Suffix) ->
    PidStr = pid_to_list(self()),
    Now = integer_to_list(erlang:system_time(microsecond)),
    "atomvm_safe_" ++ Suffix ++ "_" ++ Now ++ "_" ++ PidStr.

test_safe_option() ->
    foo = binary_to_term(<<131, 119, 3, "foo">>, [safe]),
    {foo, bar} = binary_to_term(
        <<131, 104, 2, 119, 3, "foo", 119, 3, "bar">>, [safe]
    ),
    true = binary_to_term(<<131, 119, 4, "true">>, [safe]),

    42 = binary_to_term(<<131, 97, 42>>, [safe]),
    [] = binary_to_term(<<131, 106>>, [safe]),
    <<"hi">> = binary_to_term(<<131, 109, 0, 0, 0, 2, "hi">>, [safe]),

    {32768, 6} = binary_to_term(<<131, 98, 0, 0, 128, 0, 127>>, [safe, used]),

    FreshName1 = unique_atom_name("one"),
    FreshBin1 = make_fresh_small_atom_bin(FreshName1),
    ok = expect_badarg(fun() -> binary_to_term(FreshBin1, [safe]) end),
    FreshAtom1 = binary_to_term(FreshBin1),
    true = is_atom(FreshAtom1),
    FreshAtom1 = binary_to_term(FreshBin1, [safe]),

    % ATOM_UTF8_EXT
    FreshName2 = unique_atom_name("two"),
    FreshBin2 = make_fresh_atom_utf8_bin(FreshName2),
    ok = expect_badarg(fun() -> binary_to_term(FreshBin2, [safe]) end),
    FreshAtom2 = binary_to_term(FreshBin2),
    true = is_atom(FreshAtom2),
    FreshAtom2 = binary_to_term(FreshBin2, [safe]),

    % ATOM_EXT (tag 100), pure ASCII payload
    FreshName2a = unique_atom_name("twoa"),
    FreshBin2a = make_fresh_atom_ext_bin(list_to_binary(FreshName2a)),
    ok = expect_badarg(fun() -> binary_to_term(FreshBin2a, [safe]) end),
    FreshAtom2a = binary_to_term(FreshBin2a),
    true = is_atom(FreshAtom2a),
    FreshAtom2a = binary_to_term(FreshBin2a, [safe]),

    % ATOM_EXT (tag 100), latin1 with non-ASCII bytes
    FreshName2bAscii = unique_atom_name("twob"),
    Latin1Tail = <<16#E9, 16#FF, 16#C0>>,
    FreshBytes2b = <<(list_to_binary(FreshName2bAscii))/binary, Latin1Tail/binary>>,
    FreshBin2b = make_fresh_atom_ext_bin(FreshBytes2b),
    ok = expect_badarg(fun() -> binary_to_term(FreshBin2b, [safe]) end),
    FreshAtom2b = binary_to_term(FreshBin2b),
    true = is_atom(FreshAtom2b),
    FreshAtom2b = binary_to_term(FreshBin2b, [safe]),

    ExpectedUtf8Tail = unicode:characters_to_binary(Latin1Tail, latin1, utf8),
    ExpectedUtf8 = <<(list_to_binary(FreshName2bAscii))/binary, ExpectedUtf8Tail/binary>>,
    ExpectedUtf8 = atom_to_binary(FreshAtom2b, utf8),

    FreshName3 = unique_atom_name("three"),
    NameBin3 = list_to_binary(FreshName3),
    NameLen3 = byte_size(NameBin3),
    TupleWithFresh =
        <<131, 104, 2, 97, 1, 119, NameLen3, NameBin3/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(TupleWithFresh, [safe]) end),
    {1, _} = binary_to_term(TupleWithFresh),
    ok = expect_badarg(
        fun() ->
            FreshName4 = unique_atom_name("four"),
            NameBin4 = list_to_binary(FreshName4),
            NameLen4 = byte_size(NameBin4),
            ListWithFresh =
                <<131, 108, 0, 0, 0, 1, 119, NameLen4, NameBin4/binary, 106>>,
            binary_to_term(ListWithFresh, [safe])
        end
    ),
    % MAP_EXT
    ok = expect_badarg(
        fun() ->
            FreshName5 = unique_atom_name("five"),
            NameBin5 = list_to_binary(FreshName5),
            NameLen5 = byte_size(NameBin5),
            MapWithFresh =
                <<131, 116, 0, 0, 0, 1, 97, 1, 119, NameLen5, NameBin5/binary>>,
            binary_to_term(MapWithFresh, [safe])
        end
    ),

    % EXPORT_EXT — M:F/A must resolve to an existing export under [safe]
    ModuleBin = atom_to_binary(?MODULE, utf8),
    ModuleLen = byte_size(ModuleBin),
    ExportBinExisting =
        <<131, 113, 119, ModuleLen, ModuleBin/binary, 119, 2, "id", 97, 1>>,
    ExportFun = binary_to_term(ExportBinExisting, [safe]),
    true = is_function(ExportFun),

    FreshModName = unique_atom_name("modname"),
    FreshModBin = list_to_binary(FreshModName),
    FreshModLen = byte_size(FreshModBin),
    ExportBinFreshMod =
        <<131, 113, 119, FreshModLen, FreshModBin/binary, 119, 2, "id", 97, 1>>,
    ok = expect_badarg(fun() -> binary_to_term(ExportBinFreshMod, [safe]) end),
    ExportFun2 = binary_to_term(ExportBinFreshMod),
    true = is_function(ExportFun2),

    % EXPORT_EXT with existing atoms but the function does not exist in the
    % loaded module — [safe] must reject, plain decode must build the fun.
    ExportBinNonexistentFun =
        <<131, 113, 119, ModuleLen, ModuleBin/binary, 119, 2, "id", 97, 99>>,
    ok = expect_badarg(fun() -> binary_to_term(ExportBinNonexistentFun, [safe]) end),
    NonexistentFun = binary_to_term(ExportBinNonexistentFun),
    true = is_function(NonexistentFun),

    % EXPORT_EXT for a BIF — [safe] must accept.
    BifBin = <<131, 113, 119, 6, "erlang", 119, 1, "+", 97, 2>>,
    BifFun = binary_to_term(BifBin, [safe]),
    true = is_function(BifFun),

    % EXPORT_EXT with arity 256 — outside the valid 0..255 range.
    %% encoded as INTEGER_EXT (98), 32-bit signed
    InvalidArityBin =
        <<131, 113, 119, ModuleLen, ModuleBin/binary, 119, 2, "id", 98, 0, 0, 1, 0>>,
    ok = expect_badarg(fun() -> binary_to_term(InvalidArityBin, [safe]) end),

    % NEW_FUN_EXT
    LocalFun = fun(X) -> X * 2 end,
    NewFunBin = term_to_binary(LocalFun),
    DecodedFun = binary_to_term(NewFunBin, [safe]),
    true = is_function(DecodedFun),

    % NEW_FUN_EXT with num_free = 0 referencing a fresh module atom.
    % The whole fresh-atom check would otherwise be skipped when no free
    % variables are captured, letting [safe] accept a binary that creates
    % a brand-new module atom.
    FreshFunMod = unique_atom_name("newfunmod"),
    FreshFunModBin = list_to_binary(FreshFunMod),
    FreshFunModLen = byte_size(FreshFunModBin),
    ModuleSelfBin = atom_to_binary(?MODULE, utf8),
    ModuleSelfLen = byte_size(ModuleSelfBin),
    NewFunBody =
        <<119, FreshFunModLen, FreshFunModBin/binary, 97, 0, 97, 0, 88, 119, ModuleSelfLen,
            ModuleSelfBin/binary, 0:32, 0:32, 0:32>>,
    NewFunRest =
        <<1, 0:128, 0:32, 0:32, NewFunBody/binary>>,
    NewFunSize = byte_size(NewFunRest) + 4,
    NewFunFreshBin = <<131, 112, NewFunSize:32, NewFunRest/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(NewFunFreshBin, [safe]) end),
    DecodedFreshFun = binary_to_term(NewFunFreshBin),
    true = is_function(DecodedFreshFun),

    % NEW_FUN_EXT with num_free = 0 whose embedded pid carries a fresh node
    % atom — [safe] must reject the entire fun.
    FreshFunPidNode = unique_atom_name("funpidnode"),
    FreshFunPidNodeBin = list_to_binary(FreshFunPidNode),
    FreshFunPidNodeLen = byte_size(FreshFunPidNodeBin),
    NewFunFreshPidBody =
        <<119, ModuleSelfLen, ModuleSelfBin/binary, 97, 0, 97, 0, 88, 119, FreshFunPidNodeLen,
            FreshFunPidNodeBin/binary, 0:32, 0:32, 0:32>>,
    NewFunFreshPidRest = <<1, 0:128, 0:32, 0:32, NewFunFreshPidBody/binary>>,
    NewFunFreshPidSize = byte_size(NewFunFreshPidRest) + 4,
    NewFunFreshPidBin = <<131, 112, NewFunFreshPidSize:32, NewFunFreshPidRest/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(NewFunFreshPidBin, [safe]) end),
    DecodedFreshPidFun = binary_to_term(NewFunFreshPidBin),
    true = is_function(DecodedFreshPidFun),

    % NEW_FUN_EXT with num_free = 1 whose captured free var is a fresh atom
    % — [safe] must reject.
    FreshFreeVar = unique_atom_name("freevar"),
    FreshFreeVarBin = list_to_binary(FreshFreeVar),
    FreshFreeVarLen = byte_size(FreshFreeVarBin),
    NewFunFreshFreeBody =
        <<119, ModuleSelfLen, ModuleSelfBin/binary, 97, 0, 97, 0, 88, 119, ModuleSelfLen,
            ModuleSelfBin/binary, 0:32, 0:32, 0:32, 119, FreshFreeVarLen, FreshFreeVarBin/binary>>,
    NewFunFreshFreeRest = <<1, 0:128, 0:32, 1:32, NewFunFreshFreeBody/binary>>,
    NewFunFreshFreeSize = byte_size(NewFunFreshFreeRest) + 4,
    NewFunFreshFreeBin =
        <<131, 112, NewFunFreshFreeSize:32, NewFunFreshFreeRest/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(NewFunFreshFreeBin, [safe]) end),
    DecodedFreshFreeFun = binary_to_term(NewFunFreshFreeBin),
    true = is_function(DecodedFreshFreeFun),

    FreshNodeName = unique_atom_name("nodename"),
    NodeBin = list_to_binary(FreshNodeName),
    NodeLen = byte_size(NodeBin),
    PidWithFreshNode =
        <<131, 88, 119, NodeLen, NodeBin/binary, 1:32, 0:32, 0:32>>,
    ok = expect_badarg(fun() -> binary_to_term(PidWithFreshNode, [safe]) end),
    DecodedPid = binary_to_term(PidWithFreshNode),
    true = is_pid(DecodedPid),

    PidWithFreshNode2 =
        <<131, 88, 119, NodeLen, NodeBin/binary, 2:32, 0:32, 0:32>>,
    DecodedPid2 = binary_to_term(PidWithFreshNode2, [safe]),
    true = is_pid(DecodedPid2),

    ok.

test_invalid_export_fun_encoding() ->
    ModuleSelfBin = atom_to_binary(?MODULE, utf8),
    ModuleSelfLen = byte_size(ModuleSelfBin),
    PidBin = <<88, 119, ModuleSelfLen, ModuleSelfBin/binary, 0:32, 0:32, 0:32>>,

    %% EXPORT_EXT with non-atom module
    ok = expect_badarg(
        fun() ->
            binary_to_term(<<131, 113, 97, 42, 119, 3, "foo", 97, 0>>)
        end
    ),
    %% EXPORT_EXT with non-atom function
    ok = expect_badarg(
        fun() ->
            binary_to_term(<<131, 113, 119, 3, "foo", 97, 42, 97, 0>>)
        end
    ),
    %% EXPORT_EXT with non-integer arity (atom in place of arity)
    ok = expect_badarg(
        fun() ->
            binary_to_term(<<131, 113, 119, 3, "foo", 119, 3, "bar", 119, 3, "baz">>)
        end
    ),

    %% NEW_FUN_EXT with non-atom module
    Body1 = <<97, 42, 97, 0, 97, 0, PidBin/binary>>,
    Rest1 = <<1, 0:128, 0:32, 0:32, Body1/binary>>,
    Size1 = byte_size(Rest1) + 4,
    Bin1 = <<131, 112, Size1:32, Rest1/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(Bin1) end),

    %% NEW_FUN_EXT with non-integer old_index (atom in place)
    Body2 =
        <<119, ModuleSelfLen, ModuleSelfBin/binary, 119, 3, "abc", 97, 0, PidBin/binary>>,
    Rest2 = <<1, 0:128, 0:32, 0:32, Body2/binary>>,
    Size2 = byte_size(Rest2) + 4,
    Bin2 = <<131, 112, Size2:32, Rest2/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(Bin2) end),

    %% NEW_FUN_EXT with non-integer old_uniq (atom in place)
    Body3 =
        <<119, ModuleSelfLen, ModuleSelfBin/binary, 97, 0, 119, 3, "abc", PidBin/binary>>,
    Rest3 = <<1, 0:128, 0:32, 0:32, Body3/binary>>,
    Size3 = byte_size(Rest3) + 4,
    Bin3 = <<131, 112, Size3:32, Rest3/binary>>,
    ok = expect_badarg(fun() -> binary_to_term(Bin3) end),

    %% NEW_FUN_EXT with non-pid in pid slot (integer in place).
    %% OTP < 27 accepts this; OTP 27+ and AtomVM reject it.
    case get_otp_version() of
        OTPVersion when is_integer(OTPVersion), OTPVersion < 27 ->
            ok;
        _ ->
            Body4 =
                <<119, ModuleSelfLen, ModuleSelfBin/binary, 97, 0, 97, 0, 97, 42>>,
            Rest4 = <<1, 0:128, 0:32, 0:32, Body4/binary>>,
            Size4 = byte_size(Rest4) + 4,
            Bin4 = <<131, 112, Size4:32, Rest4/binary>>,
            ok = expect_badarg(fun() -> binary_to_term(Bin4) end)
    end,
    ok.

test_atom_utf8_ext_node() ->
    %% OTP accepts ATOM_UTF8_EXT (tag 118) for the embedded node atom in
    %% NEW_PID_EXT, V4_PORT_EXT, and NEWER_REFERENCE_EXT, even though OTP29
    %% encoder always emits SMALL_ATOM_UTF8_EXT for short node names.
    NodeUtf8 = <<118, 0, 13, "nonode@nohost">>,

    %% NEW_PID_EXT
    PidBin = <<131, 88, NodeUtf8/binary, 1:32, 0:32, 0:32>>,
    Pid = binary_to_term(PidBin),
    true = is_pid(Pid),

    %% V4_PORT_EXT
    PortBin = <<131, 120, NodeUtf8/binary, 1:64, 0:32>>,
    Port = binary_to_term(PortBin),
    true = is_port(Port),

    %% NEWER_REFERENCE_EXT
    RefBin = <<131, 90, 0, 2, NodeUtf8/binary, 0:32, 1:32, 2:32>>,
    Ref = binary_to_term(RefBin),
    true = is_reference(Ref),
    ok.

test_invalid_bit_binary() ->
    %% BIT_BINARY_EXT (tag 77) layout: <<77, Len:32, Bits:8, Data:Len/bytes>>.
    %% Bits is the number of significant bits in the trailing byte: it must be
    %% in 1..8 (8 meaning a byte-aligned last byte) and Len must be at least 1.
    %% Anything else is a malformed encoding and must be rejected, matching OTP.

    %% Bits = 0 with a non-empty binary
    ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 1, 0, 128>>) end),
    %% Bits = 9 (> 8)
    ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 1, 9, 128>>) end),
    %% Bits = 255 (> 8)
    ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 1, 255, 128>>) end),
    %% Len = 0 with non-zero trailing bits
    ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 0, 5>>) end),

    ok = test_cve_2026_54890(),

    %% Valid encodings still decode. Bits = 8 is a byte-aligned last byte.
    <<255>> = binary_to_term(<<131, 77, 0, 0, 0, 1, 8, 255>>),
    <<1:1>> = binary_to_term(<<131, 77, 0, 0, 0, 1, 1, 128>>),
    <<255, 5:7>> = binary_to_term(<<131, 77, 0, 0, 0, 2, 7, 255, 10>>),
    ok.

test_cve_2026_54890() ->
    case rejects_zero_length_bit_binary() of
        false ->
            ok;
        true ->
            ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 0, 0>>) end),
            ok = expect_badarg(fun() -> binary_to_term(<<131, 77, 0, 0, 0, 0, 0>>, [safe]) end)
    end.

rejects_zero_length_bit_binary() ->
    case erlang:system_info(machine) of
        "BEAM" -> erts_has_cve_2026_54890_fix(erts_version());
        _ -> true
    end.

%% Fixed in erts 15.2.7.11 (OTP 27.3.4.15), 16.4.0.4 (OTP 28.5.0.4) and
%% 17.0.4 (OTP 29.0.4). Affected: 15.0-15.2.7.10, 16.0-16.4.0.3, 17.0-17.0.3.
erts_has_cve_2026_54890_fix(Version) ->
    (Version >= [15, 2, 7, 11] andalso Version < [16]) orelse
        (Version >= [16, 4, 0, 4] andalso Version < [17]) orelse
        Version >= [17, 0, 4].

erts_version() ->
    erts_version(erlang:system_info(version), 0, []).

erts_version([], Current, Acc) ->
    lists:reverse([Current | Acc]);
erts_version([$. | Rest], Current, Acc) ->
    erts_version(Rest, 0, [Current | Acc]);
erts_version([C | Rest], Current, Acc) when C >= $0, C =< $9 ->
    erts_version(Rest, Current * 10 + (C - $0), Acc).

make_binterm_fun(Id) ->
    fun() ->
        Bin = ?MODULE:get_binary(Id),
        erlang:binary_to_term(Bin)
    end.

compare_pair(Id) ->
    A = ?MODULE:get_atom(Id),
    B = ?MODULE:get_binary(Id),
    A == erlang:binary_to_term(B).

compare_pair_encoding(Id) ->
    A = ?MODULE:get_atom(Id),
    B = ?MODULE:get_binary(Id),
    A = erlang:binary_to_term(erlang:term_to_binary(A)),
    B == erlang:term_to_binary(A).

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

sleep(Ms) ->
    receive
    after Ms -> ok
    end.

id(X) ->
    X.
