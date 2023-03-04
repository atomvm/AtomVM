%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(test_bs_utf).

-export([start/0, id/1]).

start() ->
    ok = test_put_utf8(),
    ok = test_get_utf8(),
    ok = test_skip_utf8(),
    ok = test_put_utf16(),
    ok = test_get_utf16(),
    ok = test_skip_utf16(),
    ok = test_put_utf32(),
    ok = test_get_utf32(),
    ok = test_skip_utf32(),
    0.

test_put_utf8() ->
    X0 = ?MODULE:id(16#10437),
    B0 = <<X0/utf8>>,
    <<240, 144, 144, 183>> = B0,
    X1 = ?MODULE:id($暑),
    B1 = <<X1/utf8>>,
    <<230, 154, 145>> = B1,
    X2 = ?MODULE:id($é),
    B2 = <<X2/utf8>>,
    <<195, 169>> = B2,
    X3 = ?MODULE:id($e),
    B3 = <<X3/utf8>>,
    <<101>> = B3,
    XF1 = ?MODULE:id([]),
    ok = assert_badarg(fun() -> <<XF1/utf8>> end),
    XF2 = ?MODULE:id(-1),
    ok = assert_badarg(fun() -> <<XF2/utf8>> end),
    XF3 = ?MODULE:id(16#110000),
    ok = assert_badarg(fun() -> <<XF3/utf8>> end),
    XF4 = ?MODULE:id(16#D800),
    ok = assert_badarg(fun() -> <<XF4/utf8>> end),
    ok.

test_get_utf8() ->
    B0 = ?MODULE:id(<<240, 144, 144, 183>>),
    <<X0/utf8, Rest/binary>> = B0,
    X0 = ?MODULE:id(16#10437),
    <<"">> = Rest,

    B1 = ?MODULE:id(<<0, 230, 154, 145>>),
    <<_:8, X1/utf8>> = B1,
    X1 = ?MODULE:id($暑),

    B2 = ?MODULE:id(<<$g, $a, $r, $b, $a, $g, $e, 195, 169, $t, $r, $a, $s, $h>>),
    <<Garbage:7/binary, X2/utf8, Trash:5/binary>> = B2,
    <<"garbage">> = Garbage,
    X2 = ?MODULE:id($é),
    <<"trash">> = Trash,

    B3 = ?MODULE:id(<<101>>),
    <<X3/utf8>> = B3,
    X3 = ?MODULE:id($e),

    B4 = ?MODULE:id(<<127>>),
    <<X4/utf8>> = B4,
    X4 = ?MODULE:id(16#7F),

    B5 = ?MODULE:id(<<194, 128>>),
    <<X5/utf8>> = B5,
    X5 = ?MODULE:id(16#80),

    B6 = ?MODULE:id(<<223, 191>>),
    <<X6/utf8>> = B6,
    X6 = ?MODULE:id(16#7FF),

    B7 = ?MODULE:id(<<224, 160, 128>>),
    <<X7/utf8>> = B7,
    X7 = ?MODULE:id(16#800),

    XF1 = ?MODULE:id(<<"">>),
    ok = assert_badmatch(fun() ->
        <<XFM1/utf8>> = XF1,
        XFM1
    end),

    %% improper encoding -- too short!
    verify_illegal_utf8_encoding(<<240, 144, 144>>),

    % encoding of D800-DFFF illegal
    verify_illegal_utf8_encoding(<<2#11101101, 2#10100000, 2#10000000>>),

    % encoding of overlong illegal
    verify_illegal_utf8_encoding(<<2#11110000, 2#10000000, 2#10000001, 2#10000101>>),
    verify_illegal_utf8_encoding(<<2#11100000, 2#10000001, 2#10000101>>),
    verify_illegal_utf8_encoding(<<2#11000000, 2#10000101>>),

    ok.

test_skip_utf8() ->
    B0 = ?MODULE:id(<<0, 240, 144, 144, 183, 1>>),
    <<X:8, _X0/utf8, Y:8>> = B0,
    0 = X,
    1 = Y,

    ok.

test_put_utf16() ->
    X0 = ?MODULE:id(16#10437),
    B0 = <<X0/utf16>>,
    <<216, 1, 220, 55>> = B0,
    X1 = ?MODULE:id($暑),
    B1 = <<X1/utf16>>,
    <<102, 145>> = B1,
    X2 = ?MODULE:id($é),
    B2 = <<X2/utf16>>,
    <<0, 233>> = B2,
    X3 = ?MODULE:id($e),
    B3 = <<X3/utf16>>,
    <<0, 101>> = B3,
    X4 = ?MODULE:id($a),
    B4 = <<X4/utf16-big>>,
    <<0, 97>> = B4,
    B5 = <<X4/utf16-little>>,
    <<97, 0>> = B5,
    BugOTP17713 =
        case erlang:system_info(machine) of
            "BEAM" ->
                Version = [
                    list_to_integer(N)
                 || N <- string:tokens(erlang:system_info(version), ".")
                ],
                Version < [12, 2];
            _ ->
                false
        end,
    if
        BugOTP17713 ->
            ok;
        true ->
            B6 = <<X4/utf16-native>>,
            <<97:16/native>> = B6
    end,
    B7 = <<X0/utf16-big>>,
    <<216, 1, 220, 55>> = B7,
    B8 = <<X0/utf16-little>>,
    <<1, 216, 55, 220>> = B8,
    XF1 = ?MODULE:id([]),
    ok = assert_badarg(fun() -> <<XF1/utf16>> end),
    XF2 = ?MODULE:id(-1),
    ok = assert_badarg(fun() -> <<XF2/utf16>> end),
    XF3 = ?MODULE:id(16#110000),
    ok = assert_badarg(fun() -> <<XF3/utf16>> end),
    XF4 = ?MODULE:id(16#D800),
    ok = assert_badarg(fun() -> <<XF4/utf16>> end),
    ok.

test_get_utf16() ->
    B0 = ?MODULE:id(<<216, 1, 220, 55>>),
    <<X0/utf16>> = B0,
    X0 = 16#10437,

    B1 = ?MODULE:id(<<102, 145>>),
    <<X1/utf16>> = B1,
    $暑 = X1,

    B2 = ?MODULE:id(<<0, 233>>),
    <<X2/utf16>> = B2,
    $é = X2,

    B3 = ?MODULE:id(<<0, 101>>),
    <<X3/utf16>> = B3,
    $e = X3,

    B4 = ?MODULE:id(<<0, 97>>),
    <<X4/utf16-big>> = B4,
    $a = X4,

    B5 = ?MODULE:id(<<97, 0>>),
    <<X5/utf16-little>> = B5,
    $a = X5,

    BugOTP17713 =
        case erlang:system_info(machine) of
            "BEAM" ->
                Version = [
                    list_to_integer(N)
                 || N <- string:tokens(erlang:system_info(version), ".")
                ],
                Version < [12, 2];
            _ ->
                false
        end,
    if
        BugOTP17713 ->
            ok;
        true ->
            B6 = ?MODULE:id(<<97:16/native>>),
            <<X6/utf16-native>> = B6,
            $a = X6
    end,

    B7 = ?MODULE:id(<<216, 1, 220, 55>>),
    <<X7/utf16-big>> = B7,
    16#10437 = X7,

    B8 = ?MODULE:id(<<1, 216, 55, 220>>),
    <<X8/utf16-little>> = B8,
    16#10437 = X8,

    XF1 = ?MODULE:id(<<"">>),
    ok = assert_badmatch(fun() ->
        <<XFM1/utf16>> = XF1,
        XFM1
    end),

    %% improper encoding -- too short!
    verify_illegal_utf16_encoding(<<216, 1, 220>>),
    %% improper encoding -- gt 0x10FFFF
    verify_illegal_utf16_encoding(<<4, 0, 0, 0>>),
    verify_illegal_utf16_encoding(maybe_illegal_utf16_encode(0)),
    verify_illegal_utf16_encoding(maybe_illegal_utf16_encode(16#D800)),
    verify_illegal_utf16_encoding(maybe_illegal_utf16_encode(16#DFFF)),
    verify_illegal_utf16_encoding(maybe_illegal_utf16_encode(-1)),

    ok.

test_skip_utf16() ->
    B0 = ?MODULE:id(<<0, 216, 1, 220, 55, 1>>),
    <<X:8, _X0/utf16, Y:8>> = B0,
    0 = X,
    1 = Y,

    ok.

test_put_utf32() ->
    X0 = ?MODULE:id(16#10437),
    B0 = <<X0/utf32>>,
    <<0, 1, 4, 55>> = B0,
    X1 = ?MODULE:id($暑),
    B1 = <<X1/utf32>>,
    <<0, 0, 102, 145>> = B1,
    X2 = ?MODULE:id($é),
    B2 = <<X2/utf32>>,
    <<0, 0, 0, 233>> = B2,
    X3 = ?MODULE:id($e),
    B3 = <<X3/utf32>>,
    <<0, 0, 0, 101>> = B3,
    X4 = ?MODULE:id($a),
    B4 = <<X4/utf32-big>>,
    <<0, 0, 0, 97>> = B4,
    B5 = <<X4/utf32-little>>,
    <<97, 0, 0, 0>> = B5,
    B6 = <<X4/utf32-native>>,
    <<97:32/native>> = B6,
    B7 = <<X0/utf32-big>>,
    <<0, 1, 4, 55>> = B7,
    B8 = <<X0/utf32-little>>,
    <<55, 4, 1, 0>> = B8,
    XF1 = ?MODULE:id([]),
    ok = assert_badarg(fun() -> <<XF1/utf32>> end),
    XF2 = ?MODULE:id(-1),
    ok = assert_badarg(fun() -> <<XF2/utf32>> end),
    XF3 = ?MODULE:id(16#110000),
    ok = assert_badarg(fun() -> <<XF3/utf32>> end),
    XF4 = ?MODULE:id(16#D800),
    ok = assert_badarg(fun() -> <<XF4/utf32>> end),
    ok.

test_get_utf32() ->
    B0 = ?MODULE:id(<<0, 1, 4, 55>>),
    <<X0/utf32>> = B0,
    16#10437 = X0,

    B1 = ?MODULE:id(<<0, 0, 102, 145>>),
    <<X1/utf32>> = B1,
    $暑 = X1,

    B2 = ?MODULE:id(<<0, 0, 0, 233>>),
    <<X2/utf32>> = B2,
    $é = X2,

    B3 = ?MODULE:id(<<0, 0, 0, 101>>),
    <<X3/utf32>> = B3,
    $e = X3,

    B4 = ?MODULE:id(<<0, 0, 0, 97>>),
    <<X4/utf32>> = B4,
    $a = X4,

    B5 = ?MODULE:id(<<97, 0, 0, 0>>),
    <<X4/utf32-little>> = B5,

    B6 = ?MODULE:id(<<97:32/native>>),
    <<X4/utf32-native>> = B6,

    B7 = ?MODULE:id(<<0, 1, 4, 55>>),
    <<X0/utf32-big>> = B7,

    B8 = ?MODULE:id(<<55, 4, 1, 0>>),
    <<X0/utf32-little>> = B8,

    verify_illegal_utf32_encoding(<<1, 2, 3>>),
    verify_illegal_utf32_encoding(<<255, 255, 255, 255>>),
    verify_illegal_utf32_encoding(<<255, 255, 255, 255>>),
    verify_illegal_utf32_encoding(<<0, 0, 16#D8, 16#00>>),
    verify_illegal_utf32_encoding(<<0, 0, 16#D8, 16#FF>>),

    ok.

test_skip_utf32() ->
    B0 = ?MODULE:id(<<0, 0, 1, 4, 55, 1>>),
    <<X:8, _X0/utf32, Y:8>> = B0,
    0 = X,
    1 = Y,

    ok.

id(I) -> I.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.

assert_badmatch(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:{badmatch, _} -> ok
    end.

verify_illegal_utf8_encoding(X) ->
    ok = assert_badmatch(
        fun() ->
            <<XF/utf8>> = X,
            XF
        end
    ).

verify_illegal_utf16_encoding(X) ->
    ok = assert_badmatch(
        fun() ->
            <<XF/utf16>> = X,
            XF
        end
    ).

verify_illegal_utf32_encoding(X) ->
    ok = assert_badmatch(
        fun() ->
            <<XF/utf32>> = X,
            XF
        end
    ).

maybe_illegal_utf16_encode(C0) ->
    C1 = C0 - 16#10000,
    A = (C1 bsr 18) bor 16#D8,
    B = (C1 bsr 10) band 16#FF,
    C = ((C1 bsr 8) band 16#03) bor 16#DC,
    D = (C1 bsr 0) band 16#FF,
    <<A, B, C, D>>.
