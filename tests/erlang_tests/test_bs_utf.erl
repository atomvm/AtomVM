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
    ok = test_put_utf16(),
    ok = test_put_utf32(),
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
    ok.

id(I) -> I.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.
