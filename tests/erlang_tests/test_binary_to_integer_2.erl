%
% This file is part of AtomVM.
%
% Copyright 2024 Yuto Oguchi <oguchiyuto@realglobe.jp>, Realglobe Inc.
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

-module(test_binary_to_integer_2).

-export([start/0, id/1]).

start() ->
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"10">>), -1) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"10">>), 0) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"10">>), 1) end),
    2 = binary_to_integer(?MODULE:id(<<"10">>), 2),
    36 = binary_to_integer(?MODULE:id(<<"10">>), 36),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"10">>), 37) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"">>), 10) end),
    10 = binary_to_integer(?MODULE:id(<<"0A">>), 16),
    10 = binary_to_integer(?MODULE:id(<<"0a">>), 16),

    1330 = binary_to_integer(?MODULE:id(<<"AAA">>), 11),
    1330 = binary_to_integer(?MODULE:id(<<"0000AAA">>), 11),
    1330 = binary_to_integer(?MODULE:id(<<"+AAA">>), 11),
    1330 = binary_to_integer(?MODULE:id(<<"+00000AAA">>), 11),
    -1330 = binary_to_integer(?MODULE:id(<<"-AAA">>), 11),
    -1330 = binary_to_integer(?MODULE:id(<<"-0000AAA">>), 11),

    2147483647 = binary_to_integer(?MODULE:id(<<"2147483647">>), 10),
    -2147483648 = binary_to_integer(?MODULE:id(<<"-2147483648">>), 10),
    2147483648 = binary_to_integer(?MODULE:id(<<"2147483648">>), 10),
    -2147483649 = binary_to_integer(?MODULE:id(<<"-2147483649">>), 10),
    9223372036854775807 = binary_to_integer(?MODULE:id(<<"00009223372036854775807">>), 10),
    -9223372036854775808 = binary_to_integer(?MODULE:id(<<"-009223372036854775808">>), 10),

    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"102">>), 2) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0000009">>), 7) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"9">>), 7) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"9">>), 9) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"FF">>), 15) end),

    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"123     ">>), 10) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<" 123">>), 10) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<" 0xFF">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0xab">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0xAB">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0XAB">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0x">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"00000x5">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0x000005">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"0x0x5">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<"-0xAB">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<" -0xAB">>), 16) end),
    ok = assert_badarg(fun() -> binary_to_integer(?MODULE:id(<<" +0xAB">>), 16) end),

    0.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.

id(B) ->
    B.
