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

-export([start/0]).

start() ->
    ok = assert_badarg(fun() -> binary_to_integer(<<"10">>, -1) end),
    ok = assert_badarg(fun() -> binary_to_integer(<<"10">>, 0) end),
    ok = assert_badarg(fun() -> binary_to_integer(<<"10">>, 1) end),
    2 = binary_to_integer(<<"10">>, 2),
    36 = binary_to_integer(<<"10">>, 36),
    ok = assert_badarg(fun() -> binary_to_integer(<<"10">>, 37) end),
    ok = assert_badarg(fun() -> binary_to_integer(<<"">>, 10) end),
    10 = binary_to_integer(<<"0A">>, 16),
    10 = binary_to_integer(<<"0a">>, 16),
    0.

assert_badarg(F) ->
    try
        R = F(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.
