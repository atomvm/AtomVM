%
% This file is part of AtomVM.
%
% Copyright 2025 Peter Madsen-Mygdal
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

-module(test_multi_value_comprehension).

-export([start/0, id/1]).

%% Test multi-valued comprehensions (EEP 78, OTP 29+)

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 29).
-define(OTP29_OR_LATER, true).
-endif.
-endif.

-ifdef(OTP29_OR_LATER).

start() ->
    ok = test_basic_multi_value(),
    ok = test_single_value(),
    ok = test_multi_value_with_filter(),
    ok = test_multi_value_binary_comprehension(),
    0.

test_basic_multi_value() ->
    [-1, 1, -2, 2, -3, 3] = [-I, I || I <- ?MODULE:id([1, 2, 3])],
    ok.

test_single_value() ->
    [1, 2, 3] = [I || I <- ?MODULE:id([1, 2, 3])],
    ok.

test_multi_value_with_filter() ->
    [-2, 2] = [-I, I || I <- ?MODULE:id([1, 2, 3]), I rem 2 =:= 0],
    ok.

test_multi_value_binary_comprehension() ->
    <<1, 2, 3, 4>> = <<<<I, (I + 1)>> || I <- ?MODULE:id([1, 3])>>,
    ok.

-else.

start() ->
    0.

-endif.

id(X) -> X.
