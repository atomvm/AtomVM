%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_min_max_guard).

-export([start/0, test_guard/4, test_without_guard/4]).

start() ->
    ok = test_guard(min, 2, 1, 2),
    ok = test_guard(max, 2, 1, 1),
    ok = test_without_guard(min, 2, 1, 2),
    ok = test_without_guard(max, 2, 1, 1),
    ok = test_tail(),
    0.

-ifdef(OTP_RELEASE).
%% OTP 21 or higher
-if(?OTP_RELEASE >= 26).
test_guard(min, X, Y, Z) when min(X, Y) < Z ->
    ok;
test_guard(max, X, Y, Z) when max(X, Y) > Z ->
    ok;
test_guard(_Op, _X, _Y, _Z) ->
    fail.
-else.
test_guard(_Op, _X, _Y, _Z) ->
    ok.
-endif.
-else.
test_guard(_Op, _X, _Y, _Z) ->
    ok.
-endif.

test_without_guard(min, X, Y, Z) ->
    case min(X, Y) < Z of
        true -> ok;
        false -> fail
    end;
test_without_guard(max, X, Y, Z) ->
    case max(X, Y) > Z of
        true -> ok;
        false -> fail
    end;
test_without_guard(_Op, _X, _Y, _Z) ->
    fail.

test_tail() ->
    1 = tail_min(infinity, [5, 4, 3, 2, 1]),
    5 = tail_max(0, [1, 2, 3, 4, 5]),
    ok.

tail_min(X, [Y]) ->
    % OP_CALL_EXT_ONLY
    min(X, Y);
tail_min(X, [H | T]) ->
    tail_min(min(X, H), T).

tail_max(X, [Y, Z]) ->
    M1 = max(Y, Z),
    % OP_CALL_EXT_LAST
    max(X, M1);
tail_max(X, [H | T]) ->
    tail_max(max(X, H), T).
