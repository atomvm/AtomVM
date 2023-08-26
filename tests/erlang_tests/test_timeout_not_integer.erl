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

-module(test_timeout_not_integer).

-export([start/0]).

start() ->
    test_infinity() + test_float().

test_infinity() ->
    Parent = self(),
    _Pid1 = spawn_opt(fun() -> waiter1(Parent) end, []),
    _Pid2 = spawn_opt(fun() -> waiter2(Parent) end, []),
    receive
        _ -> 1
        % wait 2 secs as with bug we waited few ms
    after 2000 ->
        0
    end.

waiter1(Parent) ->
    Result =
        receive
            _ -> unexpected
        after infinity -> infinity
        end,
    Parent ! {waiter1, Result}.

waiter2(Parent) ->
    waiter2(Parent, infinity).

waiter2(Parent, Timeout) ->
    Result =
        receive
            _ -> unexpected
        after Timeout -> infinity
        end,
    Parent ! {waiter2, Result}.

test_float() ->
    try
        receive
            _ -> 20
        after 1.0 -> 10
        end
    catch
        error:timeout_value -> 0
    end.
