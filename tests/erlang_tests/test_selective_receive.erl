%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(test_selective_receive).

-export([start/0]).

start() ->
    test_selective_receive(),
    test_selective_receive_with_timeout(),
    0.

test_selective_receive() ->
    Self = self(),
    spawn_opt(
        fun() ->
            Self ! three
        end,
        []
    ),
    spawn_opt(
        fun() ->
            receive
            after 250 -> ok
            end,
            Self ! two
        end,
        []
    ),
    spawn_opt(
        fun() ->
            receive
            after 500 -> ok
            end,
            Self ! one
        end,
        []
    ),
    receive
        one ->
            ok
    end,
    receive
        two ->
            ok
    end,
    receive
        three ->
            ok
    end.

test_selective_receive_with_timeout() ->
    Self = self(),
    spawn_opt(
        fun() ->
            Self ! two
        end,
        []
    ),
    spawn_opt(
        fun() ->
            receive
            after 500 -> ok
            end,
            Self ! one
        end,
        []
    ),
    ok =
        receive
            one ->
                expected_timeout
        after 250 -> ok
        end,
    receive
        two ->
            ok
    end.
