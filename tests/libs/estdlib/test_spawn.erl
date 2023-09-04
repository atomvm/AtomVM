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

-module(test_spawn).

-export([test/0, f/1]).

-include("etest.hrl").

test() ->
    ok = test_spawn(),
    ok = test_spawn_link(),
    ok.

%% spawn/1, spawn/3, spawn_link/1, spawn_link/3 are not nifs but implemented
%% in erlang module.

test_spawn() ->
    Parent = self(),
    Pid1 = spawn(fun() -> f(Parent) end),
    ok =
        receive
            {Pid1, {links, []}} -> ok
        after 500 -> timeout
        end,
    Pid2 = spawn(?MODULE, f, [Parent]),
    ok =
        receive
            {Pid2, {links, []}} -> ok
        after 500 -> timeout
        end,
    ok.

test_spawn_link() ->
    Parent = self(),
    Pid1 = spawn_link(fun() -> f(Parent) end),
    ok =
        receive
            {Pid1, {links, [Parent]}} -> ok
        after 500 -> timeout
        end,
    Pid2 = spawn_link(?MODULE, f, [Parent]),
    ok =
        receive
            {Pid2, {links, [Parent]}} -> ok
        after 500 -> timeout
        end,
    ok.

f(Parent) ->
    Parent ! {self(), erlang:process_info(self(), links)}.
