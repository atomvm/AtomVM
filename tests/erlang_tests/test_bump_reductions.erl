%
% This file is part of AtomVM.
%
% Copyright 2025 Franciszek Kubis <franciszek.kubis@swmansion.com>
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

-module(test_bump_reductions).

-export([start/0, process_test/0]).

start() ->
    {reductions, InitialReductions0} = erlang:process_info(self(), reductions),
    erlang:bump_reductions(1),
    InitialReductions1 = InitialReductions0 + 2,
    {reductions, InitialReductions1} = erlang:process_info(self(), reductions),
    Pid = erlang:spawn_opt(fun() -> process_test() end, [link]),
    Pid ! {ready, self()},
    ReceivedReductions = 
        receive 
            {r1, {reductions, Reductions}} -> Reductions 
        end,
    
    receive
        {r2, {reductions, Reductions2}} ->
            Reductions2 = ReceivedReductions + 1027
    end,
    erlang:bump_reductions(2000),
    FinalReductions = InitialReductions1 + 2003,
    {reductions, FinalReductions} = erlang:process_info(self(), reductions),
    0.

process_test() ->
    receive
        {ready, Pid} ->
            Pid ! {r1, erlang:process_info(self(), reductions)},
            erlang:bump_reductions(1025),
            Pid ! {r2, erlang:process_info(self(), reductions)}
    end.
