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

-module(test_smp).
-export([start/0]).

start() ->
    {Pid1, Monitor1} = spawn_opt(fun count_loop/0, [monitor]),
    {Pid2, Monitor2} = spawn_opt(fun count_loop/0, [monitor]),
    {Pid3, Monitor3} = spawn_opt(fun count_loop/0, [monitor]),
    {Pid4, Monitor4} = spawn_opt(fun count_loop/0, [monitor]),
    receive
        {'DOWN', Monitor1, process, Pid1, normal} -> ok
    end,
    receive
        {'DOWN', Monitor2, process, Pid2, normal} -> ok
    end,
    receive
        {'DOWN', Monitor3, process, Pid3, normal} -> ok
    end,
    receive
        {'DOWN', Monitor4, process, Pid4, normal} -> ok
    end,
    ok.

count_loop() ->
    count_loop(1000).

count_loop(N) when N < 0 -> ok;
count_loop(X) ->
    case X rem 2 of
        0 -> count_loop(X - 1);
        1 -> count_loop(X - 3)
    end.
