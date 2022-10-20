%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
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

-module(trap_exit_flag).

-export([start/0, sum/1, proc/1]).

start() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    Pid = spawn(?MODULE, proc, [L]),
    false = erlang:process_flag(trap_exit, true),
    erlang:link(Pid),
    Pid ! {self(), sum},
    receive
        {'EXIT', Pid, {{nocatch, test}, EL}} when is_pid(Pid) andalso is_list(EL) ->
            1;
        {'EXIT', _, normal} = T ->
            erlang:display(T),
            2;
        {'EXIT', _, _} = T ->
            erlang:display(T),
            3;
        T when is_tuple(T) ->
            erlang:display(T),
            4;
        T ->
            erlang:display(T),
            5
    after 30000 ->
        erlang:display(timeout),
        6
    end.

proc(_L) ->
    receive
        {_Pid, sum} ->
            throw(test)
    end.

sum([]) ->
    0;
sum([H | T]) ->
    H + sum(T).
