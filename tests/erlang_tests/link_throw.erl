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

-module(link_throw).

-export([start/0, sum/1, proc/1]).

start() ->
    {Pid, Ref} = spawn_opt(fun start2/0, [monitor]),
    Pid ! {self(), sum},
    CM =
        receive
            N2 -> N2
        end,
    case CM of
        {'DOWN', Ref, process, Pid, {{nocatch, test}, L}} when
            is_reference(Ref) andalso
                is_pid(Pid) andalso
                is_list(L)
        ->
            1;
        {'DOWN', _, process, _, normal} ->
            2;
        {'DOWN', _, process, _, _} ->
            3;
        T when is_tuple(T) ->
            4;
        _ ->
            5
    end.

start2() ->
    L = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    _Parent =
        receive
            {ParentPid, sum} -> ParentPid
        end,
    Pid = spawn_opt(?MODULE, proc, [L], []),
    erlang:link(Pid),
    Pid ! {self(), sum},
    receive
        N when is_integer(N) -> N
    end,
    CM =
        receive
            N2 -> N2
        end,
    case CM of
        {'DOWN', Ref, process, Pid, normal} when is_reference(Ref) andalso is_pid(Pid) ->
            1;
        {'DOWN', _, process, _, normal} ->
            2;
        {'DOWN', _, process, _, _} ->
            3;
        T when is_tuple(T) ->
            4;
        _ ->
            5
    end.

proc(L) ->
    receive
        {Pid, sum} ->
            Pid ! sum(L),
            throw(test)
    end.

sum([]) ->
    0;
sum([H | T]) ->
    H + sum(T).
