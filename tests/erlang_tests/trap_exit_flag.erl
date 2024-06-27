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

-export([start/0]).

start() ->
    false = erlang:process_flag(trap_exit, true),
    ok = test_nocatch(),
    ok = test_normal(),
    true = erlang:process_flag(trap_exit, false),
    ok = test_trap_exit_false(),
    0.

test_nocatch() ->
    MyPid = self(),
    Pid = spawn_opt(fun proc/0, []),
    erlang:link(Pid),
    erlang:link(Pid),
    {links, LinkedPids1} = erlang:process_info(MyPid, links),
    1 = no_of_linked_pids(Pid, LinkedPids1),
    Pid ! do_throw,
    ok =
        receive
            {'EXIT', Pid, {{nocatch, test}, EL}} when is_list(EL) ->
                {links, LinkedPids2} = erlang:process_info(MyPid, links),
                0 = no_of_linked_pids(Pid, LinkedPids2),
                ok;
            Other ->
                {unexpected, Other}
        after 500 ->
            timeout
        end.

test_normal() ->
    Pid = spawn_opt(fun proc/0, []),
    erlang:link(Pid),
    Pid ! exit_normally,
    ok =
        receive
            {'EXIT', Pid, normal} ->
                ok;
            Other ->
                {unexpected, Other}
        after 500 ->
            timeout
        end.

test_trap_exit_false() ->
    Pid = spawn_opt(fun proc/0, []),
    erlang:link(Pid),
    Pid ! exit_normally,
    ok =
        receive
            Other ->
                {unexpected, Other}
        after 500 ->
            ok
        end.

proc() ->
    receive
        do_throw ->
            throw(test);
        exit_normally ->
            ok
    end.

no_of_linked_pids(Pid, L) ->
    erlang:length([X || X <- L, X =:= Pid]).
