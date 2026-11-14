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

-export([test/0, f/1, external_child/0, external_error/0]).

-include("etest.hrl").

test() ->
    ok = test_spawn(),
    ok = test_spawn_link(),
    ok = test_external_funs(),
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

test_external_funs() ->
    Previous = process_flag(trap_exit, true),
    try
        [
            begin
                {Pid, Ref} = spawn_opt(fun ?MODULE:external_child/0, Options),
                Pid ! {self(), <<1, 2, 3>>},
                receive
                    {Pid, <<1, 2, 3>>} -> ok
                after 1000 -> error(no_reply)
                end,
                normal = child_down(Pid, Ref),
                case lists:member(link, Options) of
                    true ->
                        receive
                            {'EXIT', Pid, normal} -> ok
                        after 1000 -> error(no_exit)
                        end;
                    false ->
                        ok
                end
            end
         || Options <- [[monitor], [monitor, link], [monitor, {min_heap_size, 100}]]
        ],
        {BifPid, BifRef} = spawn_opt(fun erlang:self/0, [monitor]),
        normal = child_down(BifPid, BifRef),
        {ErrorPid, ErrorRef} = spawn_opt(fun ?MODULE:external_error/0, [monitor]),
        {child_error, ErrorStack} = child_down(ErrorPid, ErrorRef),
        true = is_list(ErrorStack),
        [
            begin
                {MissingPid, MissingRef} = spawn_opt(Fun, [monitor]),
                {undef, Stack} = child_down(MissingPid, MissingRef),
                true = is_list(Stack)
            end
         || Fun <- [fun ?MODULE:missing/0, fun atomvm_missing_spawn_module:missing/0]
        ],
        Captured = {boxed, self(), list_to_binary([1, 2, 3])},
        [
            begin
                {BadPid, BadRef} = spawn_opt(Fun, [monitor]),
                {{badarity, {Fun, []}}, Stack} = child_down(BadPid, BadRef),
                true = is_list(Stack)
            end
         || Fun <- [fun(X) -> {Captured, X} end, fun ?MODULE:f/1, fun erlang:abs/1]
        ],
        ok
    after
        process_flag(trap_exit, Previous)
    end.

child_down(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 1000 ->
        error(no_down)
    end.

external_child() ->
    receive
        {Parent, Value} -> Parent ! {self(), Value}
    end.

external_error() -> error(child_error).

f(Parent) ->
    Parent ! {self(), erlang:process_info(self(), links)}.
