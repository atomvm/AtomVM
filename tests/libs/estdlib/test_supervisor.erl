%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(test_supervisor).

-export([test/0, init/1, start_link/1]).

test() ->
    ok = test_basic_supervisor(),
    ok = test_supervisor_order(),
    ok.

test_basic_supervisor() ->
    {ok, _SupPid} = start_link(self()),
    Pid1 = get_and_test_server(),
    gen_server:cast(Pid1, {crash, test}),
    Pid2 = get_and_test_server(),
    %   MonitorRef1 = erlang:monitor(process, Pid2),
    ok = gen_server:call(Pid2, {stop, abnormal}),
    %   receive {'DOWN', MonitorRef1, process, Pid2, abnormal} -> ok end,
    Pid3 = get_and_test_server(),
    %   MonitorRef2 = erlang:monitor(process, Pid3),
    ok = gen_server:call(Pid3, {stop, normal}),
    %   receive {'DOWN', MonitorRef2, process, Pid3, normal} -> ok end,
    no_restart =
        receive
            {ping_pong_server_ready, Pid3} ->
                Pid3
        after 100 -> no_restart
        end,
    ok.

get_and_test_server() ->
    Pid =
        receive
            {ping_pong_server_ready, ReadyServer} ->
                ReadyServer
        after 2000 -> throw(timeout)
        end,
    pong = gen_server:call(Pid, ping),
    Pid.

start_link(Parent) ->
    supervisor:start_link({local, testsup}, ?MODULE, {test_basic_supervisor, Parent}).

init({test_basic_supervisor, Parent}) ->
    ChildSpecs = [
        {test_child, {ping_pong_server, start_link, [Parent]}, transient, brutal_kill, worker, [
            ping_pong_server
        ]}
    ],
    {ok, {{one_for_one, 10000, 3600}, ChildSpecs}};
init({test_supervisor_order, Parent}) ->
    ChildSpecs = [
        {ready_1, {notify_init_server, start_link, [{Parent, ready_1}]}, transient, brutal_kill,
            worker, [
                notify_init_server
            ]},
        {ready_2, {notify_init_server, start_link, [{Parent, ready_2}]}, transient, brutal_kill,
            worker, [
                notify_init_server
            ]}
    ],
    {ok, {{one_for_one, 10000, 3600}, ChildSpecs}}.

test_supervisor_order() ->
    supervisor:start_link(?MODULE, {test_supervisor_order, self()}),
    ready_1 =
        receive
            Msg1 ->
                Msg1
        after 1000 ->
            {error, {timeout, ready_1}}
        end,
    ready_2 =
        receive
            Msg2 ->
                Msg2
        after 1000 ->
            {error, {timeout, ready_2}}
        end,
    ok.
