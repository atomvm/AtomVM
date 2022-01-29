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

-module(test_supervisor).

-export([test/0, init/1, start_link/1]).

test() ->
    {ok, _SupPid} = start_link(self()),
    Pid1 =
        receive
            {ping_pong_server_ready, Pid} ->
                Pid
        after 2000 -> throw(timeout)
        end,
    pong = gen_server:call(Pid1, ping),
    gen_server:cast(Pid1, {crash, test}),
    RestartedPid =
        receive
            {ping_pong_server_ready, Pid2} ->
                Pid2
        after 2000 -> throw(timeout)
        end,
    pong = gen_server:call(RestartedPid, ping),
    false = erlang:is_process_alive(Pid1),
    true = erlang:is_process_alive(RestartedPid),
    ok = gen_server:call(RestartedPid, exit),
    no_restart =
        receive
            {ping_pong_server_ready, Pid3} ->
                Pid3
        after 100 -> no_restart
        end,
    ok.

start_link(Parent) ->
    supervisor:start_link({local, testsup}, ?MODULE, [Parent]).

init([Parent]) ->
    ChildSpecs = [
        {test_child, {ping_pong_server, start_link, [Parent]}, transient, brutal_kill, worker, [
            ping_pong_server
        ]}
    ],
    {ok, {{one_for_one, 10000, 3600}, ChildSpecs}}.
