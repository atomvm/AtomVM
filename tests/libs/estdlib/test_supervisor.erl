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

-export([start/0, test/0, init/1, start_link/1, child_start/1]).

start() ->
    ok = test().

test() ->
    ok = test_basic_supervisor(),
    ok = test_map_supervisor(),
    ok = test_start_child(),
    ok = test_start_child_ping_pong(),
    ok = test_supervisor_order(),
    ok = test_terminate_delete_child(),
    ok = test_terminate_timeout(),
    ok.

test_basic_supervisor() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_basic_supervisor, self()}),
    ok = test_ping_pong(SupPid).

test_map_supervisor() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_map_supervisor, self()}),
    ok = test_ping_pong(SupPid).

test_start_child_ping_pong() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    supervisor:start_child(SupPid, #{
        id => test_child,
        start => {ping_pong_server, start_link, [self()]},
        restart => transient,
        shutdown => brutal_kill,
        type => worker,
        modules => [
            ping_pong_server
        ]
    }),
    ok = test_ping_pong(SupPid).

test_start_child() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    {ok, undefined} = supervisor:start_child(SupPid, #{
        id => child_ignore, start => {?MODULE, child_start, [ignore]}
    }),
    {error, already_present} = supervisor:start_child(SupPid, #{
        id => child_ignore, start => {?MODULE, child_start, [ignore]}
    }),
    {ok, Pid} = supervisor:start_child(SupPid, #{
        id => child_start, start => {?MODULE, child_start, [start]}
    }),
    {error, {already_started, Pid}} = supervisor:start_child(SupPid, #{
        id => child_start, start => {?MODULE, child_start, [start]}
    }),
    {error, {child_error, _ChildRecord1}} = supervisor:start_child(SupPid, #{
        id => child_error, start => {?MODULE, child_start, [error]}
    }),
    {error, {fail, _ChildRecord2}} = supervisor:start_child(SupPid, #{
        id => child_error, start => {?MODULE, child_start, [fail]}
    }),
    {error, {{'EXIT', _Error}, _ChildRecord3}} = supervisor:start_child(SupPid, #{
        id => child_error, start => {?MODULE, child_start, [raise]}
    }),
    {ok, _InfoPid, info} = supervisor:start_child(SupPid, #{
        id => child_info, start => {?MODULE, child_start, [info]}
    }),
    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.

test_terminate_delete_child() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    {ok, Pid} = supervisor:start_child(SupPid, #{
        id => child_start, start => {?MODULE, child_start, [start]}
    }),
    {error, not_found} = supervisor:terminate_child(SupPid, Pid),
    {error, running} = supervisor:delete_child(SupPid, child_start),
    ok = supervisor:terminate_child(SupPid, child_start),
    ok = supervisor:delete_child(SupPid, child_start),
    {error, not_found} = supervisor:delete_child(SupPid, child_start),
    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.

test_terminate_timeout() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    Self = self(),
    {ok, Pid} = supervisor:start_child(SupPid, #{
        id => child_start, start => {?MODULE, child_start, [{trap_exit, Self}]}, shutdown => 500
    }),
    ok = supervisor:terminate_child(SupPid, child_start),
    ok =
        receive
            {Pid, {SupPid, shutdown}} -> ok
        after 1000 -> timeout
        end,
    {ok, Pid2} = supervisor:restart_child(SupPid, child_start),
    Pid2 ! ok,
    ok = supervisor:terminate_child(SupPid, child_start),
    ok =
        receive
            {Pid2, {SupPid, shutdown}} -> ok
        after 1000 -> timeout
        end,
    ok = supervisor:delete_child(SupPid, child_start),
    {ok, Pid3} = supervisor:start_child(SupPid, #{
        id => child_start, start => {?MODULE, child_start, [{trap_exit, Self}]}, shutdown => 500
    }),
    unlink(SupPid),
    exit(SupPid, shutdown),
    ok =
        receive
            {Pid3, {SupPid, shutdown}} -> ok
        after 1000 -> timeout
        end,
    ok.

child_start(ignore) ->
    ignore;
child_start(start) ->
    Pid = spawn_link(fun() ->
        receive
            ok -> ok
        end
    end),
    {ok, Pid};
child_start(info) ->
    Pid = spawn_link(fun() ->
        receive
            ok -> ok
        end
    end),
    {ok, Pid, info};
child_start(error) ->
    {error, child_error};
child_start(fail) ->
    fail;
child_start({trap_exit, Parent}) ->
    Pid = spawn_link(fun() ->
        process_flag(trap_exit, true),
        receive
            {'EXIT', From, Reason} -> Parent ! {self(), {From, Reason}}
        end,
        receive
            ok -> ok
        end
    end),
    {ok, Pid}.

test_ping_pong(SupPid) ->
    Pid1 = get_and_test_server(),
    gen_server:cast(Pid1, {crash, test}),
    Pid2 = get_and_test_server(),
    MonitorRef1 = erlang:monitor(process, Pid2),
    ok = gen_server:call(Pid2, {stop, abnormal}),
    receive
        {'DOWN', MonitorRef1, process, Pid2, abnormal} -> ok
    end,
    Pid3 = get_and_test_server(),
    MonitorRef2 = erlang:monitor(process, Pid3),
    ok = gen_server:call(Pid3, {stop, normal}),
    receive
        {'DOWN', MonitorRef2, process, Pid3, normal} -> ok
    end,
    no_restart =
        receive
            {ping_pong_server_ready, Pid3} ->
                Pid3
        after 100 -> no_restart
        end,
    unlink(SupPid),
    exit(SupPid, shutdown),
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
    supervisor:start_link(?MODULE, {test_basic_supervisor, Parent}).

init({test_basic_supervisor, Parent}) ->
    ChildSpecs = [
        {test_child, {ping_pong_server, start_link, [Parent]}, transient, brutal_kill, worker, [
            ping_pong_server
        ]}
    ],
    {ok, {{one_for_one, 10000, 3600}, ChildSpecs}};
init({test_map_supervisor, Parent}) ->
    ChildSpecs = [
        #{
            id => test_child,
            start => {ping_pong_server, start_link, [Parent]},
            restart => transient,
            shutdown => brutal_kill,
            type => worker,
            modules => [
                ping_pong_server
            ]
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10000, period => 3600}, ChildSpecs}};
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
    {ok, {{one_for_one, 10000, 3600}, ChildSpecs}};
init({test_no_child, _Parent}) ->
    {ok, {#{strategy => one_for_one, intensity => 10000, period => 3600}, []}}.

test_supervisor_order() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_supervisor_order, self()}),
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
    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.
