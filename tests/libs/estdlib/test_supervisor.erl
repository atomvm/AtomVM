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
    ok = test_which_children(),
    ok = test_count_children(),
    ok = test_one_for_all(),
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

test_count_children() ->
    % Test with no children - all counts should be zero
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    [{specs, 0}, {active, 0}, {supervisors, 0}, {workers, 0}] = supervisor:count_children(SupPid),

    % Add a worker child and verify counts
    {ok, ChildPid} = supervisor:start_child(SupPid, #{
        id => test_worker,
        start => {ping_pong_server, start_link, [self()]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }),

    % Receive message sent back so it won't be left for another test to receive erroneously.
    ChildPid = get_and_test_server(),

    % Check count_children with one active worker
    [{specs, 1}, {active, 1}, {supervisors, 0}, {workers, 1}] = supervisor:count_children(SupPid),

    % Add a supervisor child with no children and verify counts
    {ok, _SupervisorPid} = supervisor:start_child(SupPid, #{
        id => test_supervisor,
        start => {supervisor, start_link, [?MODULE, {test_no_child, self()}]},
        restart => permanent,
        shutdown => infinity,
        type => supervisor
    }),

    % Check count_children with one worker and one supervisor
    [{specs, 2}, {active, 2}, {supervisors, 1}, {workers, 1}] = supervisor:count_children(SupPid),

    % Terminate the worker child - spec remains but child becomes inactive
    ok = supervisor:terminate_child(SupPid, test_worker),
    [{specs, 2}, {active, 1}, {supervisors, 1}, {workers, 1}] = supervisor:count_children(SupPid),

    % Delete the worker child - removes the spec completely
    ok = supervisor:delete_child(SupPid, test_worker),
    [{specs, 1}, {active, 1}, {supervisors, 1}, {workers, 0}] = supervisor:count_children(SupPid),

    % Terminate the supervisor child - spec remains but child becomes inactive
    ok = supervisor:terminate_child(SupPid, test_supervisor),
    [{specs, 1}, {active, 0}, {supervisors, 1}, {workers, 0}] = supervisor:count_children(SupPid),

    % Delete the supervisor child - removes the spec completely
    ok = supervisor:delete_child(SupPid, test_supervisor),
    [{specs, 0}, {active, 0}, {supervisors, 0}, {workers, 0}] = supervisor:count_children(SupPid),

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
            {ping_pong_server_ready, Pid4} ->
                Pid4
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
    {ok, {#{strategy => one_for_one, intensity => 10000, period => 3600}, []}};
init({test_one_for_all, Parent}) ->
    ChildSpecs = [
        #{
            id => ping_pong_1,
            start => {ping_pong_server, start_link, [Parent]},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker,
            modules => [ping_pong_server]
        },
        #{
            id => ping_pong_2,
            start => {ping_pong_server, start_link, [Parent]},
            restart => transient,
            shutdown => brutal_kill,
            type => worker,
            modules => [ping_pong_server]
        },
        #{
            id => ready_0,
            start => {notify_init_server, start_link, [{Parent, ready_0}]},
            restart => temporary,
            shutdown => brutal_kill,
            type => worker,
            modules => [notify_init_server]
        }
    ],
    {ok, {#{strategy => one_for_all, intensity => 10000, period => 3600}, ChildSpecs}}.

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

test_which_children() ->
    % Test with no children
    {ok, SupPid} = supervisor:start_link(?MODULE, {test_no_child, self()}),
    [] = supervisor:which_children(SupPid),

    % Add a child and test
    {ok, ChildPid} = supervisor:start_child(SupPid, #{
        id => test_child,
        start => {ping_pong_server, start_link, [self()]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }),

    % Receive message sent back so it won't be left for another test to receive erroneously.
    ChildPid = get_and_test_server(),

    % Check which_children returns the child info
    [{test_child, ChildPid, worker, [ping_pong_server]}] = supervisor:which_children(SupPid),
    true = is_pid(ChildPid),

    % Terminate the child and check it shows as undefined
    ok = supervisor:terminate_child(SupPid, test_child),
    [{test_child, undefined, worker, [ping_pong_server]}] = supervisor:which_children(SupPid),

    % Delete the child and check empty list
    ok = supervisor:delete_child(SupPid, test_child),
    [] = supervisor:which_children(SupPid),

    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.

test_one_for_all() ->
    {ok, SupPid} = supervisor:start_link({local, allforone}, ?MODULE, {test_one_for_all, self()}),
    % Collect startup message from permanent ping_pong_server
    Server_1 = get_and_test_server(),
    % Collect startup message from transient ping_pong_server
    Server_2 = get_and_test_server(),
    % Collect startup message from temporary notify_init_server
    ready_0 =
        receive
            Msg1 -> Msg1
        after 1000 -> error({timeout, {start, ready_0}})
        end,

    [{specs, 3}, {active, 3}, {supervisors, 0}, {workers, 3}] = supervisor:count_children(SupPid),

    %% Monitor transient Server_2 to make sure it is stopped, and restarted when
    %% permanent Server_1 is shutdown.
    MonRef = monitor(process, Server_2),
    ok = gen_server:call(Server_1, {stop, test_crash}),
    %% Server_2 should exit before the first child is restarted, but exit messages from
    %% monitored processes may take some time to be received so we may get the message
    %% from the first restarted child first.
    First =
        receive
            {'DOWN', MonRef, process, Server_2, killed} ->
                down;
            {ping_pong_server_ready, Restart1} when is_pid(Restart1) ->
                ready
        after 1000 ->
            error({timeout, restart_after_crash})
        end,
    ok =
        case First of
            down ->
                receive
                    {ping_pong_server_ready, Restart_1} when is_pid(Restart_1) -> ok
                after 1000 ->
                    error({timeout, restart_after_crash})
                end;
            ready ->
                receive
                    {'DOWN', MonRef, process, Server_2, killed} -> ok
                after 1000 ->
                    error({timeout, restart_after_crash})
                end
        end,

    demonitor(MonRef),

    % Collect startup message from restarted transient ping_pong_server child
    _Restart_2 = get_and_test_server(),
    % Make sure temporary notify_init_server is not restarted
    no_start =
        receive
            ready_0 -> error({error, restarted_temporary})
        after 1000 -> no_start
        end,

    % Ensure correct number of children
    [{specs, 2}, {active, 2}, {supervisors, 0}, {workers, 2}] = supervisor:count_children(SupPid),

    unlink(SupPid),
    exit(SupPid, shutdown),
    ok.
