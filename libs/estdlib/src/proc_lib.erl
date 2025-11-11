%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP proc_lib interface.
%%
%% This module implements a strict subset of the Erlang/OTP proc_lib
%% interface.
%%
%% When spawning a single function, calling `spawn/1' or `spawn_link/1',
%% Erlang/OTP proc_lib sets `initial_call' to a tuple with a unique
%% atom determining where the function was created, for example
%% ``{some_module, '-work/3-fun-0-', 0}''. AtomVM literaly calls `erlang:apply/2'
%% and therefore in these cases, `initial_call` is `{erlang, apply, 2}'.
%%-----------------------------------------------------------------------------

-module(proc_lib).

%% Public API
-export([
    spawn/1,
    spawn/3,
    spawn_link/1,
    spawn_link/3,
    start/3,
    start/4,
    start/5,
    start_link/3,
    start_link/4,
    start_link/5,
    start_monitor/3,
    start_monitor/4,
    start_monitor/5,
    init_ack/1,
    init_ack/2,

    initial_call/1,
    translate_initial_call/1
]).

-export([
    init_p/5
]).

-export_type([
    start_spawn_option/0
]).

-compile({no_auto_import, [spawn/3, spawn_link/3]}).

-include_lib("kernel/include/logger.hrl").

%% @doc Restricted set of spawn options. `monitor' is not supported.
-type start_spawn_option() ::
    {min_heap_size, pos_integer()}
    | {max_heap_size, pos_integer()}
    | {atomvm_heap_growth, erlang:atomvm_heap_growth_strategy()}
    | link.

%% @equiv spawn(erlang, apply, [Fun, []])
-spec spawn(fun(() -> any())) -> pid().
spawn(Fun) ->
    spawn(erlang, apply, [Fun, []]).

%%-----------------------------------------------------------------------------
%% @param   Module of the function to call
%% @param   Function to call
%% @param   Args arguments to pass to the function
%% @doc     Spawn a new process and initialize it.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn(module(), atom(), [any()]) -> pid().
spawn(Module, Function, Args) ->
    Parent = self(),
    Ancestors = get_ancestors(),
    erlang:spawn(?MODULE, init_p, [Parent, Ancestors, Module, Function, Args]).

%% @equiv spawn_link(erlang, apply, [Fun, []])
-spec spawn_link(fun(() -> any())) -> pid().
spawn_link(Fun) ->
    spawn_link(erlang, apply, [Fun, []]).

%%-----------------------------------------------------------------------------
%% @param   Module of the function to call
%% @param   Function to call
%% @param   Args arguments to pass to the function
%% @doc     Spawn and atomically link a new process and initialize it.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn_link(module(), atom(), [any()]) -> pid().
spawn_link(Module, Function, Args) ->
    Parent = self(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(?MODULE, init_p, [Parent, Ancestors, Module, Function, Args]).

%% @equiv start(Module, Function, Args, infinity)
-spec start(module(), atom(), [any()]) -> any().
start(Module, Function, Args) ->
    start(Module, Function, Args, infinity).

%% @equiv start(Module, Function, Args, Timeout, [])
-spec start(module(), atom(), [any()], timeout()) -> any().
start(Module, Function, Args, Timeout) ->
    start(Module, Function, Args, Timeout, []).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the callbacks are defined
%% @param   Function to call for initialization
%% @param   Args arguments to pass to the function
%% @param   Timeout timeout for the initialization to be done
%% @param   SpawnOpts options passed to spawn. `monitor' is not allowed.
%% @doc     Start a new process synchronously. Wait for the process to call
%%          `init_ack/1,2' or `init_fail/2,3'.
%% @end
%%-----------------------------------------------------------------------------
-spec start(module(), atom(), [any()], timeout(), [start_spawn_option()]) -> any().
start(Module, Function, Args, Timeout, SpawnOpts) ->
    start0(Module, Function, Args, Timeout, SpawnOpts, false, false).

%% @equiv start_link(Module, Function, Args, infinity)
-spec start_link(module(), atom(), [any()]) -> any().
start_link(Module, Function, Args) ->
    start_link(Module, Function, Args, infinity).

%% @equiv start_link(Module, Function, Args, Timeout, [])
-spec start_link(module(), atom(), [any()], timeout()) -> any().
start_link(Module, Function, Args, Timeout) ->
    start_link(Module, Function, Args, Timeout, []).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the callbacks are defined
%% @param   Function to call for initialization
%% @param   Args arguments to pass to the function
%% @param   Timeout timeout for the initialization to be done
%% @param   SpawnOpts options passed to spawn_link. `monitor' is not allowed.
%% @doc     Start a new process synchronously and atomically link it.
%%          Wait for the process to call `init_ack/1,2' or `init_fail/2,3'.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(module(), atom(), [any()], timeout(), [start_spawn_option()]) -> any().
start_link(Module, Function, Args, Timeout, SpawnOpts) ->
    start0(Module, Function, Args, Timeout, [link | SpawnOpts], true, false).

%% @equiv start_monitor(Module, Function, Args, infinity)
-spec start_monitor(module(), atom(), [any()]) -> any().
start_monitor(Module, Function, Args) ->
    start_monitor(Module, Function, Args, infinity).

%% @equiv start_monitor(Module, Function, Args, Timeout, [])
-spec start_monitor(module(), atom(), [any()], timeout()) -> any().
start_monitor(Module, Function, Args, Timeout) ->
    start_monitor(Module, Function, Args, Timeout, []).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the callbacks are defined
%% @param   Function to call for initialization
%% @param   Args arguments to pass to the function
%% @param   Timeout timeout for the initialization to be done
%% @param   SpawnOpts options passed to spawn_link. `monitor' is not allowed.
%% @doc     Start a new process synchronously and atomically link it.
%%          Wait for the process to call `init_ack/1,2' or `init_fail/2,3'.
%% @end
%%-----------------------------------------------------------------------------
-spec start_monitor(module(), atom(), [any()], timeout(), [start_spawn_option()]) -> any().
start_monitor(Module, Function, Args, Timeout, SpawnOpts) ->
    start0(Module, Function, Args, Timeout, SpawnOpts, true, true).

%% @private
start0(Module, Function, Args, Timeout, SpawnOpts, Link, Monitor) ->
    case lists:member(monitor, SpawnOpts) of
        true -> error(badarg);
        false -> ok
    end,
    Parent = self(),
    Ancestors = get_ancestors(),
    {Pid, MonitorRef} = spawn_opt(?MODULE, init_p, [Parent, Ancestors, Module, Function, Args], [
        monitor | SpawnOpts
    ]),
    receive
        {ack, Pid, Result} when Monitor ->
            {Result, MonitorRef};
        {ack, Pid, Result} ->
            erlang:demonitor(MonitorRef, [flush]),
            Result;
        {'DOWN', MonitorRef, process, Pid, Reason} when Link ->
            receive
                {'EXIT', Pid, _} -> ok
            after 0 -> ok
            end,
            receive
                {'DOWN', MonitorRef, process, Pid, _} -> ok
            end,
            {error, Reason};
        {'DOWN', MonitorRef, process, Pid, Reason} when Monitor ->
            {{error, Reason}, MonitorRef};
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            {error, Reason}
    after Timeout ->
        if
            Link ->
                unlink(Pid),
                exit(Pid, kill),
                receive
                    {'EXIT', Pid, _} -> ok
                after 0 -> ok
                end;
            true ->
                exit(Pid, kill)
        end,
        receive
            {'DOWN', MonitorRef, process, Pid, _} -> ok
        end,
        case Monitor of
            true ->
                {{error, timeout}, MonitorRef};
            false ->
                {error, timeout}
        end
    end.

%% @private
get_ancestors() ->
    case get('$ancestors') of
        A when is_list(A) -> A;
        _ -> []
    end.

%%-----------------------------------------------------------------------------
%% @param   Result result sent back to parent
%% @doc     Callback to signal that initialization succeeded.
%% @end
%%-----------------------------------------------------------------------------
-spec init_ack(any()) -> ok.
init_ack(Result) ->
    [Parent | _] = get('$ancestors'),
    init_ack(Parent, Result).

%%-----------------------------------------------------------------------------
%% @param   Parent parent process
%% @param   Result result sent back to parent
%% @doc     Callback to signal that initialization succeeded.
%% @end
%%-----------------------------------------------------------------------------
-spec init_ack(pid(), any()) -> ok.
init_ack(Parent, Result) ->
    Parent ! {ack, self(), Result},
    ok.

%% @private
-spec init_p(pid(), [pid()], atom(), atom(), [any()]) -> any().
init_p(Parent, Ancestors, Module, Function, Args) ->
    put('$ancestors', [Parent | Ancestors]),
    put('$initial_call', {Module, Function, length(Args)}),
    try
        apply(Module, Function, Args)
    catch
        Class:Reason:Stacktrace ->
            exit_p(Class, Reason, Stacktrace)
    end.

%% @private
exit_p(Class, Reason, Stacktrace) ->
    MFA = get('$initial_call'),
    crash_report(Class, Reason, MFA, Stacktrace),
    exit_raise(Class, Reason, Stacktrace).

%% @private
exit_raise(error, Reason, Stacktrace) ->
    erlang:raise(exit, {Reason, Stacktrace}, Stacktrace);
exit_raise(exit, Reason, Stacktrace) ->
    erlang:raise(exit, Reason, Stacktrace);
exit_raise(throw, Reason, Stacktrace) ->
    erlang:raise(exit, {{nocatch, Reason}, Stacktrace}, Stacktrace).

%% @private
crash_report(exit, normal, _, _) ->
    ok;
crash_report(exit, shutdown, _, _) ->
    ok;
crash_report(exit, {shutdown, _}, _, _) ->
    ok;
crash_report(Class, Reason, MFA, Stacktrace) ->
    {ICM, ICF, ICA} =
        case MFA of
            {M, F, A} when is_atom(M), is_atom(F), is_integer(A) -> {M, F, A};
            _ -> {proc_lib, init_p, 5}
        end,
    {message_queue_len, MessageQueueLen} = process_info(self(), message_queue_len),
    {links, Links} = process_info(self(), links),
    {total_heap_size, TotalHeapSize} = process_info(self(), total_heap_size),
    {stack_size, StackSize} = process_info(self(), stack_size),
    ?LOG_ERROR(
        "=CRASH REPORT====\n"
        "  crasher:\n"
        "   initial call: ~s:~s/~B\n"
        "   pid: ~p\n"
        "   exception error: ~p\n"
        "   ancestors: ~w\n"
        "   message_queue_len: ~B\n"
        "   links: ~w\n"
        "   total_heap_size: ~B\n"
        "   stack_size: ~B\n"
        "   stack_trace: ~p\n",
        [
            ICM,
            ICF,
            ICA,
            self(),
            {Class, Reason},
            get_ancestors(),
            MessageQueueLen,
            Links,
            TotalHeapSize,
            StackSize,
            Stacktrace
        ]
    ).

%%-----------------------------------------------------------------------------
%% @param   Process process to get the initial call for
%% @return  `false' until we support `process_info(Pid, dictionary)'
%% @doc     Get the initial call for a given process or `false'
%%          if it's not available. Arguments are replaced with atoms.
%% @end
%%-----------------------------------------------------------------------------
-spec initial_call(pid()) -> {module(), atom(), [atom()]} | false.
initial_call(_Process) ->
    false.

%%-----------------------------------------------------------------------------
%% @param   Process process to get the initial call for
%% @return  `{proc_lib, init_p, 5}' until we support `process_info(Pid, dictionary)'
%% @doc     Get the initial call for a given process or `{proc_lib, init_p, 5}'
%%          if it's not available.
%% @end
%%-----------------------------------------------------------------------------
-spec translate_initial_call(pid()) -> {module(), atom(), non_neg_integer()}.
translate_initial_call(_Process) ->
    {proc_lib, init_p, 5}.
