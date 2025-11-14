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
%% @doc An implementation of the Erlang/OTP sys interface.
%%
%% This module implements a strict subset of the Erlang/OTP sys
%% interface.
%%-----------------------------------------------------------------------------

-module(sys).

-export([
    change_code/4,
    change_code/5,
    get_state/1,
    get_state/2,
    get_status/1,
    get_status/2,
    replace_state/2,
    replace_state/3,
    resume/1,
    resume/2,
    suspend/1,
    suspend/2,
    terminate/2,
    terminate/3,
    trace/2,
    trace/3
]).

-export([
    debug_options/1,
    handle_debug/4,
    handle_system_msg/6
]).

-export_type([dbg_opt/0, debug_option/0, system_event/0]).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------

-type name() :: pid() | atom() | {global, term()} | {via, module(), term()}.
-type system_event() :: any().
-type format_fun() :: fun((standard_io, Event :: any(), Extra :: system_event()) -> any()).

-opaque dbg_opt() :: {trace, true}.
-type debug_option() :: trace.

%%-----------------------------------------------------------------------------
%% Defines
%%-----------------------------------------------------------------------------

-define(DEFAULT_TIMEOUT, 5000).

%%-----------------------------------------------------------------
%% Callbacks
%%-----------------------------------------------------------------

-callback system_code_change(Misc, Module, OldVsn, Extra) -> {ok, NMisc} when
    Misc :: term(),
    OldVsn :: undefined | term(),
    Module :: atom(),
    Extra :: term(),
    NMisc :: term().

-callback system_continue(Parent, Debug, Misc) -> no_return() when
    Parent :: pid(),
    Debug :: [dbg_opt()],
    Misc :: term().

-callback system_get_state(Misc) -> {ok, State} when
    Misc :: term(), State :: term().

-callback system_replace_state(StateFun, Misc) -> {ok, NState, NMisc} when
    Misc :: term(),
    NState :: term(),
    NMisc :: term(),
    StateFun :: fun((State :: term()) -> NState).

-callback system_terminate(Reason, Parent, Debug, Misc) -> no_return() when
    Reason :: term(),
    Parent :: pid(),
    Debug :: [dbg_opt()],
    Misc :: term().

%%-----------------------------------------------------------------------------
%% Public API
%%-----------------------------------------------------------------------------

%% @equiv change_code(Name, Module, OldVsn, Extra, 5000)
-spec change_code(Name :: name(), Module :: module(), OldVsn :: undefined | any(), Extra :: any()) ->
    ok | {error, any()}.
change_code(Name, Module, OldVsn, Extra) ->
    change_code(Name, Module, OldVsn, Extra, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to tell to change code.
%% @param Module callback module that should implement `system_code_change/4'.
%% @param OldVsn old version
%% @param Extra any extra term passed from the update script
%% @param Timeout timeout for the code change
%% @return `ok' or an error tuple
%% @doc Tells the process to change code.
%% @end
%%-----------------------------------------------------------------------------
-spec change_code(
    Name :: name(),
    Module :: module(),
    OldVsn :: undefined | any(),
    Extra :: any(),
    Timeout :: timeout()
) -> ok | {error, any()}.
change_code(Name, Module, OldVsn, Extra, Timeout) ->
    {ok, Reply} = gen:call(Name, system, {change_code, Module, OldVsn, Extra}, Timeout),
    Reply.

%% @equiv get_state(Name, 5000)
-spec get_state(Name :: name()) -> any().
get_state(Name) ->
    get_state(Name, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to get the state of.
%% @param Timeout timeout for getting the state
%% @return the state or raises an error if an error occurred.
%% @doc Gets the state of the process. This function is only meant for
%% debugging.
%% @end
%%-----------------------------------------------------------------------------
-spec get_state(Name :: name(), timeout()) -> any().
get_state(Name, Timeout) ->
    {ok, Reply} = gen:call(Name, system, get_state, Timeout),
    case Reply of
        {ok, State} -> State;
        {error, Reason} -> error(Reason)
    end.

%% @equiv get_state(Name, 5000)
-spec get_status(Name :: name()) -> any().
get_status(Name) ->
    get_status(Name, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to get the status of.
%% @param Timeout timeout for getting the status
%% @return the status or raises an error if an error occurred.
%% @doc Gets the status of the process. This function is only meant for
%% debugging and returns raw state.
%% @end
%%-----------------------------------------------------------------------------
-spec get_status(Name :: name(), Timeout :: timeout()) ->
    {status, pid(), {module, module()}, [SItem :: any()]}.
get_status(Name, Timeout) ->
    {ok, Reply} = gen:call(Name, system, get_status, Timeout),
    Reply.

%% @equiv replace_state(Name, StateFun, 5000)
-spec replace_state(Name :: name(), StateFun :: fun((any()) -> any())) -> ok.
replace_state(Name, StateFun) ->
    replace_state(Name, StateFun, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to replace the state of.
%% @param Timeout timeout for replacing the state.
%% @return the new state or raises an error if an error occurred.
%% @doc Replaces the state of the process, and returns the new state. This
%% function is only meant for debugging.
%% @end
%%-----------------------------------------------------------------------------
-spec replace_state(Name :: name(), StateFun :: fun((any()) -> any()), Timeout :: timeout()) -> ok.
replace_state(Name, StateFun, Timeout) ->
    {ok, Reply} = gen:call(Name, system, {replace_state, StateFun}, Timeout),
    case Reply of
        {ok, State} -> State;
        {error, Reason} -> error(Reason)
    end.

%% @equiv resume(Name, 5000)
-spec resume(Name :: name()) -> ok.
resume(Name) ->
    resume(Name, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to resume.
%% @param Timeout timeout for resuming the process.
%% @return `ok' or raises an exception if an error occurred.
%% @doc Resume a suspended process.
%% @end
%%-----------------------------------------------------------------------------
-spec resume(Name :: name(), Timeout :: timeout()) -> ok.
resume(Name, Timeout) ->
    {ok, ok} = gen:call(Name, system, resume, Timeout),
    ok.

%% @equiv suspend(Name, 5000)
-spec suspend(Name :: name()) -> ok.
suspend(Name) ->
    suspend(Name, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to suspend.
%% @param Timeout timeout for suspending the process.
%% @return `ok' or raises an exception if an error occurred.
%% @doc Suspend the process. When a process is suspended, it only responds
%% to other system messages, but not to any other message.
%% @end
%%-----------------------------------------------------------------------------
-spec suspend(Name :: name(), Timeout :: timeout()) -> ok.
suspend(Name, Timeout) ->
    {ok, ok} = gen:call(Name, system, suspend, Timeout),
    ok.

%% @equiv terminate(Name, Reason, 5000)
-spec terminate(Name :: name(), Reason :: any()) -> ok.
terminate(Name, Reason) ->
    terminate(Name, Reason, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to terminate
%% @param Reason reason for termination
%% @param Timeout timeout for terminating
%% @return `ok' or raises an exception if an error occurred.
%% @doc Terminate a process
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(Name :: name(), Reason :: any(), Timeout :: timeout()) -> ok.
terminate(Name, Reason, Timeout) ->
    {ok, ok} = gen:call(Name, system, {terminate, Reason}, Timeout),
    ok.

%% @equiv trace(Name, Flag, 5000)
-spec trace(Name :: name(), Flag :: boolean()) -> ok.
trace(Name, Flag) ->
    trace(Name, Flag, ?DEFAULT_TIMEOUT).

%%-----------------------------------------------------------------------------
%% @param Name process to change trace of.
%% @param Flag whether to enable tracing or to disable it.
%% @param Timeout timeout for changing trace state.
%% @return `ok' or raises an exception if an error occurred.
%% @doc Enable or disable trace on a process.
%% @end
%%-----------------------------------------------------------------------------
-spec trace(Name :: name(), Flag :: boolean(), Timeout :: timeout()) -> ok.
trace(Name, Flag, Timeout) ->
    {ok, ok} = gen:call(Name, system, {debug, {trace, Flag}}, Timeout),
    ok.

%%-----------------------------------------------------------------------------
%% Process Implementation Functions
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @group Process Implementation Functions
%% @param Opt debug options.
%% @return opaque debug options to be passed to handlers.
%% @doc Initiates debug structure with a list of options. The only supported
%% option is `trace'.
%% @end
%%-----------------------------------------------------------------------------
-spec debug_options([Opt :: debug_option()]) -> [dbg_opt()].
debug_options([]) ->
    [];
debug_options([trace]) ->
    [{trace, true}].

%%-----------------------------------------------------------------------------
%% @group Process Implementation Functions
%% @param Debug debug options.
%% @param FormFunc formatting function.
%% @param Extra extra parameter passed to formatting function.
%% @param Event current system event
%% @return new debug optinons
%% @doc Call FormatFunc if tracing is enabled
%% @end
%%-----------------------------------------------------------------------------
-spec handle_debug(
    Debug :: [dbg_opt()], FormFunc :: format_fun(), Extra :: any(), Event :: system_event()
) -> [dbg_opt()].
handle_debug([{trace, true}] = DbgOpts, FormFunc, State, Event) ->
    FormFunc(standard_io, Event, State),
    DbgOpts;
handle_debug([], _FormFunc, _State, _Event) ->
    [].

%%-----------------------------------------------------------------------------
%% @group Process Implementation Functions
%% @param Opt debug options.
%% @return opaque debug options to be passed to handlers.
%% @doc This function is used by a process module to take care of system
%% messages. The process receives a `{system, From, Msg}' message and passes
%% `Msg' and `From' to this function.
%% This function is meant to be tail-called and will call either:
%% - `Module:system_continue/3'
%% - `Module:system_terminate/4'
%% @end
%%-----------------------------------------------------------------------------
-spec handle_system_msg(
    Msg :: any(),
    From :: {pid(), any()},
    Parent :: pid(),
    Module :: module(),
    Debug :: [dbg_opt()],
    Misc :: any()
) -> no_return().
handle_system_msg(Msg, From, Parent, Module, Debug, Misc) ->
    handle_system_msg(running, Msg, From, Parent, Module, Debug, Misc).

%% @private
handle_system_msg(SysState, Msg, From, Parent, Module, Debug0, Misc0) ->
    case do_handle_system_msg(SysState, Msg, Parent, Module, Debug0, Misc0) of
        {suspended, Reply, Debug1, Misc1} ->
            _ = gen:reply(From, Reply),
            suspend_loop(suspended, Parent, Module, Debug1, Misc1);
        {running, Reply, Debug1, Misc1} ->
            _ = gen:reply(From, Reply),
            Module:system_continue(Parent, Debug1, Misc1);
        {{terminating, Reason}, Reply, Debug1, Misc1} ->
            _ = gen:reply(From, Reply),
            Module:system_terminate(Reason, Parent, Debug1, Misc1)
    end.

%% @private
do_handle_system_msg(_SysState, suspend, _Parent, _Mod, Debug, Misc) ->
    {suspended, ok, Debug, Misc};
do_handle_system_msg(_SysState, resume, _Parent, _Mod, Debug, Misc) ->
    {running, ok, Debug, Misc};
do_handle_system_msg(SysState, get_state, _Parent, Mod, Debug, Misc) ->
    Result = do_get_state(Mod, Misc),
    {SysState, Result, Debug, Misc};
do_handle_system_msg(SysState, {replace_state, StateFun}, _Parent, Mod, Debug, Misc0) ->
    {Result, Misc1} = do_replace_state(StateFun, Mod, Misc0),
    {SysState, Result, Debug, Misc1};
do_handle_system_msg(SysState, get_status, Parent, Mod, Debug, Misc) ->
    Res = do_get_status(SysState, Parent, Mod, Debug, Misc),
    {SysState, Res, Debug, Misc};
do_handle_system_msg(SysState, {debug, DebugOpt}, _Parent, _Mod, Debug0, Misc) ->
    {Result, Debug1} = do_debug(DebugOpt, Debug0),
    {SysState, Result, Debug1, Misc};
do_handle_system_msg(_, {terminate, Reason}, _Parent, _Mod, Debug, Misc) ->
    {{terminating, Reason}, ok, Debug, Misc};
do_handle_system_msg(
    suspended,
    {change_code, Module, Vsn, Extra},
    _Parent,
    Mod,
    Debug,
    Misc0
) ->
    {Result, Misc1} = do_change_code(Mod, Module, Vsn, Extra, Misc0),
    {suspended, Result, Debug, Misc1};
do_handle_system_msg(SysState, Other, _Parent, _Mod, Debug, Misc) ->
    {SysState, {error, {unknown_system_msg, Other}}, Debug, Misc}.

%% @private
suspend_loop(SysState, Parent, Mod, Debug, Misc) ->
    receive
        {system, From, Msg} ->
            handle_system_msg(SysState, Msg, From, Parent, Mod, Debug, Misc);
        {'EXIT', Parent, Reason} ->
            Mod:system_terminate(Reason, Parent, Debug, Misc)
    end.

%% @private
do_get_state(Mod, Misc) ->
    case erlang:function_exported(Mod, system_get_state, 1) of
        true ->
            try Mod:system_get_state(Misc) of
                {ok, _} = Result -> Result;
                Other -> {error, {callback_failed, {Mod, system_get_state}, {bad_return, Other}}}
            catch
                Class:Exc ->
                    {error, {callback_failed, {Mod, system_get_state}, {Class, Exc}}}
            end;
        false ->
            {ok, Misc}
    end.

%% @private
do_replace_state(StateFun, Mod, Misc0) ->
    case erlang:function_exported(Mod, system_replace_state, 1) of
        true ->
            try Mod:system_replace_state(StateFun, Misc0) of
                {ok, State, Misc1} ->
                    {{ok, State}, Misc1};
                Other ->
                    {
                        {error,
                            {callback_failed, {Mod, system_replace_state}, {bad_return, Other}}},
                        Misc0
                    }
            catch
                Class:Exc ->
                    {{error, {callback_failed, {Mod, system_replace_state}, {Class, Exc}}}, Misc0}
            end;
        false ->
            try
                Misc1 = StateFun(Misc0),
                {{ok, Misc1}, Misc1}
            catch
                Class:Exc ->
                    {{error, {callback_failed, StateFun, {Class, Exc}}}, Misc0}
            end
    end.

%% @private
do_get_status(SysState, Parent, Mod, Debug, Misc) ->
    ProcessDictionary = get(),
    {status, self(), {module, Mod}, [ProcessDictionary, SysState, Parent, Debug, Misc]}.

%% @private
do_debug({trace, true} = Tuple, Debug0) ->
    Debug1 = lists:keystore(trace, 1, Debug0, Tuple),
    {ok, Debug1};
do_debug({trace, false}, Debug0) ->
    Debug1 = lists:keydelete(trace, 1, Debug0),
    {ok, Debug1};
do_debug(_Other, Debug0) ->
    {unknown_debug, Debug0}.

do_change_code(Mod, Module, Vsn, Extra, Misc0) ->
    case catch Mod:system_code_change(Misc0, Module, Vsn, Extra) of
        {ok, Misc1} -> {ok, Misc1};
        Other -> {{error, Other}, Misc0}
    end.
