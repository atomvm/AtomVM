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
%% This module implements common code in gen_* modules, following what
%% Erlang/OTP does with gen module. However, none of the functions exported
%% here are public interface.
%%-----------------------------------------------------------------------------

-module(gen).
-moduledoc false.

-export([
    start/5,
    start/6,
    call/4,
    cast/2,
    reply/2
]).

-type server_ref() :: atom() | pid().
-type from() :: {pid(), reference()}.
-type linkage() :: link | nolink | monitor.
-type emgr_name() :: {local, atom()}.

%%-----------------------------------------------------------------------------
%% @doc     Start a `gen_*' process without a registered name. This is the
%% OTP-private entry point Elixir's `GenServer.start_link/start' calls
%% (`:gen.start(:gen_server, Link, Module, Args, Options)'). It bridges to the
%% AtomVM `gen_*' module's `init_it' protocol via {@link proc_lib}.
%% @end
%%-----------------------------------------------------------------------------
-spec start(
    GenMod :: module(),
    LinkP :: linkage(),
    Module :: module(),
    Args :: term(),
    Options :: list()
) -> {ok, pid()} | {ok, {pid(), reference()}} | ignore | {error, term()}.
start(GenMod, LinkP, Module, Args, Options) ->
    do_start(GenMod, LinkP, [self(), Module, Args, Options], Options).

%%-----------------------------------------------------------------------------
%% @doc     Start a `gen_*' process with a registered name, as called by Elixir
%% (`:gen.start(:gen_server, Link, {local, Name}, Module, Args, Options)').
%%
%%          Only `{local, Name}' is supported.
%% @end
%%-----------------------------------------------------------------------------
-spec start(
    GenMod :: module(),
    LinkP :: linkage(),
    Name :: emgr_name(),
    Module :: module(),
    Args :: term(),
    Options :: list()
) -> {ok, pid()} | {ok, {pid(), reference()}} | ignore | {error, term()}.
start(GenMod, LinkP, {local, Name}, Module, Args, Options) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            do_start(
                GenMod, LinkP, [self(), Name, Module, Args, [{name, Name} | Options]], Options
            );
        Pid ->
            {error, {already_started, Pid}}
    end.

%% @private
do_start(GenMod, LinkP, InitArgs, Options) ->
    Timeout = proplists:get_value(timeout, Options, infinity),
    SpawnOpts = proplists:get_value(spawn_opt, Options, []),
    do_start(GenMod, LinkP, InitArgs, Timeout, SpawnOpts).

do_start(GenMod, link, InitArgs, Timeout, SpawnOpts) ->
    proc_lib:start_link(GenMod, init_it, InitArgs, Timeout, SpawnOpts);
do_start(GenMod, nolink, InitArgs, Timeout, SpawnOpts) ->
    proc_lib:start(GenMod, init_it, InitArgs, Timeout, SpawnOpts);
do_start(GenMod, monitor, InitArgs, Timeout, SpawnOpts) ->
    case proc_lib:start_monitor(GenMod, init_it, InitArgs, Timeout, SpawnOpts) of
        {{ok, Pid}, Mon} -> {ok, {Pid, Mon}};
        {Error, _Mon} -> Error
    end.

%%-----------------------------------------------------------------------------
%% @doc     Perform a call on a gen server. This API not documented by OTP,
%% yet Elixir uses it, so this function matches the current OTP-28 behavior.
%% @end
%% @param ServerRef the server to call
%% @param Label the label for the message, typically `system' or `$gen_call'
%% @param Request the message to send
%% @param Timeout timeout for sending the message
%% @returns `{ok, Result}' or raises an exit exception
%%-----------------------------------------------------------------------------
-spec call(ServerRef :: server_ref(), Label :: atom(), Request :: term(), Timeout :: timeout()) ->
    {ok, Reply :: term()} | {error, Reason :: term()}.
call(ServerRef, Label, Request, Timeout) ->
    MonitorRef = monitor(process, ServerRef),
    ok =
        try
            ServerRef ! {Label, {self(), MonitorRef}, Request},
            ok
        catch
            error:badarg ->
                % Process no longer exists, monitor will send a message
                ok
        end,
    receive
        {'DOWN', MonitorRef, process, _, {E, []} = _Reason} ->
            exit(E);
        {'DOWN', MonitorRef, process, _, {_E, _L} = Reason} ->
            exit(Reason);
        {'DOWN', MonitorRef, process, _, Atom} when is_atom(Atom) ->
            exit(Atom);
        {MonitorRef, Reply} ->
            demonitor(MonitorRef, [flush]),
            {ok, Reply}
    after Timeout ->
        % If Timeout is small enough (0), the error message might be timeout
        % instead of noproc as there could be a race condition with the monitor.
        demonitor(MonitorRef, [flush]),
        exit(timeout)
    end.

%% @private
-spec cast(ServerRef :: server_ref(), Message :: any()) -> ok.
cast(ServerRef, Message) ->
    try
        ServerRef ! {'$gen_cast', Message},
        ok
    catch
        error:_ ->
            % Process does not exist, ignore error
            ok
    end.

%% @private
-spec reply(From :: from(), Reply :: any()) -> ok.
reply({Pid, Ref}, Reply) ->
    try
        Pid ! {Ref, Reply},
        ok
    catch
        _:_ ->
            ok
    end.
