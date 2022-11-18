%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP gen_server interface.
%%
%% This module implements a strict susbset of the Erlang/OTP gen_server
%% interface, supporting operations for local creation and management of
%% gen_server instances.
%%
%% This module is designed to be API-compatible with gen_server, with exceptions noted
%% below.
%%
%% Caveats:
%% <ul>
%%     <li>Support only for locally named procs</li>
%%     <li>No support for abcast</li>
%%     <li>No support for enter_loop</li>
%%     <li>No support for format_status</li>
%%     <li>No support for multi_call</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(gen_server).

-export([
    start/3, start/4,
    start_link/3, start_link/4,
    stop/1, stop/3,
    call/2, call/3,
    cast/2,
    reply/2
]).

-export([loop/1, init_it/4, init_it/5]).

-record(state, {
    name = undefined :: atom(),
    mod :: module(),
    mod_state :: term()
}).

-include("logger.hrl").

-type options() :: list({atom(), term()}).
-type server_ref() :: atom() | pid().
-type from() :: any().

%% @private
do_spawn(Module, Args, Options, SpawnOpts) ->
    Pid = spawn_opt(?MODULE, init_it, [self(), Module, Args, Options], SpawnOpts),
    case wait_ack(Pid) of
        ok -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% @private
do_spawn(Name, Module, Args, Options, SpawnOpts) ->
    Pid = spawn_opt(?MODULE, init_it, [self(), Name, Module, Args, Options], SpawnOpts),
    case wait_ack(Pid) of
        ok -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

init_it(Starter, Name, Module, Args, Options) ->
    case erlang:register(Name, self()) of
        true ->
            init_it(Starter, Module, Args, Options);
        false ->
            init_ack(Starter, {error, {already_started, whereis(Name)}})
    end.

init_it(Starter, Module, Args, Options) ->
    State =
        try
            case Module:init(Args) of
                {ok, ModState} ->
                    init_ack(Starter, ok),
                    #state{
                        name = proplists:get_value(name, Options),
                        mod = Module,
                        mod_state = ModState
                    };
                {stop, Reason} ->
                    init_ack(Starter, {error, {init_stopped, Reason}}),
                    undefined;
                Reply ->
                    init_ack(Starter, {error, {unexpected_reply_from_init, Reply}}),
                    undefined
            end
        catch
            _:E ->
                init_ack(Starter, {error, {bad_return_value, E}}),
                undefined
        end,
    loop(State).

init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return},
    ok.

wait_ack(Pid) ->
    receive
        {ack, Pid, Return} -> Return
    end.

%%-----------------------------------------------------------------------------
%% @param   ServerName the name with which to register the gen_server
%% @param   Module the module in which the gen_server callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_server
%% @returns the gen_server pid, if successful; {error, Reason}, otherwise.
%% @doc     Start a named gen_server.
%%
%%          This function will start a gen_server instance and register the
%%          newly created process with the process registry.  Subsequent calls
%%          may use the gen_server name, in lieu of the process id.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(
    ServerName :: {local, Name :: atom()},
    Module :: module(),
    Args :: term(),
    Options :: options()
) -> {ok, pid()} | {error, Reason :: term()}.
start({local, Name}, Module, Args, Options) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            do_spawn(Name, Module, Args, [{name, Name} | Options], []);
        Pid ->
            {error, {already_started, Pid}}
    end.

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the gen_server callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_server
%% @returns the gen_server pid, if successful; {error, Reason}, otherwise.
%% @doc     Start an un-named gen_server.
%%
%%          This function will start a gen_server instance.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(Module :: module(), Args :: term(), Options :: options()) ->
    {ok, pid()} | {error, Reason :: term()}.
start(Module, Args, Options) ->
    do_spawn(Module, Args, Options, []).

%%-----------------------------------------------------------------------------
%% @param   ServerName the name with which to register the gen_server
%% @param   Module the module in which the gen_server callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_server
%% @returns the gen_server pid, if successful; {error, Reason}, otherwise.
%% @doc     Start and link a named gen_server.
%%
%%          This function will start a gen_server instance and register the
%%          newly created process with the process registry.  Subsequent calls
%%          may use the gen_server name, in lieu of the process id.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(
    ServerName :: {local, Name :: atom()},
    Module :: module(),
    Args :: term(),
    Options :: options()
) -> {ok, pid()} | {error, Reason :: term()}.
start_link({local, Name}, Module, Args, Options) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            do_spawn(Name, Module, Args, [{name, Name} | Options], [link]);
        Pid ->
            {error, {already_started, Pid}}
    end.

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the gen_server callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_server
%% @returns the gen_server pid, if successful; {error, Reason}, otherwise.
%% @doc     Start and link an un-named gen_server.
%%
%%          This function will start a gen_server instance.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(Module :: module(), Args :: term(), Options :: options()) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(Module, Args, Options) ->
    do_spawn(Module, Args, Options, [link]).

%%-----------------------------------------------------------------------------
%% @equiv   stop(ServerRef, normal, infinity)
%% @doc     Stop a previously started gen_server instance.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef :: server_ref()) -> ok | {error, Reason :: term()}.
stop(ServerRef) ->
    stop(ServerRef, normal, infinity).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_server acquired via start
%% @returns ok, if the gen_server stopped; {error, Reason}, otherwise.
%% @doc     Stop a previously started gen_server instance.
%%
%%          This function will stop a gen_server instance, providing the supplied
%%          Reason to the gen_server's terminate/2 callback function.
%%          If the gen_server is named, then the gen_server name may be used
%%          to stop the gen_server.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef :: server_ref(), Reason :: term(), Timeout :: non_neg_integer() | infinity) ->
    ok | {error, Reason :: term()}.
stop(Name, Reason, Timeout) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, undefined};
        Pid when is_pid(Pid) ->
            stop(Pid, Reason, Timeout)
    end;
stop(Pid, Reason, Timeout) when is_pid(Pid) ->
    Ref = erlang:make_ref(),
    call_internal(Pid, {'$stop', {self(), Ref}, Reason}, Timeout).

%%-----------------------------------------------------------------------------
%% @equiv   call(ServerRef, Request, 5000)
%% @doc     Send a request to a gen_server instance, and wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef :: server_ref(), Request :: term) ->
    Reply :: term() | {error, Reason :: term()}.
call(ServerRef, Request) ->
    call(ServerRef, Request, 5000).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_server acquired via start
%% @param   Request the request to send to the gen_server
%% @param   Timeout the amount of time in milliseconds to wait for a reply
%% @returns the reply sent back from the gen_server; {error, Reason}, otherwise.
%% @doc     Send a request to a gen_server instance, and wait for a reply.
%%
%%          This function will send the specified request to the specified
%%          gen_server instance, and wait at least Timeout milliseconds for a
%%          reply from the gen_server.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef :: server_ref(), Request :: term(), TimeoutMs :: timeout()) ->
    Reply :: term() | {error, Reason :: term()}.
call(Name, Request, TimeoutMs) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, undefined};
        Pid when is_pid(Pid) ->
            call(Pid, Request, TimeoutMs)
    end;
call(Pid, Request, TimeoutMs) when is_pid(Pid) ->
    Ref = erlang:make_ref(),
    call_internal(Pid, {'$call', {self(), Ref}, Request}, TimeoutMs).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_server acquired via start
%% @param   Request the request to send to the gen_server
%% @returns ok | {error, Reason}
%% @doc     Send a request to a gen_server instance.
%%
%%          This function will send the specified request to the specified
%%          gen_server instance, but will not wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec cast(ServerRef :: server_ref(), Request :: term()) -> ok | {error, Reason :: term()}.
cast(Name, Request) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, undefined};
        Pid when is_pid(Pid) ->
            cast(Pid, Request)
    end;
cast(Pid, Request) when is_pid(Pid) ->
    Pid ! {'$cast', Request},
    ok.

%%-----------------------------------------------------------------------------
%% @param   From the client to whom to send the reply
%% @param   Reply the reply to send to the client
%% @returns an arbitrary term, that should be ignored
%% @doc     Send a reply to a calling client.
%%
%%          This function will send the specified reply back to the specified
%%          gen_server client (e.g, via call/3).  The return value of this
%%          function can be safely ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec reply(from(), Reply :: term) -> term().
reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply},
    ok.

%%
%% Internal operations
%%

%% @private
call_internal(Pid, {_Tag, From, _Term} = Msg, TimeoutMs) ->
    Pid ! Msg,
    wait_reply(From, TimeoutMs).

wait_reply({_Pid, Ref}, TimeoutMs) ->
    receive
        {Ref, Reply} -> Reply
    after TimeoutMs -> exit(timeout)
    end.

%% @private
loop(undefined) ->
    ok;
loop(#state{mod = Mod, mod_state = ModState} = State) ->
    receive
        {'$call', {_Pid, _Ref} = From, Request} ->
            case Mod:handle_call(Request, From, ModState) of
                {reply, Reply, NewModState} ->
                    ok = reply(From, Reply),
                    loop(State#state{mod_state = NewModState});
                {noreply, NewModState} ->
                    loop(State#state{mod_state = NewModState});
                {stop, Reason, Reply, NewModState} ->
                    ok = reply(From, Reply),
                    do_terminate(State, Reason, NewModState);
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end;
        {'$cast', Request} ->
            case Mod:handle_cast(Request, ModState) of
                {noreply, NewModState} ->
                    loop(State#state{mod_state = NewModState});
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end;
        {'$stop', {_Pid, _Ref} = From, Reason} ->
            do_terminate(State, Reason, ModState),
            ok = reply(From, ok);
        Info ->
            case Mod:handle_info(Info, ModState) of
                {noreply, NewModState} ->
                    loop(State#state{mod_state = NewModState});
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end
    end.

%% @private
do_terminate(#state{mod = Mod, name = Name} = _State, Reason, ModState) ->
    case Name of
        undefined -> ok;
        %% TODO unregister
        _Pid -> ok
    end,
    case erlang:function_exported(Mod, terminate, 2) of
        true ->
            Mod:terminate(Reason, ModState);
        false ->
            ok
    end,
    case Reason of
        normal -> ok;
        Other -> exit(Other)
    end.
