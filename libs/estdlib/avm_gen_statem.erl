%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP gen_statem interface.
%%
%% This module implements a strict susbset of the Erlang/OTP gen_statem
%% interface, supporting operations for local creation and management of
%% gen_statem instances.
%%
%% This module is designed to be API-compatible with gen_statem, with exceptions noted
%% below.
%%
%% Caveats:
%% <ul>
%%     <li>No support for start_link</li>
%%     <li>Support only for locally named gen_statem instances</li>
%%     <li>Support only for state function event handlers</li>
%%     <li>No support for keeep_state or repeat_state return values from Module:StateName/3 callbacks</li>
%%     <li>No support for postpone or hibernate state transition actions</li>
%%     <li>No support for state enter calls</li>
%%     <li>No support for multi_call</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(avm_gen_statem).

-export([start/3, start/4, stop/1, stop/3, call/2, call/3, cast/2, reply/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("estdlib.hrl").

-record(state, {
    mod :: module(),
    current_state :: atom(),
    data :: term()
}).

-include("logger.hrl").

-type options() :: list({atom(), term()}).
-type server_ref() :: atom() | pid().


%%-----------------------------------------------------------------------------
%% @param   ServerName the name with which to register the gen_statem
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successfule; {error, Reason}, otherwise.
%% @doc     Start a named gen_statem.
%%
%%          This function will start a gen_statem instance and register the
%%          newly created process with the process registry.  Subsequent calls
%%          may use the gen_statem name, in lieu of the process id.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(ServerName::{local, Name::atom()}, Module::module(), Args::term(), Options::options()) -> {ok, pid()} | {error, Reason::term()}.
start({local, Name} = ServerName, Module, Args, Options) when is_atom(Name) ->
    ?LOG_DEBUG({start, ServerName, Module, Args, Options}),
    ?GEN_SERVER:start(ServerName, ?MODULE, {Module, Args}, Options).

%%-----------------------------------------------------------------------------
%% @param   Module the module in which the gen_statem callbacks are defined
%% @param   Args the arguments to pass to the module's init callback
%% @param   Options the options used to create the gen_statem
%% @returns the gen_statem pid, if successfule; {error, Reason}, otherwise.
%% @doc     Start an un-named gen_statem.
%%
%%          This function will start a gen_statem instance.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec start(Module::module(), Args::term(), Options::options()) -> {ok, pid()} | {error, Reason::term()}.
start(Module, Args, Options) ->
    ?LOG_DEBUG({start, Module, Args, Options}),
    ?GEN_SERVER:start(?MODULE, {Module, Args}, Options).


%%-----------------------------------------------------------------------------
%% @equiv   stop(ServerRef, normal, infinity)
%% @doc     Stop a previously started gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef::server_ref()) -> ok | {error, Reason::term()}.
stop(ServerRef) ->
    ?LOG_DEBUG({stop, ServerRef}),
    ?GEN_SERVER:stop(ServerRef).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @returns ok, if the gen_statem stopped; {error, Reason}, otherwise.
%% @doc     Stop a previously started gen_statem instance.
%%
%%          This function will stop a gen_statem instance, providing the supplied
%%          Reason to the .  If the gen_statem is
%%          a named gen_statem, then the gen_statem name may be used to stop the gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef::server_ref(), Reason::term(), Timeout::non_neg_integer() | infinity) -> ok | {error, Reason::term()}.
stop(ServerRef, Reason, Timeout) ->
    ?LOG_DEBUG({stop, ServerRef, Reason, Timeout}),
    ?GEN_SERVER:stop(ServerRef, Reason, Timeout).

%%-----------------------------------------------------------------------------
%% @equiv   call(ServerRef, Request, infinity)
%% @doc     Send a request to a gen_statem instance, and wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef::server_ref(), Request::term) -> Reply::term() | {error, Reason::term()}.
call(ServerRef, Request) ->
    call(ServerRef, Request, 5000).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @param   Request the request to send to the gen_statem
%% @param   Timeout the amount of time in milliseconds to wait for a reply
%% @returns the reply sent back from the gen_statem; {error, Reason}, otherwise.
%% @doc     Send a request to a gen_statem instance, and wait for a reply..
%%
%%          This function will send the specified request to the specified
%%          gen_statem instance, and wait at least Timeout milliseconds for a
%%          reply from the gen_statem.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef::server_ref(), Request::term(), Timeout::timeout()) -> Reply::term() | {error, Reason::term()}.
call(ServerRef, Request, Timeout) ->
    ?LOG_DEBUG({call, ServerRef, Request, Timeout}),
    ?GEN_SERVER:call(ServerRef, Request, Timeout).

%%-----------------------------------------------------------------------------
%% @param   ServerRef a reference to the gen_statem acquired via start
%% @param   Request the request to send to the gen_statem
%% @returns ok | {error, Reason}
%% @doc     Send a request to a gen_statem instance.
%%
%%          This function will send the specified request to the specified
%%          gen_statem instance, but will not wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
cast(ServerRef, Request) ->
    ?LOG_DEBUG({cast, ServerRef, Request}),
    ?GEN_SERVER:cast(ServerRef, Request).

%%-----------------------------------------------------------------------------
%% @param   Client the client to whom to send the reply
%% @param   Reply the reply to send to the client
%% @returns an arbitrary term, that should be ignored
%% @doc     Send a reply to a calling client.
%%
%%          This function will send the specified reply back to the specified
%%          gen_statem client (e.g, via call/3).  The return value of this
%%          function can be safely ignored.
%% @end
%%-----------------------------------------------------------------------------
reply(Client, Reply) ->
    ?LOG_DEBUG({reply, Client, Reply}),
    ?GEN_SERVER:reply(Client, Reply).

%%
%% gen_statem callbacks
%%

%% @hidden
init({Module, Args}) ->
    case Module:init(Args) of
        {ok, NextState, Data} ->
            {ok, #state{mod=Module, current_state=NextState, data=Data}};
        {ok, NextState, Data, Actions} ->
            handle_actions(Actions, [{next_state, NextState}]),
            {ok, #state{mod=Module, current_state=NextState, data=Data}};
        {stop, Reason} ->
            {stop, Reason};
        _ ->
            {stop, {error, undefined}}
    end.


%% @hidden
handle_call(Request, From, State) ->
    ?LOG_DEBUG({handle_call, Request, From, State}),
    do_handle_state({call, From}, Request, State).


%% @hidden
handle_cast(Request, State) ->
    ?LOG_DEBUG({handle_cast, Request, State}),
    do_handle_state(cast, Request, State).


%% @hidden
handle_info({timeout, _TimerRef, {state_timeout, State, Msg}}, #state{current_state=CurrentState} = State) ->
    ?LOG_DEBUG({handle_info, {state_timeout, CurrentState, Msg}, State}),
    case State of
        CurrentState ->
            do_handle_state(state_timeout, Msg, State);
        _ -> ok
    end;
handle_info(Request, State) ->
    ?LOG_DEBUG({handle_info, Request, State}),
    do_handle_state(info, Request, State).


%% @hidden
terminate(Reason, #state{mod=Module, current_state=CurrentState, data=Data} = _State) ->
    Module:terminate(Reason, CurrentState, Data),
    ok.

%%
%% Internal operations
%%

%% @private
do_handle_state(EventType, Request, #state{mod=Module, current_state=CurrentState, data=Data} = State) ->
    ?LOG_DEBUG({do_handle_state, [EventType, Request, State]}),
    case Module:CurrentState(EventType, Request, Data) of
        {next_state, NextState, NewData} ->
            maybe_log_state_transition(CurrentState, NextState),
            {noreply, State#state{current_state=NextState, data=NewData}};
        {next_state, NextState, NewData, Actions} ->
            maybe_log_state_transition(CurrentState, NextState),
            handle_actions(Actions, [{current_state, CurrentState}, {next_state, NextState}]),
            {noreply, State#state{current_state=NextState, data=NewData}};
        {stop, Reason} ->
            {stop, Reason, State};
        {stop, Reason, NewData} ->
            {stop, Reason, State#state{data=NewData}};
        {stop_and_reply, Reason, Replies} ->
            handle_actions(Replies, []),
            {stop, Reason, State};
        {stop_and_reply, Reason, Replies, NewData} ->
            handle_actions(Replies, []),
            {stop, Reason, State#state{data=NewData}};
        Reply ->
            {error, {unexpected_reply, Reply}}
    end.

%% @private
handle_actions([], _Context) ->
    ?LOG_DEBUG({handle_actions, []}),
    ok;
handle_actions([{reply, From, Reply} | T], Context) ->
    ?LOG_DEBUG({handle_actions, From, Reply, T, Context}),
    reply(From, Reply),
    handle_actions(T, Context);
handle_actions([{state_timeout, Timeout, Msg} | T], Context) ->
    ?LOG_DEBUG({handle_actions, state_timeout}),
    timer_manager:start_timer(Timeout, self(), {state_timeout, ?PROPLISTS:get_value(next_state, Context), Msg}),
    handle_actions(T, Context);
handle_actions([_ | T], Context) ->
    ?LOG_DEBUG({handle_actions, rest, T}),
    handle_actions(T, Context).

%% @private
maybe_log_state_transition(CurrentState, CurrentState) ->
    ok;
maybe_log_state_transition(CurrentState, NextState) ->
    ?LOG_DEBUG({state_transition, CurrentState, NextState}).
