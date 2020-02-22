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
%%     <li>No support for start_link</li>
%%     <li>Support only for locally named procs</li>
%%     <li>No support for abcast</li>
%%     <li>No support for enter_loop</li>
%%     <li>No support for format_status</li>
%%     <li>No support for multi_call</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(gen_server).

-export([start/3, start/4, stop/1, stop/3, call/2, call/3, cast/2, reply/2]).
-export([loop/1]).

-include("estdlib.hrl").

-record(state, {
    name = undefined :: atom(),
    mod :: module(),
    mod_state :: term()
}).

-include("logger.hrl").

-type options() :: list({atom(), term()}).
-type server_ref() :: atom() | pid().


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
-spec start(ServerName::{local, Name::atom()}, Module::module(), Args::term(), Options::options()) -> {ok, pid()} | {error, Reason::term()}.
start({local, Name}, Module, Args, Options) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            Response = start(Module, Args, [{name, Name} | Options]),
            case Response of
                {ok, Pid} ->
                    erlang:register(Name, Pid),
                    ok;
                _ -> ok
            end,
            Response;
        _Pid ->
            {error, already_started}
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
-spec start(Module::module(), Args::term(), Options::options()) -> {ok, pid()} | {error, Reason::term()}.
start(Module, Args, Options) ->
    case Module:init(Args) of
        {ok, ModState} ->
            State = #state{
                name = ?PROPLISTS:get_value(name, Options),
                mod = Module,
                mod_state = ModState
            },
            Pid = spawn(?MODULE, loop, [State]),
            {ok, Pid};
        {stop, Reason} ->
            {error, {init_stopped, Reason}};
        _ ->
            {error, unexpected_reply_from_init}
    end.


%%-----------------------------------------------------------------------------
%% @equiv   stop(ServerRef, normal, infinity)
%% @doc     Stop a previously started gen_server instance.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(ServerRef::server_ref()) -> ok | {error, Reason::term()}.
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
-spec stop(ServerRef::server_ref(), Reason::term(), Timeout::non_neg_integer() | infinity) -> ok | {error, Reason::term()}.
stop(Name, Reason, Timeout) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, undefined};
        Pid when is_pid(Pid) ->
            stop(Pid, Reason, Timeout)
    end;
stop(Pid, Reason, Timeout) when is_pid(Pid) ->
    Ref = erlang:make_ref(),
    call_internal(Pid, Ref, {'$stop', self(), Ref, Reason}, Timeout).

%%-----------------------------------------------------------------------------
%% @equiv   call(ServerRef, Request, 5000)
%% @doc     Send a request to a gen_server instance, and wait for a reply.
%% @end
%%-----------------------------------------------------------------------------
-spec call(ServerRef::server_ref(), Request::term) -> Reply::term() | {error, Reason::term()}.
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
-spec call(ServerRef::server_ref(), Request::term(), Timeout::timeout()) -> Reply::term() | {error, Reason::term()}.
call(Name, Request, Timeout) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            {error, undefined};
        Pid when is_pid(Pid) ->
            call(Pid, Request, Timeout)
    end;
call(Pid, Request, Timeout) when is_pid(Pid) ->
    Ref = erlang:make_ref(),
    call_internal(Pid, Ref, {'$call', self(), Ref, Request}, Timeout).

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
-spec cast(ServerRef::server_ref(), Request::term()) -> ok | {error, Reason::term()}.
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
%% @param   Client the client to whom to send the reply
%% @param   Reply the reply to send to the client
%% @returns an arbitrary term, that should be ignored
%% @doc     Send a reply to a calling client.
%%
%%          This function will send the specified reply back to the specified
%%          gen_server client (e.g, via call/3).  The return value of this
%%          function can be safely ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec reply(Client::term(), Reply::term) -> term().
reply({Pid, Ref} = _Client, Reply) ->
    Pid ! {Ref, Reply}, ok.


%%
%% Internal operations
%%

%% @private
call_internal(Pid, Ref, Msg, Timeout) ->
    Pid ! Msg,
    receive
        {Ref, Reply} ->
        Reply
    after Timeout ->
        %% throw(timeout)
        {error, timeout}
    end.

%% @private
loop(#state{mod=Mod, mod_state=ModState} = State) ->
    receive
        {'$call', Pid, Ref, Request} ->
            case Mod:handle_call(Request, {Pid, Ref}, ModState) of
                {reply, Reply, NewModState} ->
                    Pid ! {Ref, Reply},
                    loop(State#state{mod_state=NewModState});
                {noreply, NewModState} ->
                    loop(State#state{mod_state=NewModState});
                {stop, Reason, Reply, NewModState} ->
                     Pid ! {Ref, Reply},
                     do_terminate(State, Reason, NewModState);
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end;
        {'$cast', Request} ->
            case Mod:handle_cast(Request, ModState) of
                {noreply, NewModState} ->
                    loop(State#state{mod_state=NewModState});
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end;
        {'$stop', Pid, Ref, Reason} ->
            do_terminate(State, Reason, ModState),
            Pid ! {Ref, ok};
        Info ->
            case Mod:handle_info(Info, ModState) of
                {noreply, NewModState} ->
                    loop(State#state{mod_state=NewModState});
                {stop, Reason, NewModState} ->
                    do_terminate(State, Reason, NewModState);
                _ ->
                    do_terminate(State, {error, unexpected_reply}, ModState)
            end
    end.

%% @private
do_terminate(#state{mod=Mod, name=Name} = _State, Reason, ModState) ->
    case Name of
        undefined -> ok;
        _Pid -> ok %% TODO unregister
    end,
    Mod:terminate(Reason, ModState),
    ok.
