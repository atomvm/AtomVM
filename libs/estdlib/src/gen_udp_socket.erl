%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

%% @hidden
-module(gen_udp_socket).

-export([
    open/1, open/2,
    send/4,
    recv/2, recv/3,
    close/1,
    controlling_process/2
]).

-include("inet-priv.hrl").
-include_lib("kernel/include/logger.hrl").

%% inet API
-export([port/1, sockname/1]).

%% gen_server implementation (hidden)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type packet() :: string() | binary().
-type reason() :: term().

-type option() ::
    {active, boolean()}
    | {buffer, pos_integer()}
    | {timeout, timeout()}
    | list
    | binary
    | {binary, boolean()}
    | {ifaddr, inet:ip_address()}.

-define(DEFAULT_OPTIONS, #{
    active => true,
    buffer => 0,
    timeout => infinity
}).

%% @hidden
-spec open(Port :: inet:port_number()) -> {ok, inet:socket()} | {error, Reason :: reason()}.
open(Port) ->
    open(Port, []).

%% @hidden
-spec open(Port :: inet:port_number(), Options :: [option()]) ->
    {ok, inet:socket()} | {error, Reason :: reason()}.
open(Port, Options) ->
    ControllingProcess = self(),
    case socket:open(inet, dgram, udp) of
        {ok, Socket} ->
            EffectiveOptions = maps:merge(?DEFAULT_OPTIONS, proplist_to_map(Options)),
            Addr = maps:get(ifaddr, EffectiveOptions, any),
            case socket:bind(Socket, #{family => inet, addr => Addr, port => Port}) of
                ok ->
                    gen_server:start_link(
                        ?MODULE, {Socket, ControllingProcess, EffectiveOptions}, []
                    );
                BindError ->
                    BindError
            end;
        OpenError ->
            OpenError
    end.

%% @hidden
-spec send(
    Socket :: inet:socket(),
    Address :: inet:ip_address(),
    Port :: inet:port_number(),
    Packet :: packet()
) -> ok | {error, reason()}.
send(Socket, Address, Port, Packet) ->
    call(Socket, {sendto, Address, Port, Packet}).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer(), Timeout :: timeout()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length, Timeout) ->
    call(Socket, {recvfrom, Length, Timeout}).

%% @hidden
-spec close(inet:socket()) -> ok.
close(Socket) ->
    call(Socket, close).

%% @hidden
-spec controlling_process(Socket :: inet:socket(), Pid :: pid()) ->
    ok | {error, Reason :: reason()}.
controlling_process(Socket, Pid) when is_pid(Pid) ->
    %% WARNING: calling process is potentially spoofable
    call(Socket, {controlling_process, Pid, self()}).

%%
%% inet API
%%

%% @hidden
port(Socket) ->
    call(Socket, port).

%% @hidden
sockname(Socket) ->
    call(Socket, sockname).

%%
%% gen_server implementation
%%

-record(state, {
    socket,
    controlling_process = undefined,
    monitor_ref = undefined,
    active,
    options,
    pending_selects = #{}
}).

init({Socket, ControllingProcess, Options}) ->
    case maps:get(active, Options, true) of
        true ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    MonitorRef = erlang:monitor(process, ControllingProcess),
                    {ok, #state{
                        socket = Socket,
                        controlling_process = ControllingProcess,
                        monitor_ref = MonitorRef,
                        active = true,
                        options = Options,
                        pending_selects = #{Ref => active}
                    }};
                {error, _Reason} = Error ->
                    Error
            end;
        _ ->
            {ok, #state{
                socket = Socket,
                controlling_process = ControllingProcess,
                active = false,
                options = Options
            }}
    end.

%% @hidden
handle_call({sendto, Address, Port, Packet}, _From, State) ->
    Dest = #{
        family => inet,
        port => Port,
        addr => Address
    },
    {reply, socket:sendto(State#state.socket, Packet, Dest), State};
handle_call({recvfrom, Length, Timeout}, From, State) ->
    case State#state.active of
        true ->
            {reply, {error, einval}, State};
        _ ->
            Ref = erlang:make_ref(),
            case Timeout of
                TimeoutMs when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
                    erlang:send_after(TimeoutMs, self(), {timeout, Ref, From});
                infinity ->
                    ok
            end,
            case socket:nif_select_read(State#state.socket, Ref) of
                ok ->
                    PendingSelects = State#state.pending_selects,
                    NewPendingSelects = PendingSelects#{Ref => {passive, From, Length, Timeout}},
                    {noreply, State#state{pending_selects = NewPendingSelects}};
                {error, _Reason} = Error ->
                    ?LOG_ERROR("An error occurred calling socket:nif_select_read/2: ~p", [Error]),
                    {noreply, State}
            end
    end;
handle_call(port, _From, State) ->
    Reply =
        case socket:sockname(State#state.socket) of
            {ok, Addr} ->
                #{port := Port} = Addr,
                {ok, Port};
            SocknameError ->
                SocknameError
        end,
    {reply, Reply, State};
handle_call(sockname, _From, State) ->
    Reply =
        case socket:sockname(State#state.socket) of
            {ok, Addr} ->
                #{port := Port, addr := Address} = Addr,
                {ok, {Address, Port}};
            SocknameError ->
                SocknameError
        end,
    {reply, Reply, State};
handle_call(close, _From, State) ->
    {stop, normal, socket:close(State#state.socket), State};
handle_call({controlling_process, Pid, Caller}, _From, State) ->
    case State#state.active of
        false ->
            {reply, {error, einval}, State};
        _ ->
            case State#state.controlling_process =/= Caller of
                true ->
                    {reply, {error, not_owner}, State};
                _ ->
                    MonitorRef = erlang:monitor(process, Pid),
                    true = erlang:demonitor(State#state.monitor_ref),
                    {reply, ok, State#state{controlling_process = Pid, monitor_ref = MonitorRef}}
            end
    end;
handle_call(Request, _From, State) ->
    {reply, {unknown_request, Request}, State}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info({select, _Socket, Ref, ready_input}, State) ->
    case maps:get(Ref, State#state.pending_selects, undefined) of
        undefined ->
            ?LOG_INFO("Unable to find select ref ~p in pending selects", [Ref]),
            {noreply, State};
        active ->
            NewState = handle_active_recvfrom(State),
            {noreply, NewState};
        {passive, From, Length, Timeout} ->
            NewState = handle_passive_recvfrom(State, From, Length, Timeout),
            {noreply, NewState#state{
                pending_selects = maps:remove(Ref, State#state.pending_selects)
            }}
    end;
handle_info({timeout, Ref, From}, State) ->
    case maps:get(Ref, State#state.pending_selects, undefined) of
        undefined ->
            %% in all liklhood the request as processed.  Ignore the message
            {noreply, State};
        _ ->
            ?LOG_INFO("Select ref ~p in pending selects has timed out.", [Ref]),
            gen_server:reply(From, {error, timeout}),
            {noreply, State#state{pending_selects = maps:remove(Ref, State#state.pending_selects)}}
    end;
handle_info({'DOWN', MonitorRef, process, ControllingProcess, _Info}, State) ->
    case State#state.monitor_ref =:= MonitorRef of
        true ->
            ?LOG_INFO("Controlling process ~p has terminated.  Stopping ~p.", [
                ControllingProcess, ?MODULE
            ]),
            {stop, normal, State};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(Reason, State) ->
    ?LOG_INFO("Closing socket ~p for reason ~p", [State#state.socket, Reason]),
    catch socket:close(State#state.socket).

%%
%% Internal operations
%%

%% @private
proplist_to_map(PropList) ->
    proplist_to_map(PropList, #{}).

%% @private
proplist_to_map([], Accum) ->
    Accum;
proplist_to_map([Atom | T], Accum) when is_atom(Atom) ->
    proplist_to_map(T, Accum#{Atom => true});
proplist_to_map([{K, V} | T], Accum) ->
    proplist_to_map(T, Accum#{K => V}).

%% @private
handle_active_recvfrom(State) ->
    Socket = State#state.socket,
    Length = maps:get(buffer, State#state.options, 0),
    ControllingProcess = State#state.controlling_process,
    WrappedSocket = {?GEN_UDP_MONIKER, self(), ?MODULE},
    %% CAUTION: internal API
    case socket:nif_recvfrom(Socket, Length) of
        {ok, {Address, Data}} ->
            BinaryOrList = maybe_encode_binary(State#state.options, Data),
            #{addr := Addr, port := Port} = Address,
            ?LOG_INFO("Sending message to ControllingProcess ~p", [ControllingProcess]),
            ControllingProcess ! {udp, WrappedSocket, Addr, Port, BinaryOrList},

            %% start a new select
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    PendingSelects = State#state.pending_selects,
                    NewPendingSelects = maps:remove(Ref, PendingSelects),
                    State#state{pending_selects = NewPendingSelects#{Ref => active}};
                _ ->
                    State
            end;
        {closed, _Ref} ->
            % socket was closed by another process
            % TODO: we need to handle:
            % (a) SELECT_STOP being scheduled
            % (b) flush of messages as we can have both
            % {closed, Ref} and {select, _, Ref, _} in the
            % queue
            State#state.controlling_process ! {udp_closed, WrappedSocket},
            State;
        {error, _} = E ->
            %% CAUTION: internal API
            socket:nif_select_stop(Socket),
            ?LOG_INFO("unable to receive on pending select~n"),
            State#state.controlling_process ! {udp_error, WrappedSocket, E},
            State
    end.

%% @private
handle_passive_recvfrom(State, From, Length, _Timeout) ->
    Socket = State#state.socket,
    %% CAUTION: internal API
    case socket:nif_recvfrom(Socket, Length) of
        {ok, {Address, Data}} ->
            BinaryOrList = maybe_encode_binary(State#state.options, Data),
            #{addr := Addr, port := Port} = Address,
            % WrappedSocket = {?GEN_UDP_MONIKER, self(), ?MODULE},
            gen_server:reply(From, {ok, {Addr, Port, BinaryOrList}}),
            State;
        {error, _} = E ->
            socket:nif_select_stop(Socket),
            ?LOG_INFO("unable to receive on pending select~n"),
            gen_server:reply(From, E),
            State
    end.

%% @private
call(Pid, Request) ->
    gen_server:call(Pid, Request, infinity).

%% @private
maybe_encode_binary(Options, Data) ->
    case encode_binary(Options) of
        true ->
            Data;
        _ ->
            binary_to_list(Data)
    end.

%% @private
encode_binary(Options) ->
    case {maps:get(binary, Options, undefined), maps:get(list, Options, undefined)} of
        {undefined, undefined} ->
            true;
        {true, _} ->
            true;
        _ ->
            false
    end.
