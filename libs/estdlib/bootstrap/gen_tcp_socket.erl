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
-module(gen_tcp_socket).

-export([
    connect/3,
    send/2,
    recv/2,
    recv/3,
    close/1,
    listen/2,
    accept/1,
    accept/2,
    controlling_process/2
]).

-include("inet-priv.hrl").
-include_lib("kernel/include/logger.hrl").

%% inet API
-export([port/1, sockname/1, peername/1]).

%% gen_server implementation (hidden)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type reason() :: term().

-type option() ::
    {active, boolean()}
    | {buffer, pos_integer()}
    | {timeout, timeout()}
    | list
    | binary.

-type listen_option() :: option().
-type connect_option() :: option().
-type packet() :: string() | binary().

-define(DEFAULT_OPTIONS, #{
    active => true,
    buffer => 0,
    timeout => infinity
}).

-spec connect(
    Address :: inet:ip_address() | inet:hostname(),
    Port :: inet:port_number(),
    Options :: [connect_option()]
) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
%% @hidden
connect(Address, Port, Options) ->
    ControllingProcess = self(),
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            case socket:connect(Socket, #{family => inet, addr => Address, port => Port}) of
                ok ->
                    EffectiveOptions = maps:merge(?DEFAULT_OPTIONS, proplist_to_map(Options)),
                    gen_server:start_link(
                        ?MODULE, {Socket, ControllingProcess, connect, EffectiveOptions}, []
                    );
                ConnectError ->
                    ConnectError
            end;
        OpenError ->
            OpenError
    end.

%% @hidden
-spec send(Socket :: inet:socket(), Packet :: packet()) -> ok | {error, Reason :: reason()}.
send(Socket, Packet) ->
    call(Socket, {send, Packet}).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer()) ->
    {ok, packet()} | {error, Reason :: reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer(), Timeout :: non_neg_integer()) ->
    {ok, packet()} | {error, Reason :: reason()}.
recv(Socket, Length, Timeout) ->
    call(Socket, {recv, Length, Timeout}).

%% @hidden
-spec listen(Port :: inet:port_number(), Options :: [listen_option()]) ->
    {ok, ListeningSocket :: inet:socket()} | {error, Reason :: reason()}.
listen(Port, Options) ->
    ControllingProcess = self(),
    case socket:open(inet, stream, tcp) of
        {ok, Socket} ->
            EffectiveOptions = maps:merge(
                ?DEFAULT_OPTIONS, proplist_to_map(Options)
            ),
            Addr = maps:get(ifaddr, EffectiveOptions, any),
            case socket:bind(Socket, #{family => inet, addr => Addr, port => Port}) of
                ok ->
                    case socket:listen(Socket) of
                        ok ->
                            gen_server:start_link(
                                ?MODULE, {Socket, ControllingProcess, listen, EffectiveOptions}, []
                            );
                        ListenError ->
                            socket:close(Socket),
                            ListenError
                    end;
                BindError ->
                    socket:close(Socket),
                    BindError
            end;
        OpenError ->
            OpenError
    end.

%% @hidden
-spec accept(ListenSocket :: inet:socket()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
accept(ListenSocket) ->
    accept(ListenSocket, infinity).

%% @hidden
-spec accept(ListenSocket :: inet:socket(), Timeout :: timeout()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
accept(ListenSocket, Timeout) ->
    AcceptingProc = self(),
    call(ListenSocket, {accept, Timeout, AcceptingProc}).

%% @hidden
-spec close(Socket :: inet:socket()) -> ok.
close(Socket) ->
    call(Socket, close).

%% @hidden
-spec controlling_process(Socket :: inet:socket(), Pid :: pid()) ->
    ok | {error, Reason :: reason()}.
controlling_process(Socket, Pid) ->
    %% WARNING: calling process is potentially spoofable
    call(Socket, {controlling_process, Pid, self()}).

%%
%% inet API
%%

%% @hidden
port(Socket) ->
    call(Socket, get_port).

%% @hidden
sockname(Socket) ->
    call(Socket, sockname).

%% @hidden
peername(Socket) ->
    call(Socket, peername).

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

init({Socket, ControllingProcess, Mode, Options}) ->
    case (Mode =:= connect orelse Mode =:= accept) andalso maps:get(active, Options, true) of
        true ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    ?LOG_INFO(
                        "Starting process in ~p (active) mode.  Socket=~p ControllingProcess=~p Options=~p Ref=~p",
                        [
                            Mode, Socket, ControllingProcess, Options, Ref
                        ]
                    ),
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
                    %% is there a better return from init?
                    Error
            end;
        _ ->
            ?LOG_INFO(
                "Starting process in ~p (passive) mode.  Socket=~p ControllingProcess=~p Options=~p",
                [
                    Mode, Socket, ControllingProcess, Options
                ]
            ),
            {ok, #state{
                socket = Socket,
                active = false,
                options = Options
            }}
    end.

%% @hidden
handle_call({accept, Timeout, AcceptingProc}, From, State) ->
    ?LOG_DEBUG("handle_call [~p], ~p, ~p]", [
        {accept, Timeout, AcceptingProc}, From, State
    ]),
    Ref = erlang:make_ref(),
    case Timeout of
        TimeoutMs when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
            erlang:send_after(TimeoutMs, self(), {timeout, Ref, From});
        infinity ->
            ok
    end,
    case socket:nif_select_read(State#state.socket, Ref) of
        ok ->
            ?LOG_INFO("Starting accept select with ref ~p", [Ref]),
            PendingSelects = State#state.pending_selects,
            NewPendingSelects = PendingSelects#{Ref => {accept, From, AcceptingProc, Timeout}},
            {noreply, State#state{pending_selects = NewPendingSelects}};
        {error, _Reason} = Error ->
            ?LOG_ERROR("An error occurred in select for accept ~p", [Error]),
            {reply, Error, State}
    end;
handle_call({send, Packet}, From, State) ->
    ?LOG_DEBUG("handle_call [~p], ~p, ~p]", [
        {send, Packet}, From, State
    ]),
    ?LOG_INFO("Sending packet"),
    {reply, socket:send(State#state.socket, Packet), State};
handle_call({recv, Length, Timeout}, From, State) ->
    ?LOG_DEBUG("handle_call [~p], ~p, ~p]", [
        {recv, Length, Timeout}, From, State
    ]),
    case State#state.active of
        true ->
            {reply, {error, einval}, State};
        _ ->
            Ref = erlang:make_ref(),
            case Timeout of
                TimeoutMs when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
                    ?LOG_DEBUG("setting timeout counter for TimeoutMs=~p on ref=~p", [
                        TimeoutMs, Ref
                    ]),
                    erlang:send_after(TimeoutMs, self(), {timeout, Ref, From});
                infinity ->
                    %% TEST
                    erlang:send_after(30000, self(), {timeout, Ref, From}),
                    ok
            end,
            case socket:nif_select_read(State#state.socket, Ref) of
                ok ->
                    PendingSelects = State#state.pending_selects,
                    NewPendingSelects = PendingSelects#{Ref => {passive, From, Length, Timeout}},
                    ?LOG_INFO("Added pending select.  NewPendingSelects=~p", [NewPendingSelects]),
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
handle_call(peername, _From, State) ->
    Reply =
        case socket:peername(State#state.socket) of
            {ok, Addr} ->
                #{port := Port, addr := Address} = Addr,
                {ok, {Address, Port}};
            SocknameError ->
                SocknameError
        end,
    {reply, Reply, State};
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
                    ?LOG_INFO("Set controlling process to ~p", [Pid]),
                    {reply, ok, State#state{controlling_process = Pid, monitor_ref = MonitorRef}}
            end
    end;
handle_call(close, _From, State) ->
    {stop, normal, socket:close(State#state.socket), State};
handle_call(Request, _From, State) ->
    {reply, {unknown_request, Request}, State}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info({select, _Socket, Ref, ready_input}, State) ->
    ?LOG_DEBUG("handle_info [~p], ~p]", [
        {select, _Socket, Ref, ready_input}, State
    ]),
    %% TODO cancel timer
    case maps:get(Ref, State#state.pending_selects, undefined) of
        undefined ->
            ?LOG_WARNING("Unable to find select ref ~p in pending selects", [Ref]),
            %% select_stop?
            {noreply, State};
        {accept, From, AcceptingProc, _Timeout} ->
            ?LOG_INFO("Select ready for read on accept"),
            NewState = handle_accept(State, From, AcceptingProc),
            {noreply, NewState#state{
                pending_selects = maps:remove(Ref, State#state.pending_selects)
            }};
        active ->
            ?LOG_INFO("Select ready for read on active recv"),
            NewState = handle_active_recv(State),
            {noreply, NewState};
        {passive, From, Length, Timeout} ->
            ?LOG_INFO("Select ready for read on passive recv"),
            NewState = handle_passive_recv(State, From, Length, Timeout),
            {noreply, NewState#state{
                pending_selects = maps:remove(Ref, State#state.pending_selects)
            }}
    end;
handle_info({timeout, Ref, From}, State) ->
    ?LOG_DEBUG("handle_info [~p], ~p]", [
        {timeout, Ref, From}, State
    ]),
    case maps:get(Ref, State#state.pending_selects, undefined) of
        undefined ->
            %% The request was already processed.  Ignore the message
            {noreply, State};
        _ ->
            ?LOG_INFO("Select ref ~p in pending selects has timed out.", [Ref]),
            gen_server:reply(From, {error, timeout}),
            {noreply, State#state{pending_selects = maps:remove(Ref, State#state.pending_selects)}}
    end;
handle_info({'DOWN', MonitorRef, process, ControllingProcess, Info}, State) ->
    ?LOG_DEBUG("handle_info [~p], ~p", [
        {'DOWN', MonitorRef, process, ControllingProcess, Info}
    ]),
    case State#state.monitor_ref =:= MonitorRef of
        true ->
            ?LOG_INFO("Controlling process ~p has terminated.  Stopping ~p.", [
                ControllingProcess, ?MODULE
            ]),
            {stop, normal, State};
        _ ->
            {noreply, State}
    end;
handle_info(Info, State) ->
    ?LOG_WARNING("Received unexpected Info msg ~p with State ~p", [Info, State]),
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
handle_accept(State, From, AcceptingProc) ->
    Socket = State#state.socket,
    ControllingProcess = AcceptingProc,
    Options = State#state.options,
    %% CAUTION: internal API
    case socket:nif_accept(Socket) of
        {ok, ConnectedSocket} ->
            Reply =
                gen_server:start_link(
                    ?MODULE, {ConnectedSocket, ControllingProcess, accept, Options}, []
                ),
            ?LOG_INFO("Accepted connection.  Attempted to start connected socket: ~p", [Reply]),
            gen_server:reply(From, Reply),
            State;
        {error, _Reason} = Error ->
            ?LOG_ERROR("Failed to accept connection: ~p", [Error]),
            %% CAUTION: internal API
            socket:nif_select_stop(Socket),
            gen_server:reply(From, Error),
            State
    end.

%% @private
handle_active_recv(State) ->
    Socket = State#state.socket,
    Length = maps:get(buffer, State#state.options, 0),
    WrappedSocket = {?GEN_TCP_MONIKER, self(), ?MODULE},
    %% CAUTION: internal API
    case socket:nif_recv(Socket, Length) of
        {ok, Data} ->
            ?LOG_INFO("Received data len=~p", [erlang:byte_size(Data)]),
            BinaryOrList = maybe_encode_binary(State#state.options, Data),
            ?LOG_INFO("Sending tcp message to controlling process ~p", [
                State#state.controlling_process
            ]),
            State#state.controlling_process ! {tcp, WrappedSocket, BinaryOrList},

            %% start a new select
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    ?LOG_INFO("Started read select on ref=~p", [Ref]),
                    PendingSelects = State#state.pending_selects,
                    NewPendingSelects = maps:remove(Ref, PendingSelects),
                    State#state{pending_selects = NewPendingSelects#{Ref => active}};
                Error ->
                    ?LOG_ERROR("Unable to start select on new ref=~p Error=~p", [Ref, Error]),
                    State
            end;
        {closed, OtherRef} ->
            ?LOG_INFO("Socket was closed other ref=~p", [OtherRef]),
            % socket was closed by another process
            % TODO: we need to handle:
            % (a) SELECT_STOP being scheduled
            % (b) flush of messages as we can have both
            % {closed, Ref} and {select, _, Ref, _} in the
            % queue
            State#state.controlling_process ! {tcp_closed, WrappedSocket},
            State;
        {error, closed} ->
            ?LOG_INFO("Socket was closed by peer"),
            %% CAUTION: internal API
            socket:nif_select_stop(Socket),
            %% peer has closed the connection
            WrappedSocket = {?GEN_TCP_MONIKER, self(), ?MODULE},
            State#state.controlling_process ! {tcp_closed, WrappedSocket},
            State;
        {error, _} = Error ->
            ?LOG_ERROR("Error on receive on pending select Error=~p", [Error]),
            %% CAUTION: internal API
            socket:nif_select_stop(Socket),
            %% TODO is this right?  I don't think active receivers would know
            %% what to do with this message.  Maybe log it instead?
            State#state.controlling_process ! {tcp_error, WrappedSocket, Error},
            State
    end.

%% @private
handle_passive_recv(State, From, Length, _Timeout) ->
    Socket = State#state.socket,
    %% CAUTION: internal API
    case socket:nif_recv(Socket, Length) of
        {ok, Data} ->
            ?LOG_INFO("Received data len=~p", [erlang:byte_size(Data)]),
            BinaryOrList = maybe_encode_binary(State#state.options, Data),
            gen_server:reply(From, {ok, BinaryOrList}),
            State;
        {closed, OtherRef} ->
            ?LOG_INFO("Socket was closed other ref=~p", [OtherRef]),
            % socket was closed by another process
            % TODO: we need to handle:
            % (a) SELECT_STOP being scheduled
            % (b) flush of messages as we can have both
            % {closed, Ref} and {select, _, Ref, _} in the
            % queue
            gen_server:reply(From, {error, closed}),
            State;
        {error, _} = Error ->
            ?LOG_ERROR("Error on receive from socket:nif_recv Error=~p", [Error]),
            socket:nif_select_stop(Socket),
            ?LOG_INFO("unable to receive on pending select~n"),
            gen_server:reply(From, Error),
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
