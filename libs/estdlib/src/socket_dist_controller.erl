%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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
-module(socket_dist_controller).

% Required include headers
-include_lib("kernel/include/net_address.hrl").

% BEAM's dist_util expect packets to be list of integers.
-ifdef(BEAM_INTERFACE).
-define(POST_PROCESS(Packet), binary_to_list(Packet)).
-define(PRE_PROCESS(Packet), iolist_to_binary(Packet)).
-else.
-define(POST_PROCESS(Packet), Packet).
-define(PRE_PROCESS(Packet), Packet).
-endif.

% interface with socket_dist
-export([
    start/1,
    supervisor/2,
    recv/3,
    send/2,
    setopts_pre_nodeup/1,
    setopts_post_nodeup/1,
    getll/1,
    address/2,
    tick/1,
    getstat/1,
    handshake_complete/3
]).

% gen_server API
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start(Socket) ->
    gen_server:start(?MODULE, Socket, []).

supervisor(Controller, Pid) ->
    gen_server:call(Controller, {supervisor, Pid}).

recv(Controller, Length, Timeout) ->
    Socket = gen_server:call(Controller, socket),
    Result =
        case socket:recv(Socket, 2 + Length, Timeout) of
            {ok, <<Len:16, Tail/binary>>} ->
                Remaining = Len - byte_size(Tail),
                case Remaining of
                    0 ->
                        {ok, Tail};
                    _ ->
                        case socket:recv(Socket, Remaining, Timeout) of
                            {ok, Packet} ->
                                {ok, ?POST_PROCESS(<<Tail/binary, Packet/binary>>)};
                            {error, _} = Err0 ->
                                Err0
                        end
                end;
            {error, _} = Err1 ->
                Err1
        end,
    Result.

send(Controller, Data) ->
    Socket = gen_server:call(Controller, socket),
    DataBin = iolist_to_binary(Data),
    DataSize = byte_size(DataBin),
    socket:send(Socket, <<DataSize:16, DataBin/binary>>).

setopts_pre_nodeup(_Controller) ->
    ok.

setopts_post_nodeup(_Controller) ->
    ok.

getll(Controller) ->
    {ok, Controller}.

address(Controller, Node) ->
    Socket = gen_server:call(Controller, socket),
    {ok, #{addr := Addr, port := Port}} = socket:peername(Socket),
    case string:split(atom_to_list(Node), "@") of
        [_Name, Host] ->
            #net_address{address = {Addr, Port}, host = Host, protocol = tcp, family = inet};
        _ ->
            {error, no_node}
    end.

tick(Controller) ->
    gen_server:cast(Controller, tick).

getstat(Controller) ->
    gen_server:call(Controller, getstat).

handshake_complete(Controller, _Node, DHandle) ->
    gen_server:cast(Controller, {handshake_complete, DHandle}),
    ok.

-record(state, {
    socket :: socket:socket(),
    dhandle :: reference() | undefined,
    select_handle :: reference() | undefined,
    write_select_handle :: reference() | undefined,
    pending_output = [] :: [{binary(), non_neg_integer()}],
    buffer :: binary(),
    received :: non_neg_integer(),
    sent :: non_neg_integer()
}).

init(Socket) ->
    {ok, #state{socket = Socket, buffer = <<>>, received = 0, sent = 0}}.

handle_call({supervisor, Pid}, _From, #state{} = State0) ->
    Result = link(Pid),
    {reply, Result, State0};
handle_call(socket, _From, #state{socket = Socket} = State) ->
    {reply, Socket, State};
handle_call(getstat, _From, #state{received = Received, sent = Sent} = State) ->
    {reply, {ok, Received, Sent, 0}, State}.

handle_cast(tick, State) ->
    case queue_output(State, <<0:32>>) of
        {ok, State1} ->
            {noreply, State1};
        {stop, Reason, State1} ->
            {stop, Reason, State1}
    end;
handle_cast({handshake_complete, DHandle}, State0) ->
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    % We now need to get messages when data is coming.
    State1 = State0#state{dhandle = DHandle},
    case recv_data_loop(State1) of
        {ok, State2} -> {noreply, State2};
        {stop, Reason, State2} -> {stop, Reason, State2}
    end.

handle_info(dist_data, State0) ->
    case send_data_loop(State0) of
        {ok, State1} -> {noreply, State1};
        {stop, Reason, State1} -> {stop, Reason, State1}
    end;
handle_info(
    {'$socket', Socket, select, WriteSelectHandle},
    #state{socket = Socket, write_select_handle = WriteSelectHandle} = State0
) ->
    State1 = State0#state{write_select_handle = undefined},
    case flush_output(State1) of
        {ok, #state{pending_output = []} = State2} ->
            case send_data_loop(State2) of
                {ok, State3} -> {noreply, State3};
                {stop, Reason, State3} -> {stop, Reason, State3}
            end;
        {ok, State2} ->
            {noreply, State2};
        {stop, Reason, State2} ->
            {stop, Reason, State2}
    end;
handle_info(
    {'$socket', Socket, abort, {WriteSelectHandle, closed}},
    #state{socket = Socket, write_select_handle = WriteSelectHandle} = State
) ->
    {stop, {send_error, closed}, State};
handle_info(
    {'$socket', Socket, select, SelectHandle},
    #state{socket = Socket, select_handle = SelectHandle} = State0
) ->
    case recv_data_loop(State0) of
        {ok, State1} -> {noreply, State1};
        {stop, Reason, State1} -> {stop, Reason, State1}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

recv_data_loop(
    #state{dhandle = DHandle, socket = Socket, buffer = Buffer, received = Received} = State
) ->
    case socket:recv(Socket, 0, nowait) of
        {ok, Data} ->
            {NewBuffer, NewReceived} = process_recv_buffer(
                DHandle, <<Buffer/binary, Data/binary>>, Received
            ),
            recv_data_loop(State#state{buffer = NewBuffer, received = NewReceived});
        {select, {{select_info, recv, SelectHandle}, Data}} when is_reference(SelectHandle) ->
            {NewBuffer, NewReceived} = process_recv_buffer(
                DHandle, <<Buffer/binary, Data/binary>>, Received
            ),
            {ok, State#state{
                buffer = NewBuffer, select_handle = SelectHandle, received = NewReceived
            }};
        {select, {select_info, recv, SelectHandle}} when is_reference(SelectHandle) ->
            {ok, State#state{select_handle = SelectHandle}};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            {stop, {recv_error, Reason}, State}
    end.

process_recv_buffer(DHandle, <<Size:32, Rest/binary>>, Received) when byte_size(Rest) >= Size ->
    {Packet, Tail} = split_binary(Rest, Size),
    case Packet of
        <<>> -> ok;
        _ -> ok = erlang:dist_ctrl_put_data(DHandle, Packet)
    end,
    process_recv_buffer(DHandle, Tail, Received + 1);
process_recv_buffer(_DHandle, Other, Received) ->
    {Other, Received}.

send_data_loop(#state{pending_output = [_ | _]} = State) ->
    {ok, State};
send_data_loop(#state{dhandle = DHandle} = State) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            ok = erlang:dist_ctrl_get_data_notification(DHandle),
            {ok, State};
        Data ->
            DataBin = ?PRE_PROCESS(Data),
            DataSize = byte_size(DataBin),
            case queue_output(State, <<DataSize:32, DataBin/binary>>) of
                {ok, #state{pending_output = []} = State1} ->
                    send_data_loop(State1);
                {ok, State1} ->
                    {ok, State1};
                {stop, _Reason, _State1} = Stop ->
                    Stop
            end
    end.

queue_output(#state{pending_output = PendingOutput} = State, Data) ->
    flush_output(State#state{pending_output = PendingOutput ++ [{Data, 1}]}).

flush_output(#state{write_select_handle = Ref} = State) when is_reference(Ref) ->
    {ok, State};
flush_output(#state{pending_output = []} = State) ->
    {ok, State};
flush_output(
    #state{socket = Socket, pending_output = [{Data, Increment} | Tail], sent = Sent} = State
) ->
    case socket:nif_send(Socket, Data) of
        ok ->
            flush_output(State#state{pending_output = Tail, sent = Sent + Increment});
        {ok, Rest} ->
            wait_for_write(State#state{pending_output = [{Rest, Increment} | Tail]});
        {error, eagain} ->
            wait_for_write(State);
        {error, Reason} ->
            {stop, {send_error, Reason}, State}
    end.

wait_for_write(#state{socket = Socket} = State) ->
    Ref = erlang:make_ref(),
    case socket:nif_select_write(Socket, Ref) of
        ok ->
            {ok, State#state{write_select_handle = Ref}};
        {error, Reason} ->
            {stop, {send_error, Reason}, State}
    end.
