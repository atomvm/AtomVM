%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc Distribution controller for serial (UART) connections.
%%
%% This module manages the serial link for the Erlang distribution protocol.
%% It is used by {@link serial_dist} and follows the same pattern as
%% {@link socket_dist_controller}.
%%
%% All packets on the wire use the following frame format:
%%
%% ```
%% <<16#AA, 16#55, Length:LenBits, Payload:Length/binary, CRC32:32>>
%% '''
%%
%% where `LenBits' is 16 during the handshake phase and 32 during the
%% data phase. CRC32 covers the `Length' and `Payload' bytes.
%%
%% The receiver scans for the `<<16#AA, 16#55>>' sync marker, reads the
%% length, validates against a maximum frame size (to reject false sync
%% matches), then verifies the CRC32. On CRC failure, the connection is
%% torn down (no retry/ACK mechanism).
%%
%% During the handshake phase, `send/2' and `recv/3' are called
%% synchronously.
%%
%% After `handshake_complete/3', the controller switches to asynchronous
%% mode: a dedicated reader process continuously reads from the UART and
%% forwards data to the controller, which reassembles framed distribution
%% packets and feeds them to `erlang:dist_ctrl_put_data/2'.
%% @end
%%-----------------------------------------------------------------------------
-module(serial_dist_controller).

-include_lib("kernel/include/net_address.hrl").

% BEAM's dist_util expects packets to be list of integers.
-ifdef(BEAM_INTERFACE).
-define(POST_PROCESS(Packet), binary_to_list(Packet)).
-else.
-define(POST_PROCESS(Packet), Packet).
-endif.

-define(SYNC_MAGIC, <<16#AA, 16#55>>).
-define(MAX_HANDSHAKE_FRAME_SIZE, 8192).
-define(MAX_DATA_FRAME_SIZE, 16#100000).
-define(PREAMBLE_COUNT, 16).

% interface with serial_dist
-export([
    scan_frame/2,
    send_preamble/2,
    start/2,
    start/3,
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

-record(state, {
    uart :: pid() | port(),
    uart_module :: module(),
    dhandle :: reference() | undefined,
    buffer :: binary(),
    received :: non_neg_integer(),
    sent :: non_neg_integer(),
    reader :: pid() | undefined
}).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

%% @doc Start a controller for an outgoing (setup) connection.
start(Uart, UartMod) ->
    gen_server:start(?MODULE, {Uart, UartMod, <<>>}, []).

%% @doc Start a controller for an incoming (accept) connection with
%% initial data already read from the UART by the accept loop.
start(Uart, UartMod, InitialData) when is_binary(InitialData) ->
    gen_server:start(?MODULE, {Uart, UartMod, InitialData}, []).

supervisor(Controller, Pid) ->
    gen_server:call(Controller, {supervisor, Pid}).

%% @doc Synchronous receive during handshake. Reads one complete
%% framed packet (with 16-bit length field) from UART.
recv(Controller, Length, Timeout) ->
    gen_server:call(Controller, {recv, Length, Timeout}, infinity).

%% @doc Synchronous send during handshake. Wraps data in a framed
%% packet with 16-bit length field.
send(Controller, Data) ->
    gen_server:call(Controller, {send, Data}).

setopts_pre_nodeup(_Controller) ->
    ok.

setopts_post_nodeup(_Controller) ->
    ok.

getll(Controller) ->
    {ok, Controller}.

address(_Controller, Node) ->
    case string:split(atom_to_list(Node), "@") of
        [_Name, Host] ->
            #net_address{address = serial, host = Host, protocol = serial, family = serial};
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

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({Uart, UartMod, InitialBuffer}) ->
    {ok, #state{
        uart = Uart,
        uart_module = UartMod,
        buffer = InitialBuffer,
        received = 0,
        sent = 0
    }}.

handle_call({supervisor, Pid}, _From, State) ->
    Result = link(Pid),
    {reply, Result, State};
handle_call(
    {recv, _Length, Timeout},
    _From,
    #state{uart = Uart, uart_module = UartMod, buffer = Buffer} = State
) ->
    case recv_handshake_packet(Uart, UartMod, Buffer, Timeout) of
        {ok, Packet, NewBuffer} ->
            {reply, {ok, ?POST_PROCESS(Packet)}, State#state{buffer = NewBuffer}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call(
    {send, Data},
    _From,
    #state{uart = Uart, uart_module = UartMod, sent = Sent} = State
) ->
    DataBin = iolist_to_binary(Data),
    DataSize = byte_size(DataBin),
    Result = send_framed(UartMod, Uart, <<DataSize:16, DataBin/binary>>),
    {reply, Result, State#state{sent = Sent + 1}};
handle_call(getstat, _From, #state{received = Received, sent = Sent} = State) ->
    {reply, {ok, Received, Sent, 0}, State}.

handle_cast(tick, #state{uart = Uart, uart_module = UartMod, sent = Sent} = State) ->
    case send_framed(UartMod, Uart, <<0:32>>) of
        ok ->
            {noreply, State#state{sent = Sent + 1}};
        {error, Reason} ->
            {stop, {serial_write_error, Reason}, State}
    end;
handle_cast(
    {handshake_complete, DHandle},
    #state{uart = Uart, uart_module = UartMod, buffer = Buffer, received = Received} = State0
) ->
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    % Process any data left over in the buffer from the handshake phase.
    % After the handshake, packets use a 4-byte length prefix.
    {NewBuffer, NewReceived} = process_recv_buffer(DHandle, Buffer, Received),
    % Spawn a dedicated reader that blocks on uart:read/1 and
    % forwards chunks to this gen_server.
    Self = self(),
    Reader = spawn_link(fun() -> reader_loop(Self, Uart, UartMod) end),

    {noreply, State0#state{
        dhandle = DHandle,
        buffer = NewBuffer,
        received = NewReceived,
        reader = Reader
    }}.

handle_info(dist_data, State0) ->
    State1 = send_data_loop(State0),
    {noreply, State1};
handle_info(
    {serial_data, Data},
    #state{dhandle = DHandle, buffer = Buffer, received = Received} = State
) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    {NewBuffer2, NewReceived} = process_recv_buffer(DHandle, NewBuffer, Received),

    {noreply, State#state{buffer = NewBuffer2, received = NewReceived}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Handshake-phase packet I/O
%%--------------------------------------------------------------------

recv_handshake_packet(Uart, UartMod, Buffer, Timeout) ->
    case scan_frame(Buffer, 16) of
        {ok, Packet, Rest} ->
            {ok, Packet, Rest};
        {crc_error, _Rest} ->
            {error, crc_error};
        {need_more, Trimmed} ->
            case UartMod:read(Uart, Timeout) of
                {ok, Data} ->
                    recv_handshake_packet(
                        Uart, UartMod, <<Trimmed/binary, Data/binary>>, Timeout
                    );
                {error, _} = Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% Async reader process
%%--------------------------------------------------------------------

reader_loop(Controller, Uart, UartMod) ->
    case UartMod:read(Uart) of
        {ok, Data} ->
            Controller ! {serial_data, Data},
            reader_loop(Controller, Uart, UartMod);
        {error, Reason} ->
            exit({serial_read_error, Reason})
    end.

%%--------------------------------------------------------------------
%% Data-phase packet I/O (4-byte length prefix)
%%--------------------------------------------------------------------

process_recv_buffer(DHandle, Buffer, Received) ->
    case scan_frame(Buffer, 32) of
        {ok, Packet, Rest} ->
            case Packet of
                <<>> -> ok;
                _ -> ok = erlang:dist_ctrl_put_data(DHandle, Packet)
            end,
            process_recv_buffer(DHandle, Rest, Received + 1);
        {crc_error, _Rest} ->
            exit({serial_dist, crc_error});
        {need_more, Trimmed} ->
            {Trimmed, Received}
    end.

send_data_loop(#state{dhandle = DHandle, uart = Uart, uart_module = UartMod, sent = Sent} = State) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            ok = erlang:dist_ctrl_get_data_notification(DHandle),
            State;
        Data ->
            DataBin = iolist_to_binary(Data),
            DataSize = byte_size(DataBin),
            case send_framed(UartMod, Uart, <<DataSize:32, DataBin/binary>>) of
                ok ->
                    send_data_loop(State#state{sent = Sent + 1});
                {error, Reason} ->
                    exit({serial_write_error, Reason})
            end
    end.

%%--------------------------------------------------------------------
%% Frame encoding / decoding
%%--------------------------------------------------------------------

send_framed(UartMod, Uart, LenAndPayload) ->
    CRC = erlang:crc32(LenAndPayload),
    UartMod:write(Uart, <<16#AA, 16#55, LenAndPayload/binary, CRC:32>>).

%% @doc Scan a buffer for a framed packet.
%%
%% `LenBits' is 16 (handshake) or 32 (data phase).
%% Returns:
%%   `{ok, Payload, Rest}' - a complete, CRC-verified frame was found
%%   `{need_more, TrimmedBuffer}' - no complete frame yet
%%   `{crc_error, Rest}' - a frame was found but CRC did not match
-spec scan_frame(binary(), 16 | 32) ->
    {ok, binary(), binary()} | {need_more, binary()} | {crc_error, binary()}.
scan_frame(Buffer, LenBits) ->
    scan_frame(Buffer, LenBits, 0).

scan_frame(Buffer, LenBits, StartPos) ->
    MaxSize =
        case LenBits of
            16 -> ?MAX_HANDSHAKE_FRAME_SIZE;
            32 -> ?MAX_DATA_FRAME_SIZE
        end,
    LenBytes = LenBits div 8,
    BufSize = byte_size(Buffer),
    SearchLen = BufSize - StartPos,
    case SearchLen < 2 of
        true ->
            %% Not enough bytes to find sync; keep trailing 0xAA if present
            {need_more, keep_trailing_sync(Buffer, StartPos)};
        false ->
            case binary:match(Buffer, ?SYNC_MAGIC, [{scope, {StartPos, SearchLen}}]) of
                nomatch ->
                    {need_more, keep_trailing_sync(Buffer, StartPos)};
                {Pos, 2} ->
                    AfterSync = Pos + 2,
                    Remaining = BufSize - AfterSync,
                    case Remaining < LenBytes of
                        true ->
                            %% Have sync but not enough for length field
                            <<_:Pos/binary, Kept/binary>> = Buffer,
                            {need_more, Kept};
                        false ->
                            <<_:AfterSync/binary, Len:LenBits, _/binary>> = Buffer,
                            case Len > MaxSize of
                                true ->
                                    %% Length too large -- false sync, skip past it
                                    scan_frame(Buffer, LenBits, Pos + 1);
                                false ->
                                    FrameEnd = AfterSync + LenBytes + Len + 4,
                                    case BufSize < FrameEnd of
                                        true ->
                                            <<_:Pos/binary, Kept/binary>> = Buffer,
                                            {need_more, Kept};
                                        false ->
                                            {_, AfterSyncBin} = split_binary(Buffer, AfterSync),
                                            {LenAndPayloadBin, CrcAndRest} = split_binary(
                                                AfterSyncBin, LenBytes + Len
                                            ),
                                            <<WireCRC:32, Rest/binary>> = CrcAndRest,
                                            case erlang:crc32(LenAndPayloadBin) of
                                                WireCRC ->
                                                    {_, Payload} = split_binary(
                                                        LenAndPayloadBin, LenBytes
                                                    ),
                                                    {ok, Payload, Rest};
                                                _ ->
                                                    {crc_error, Rest}
                                            end
                                    end
                            end
                    end
            end
    end.

%% Keep a trailing 16#AA byte that might be the start of a sync marker.
keep_trailing_sync(Buffer, SearchFrom) ->
    BufSize = byte_size(Buffer),
    case BufSize > SearchFrom of
        true ->
            SkipBytes = BufSize - 1,
            case Buffer of
                <<_:SkipBytes/binary, 16#AA>> -> <<16#AA>>;
                _ -> <<>>
            end;
        false ->
            <<>>
    end.

send_preamble(UartMod, Uart) ->
    UartMod:write(Uart, binary:copy(?SYNC_MAGIC, ?PREAMBLE_COUNT)).
