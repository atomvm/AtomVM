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

-module(test_serial_dist).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    ok = test_mock_uart_roundtrip(),
    ok = test_mock_uart_read_timeout(),
    ok = test_mock_uart_bidirectional(),
    ok = test_mock_uart_buffering(),
    ok = test_mock_uart_concurrent_write_during_read(),
    ok = test_scan_frame_valid_at_start(),
    ok = test_scan_frame_valid_after_garbage(),
    ok = test_scan_frame_false_sync_large_length(),
    ok = test_scan_frame_crc_error(),
    ok = test_scan_frame_empty_buffer(),
    ok = test_scan_frame_only_sync_markers(),
    ok = test_scan_frame_trailing_aa(),
    ok = test_scan_frame_partial_length(),
    ok = test_scan_frame_partial_payload(),
    ok = test_scan_frame_empty_payload(),
    ok = test_scan_frame_32bit_mode(),
    ok = test_scan_frame_multiple_frames(),
    ok = test_scan_frame_max_handshake_boundary(),
    case erlang:system_info(machine) of
        "BEAM" ->
            %% Controller tests use mock_uart_hal which requires AtomVM
            io:format("test_serial_dist: skipping controller tests on BEAM~n"),
            ok;
        _ ->
            ok = test_controller_handshake_send_framing(),
            ok = test_controller_handshake_recv(),
            ok = test_controller_handshake_recv_fragmented(),
            ok = test_controller_roundtrip(),
            ok = test_controller_tick(),
            ok = test_controller_getstat(),
            ok = test_controller_initial_data(),
            ok
    end.

%%--------------------------------------------------------------------
%% Mock UART HAL tests
%%--------------------------------------------------------------------

test_mock_uart_roundtrip() ->
    {A, B} = mock_uart_hal:create_pair(),
    ok = mock_uart_hal:write(A, <<"hello">>),
    {ok, <<"hello">>} = mock_uart_hal:read(B),
    ok = mock_uart_hal:write(B, <<"world">>),
    {ok, <<"world">>} = mock_uart_hal:read(A),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_mock_uart_read_timeout() ->
    {A, B} = mock_uart_hal:create_pair(),
    {error, timeout} = mock_uart_hal:read(B, 100),
    %% Verify the endpoint is still functional after a timeout
    ok = mock_uart_hal:write(A, <<"after_timeout">>),
    {ok, <<"after_timeout">>} = mock_uart_hal:read(B, 1000),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_mock_uart_bidirectional() ->
    {A, B} = mock_uart_hal:create_pair(),
    ok = mock_uart_hal:write(A, <<"from_a">>),
    ok = mock_uart_hal:write(B, <<"from_b">>),
    {ok, <<"from_b">>} = mock_uart_hal:read(A),
    {ok, <<"from_a">>} = mock_uart_hal:read(B),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_mock_uart_buffering() ->
    {A, B} = mock_uart_hal:create_pair(),
    %% Multiple writes before a read should concatenate
    ok = mock_uart_hal:write(A, <<"hel">>),
    ok = mock_uart_hal:write(A, <<"lo">>),
    %% Small delay to let both messages be processed by B's gen_server
    receive
    after 50 -> ok
    end,
    {ok, <<"hello">>} = mock_uart_hal:read(B),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_mock_uart_concurrent_write_during_read() ->
    %% Verify that a write completes while a read is pending on the peer.
    %% This is critical: serial_dist_controller's reader_loop blocks on
    %% read/1 while the gen_server must still accept write/2 calls.
    {A, B} = mock_uart_hal:create_pair(),
    Parent = self(),
    %% Start a read that will block (no data yet)
    spawn_link(fun() ->
        {ok, Data} = mock_uart_hal:read(B),
        Parent ! {read_result, Data}
    end),
    %% Give the reader time to block
    receive
    after 50 -> ok
    end,
    %% Write should succeed even though a read is pending on the peer
    ok = mock_uart_hal:write(A, <<"concurrent">>),
    receive
        {read_result, <<"concurrent">>} -> ok
    after 5000 ->
        exit(concurrent_write_timeout)
    end,
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

%%--------------------------------------------------------------------
%% scan_frame unit tests
%%--------------------------------------------------------------------

test_scan_frame_valid_at_start() ->
    %% Complete valid frame at the start of buffer
    Frame = make_handshake_frame(<<"hello">>),
    {ok, <<"hello">>, <<>>} = serial_dist_controller:scan_frame(Frame, 16),
    ok.

test_scan_frame_valid_after_garbage() ->
    %% Valid frame preceded by garbage bytes
    Frame = make_handshake_frame(<<"data">>),
    Buffer = <<1, 2, 3, 4, Frame/binary>>,
    {ok, <<"data">>, <<>>} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_false_sync_large_length() ->
    %% False sync marker followed by a length that exceeds max frame size
    %% should be skipped; the real frame follows.
    RealFrame = make_handshake_frame(<<"ok">>),
    %% 16#FF00 > MAX_HANDSHAKE_FRAME_SIZE (8192), so this is a false sync
    FalseSync = <<16#AA, 16#55, 16#FF, 16#00>>,
    Buffer = <<FalseSync/binary, RealFrame/binary>>,
    {ok, <<"ok">>, <<>>} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_crc_error() ->
    %% Valid frame structure but corrupted payload -> CRC mismatch
    Payload = <<"test">>,
    Len = byte_size(Payload),
    LenAndPayload = <<Len:16, Payload/binary>>,
    BadCRC = erlang:crc32(LenAndPayload) bxor 16#FFFFFFFF,
    Buffer = <<16#AA, 16#55, LenAndPayload/binary, BadCRC:32>>,
    {crc_error, <<>>} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_empty_buffer() ->
    {need_more, <<>>} = serial_dist_controller:scan_frame(<<>>, 16),
    ok.

test_scan_frame_only_sync_markers() ->
    %% Buffer containing only sync markers -- these are idle sync frames
    %% from the link manager, not real packets.
    Buffer = <<16#AA, 16#55, 16#AA, 16#55>>,
    %% The scanner will find sync at pos 0, read the 2-byte length
    %% field as <<16#AA, 16#55>> = 43605 which exceeds max handshake
    %% frame size, so it skips. Then finds sync at pos 2, but not
    %% enough bytes for a length field.
    {need_more, <<16#AA, 16#55>>} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_trailing_aa() ->
    %% A single trailing 0xAA that might be the start of a sync marker
    %% should be kept in the buffer for the next read.
    {need_more, <<16#AA>>} = serial_dist_controller:scan_frame(<<16#AA>>, 16),
    ok.

test_scan_frame_partial_length() ->
    %% Sync marker present but not enough bytes for the length field
    Buffer = <<16#AA, 16#55, 16#00>>,
    {need_more, <<16#AA, 16#55, 16#00>>} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_partial_payload() ->
    %% Sync + length present but payload is incomplete
    Buffer = <<16#AA, 16#55, 5:16, "hel">>,
    %% Length = 5, but only 3 bytes of payload present (no CRC either)
    {need_more, <<16#AA, 16#55, 5:16, "hel">>} =
        serial_dist_controller:scan_frame(Buffer, 16),
    ok.

test_scan_frame_empty_payload() ->
    %% Frame with zero-length payload (used for handshake edge cases)
    Frame = make_handshake_frame(<<>>),
    {ok, <<>>, <<>>} = serial_dist_controller:scan_frame(Frame, 16),
    ok.

test_scan_frame_32bit_mode() ->
    %% Data-phase frame with 32-bit length prefix
    Payload = <<"data_phase">>,
    Len = byte_size(Payload),
    LenAndPayload = <<Len:32, Payload/binary>>,
    CRC = erlang:crc32(LenAndPayload),
    Frame = <<16#AA, 16#55, LenAndPayload/binary, CRC:32>>,
    {ok, <<"data_phase">>, <<>>} = serial_dist_controller:scan_frame(Frame, 32),
    ok.

test_scan_frame_multiple_frames() ->
    %% Two complete frames concatenated; scan_frame returns the first
    %% and the rest contains the second.
    Frame1 = make_handshake_frame(<<"first">>),
    Frame2 = make_handshake_frame(<<"second">>),
    Buffer = <<Frame1/binary, Frame2/binary>>,
    {ok, <<"first">>, Rest} = serial_dist_controller:scan_frame(Buffer, 16),
    {ok, <<"second">>, <<>>} = serial_dist_controller:scan_frame(Rest, 16),
    ok.

test_scan_frame_max_handshake_boundary() ->
    %% Frame with length exactly at MAX_HANDSHAKE_FRAME_SIZE (8192) should
    %% be accepted; length at 8193 should be treated as false sync.
    %% We don't actually build an 8192-byte payload (too large for test),
    %% but we can verify that a length of 8193 is rejected as false sync.
    LenAndPayload = <<8193:16, 0>>,
    CRC = erlang:crc32(LenAndPayload),
    Buffer = <<16#AA, 16#55, LenAndPayload/binary, CRC:32>>,
    %% 8193 > 8192 so this sync is treated as false; scanner looks for
    %% another sync marker and finds none.
    {need_more, _} = serial_dist_controller:scan_frame(Buffer, 16),
    ok.

%%--------------------------------------------------------------------
%% Controller handshake-phase tests
%%--------------------------------------------------------------------

test_controller_handshake_send_framing() ->
    %% Verify that send/2 produces a framed packet:
    %% <<16#AA, 16#55, Len:16, Payload, CRC32:32>>
    {A, B} = mock_uart_hal:create_pair(),
    {ok, Ctrl} = serial_dist_controller:start(A, mock_uart_hal),
    ok = serial_dist_controller:send(Ctrl, <<"hello">>),
    {ok, Raw} = mock_uart_hal:read(B),
    Expected = make_handshake_frame(<<"hello">>),
    Expected = Raw,
    %% Test with empty payload
    ok = serial_dist_controller:send(Ctrl, <<>>),
    {ok, Raw2} = mock_uart_hal:read(B),
    Expected2 = make_handshake_frame(<<>>),
    Expected2 = Raw2,
    stop_controller(Ctrl),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_handshake_recv() ->
    %% Verify that recv/3 correctly parses a framed handshake packet
    {A, B} = mock_uart_hal:create_pair(),
    {ok, Ctrl} = serial_dist_controller:start(A, mock_uart_hal),
    %% Write a properly framed handshake packet to the peer side
    ok = mock_uart_hal:write(B, make_handshake_frame(<<"hello">>)),
    {ok, Packet} = serial_dist_controller:recv(Ctrl, 0, 5000),
    <<"hello">> = iolist_to_binary(Packet),
    stop_controller(Ctrl),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_handshake_recv_fragmented() ->
    %% Verify that recv reassembles packets arriving in fragments.
    %% This is the most common scenario on real UART links, especially
    %% at low baud rates where OS/driver may deliver partial chunks.
    {A, B} = mock_uart_hal:create_pair(),
    {ok, Ctrl} = serial_dist_controller:start(A, mock_uart_hal),
    Parent = self(),
    Frame = make_handshake_frame(<<"hello">>),
    %% Split the frame into 3 chunks
    ChunkSize1 = 4,
    ChunkSize2 = 3,
    <<Chunk1:ChunkSize1/binary, Chunk2:ChunkSize2/binary, Chunk3/binary>> = Frame,
    %% Start recv in a separate process (it will block)
    spawn_link(fun() ->
        {ok, Packet} = serial_dist_controller:recv(Ctrl, 0, 10000),
        Parent ! {recv_result, iolist_to_binary(Packet)}
    end),
    %% Send first chunk (sync magic + partial length)
    receive
    after 50 -> ok
    end,
    ok = mock_uart_hal:write(B, Chunk1),
    %% Send second chunk (rest of length + partial payload)
    receive
    after 50 -> ok
    end,
    ok = mock_uart_hal:write(B, Chunk2),
    %% Send third chunk (rest of payload + CRC)
    receive
    after 50 -> ok
    end,
    ok = mock_uart_hal:write(B, Chunk3),
    receive
        {recv_result, <<"hello">>} -> ok
    after 10000 ->
        exit(fragmented_recv_timeout)
    end,
    stop_controller(Ctrl),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_roundtrip() ->
    %% Two controllers connected via mock UART: send from one, recv on other
    {A, B} = mock_uart_hal:create_pair(),
    {ok, CtrlA} = serial_dist_controller:start(A, mock_uart_hal),
    {ok, CtrlB} = serial_dist_controller:start(B, mock_uart_hal),
    %% A sends, B receives
    ok = serial_dist_controller:send(CtrlA, <<"from_a">>),
    {ok, PacketB} = serial_dist_controller:recv(CtrlB, 0, 5000),
    <<"from_a">> = iolist_to_binary(PacketB),
    %% B sends, A receives
    ok = serial_dist_controller:send(CtrlB, <<"from_b">>),
    {ok, PacketA} = serial_dist_controller:recv(CtrlA, 0, 5000),
    <<"from_b">> = iolist_to_binary(PacketA),
    %% Multiple packets in sequence
    ok = serial_dist_controller:send(CtrlA, <<"msg1">>),
    ok = serial_dist_controller:send(CtrlA, <<"msg2">>),
    {ok, P1} = serial_dist_controller:recv(CtrlB, 0, 5000),
    <<"msg1">> = iolist_to_binary(P1),
    {ok, P2} = serial_dist_controller:recv(CtrlB, 0, 5000),
    <<"msg2">> = iolist_to_binary(P2),
    stop_controller(CtrlA),
    stop_controller(CtrlB),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_tick() ->
    %% Verify tick sends a framed 4-byte zero (keepalive)
    {A, B} = mock_uart_hal:create_pair(),
    {ok, Ctrl} = serial_dist_controller:start(A, mock_uart_hal),
    ok = serial_dist_controller:tick(Ctrl),
    %% tick is a cast, give it a moment to process
    receive
    after 50 -> ok
    end,
    {ok, Raw} = mock_uart_hal:read(B, 1000),
    Expected = make_frame(<<0:32>>),
    Expected = Raw,
    stop_controller(Ctrl),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_getstat() ->
    %% Verify sent/received counters
    {A, B} = mock_uart_hal:create_pair(),
    {ok, CtrlA} = serial_dist_controller:start(A, mock_uart_hal),
    {ok, CtrlB} = serial_dist_controller:start(B, mock_uart_hal),
    %% Initial stats: 0 sent, 0 received
    {ok, 0, 0, 0} = serial_dist_controller:getstat(CtrlA),
    %% Send two packets
    ok = serial_dist_controller:send(CtrlA, <<"msg1">>),
    ok = serial_dist_controller:send(CtrlA, <<"msg2">>),
    {ok, 0, 2, 0} = serial_dist_controller:getstat(CtrlA),
    %% Tick also increments sent counter
    ok = serial_dist_controller:tick(CtrlA),
    receive
    after 50 -> ok
    end,
    {ok, 0, 3, 0} = serial_dist_controller:getstat(CtrlA),
    %% Drain the packets on B side (recv doesn't increment received
    %% counter — that only happens in data phase via process_recv_buffer)
    {ok, _} = serial_dist_controller:recv(CtrlB, 0, 1000),
    {ok, _} = serial_dist_controller:recv(CtrlB, 0, 1000),
    stop_controller(CtrlA),
    stop_controller(CtrlB),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

test_controller_initial_data() ->
    %% Verify that initial data passed to start/3 is used for the first recv.
    %% This simulates the accept path where the link manager reads the first
    %% chunk from UART and passes it to the controller.
    {A, B} = mock_uart_hal:create_pair(),
    InitialData = make_handshake_frame(<<"hello">>),
    {ok, Ctrl} = serial_dist_controller:start(A, mock_uart_hal, InitialData),
    %% recv should return the packet from initial data without reading UART
    {ok, Packet} = serial_dist_controller:recv(Ctrl, 0, 1000),
    <<"hello">> = iolist_to_binary(Packet),
    %% Subsequent recv should read from UART as normal
    ok = mock_uart_hal:write(B, make_handshake_frame(<<"abc">>)),
    {ok, Packet2} = serial_dist_controller:recv(Ctrl, 0, 5000),
    <<"abc">> = iolist_to_binary(Packet2),
    stop_controller(Ctrl),
    mock_uart_hal:close(A),
    mock_uart_hal:close(B),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Build a framed packet as the controller would send it on the wire.
%% Format: <<16#AA, 16#55, LenAndPayload/binary, CRC32:32>>
make_frame(LenAndPayload) ->
    CRC = erlang:crc32(LenAndPayload),
    <<16#AA, 16#55, LenAndPayload/binary, CRC:32>>.

%% Build a handshake frame (16-bit length prefix).
make_handshake_frame(Payload) ->
    Len = byte_size(Payload),
    make_frame(<<Len:16, Payload/binary>>).

stop_controller(Ctrl) ->
    unlink(Ctrl),
    exit(Ctrl, shutdown),
    receive
    after 10 -> ok
    end.
