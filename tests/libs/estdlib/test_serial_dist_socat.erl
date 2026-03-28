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

%% @doc Tests for serial (UART) distribution over socat virtual serial ports.
%%
%% These tests create a socat pty pair, start a second AtomVM process as
%% the peer node, and verify distribution works over the serial link.
%%
%% Requires: socat, AtomVM binary in PATH or known location.

-module(test_serial_dist_socat).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            case has_socat() andalso has_working_ptys() of
                true ->
                    %% Pre-warm JIT compilation of modules used during
                    %% the distribution handshake so their compilation
                    %% doesn't eat into the handshake timer.
                    _ = crypto:module_info(),
                    _ = dist_util:module_info(),
                    _ = uart:module_info(),
                    _ = timer_manager:module_info(),
                    ok = test_ping_over_serial(),
                    ok = test_rpc_over_serial(),
                    ok = test_beam_ping_over_serial(),
                    ok = test_beam_rpc_over_serial(),
                    ok = test_multi_port_ping(),
                    ok;
                false ->
                    io:format("test_serial_dist_socat: socat/ptys not available, skipping~n"),
                    ok
            end;
        _ ->
            io:format("test_serial_dist_socat: skipping on BEAM~n"),
            ok
    end.

has_socat() ->
    try
        {ok, _, Fd} = atomvm:subprocess(
            "/bin/sh", ["sh", "-c", "command -v socat"], undefined, [stdout]
        ),
        Result =
            case atomvm:posix_read(Fd, 200) of
                eof -> false;
                {ok, _} -> true
            end,
        ok = atomvm:posix_close(Fd),
        Result
    catch
        _:_ -> false
    end.

%% Verify that socat ptys support termios (fails under qemu-user).
has_working_ptys() ->
    try
        {OsPid, SocatFd, PtyA, _PtyB} = start_socat(),
        Result =
            case atomvm:posix_open(PtyA, [o_rdwr, o_noctty]) of
                {ok, Fd} ->
                    case atomvm:posix_tcgetattr(Fd) of
                        {ok, _} ->
                            atomvm:posix_close(Fd),
                            true;
                        {error, _} ->
                            atomvm:posix_close(Fd),
                            false
                    end;
                {error, _} ->
                    false
            end,
        stop_socat(OsPid, SocatFd),
        Result
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% Test: ping over serial distribution
%%--------------------------------------------------------------------

test_ping_over_serial() -> run_serial_test(atomvm, "ping").
test_rpc_over_serial() -> run_serial_test(atomvm, "rpc").
test_beam_ping_over_serial() -> run_serial_test(beam, "ping").
test_beam_rpc_over_serial() -> run_serial_test(beam, "rpc").

run_serial_test(PeerType, TestName) ->
    {OsPid, SocatFd, PtyA, PtyB} = start_socat(),
    try
        {ok, _} = net_kernel:start('a@serial.local', #{
            name_domain => longnames,
            proto_dist => serial_dist,
            avm_dist_opts => #{
                uart_opts => [{peripheral, binary_to_list(PtyA)}, {speed, 115200}],
                uart_module => uart
            }
        }),
        erlang:set_cookie('SerialTest'),
        register(test_serial, self()),
        PeerFd = start_peer_node(PeerType, PtyB, TestName),
        try
            handle_peer_request(TestName, PeerFd),
            Result = read_peer_line(PeerFd),
            Expected = expected_result(TestName),
            io:format("~p ~s result: ~s~n", [PeerType, TestName, Result]),
            Expected = Result,
            ok
        after
            catch unregister(test_serial),
            atomvm:posix_close(PeerFd)
        end
    after
        catch net_kernel:stop(),
        stop_socat(OsPid, SocatFd)
    end.

start_peer_node(atomvm, PtyB, TestName) -> start_peer(PtyB, TestName);
start_peer_node(beam, PtyB, TestName) -> start_beam_peer(PtyB, TestName).

handle_peer_request("ping", PeerFd) ->
    receive
        {PeerPid, ping} -> PeerPid ! {self(), pong}
    after 30000 ->
        drain_peer_output(PeerFd),
        error(ping_timeout)
    end;
handle_peer_request("rpc", PeerFd) ->
    receive
        {PeerPid, {apply, M, F, A}} ->
            Result = apply(M, F, A),
            PeerPid ! {self(), Result}
    after 30000 ->
        drain_peer_output(PeerFd),
        error(rpc_timeout)
    end.

expected_result("ping") -> <<"pong">>;
expected_result("rpc") -> <<"ATOM">>.

%%--------------------------------------------------------------------
%% Test: multi-port ping (two peers via two UARTs)
%%--------------------------------------------------------------------

test_multi_port_ping() ->
    %% Create two socat pairs: one for peer B, one for peer C.
    %% Needs 4 PTYs; skip if the system cannot allocate them.
    {OsPid1, SocatFd1, PtyA1, PtyB} = start_socat(),
    try start_socat() of
        {OsPid2, SocatFd2, PtyA2, PtyC} ->
            test_multi_port_ping(OsPid1, SocatFd1, PtyA1, PtyB, OsPid2, SocatFd2, PtyA2, PtyC)
    catch
        _:_ ->
            stop_socat(OsPid1, SocatFd1),
            io:format("test_multi_port_ping: not enough PTYs, skipping~n"),
            ok
    end.

test_multi_port_ping(OsPid1, SocatFd1, PtyA1, PtyB, OsPid2, SocatFd2, PtyA2, PtyC) ->
    try
        %% Start node A with two UART ports
        {ok, _} = net_kernel:start('a@serial.local', #{
            name_domain => longnames,
            proto_dist => serial_dist,
            avm_dist_opts => #{
                uart_ports => [
                    [{peripheral, binary_to_list(PtyA1)}, {speed, 115200}],
                    [{peripheral, binary_to_list(PtyA2)}, {speed, 115200}]
                ],
                uart_module => uart
            }
        }),
        erlang:set_cookie('SerialTest'),
        register(test_serial, self()),
        %% Connect peers sequentially to avoid handshake timer
        %% contention from simultaneous JIT compilation.
        %% Peer B on first UART
        PeerFdB = start_peer_node(atomvm, PtyB, "ping"),
        handle_peer_request("ping", PeerFdB),
        ResultB = read_peer_line(PeerFdB),
        io:format("multi_port peer B result: ~s~n", [ResultB]),
        <<"pong">> = ResultB,
        %% Peer C on second UART (keep B alive to avoid disturbing A)
        PeerFdC = start_peer_node(atomvm, PtyC, "ping"),
        handle_peer_request("ping", PeerFdC),
        ResultC = read_peer_line(PeerFdC),
        io:format("multi_port peer C result: ~s~n", [ResultC]),
        <<"pong">> = ResultC,
        catch unregister(test_serial),
        ok
    after
        catch net_kernel:stop(),
        stop_socat(OsPid1, SocatFd1),
        stop_socat(OsPid2, SocatFd2)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_socat() ->
    {ok, OsPid, Fd} = atomvm:subprocess(
        "/bin/sh",
        ["sh", "-c", "socat -d -d pty,raw,echo=0 pty,raw,echo=0 2>&1"],
        undefined,
        [stdout]
    ),
    PtyA = extract_pty(read_line(Fd)),
    PtyB = extract_pty(read_line(Fd)),
    receive
    after 200 -> ok
    end,
    {OsPid, Fd, PtyA, PtyB}.

stop_socat(OsPid, Fd) ->
    atomvm:posix_close(Fd),
    {ok, _, KillFd} = atomvm:subprocess(
        "/bin/kill", ["kill", integer_to_list(OsPid)], undefined, [stdout]
    ),
    atomvm:posix_close(KillFd).

start_peer(PtyPath, TestName) ->
    AvmBin = find_atomvm_binary(),
    PeerAvm = find_peer_avm(),
    Cmd =
        "PTY_PATH=" ++ binary_to_list(PtyPath) ++
            " TEST_NAME=" ++ TestName ++
            " " ++ AvmBin ++ " " ++ PeerAvm ++ " 2>&1",
    io:format("test_serial_dist_socat: peer cmd: ~s~n", [Cmd]),
    {ok, _PeerPid, PeerFd} = atomvm:subprocess(
        "/bin/sh", ["sh", "-c", Cmd], undefined, [stdout]
    ),
    PeerFd.

start_beam_peer(PtyPath, TestName) ->
    BeamBeams = find_beam_beams(),
    TestBeams = find_test_beams(),
    Cmd =
        "PTY_PATH=" ++ binary_to_list(PtyPath) ++
            " TEST_NAME=" ++ TestName ++
            " erl -proto_dist serial -start_epmd false -kernel logger_level none" ++
            " -pa " ++ BeamBeams ++ " -pa " ++ TestBeams ++
            " -noshell -s test_serial_dist_beam_peer -s init stop 2>/dev/null",
    io:format("test_serial_dist_socat: BEAM peer cmd: ~s~n", [Cmd]),
    {ok, _PeerPid, PeerFd} = atomvm:subprocess(
        "/bin/sh", ["sh", "-c", Cmd], undefined, [stdout]
    ),
    PeerFd.

find_beam_beams() ->
    case atomvm:posix_stat("./tests/libs/estdlib/beam_beams") of
        {ok, _} -> "./tests/libs/estdlib/beam_beams";
        _ -> "tests/libs/estdlib/beam_beams"
    end.

find_test_beams() ->
    case atomvm:posix_stat("./tests/libs/estdlib/beams") of
        {ok, _} -> "./tests/libs/estdlib/beams";
        _ -> "tests/libs/estdlib/beams"
    end.

find_atomvm_binary() ->
    case atomvm:posix_stat("./src/AtomVM") of
        {ok, _} ->
            "./src/AtomVM";
        _ ->
            case atomvm:posix_stat("./AtomVM") of
                {ok, _} -> "./AtomVM";
                _ -> "AtomVM"
            end
    end.

find_peer_avm() ->
    case atomvm:posix_stat("./tests/libs/estdlib/test_serial_dist_socat_peer.avm") of
        {ok, _} -> "./tests/libs/estdlib/test_serial_dist_socat_peer.avm";
        _ -> "test_serial_dist_socat_peer.avm"
    end.

read_line(Fd) ->
    read_line(Fd, <<>>).

read_line(Fd, Acc) ->
    case atomvm:posix_read(Fd, 1) of
        {ok, <<$\n>>} ->
            Acc;
        {ok, Byte} ->
            read_line(Fd, <<Acc/binary, Byte/binary>>);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 5000 ->
                exit(socat_read_timeout)
            end,
            read_line(Fd, Acc)
    end.

read_peer_line(Fd) ->
    Line = read_peer_line(Fd, <<>>),
    %% Skip JIT compilation output, OTP reports, and other diagnostic lines
    case Line of
        <<"Compilation of ", _/binary>> -> read_peer_line(Fd);
        <<"+Compilation of ", _/binary>> -> read_peer_line(Fd);
        <<"Unable to open ", _/binary>> -> read_peer_line(Fd);
        <<"Failed load module", _/binary>> -> read_peer_line(Fd);
        <<"Warning", _/binary>> -> read_peer_line(Fd);
        <<"=", _/binary>> -> read_peer_line(Fd);
        <<" ", _/binary>> -> read_peer_line(Fd);
        <<>> -> read_peer_line(Fd);
        _ -> Line
    end.

read_peer_line(Fd, Acc) ->
    case atomvm:posix_read(Fd, 1) of
        {ok, <<$\n>>} ->
            Acc;
        {ok, <<$\r>>} ->
            read_peer_line(Fd, Acc);
        {ok, Byte} ->
            read_peer_line(Fd, <<Acc/binary, Byte/binary>>);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 30000 ->
                exit({peer_read_timeout, got_so_far, Acc})
            end,
            read_peer_line(Fd, Acc);
        eof ->
            Acc
    end.

drain_peer_output(Fd) ->
    case atomvm:posix_read(Fd, 4096) of
        {ok, Data} ->
            io:format("test_serial_dist_socat: peer output: ~s~n", [Data]),
            drain_peer_output(Fd);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} ->
                    case atomvm:posix_read(Fd, 4096) of
                        {ok, Data} ->
                            io:format("test_serial_dist_socat: peer output: ~s~n", [Data]);
                        _ ->
                            ok
                    end
            after 2000 ->
                io:format("test_serial_dist_socat: no more peer output~n")
            end;
        eof ->
            io:format("test_serial_dist_socat: peer EOF~n");
        {error, Reason} ->
            io:format("test_serial_dist_socat: peer read error: ~p~n", [Reason])
    end.

extract_pty(Line) ->
    case binary:match(Line, <<"PTY is ">>) of
        {Pos, Len} ->
            binary:part(Line, Pos + Len, byte_size(Line) - Pos - Len);
        nomatch ->
            exit({unexpected_socat_output, Line})
    end.
