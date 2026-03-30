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

-module(test_uart).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            case has_socat() of
                true ->
                    case has_working_ptys() of
                        true ->
                            ok = test_posix_tcgetattr(),
                            ok = test_posix_tcsetattr_raw(),
                            ok = test_uart_roundtrip(),
                            ok = test_uart_read_timeout(),
                            ok = test_uart_bidirectional(),
                            ok = test_uart_large_payload(),
                            ok;
                        false ->
                            io:format(
                                "test_uart: ptys not functional (e.g. qemu-user), skipping~n"
                            ),
                            ok
                    end;
                false ->
                    io:format("test_uart: socat not found, skipping~n"),
                    ok
            end;
        _ ->
            io:format("test_uart: not supported on this platform, skipping~n"),
            ok
    end.

%%--------------------------------------------------------------------
%% socat pty pair management
%%--------------------------------------------------------------------

has_socat() ->
    try
        {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "command -v socat"], undefined, [
            stdout
        ]),
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

%% Verify that socat ptys actually support termios (fails under qemu-user)
has_working_ptys() ->
    {SocatFd, PtyA, _PtyB} = start_socat(),
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
    stop_socat(SocatFd),
    Result.

%% Start socat and return {SocatFd, PtyA, PtyB}
%% socat -d -d pty,raw,echo=0 pty,raw,echo=0
%% outputs on stderr: "N PTY is /dev/ttysXXX" twice
start_socat() ->
    {ok, _Pid, Fd} = atomvm:subprocess(
        "/bin/sh",
        ["sh", "-c", "socat -d -d pty,raw,echo=0 pty,raw,echo=0 2>&1"],
        undefined,
        [stdout]
    ),
    PtyA = read_pty_path(Fd),
    PtyB = read_pty_path(Fd),
    %% Give socat a moment to set up the data transfer loop
    receive
    after 200 -> ok
    end,
    {Fd, PtyA, PtyB}.

stop_socat(Fd) ->
    atomvm:posix_close(Fd).

%% Read a line like "... N PTY is /dev/ttysXXX" and extract the path
read_pty_path(Fd) ->
    read_pty_path(Fd, <<>>).

read_pty_path(Fd, Acc) ->
    case atomvm:posix_read(Fd, 1) of
        {ok, <<$\n>>} ->
            extract_pty(Acc);
        {ok, Byte} ->
            read_pty_path(Fd, <<Acc/binary, Byte/binary>>);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 5000 ->
                exit(socat_read_timeout)
            end,
            read_pty_path(Fd, Acc)
    end.

extract_pty(Line) ->
    %% Line: "2026/... socat[...] N PTY is /dev/ttysXXX"
    case binary:match(Line, <<"PTY is ">>) of
        {Pos, Len} ->
            binary:part(Line, Pos + Len, byte_size(Line) - Pos - Len);
        nomatch ->
            exit({unexpected_socat_output, Line})
    end.

%%--------------------------------------------------------------------
%% POSIX termios NIF tests (need a real tty fd)
%%--------------------------------------------------------------------

test_posix_tcgetattr() ->
    {SocatFd, PtyA, _PtyB} = start_socat(),
    {ok, Fd} = atomvm:posix_open(PtyA, [o_rdwr, o_noctty]),
    {ok, Tio} = atomvm:posix_tcgetattr(Fd),
    true = is_map(Tio),
    true = is_integer(maps:get(cflag, Tio)),
    true = is_integer(maps:get(iflag, Tio)),
    true = is_integer(maps:get(oflag, Tio)),
    true = is_integer(maps:get(lflag, Tio)),
    true = is_integer(maps:get(ispeed, Tio)),
    true = is_integer(maps:get(ospeed, Tio)),
    ok = atomvm:posix_close(Fd),
    stop_socat(SocatFd),
    ok.

test_posix_tcsetattr_raw() ->
    {SocatFd, PtyA, _PtyB} = start_socat(),
    {ok, Fd} = atomvm:posix_open(PtyA, [o_rdwr, o_noctty]),
    %% Set raw mode + speed
    ok = atomvm:posix_tcsetattr(Fd, tcsanow, #{
        raw => true,
        ispeed => 115200,
        ospeed => 115200
    }),
    %% Verify speed was set
    {ok, Tio} = atomvm:posix_tcgetattr(Fd),
    115200 = maps:get(ispeed, Tio),
    115200 = maps:get(ospeed, Tio),
    ok = atomvm:posix_close(Fd),
    stop_socat(SocatFd),
    ok.

%%--------------------------------------------------------------------
%% UART HAL tests over socat pty pair
%%--------------------------------------------------------------------

test_uart_roundtrip() ->
    {SocatFd, PtyA, PtyB} = start_socat(),
    UartA = uart:open([{peripheral, PtyA}, {speed, 115200}]),
    UartB = uart:open([{peripheral, PtyB}, {speed, 115200}]),
    ok = uart:write(UartA, <<"hello">>),
    {ok, <<"hello">>} = uart:read(UartB, 2000),
    ok = uart:write(UartB, <<"world">>),
    {ok, <<"world">>} = uart:read(UartA, 2000),
    uart:close(UartA),
    uart:close(UartB),
    stop_socat(SocatFd),
    ok.

test_uart_read_timeout() ->
    {SocatFd, PtyA, _PtyB} = start_socat(),
    UartA = uart:open([{peripheral, PtyA}, {speed, 115200}]),
    {error, timeout} = uart:read(UartA, 200),
    uart:close(UartA),
    stop_socat(SocatFd),
    ok.

test_uart_bidirectional() ->
    {SocatFd, PtyA, PtyB} = start_socat(),
    UartA = uart:open([{peripheral, PtyA}, {speed, 115200}]),
    UartB = uart:open([{peripheral, PtyB}, {speed, 115200}]),
    %% Send from both sides simultaneously
    ok = uart:write(UartA, <<"from_a">>),
    ok = uart:write(UartB, <<"from_b">>),
    {ok, <<"from_b">>} = uart:read(UartA, 2000),
    {ok, <<"from_a">>} = uart:read(UartB, 2000),
    uart:close(UartA),
    uart:close(UartB),
    stop_socat(SocatFd),
    ok.

test_uart_large_payload() ->
    {SocatFd, PtyA, PtyB} = start_socat(),
    UartA = uart:open([{peripheral, PtyA}, {speed, 115200}]),
    UartB = uart:open([{peripheral, PtyB}, {speed, 115200}]),
    %% Send a payload larger than a single read buffer (256 bytes)
    Payload = list_to_binary(lists:duplicate(500, $x)),
    ok = uart:write(UartA, Payload),
    %% May need multiple reads to get all data
    Received = read_all(UartB, byte_size(Payload), 5000, <<>>),
    Payload = Received,
    uart:close(UartA),
    uart:close(UartB),
    stop_socat(SocatFd),
    ok.

%% Read until we have ExpectedSize bytes or timeout
read_all(_Uart, ExpectedSize, _Timeout, Acc) when byte_size(Acc) >= ExpectedSize ->
    Acc;
read_all(Uart, ExpectedSize, Timeout, Acc) ->
    case uart:read(Uart, Timeout) of
        {ok, Data} ->
            read_all(Uart, ExpectedSize, Timeout, <<Acc/binary, Data/binary>>);
        {error, timeout} ->
            exit({read_all_timeout, expected, ExpectedSize, got, byte_size(Acc)})
    end.
