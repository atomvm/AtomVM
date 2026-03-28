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

%% @doc Example: distributed Erlang over serial (UART).
%%
%% This example starts distribution using one or more UART connections
%% instead of TCP/IP. It works on ESP32 and Unix (using real serial
%% devices or virtual serial ports created with socat).
%%
%% <h3>ESP32 wiring (single peer)</h3>
%%
%% ```
%% ESP32 TX (GPIO 17) -> Peer RX
%% ESP32 RX (GPIO 16) -> Peer TX
%% ESP32 GND          -> Peer GND
%% '''
%%
%% <h3>ESP32 with two peers</h3>
%%
%% Many ESP32 boards have UART1 and UART2. Set `SERIAL_MULTI=true'
%% to connect to two peers simultaneously:
%%
%% ```
%% UART1: TX=17, RX=16 -> Peer A
%% UART2: TX=4,  RX=5  -> Peer B
%% '''
%%
%% <h3>Unix with socat</h3>
%%
%% Create a virtual serial port pair:
%% ```
%% socat -d -d pty,raw,echo=0 pty,raw,echo=0
%% '''
%% Then set the SERIAL_DEVICE environment variable to one of the pty
%% paths before running this example. The peer node uses the other pty.
%%
%% <h3>Connecting</h3>
%%
%% The node name is derived from the serial device, e.g.
%% `ttys003@serial.local' or `uart1@serial.local'. Once both nodes are
%% running, trigger autoconnect from either side:
%% ```
%% {serial_disterl, 'ttys003@serial.local'} ! {hello, node()}.
%% '''
-module(serial_disterl).

-export([start/0]).

start() ->
    UartConfigs = uart_configs(),
    NodeName = make_node_name(hd(UartConfigs)),
    DistOpts =
        case UartConfigs of
            [Single] -> #{uart_opts => Single};
            Multiple -> #{uart_ports => Multiple}
        end,
    {ok, _NetKernelPid} = net_kernel:start(NodeName, #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => DistOpts
    }),
    io:format("Distribution started over serial (~p port(s))~n", [length(UartConfigs)]),
    io:format("Node: ~p~n", [node()]),
    net_kernel:set_cookie(<<"AtomVM">>),
    io:format("Cookie: ~s~n", [net_kernel:get_cookie()]),
    register(serial_disterl, self()),
    io:format("Registered as 'serial_disterl'. Waiting for messages.~n"),
    io:format("From the peer:~n"),
    io:format("  {serial_disterl, '~s'} ! {hello, node()}.~n", [node()]),
    loop().

%% Build a node name from the serial device.
%% e.g. "UART1" -> 'uart1@serial.local'
%%      "/dev/ttys003" -> 'ttys003@serial.local'
make_node_name(UartOpts) ->
    Peripheral = proplists:get_value(peripheral, UartOpts, "serial"),
    BaseName = basename(Peripheral),
    list_to_atom(string:to_lower(BaseName) ++ "@serial.local").

basename(Path) ->
    case lists:last(string:split(Path, "/", all)) of
        [] -> Path;
        Name -> Name
    end.

%% Platform-specific UART configuration.
%% Returns a list of UART option proplists (one per port).
uart_configs() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            case atomvm:platform() of
                esp32 ->
                    case os:getenv("SERIAL_MULTI") of
                        "true" ->
                            %% Two UARTs: connect to two peers
                            [
                                [{peripheral, "UART1"}, {speed, 115200}, {tx, 17}, {rx, 16}],
                                [{peripheral, "UART2"}, {speed, 115200}, {tx, 4}, {rx, 5}]
                            ];
                        _ ->
                            [[{peripheral, "UART1"}, {speed, 115200}, {tx, 17}, {rx, 16}]]
                    end;
                generic_unix ->
                    Device = os:getenv("SERIAL_DEVICE"),
                    case Device of
                        false ->
                            io:format("Error: set SERIAL_DEVICE env var to a serial port path~n"),
                            io:format("  e.g. /dev/ttyUSB0 or a socat pty~n"),
                            exit(no_serial_device);
                        _ ->
                            [[{peripheral, Device}, {speed, 115200}]]
                    end;
                Other ->
                    io:format("Error: unsupported platform ~p~n", [Other]),
                    exit({unsupported_platform, Other})
            end;
        "BEAM" ->
            io:format("Error: this example requires AtomVM~n"),
            io:format("  See serial_dist module doc for BEAM usage~n"),
            exit(beam_not_supported)
    end.

loop() ->
    receive
        quit ->
            io:format("Received quit, stopping.~n"),
            ok;
        {hello, From} ->
            io:format("Hello from ~p!~n", [From]),
            loop();
        Other ->
            io:format("Received: ~p~n", [Other]),
            loop()
    end.
