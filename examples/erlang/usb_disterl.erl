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

%% @doc Example: distributed Erlang over USB CDC.
%%
%% This example starts distribution using USB CDC instead of TCP/IP.
%% It works on ESP32-S2/S3, RP2040/RP2350, and STM32 (MCU side) and
%% on Unix (host side, where USB CDC devices appear as /dev/ttyACMx).
%%
%% <h3>MCU side (ESP32-S3, RP2040, or STM32)</h3>
%%
%% Flash this example to the MCU. It will start distribution over the
%% USB CDC interface and register a process that responds to messages.
%%
%% <h3>Unix host side</h3>
%%
%% Set `SERIAL_DEVICE' to the device path of the MCU's USB CDC port:
%% ```
%% SERIAL_DEVICE=/dev/ttyACM0 AtomVM usb_disterl.avm
%% '''
%%
%% <h3>Multi-device (Unix host with USB hub)</h3>
%%
%% Connect multiple MCUs through a USB hub. Each appears as a separate
%% ttyACMx device. Set `SERIAL_MULTI=true' and `SERIAL_DEVICE_1' and
%% `SERIAL_DEVICE_2':
%% ```
%% SERIAL_MULTI=true SERIAL_DEVICE_1=/dev/ttyACM0 SERIAL_DEVICE_2=/dev/ttyACM1 AtomVM usb_disterl.avm
%% '''
%% @end
-module(usb_disterl).

-export([start/0]).

start() ->
    {UartModule, UartConfig} = get_platform_config(),
    NodeName = make_node_name(UartConfig),
    io:format("Starting USB distribution as ~p~n", [NodeName]),
    {ok, _} = net_kernel:start(NodeName, #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => UartConfig#{uart_module => UartModule}
    }),
    ok = net_kernel:set_cookie(<<"AtomVM">>),
    io:format("Distribution started. Node: ~p~n", [node()]),
    register(usb_disterl, self()),
    loop().

loop() ->
    receive
        {From, ping} ->
            io:format("Got ping from ~p~n", [From]),
            From ! {self(), pong},
            loop();
        {From, {echo, Msg}} ->
            io:format("Echo ~p from ~p~n", [Msg, From]),
            From ! {self(), {echo_reply, Msg}},
            loop();
        stop ->
            io:format("Stopping~n"),
            net_kernel:stop(),
            ok;
        Other ->
            io:format("Unknown message: ~p~n", [Other]),
            loop()
    end.

%% @private
get_platform_config() ->
    case atomvm:platform() of
        esp32 ->
            {usb_cdc, #{uart_opts => [{peripheral, "CDC0"}]}};
        pico ->
            {usb_cdc, #{uart_opts => []}};
        stm32 ->
            {usb_cdc, #{uart_opts => []}};
        generic_unix ->
            get_unix_config()
    end.

%% @private
get_unix_config() ->
    case os:getenv("SERIAL_MULTI") of
        "true" ->
            Dev1 = getenv_default("SERIAL_DEVICE_1", "/dev/ttyACM0"),
            Dev2 = getenv_default("SERIAL_DEVICE_2", "/dev/ttyACM1"),
            {uart, #{
                uart_ports => [
                    [{peripheral, Dev1}, {speed, 115200}],
                    [{peripheral, Dev2}, {speed, 115200}]
                ]
            }};
        _ ->
            Device = getenv_default("SERIAL_DEVICE", "/dev/ttyACM0"),
            {uart, #{
                uart_opts => [{peripheral, Device}, {speed, 115200}]
            }}
    end.

%% @private
getenv_default(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Value -> Value
    end.

%% @private
make_node_name(#{uart_ports := [FirstPort | _]}) ->
    Peripheral = proplists:get_value(peripheral, FirstPort, "usb"),
    name_from_peripheral(Peripheral);
make_node_name(#{uart_opts := Opts}) ->
    Peripheral = proplists:get_value(peripheral, Opts, "usb"),
    name_from_peripheral(Peripheral);
make_node_name(_) ->
    'usb@serial.local'.

%% @private
name_from_peripheral(Peripheral) when is_list(Peripheral) ->
    Base = basename(Peripheral),
    Lower = string:to_lower(Base),
    list_to_atom(Lower ++ "@serial.local");
name_from_peripheral(_) ->
    'usb@serial.local'.

%% @private
basename(Path) ->
    case lists:last(string:split(Path, "/", all)) of
        [] -> Path;
        Name -> Name
    end.
