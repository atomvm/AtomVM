%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
%% @doc GRiSP 2 board support module.
%%
%% This module provides high-level APIs and peripheral configurations for the
%% GRiSP 2 embedded hardware platform running on RTEMS.
%%
%% Features provided:
%% <ul>
%%   <li>RGB LED control (`led/2`, `led_off/1`) for RGB1 and RGB2.</li>
%%   <li>Board EEPROM and EUI-48 MAC decoding (`eeprom/0`, `serial_number/0`,
%%       `hw_rev/0`, `mac_address/0`).</li>
%%   <li>Jumper keys reading (`jumper/1`, `jumpers/0`).</li>
%%   <li>Pmod connector configurations for SPI, I2C, UART, and GPIO.</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(grisp).

-export([
    platform/0,
    % LEDs
    led/2,
    led_off/1,
    led_pin/2,
    % Jumpers
    jumper/1,
    jumpers/0,
    jumper_pin/1,
    % EEPROM / Board Identity
    eeprom/0,
    serial_number/0,
    batch/0,
    hw_rev/0,
    mac_address/0,
    mac_address_string/0,
    % SPI
    spi_bus_config/0,
    spi_pin/1,
    spi_open/1,
    spi_open/2,
    spi_close/1,
    % I2C
    i2c_bus_config/1,
    i2c_open/1,
    i2c_close/1,
    i2c_address/1,
    i2c_pin/1,
    % UART
    uart_config/1,
    uart_open/1,
    uart_open/2,
    uart_close/1,
    % Pmod GPIO
    pmod_gpio_pin/1,
    pin/1,
    % Wi-Fi
    wifi_connect/2,
    wifi_connect/3,
    wifi_disconnect/0,
    wifi_status/0,
    wifi_wait/0,
    wifi_wait/1
]).

-define(I2C_EEPROM_ADDR, 16#57).
-define(I2C_EUI_ADDR, 16#5F).
-define(EUI_REGISTER, 16#9A).

-type led_id() :: 1 | 2.
-type color() ::
    off | black | red | green | blue | yellow | magenta | cyan | white |
    {0..255, 0..255, 0..255} | {boolean(), boolean(), boolean()}.
-type jumper_id() :: 1..5.
-type level() :: high | low.
-type eeprom_info() :: #{
    sig_version => byte(),
    serial => non_neg_integer(),
    batch => non_neg_integer(),
    pcb_variant => byte(),
    pcb_version => byte(),
    assembly_variant => byte(),
    assembly_version => byte(),
    crc16 => non_neg_integer()
}.

%%-----------------------------------------------------------------------------
%% @doc Return the target platform identifier.
%% @end
%%-----------------------------------------------------------------------------
-spec platform() -> grisp2.
platform() ->
    grisp2.

%%-----------------------------------------------------------------------------
%% @doc Set color of RGB LED 1 or 2 on GRiSP 2.
%%
%% Supported colors: `off`, `black`, `red`, `green`, `blue`, `yellow`,
%% `magenta`, `cyan`, `white`, or an `{R, G, B}` tuple of levels.
%% @end
%%-----------------------------------------------------------------------------
-spec led(Led :: led_id(), Color :: color()) -> ok.
led(Led, Color) when Led =:= 1 orelse Led =:= 2 ->
    {R, G, B} = color_to_rgb(Color),
    set_led_channel(Led, red, R),
    set_led_channel(Led, green, G),
    set_led_channel(Led, blue, B),
    ok;
led(_Led, _Color) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Turn off RGB LED 1 or 2.
%% @end
%%-----------------------------------------------------------------------------
-spec led_off(Led :: led_id()) -> ok.
led_off(Led) ->
    led(Led, off).

%%-----------------------------------------------------------------------------
%% @doc Return the FDT pin map for an RGB LED channel.
%% @end
%%-----------------------------------------------------------------------------
-spec led_pin(Led :: led_id(), Channel :: red | green | blue) -> gpio:pin().
led_pin(1, red) ->
    #{path => "/leds/grisp-rgb1-red", property => "gpios", index => 0};
led_pin(1, green) ->
    #{path => "/leds/grisp-rgb1-green", property => "gpios", index => 0};
led_pin(1, blue) ->
    #{path => "/leds/grisp-rgb1-blue", property => "gpios", index => 0};
led_pin(2, red) ->
    #{path => "/leds/grisp-rgb2-red", property => "gpios", index => 0};
led_pin(2, green) ->
    #{path => "/leds/grisp-rgb2-green", property => "gpios", index => 0};
led_pin(2, blue) ->
    #{path => "/leds/grisp-rgb2-blue", property => "gpios", index => 0};
led_pin(_Led, _Channel) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Read the state of a jumper key (1..5).
%% @end
%%-----------------------------------------------------------------------------
-spec jumper(Jumper :: jumper_id()) -> level().
jumper(Jumper) when Jumper >= 1 andalso Jumper =< 5 ->
    Pin = jumper_pin(Jumper),
    ok = gpio:set_pin_mode(Pin, input),
    ok = gpio:set_pin_pull(Pin, up),
    gpio:digital_read(Pin);
jumper(_Jumper) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Read all 5 jumpers and return their states `[J1, J2, J3, J4, J5]`.
%% @end
%%-----------------------------------------------------------------------------
-spec jumpers() -> [level()].
jumpers() ->
    [jumper(N) || N <- [1, 2, 3, 4, 5]].

%%-----------------------------------------------------------------------------
%% @doc Return the FDT pin map for a jumper key (1..5).
%% @end
%%-----------------------------------------------------------------------------
-spec jumper_pin(Jumper :: jumper_id()) -> gpio:pin().
jumper_pin(Jumper) when Jumper >= 1 andalso Jumper =< 5 ->
    #{path => "/jumper-keys", property => "grisp,gpios", index => Jumper - 1};
jumper_pin(_Jumper) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Read and decode the board AT24MAC402 EEPROM at I2C address `0x57`.
%% @end
%%-----------------------------------------------------------------------------
-spec eeprom() -> {ok, eeprom_info()} | {error, term()}.
eeprom() ->
    I2C = i2c:open(i2c_bus_config(internal)),
    try
        case i2c:read_bytes(I2C, ?I2C_EEPROM_ADDR, 0, 16, [{mem_add_size, 8}]) of
            {ok, <<SigVer:8, _Pad:3/binary, Serial:32/little, Batch:16/little,
                   PcbVar:8, PcbVer:8, AssVar:8, AssVer:8,
                   CRC16:16/little, _Rest/binary>> = Bin} ->
                HeaderBytes = binary:part(Bin, 0, 14),
                case calc_crc16(HeaderBytes) =:= CRC16 of
                    true ->
                        {ok, #{
                            sig_version => SigVer,
                            serial => Serial,
                            batch => Batch,
                            pcb_variant => PcbVar,
                            pcb_version => PcbVer,
                            assembly_variant => AssVar,
                            assembly_version => AssVer,
                            crc16 => CRC16
                        }};
                    false ->
                        {error, crc_mismatch}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    after
        i2c:close(I2C)
    end.

%%-----------------------------------------------------------------------------
%% @doc Return the board's unique serial number.
%% @end
%%-----------------------------------------------------------------------------
-spec serial_number() -> {ok, non_neg_integer()} | {error, term()}.
serial_number() ->
    case eeprom() of
        {ok, #{serial := Serial}} ->
            {ok, Serial};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%% @doc Return the board's production batch number.
%% @end
%%-----------------------------------------------------------------------------
-spec batch() -> {ok, non_neg_integer()} | {error, term()}.
batch() ->
    case eeprom() of
        {ok, #{batch := Batch}} ->
            {ok, Batch};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%% @doc Return `{ok, {PcbVariant, PcbVersion}}`.
%% @end
%%-----------------------------------------------------------------------------
-spec hw_rev() -> {ok, {byte(), byte()}} | {error, term()}.
hw_rev() ->
    case eeprom() of
        {ok, #{pcb_variant := Var, pcb_version := Ver}} ->
            {ok, {Var, Ver}};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%% @doc Read the factory-programmed 48-bit EUI MAC address from AT24MAC402 at `0x5F`.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_address() -> {ok, binary()} | {error, term()}.
mac_address() ->
    I2C = i2c:open(i2c_bus_config(internal)),
    try
        case i2c:read_bytes(I2C, ?I2C_EUI_ADDR, ?EUI_REGISTER, 6, [{mem_add_size, 8}]) of
            {ok, <<_A, _B, _C, _D, _E, _F>> = Mac} ->
                {ok, Mac};
            {ok, Other} ->
                {error, {bad_mac_length, byte_size(Other)}};
            {error, Reason} ->
                {error, Reason}
        end
    after
        i2c:close(I2C)
    end.

%%-----------------------------------------------------------------------------
%% @doc Return the factory MAC address formatted as a standard colon-separated string.
%% @end
%%-----------------------------------------------------------------------------
-spec mac_address_string() -> {ok, string()} | {error, term()}.
mac_address_string() ->
    case mac_address() of
        {ok, <<A, B, C, D, E, F>>} ->
            Formatted = io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [A, B, C, D, E, F]),
            {ok, lists:flatten(Formatted)};
        {error, Reason} ->
            {error, Reason}
    end.

%%-----------------------------------------------------------------------------
%% @doc Return the SPI bus configuration for the GRiSP 2 Pmod SPI connector (X1402).
%% @end
%%-----------------------------------------------------------------------------
-spec spi_bus_config() -> [{peripheral, string()} | {fdt_alias, string()}].
spi_bus_config() ->
    [{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}].

%%-----------------------------------------------------------------------------
%% @doc Return the FDT pin map for SPI Pmod pins.
%%
%% Supported PinIds:
%% <ul>
%%   <li>`cs1`: Primary Chip Select (X1402 pin 1, GPIO3 10)</li>
%%   <li>`cs2`: Secondary Chip Select (X1402 pin 8, GPIO4 27)</li>
%%   <li>`cs3`: Secondary Chip Select (X1402 pin 9, GPIO4 28)</li>
%%   <li>`cs4`: Secondary Chip Select (X1402 pin 10, GPIO4 25)</li>
%%   <li>`int`: Interrupt input pin (X1402 pin 7, GPIO4 19)</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec spi_pin(PinId :: cs1 | cs2 | cs3 | cs4 | int) -> gpio:pin().
spi_pin(cs1) ->
    #{path => "/pmod-spi", property => "grisp,gpios", index => 0};
spi_pin(cs2) ->
    #{path => "/pmod-spi", property => "grisp,gpios", index => 1};
spi_pin(cs3) ->
    #{path => "/pmod-spi", property => "grisp,gpios", index => 2};
spi_pin(cs4) ->
    #{path => "/pmod-spi", property => "grisp,gpios", index => 3};
spi_pin(int) ->
    #{path => "/pmod-spi", property => "grisp,int-gpios", index => 0};
spi_pin(_PinId) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Open the Pmod SPI bus with a default device configuration.
%% @end
%%-----------------------------------------------------------------------------
-spec spi_open(DeviceName :: atom()) -> spi:spi().
spi_open(DeviceName) ->
    spi_open(DeviceName, []).

%%-----------------------------------------------------------------------------
%% @doc Open the Pmod SPI bus with a specific device configuration.
%%
%% Options:
%% <ul>
%%   <li>`{cs, cs1 | cs2 | cs3 | cs4 | gpio:pin()}` (default: `cs1`)</li>
%%   <li>`{clock_speed_hz, integer()}` (default: 1000000)</li>
%%   <li>`{mode, 0..3}` (default: 0)</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec spi_open(DeviceName :: atom(), DeviceOpts :: proplists:proplist()) -> spi:spi().
spi_open(DeviceName, DeviceOpts) ->
    CsPin = case proplists:get_value(cs, DeviceOpts, cs1) of
        cs1 -> spi_pin(cs1);
        cs2 -> spi_pin(cs2);
        cs3 -> spi_pin(cs3);
        cs4 -> spi_pin(cs4);
        Pin when is_map(Pin) orelse is_tuple(Pin) -> Pin
    end,
    Speed = proplists:get_value(clock_speed_hz, DeviceOpts, 1000000),
    Mode = proplists:get_value(mode, DeviceOpts, 0),
    BusConfig = spi_bus_config(),
    DeviceConfig = [
        {DeviceName, [
            {cs, CsPin},
            {clock_speed_hz, Speed},
            {mode, Mode}
        ]}
    ],
    spi:open([
        {bus_config, BusConfig},
        {device_config, DeviceConfig}
    ]).

%%-----------------------------------------------------------------------------
%% @doc Close a previously opened SPI port.
%% @end
%%-----------------------------------------------------------------------------
-spec spi_close(SPI :: spi:spi()) -> ok.
spi_close(SPI) ->
    spi:close(SPI).

%%-----------------------------------------------------------------------------
%% @doc Return the I2C bus configuration for the internal bus (`internal`) or
%% external Pmod connector (`pmod`).
%% @end
%%-----------------------------------------------------------------------------
-spec i2c_bus_config(Bus :: internal | pmod) -> [{peripheral, string()} | {fdt_alias, string()}].
i2c_bus_config(internal) ->
    [{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}];
i2c_bus_config(pmod) ->
    [{peripheral, "/dev/i2c-1"}, {fdt_alias, "i2c1"}];
i2c_bus_config(_Bus) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Open the internal (`internal`) or Pmod (`pmod`) I2C bus.
%% @end
%%-----------------------------------------------------------------------------
-spec i2c_open(Bus :: internal | pmod) -> i2c:i2c().
i2c_open(Bus) ->
    i2c:open(i2c_bus_config(Bus)).

%%-----------------------------------------------------------------------------
%% @doc Close a previously opened I2C bus.
%% @end
%%-----------------------------------------------------------------------------
-spec i2c_close(I2C :: i2c:i2c()) -> ok.
i2c_close(I2C) ->
    i2c:close(I2C).

%%-----------------------------------------------------------------------------
%% @doc Return the 7-bit I2C address for standard onboard components.
%% @end
%%-----------------------------------------------------------------------------
-spec i2c_address(Component :: eeprom | eui | crypto | onewire) -> 16#18 | 16#36 | 16#57 | 16#5F.
i2c_address(eeprom)  -> ?I2C_EEPROM_ADDR;
i2c_address(eui)     -> ?I2C_EUI_ADDR;
i2c_address(crypto)  -> 16#36;
i2c_address(onewire) -> 16#18;
i2c_address(_Comp)   -> erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Return the FDT pin map for Pmod I2C interrupt and reset lines.
%% @end
%%-----------------------------------------------------------------------------
-spec i2c_pin(PinId :: int | reset) -> gpio:pin().
i2c_pin(int) ->
    #{path => "/pmod-i2c", property => "grisp,int-gpios", index => 0};
i2c_pin(reset) ->
    #{path => "/pmod-i2c", property => "grisp,reset-gpios", index => 0};
i2c_pin(_PinId) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Return the UART configuration for `console` or `pmod`.
%% @end
%%-----------------------------------------------------------------------------
-spec uart_config(Port :: console | pmod) -> [{peripheral, string()} | {speed, integer()}].
uart_config(console) ->
    [{peripheral, "/dev/console"}, {speed, 115200}];
uart_config(pmod) ->
    [{peripheral, "/dev/ttyS3"}, {speed, 115200}];
uart_config(_Port) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Open a UART port (`console` or `pmod`) at default 115200 baud.
%% @end
%%-----------------------------------------------------------------------------
-spec uart_open(Port :: console | pmod) -> uart:uart().
uart_open(Port) ->
    uart_open(Port, []).

%%-----------------------------------------------------------------------------
%% @doc Open a UART port with custom baud rate or configuration options.
%% @end
%%-----------------------------------------------------------------------------
-spec uart_open(Port :: console | pmod, SpeedOrOpts :: integer() | proplists:proplist()) -> uart:uart().
uart_open(Port, Speed) when is_integer(Speed) ->
    uart_open(Port, [{speed, Speed}]);
uart_open(Port, Opts) when is_list(Opts) ->
    BaseConfig = uart_config(Port),
    Config = lists:foldl(
        fun({K, V}, Acc) -> lists:keystore(K, 1, Acc, {K, V}) end,
        BaseConfig,
        Opts
    ),
    uart:open(Config);
uart_open(_Port, _Opts) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Close a previously opened UART port.
%% @end
%%-----------------------------------------------------------------------------
-spec uart_close(UART :: uart:uart()) -> ok.
uart_close(UART) ->
    uart:close(UART).

%%-----------------------------------------------------------------------------
%% @doc Return the FDT pin map for Pmod GPIO connector (X1404) pin 1..8.
%% @end
%%-----------------------------------------------------------------------------
-spec pmod_gpio_pin(Pin :: 1..8) -> gpio:pin().
pmod_gpio_pin(Pin) when Pin >= 1 andalso Pin =< 8 ->
    #{path => "/pmod-gpio", property => "grisp,gpios", index => Pin - 1};
pmod_gpio_pin(_Pin) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc General pin resolver for GRiSP 2 pins.
%% @end
%%-----------------------------------------------------------------------------
-spec pin(PinSpec :: {led, led_id(), red | green | blue} | {jumper, jumper_id()} | {pmod_gpio, 1..8} | {spi, atom()} | {i2c, atom()}) -> gpio:pin().
pin({led, Led, Channel}) ->
    led_pin(Led, Channel);
pin({jumper, Jumper}) ->
    jumper_pin(Jumper);
pin({pmod_gpio, N}) ->
    pmod_gpio_pin(N);
pin({spi, PinId}) ->
    spi_pin(PinId);
pin({i2c, PinId}) ->
    i2c_pin(PinId);
pin(_Other) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Connect to a wireless access point using WPA-PSK or WPA2-PSK,
%% waiting up to 15 seconds for association and DHCP lease.
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_connect(SSID :: string() | binary(), PSK :: string() | binary() | none) ->
    ok | {error, term()}.
wifi_connect(SSID, PSK) ->
    wifi_connect(SSID, PSK, []).

%%-----------------------------------------------------------------------------
%% @doc Connect to a wireless access point with custom options.
%%
%% Options:
%% <ul>
%%   <li>`{timeout, integer()}`: Timeout in milliseconds to wait for DHCP (default: 15000).</li>
%%   <li>`{wait, boolean()}`: Whether to wait for DHCP lease (default: true).</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_connect(SSID :: string() | binary(), PSK :: string() | binary() | none, Opts :: proplists:proplist()) ->
    ok | {error, term()}.
wifi_connect(SSID, PSK, Opts) when is_list(Opts) ->
    case atomvm_rtems:wifi_start(SSID, PSK) of
        ok ->
            case proplists:get_value(wait, Opts, true) of
                true ->
                    Timeout = proplists:get_value(timeout, Opts, 15000),
                    atomvm_rtems:wait_wifi(Timeout);
                false ->
                    ok
            end;
        {error, Reason} ->
            {error, Reason}
    end;
wifi_connect(_SSID, _PSK, _Opts) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% @doc Disconnect from the current Wi-Fi network and bring down `wlan0`.
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_disconnect() -> ok | {error, term()}.
wifi_disconnect() ->
    atomvm_rtems:wifi_stop().

%%-----------------------------------------------------------------------------
%% @doc Return current wireless interface status, state, and IP parameters.
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_status() -> {ok, [{atom(), term()}]} | {error, term()}.
wifi_status() ->
    atomvm_rtems:wifi_status().

%%-----------------------------------------------------------------------------
%% @doc Wait up to 15 seconds for Wi-Fi association and DHCP IP assignment.
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_wait() -> ok | {error, timeout | enotsup | term()}.
wifi_wait() ->
    wifi_wait(15000).

%%-----------------------------------------------------------------------------
%% @doc Wait up to TimeoutMs for Wi-Fi association and DHCP IP assignment.
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_wait(TimeoutMs :: non_neg_integer()) -> ok | {error, timeout | enotsup | term()}.
wifi_wait(TimeoutMs) when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
    atomvm_rtems:wait_wifi(TimeoutMs);
wifi_wait(_TimeoutMs) ->
    erlang:error(badarg).

%%-----------------------------------------------------------------------------
%% Internal helpers
%%-----------------------------------------------------------------------------

%% @private
set_led_channel(Led, Channel, Value) ->
    Pin = led_pin(Led, Channel),
    ok = gpio:set_pin_mode(Pin, output),
    Level = if Value > 0 -> high; true -> low end,
    gpio:digital_write(Pin, Level).

%% @private
color_to_rgb(off)   -> {0, 0, 0};
color_to_rgb(black) -> {0, 0, 0};
color_to_rgb(red)   -> {1, 0, 0};
color_to_rgb(green) -> {0, 1, 0};
color_to_rgb(blue)  -> {0, 0, 1};
color_to_rgb(yellow)-> {1, 1, 0};
color_to_rgb(magenta)->{1, 0, 1};
color_to_rgb(cyan)  -> {0, 1, 1};
color_to_rgb(white) -> {1, 1, 1};
color_to_rgb({R, G, B}) ->
    {to_int_val(R), to_int_val(G), to_int_val(B)};
color_to_rgb(_Other) ->
    erlang:error(badarg).

%% @private
to_int_val(true) -> 1;
to_int_val(false)-> 0;
to_int_val(N) when is_integer(N) andalso N > 0 -> 1;
to_int_val(0) -> 0;
to_int_val(_) -> 0.

%% @private
%% RFC 1331 / CCITT-16 CRC with 0x8408 polynomial and 0 initial value
calc_crc16(Bin) ->
    calc_crc16(Bin, 0).

calc_crc16(<<>>, CRC) ->
    CRC;
calc_crc16(<<B:8, Rest/binary>>, CRC) ->
    Idx = (CRC bxor B) band 16#FF,
    NewCRC = (CRC bsr 8) bxor crc16_table(Idx),
    calc_crc16(Rest, NewCRC).

%% CRC-16 table for polynomial 0x8408 (RFC 1331 / CCITT-16 reversed)
crc16_table(0) -> 16#0000; crc16_table(1) -> 16#1189; crc16_table(2) -> 16#2312; crc16_table(3) -> 16#329B;
crc16_table(4) -> 16#4624; crc16_table(5) -> 16#57AD; crc16_table(6) -> 16#6536; crc16_table(7) -> 16#74BF;
crc16_table(8) -> 16#8C48; crc16_table(9) -> 16#9DC1; crc16_table(10) -> 16#AF5A; crc16_table(11) -> 16#BED3;
crc16_table(12) -> 16#CA6C; crc16_table(13) -> 16#DBE5; crc16_table(14) -> 16#E97E; crc16_table(15) -> 16#F8F7;
crc16_table(16) -> 16#1081; crc16_table(17) -> 16#0108; crc16_table(18) -> 16#3393; crc16_table(19) -> 16#221A;
crc16_table(20) -> 16#56A5; crc16_table(21) -> 16#472C; crc16_table(22) -> 16#75B7; crc16_table(23) -> 16#643E;
crc16_table(24) -> 16#9CC9; crc16_table(25) -> 16#8D40; crc16_table(26) -> 16#BFDB; crc16_table(27) -> 16#AE52;
crc16_table(28) -> 16#DAED; crc16_table(29) -> 16#CB64; crc16_table(30) -> 16#F9FF; crc16_table(31) -> 16#E876;
crc16_table(32) -> 16#2102; crc16_table(33) -> 16#308B; crc16_table(34) -> 16#0210; crc16_table(35) -> 16#1399;
crc16_table(36) -> 16#6726; crc16_table(37) -> 16#76AF; crc16_table(38) -> 16#4434; crc16_table(39) -> 16#55BD;
crc16_table(40) -> 16#AD4A; crc16_table(41) -> 16#BCC3; crc16_table(42) -> 16#8E58; crc16_table(43) -> 16#9FD1;
crc16_table(44) -> 16#EB6E; crc16_table(45) -> 16#FAE7; crc16_table(46) -> 16#C87C; crc16_table(47) -> 16#D9F5;
crc16_table(48) -> 16#3183; crc16_table(49) -> 16#200A; crc16_table(50) -> 16#1291; crc16_table(51) -> 16#0318;
crc16_table(52) -> 16#77A7; crc16_table(53) -> 16#662E; crc16_table(54) -> 16#54B5; crc16_table(55) -> 16#453C;
crc16_table(56) -> 16#BDCB; crc16_table(57) -> 16#AC42; crc16_table(58) -> 16#9ED9; crc16_table(59) -> 16#8F50;
crc16_table(60) -> 16#FBEF; crc16_table(61) -> 16#EA66; crc16_table(62) -> 16#D8FD; crc16_table(63) -> 16#C974;
crc16_table(64) -> 16#4204; crc16_table(65) -> 16#538D; crc16_table(66) -> 16#6116; crc16_table(67) -> 16#709F;
crc16_table(68) -> 16#0420; crc16_table(69) -> 16#15A9; crc16_table(70) -> 16#2732; crc16_table(71) -> 16#36BB;
crc16_table(72) -> 16#CE4C; crc16_table(73) -> 16#DFC5; crc16_table(74) -> 16#ED5E; crc16_table(75) -> 16#FCD7;
crc16_table(76) -> 16#8868; crc16_table(77) -> 16#99E1; crc16_table(78) -> 16#AB7A; crc16_table(79) -> 16#BAF3;
crc16_table(80) -> 16#5285; crc16_table(81) -> 16#430C; crc16_table(82) -> 16#7197; crc16_table(83) -> 16#601E;
crc16_table(84) -> 16#14A1; crc16_table(85) -> 16#0528; crc16_table(86) -> 16#37B3; crc16_table(87) -> 16#263A;
crc16_table(88) -> 16#DECD; crc16_table(89) -> 16#CF44; crc16_table(90) -> 16#FDDF; crc16_table(91) -> 16#EC56;
crc16_table(92) -> 16#98E9; crc16_table(93) -> 16#8960; crc16_table(94) -> 16#BBFB; crc16_table(95) -> 16#AA72;
crc16_table(96) -> 16#6306; crc16_table(97) -> 16#728F; crc16_table(98) -> 16#4014; crc16_table(99) -> 16#519D;
crc16_table(100) -> 16#2522; crc16_table(101) -> 16#34AB; crc16_table(102) -> 16#0630; crc16_table(103) -> 16#17B9;
crc16_table(104) -> 16#EF4E; crc16_table(105) -> 16#FEC7; crc16_table(106) -> 16#CC5C; crc16_table(107) -> 16#DDD5;
crc16_table(108) -> 16#A96A; crc16_table(109) -> 16#B8E3; crc16_table(110) -> 16#8A78; crc16_table(111) -> 16#9BF1;
crc16_table(112) -> 16#7387; crc16_table(113) -> 16#620E; crc16_table(114) -> 16#5095; crc16_table(115) -> 16#411C;
crc16_table(116) -> 16#35A3; crc16_table(117) -> 16#242A; crc16_table(118) -> 16#16B1; crc16_table(119) -> 16#0738;
crc16_table(120) -> 16#FFCF; crc16_table(121) -> 16#EE46; crc16_table(122) -> 16#DCDD; crc16_table(123) -> 16#CD54;
crc16_table(124) -> 16#B9EB; crc16_table(125) -> 16#A862; crc16_table(126) -> 16#9AF9; crc16_table(127) -> 16#8B70;
crc16_table(128) -> 16#8408; crc16_table(129) -> 16#9581; crc16_table(130) -> 16#A71A; crc16_table(131) -> 16#B693;
crc16_table(132) -> 16#C22C; crc16_table(133) -> 16#D3A5; crc16_table(134) -> 16#E13E; crc16_table(135) -> 16#F0B7;
crc16_table(136) -> 16#0840; crc16_table(137) -> 16#19C9; crc16_table(138) -> 16#2B52; crc16_table(139) -> 16#3ADB;
crc16_table(140) -> 16#4E64; crc16_table(141) -> 16#5FED; crc16_table(142) -> 16#6D76; crc16_table(143) -> 16#7CFF;
crc16_table(144) -> 16#9489; crc16_table(145) -> 16#8500; crc16_table(146) -> 16#B79B; crc16_table(147) -> 16#A612;
crc16_table(148) -> 16#D2AD; crc16_table(149) -> 16#C324; crc16_table(150) -> 16#F1BF; crc16_table(151) -> 16#E036;
crc16_table(152) -> 16#18C1; crc16_table(153) -> 16#0948; crc16_table(154) -> 16#3BD3; crc16_table(155) -> 16#2A5A;
crc16_table(156) -> 16#5EE5; crc16_table(157) -> 16#4F6C; crc16_table(158) -> 16#7DF7; crc16_table(159) -> 16#6C7E;
crc16_table(160) -> 16#A50A; crc16_table(161) -> 16#B483; crc16_table(162) -> 16#8618; crc16_table(163) -> 16#9791;
crc16_table(164) -> 16#E32E; crc16_table(165) -> 16#F2A7; crc16_table(166) -> 16#C03C; crc16_table(167) -> 16#D1B5;
crc16_table(168) -> 16#2942; crc16_table(169) -> 16#38CB; crc16_table(170) -> 16#0A50; crc16_table(171) -> 16#1BD9;
crc16_table(172) -> 16#6F66; crc16_table(173) -> 16#7EEF; crc16_table(174) -> 16#4C74; crc16_table(175) -> 16#5DFD;
crc16_table(176) -> 16#B58B; crc16_table(177) -> 16#A402; crc16_table(178) -> 16#9699; crc16_table(179) -> 16#8710;
crc16_table(180) -> 16#F3AF; crc16_table(181) -> 16#E226; crc16_table(182) -> 16#D0BD; crc16_table(183) -> 16#C134;
crc16_table(184) -> 16#39C3; crc16_table(185) -> 16#284A; crc16_table(186) -> 16#1AD1; crc16_table(187) -> 16#0B58;
crc16_table(188) -> 16#7FE7; crc16_table(189) -> 16#6E6E; crc16_table(190) -> 16#5CF5; crc16_table(191) -> 16#4D7C;
crc16_table(192) -> 16#C60C; crc16_table(193) -> 16#D785; crc16_table(194) -> 16#E51E; crc16_table(195) -> 16#F497;
crc16_table(196) -> 16#8028; crc16_table(197) -> 16#91A1; crc16_table(198) -> 16#A33A; crc16_table(199) -> 16#B2B3;
crc16_table(200) -> 16#4A44; crc16_table(201) -> 16#5BCD; crc16_table(202) -> 16#6956; crc16_table(203) -> 16#78DF;
crc16_table(204) -> 16#0C60; crc16_table(205) -> 16#1DE9; crc16_table(206) -> 16#2F72; crc16_table(207) -> 16#3EFB;
crc16_table(208) -> 16#D68D; crc16_table(209) -> 16#C704; crc16_table(210) -> 16#F59F; crc16_table(211) -> 16#E416;
crc16_table(212) -> 16#90A9; crc16_table(213) -> 16#8120; crc16_table(214) -> 16#B3BB; crc16_table(215) -> 16#A232;
crc16_table(216) -> 16#5AC5; crc16_table(217) -> 16#4B4C; crc16_table(218) -> 16#79D7; crc16_table(219) -> 16#685E;
crc16_table(220) -> 16#1CE1; crc16_table(221) -> 16#0D68; crc16_table(222) -> 16#3FF3; crc16_table(223) -> 16#2E7A;
crc16_table(224) -> 16#E70E; crc16_table(225) -> 16#F687; crc16_table(226) -> 16#C41C; crc16_table(227) -> 16#D595;
crc16_table(228) -> 16#A12A; crc16_table(229) -> 16#B0A3; crc16_table(230) -> 16#8238; crc16_table(231) -> 16#93B1;
crc16_table(232) -> 16#6B46; crc16_table(233) -> 16#7ACF; crc16_table(234) -> 16#4854; crc16_table(235) -> 16#59DD;
crc16_table(236) -> 16#2D62; crc16_table(237) -> 16#3CEB; crc16_table(238) -> 16#0E70; crc16_table(239) -> 16#1FF9;
crc16_table(240) -> 16#F78F; crc16_table(241) -> 16#E606; crc16_table(242) -> 16#D49D; crc16_table(243) -> 16#C514;
crc16_table(244) -> 16#B1AB; crc16_table(245) -> 16#A022; crc16_table(246) -> 16#92B9; crc16_table(247) -> 16#8330;
crc16_table(248) -> 16#7BC7; crc16_table(249) -> 16#6A4E; crc16_table(250) -> 16#58D5; crc16_table(251) -> 16#495C;
crc16_table(252) -> 16#3DE3; crc16_table(253) -> 16#2C6A; crc16_table(254) -> 16#1EF1; crc16_table(255) -> 16#0F78.
