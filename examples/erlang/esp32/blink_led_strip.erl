%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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
% Many Espressif ESP32 Devkit boards have an RGB LED of type "LED strip"
% See also: https://components.espressif.com/components/espressif/led_strip

-module(blink_led_strip).
-export([start/0]).

start() ->
    SI = erlang:system_info(esp32_chip_info),
    Model = maps:get(model, SI),
    io:format("Model ~p~n",[Model]),
    case lists:member(Model, [esp32_c3, esp32_c6, esp32_h2,
                              esp32_s2, esp32_s3]) of
        true ->
            SPI = spi:open(spi_config(Model)),
            spi_setcolors(SPI),
            ok = spi:close(SPI);
        false ->
            io:format("RGB LED strip not on board~n"),
            {error, not_supported}
    end.

spi_config(Model) ->
    [
     {bus_config,
      [
       {miso, -1}, %% Not used
       {mosi, gpio_pin(Model)},  %% On board RGB LED GPIO on ESP32-XX-DevKiTs
       {sclk, -1}  %% Not used
      ]},
     {device_config,
      [
       {led_strip_device,
        [
         {clock_speed_hz, 2400000}, %% 3 SPI bits per LED bit ~ 1.25 uS
         {mode, 0},
         {cs, -1}, %% Not used
         {address_len_bits, 8}
        ]}
      ]}
    ].

gpio_pin(esp32_c3) -> 8;
gpio_pin(esp32_c6) -> 8;
gpio_pin(esp32_h2) -> 8;
gpio_pin(esp32_s2) -> 18;
gpio_pin(esp32_s3) -> 38.

spi_setcolors(SPI) ->
    set_color(SPI, 120, 0, 0),
    timer:sleep(5000),
    set_color(SPI, 0, 120, 0),
    timer:sleep(5000),
    set_color(SPI, 0, 0, 120),
    timer:sleep(5000),
    set_color(SPI, 0, 0, 0).

%% Led strip of length 1 - ESP Devkit on board LED
set_color(SPI, Red, Green, Blue) ->
    set_color(SPI, Red, Green, Blue, 1).

%% Led strip of length N - simple case with same colour on all leds
set_color(SPI, Red, Green, Blue, N) ->
    WriteData = build_stream(Red, Green, Blue, N, <<>>),
    ok = spi:write(SPI, led_strip_device, #{write_data => WriteData}).

build_stream(Red, Green, Blue, N, Acc) when N > 0 ->
    LedN = led_strip_bytes(Red, Green, Blue),
    build_stream(Red, Green, Blue, N-1, << Acc/binary, LedN/binary >>);
build_stream(_Red, _Green, _Blue, _N, Acc) ->
    Acc.

%% 1 RGB LED will be 72 SPI bits <=> 9 bytes
led_strip_bytes(Red, Green, Blue) ->
    R = led_strip_bits(Red),
    G = led_strip_bits(Green),
    B = led_strip_bits(Blue),
    << G:24, R:24, B:24 >>. % Order is GRB in the stream.

%% WS2812 LED SPI 1 bit in will be 3 bits out
%% Bit = 1 -> 110, 0.8 us high, 0.45 us low at 2.4 MHz
%% Bit = 0 -> 100. 0.4 us high, 0.85 us low
%% One byte will be 24 SPI bits ( 3 bytes )
led_strip_bits(A) when A >= 0, A < 256, is_integer(A) ->
    led_strip_bits(A, 7, 0).

led_strip_bits(_A, N, Acc) when N < 0 -> Acc;
led_strip_bits(A, N, Acc) ->
    SPIbits =
        case (A bsr N) band 16#01 of
            1 -> 6; %% 110
            0 -> 4  %% 100
        end,
    led_strip_bits(A, N-1, (Acc bsl 3) bor SPIbits).
