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
%% @doc LIS3DH accelerometer SPI example.
%%
%% Reads X, Y, Z acceleration from a LIS3DH connected via SPI and prints
%% the values every second.
%%
%% Default pins are auto-detected from the platform and chip model:
%%
%% Pico (SPI0):          SCK=GP2, MOSI=GP3, MISO=GP4, CS=GP5
%% ESP32/S2/S3 (SPI2):   SCK=18,  MOSI=23,  MISO=19,  CS=5
%% ESP32-C2/C3/C5 (SPI2): SCK=6,  MOSI=7,   MISO=2,   CS=10
%% ESP32-C6/C61 (SPI2):  SCK=6,   MOSI=7,   MISO=2,   CS=16
%%
%% LIS3DH SPI protocol: bit 7 = R/W (1=read, 0=write),
%% bit 6 = MS (1=auto-increment).
%% @end
%%-----------------------------------------------------------------------------
-module(spi_lis3dh).
-export([start/0]).

%% LIS3DH registers
-define(WHO_AM_I, 16#0F).
-define(CTRL_REG1, 16#20).
-define(CTRL_REG4, 16#23).
-define(OUT_X_L, 16#28).

%% Expected WHO_AM_I response
-define(LIS3DH_ID, 16#33).

%% SPI address bits: bit 7 = read, bit 6 = auto-increment
-define(READ_BIT, 16#80).
-define(MS_BIT, 16#40).

start() ->
    {SCK, MOSI, MISO, CS} = default_pins(),
    SPI = spi:open([
        {bus_config, [
            {sclk, SCK},
            {mosi, MOSI},
            {miso, MISO}
        ]},
        {device_config, [
            {lis3dh, [
                {cs, CS},
                {clock_speed_hz, 1000000},
                {mode, 0},
                {address_len_bits, 8},
                {command_len_bits, 0}
            ]}
        ]}
    ]),
    case check_who_am_i(SPI) of
        ok ->
            configure(SPI),
            loop(SPI);
        {error, Reason} ->
            io:format("LIS3DH not found: ~p~n", [Reason])
    end.

check_who_am_i(SPI) ->
    case spi:read_at(SPI, lis3dh, ?READ_BIT bor ?WHO_AM_I, 8) of
        {ok, ?LIS3DH_ID} ->
            io:format("LIS3DH detected (SPI)~n"),
            ok;
        {ok, Other} ->
            io:format("Unexpected WHO_AM_I: ~.16B~n", [Other]),
            {error, unexpected_id};
        {error, _} = Error ->
            Error
    end.

configure(SPI) ->
    %% CTRL_REG1: 50 Hz ODR, normal mode, X/Y/Z enabled
    %% Bits: ODR=0100 LPen=0 Zen=1 Yen=1 Xen=1 -> 0x47
    ok = spi:write(SPI, lis3dh, #{address => ?CTRL_REG1, write_data => <<16#47:8>>}),
    %% CTRL_REG4: +/- 2g full scale, high resolution
    %% Bits: BDU=1 BLE=0 FS=00 HR=1 ST=00 SIM=0 -> 0x88
    ok = spi:write(SPI, lis3dh, #{address => ?CTRL_REG4, write_data => <<16#88:8>>}).

loop(SPI) ->
    case read_acceleration(SPI) of
        {ok, {X, Y, Z}} ->
            io:format("X=~B Y=~B Z=~B~n", [X, Y, Z]);
        {error, Reason} ->
            io:format("Read error: ~p~n", [Reason])
    end,
    timer:sleep(1000),
    loop(SPI).

read_acceleration(SPI) ->
    %% Read 6 bytes starting at OUT_X_L with auto-increment (MS bit set)
    case spi:read_at(SPI, lis3dh, ?READ_BIT bor ?MS_BIT bor ?OUT_X_L, 48) of
        {ok, Value} ->
            XL = (Value bsr 40) band 16#FF,
            XH = (Value bsr 32) band 16#FF,
            YL = (Value bsr 24) band 16#FF,
            YH = (Value bsr 16) band 16#FF,
            ZL = (Value bsr 8) band 16#FF,
            ZH = Value band 16#FF,
            %% 12-bit left-justified in high-resolution mode: shift right by 4
            X = sign_extend_12(((XH bsl 8) bor XL) bsr 4),
            Y = sign_extend_12(((YH bsl 8) bor YL) bsr 4),
            Z = sign_extend_12(((ZH bsl 8) bor ZL) bsr 4),
            {ok, {X, Y, Z}};
        {error, _} = Error ->
            Error
    end.

sign_extend_12(V) when V >= 16#800 -> V - 16#1000;
sign_extend_12(V) -> V.

default_pins() ->
    default_pins(atomvm:platform()).

%%         {SCK, MOSI, MISO, CS}
default_pins(pico) -> {2, 3, 4, 5};
default_pins(esp32) -> esp32_default_pins().

esp32_default_pins() ->
    #{model := Model} = erlang:system_info(esp32_chip_info),
    esp32_default_pins(Model).

%%                          {SCK, MOSI, MISO, CS}
esp32_default_pins(esp32) -> {18, 23, 19, 5};
esp32_default_pins(esp32_s2) -> {18, 23, 19, 5};
esp32_default_pins(esp32_s3) -> {18, 23, 19, 5};
esp32_default_pins(esp32_c2) -> {6, 7, 2, 10};
esp32_default_pins(esp32_c3) -> {6, 7, 2, 10};
esp32_default_pins(esp32_c5) -> {6, 7, 2, 10};
esp32_default_pins(esp32_c6) -> {6, 7, 2, 16};
esp32_default_pins(esp32_c61) -> {6, 7, 2, 16};
esp32_default_pins(_) -> {18, 23, 19, 5}.
