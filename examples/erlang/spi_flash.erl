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
%% @doc SPI flash JEDEC ID reader example.
%%
%% Reads the JEDEC ID and Status Register 1 from a standard SPI flash chip
%% (W25Qxx or similar) and prints the results every 5 seconds.
%%
%% Default pins are auto-detected from the platform and chip model:
%%
%% Pico (SPI0):          SCK=GP2, MOSI=GP3, MISO=GP4, CS=GP5
%% ESP32/S2/S3 (SPI2):   SCK=18,  MOSI=23,  MISO=19,  CS=5
%% ESP32-C2/C3/C5 (SPI2): SCK=6,  MOSI=7,   MISO=2,   CS=10
%% ESP32-C6/C61 (SPI2):  SCK=6,   MOSI=7,   MISO=2,   CS=16
%%
%% Note: some breakout boards label pins using QSPI convention where
%% D0 is MISO and D1 is MOSI (opposite of DI/DO). If you read all
%% zeros, try swapping D0 and D1.
%%
%% The flash command byte is sent as the "address" parameter of read_at/4,
%% using address_len_bits => 8 and command_len_bits => 0.
%% @end
%%-----------------------------------------------------------------------------
-module(spi_flash).
-export([start/0]).

%% SPI flash commands
-define(CMD_JEDEC_ID, 16#9F).
-define(CMD_READ_STATUS1, 16#05).

start() ->
    {SCK, MOSI, MISO, CS} = default_pins(),
    SPI = spi:open([
        {bus_config, [
            {sclk, SCK},
            {mosi, MOSI},
            {miso, MISO}
        ]},
        {device_config, [
            {flash, [
                {cs, CS},
                {clock_speed_hz, 1000000},
                {mode, 0},
                {address_len_bits, 8},
                {command_len_bits, 0}
            ]}
        ]}
    ]),
    loop(SPI).

loop(SPI) ->
    read_jedec_id(SPI),
    read_status(SPI),
    timer:sleep(5000),
    loop(SPI).

read_jedec_id(SPI) ->
    case spi:read_at(SPI, flash, ?CMD_JEDEC_ID, 24) of
        {ok, Value} ->
            Manufacturer = (Value bsr 16) band 16#FF,
            MemType = (Value bsr 8) band 16#FF,
            Capacity = Value band 16#FF,
            io:format(
                "JEDEC ID: manufacturer=0x~2.16.0B mem_type=0x~2.16.0B capacity=0x~2.16.0B~n", [
                    Manufacturer, MemType, Capacity
                ]
            ),
            case manufacturer_name(Manufacturer) of
                unknown -> ok;
                Name -> io:format("  Manufacturer: ~s~n", [Name])
            end;
        {error, Reason} ->
            io:format("JEDEC ID read error: ~p~n", [Reason])
    end.

read_status(SPI) ->
    case spi:read_at(SPI, flash, ?CMD_READ_STATUS1, 8) of
        {ok, Status} ->
            Busy = Status band 1,
            Wel = (Status bsr 1) band 1,
            io:format("Status Register 1: 0x~2.16.0B (BUSY=~B WEL=~B)~n", [
                Status, Busy, Wel
            ]);
        {error, Reason} ->
            io:format("Status read error: ~p~n", [Reason])
    end.

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

manufacturer_name(16#EF) -> "Winbond";
manufacturer_name(16#C8) -> "GigaDevice";
manufacturer_name(16#20) -> "Micron";
manufacturer_name(16#01) -> "Spansion/Cypress";
manufacturer_name(16#1F) -> "Adesto/Atmel";
manufacturer_name(16#BF) -> "Microchip/SST";
manufacturer_name(16#9D) -> "ISSI";
manufacturer_name(_) -> unknown.
