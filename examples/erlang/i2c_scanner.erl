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
%% @doc I2C bus scanner example.
%%
%% Scans all valid 7-bit I2C addresses (0x08-0x77) and prints which devices
%% respond with an ACK.
%%
%% Default pins are auto-detected from the platform and chip model:
%%
%% Pico (I2C0):            SDA=GP4, SCL=GP5
%% ESP32/S2/S3 (I2C0):     SDA=21,  SCL=22
%% ESP32-C2/C3/C5 (I2C0):  SDA=8,   SCL=9
%% ESP32-C6/C61 (I2C0):    SDA=8,   SCL=9
%% @end
%%-----------------------------------------------------------------------------
-module(i2c_scanner).
-export([start/0]).

start() ->
    Config = default_config(),
    I2C = i2c:open([{clock_speed_hz, 100000} | Config]),
    io:format("I2C bus scan (0x08-0x77):~n"),
    Found = scan(I2C, 16#08, []),
    case Found of
        [] ->
            io:format("No devices found.~n");
        _ ->
            io:format("Done. Found ~B device(s).~n", [length(Found)])
    end.

scan(_I2C, Addr, Acc) when Addr > 16#77 ->
    lists:reverse(Acc);
scan(I2C, Addr, Acc) ->
    NewAcc =
        case i2c:read_bytes(I2C, Addr, 1) of
            {ok, _} ->
                io:format("  0x~.16B ACK~n", [Addr]),
                [Addr | Acc];
            {error, _} ->
                Acc
        end,
    scan(I2C, Addr + 1, NewAcc).

default_config() ->
    default_config(atomvm:platform()).

default_config(pico) -> [{sda, 4}, {scl, 5}, {peripheral, 0}];
default_config(esp32) -> esp32_default_config().

esp32_default_config() ->
    #{model := Model} = erlang:system_info(esp32_chip_info),
    esp32_default_config(Model).

%%                                  {SDA, SCL}
esp32_default_config(esp32) -> [{sda, 21}, {scl, 22}, {peripheral, "i2c0"}];
esp32_default_config(esp32_s2) -> [{sda, 21}, {scl, 22}, {peripheral, "i2c0"}];
esp32_default_config(esp32_s3) -> [{sda, 21}, {scl, 22}, {peripheral, "i2c0"}];
esp32_default_config(esp32_c2) -> [{sda, 8}, {scl, 9}, {peripheral, "i2c0"}];
esp32_default_config(esp32_c3) -> [{sda, 8}, {scl, 9}, {peripheral, "i2c0"}];
esp32_default_config(esp32_c5) -> [{sda, 8}, {scl, 9}, {peripheral, "i2c0"}];
esp32_default_config(esp32_c6) -> [{sda, 8}, {scl, 9}, {peripheral, "i2c0"}];
esp32_default_config(esp32_c61) -> [{sda, 8}, {scl, 9}, {peripheral, "i2c0"}];
esp32_default_config(_) -> [{sda, 21}, {scl, 22}, {peripheral, "i2c0"}].
