%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Davide Bettio <davide@uninstall.it>
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

-module(sht31).
-export([start/0, read/1]).

-define(SHT31_BASE_ADDR, 16#44).
-define(SHT31_MEAS_HIGH_REP, 16#2400).

start() ->
    I2C = i2c:open([{scl, 15}, {sda, 4}, {clock_speed_hz, 1000000}]),
    loop(I2C).

loop(I2C) ->
    Val = read(I2C),
    erlang:display(Val),
    timer:sleep(10000),
    loop(I2C).

read(I2C) ->
    {ok, Bin} = read_sensor(I2C),
    parse_bin(Bin).

parse_bin(B) ->
    IntTemp = (binary:at(B, 0) bsl 8) bor binary:at(B, 1),
    Temp = float_temp(IntTemp, 0.01),
    IntHum = (binary:at(B, 3) bsl 8) bor binary:at(B, 4),
    Hum = float_hum(IntHum, 0.01),
    {ok, Temp, Hum}.

float_temp(IntTemp, S) ->
    (((4375 * IntTemp) bsr 14) - 4500) * S.

float_hum(IntHum, S) ->
    ((625 * IntHum) bsr 12) * S.

read_sensor(I2C) ->
    send_command(I2C, ?SHT31_MEAS_HIGH_REP),
    timer:sleep(20),
    i2c:read_bytes(I2C, ?SHT31_BASE_ADDR, 6).

send_command(I2C, Command) ->
    i2c:begin_transmission(I2C, ?SHT31_BASE_ADDR),
    i2c:write_byte(I2C, (Command bsr 8) band 16#FF),
    i2c:write_byte(I2C, Command band 16#FF),
    i2c:end_transmission(I2C).
