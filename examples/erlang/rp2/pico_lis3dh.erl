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
%% @doc LIS3DH accelerometer example for Pico.
%%
%% Reads X, Y, Z acceleration from a LIS3DH connected via I2C and prints
%% the values every second.
%%
%% Default wiring (Pico pin numbers):
%%   SDA -> GP4 (pin 6)
%%   SCL -> GP5 (pin 7)
%%
%% These are the default I2C0 pins on the Pico.
%% @end
%%-----------------------------------------------------------------------------
-module(pico_lis3dh).
-export([start/0]).

%% I2C pins (I2C0 default on Pico)
-define(SDA_PIN, 4).
-define(SCL_PIN, 5).

%% LIS3DH I2C address (0x18 when SDO/SA0 is low, 0x19 when high)
-define(LIS3DH_ADDR, 16#19).

%% LIS3DH registers
-define(WHO_AM_I, 16#0F).
-define(CTRL_REG1, 16#20).
-define(CTRL_REG4, 16#23).
-define(OUT_X_L, 16#28).

%% Expected WHO_AM_I response
-define(LIS3DH_ID, 16#33).

start() ->
    I2C = i2c:open([
        {scl, ?SCL_PIN},
        {sda, ?SDA_PIN},
        {clock_speed_hz, 400000},
        {peripheral, 0}
    ]),
    case check_who_am_i(I2C) of
        ok ->
            configure(I2C),
            loop(I2C);
        {error, Reason} ->
            console:puts("LIS3DH not found: "),
            console:puts(erlang:atom_to_list(Reason)),
            console:puts("\n")
    end.

check_who_am_i(I2C) ->
    case i2c:read_bytes(I2C, ?LIS3DH_ADDR, ?WHO_AM_I, 1) of
        {ok, <<?LIS3DH_ID:8>>} ->
            console:puts("LIS3DH detected\n"),
            ok;
        {ok, <<Other:8>>} ->
            console:puts("Unexpected WHO_AM_I: "),
            console:puts(erlang:integer_to_list(Other, 16)),
            console:puts("\n"),
            {error, unexpected_id};
        {error, _} = Error ->
            Error
    end.

configure(I2C) ->
    %% CTRL_REG1: 50 Hz ODR, normal mode, X/Y/Z enabled
    %% Bits: ODR=0100 LPen=0 Zen=1 Yen=1 Xen=1 -> 0x47
    ok = i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG1, 16#47),
    %% CTRL_REG4: +/- 2g full scale, high resolution
    %% Bits: BDU=1 BLE=0 FS=00 HR=1 ST=00 SIM=0 -> 0x88
    ok = i2c:write_bytes(I2C, ?LIS3DH_ADDR, ?CTRL_REG4, 16#88).

loop(I2C) ->
    case read_acceleration(I2C) of
        {ok, {X, Y, Z}} ->
            console:puts("X="),
            console:puts(erlang:integer_to_list(X)),
            console:puts(" Y="),
            console:puts(erlang:integer_to_list(Y)),
            console:puts(" Z="),
            console:puts(erlang:integer_to_list(Z)),
            console:puts("\n");
        {error, Reason} ->
            console:puts("Read error: "),
            console:puts(erlang:atom_to_list(Reason)),
            console:puts("\n")
    end,
    timer:sleep(1000),
    loop(I2C).

read_acceleration(I2C) ->
    %% Read 6 bytes starting at OUT_X_L with auto-increment (bit 7 set)
    case i2c:read_bytes(I2C, ?LIS3DH_ADDR, ?OUT_X_L bor 16#80, 6) of
        {ok, <<XL:8, XH:8/signed, YL:8, YH:8/signed, ZL:8, ZH:8/signed>>} ->
            %% 12-bit left-justified in high-resolution mode: shift right by 4
            X = ((XH bsl 8) bor XL) bsr 4,
            Y = ((YH bsl 8) bor YL) bsr 4,
            Z = ((ZH bsl 8) bor ZL) bsr 4,
            {ok, {X, Y, Z}};
        {error, _} = Error ->
            Error
    end.
