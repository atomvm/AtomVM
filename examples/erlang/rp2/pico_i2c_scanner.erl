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
%% @doc I2C bus scanner example for Pico.
%%
%% Scans all valid 7-bit I2C addresses (0x08-0x77) and prints which devices
%% respond with an ACK.
%%
%% Default wiring (Pico pin numbers):
%%   SDA -> GP4 (pin 6)
%%   SCL -> GP5 (pin 7)
%%   GND -> pin 8
%%   3V3 -> pin 36
%%
%% These are the default I2C0 pins on the Pico.
%% @end
%%-----------------------------------------------------------------------------
-module(pico_i2c_scanner).
-export([start/0]).

%% I2C pins (I2C0 default on Pico)
-define(SDA_PIN, 4).
-define(SCL_PIN, 5).

start() ->
    I2C = i2c:open([
        {scl, ?SCL_PIN},
        {sda, ?SDA_PIN},
        {clock_speed_hz, 100000},
        {peripheral, 0}
    ]),
    console:puts("I2C bus scan (0x08-0x77):\n"),
    Found = scan(I2C, 16#08, []),
    case Found of
        [] ->
            console:puts("No devices found.\n");
        _ ->
            console:puts("Done. Found "),
            console:puts(erlang:integer_to_list(length(Found))),
            console:puts(" device(s).\n")
    end.

scan(_I2C, Addr, Acc) when Addr > 16#77 ->
    lists:reverse(Acc);
scan(I2C, Addr, Acc) ->
    NewAcc =
        case i2c:read_bytes(I2C, Addr, 1) of
            {ok, _} ->
                console:puts("  0x"),
                console:puts(erlang:integer_to_list(Addr, 16)),
                console:puts(" ACK\n"),
                [Addr | Acc];
            {error, _} ->
                Acc
        end,
    scan(I2C, Addr + 1, NewAcc).
