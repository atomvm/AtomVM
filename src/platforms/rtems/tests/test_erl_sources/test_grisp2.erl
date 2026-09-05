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

%% Hardware smoke test for a GRiSP 2 image.  This is intentionally separate
%% from test_boot: it requires the GRiSP 2 FDT and a board EEPROM.
-module(test_grisp2).
-export([start/0]).

start() ->
    erlang:display({atomvm_grisp2, atomvm:platform()}),
    uart_smoke(),
    led_smoke(),
    gpio_smoke(),
    i2c_eeprom_smoke(),
    ok.

uart_smoke() ->
    Uart = uart:open([{peripheral, "/dev/console"}, {speed, 115200}]),
    ok = uart:write(Uart, <<"grisp2-uart-ok\n">>),
    ok = uart:close(Uart),
    erlang:display({grisp2, uart, ok}),
    ok.

led_smoke() ->
    Pin = #{path => "/leds/grisp-rgb1-red", property => "gpios", index => 0},
    ok = gpio:set_pin_mode(Pin, output),
    ok = gpio:digital_write(Pin, high),
    timer:sleep(500),
    ok = gpio:digital_write(Pin, low),
    erlang:display({grisp2, led, ok}),
    ok.

gpio_smoke() ->
    Pin = #{path => "/jumper-keys", property => "grisp,gpios", index => 0},
    ok = gpio:init(Pin),
    Level = gpio:digital_read(Pin),
    true = (Level =:= high orelse Level =:= low),
    erlang:display({grisp2, gpio, Level}),
    ok = gpio:deinit(Pin),
    ok.

i2c_eeprom_smoke() ->
    {ok, Resource} = i2c:init([
        {peripheral, "/dev/i2c-0"},
        {fdt_alias, "i2c0"}
    ]),
    {ok, Data} = i2c:mem_read(Resource, 16#57, 0, 8, 8, 1000),
    8 = byte_size(Data),
    erlang:display({grisp2, eeprom, ok}),
    ok = i2c:deinit(Resource),
    ok.
