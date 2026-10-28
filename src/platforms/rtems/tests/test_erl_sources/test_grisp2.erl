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
    spi_smoke(),
    grisp_smoke(),
    ok.

grisp_smoke() ->
    grisp2 = grisp:platform(),
    ok = grisp:led(1, red),
    ok = grisp:led(1, off),
    ok = grisp:led(2, blue),
    ok = grisp:led_off(2),
    J1 = grisp:jumper(1),
    true = (J1 =:= high orelse J1 =:= low),
    [_, _, _, _, _] = grisp:jumpers(),
    [{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}] = grisp:spi_bus_config(),
    [{peripheral, "/dev/console"}, {speed, 115200}] = grisp:uart_config(console),
    [{peripheral, "/dev/ttyS3"}, {speed, 115200}] = grisp:uart_config(pmod),
    16#57 = grisp:i2c_address(eeprom),
    16#5F = grisp:i2c_address(eui),
    16#36 = grisp:i2c_address(crypto),
    16#18 = grisp:i2c_address(onewire),
    erlang:display({grisp2, grisp_module, ok}),
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
    high = gpio:digital_read(Pin),
    timer:sleep(500),
    ok = gpio:digital_write(Pin, low),
    low = gpio:digital_read(Pin),
    ok = gpio:set_pin_mode(Pin, output_od),
    low = gpio:digital_read(Pin),
    ok = gpio:digital_write(Pin, low),
    ok = gpio:set_pin_mode(Pin, output),
    low = gpio:digital_read(Pin),
    ok = gpio:set_pin_mode(Pin, input),
    ok = gpio:digital_write(Pin, high),
    ok = gpio:set_pin_pull(Pin, down),
    ok = gpio:set_pin_mode(Pin, output),
    high = gpio:digital_read(Pin),
    ok = gpio:deinit(Pin),
    ok = gpio:set_pin_mode(Pin, output),
    ok = gpio:digital_write(Pin, high),
    high = gpio:digital_read(Pin),
    ok = gpio:deinit(Pin),
    erlang:display({grisp2, led, ok}),
    ok.

gpio_smoke() ->
    Pin = #{path => "/jumper-keys", property => "grisp,gpios", index => 0},
    ok = gpio:init(Pin),
    ok = gpio:set_pin_pull(Pin, up),
    ok = gpio:set_pin_pull(Pin, down),
    ok = gpio:set_pin_pull(Pin, floating),
    {error, enotsup} = gpio:set_pin_pull(Pin, up_down),
    ok = gpio:set_pin_mux(Pin, 5),
    Level = gpio:digital_read(Pin),
    true = (Level =:= high orelse Level =:= low),
    erlang:display({grisp2, gpio, Level}),
    GPIO = gpio:start(),
    ok = gpio:set_int(GPIO, Pin, falling),
    ok = gpio:remove_int(GPIO, Pin),
    ok = gpio:close(GPIO),
    erlang:display({grisp2, gpio_int, ok}),
    ok = gpio:deinit(Pin),
    ok.

i2c_eeprom_smoke() ->
    {ok, Resource} = i2c:init([
        {peripheral, "/dev/i2c-0"},
        {fdt_alias, "i2c0"},
        {retries, 2}
    ]),
    ok = i2c:is_device_ready(Resource, 16#57, 1, 1000),
    {error, nack} = i2c:is_device_ready(Resource, 16#76, 1, 100),
    {ok, Addresses} = i2c:scan(Resource),
    true = lists:member(16#57, Addresses),
    {ok, [ok, ReadBytes]} = i2c:transfer(
        Resource,
        [
            {write, 16#57, <<0:8>>},
            {read, 16#57, 4}
        ],
        1000
    ),
    4 = byte_size(ReadBytes),
    {ok, Data} = i2c:mem_read(Resource, 16#57, 0, 8, 8, 1000),
    8 = byte_size(Data),
    ok = i2c:deinit(Resource),
    I2C = i2c:open([
        {peripheral, "/dev/i2c-0"},
        {fdt_alias, "i2c0"}
    ]),
    ok = i2c:is_device_ready(I2C, 16#57),
    {ok, DataHigh} = i2c:read_bytes(I2C, 16#57, 0, 4, [{mem_add_size, 8}]),
    4 = byte_size(DataHigh),
    ok = i2c:close(I2C),
    erlang:display({grisp2, eeprom, ok}),
    ok.

spi_smoke() ->
    case spi:init([{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}]) of
        {ok, Resource} ->
            erlang:display({grisp2, spi, ok}),
            ok = spi:deinit(Resource);
        {error, Reason} ->
            erlang:display({grisp2, spi, {error, Reason}})
    end.
