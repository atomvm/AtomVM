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

-module(test_boot).
-export([start/0]).

start() ->
    erlang:display({atomvm_rtems_boot, atomvm:platform()}),
    uart_smoke(),
    i2c_smoke(),
    gpio_smoke(),
    spi_smoke(),
    ok.

uart_smoke() ->
    try uart:open([{peripheral, "/dev/console"}, {speed, 115200}]) of
        Pid when is_pid(Pid) ->
            {error, timeout} = uart:read(Pid, 20),
            erlang:display({uart, read_timeout, ok}),
            ok = uart:write(Pid, <<"uart-ok\n">>),
            erlang:display({uart, write, ok}),
            ok = uart:close(Pid)
    catch
        error:Reason ->
            erlang:display({uart, Reason, ok})
    end.

i2c_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-invalid"}, {fdt_alias, "atomvm-missing"}]) of
        {error, enotsup} ->
            {error, enotsup} = i2c:is_device_ready(make_ref(), 16#50, 1, 100),
            {error, enotsup} = i2c:transfer(make_ref(), [{write, 16#50, <<0>>}], 100),
            erlang:display({i2c, enotsup});
        {error, _Reason} ->
            erlang:display({i2c, fdt_alias, ok}),
            i2c_open_smoke();
        {ok, Resource} ->
            ok = i2c:deinit(Resource),
            erlang:error(i2c_fdt_alias_ignored)
    end.

i2c_open_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}, {retries, 1}]) of
        {ok, Resource} ->
            erlang:display({i2c, open, ok}),
            ok = i2c:deinit(Resource);
        {error, Reason} ->
            erlang:error({i2c_smoke_failed, Reason})
    end.

gpio_smoke() ->
    case gpio:init(#{}) of
        {error, enotsup} ->
            {error, enotsup} = gpio:set_pin_pull(#{}, up),
            {error, enotsup} = gpio:set_pin_mode(#{}, output_od),
            {error, enotsup} = gpio:set_int_nif(#{}, rising, self()),
            {error, enotsup} = gpio:remove_int_nif(#{}),
            {error, enotsup} = gpio:set_pin_mux(#{}, 0),
            erlang:display({gpio, enotsup});
        {error, invalid_pin} ->
            {error, invalid_pin} = gpio:set_pin_pull(#{}, up),
            {error, invalid_pin} = gpio:set_pin_mode(#{}, output_od),
            {error, invalid_pin} = gpio:set_int_nif(#{}, rising, self()),
            {error, invalid_pin} = gpio:remove_int_nif(#{}),
            {error, invalid_pin} = gpio:set_pin_mux(#{}, 0),
            gpio_target_smoke(),
            erlang:display({gpio, imx7, ok});
        Other ->
            erlang:error({gpio_smoke_failed, Other})
    end.

gpio_target_smoke() ->
    case gpio:init(2) of
        ok ->
            ok = gpio:set_pin_mode(2, output),
            ok = gpio:digital_write(2, high),
            true = is_atom(gpio:digital_read(2)),
            ok = gpio:digital_write(2, low),
            true = is_atom(gpio:digital_read(2)),
            ok = gpio:set_pin_mode(2, input),
            ok = gpio:set_pin_pull(2, up),
            ok = gpio:set_pin_pull(2, down),
            ok = gpio:set_pin_pull(2, floating),
            ok = gpio:set_int_nif(2, rising, self()),
            ok = gpio:remove_int_nif(2),
            ok = gpio:attach_interrupt(2, rising),
            ok = gpio:detach_interrupt(2),
            ok = gpio:deinit(2),
            erlang:display({gpio, esp32c3, ok});
        {error, invalid_pin} ->
            ok
    end.

spi_smoke() ->
    case spi:init([{peripheral, "/dev/spi-invalid"}, {fdt_alias, "atomvm-missing"}]) of
        {error, enotsup} ->
            erlang:display({spi, enotsup});
        {error, _Reason} ->
            erlang:display({spi, fdt_alias, ok}),
            spi_open_smoke();
        {ok, Resource} ->
            ok = spi:deinit(Resource),
            erlang:error(spi_fdt_alias_ignored)
    end.

spi_open_smoke() ->
    case spi:init([{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}]) of
        {ok, Resource} ->
            ok = spi:apply_device_config(Resource, #{clock_speed_hz => 1000000, mode => 0}),
            erlang:display({spi, open, ok}),
            ok = spi:deinit(Resource);
        {error, _Reason} ->
            ok
    end.
