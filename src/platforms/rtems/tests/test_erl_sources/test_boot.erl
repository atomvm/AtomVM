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
    ok.

uart_smoke() ->
    Pid = uart:open([{peripheral, "/dev/console"}, {speed, 115200}]),
    true = is_pid(Pid),
    {error, timeout} = uart:read(Pid, 20),
    erlang:display({uart, read_timeout, ok}),
    ok = uart:write(Pid, <<"uart-ok\n">>),
    erlang:display({uart, write, ok}),
    ok = uart:close(Pid).

i2c_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-invalid"}, {fdt_alias, "atomvm-missing"}]) of
        {error, enotsup} ->
            erlang:display({i2c, enotsup});
        {error, _Reason} ->
            erlang:display({i2c, fdt_alias, ok}),
            i2c_open_smoke();
        {ok, Resource} ->
            ok = i2c:deinit(Resource),
            erlang:error(i2c_fdt_alias_ignored)
    end.

i2c_open_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}]) of
        {ok, Resource} ->
            erlang:display({i2c, open, ok}),
            ok = i2c:deinit(Resource);
        {error, Reason} ->
            erlang:error({i2c_smoke_failed, Reason});
        Other ->
            erlang:error({i2c_smoke_failed, Other})
    end.

gpio_smoke() ->
    case gpio:init({8, 0}) of
        {error, enotsup} ->
            erlang:display({gpio, enotsup});
        {error, invalid_pin} ->
            erlang:display({gpio, imx7, ok});
        Other ->
            erlang:error({gpio_smoke_failed, Other})
    end.
