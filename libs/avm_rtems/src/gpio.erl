%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
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

%%-----------------------------------------------------------------------------
%% @doc GPIO implementation for the RTEMS i.MX7 BSP.
%%
%% Pins are identified either as `{Bank, Pin}', where Bank is 1..7 and Pin is
%% 0..31, or by an FDT property map:
%% `#{path => Path, property => Property, index => Index}'. The direct form
%% requires the pin mux to have already been configured by the BSP or FDT.
%% GPIO is unsupported on erc32 and returns `{error, enotsup}'. Pull resistors,
%% open-drain output, and interrupts are not currently supported.
%% @end
%%-----------------------------------------------------------------------------
-module(gpio).

-behaviour(gpio_hal).

-export([
    start/0,
    open/0,
    close/1,
    stop/0,
    read/2,
    set_direction/3,
    set_level/3,
    set_int/3,
    set_int/4,
    remove_int/2
]).
-export([
    init/1,
    deinit/1,
    set_pin_mode/2,
    set_pin_pull/2,
    digital_write/2,
    digital_read/1
]).

-type pin() :: {1..7, 0..31} | map().
-type direction() :: input | output | output_od.
-type pull() :: up | down | up_down | floating.
-type level() :: low | 0 | high | 1.
-type trigger() :: none | rising | falling | both | low | high.
-type gpio() :: pid().

-export_type([pin/0, gpio/0]).

-spec start() -> gpio().
start() ->
    case whereis(gpio) of
        undefined -> open();
        GPIO -> GPIO
    end.

-spec open() -> gpio().
open() ->
    Pid = spawn(fun gpio_loop/0),
    register(gpio, Pid),
    Pid.

-spec close(GPIO :: gpio()) -> ok | {error, timeout}.
close(GPIO) ->
    Ref = make_ref(),
    GPIO ! {'$call', {self(), Ref}, close},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

-spec stop() -> ok | {error, timeout}.
stop() ->
    case whereis(gpio) of
        undefined -> ok;
        GPIO -> close(GPIO)
    end.

-spec read(GPIO :: gpio(), Pin :: pin()) -> high | low | {error, atom()} | error.
read(_GPIO, Pin) ->
    digital_read(Pin).

-spec set_direction(GPIO :: gpio(), Pin :: pin(), Direction :: direction()) ->
    ok | {error, atom()} | error.
set_direction(_GPIO, Pin, Direction) ->
    set_pin_mode(Pin, Direction).

-spec set_level(GPIO :: gpio(), Pin :: pin(), Level :: level()) ->
    ok | {error, atom()} | error.
set_level(_GPIO, Pin, Level) ->
    digital_write(Pin, Level).

-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger()) -> {error, enotsup}.
set_int(_GPIO, _Pin, _Trigger) ->
    {error, enotsup}.

-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger(), Pid :: pid()) ->
    {error, enotsup}.
set_int(_GPIO, _Pin, _Trigger, _Pid) ->
    {error, enotsup}.

-spec remove_int(GPIO :: gpio(), Pin :: pin()) -> {error, enotsup}.
remove_int(_GPIO, _Pin) ->
    {error, enotsup}.

-spec init(Pin :: pin()) -> ok | {error, atom()}.
init(_Pin) ->
    erlang:nif_error(undefined).

-spec deinit(Pin :: pin()) -> ok | {error, atom()}.
deinit(_Pin) ->
    erlang:nif_error(undefined).

-spec set_pin_mode(Pin :: pin(), Direction :: direction()) ->
    ok | {error, atom()} | error.
set_pin_mode(_Pin, _Direction) ->
    erlang:nif_error(undefined).

-spec set_pin_pull(Pin :: pin(), Pull :: pull()) -> ok | {error, atom()} | error.
set_pin_pull(_Pin, _Pull) ->
    erlang:nif_error(undefined).

-spec digital_write(Pin :: pin(), Level :: level()) -> ok | {error, atom()} | error.
digital_write(_Pin, _Level) ->
    erlang:nif_error(undefined).

-spec digital_read(Pin :: pin()) -> high | low | {error, atom()} | error.
digital_read(_Pin) ->
    erlang:nif_error(undefined).

gpio_loop() ->
    receive
        {'$call', {Pid, Ref}, close} ->
            unregister(gpio),
            Pid ! {Ref, ok}
    end.
