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
%% @doc GPIO implementation for the RTEMS platform (i.MX7 / GRiSP 2).
%%
%% Pins are identified either as `{Bank, Pin}', where Bank is 1..7 and Pin is
%% 0..31, or by an FDT property map:
%% `#{path => Path, property => Property, index => Index}'.
%%
%% Supports basic digital I/O, configurable pull resistors (up, down, floating),
%% open-drain output (output_od), hardware interrupts with notification messages
%% (`{gpio_interrupt, Pin}'), and dynamic pinmuxing.
%%
%% GPIO on erc32 and RealView returns `{error, enotsup}'.
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
    set_pin_mux/2,
    digital_write/2,
    digital_read/1,
    attach_interrupt/2,
    detach_interrupt/1,
    set_int_nif/3,
    remove_int_nif/1
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
    Pid = spawn(fun() -> gpio_loop(#{}) end),
    register(gpio, Pid),
    Pid.

-spec close(GPIO :: gpio()) -> ok | {error, timeout}.
close(GPIO) ->
    call(GPIO, close).

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

-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger()) ->
    ok | {error, atom()} | error.
set_int(GPIO, Pin, Trigger) ->
    set_int(GPIO, Pin, Trigger, self()).

-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger(), Pid :: pid()) ->
    ok | {error, atom()} | error.
set_int(GPIO, Pin, Trigger, Pid) when is_pid(Pid) ->
    call(GPIO, {set_int, Pin, Trigger, Pid}).

-spec remove_int(GPIO :: gpio(), Pin :: pin()) -> ok | {error, atom()} | error.
remove_int(GPIO, Pin) ->
    call(GPIO, {remove_int, Pin}).

-spec attach_interrupt(Pin :: pin(), Trigger :: trigger()) ->
    ok | {error, atom()} | error.
attach_interrupt(Pin, Trigger) ->
    set_int(start(), Pin, Trigger).

-spec detach_interrupt(Pin :: pin()) -> ok | {error, atom()} | error.
detach_interrupt(Pin) ->
    remove_int(start(), Pin).

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

-spec set_pin_mux(Pin :: pin(), Mux :: non_neg_integer()) -> ok | {error, atom()} | error.
set_pin_mux(_Pin, _Mux) ->
    erlang:nif_error(undefined).

-spec digital_write(Pin :: pin(), Level :: level()) -> ok | {error, atom()} | error.
digital_write(_Pin, _Level) ->
    erlang:nif_error(undefined).

-spec digital_read(Pin :: pin()) -> high | low | {error, atom()} | error.
digital_read(_Pin) ->
    erlang:nif_error(undefined).

-spec set_int_nif(Pin :: pin(), Trigger :: trigger(), Pid :: pid()) ->
    ok | {error, atom()} | error.
set_int_nif(_Pin, _Trigger, _Pid) ->
    erlang:nif_error(undefined).

-spec remove_int_nif(Pin :: pin()) -> ok | {error, atom()} | error.
remove_int_nif(_Pin) ->
    erlang:nif_error(undefined).

call(GPIO, Request) ->
    Ref = make_ref(),
    GPIO ! {'$call', {self(), Ref}, Request},
    receive
        {Ref, Result} -> Result
    after 5000 ->
        {error, timeout}
    end.

gpio_loop(Listeners) ->
    receive
        {'$call', {Pid, Ref}, close} ->
            maps:foreach(
                fun(Pin, {_, MonRef}) ->
                    remove_int_nif(Pin),
                    demonitor(MonRef, [flush])
                end,
                Listeners
            ),
            unregister(gpio),
            Pid ! {Ref, ok};
        {'$call', {Pid, Ref}, {set_int, Pin, none, _TargetPid}} ->
            Res = remove_int_nif(Pin),
            NewListeners =
                case maps:find(Pin, Listeners) of
                    {ok, {_, MonRef}} ->
                        demonitor(MonRef, [flush]),
                        maps:remove(Pin, Listeners);
                    error ->
                        Listeners
                end,
            Pid ! {Ref, Res},
            gpio_loop(NewListeners);
        {'$call', {Pid, Ref}, {set_int, Pin, Trigger, TargetPid}} ->
            case set_int_nif(Pin, Trigger, self()) of
                ok ->
                    case maps:find(Pin, Listeners) of
                        {ok, {_, PrevMon}} ->
                            demonitor(PrevMon, [flush]);
                        error ->
                            ok
                    end,
                    MonRef = monitor(process, TargetPid),
                    NewListeners = maps:put(Pin, {TargetPid, MonRef}, Listeners),
                    Pid ! {Ref, ok},
                    gpio_loop(NewListeners);
                {error, Reason} ->
                    Pid ! {Ref, {error, Reason}},
                    gpio_loop(Listeners)
            end;
        {'$call', {Pid, Ref}, {remove_int, Pin}} ->
            Res = remove_int_nif(Pin),
            NewListeners =
                case maps:find(Pin, Listeners) of
                    {ok, {_, MonRef}} ->
                        demonitor(MonRef, [flush]),
                        maps:remove(Pin, Listeners);
                    error ->
                        Listeners
                end,
            Pid ! {Ref, Res},
            gpio_loop(NewListeners);
        {gpio_interrupt, DriverPin} ->
            notify_listeners(DriverPin, Listeners),
            gpio_loop(Listeners);
        {'DOWN', MonRef, process, _Pid, _Reason} ->
            NewListeners = maps:filter(
                fun(Pin, {_, Ref}) ->
                    if
                        Ref =:= MonRef ->
                            remove_int_nif(Pin),
                            false;
                        true ->
                            true
                    end
                end,
                Listeners
            ),
            gpio_loop(NewListeners)
    end.

notify_listeners(DriverPin, Listeners) ->
    case maps:find(DriverPin, Listeners) of
        {ok, {TargetPid, _}} ->
            TargetPid ! {gpio_interrupt, DriverPin};
        error ->
            maps:foreach(
                fun(Pin, {TargetPid, _}) ->
                    TargetPid ! {gpio_interrupt, Pin}
                end,
                Listeners
            )
    end.
