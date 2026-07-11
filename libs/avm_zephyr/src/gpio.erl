%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc AtomVM GPIO interface for Zephyr.
%%
%% An integer pin uses the `atomvm,gpio' chosen controller, falling back to
%% `gpio0'. `{Controller, Pin}' selects a controller by index or device name.
-module(gpio).

-behaviour(gpio_hal).

-export([
    start/0, open/0, close/1, stop/0,
    read/2, set_direction/3, set_level/3,
    set_int/3, set_int/4, remove_int/2
]).
-export([
    init/1, deinit/1, set_pin_mode/2, set_pin_pull/2,
    digital_write/2, digital_read/1, attach_interrupt/3, detach_interrupt/1
]).

-type controller() :: non_neg_integer() | string() | binary().
-type pin() :: non_neg_integer() | {controller(), non_neg_integer()}.
-type gpio() :: pid().
-type direction() :: input | output | output_od.
-type pull() :: up | down | up_down | floating.
-type level() :: low | high | 0 | 1.
-type trigger() :: none | rising | falling | both | low | high.

-spec start() -> gpio().
start() ->
    case whereis(gpio) of
        undefined -> open();
        GPIO -> GPIO
    end.

-spec open() -> gpio().
open() ->
    Pid = spawn(fun() -> loop([]) end),
    register(gpio, Pid),
    Pid.

-spec close(gpio()) -> ok | {error, term()}.
close(GPIO) when is_pid(GPIO) ->
    call(GPIO, close).

-spec stop() -> ok | {error, term()}.
stop() ->
    case whereis(gpio) of
        undefined -> ok;
        GPIO -> close(GPIO)
    end.

-spec read(gpio(), pin()) -> high | low | {error, term()}.
read(_GPIO, Pin) -> ?MODULE:digital_read(Pin).

-spec set_direction(gpio(), pin(), direction()) -> ok | {error, term()}.
set_direction(_GPIO, Pin, Direction) -> ?MODULE:set_pin_mode(Pin, Direction).

-spec set_level(gpio(), pin(), level()) -> ok | {error, term()}.
set_level(_GPIO, Pin, Level) -> ?MODULE:digital_write(Pin, Level).

-spec set_int(gpio(), pin(), trigger()) -> ok | {error, term()}.
set_int(GPIO, Pin, Trigger) -> call(GPIO, {set_int, Pin, Trigger, self()}).

-spec set_int(gpio(), pin(), trigger(), pid()) -> ok | {error, term()}.
set_int(GPIO, Pin, Trigger, Pid) -> call(GPIO, {set_int, Pin, Trigger, Pid}).

-spec remove_int(gpio(), pin()) -> ok | {error, term()}.
remove_int(GPIO, Pin) -> call(GPIO, {remove_int, Pin}).

-spec init(pin()) -> ok | {error, term()}.
init(_Pin) -> erlang:nif_error(undefined).

-spec deinit(pin()) -> ok | {error, term()}.
deinit(_Pin) -> erlang:nif_error(undefined).

-spec set_pin_mode(pin(), direction()) -> ok | {error, term()}.
set_pin_mode(_Pin, _Direction) -> erlang:nif_error(undefined).

-spec set_pin_pull(pin(), pull()) -> ok | {error, term()}.
set_pin_pull(_Pin, _Pull) -> erlang:nif_error(undefined).

-spec digital_write(pin(), level()) -> ok | {error, term()}.
digital_write(_Pin, _Level) -> erlang:nif_error(undefined).

-spec digital_read(pin()) -> high | low | {error, term()}.
digital_read(_Pin) -> erlang:nif_error(undefined).

-spec attach_interrupt(pin(), trigger(), pid()) -> ok | {error, term()}.
attach_interrupt(_Pin, _Trigger, _Pid) -> erlang:nif_error(undefined).

-spec detach_interrupt(pin()) -> ok | {error, term()}.
detach_interrupt(_Pin) -> erlang:nif_error(undefined).

call(GPIO, Request) ->
    Ref = make_ref(),
    GPIO ! {'$call', {self(), Ref}, Request},
    receive
        {Ref, Reply} -> Reply
    after 5000 ->
        {error, timeout}
    end.

loop(Pins) ->
    receive
        {'$call', {From, Ref}, close} ->
            lists:foreach(fun(Pin) -> ?MODULE:detach_interrupt(Pin) end, Pins),
            unregister(gpio),
            From ! {Ref, ok};
        {'$call', {From, Ref}, {set_int, Pin, none, _Pid}} ->
            Result = ?MODULE:detach_interrupt(Pin),
            From ! {Ref, Result},
            loop(lists:delete(Pin, Pins));
        {'$call', {From, Ref}, {set_int, Pin, Trigger, Pid}} ->
            Result = ?MODULE:attach_interrupt(Pin, Trigger, Pid),
            From ! {Ref, Result},
            NextPins = case Result of ok -> [Pin | lists:delete(Pin, Pins)]; _ -> Pins end,
            loop(NextPins);
        {'$call', {From, Ref}, {remove_int, Pin}} ->
            Result = ?MODULE:detach_interrupt(Pin),
            From ! {Ref, Result},
            loop(lists:delete(Pin, Pins))
    end.
