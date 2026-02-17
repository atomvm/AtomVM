%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Davide Bettio <davide@uninstall.it>
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
%% @doc GPIO driver module for RP2 (Pico)
%%
%% This module provides functions for interacting with micro-controller GPIO
%% (General Purpose Input and Output) pins on the RP2 platform.
%%
%% The port-based API (start/0, open/0, read/2, set_direction/3, set_level/3)
%% is implemented as a wrapper around NIFs. Interrupt functions (set_int/3,4
%% and remove_int/2) are not supported on this platform and return
%% `{error, not_supported}'.
%% @end
%%-----------------------------------------------------------------------------
-module(gpio).

-behaviour(gpio_hal).

-export([
    start/0,
    open/0,
    read/2,
    set_direction/3,
    set_level/3,
    set_int/3, set_int/4,
    remove_int/2,
    stop/0,
    close/1
]).
-export([
    init/1,
    deinit/1,
    set_pin_mode/2,
    set_pin_pull/2,
    digital_write/2,
    digital_read/1
]).

-type pin() :: non_neg_integer() | pin_tuple().
%% The pin definition for RP2040 is a non-negative integer. A tuple is used for the extra "WL" pins on the Pico-W.
-type pin_tuple() :: {wl, 0..2}.
%% The extra "WL" pins on Pico-W use bank `wl'.
-type direction() :: input | output | output_od.
%% The direction is used to set the mode of operation for a GPIO pin, either as an input, an output, or output with open drain.
-type pull() :: up | down | up_down | floating.
%% Internal resistor pull mode.
-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
%% Valid pin levels can be atom or binary representation.
-type gpio() :: pid().
%% This is the pid returned by `gpio:start/0'. Unlike ESP32 and STM32, this
%% is not a real port but a process wrapping NIF calls.
-type trigger() :: none | rising | falling | both | low | high.
%% Event type that will trigger a `gpio_interrupt'.

%%-----------------------------------------------------------------------------
%% @returns Pid | error | {error, Reason}
%% @doc     Start the GPIO driver
%%
%%          Returns the pid of the active GPIO driver process, otherwise the GPIO
%%          driver process will be started and registered as `gpio'. The use of
%%          `gpio:open/0' or `gpio:start/0' is required before using any functions
%%          that require a GPIO pid as a parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> gpio() | {error, Reason :: atom()} | error.
start() ->
    case whereis(gpio) of
        undefined ->
            open();
        GPIO ->
            GPIO
    end.

%%-----------------------------------------------------------------------------
%% @returns Pid | error | {error, Reason}
%% @doc     Start the GPIO driver
%%
%%          The GPIO driver process will be started and registered as `gpio'. If the
%%          process has already been started through `gpio:open/0' or
%%          `gpio:start/0' the command will fail.
%% @end
%%-----------------------------------------------------------------------------
-spec open() -> gpio() | {error, Reason :: atom()} | error.
open() ->
    Pid = spawn(fun gpio_loop/0),
    register(gpio, Pid),
    Pid.

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from gpio:start/0
%% @returns ok | error | {error, Reason}
%% @doc     Stop the GPIO driver
%% @end
%%-----------------------------------------------------------------------------
-spec close(GPIO :: gpio()) -> ok | {error, Reason :: atom()} | error.
close(GPIO) ->
    Ref = make_ref(),
    GPIO ! {'$call', {self(), Ref}, {close}},
    receive
        {Ref, Result} -> Result
    end.

%%-----------------------------------------------------------------------------
%% @returns ok | error | {error, Reason}
%% @doc     Stop the GPIO driver
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: atom()} | error.
stop() ->
    case whereis(gpio) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            close(Pid)
    end.

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from gpio:start/0
%% @param   Pin number of the pin to read
%% @returns high | low | error | {error, Reason}
%% @doc     Read the digital state of a GPIO pin
%%
%%          Read if an input pin state is `high' or `low'.
%%          Warning: if the pin was not previously configured as an input using
%%          `gpio:set_direction/3' it will always read as low.
%% @end
%%-----------------------------------------------------------------------------
-spec read(GPIO :: gpio(), Pin :: pin()) -> high | low | {error, Reason :: atom()} | error.
read(_GPIO, Pin) ->
    digital_read(Pin).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to configure
%% @param   Direction is `input', `output', or `output_od'
%% @returns ok | error | {error, Reason}
%% @doc     Set the operational mode of a pin
%%
%%          Pins can be used for input, output, or output with open drain.
%% @end
%%-----------------------------------------------------------------------------
-spec set_direction(GPIO :: gpio(), Pin :: pin(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.
set_direction(_GPIO, Pin, Direction) ->
    init(Pin),
    set_pin_mode(Pin, Direction).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to write
%% @param   Level the desired output level to set
%% @returns ok | error | {error, Reason}
%% @doc     Set GPIO digital output level
%%
%%          Set a pin to `high' (1) or `low' (0).
%% @end
%%-----------------------------------------------------------------------------
-spec set_level(GPIO :: gpio(), Pin :: pin(), Level :: level()) ->
    ok | {error, Reason :: atom()} | error.
set_level(_GPIO, Pin, Level) ->
    digital_write(Pin, Level).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to set the interrupt on
%% @param   Trigger is the state that will trigger an interrupt
%% @returns {error, not_supported}
%% @doc     Not supported on RP2.
%% @end
%%-----------------------------------------------------------------------------
-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger()) ->
    {error, not_supported}.
set_int(_GPIO, _Pin, _Trigger) ->
    {error, not_supported}.

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to set the interrupt on
%% @param   Trigger is the state that will trigger an interrupt
%% @param   Pid is the process that will receive the interrupt message
%% @returns {error, not_supported}
%% @doc     Not supported on RP2.
%% @end
%%-----------------------------------------------------------------------------
-spec set_int(GPIO :: gpio(), Pin :: pin(), Trigger :: trigger(), Pid :: pid()) ->
    {error, not_supported}.
set_int(_GPIO, _Pin, _Trigger, _Pid) ->
    {error, not_supported}.

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to remove the interrupt
%% @returns {error, not_supported}
%% @doc     Not supported on RP2.
%% @end
%%-----------------------------------------------------------------------------
-spec remove_int(GPIO :: gpio(), Pin :: pin()) -> {error, not_supported}.
remove_int(_GPIO, _Pin) ->
    {error, not_supported}.

%%-----------------------------------------------------------------------------
%% @param   Pin number to initialize
%% @returns ok
%% @doc     Initialize a pin to be used as GPIO.
%%          This is required on RP2040.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Pin :: pin()) -> ok.
init(_Pin) ->
    ok.

%%-----------------------------------------------------------------------------
%% @param   Pin number to deinitialize
%% @returns ok
%% @doc     Reset a pin back to the NULL function.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Pin :: pin()) -> ok.
deinit(_Pin) ->
    ok.

%%-----------------------------------------------------------------------------
%% @param   Pin number to set operational mode
%% @param   Direction is `input', `output', or `output_od'
%% @returns ok | error | {error, Reason}
%% @doc     Set the operational mode of a pin
%%
%%          Pins can be used for input, output, or output with open drain.
%% @end
%%-----------------------------------------------------------------------------
-spec set_pin_mode(Pin :: pin(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.
set_pin_mode(_Pin, _Mode) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pin number to set internal resistor direction
%% @param   Pull is the internal resistor state
%% @returns ok | error
%% @doc     Set the internal resistor of a pin
%%
%%          Pins can be internally pulled `up', `down', `up_down' (pulled in
%%          both directions), or left `floating'.
%% @end
%%-----------------------------------------------------------------------------
-spec set_pin_pull(Pin :: pin(), Pull :: pull()) -> ok | error.
set_pin_pull(_Pin, _Pull) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to write
%% @param   Level the desired output level to set
%% @returns ok | error | {error, Reason}
%% @doc     Set GPIO digital output level
%%
%%          Set a pin to `high' (1) or `low' (0).
%%
%%          The LED pin on the Pico-W can be controlled on the extended pin `{wl, 0}', and does not
%%          require or accept `set_pin_mode' or `set_pin_pull' before use.
%% @end
%%-----------------------------------------------------------------------------
-spec digital_write(Pin :: pin(), Level :: level()) -> ok | {error, Reason :: atom()} | error.
digital_write(_Pin, _Level) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to read
%% @returns high | low | error | {error, Reason}
%% @doc     Read the digital state of a GPIO pin
%%
%%          Read if an input pin state is high or low.
%%          Warning: if the pin was not previously configured as an input using
%%          `gpio:set_pin_mode/2' it will always read as low.
%%
%%          The VBUS detect pin on the Pico-W can be read on the extended pin `{wl, 2}',
%%          and does not require or accept `set_pin_mode' or `set_pin_pull' before use.
%% @end
%%-----------------------------------------------------------------------------
-spec digital_read(Pin :: pin()) -> high | low | {error, Reason :: atom()} | error.
digital_read(_Pin) ->
    erlang:nif_error(undefined).

%% @private
gpio_loop() ->
    receive
        {'$call', {Pid, Ref}, {close}} ->
            unregister(gpio),
            Pid ! {Ref, ok}
    end.
