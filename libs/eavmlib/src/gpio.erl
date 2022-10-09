%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
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

%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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
%% @doc GPIO driver module
%%
%% This module provides functions for interacting with micro-controller GPIO
%% (General Purpose Input and Output) pins.
%% @end
%%-----------------------------------------------------------------------------
-module(gpio).

-export([start/0, open/0, read/2, set_direction/3, set_level/3, set_int/3, remove_int/2]).
-export([
    set_pin_mode/2,
    set_pin_pull/2,
    hold_en/1,
    hold_dis/1,
    deep_sleep_hold_en/0,
    deep_sleep_hold_dis/0,
    digital_write/2,
    digital_read/1,
    attach_interrupt/2,
    detach_interrupt/1
]).

-type gpio() :: pid().
-type pin() :: non_neg_integer().
-type direction() :: input | output | output_od.
-type pull() :: up | down | up_down | floating.
-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
-type trigger() :: none | rising | falling | both | low | high.

%%-----------------------------------------------------------------------------
%% @returns Pid
%% @doc     Start the GPIO driver port
%%
%%          Returns the pid of the active GPIO port driver, otherwise the GPIO
%%          port driver will be stared and registered as `gpio'. The use of
%%          `gpio:open/0' or `gpio:start/0' is required before using any functions
%%          that require a GPIO pid as a parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> gpio().
start() ->
    case whereis(gpio) of
        undefined ->
            open();
        GPIO ->
            GPIO
    end.

%%-----------------------------------------------------------------------------
%% @returns Pid
%% @doc     Start the GPIO driver port
%%
%%          The GPIO port driver will be stared and registered as `gpio'. If the
%%          port has already been started through the `gpio:open/0' or
%%          `gpio:start/0' the command will fail. The use of `gpio:open/0' or
%%          `gpio:start/0' is required before using any functions that require a
%%          GPIO pid as a parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec open() -> gpio().
open() ->
    open_port({spawn, "gpio"}, []).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from gpio:start/0
%% @param   Pin number of the pin to read
%% @returns high | low
%% @doc     Read the digital state of a GPIO pin
%%
%%          Read if an input pin state is `high' or `low'.
%%          Warning: if a is not previously configured as an input using
%%          `gpio:set_direction/3' it will always read as low.
%% @end
%%-----------------------------------------------------------------------------
-spec read(GPIO :: gpio(), GPIONum :: pin()) -> high | low.
read(GPIO, GPIONum) ->
    port:call(GPIO, {read, GPIONum}).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to configure
%% @param   Direction is `input', `output', or `output_od'
%% @returns ok | error
%% @doc     Set the operational mode of a pin
%%
%%          Pins can be used for input, output, or output with open drain.
%% @end
%%-----------------------------------------------------------------------------
-spec set_direction(GPIO :: gpio(), GPIONum :: pin(), Direction :: direction()) -> ok | error.
set_direction(GPIO, GPIONum, Direction) ->
    port:call(GPIO, {set_direction, GPIONum, Direction}).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to write
%% @param   Desired output level to set
%% @returns ok | error
%% @doc     Set GPIO digital output level
%%
%%          Set a pin to `high' (1) or `low' (0).
%% @end
%%-----------------------------------------------------------------------------
-spec set_level(GPIO :: gpio(), GPIONum :: pin(), Level :: level()) -> ok | error.
set_level(GPIO, GPIONum, Level) ->
    port:call(GPIO, {set_level, GPIONum, Level}).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to set the interrupt on
%% @param   Trigger is the state that will trigger an interrupt
%% @returns ok | error
%% @doc     Set a GPIO interrupt
%%
%%          Available triggers are `none' (which is the same as disabling an
%%          interrupt), `rising', `falling', `both' (rising or falling), `low', and
%%          `high'. When the interrupt is triggered it will send a tuple:
%%            `{gpio_interrupt, Pin}'
%%          to the process that set the interrupt. Pin will be the number
%%          of the pin that triggered the interrupt.
%% @end
%%-----------------------------------------------------------------------------
-spec set_int(GPIO :: gpio(), GPIONum :: pin(), Trigger :: trigger()) -> ok | error.
set_int(GPIO, GPIONum, Trigger) ->
    port:call(GPIO, {set_int, GPIONum, Trigger}).

%%-----------------------------------------------------------------------------
%% @param   GPIO pid that was returned from `gpio:start/0'
%% @param   Pin number of the pin to remove the interrupt
%% @returns ok | error
%% @doc     Remove a GPIO interrupt
%%
%%          Removes an interrupt from the specified pin.
%% @end
%%-----------------------------------------------------------------------------
-spec remove_int(GPIO :: gpio(), GPIONum :: pin()) -> ok | error.
remove_int(GPIO, GPIONum) ->
    port:call(GPIO, {remove_int, GPIONum}).

%%-----------------------------------------------------------------------------
%% @param   Pin number to set operational mode
%% @param   Direction is `input', `output', or `output_od'
%% @returns ok | error
%% @doc     Set the operational mode of a pin
%%
%%          Pins can be used for input, output, or output with open drain.
%% @end
%%-----------------------------------------------------------------------------
-spec set_pin_mode(GPIONum :: pin(), Direction :: direction()) -> ok | error.
set_pin_mode(_GPIONum, _Mode) ->
    throw(nif_error).

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
-spec set_pin_pull(GPIONum :: pin(), Pull :: pull()) -> ok | error.
set_pin_pull(_GPIONum, _Pull) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to be held
%% @returns ok | error
%% @doc     Hold the state of a pin
%%
%%          The gpio pad hold function works in both input and output modes,
%%          but must be output-capable gpios.
%%
%%          If pad hold enabled:
%%          In output mode: the output level of the pad will be force locked
%%          and can not be changed.
%%          In input mode: the input value read will not change, regardless
%%          the changes of input signal.
%%
%%          The state of digital gpio cannot be held during Deep-sleep, and it
%%          will resume the hold function when the chip wakes up from
%%          Deep-sleep. If the digital gpio also needs to be held during
%%          Deep-sleep `gpio:deep_sleep_hold_en' should also be called.
%% @end
%%-----------------------------------------------------------------------------
-spec hold_en(GPIONum :: pin()) -> ok | error.
hold_en(_GPIONum) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to be released
%% @returns ok | error
%% @doc     Release a pin from a hold state.
%%
%%          When the chip is woken up from Deep-sleep, the gpio will be set to
%%          the default mode, so, the gpio will output the default level if
%%          this function is called. If you don’t want the level changes, the
%%          gpio should be configured to a known state before this function is
%%          called. e.g. If you hold gpio18 high during Deep-sleep, after the
%%          chip is woken up and `gpio:hold_dis' is called, gpio18 will output
%%          low level(because gpio18 is input mode by default). If you don’t
%%          want this behavior, you should configure gpio18 as output mode and
%%          set it to hight level before calling `gpio:hold_dis'.
%% @end
%%-----------------------------------------------------------------------------
-spec hold_dis(GPIONum :: pin()) -> ok | error.
hold_dis(_GPIONum) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Enable all hold functions to continue in deep sleep.
%%
%%          The gpio pad hold function works in both input and output modes,
%%          but must be output-capable gpios.
%%
%%          When the chip is in Deep-sleep mode, all digital gpio will hold the
%%          state before sleep, and when the chip is woken up, the status of
%%          digital gpio will not be held. Note that the pad hold feature only
%%          works when the chip is in Deep-sleep mode, when not in sleep mode,
%%          the digital gpio state can be changed even you have called this
%%          function.
%%
%%          Power down or call `gpio_hold_dis' will disable this function,
%%          otherwise, the digital gpio hold feature works as long as the chip
%%          enters Deep-sleep.
%% @end
%%-----------------------------------------------------------------------------
-spec deep_sleep_hold_en() -> ok.
deep_sleep_hold_en() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Disable all gpio pad functions during Deep-sleep.
%% @end
%%-----------------------------------------------------------------------------
-spec deep_sleep_hold_dis() -> ok.
deep_sleep_hold_dis() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to write
%% @param   Desired output level to set
%% @returns ok | error
%% @doc     Set GPIO digital output level
%%
%%          Set a pin to `high' (1) or `low' (0).
%% @end
%%-----------------------------------------------------------------------------
-spec digital_write(GPIONum :: pin(), Level :: level()) -> ok | error.
digital_write(_GPIONum, _Level) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to read
%% @returns high | low
%% @doc     Read the digital state of a GPIO pin
%%
%%          Read if an input pin state is high or low.
%%          Warning: if a is not previously configured as an input using
%%          `gpio:set_pin_mode/2' it will always read as low.
%% @end
%%-----------------------------------------------------------------------------
-spec digital_read(GPIONum :: pin()) -> high | low.
digital_read(_GPIONum) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to set the interrupt on
%% @param   Trigger is the state that will trigger an interrupt
%% @returns ok | error
%% @doc     Convenience function for `gpio:set_int/3'
%%
%%          This is a convenience function for `gpio:set_int/3' that allows an
%%          interrupt to be set using only the pin number and trigger as
%%          arguments.
%%
%%          This function should only be used when only one gpio trigger is
%%          used in an application. If multiple pins are being configured with
%%          interrupt triggers gpio:set_int/3 should be used otherwise there is
%%          a race condition when start() is called internally by this function.
%% @end
%%-----------------------------------------------------------------------------
-spec attach_interrupt(GPIONum :: pin(), Trigger :: trigger()) -> ok | error.
attach_interrupt(GPIONum, Trigger) ->
    set_int(start(), GPIONum, Trigger).

%-----------------------------------------------------------------------------
%% @param   Pin number of the pin to remove the interrupt
%% @returns ok | error
%% @doc     Convenience function for `gpio:remove_int/2'
%%
%%          This is a convenience function for `gpio:remove_int/2' that allows an
%%          interrupt to be removed using only the pin number as an argument.
%%
%%          Unlike `gpio:attach_interrupt/2' this function can be safely used
%%          regardless of the number of interrupt pins used in the application.
%% @end
%%-----------------------------------------------------------------------------
-spec detach_interrupt(GPIONum :: pin()) -> ok | error.
detach_interrupt(GPIONum) ->
    remove_int(whereis(gpio), GPIONum).
