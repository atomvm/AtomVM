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
%% @doc GPIO Hardware Abstraction Layer behavior
%%
%% This module defines the behavior that platform-specific GPIO modules
%% must implement. It provides a common interface for basic GPIO operations
%% across all supported platforms (ESP32, RP2, STM32).
%%
%% There are two APIs for GPIO operations:
%%
%% <h3>NIF-based API</h3>
%%
%% The NIF-based API provides direct access to GPIO pins without requiring
%% a driver process or port. Functions operate directly on pin numbers.
%%
%% <ul>
%% <li>`init/1' - Initialize a pin for GPIO use (required on RP2, no-op on
%% ESP32 and STM32)</li>
%% <li>`deinit/1' - Release a pin from GPIO use</li>
%% <li>`set_pin_mode/2' - Configure a pin as input, output, or output with
%% open drain</li>
%% <li>`set_pin_pull/2' - Configure internal pull resistors</li>
%% <li>`digital_write/2' - Set pin output level</li>
%% <li>`digital_read/1' - Read pin input level</li>
%% </ul>
%%
%% <h3>Port-based API</h3>
%%
%% The port-based API requires starting a GPIO driver using `start/0' or
%% `open/0', which returns a handle (a port on ESP32 and STM32, a pid on
%% RP2). This handle is passed to all subsequent operations.
%%
%% <ul>
%% <li>`start/0' - Start the GPIO driver, or return an existing one if
%% already started</li>
%% <li>`open/0' - Start a new GPIO driver instance</li>
%% <li>`close/1' - Stop a GPIO driver and release its resources</li>
%% <li>`stop/0' - Stop the registered GPIO driver if one is running</li>
%% <li>`read/2' - Read pin input level</li>
%% <li>`set_direction/3' - Configure a pin as input, output, or output with
%% open drain</li>
%% <li>`set_level/3' - Set pin output level</li>
%% <li>`set_int/3' - Set a GPIO interrupt on a pin, notifications are sent
%% to the calling process</li>
%% <li>`set_int/4' - Set a GPIO interrupt on a pin, notifications are sent
%% to the specified process</li>
%% <li>`remove_int/2' - Remove a previously set GPIO interrupt</li>
%% </ul>
%%
%% <h3>Pin definitions</h3>
%%
%% Pin definitions vary by platform:
%% <ul>
%% <li>ESP32: a non-negative integer (e.g. `2', `15')</li>
%% <li>RP2: a non-negative integer, or `{wl, 0..2}' for Pico-W
%% wireless pins</li>
%% <li>STM32: a tuple `{Bank, PinNum}' where Bank is an atom `a' through
%% `k' and PinNum is `0..15', a list of pin numbers, or the atom
%% `all'</li>
%% </ul>
%%
%% <h3>Platform differences</h3>
%%
%% <ul>
%% <li>Interrupt support: ESP32 and STM32 support interrupts. RP2 does not
%% (returns `{error, not_supported}').</li>
%% <li>Interrupt message format: on ESP32, `{gpio_interrupt, Pin}' where
%% Pin is an integer; on STM32, `{gpio_interrupt, {Bank, Pin}}'.</li>
%% <li>Pull modes: ESP32 and RP2 support `up', `down', `up_down', and
%% `floating'. STM32 supports `up', `down', and `floating' only.</li>
%% <li>`init/1' must be called before using a pin on RP2. On ESP32 and
%% STM32 it is a no-op.</li>
%% <li>STM32 supports batch operations: multiple pins on the same bank
%% can be configured or written at once.</li>
%% </ul>
%%
%% <h3>Example usage (NIF-based API)</h3>
%%
%% The following example configures pin 2 as an output and sets it high:
%%
%% ```
%% gpio:init(2),
%% gpio:set_pin_mode(2, output),
%% gpio:digital_write(2, high).
%% '''
%%
%% The following example configures pin 4 as an input with a pull-up
%% resistor and reads the level:
%%
%% ```
%% gpio:init(4),
%% gpio:set_pin_mode(4, input),
%% gpio:set_pin_pull(4, up),
%% Level = gpio:digital_read(4).
%% '''
%%
%% <h3>Example usage (Port-based API)</h3>
%%
%% ```
%% GPIO = gpio:start(),
%% gpio:set_direction(GPIO, 2, output),
%% gpio:set_level(GPIO, 2, high),
%% gpio:set_int(GPIO, 4, rising),
%% receive
%%     {gpio_interrupt, 4} -> io:format("Pin 4 triggered!~n")
%% end.
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(gpio_hal).

-type direction() :: input | output | output_od.
%% The direction is used to set the mode of operation for a GPIO pin,
%% either as an input, an output, or output with open drain.

-type pull() :: up | down | up_down | floating.
%% Internal resistor pull mode.
%% Note: STM32 does not support `up_down'.

-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
%% Valid pin levels can be atom or integer representation.

-type trigger() :: none | rising | falling | both | low | high.
%% Event type that will trigger a `gpio_interrupt'.
%% Setting trigger to `none' disables the interrupt.

-type gpio() :: port() | pid().
%% Handle returned by `start/0' or `open/0'.
%% On ESP32 and STM32, this is a port. On RP2, this is a pid.

-export_type([direction/0, pull/0, level/0, trigger/0, gpio/0]).

%% NIF-based API

% Initialize a pin for GPIO use.
%
% This must be called before using a pin on the RP2 platform. On ESP32
% and STM32, this function is a no-op and always returns `ok'.
-callback init(Pin :: term()) -> ok.

% Deinitialize a pin, releasing it from GPIO use.
%
% Resets the pin back to its default (NULL) function.
-callback deinit(Pin :: term()) -> ok.

% Set the operational mode of a pin.
%
% Configures a pin as `input', `output', or `output_od' (output with
% open drain). The pin should be initialized with `init/1' first on
% platforms that require it.
-callback set_pin_mode(Pin :: term(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.

% Configure the internal pull resistor of a pin.
%
% Pins can be pulled `up', `down', `up_down' (pulled in both
% directions), or left `floating'. Not all pull modes are supported
% on all platforms (e.g. STM32 does not support `up_down').
-callback set_pin_pull(Pin :: term(), Pull :: pull()) -> ok | error.

% Set the digital output level of a pin.
%
% Sets the pin to `high' (or `1') or `low' (or `0'). The pin should
% be configured as an output using `set_pin_mode/2' first.
-callback digital_write(Pin :: term(), Level :: level()) ->
    ok | {error, Reason :: atom()} | error.

% Read the digital input level of a pin.
%
% Returns `high' or `low'. The pin should be configured as an input
% using `set_pin_mode/2' first; otherwise it may always read as `low'.
-callback digital_read(Pin :: term()) ->
    high | low | {error, Reason :: atom()} | error.

%% Port-based API

% Start the GPIO driver.
%
% Returns the handle of an existing GPIO driver if one is already
% registered, or starts a new one. The returned handle is required
% for all port-based API functions.
-callback start() -> gpio() | {error, Reason :: atom()} | error.

% Open a new GPIO driver instance.
%
% Always starts a new GPIO driver instance and registers it. Fails
% if a driver is already registered. Use `start/0' to get an existing
% instance or start a new one.
-callback open() -> gpio() | {error, Reason :: atom()} | error.

% Close a GPIO driver and release its resources.
%
% This disables any active interrupts, stops the driver, and frees
% all associated resources.
-callback close(GPIO :: gpio()) -> ok | {error, Reason :: atom()} | error.

% Stop the registered GPIO driver.
%
% If a GPIO driver is registered, it is closed and its resources are
% freed. If no driver is registered, returns `ok'.
-callback stop() -> ok | {error, Reason :: atom()} | error.

% Read the digital input level of a pin using the port-based API.
%
% Returns `high' or `low'. The pin should be configured as an input
% using `set_direction/3' first; otherwise it may always read as `low'.
-callback read(GPIO :: gpio(), Pin :: term()) ->
    high | low | {error, Reason :: atom()} | error.

% Set the operational mode of a pin using the port-based API.
%
% Configures a pin as `input', `output', or `output_od' (output with
% open drain).
-callback set_direction(GPIO :: gpio(), Pin :: term(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.

% Set the digital output level of a pin using the port-based API.
%
% Sets the pin to `high' (or `1') or `low' (or `0'). The pin should
% be configured as an output using `set_direction/3' first.
-callback set_level(GPIO :: gpio(), Pin :: term(), Level :: level()) ->
    ok | {error, Reason :: atom()} | error.

% Set a GPIO interrupt on a pin.
%
% Configures the pin to trigger an interrupt on the specified condition.
% When triggered, a message `{gpio_interrupt, Pin}' is sent to the
% calling process (the exact format of Pin in the message depends on
% the platform).
%
% Not supported on all platforms. RP2 returns `{error, not_supported}'.
-callback set_int(GPIO :: gpio(), Pin :: term(), Trigger :: trigger()) ->
    ok | {error, Reason :: atom()} | error.

% Set a GPIO interrupt on a pin, sending notifications to a
% specific process.
%
% Same as `set_int/3', but the interrupt message `{gpio_interrupt, Pin}'
% is sent to the specified `Pid' instead of the calling process.
%
% Not supported on all platforms. RP2 returns `{error, not_supported}'.
-callback set_int(GPIO :: gpio(), Pin :: term(), Trigger :: trigger(), Pid :: pid()) ->
    ok | {error, Reason :: atom()} | error.

% Remove a GPIO interrupt from a pin.
%
% Disables a previously configured interrupt on the specified pin.
%
% Not supported on all platforms. RP2 returns `{error, not_supported}'.
-callback remove_int(GPIO :: gpio(), Pin :: term()) ->
    ok | {error, Reason :: atom()} | error.
