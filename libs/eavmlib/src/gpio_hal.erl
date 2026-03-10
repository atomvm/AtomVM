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
%% @end
%%-----------------------------------------------------------------------------
-module(gpio_hal).

-type direction() :: input | output | output_od.
%% The direction is used to set the mode of operation for a GPIO pin,
%% either as an input, an output, or output with open drain.

-type pull() :: up | down | up_down | floating.
%% Internal resistor pull mode.

-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
%% Valid pin levels can be atom or integer representation.

-type trigger() :: none | rising | falling | both | low | high.
%% Event type that will trigger a `gpio_interrupt'.

-type gpio() :: port() | pid().
%% Handle returned by `start/0' or `open/0'.

-export_type([direction/0, pull/0, level/0, trigger/0, gpio/0]).

%% NIF-based API

-callback init(Pin :: term()) -> ok.

-callback deinit(Pin :: term()) -> ok.

-callback set_pin_mode(Pin :: term(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.

-callback set_pin_pull(Pin :: term(), Pull :: pull()) -> ok | error.

-callback digital_write(Pin :: term(), Level :: level()) ->
    ok | {error, Reason :: atom()} | error.

-callback digital_read(Pin :: term()) ->
    high | low | {error, Reason :: atom()} | error.

%% Port-based API

-callback start() -> gpio() | {error, Reason :: atom()} | error.

-callback open() -> gpio() | {error, Reason :: atom()} | error.

-callback close(GPIO :: gpio()) -> ok | {error, Reason :: atom()} | error.

-callback stop() -> ok | {error, Reason :: atom()} | error.

-callback read(GPIO :: gpio(), Pin :: term()) ->
    high | low | {error, Reason :: atom()} | error.

-callback set_direction(GPIO :: gpio(), Pin :: term(), Direction :: direction()) ->
    ok | {error, Reason :: atom()} | error.

-callback set_level(GPIO :: gpio(), Pin :: term(), Level :: level()) ->
    ok | {error, Reason :: atom()} | error.

-callback set_int(GPIO :: gpio(), Pin :: term(), Trigger :: trigger()) ->
    ok | {error, Reason :: atom()} | error.

-callback set_int(GPIO :: gpio(), Pin :: term(), Trigger :: trigger(), Pid :: pid()) ->
    ok | {error, Reason :: atom()} | error.

-callback remove_int(GPIO :: gpio(), Pin :: term()) ->
    ok | {error, Reason :: atom()} | error.
