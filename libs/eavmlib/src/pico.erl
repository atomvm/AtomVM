%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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
%% @doc PICO-specific APIs
%%
%% This module contains functions that are specific to the PICO platform.
%% @end
%%-----------------------------------------------------------------------------
-module(pico).

-export([
    rtc_set_datetime/1,
    cyw43_arch_gpio_get/1,
    cyw43_arch_gpio_put/2
]).

%%-----------------------------------------------------------------------------
%% @doc     Set the datetime on the RTC.
%%          The datetime can be obtained through bif erlang:localtime()
%% @end
%%-----------------------------------------------------------------------------
-spec rtc_set_datetime(calendar:datetime()) -> ok.
rtc_set_datetime(_Datetime) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   GPIO pin to read
%% @returns the level of the GPIO pin
%% @doc     Read a GPIO of the CYW43.
%%          This function is only available on Pico-W.
%% @end
%%-----------------------------------------------------------------------------
-spec cyw43_arch_gpio_get(GPIO :: 0..2) -> 0..1.
cyw43_arch_gpio_get(_GPIO) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   GPIO pin to write
%% @param   Level value to write
%% @doc     Write a GPIO of the CYW43.
%%          This function is only available on Pico-W. It is typically used to
%%          drive the on-board LED.
%% @end
%%-----------------------------------------------------------------------------
-spec cyw43_arch_gpio_put(GPIO :: 0..2, Level :: 0..1) -> ok.
cyw43_arch_gpio_put(_GPIO, _Level) ->
    erlang:nif_error(undefined).
