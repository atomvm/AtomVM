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

-spec start() -> gpio().
start() ->
    case whereis(gpio) of
        undefined ->
            open();
        GPIO ->
            GPIO
    end.

-spec open() -> gpio().
open() ->
    open_port({spawn, "gpio"}, []).

-spec read(GPIO :: gpio(), GPIONum :: pin()) -> high | low.
read(GPIO, GPIONum) ->
    port:call(GPIO, {read, GPIONum}).

-spec set_direction(GPIO :: gpio(), GPIONum :: pin(), Direction :: direction()) -> ok | error.
set_direction(GPIO, GPIONum, Direction) ->
    port:call(GPIO, {set_direction, GPIONum, Direction}).

-spec set_level(GPIO :: gpio(), GPIONum :: pin(), Level :: level()) -> ok | error.
set_level(GPIO, GPIONum, Level) ->
    port:call(GPIO, {set_level, GPIONum, Level}).

-spec set_int(GPIO :: gpio(), GPIONum :: pin(), Trigger :: trigger()) -> ok | error.
set_int(GPIO, GPIONum, Trigger) ->
    port:call(GPIO, {set_int, GPIONum, Trigger}).

-spec remove_int(GPIO :: gpio(), GPIONum :: pin()) -> ok | error.
remove_int(GPIO, GPIONum) ->
    port:call(GPIO, {remove_int, GPIONum}).

-spec set_pin_mode(GPIONum :: pin(), Direction :: direction()) -> ok | error.
set_pin_mode(_GPIONum, _Mode) ->
    throw(nif_error).

-spec set_pin_pull(GPIONum :: pin(), Pull :: pull()) -> ok | error.
set_pin_pull(_GPIONum, _Pull) ->
    throw(nif_error).

-spec hold_en(GPIONum :: pin()) -> ok | error.
hold_en(_GPIONum) ->
    throw(nif_error).

-spec hold_dis(GPIONum :: pin()) -> ok | error.
hold_dis(_GPIONum) ->
    throw(nif_error).

-spec deep_sleep_hold_en() -> ok.
deep_sleep_hold_en() ->
    throw(nif_error).

-spec deep_sleep_hold_dis() -> ok.
deep_sleep_hold_dis() ->
    throw(nif_error).

-spec digital_write(GPIONum :: pin(), Level :: level()) -> ok | error.
digital_write(_GPIONum, _Level) ->
    throw(nif_error).

-spec digital_read(GPIONum :: pin()) -> high | low.
digital_read(_GPIONum) ->
    throw(nif_error).

%% TODO should we deprecate?  It looks like a memory leak
attach_interrupt(GPIONum, Mode) ->
    set_int(open(), GPIONum, Mode).

%% TODO should we deprecate?  It looks like a memory leak
detach_interrupt(GPIONum) ->
    remove_int(open(), GPIONum).
