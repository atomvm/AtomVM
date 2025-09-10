%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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

-module(pico_blink).
-export([start/0]).

% 25 is on-board led on Pico
% This code will not work on Pico-W where GPIO 25 has another purpose.
-define(GPIO_NUM, 25).

start() ->
    gpio:init(?GPIO_NUM),
    gpio:set_pin_mode(?GPIO_NUM, output),
    loop(off).

loop(off) ->
    gpio:digital_write(?GPIO_NUM, low),
    timer:sleep(1000),
    loop(on);
loop(on) ->
    gpio:digital_write(?GPIO_NUM, high),
    timer:sleep(1000),
    loop(off).
