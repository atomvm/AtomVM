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

-module(blink_nucleo64).
-export([start/0]).

% Nucleo-64 boards: NUCLEO-F411RE, NUCLEO-L476RG, NUCLEO-G0B1RE, etc.
% Single user LED (LD2, green) = PA5

start() ->
    Pin = {a, 5},
    gpio:set_pin_mode(Pin, output),
    loop(Pin).

loop(Pin) ->
    gpio:digital_write(Pin, high),
    receive
    after 500 -> ok
    end,
    gpio:digital_write(Pin, low),
    receive
    after 500 -> ok
    end,
    loop(Pin).
