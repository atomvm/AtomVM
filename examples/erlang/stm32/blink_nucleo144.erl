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

-module(blink_nucleo144).
-export([start/0]).

% Select board revision: mb1137 (default), mb1312, or mb1364.
-ifndef(NUCLEO144_BOARD).
-define(NUCLEO144_BOARD, mb1137).
-endif.

-if(?NUCLEO144_BOARD =:= mb1137).
% MB1137: NUCLEO-F429ZI, NUCLEO-F746ZG, NUCLEO-H743ZI, etc.
%   LD1 (green) = PB0, LD2 (blue) = PB7, LD3 (red) = PB14
-define(LEDS, [{b, [0, 7, 14]}]).
-elif(?NUCLEO144_BOARD =:= mb1312).
% MB1312: NUCLEO-L496ZG-P, NUCLEO-L4R5ZI, etc.
%   LD1 (green) = PC7, LD2 (blue) = PB7, LD3 (red) = PB14
-define(LEDS, [{b, [7, 14]}, {c, 7}]).
-elif(?NUCLEO144_BOARD =:= mb1364).
% MB1364: NUCLEO-H743ZI2
%   LD1 (green) = PB0, LD2 (yellow) = PE1, LD3 (red) = PB14
-define(LEDS, [{b, [0, 14]}, {e, 1}]).
-endif.

start() ->
    set_outputs(?LEDS),
    loop(?LEDS).

loop(LEDs) ->
    write_all(LEDs, high),
    receive
    after 500 -> ok
    end,
    write_all(LEDs, low),
    receive
    after 500 -> ok
    end,
    loop(LEDs).

set_outputs([Pin | Rest]) ->
    gpio:set_pin_mode(Pin, output),
    set_outputs(Rest);
set_outputs([]) ->
    ok.

write_all([Pin | Rest], Level) ->
    gpio:digital_write(Pin, Level),
    write_all(Rest, Level);
write_all([], _Level) ->
    ok.
