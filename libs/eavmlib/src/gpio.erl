%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(gpio).

-export([open/0, read/2, set_direction/3, set_level/3, set_int/3, remove_int/2]).
-export([set_pin_mode/2, digital_write/2, digital_read/1, attach_interrupt/2, detach_interrupt/1]).

-type gpio() :: pid().
-type pin() :: non_neg_integer().
-type direction() :: input | output.
-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
-type trigger() :: none | rising | falling | both | low | high.

-spec open() -> gpio().
open() ->
    open_port({spawn, "gpio"}, []).

-spec read(GPIO::gpio(), GPIONum::pin()) -> high | low.
read(GPIO, GPIONum) ->
    port:call(GPIO, {read, GPIONum}).

-spec set_direction(GPIO::gpio(), GPIONum::pin(), Direction::direction()) -> ok | error.
set_direction(GPIO, GPIONum, Direction) ->
    port:call(GPIO, {set_direction, GPIONum, Direction}).

-spec set_level(GPIO::gpio(), GPIONum::pin(), Level::level()) -> ok | error.
set_level(GPIO, GPIONum, Level) ->
    port:call(GPIO, {set_level, GPIONum, Level}).

-spec set_int(GPIO::gpio(), GPIONum::pin(), Trigger::trigger()) -> ok | error.
set_int(GPIO, GPIONum, Trigger) ->
    port:call(GPIO, {set_int, GPIONum, Trigger}).

-spec remove_int(GPIO::gpio(), GPIONum::pin()) -> ok | error.
remove_int(GPIO, GPIONum) ->
    port:call(GPIO, {remove_int, GPIONum}).

-spec set_pin_mode(GPIONum::pin(), Direction::direction()) -> ok | error.
set_pin_mode(_GPIONum, _Mode) ->
    throw(nif_error).

-spec digital_write(GPIONum::pin(), Level::level()) -> ok | error.
digital_write(_GPIONum, _Level) ->
    throw(nif_error).

-spec digital_read(GPIONum::pin()) -> high | low.
digital_read(_GPIONum) ->
    throw(nif_error).

%% TODO should we deprecate?  It looks like a memory leak
attach_interrupt(GPIONum, Mode) ->
    set_int(open(), GPIONum, Mode).

%% TODO should we deprecate?  It looks like a memory leak
detach_interrupt(GPIONum) ->
    remove_int(open(), GPIONum).
