%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_gpio).
-export([start/0]).

start() ->
    Pin = {0, 0},
    ok = gpio:init(Pin),
    ok = gpio:set_pin_mode(Pin, output),
    ok = gpio:digital_write(Pin, low),
    low = gpio:digital_read(Pin),
    ok = gpio:digital_write(Pin, high),
    Level = gpio:digital_read(Pin),
    true = Level =:= low orelse Level =:= high,
    GPIO = gpio:start(),
    GPIO = gpio:start(),
    Level = gpio:read(GPIO, Pin),
    ok = gpio:set_pin_mode(Pin, input),
    ok = gpio:set_int(GPIO, Pin, rising),
    ok = gpio:remove_int(GPIO, Pin),
    ok = gpio:stop(),
    ok = gpio:set_pin_pull(Pin, floating),
    ok = gpio:deinit(Pin),
    ok.
