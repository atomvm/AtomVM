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
    Pin = test_pin(),
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

%% GPIO0 is fine on classic ESP32. ESP32-C3 reports
%% `riscv64-espressif-zephyr`; GPIO0 does not read back a driven low
%% on Wokwi. GPIO10 is a spare digital pin (not strapping, USB-JTAG,
%% UART, or the ADC pot on GPIO3).
test_pin() ->
    Architecture = erlang:system_info(system_architecture),
    case binary:split(Architecture, <<"-">>, [global]) of
        [<<"riscv64">>, <<"espressif">> | _] ->
            {0, 10};
        _ ->
            {0, 0}
    end.
