%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_pwm).
-export([start/0]).

%% 1 kHz period, 25% duty cycle.
-define(PERIOD_NS, 1000000).
-define(PULSE_NS, 250000).

start() ->
    {ok, PWM} = pwm:open([]),
    {ok, CyclesPerSec} = pwm:get_cycles_per_sec(PWM, 0),
    true = is_integer(CyclesPerSec) andalso CyclesPerSec > 0,
    ok = pwm:set(PWM, 0, ?PERIOD_NS, ?PULSE_NS),
    ok = pwm:set(PWM, 0, ?PERIOD_NS, 0, inverted),
    ok = pwm:set_cycles(PWM, 0, 1000, 500, normal),
    ok = pwm:close(PWM),
    ok.
