%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_adc).
-export([start/0]).

start() ->
    try
        {ok, ADC} = adc:open(),
        {ok, {Raw, Millivolts}} = adc:read(ADC, 16),
        true = Raw > 1000 andalso Raw < 3000,
        true = Millivolts > 800 andalso Millivolts < 2500,
        ok = adc:close(ADC),
        ok
    catch
        Class:Reason:Stacktrace ->
            erlang:display({Class, Reason, Stacktrace}),
            error
    end.
