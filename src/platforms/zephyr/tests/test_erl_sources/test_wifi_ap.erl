%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_wifi_ap).
-export([start/0]).

-define(AP_TIMEOUT, 15000).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            Result = start_ap(),
            _ = network:stop(),
            Result;
        Error ->
            Error
    end.

start_ap() ->
    Self = self(),
    Config = [
        {ap, [
            {ssid, <<"atomvm-ap">>},
            {ap_started, fun() -> Self ! ap_started end}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            wait_started(?AP_TIMEOUT);
        Error ->
            Error
    end.

wait_started(Timeout) ->
    receive
        ap_started ->
            io:format("AP started.~n"),
            ok
    after Timeout ->
        {error, timeout}
    end.

verify_platform(esp32) ->
    ok;
verify_platform(zephyr) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.
