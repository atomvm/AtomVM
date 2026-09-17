%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_devmode).
-export([start/0]).

start() ->
    case verify_platform(atomvm:platform()) of
        ok ->
            Result = run(),
            _ = network:stop(),
            Result;
        Error ->
            Error
    end.

run() ->
    avm_pubsub:start(default_pubsub),
    case catch esp32devmode:start_network() of
        started ->
            io:format("test_devmode started.~n"),
            ok;
        {'EXIT', Reason} ->
            {error, {devmode_exit, Reason}};
        Error ->
            Error
    end.

verify_platform(esp32) ->
    ok;
verify_platform(zephyr) ->
    ok;
verify_platform(Platform) ->
    {error, {unsupported_platform, Platform}}.
