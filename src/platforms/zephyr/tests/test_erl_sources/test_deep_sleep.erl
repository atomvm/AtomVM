%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_deep_sleep).
-export([start/0]).

-define(SLEEP_MS, 500).

start() ->
    case zephyr:sleep_get_wakeup_cause() of
        timer ->
            ok;
        undefined ->
            zephyr:deep_sleep(?SLEEP_MS);
        Other ->
            {error, {unexpected_wakeup, Other}}
    end.
