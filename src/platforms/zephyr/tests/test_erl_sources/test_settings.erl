%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_settings).
-export([start/0]).

start() ->
    ok = zephyr:settings_erase(atomvm, test_key),
    {error, not_found} = zephyr:settings_get(atomvm, test_key),
    <<"fallback">> = zephyr:settings_get(atomvm, test_key, <<"fallback">>),
    ok = zephyr:settings_put(atomvm, test_key, <<"ssid-value">>),
    {ok, <<"ssid-value">>} = zephyr:settings_get(atomvm, test_key),
    <<"ssid-value">> = zephyr:settings_get(atomvm, test_key, <<"fallback">>),
    ok = zephyr:settings_put(atomvm, test_key, <<"replaced">>),
    {ok, <<"replaced">>} = zephyr:settings_get(atomvm, test_key),
    ok = zephyr:settings_erase(atomvm, test_key),
    {error, not_found} = zephyr:settings_get(atomvm, test_key),
    ok.
