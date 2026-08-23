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
    ok = zephyr:settings_put(atomvm, test_key, <<"keep-ns">>),
    ok = zephyr:settings_put(other_ns, key_a, <<"a">>),
    ok = zephyr:settings_put(other_ns, key_b, <<"b">>),
    ok = zephyr:settings_put(other_ns, key_c, <<"c">>),
    ok = zephyr:settings_erase_all(other_ns),
    {ok, <<"keep-ns">>} = zephyr:settings_get(atomvm, test_key),
    {error, not_found} = zephyr:settings_get(other_ns, key_a),
    {error, not_found} = zephyr:settings_get(other_ns, key_b),
    {error, not_found} = zephyr:settings_get(other_ns, key_c),
    ok = zephyr:settings_reformat(),
    ok = zephyr:settings_put(atomvm, test_key, <<"after-reformat">>),
    {ok, <<"after-reformat">>} = zephyr:settings_get(atomvm, test_key),
    ok = zephyr:settings_erase(atomvm, test_key),
    ok.
