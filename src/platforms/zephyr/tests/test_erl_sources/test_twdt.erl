%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
% Copyright 2026 Peter M <petermm@gmail.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_twdt).
-export([start/0]).

start() ->
    ok = test_reconfigure(),
    ok = test_user(),
    ok = test_deinit_init(),
    ok.

test_reconfigure() ->
    ok =
        try
            zephyr:task_wdt_reconfigure(config),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            zephyr:task_wdt_reconfigure({-1, 0, false}),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            zephyr:task_wdt_reconfigure({0, 0, false}),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            zephyr:task_wdt_reconfigure({5000, -1, false}),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            zephyr:task_wdt_reconfigure({5000, 512, false}),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            zephyr:task_wdt_reconfigure({5000, 0, 0}),
            fail
        catch
            error:badarg -> ok
        end,
    ok = zephyr:task_wdt_reconfigure({5000, 0, false}),
    ok.

test_user() ->
    ok =
        try
            zephyr:task_wdt_add_user(42),
            fail
        catch
            error:badarg -> ok
        end,
    {ok, Handle} = zephyr:task_wdt_add_user(<<"42">>),
    io:format("Reconfigure to 1000ms\n"),
    ok = zephyr:task_wdt_reconfigure({1000, 0, false}),
    io:format("Reset now\n"),
    ok = zephyr:task_wdt_reset_user(Handle),
    io:format("Wait 2000ms\n"),
    timer:sleep(2000),
    io:format("Reset again\n"),
    ok = zephyr:task_wdt_reset_user(Handle),
    ok = zephyr:task_wdt_delete_user(Handle),
    ok = zephyr:task_wdt_reconfigure({5000, 0, false}),
    ok.

test_deinit_init() ->
    ok = zephyr:task_wdt_deinit(),
    {error, _} = zephyr:task_wdt_deinit(),
    {error, noproc} = zephyr:task_wdt_reconfigure({5000, 0, false}),
    ok = zephyr:task_wdt_init({5000, 0, false}),
    {error, already_started} = zephyr:task_wdt_init({5000, 0, false}),
    ok.
