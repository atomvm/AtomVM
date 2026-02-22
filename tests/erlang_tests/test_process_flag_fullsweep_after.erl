%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_process_flag_fullsweep_after).

-export([start/0]).

start() ->
    case get_otp_version() > 23 of
        true ->
            ok = test_process_flag_fullsweep_after(),
            ok = test_spawn_opt_fullsweep_after();
        false ->
            % Can we please get rid of OTP < 24 support?
            ok
    end,
    0.

test_process_flag_fullsweep_after() ->
    OldVal = erlang:process_flag(fullsweep_after, 10),
    10 = erlang:process_flag(fullsweep_after, 0),
    0 = erlang:process_flag(fullsweep_after, OldVal),
    ok = expect_badarg(fun() -> erlang:process_flag(fullsweep_after, -1) end),
    ok = expect_badarg(fun() -> erlang:process_flag(fullsweep_after, foo) end),
    ok.

test_spawn_opt_fullsweep_after() ->
    Parent = self(),
    spawn_opt(
        fun() ->
            {fullsweep_after, Val} = erlang:process_info(self(), fullsweep_after),
            Parent ! {fullsweep_after, Val}
        end,
        [{fullsweep_after, 42}]
    ),
    receive
        {fullsweep_after, 42} -> ok
    after 500 -> timeout
    end,
    ok = expect_badarg(fun() -> spawn_opt(fun() -> ok end, [{fullsweep_after, -1}]) end),
    ok = expect_badarg(fun() -> spawn_opt(fun() -> ok end, [{fullsweep_after, foo}]) end),
    ok.

expect_badarg(Fun) ->
    try
        Fun(),
        unexpected
    catch
        error:badarg -> ok
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" -> list_to_integer(erlang:system_info(otp_release));
        _ -> atomvm
    end.
