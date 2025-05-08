%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_system_info).

-export([start/0, loop/1]).

start() ->
    Machine = erlang:system_info(machine),
    ok = test_process_count(Machine),
    ok = test_port_count(Machine),
    assert(erlang:system_info(atom_count) > 0),
    assert(erlang:system_info(wordsize) > 0),
    case Machine of
        "BEAM" ->
            % beam returns a list and probably so should AtomVM.
            assert(is_list(erlang:system_info(system_architecture)));
        _ ->
            assert(is_binary(erlang:system_info(system_architecture)))
    end,
    SystemVersion = erlang:system_info(system_version),
    true = is_list(SystemVersion),
    case Machine of
        "BEAM" ->
            "Erlang/OTP " ++ _ = SystemVersion;
        _ ->
            "AtomVM " ++ _ = SystemVersion
    end,
    OTPRelease = erlang:system_info(otp_release),
    true = is_list(OTPRelease),
    OTPReleaseInt = list_to_integer(OTPRelease),
    true = OTPReleaseInt > 20,
    case Machine of
        "BEAM" ->
            % beam raises badarg, and probably so should AtomVM.
            ok =
                try
                    erlang:system_info(some_weird_unused_key),
                    unexpected
                catch
                    error:badarg ->
                        ok
                end;
        _ ->
            assert(erlang:system_info(some_weird_unused_key) =:= undefined)
    end,
    0.

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

assert(true) -> ok.

test_port_count("BEAM") ->
    N = erlang:system_info(port_count),
    true = is_integer(N),
    ok;
test_port_count(_) ->
    0 = erlang:system_info(port_count),
    ok.

test_process_count("BEAM") ->
    _ = test_process_count(),
    ok;
test_process_count(_) ->
    1 = test_process_count(),
    ok.

test_process_count() ->
    Count = erlang:system_info(process_count),
    Self = self(),
    Pid = spawn_opt(?MODULE, loop, [Self], []),
    receive
        ok -> ok
    end,
    assert(erlang:system_info(process_count) =:= Count + 1),

    Pid ! {Self, stop},
    receive
        ok -> ok
    end,
    Count.
