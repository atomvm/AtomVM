%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_epmd).

-export([test/0]).

test() ->
    % AtomVM's epmd only runs on AtomVM and OTP 24+
    CanRunEpmd =
        case erlang:system_info(machine) of
            "ATOM" ->
                true;
            "BEAM" ->
                OTPRelease = erlang:system_info(otp_release),
                OTPRelease >= "24"
        end,
    if
        CanRunEpmd ->
            case stop_epmd() of
                ok ->
                    {ok, Pid} = epmd:start_link([]),
                    ok = test_client(),
                    ok = test_two_clients(),
                    MonitorRef = monitor(process, Pid),
                    unlink(Pid),
                    exit(Pid, shutdown),
                    ok =
                        receive
                            {'DOWN', MonitorRef, process, Pid, shutdown} -> ok
                        after 5000 -> timeout
                        end,
                    ok;
                {error, not_found} ->
                    ok
            end;
        true ->
            ok
    end,
    case start_epmd() of
        ok ->
            ok = test_client(),
            ok = test_two_clients();
        {error, not_found} ->
            ok
    end,
    ok.

has_epmd(Platform) ->
    has_command(Platform, "epmd").

has_command("BEAM", Command) ->
    R = os:cmd("command -v " ++ Command),
    R =/= [];
has_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "command -v " ++ Command], undefined, [
        stdout
    ]),
    Result =
        case atomvm:posix_read(Fd, 200) of
            eof -> false;
            {ok, _Line} -> true
        end,
    ok = atomvm:posix_close(Fd),
    Result.

ensure_epmd("BEAM") ->
    _ = os:cmd("epmd -daemon"),
    ok;
ensure_epmd("ATOM") ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "epmd -daemon"], undefined, [stdout]),
    ok = atomvm:posix_close(Fd),
    ok.

stop_epmd("BEAM") ->
    case os:cmd("epmd -kill") of
        "Killed\n" ->
            timer:sleep(500),
            ok;
        "epmd: Cannot connect to local epmd\n" ->
            ok;
        "Killing not allowed - " ->
            {error, not_allowed}
    end;
stop_epmd("ATOM") ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "epmd -kill 2>&1"], undefined, [stdout]),
    Result =
        case atomvm:posix_read(Fd, 200) of
            eof ->
                {error, eof};
            {ok, <<"Killed\n">>} ->
                timer:sleep(500),
                ok;
            {ok, <<"epmd: Cannot connect to local epmd\n">>} ->
                ok;
            {ok, <<"Killing not allowed - ", _/binary>>} ->
                {error, not_allowed}
        end,
    ok = atomvm:posix_close(Fd),
    Result.

start_epmd() ->
    Platform = erlang:system_info(machine),
    case has_epmd(Platform) of
        true ->
            ok = ensure_epmd(Platform),
            % on CI, epmd may be slow to accept connections
            timer:sleep(500),
            ok;
        false ->
            {error, not_found}
    end.

stop_epmd() ->
    Platform = erlang:system_info(machine),
    case has_epmd(Platform) of
        true ->
            stop_epmd(Platform);
        false ->
            {error, not_found}
    end.

test_client() ->
    {ok, Pid1} = erl_epmd:start_link(),
    ok =
        case erl_epmd:port_please("test_epmd", "host.invalid") of
            noport ->
                ok;
            {error, nxdomain} ->
                "BEAM" = erlang:system_info(machine),
                true = erlang:system_info(otp_release) =< "22",
                ok
        end,
    noport = erl_epmd:port_please("test_epmd", "localhost"),
    {ok, Creation1} = erl_epmd:register_node("test_epmd", 12345),
    {port, 12345, Version} = erl_epmd:port_please("test_epmd", "localhost"),
    case erlang:system_info(machine) of
        "BEAM" ->
            case erlang:system_info(otp_release) of
                "21" -> 5 = Version;
                "22" -> 5 = Version;
                _ -> 6 = Version
            end;
        "ATOM" ->
            6 = Version
    end,
    {error, already_registered} = erl_epmd:register_node("test_epmd", 12345),
    {error, already_registered} = erl_epmd:register_node("test_epmd_new", 12346),
    {ok, Names} = erl_epmd:names("localhost"),
    true = lists:member({"test_epmd", 12345}, Names),
    MonitorRef1 = monitor(process, Pid1),
    unlink(Pid1),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef1, process, Pid1, Reason1} -> Reason1
        after 5000 -> timeout
        end,

    {ok, Pid2} = erl_epmd:start_link(),
    noport = erl_epmd:port_please("test_epmd", "localhost"),
    {ok, Creation2} = erl_epmd:register_node("test_epmd", 12345),
    true = Creation1 =/= Creation2,
    MonitorRef2 = monitor(process, Pid2),
    unlink(Pid2),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef2, process, Pid2, Reason2} -> Reason2
        after 5000 -> timeout
        end,

    ok.

test_two_clients() ->
    {ok, Pid1} = erl_epmd:start_link(),
    {ok, _Creation1} = erl_epmd:register_node("test_epmd_1", 12345),
    unregister(erl_epmd),
    {ok, Pid2} = erl_epmd:start_link(),
    {ok, _Creation2} = erl_epmd:register_node("test_epmd_2", 12346),
    {ok, Names} = erl_epmd:names("localhost"),
    true = lists:member({"test_epmd_1", 12345}, Names),
    true = lists:member({"test_epmd_2", 12346}, Names),
    unlink(Pid2),
    MonitorRef2 = monitor(process, Pid2),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef2, process, Pid2, Reason2} -> Reason2
        after 5000 -> timeout
        end,
    register(erl_epmd, Pid1),
    MonitorRef1 = monitor(process, Pid1),
    unlink(Pid1),
    erl_epmd:stop(),
    shutdown =
        receive
            {'DOWN', MonitorRef1, process, Pid1, Reason1} -> Reason1
        after 5000 -> timeout
        end,
    ok.
