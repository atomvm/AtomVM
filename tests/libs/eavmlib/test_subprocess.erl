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

-module(test_subprocess).

-export([test/0]).

test() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            test_atomvm();
        _ ->
            %% atomvm:subprocess/posix_kill are only available on AtomVM
            ok
    end.

test_atomvm() ->
    case atomvm:platform() of
        generic_unix ->
            ok = test_posix_kill(),
            ok = test_posix_kill_badarg(),
            ok = test_posix_kill_esrch(),
            ok;
        _ ->
            %% atomvm:subprocess/posix_kill are only available on generic_unix
            ok
    end.

%% A subprocess that would outlive the test unless killed: posix_kill
%% terminates it, observed as EOF on its stdout pipe (the write end is
%% closed when the process dies).
test_posix_kill() ->
    %% exec so the subprocess pid is sleep itself, not the wrapping shell:
    %% killing the shell would not reliably reach sleep, which inherits the
    %% stdout pipe and would keep read_until_eof/1 blocked.
    {ok, OsPid, Fd} = atomvm:subprocess(
        "/bin/sh", ["sh", "-c", "exec sleep 30"], undefined, [stdout]
    ),
    true = is_integer(OsPid),
    %% SIGTERM
    ok = atomvm:posix_kill(OsPid, 15),
    eof = read_until_eof(Fd),
    ok = atomvm:posix_close(Fd),
    ok.

test_posix_kill_badarg() ->
    ok = expect_badarg(fun() -> atomvm:posix_kill(not_a_pid, 15) end),
    ok = expect_badarg(fun() -> atomvm:posix_kill(1, not_a_signal) end),
    %% Oversized signal must be rejected rather than truncated to a valid one.
    ok = expect_badarg(fun() -> atomvm:posix_kill(1, 1 bsl 40) end),
    ok.

expect_badarg(Fun) ->
    try Fun() of
        _ -> fail
    catch
        error:badarg -> ok
    end.

%% Signalling a pid that cannot exist fails with esrch.
test_posix_kill_esrch() ->
    {error, esrch} = atomvm:posix_kill(536870911, 0),
    ok.

read_until_eof(Fd) ->
    case atomvm:posix_read(Fd, 64) of
        eof ->
            eof;
        {ok, _Data} ->
            read_until_eof(Fd);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 5000 ->
                exit(posix_kill_eof_timeout)
            end,
            read_until_eof(Fd)
    end.
