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

-module(test_net_kernel).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    Platform = erlang:system_info(machine),
    case has_compatible_erl(Platform) andalso has_epmd(Platform) of
        true ->
            ok = ensure_epmd(Platform),
            ok = setup(Platform),
            ok = test_ping_from_beam(Platform),
            ok = test_fail_with_wrong_cookie(Platform),
            ok = test_rpc_from_beam(Platform),
            ok = test_rpc_loop_from_beam(Platform),
            ok = test_autoconnect_fail(Platform),
            ok = test_autoconnect_to_beam(Platform),
            ok;
        false ->
            io:format("~s: skipped\n", [?MODULE]),
            ok
    end.

%% Determine if we can connect with BEAM.
%% This test currently supports AtomVM connecting with OTP 24+
%% as well as any version of OTP connecting with OTP
has_compatible_erl("BEAM" = Platform) ->
    has_command(Platform, "erl");
has_compatible_erl("ATOM") ->
    case has_command("ATOM", "erl") of
        true ->
            Result = execute_command(
                "ATOM",
                "erl -eval \"io:put_chars(erlang:system_info(otp_release) ++ [13,10]).\" -s init stop -noshell"
            ),
            Result >= "24";
        false ->
            false
    end.

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

test_ping_from_beam(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    % erlang:set_cookie/1 is from OTP 24.1
    erlang:set_cookie(Node, 'AtomVM'),
    Result = execute_command(
        Platform,
        "erl -sname otp -setcookie AtomVM -eval \"R = net_adm:ping('" ++
            atom_to_list(Node) ++
            "'), erlang:display(R).\" -s init stop -noshell"
    ),
    "pong" ++ _ = Result,
    net_kernel:stop(),
    ok.

test_fail_with_wrong_cookie(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    Result = execute_command(
        Platform,
        "erl -sname otp -setcookie Wrong -eval \"R = net_adm:ping('" ++
            atom_to_list(Node) ++
            "'), erlang:display(R).\" -s init stop -noshell"
    ),
    "pang" ++ _ = Result,
    net_kernel:stop(),
    ok.

test_rpc_from_beam(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    Result = execute_command(
        Platform,
        "erl -sname otp -setcookie AtomVM -eval \"R = rpc:call('" ++ atom_to_list(Node) ++
            "', erlang, system_info, [machine]), erlang:display(R).\" -s init stop -noshell"
    ),
    true = Result =:= lists:flatten(io_lib:format("~p\r\n", [Platform])),
    net_kernel:stop(),
    ok.

test_rpc_loop_from_beam(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    Result = execute_command(
        Platform,
        "erl -sname otp -setcookie AtomVM -eval \"R = lists:foldl(fun(X, Acc) -> R = rpc:call('" ++
            atom_to_list(Node) ++
            "', erlang, system_info, [machine]), if Acc =:= R -> Acc; Acc =:= undefined -> R end end, undefined, lists:seq(1, 10)), erlang:display(R).\" -s init stop -noshell"
    ),
    true = Result =:= lists:flatten(io_lib:format("~p\r\n", [Platform])),
    net_kernel:stop(),
    ok.

test_autoconnect_fail(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    [_, Host] = string:split(atom_to_list(Node), "@"),
    OTPNode = list_to_atom("otp@" ++ Host),
    {beam, OTPNode} ! {self(), ping},
    net_kernel:stop(),
    ok.

test_autoconnect_to_beam(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    spawn_link(fun() ->
        execute_command(
            Platform,
            "erl -sname otp -setcookie AtomVM -eval \""
            "register(beam, self()),"
            "F = fun(G) ->"
            " receive"
            "   {Caller, ping} -> Caller ! {self(), pong}, G(G);"
            "   {Caller, quit} -> Caller ! {self(), quit}"
            "   after 5000 -> timeout"
            " end "
            "end, "
            "F(F).\" -s init stop -noshell"
        )
    end),
    % Wait sufficiently for beam to be up, without connecting to it since
    % that's part of the test
    timer:sleep(1000),
    [_, Host] = string:split(atom_to_list(Node), "@"),
    OTPNode = list_to_atom("otp@" ++ Host),
    {beam, OTPNode} ! {self(), ping},
    {ok, OTPPid} =
        receive
            {OTPPid0, pong} -> {ok, OTPPid0}
        after 5000 -> timeout
        end,
    OTPPid ! {self(), ping},
    ok =
        receive
            {OTPPid, pong} -> ok
        after 5000 -> timeout
        end,
    erlang:send({beam, OTPNode}, {self(), ping}),
    ok =
        receive
            {OTPPid, pong} -> ok
        after 5000 -> timeout
        end,
    erlang:send(OTPPid, {self(), quit}),
    ok =
        receive
            {OTPPid, quit} -> ok
        after 5000 -> timeout
        end,
    net_kernel:stop(),
    ok.

% On AtomVM, we need to start kernel.
setup("BEAM") ->
    ok;
setup("ATOM") ->
    {ok, _KernelPid} = kernel:start(normal, []),
    ok.

execute_command("BEAM", Command) ->
    os:cmd(Command);
execute_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", Command], undefined, [stdout]),
    Result = loop_read(Fd, []),
    ok = atomvm:posix_close(Fd),
    Result.

loop_read(Fd, Acc) ->
    case atomvm:posix_read(Fd, 10) of
        eof ->
            lists:flatten(lists:reverse(Acc));
        {error, eintr} ->
            % used with lldb ;-)
            loop_read(Fd, Acc);
        {ok, Line} ->
            loop_read(Fd, [binary_to_list(Line) | Acc])
    end.

net_kernel_start("ATOM", Nodename) ->
    net_kernel:start(Nodename, #{name_domain => shortnames});
net_kernel_start("BEAM", Nodename) ->
    OTPRelease = erlang:system_info(otp_release),
    if
        OTPRelease >= "24" ->
            net_kernel:start(Nodename, #{name_domain => shortnames});
        true ->
            net_kernel:start([Nodename, shortnames])
    end.
