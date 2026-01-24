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

%% Generate unique node name for each test using line number
-define(OTP_SNAME, "otp_" ++ integer_to_list(?LINE)).

start() ->
    test().

test() ->
    Platform = erlang:system_info(machine),
    case has_compatible_erl(Platform) andalso has_epmd(Platform) of
        true ->
            ok = ensure_epmd(Platform),
            ok = test_ping_from_beam(Platform),
            ok = test_fail_with_wrong_cookie(Platform),
            ok = test_rpc_from_beam(Platform),
            ok = test_rpc_loop_from_beam(Platform),
            ok = test_autoconnect_fail(Platform),
            ok = test_autoconnect_to_beam(Platform),
            ok = test_groupleader(Platform),
            ok = test_link_remote_exit_remote(Platform),
            ok = test_link_remote_exit_local(Platform),
            ok = test_link_local_unlink_remote(Platform),
            ok = test_link_local_unlink_local(Platform),
            ok = test_is_alive(Platform),
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
        "erl -sname " ++ ?OTP_SNAME ++ " -setcookie AtomVM -eval \"R = net_adm:ping('" ++
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
        "erl -sname " ++ ?OTP_SNAME ++ " -setcookie Wrong -eval \"R = net_adm:ping('" ++
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
        "erl -sname " ++ ?OTP_SNAME ++ " -setcookie AtomVM -eval \"R = rpc:call('" ++
            atom_to_list(Node) ++
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
        "erl -sname " ++ ?OTP_SNAME ++
            " -setcookie AtomVM -eval \"R = lists:foldl(fun(X, Acc) -> R = rpc:call('" ++
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
    OtpSname = ?OTP_SNAME,
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    Parent = self(),
    erlang:set_cookie(Node, 'AtomVM'),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            Command =
                "erl -sname " ++ OtpSname ++
                    " -setcookie AtomVM -eval \""
                    "register(beam, self()),"
                    "io:put_chars(<<114,101,97,100,121,13,10>>),"
                    "F = fun(G) ->"
                    " receive"
                    "   {Caller, ping} -> Caller ! {self(), pong}, G(G);"
                    "   {Caller, net_adm_ping} -> Caller ! {self(), net_adm:ping('" ++
                    atom_to_list(Node) ++
                    "')}, G(G);"
                    "   {Caller, quit} -> Caller ! {self(), quit}"
                    "   after 5000 -> exit(timeout)"
                    " end "
                    "end, "
                    "F(F).\" -s init stop -noshell",
            Obj = subprocess(Platform, Command),
            "ready\r\n" = subprocess_line(Platform, Obj),
            Parent ! {self(), ready},
            [] = subprocess_loop(Platform, Obj)
        end,
        [link, monitor]
    ),
    ok =
        receive
            {Pid, ready} -> ok
        after 5000 -> timeout
        end,
    [_, Host] = string:split(atom_to_list(Node), "@"),
    OTPNode = list_to_atom(OtpSname ++ "@" ++ Host),
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
    OTPPid ! {self(), net_adm_ping},
    ok =
        receive
            {OTPPid, pong} -> ok
        after 5000 -> timeout
        end,
    % Ensure there is no leak
    {monitored_by, []} = process_info(whereis(net_kernel), monitored_by),
    erlang:send(OTPPid, {self(), quit}),
    ok =
        receive
            {OTPPid, quit} -> ok
        after 5000 -> timeout
        end,
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        after 5000 -> timeout
        end,
    net_kernel:stop(),
    ok.

start_apply_loop(Platform) ->
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    Node = node(),
    erlang:set_cookie(Node, 'AtomVM'),
    register(atomvm, self()),
    Parent = self(),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            Result = execute_command(
                Platform,
                "erl -sname " ++ ?OTP_SNAME ++
                    " -setcookie AtomVM -eval \""
                    "{atomvm, '" ++ atom_to_list(Node) ++
                    "'} ! {beam, self()}, "
                    "F = fun(G) ->"
                    " receive"
                    "   {Caller, apply, M, F, A} -> Result = apply(M, F, A), Caller ! {self(), Result}, G(G);"
                    "   {Caller, spawn} -> Result = spawn(fun() -> G(G) end), Caller ! {self(), Result}, G(G);"
                    "   {Caller, exit, Reason} -> Caller ! {self(), exiting}, exit(Reason);"
                    "   {Caller, quit} -> Caller ! {self(), quit};"
                    "   {Caller, flush_exit} -> receive {'EXIT', Pid, Reason} -> Caller ! {self(), {exit, Pid, Reason}} after 5000 -> exit(timeout) end, G(G)"
                    "   after 5000 -> exit(timeout)"
                    " end "
                    "end, "
                    "F(F).\" -s init stop -noshell"
            ),
            Parent ! {io_result, Result}
        end,
        [link, monitor]
    ),
    BeamMainPid = start_apply_loop_io(),
    {BeamMainPid, Pid, MonitorRef}.

start_apply_loop_io() ->
    receive
        {beam, BeamMainPid0} ->
            BeamMainPid0;
        {io_result, []} ->
            start_apply_loop_io();
        {io_result, Result0} ->
            io:format("~s\n", [Result0]),
            start_apply_loop_io()
    after 5000 -> exit(timeout)
    end.

call_apply_loop(Pid, Message) ->
    Pid ! Message,
    receive
        {Pid, Result} ->
            Result;
        {io_result, Result1} ->
            io:format("~s\n", [Result1]),
            exit(timeout)
    after 5000 -> exit(timeout)
    end.

stop_apply_loop(BeamMainPid, Pid, MonitorRef) ->
    quit = call_apply_loop(BeamMainPid, {self(), quit}),
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        after 5000 -> timeout
        end,
    net_kernel:stop(),
    unregister(atomvm),
    ok.

test_groupleader(Platform) ->
    {BeamMainPid, Pid, MonitorRef} = start_apply_loop(Platform),
    Node = node(),
    ok = call_apply_loop(
        BeamMainPid, {self(), apply, rpc, call, [Node, io, format, ["hello group leader"]]}
    ),
    ok = stop_apply_loop(BeamMainPid, Pid, MonitorRef),
    "hello group leader" =
        receive
            {io_result, IOResult} -> IOResult
        after 5000 -> timeout
        end,
    ok.

test_link_remote_exit_remote(Platform) ->
    Main = self(),
    {BeamMainPid, Pid, MonitorRef} = start_apply_loop(Platform),
    SpawnedPid = call_apply_loop(BeamMainPid, {self(), spawn}),
    {LocalSpawnedPid, LocalSpawnedMonitor} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            receive
                {'EXIT', ExitPid, Reason} -> Main ! {exit, ExitPid, Reason}
            after 5000 -> exit(timeout)
            end
        end,
        [monitor]
    ),
    true = call_apply_loop(SpawnedPid, {self(), apply, erlang, link, [LocalSpawnedPid]}),
    {links, [LocalSpawnedPid]} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    {links, [SpawnedPid]} = process_info(LocalSpawnedPid, links),
    exiting = call_apply_loop(SpawnedPid, {self(), exit, some_reason}),
    ok = stop_apply_loop(BeamMainPid, Pid, MonitorRef),
    some_reason =
        receive
            {exit, SpawnedPid, ExitReason} -> ExitReason
        after 5000 -> timeout
        end,
    normal =
        receive
            {'DOWN', LocalSpawnedMonitor, process, LocalSpawnedPid, MonitorReason} -> MonitorReason
        after 5000 -> timeout
        end,
    ok.

test_link_remote_exit_local(Platform) ->
    {BeamMainPid, Pid, MonitorRef} = start_apply_loop(Platform),
    SpawnedPid = call_apply_loop(BeamMainPid, {self(), spawn}),
    {LocalSpawnedPid, LocalSpawnedMonitor} = spawn_opt(
        fun() ->
            receive
            after 5000 -> exit(timeout)
            end
        end,
        [monitor]
    ),
    true = call_apply_loop(SpawnedPid, {self(), apply, erlang, link, [LocalSpawnedPid]}),
    {links, [LocalSpawnedPid]} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    {links, [SpawnedPid]} = process_info(LocalSpawnedPid, links),
    false = call_apply_loop(SpawnedPid, {self(), apply, erlang, process_flag, [trap_exit, true]}),
    true = exit(LocalSpawnedPid, some_reason),
    some_reason =
        receive
            {'DOWN', LocalSpawnedMonitor, process, LocalSpawnedPid, MonitorReason} -> MonitorReason
        after 5000 -> timeout
        end,
    {exit, LocalSpawnedPid, some_reason} = call_apply_loop(SpawnedPid, {self(), flush_exit}),
    quit = call_apply_loop(SpawnedPid, {self(), quit}),
    ok = stop_apply_loop(BeamMainPid, Pid, MonitorRef),
    ok.

test_link_local_unlink_remote(Platform) ->
    {BeamMainPid, Pid, MonitorRef} = start_apply_loop(Platform),
    SpawnedPid = call_apply_loop(BeamMainPid, {self(), spawn}),
    {LocalSpawnedPid, LocalSpawnedMonitor} = spawn_opt(
        fun() ->
            receive
                {Caller, link} ->
                    Result = link(SpawnedPid),
                    Caller ! {self(), Result}
            after 5000 -> exit(timeout)
            end,
            receive
                quit -> ok
            after 5000 -> exit(timeout)
            end
        end,
        [monitor]
    ),
    {links, []} = process_info(LocalSpawnedPid, links),
    LocalSpawnedPid ! {self(), link},
    receive
        {LocalSpawnedPid, true} -> ok
    end,
    {links, [SpawnedPid]} = process_info(LocalSpawnedPid, links),
    {links, [LocalSpawnedPid]} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    true = call_apply_loop(SpawnedPid, {self(), apply, erlang, unlink, [LocalSpawnedPid]}),
    {links, []} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    {links, []} = process_info(LocalSpawnedPid, links),
    LocalSpawnedPid ! quit,
    normal =
        receive
            {'DOWN', LocalSpawnedMonitor, process, LocalSpawnedPid, MonitorReason} -> MonitorReason
        after 5000 -> timeout
        end,
    quit = call_apply_loop(SpawnedPid, {self(), quit}),
    ok = stop_apply_loop(BeamMainPid, Pid, MonitorRef),
    ok.

test_link_local_unlink_local(Platform) ->
    {BeamMainPid, Pid, MonitorRef} = start_apply_loop(Platform),
    SpawnedPid = call_apply_loop(BeamMainPid, {self(), spawn}),
    {LocalSpawnedPid, LocalSpawnedMonitor} = spawn_opt(
        fun() ->
            receive
                {LinkCaller, link} ->
                    LinkResult = link(SpawnedPid),
                    LinkCaller ! {self(), LinkResult}
            after 5000 -> exit(timeout)
            end,
            receive
                {UnlinkCaller, unlink} ->
                    UnlinkResult = unlink(SpawnedPid),
                    UnlinkCaller ! {self(), UnlinkResult}
            after 5000 -> exit(timeout)
            end,
            receive
                quit -> ok
            after 5000 -> exit(timeout)
            end
        end,
        [monitor]
    ),
    {links, []} = process_info(LocalSpawnedPid, links),
    LocalSpawnedPid ! {self(), link},
    receive
        {LocalSpawnedPid, true} -> ok
    end,
    {links, [SpawnedPid]} = process_info(LocalSpawnedPid, links),
    {links, [LocalSpawnedPid]} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    LocalSpawnedPid ! {self(), unlink},
    receive
        {LocalSpawnedPid, true} -> ok
    end,
    {links, []} = call_apply_loop(
        SpawnedPid, {self(), apply, erlang, process_info, [SpawnedPid, links]}
    ),
    {links, []} = process_info(LocalSpawnedPid, links),
    LocalSpawnedPid ! quit,
    normal =
        receive
            {'DOWN', LocalSpawnedMonitor, process, LocalSpawnedPid, MonitorReason} -> MonitorReason
        after 5000 -> timeout
        end,
    quit = call_apply_loop(SpawnedPid, {self(), quit}),
    ok = stop_apply_loop(BeamMainPid, Pid, MonitorRef),
    ok.

test_is_alive(Platform) ->
    false = is_alive(),
    {ok, _NetKernelPid} = net_kernel_start(Platform, atomvm),
    true = is_alive(),
    net_kernel:stop(),
    false = is_alive(),
    ok.

subprocess("BEAM", Command) ->
    open_port({spawn_executable, "/bin/sh"}, [{args, ["-c", Command]}, {line, 256}, eof, in]);
subprocess("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", Command], undefined, [stdout]),
    Fd.

subprocess_line("BEAM", Port) ->
    receive
        {Port, {data, {eol, Line}}} -> lists:flatten([Line, "\r\n"])
    after 5000 ->
        exit(timeout)
    end;
subprocess_line("ATOM", Fd) ->
    {ok, Line} = atomvm:posix_read(Fd, 10),
    binary_to_list(Line).

subprocess_loop("BEAM", Port) ->
    Result = beam_loop_read(Port, []),
    port_close(Port),
    Result;
subprocess_loop("ATOM", Fd) ->
    Result = atomvm_loop_read(Fd, []),
    ok = atomvm:posix_close(Fd),
    Result.

execute_command(Machine, Command) ->
    Obj = subprocess(Machine, Command),
    subprocess_loop(Machine, Obj).

atomvm_loop_read(Fd, Acc) ->
    case atomvm:posix_read(Fd, 10) of
        eof ->
            lists:flatten(lists:reverse(Acc));
        {error, eintr} ->
            % used with lldb ;-)
            atomvm_loop_read(Fd, Acc);
        {ok, Line} ->
            atomvm_loop_read(Fd, [binary_to_list(Line) | Acc])
    end.

beam_loop_read(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} -> beam_loop_read(Port, ["\r\n", Line | Acc]);
        {Port, {data, {noeol, Line}}} -> beam_loop_read(Port, [Line | Acc]);
        {Port, eof} -> lists:flatten(lists:reverse(Acc))
    after 5000 ->
        exit(timeout)
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
