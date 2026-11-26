%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_exit2).

-export([start/0]).

start() ->
    ok = test_reason_other(),
    ok = test_reason_kill(),
    ok = test_reason_normal_not_self(),
    ok = test_reason_normal_self(),
    ok = test_exit_dead(),
    ok = test_exit_signal(),
    0.

test_reason_other() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
            after 500 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, other),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, other} -> ok
        after 5000 -> timeout
        end,
    Parent = self(),
    Pid2 = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end
        end,
        []
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 5000 -> timeout
        end,
    true = exit(Pid2, other),
    ok =
        receive
            {Pid2, {'EXIT', Parent, other}} -> ok;
            Other -> Other
        after 5000 -> timeout
        end,
    ok.

test_reason_kill() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
            after 5000 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, kill),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, killed} -> ok
        after 5000 -> timeout
        end,
    Parent = self(),
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            receive
            after 5000 -> ok
            end
        end,
        [monitor]
    ),
    {Pid3, Ref3} = spawn_opt(
        fun() ->
            link(Pid2),
            Parent ! {self(), ready},
            receive
            after 5000 -> ok
            end
        end,
        [monitor]
    ),
    {Pid4, Ref4} = spawn_opt(
        fun() ->
            link(Pid2),
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            receive
                {'EXIT', _From, _Reason} = ExitMessage -> Parent ! {self(), ExitMessage}
            after 5000 -> ok
            end
        end,
        [monitor]
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {Pid3, ready} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {Pid4, ready} -> ok
        after 5000 -> timeout
        end,
    true = exit(Pid2, kill),
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, killed} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref3, process, Pid3, killed} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref4, process, Pid4, normal} -> ok
        after 5000 -> timeout
        end,
    Pid2 =
        receive
            {Pid4, {'EXIT', Pid, killed}} -> Pid
        after 5000 -> timeout
        end,
    ok.

test_reason_normal_not_self() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
                {Caller, ping} -> Caller ! {self(), pong}
            after 5000 -> ok
            end
        end,
        [monitor]
    ),
    true = exit(Pid1, normal),
    Pid1 ! {self(), ping},
    ok =
        receive
            {Pid1, pong} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 5000 -> timeout
        end,
    Parent = self(),
    Pid2 = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            Parent ! {self(), ready},
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end
        end,
        []
    ),
    ok =
        receive
            {Pid2, ready} -> ok
        after 5000 -> timeout
        end,
    true = exit(Pid2, normal),
    ok =
        receive
            {Pid2, {'EXIT', Parent, normal}} -> ok;
            Other -> Other
        after 5000 -> timeout
        end,
    ok.

test_reason_normal_self() ->
    Parent = self(),
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            exit(self(), normal),
            Parent ! {self(), unexpected},
            exit(unexpected)
        end,
        [monitor]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 5000 -> timeout
        end,
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            process_flag(trap_exit, true),
            exit(self(), normal),
            ok =
                receive
                    {'EXIT', _From, _Reason} = ExitMessage ->
                        Parent ! {self(), ExitMessage},
                        ok;
                    Other ->
                        Parent ! {self(), {unexpected, Other}}
                end,
            exit(expected)
        end,
        [monitor]
    ),
    ok =
        receive
            {Pid2, {'EXIT', Pid2, normal}} -> ok;
            Other -> Other
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, expected} -> ok
        after 5000 -> timeout
        end,
    ok.

test_exit_dead() ->
    {Pid1, Ref1} = spawn_opt(fun() -> ok end, [monitor]),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 5000 -> timeout
        end,
    true = exit(Pid1, normal),
    true = exit(Pid1, kill),
    true = exit(Pid1, other),
    ok.

-if(?OTP_RELEASE >= 29).

test_exit_signal() ->
    Parent = self(),
    {Pid, Ref} = spawn_opt(
        fun() ->
            true = erlang:exit_signal(self(), normal),
            %% Receiving also forces reception of our own pending signal.
            receive
                unexpected_message -> error(unexpected_message)
            after 0 -> Parent ! {self(), survived}
            end
        end,
        [monitor]
    ),
    receive
        {'DOWN', Ref, process, Pid, DownReason} -> normal = DownReason
    end,
    receive
        {Pid, survived} -> ok
    end,
    foreach(
        fun(Reason) ->
            {Child, Monitor} = spawn_opt(
                fun() ->
                    process_flag(trap_exit, true),
                    Parent ! {self(), ready},
                    [before_signal, {'EXIT', Parent, Reason}, after_signal] =
                        [
                            receive
                                Message -> Message
                            end
                         || _ <- [1, 2, 3]
                        ]
                end,
                [monitor]
            ),
            receive
                {Child, ready} -> ok
            end,
            Child ! before_signal,
            true = erlang:exit_signal(Child, Reason),
            Child ! after_signal,
            receive
                {'DOWN', Monitor, process, Child, normal} -> ok
            end
        end,
        [normal, {shutdown, <<37:1024>>}]
    ),
    foreach(
        fun(Trap) ->
            foreach(
                fun(Reason) ->
                    {Child, Monitor} = spawn_opt(
                        fun() ->
                            process_flag(trap_exit, Trap),
                            %% Fatal signals are not catchable exceptions, even for self.
                            try
                                erlang:exit_signal(self(), Reason)
                            catch
                                _:_ -> exit(caught_signal)
                            end,
                            receive
                                unexpected_message -> exit(unexpected_message)
                            after 0 -> exit(missed_signal)
                            end
                        end,
                        [monitor]
                    ),
                    Expected =
                        case Reason of
                            kill -> killed;
                            _ -> Reason
                        end,
                    receive
                        {'DOWN', Monitor, process, Child, Expected} -> ok
                    end
                end,
                case Trap of
                    true -> [kill];
                    false -> [kill, {shutdown, 37}]
                end
            )
        end,
        [false, true]
    ),
    foreach(
        fun(Options) ->
            {Child, Monitor} = spawn_opt(
                fun() ->
                    process_flag(trap_exit, true),
                    Alias = alias(Options),
                    Parent ! {self(), Alias},
                    %% Neither exit consumes a reply alias. Messages must remain ordered.
                    [{'EXIT', Parent, first}, {'EXIT', Parent, second}] =
                        [
                            receive
                                Message -> Message
                            end
                         || _ <- [1, 2]
                        ],
                    true = unalias(Alias),
                    true = erlang:exit_signal(Alias, kill),
                    true = erlang:exit_signal(make_ref(), kill),
                    receive
                        unexpected_message -> error(unexpected_message)
                    after 0 -> ok
                    end
                end,
                [monitor]
            ),
            receive
                {Child, Alias} ->
                    true = erlang:exit_signal(Alias, first),
                    true = erlang:exit_signal(Alias, second)
            end,
            receive
                {'DOWN', Monitor, process, Child, normal} -> ok
            end
        end,
        [[], [reply]]
    ),
    true = erlang:exit_signal(Pid, kill),
    foreach(
        fun(Value) ->
            try erlang:exit_signal(Value, reason) of
                _ -> error(accepted_invalid_destination)
            catch
                error:badarg -> ok
            end
        end,
        [bad, 7, [], {self()}]
    ),
    case erlang:system_info(machine) of
        "BEAM" -> ok;
        _ -> test_exit_signal_port()
    end.

foreach(_Fun, []) ->
    ok;
foreach(Fun, [Value | Rest]) ->
    Fun(Value),
    foreach(Fun, Rest).

test_exit_signal_port() ->
    Port = open_port({spawn, "echo"}, []),
    unlink(Port),
    Ref = monitor(port, Port),
    true = erlang:exit_signal(Port, normal),
    Port ! {self(), signal_port_alive},
    receive
        signal_port_alive -> ok
    end,
    true = erlang:exit_signal(Port, kill),
    receive
        {'DOWN', Ref, port, Port, killed} -> ok
    end,
    true = erlang:exit_signal(Port, kill),
    ok.

-else.

test_exit_signal() -> ok.

-endif.
