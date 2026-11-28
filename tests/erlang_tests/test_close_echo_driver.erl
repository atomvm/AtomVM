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

-module(test_close_echo_driver).

-export([start/0]).

start() ->
    N = erlang:system_info(process_count),
    Port = do_open_port("echo", []),
    11 = length(echo(Port, "Hello World")),
    ok = wrong_monitor_type(Port),
    true = register(test_monitor_port_name, Port),
    WrongRef = monitor(process, test_monitor_port_name),
    receive
        {'DOWN', WrongRef, process, {test_monitor_port_name, Node}, noproc} ->
            true = Node =:= node()
    after 5000 -> error(missing_down)
    end,
    NamedRef = monitor(port, test_monitor_port_name),
    MonitorRef = monitor(port, Port),
    Port ! {self(), close},
    ok =
        receive
            {Port, closed} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {'DOWN', MonitorRef, port, Port, normal} -> ok
        after 5000 -> timeout
        end,
    receive
        {'DOWN', NamedRef, port, {test_monitor_port_name, Node2}, normal} ->
            true = Node2 =:= node()
    after 5000 -> error(missing_named_down)
    end,
    ok = wrong_monitor_type(Port),
    P = erlang:system_info(process_count),
    N = P,
    0.

wrong_monitor_type(Port) ->
    badarg =
        try
            monitor(process, Port)
        catch
            error:R -> R
        end,
    badarg =
        try
            monitor(process, Port, [])
        catch
            error:R2 -> R2
        end,
    ok.

do_open_port(PortName, Param) ->
    open_port({spawn, PortName}, Param).

echo(Port, SendValue) ->
    Port ! {self(), SendValue},
    receive
        Value ->
            Value
    end.
