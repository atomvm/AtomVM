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

%% @doc Peer node for serial distribution tests.
%% Reads PTY_PATH and TEST_NAME from environment variables.
%% Derives a unique node name from the PTY device path so that
%% multiple peers can connect to the same orchestrator simultaneously.

-module(test_serial_dist_socat_peer).

-export([start/0, run_test/2]).

start() ->
    PtyPath = os:getenv("PTY_PATH"),
    TestName = os:getenv("TEST_NAME"),
    %% Derive node name from PTY, e.g. "/dev/ttys035" -> 'ttys035@serial.local'
    PtyBase = lists:last(string:split(PtyPath, "/", all)),
    NodeName = list_to_atom(PtyBase ++ "@serial.local"),
    %% Force JIT compilation of modules used during handshake
    %% so their compilation doesn't eat into the handshake timer.
    _ = crypto:module_info(),
    _ = dist_util:module_info(),
    _ = uart:module_info(),
    _ = timer_manager:module_info(),
    {ok, _} = net_kernel:start(NodeName, #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => #{
            uart_opts => [{peripheral, PtyPath}, {speed, 115200}],
            uart_module => uart
        }
    }),
    erlang:set_cookie('SerialTest'),
    run_test('a@serial.local', TestName).

run_test(PeerNode, TestName) ->
    case TestName of
        "ping" ->
            {test_serial, PeerNode} ! {self(), ping},
            receive
                {_Pid, pong} ->
                    io:format("pong~n")
            after 30000 ->
                io:format("timeout~n"),
                error(timeout)
            end;
        "rpc" ->
            {test_serial, PeerNode} ! {self(), {apply, erlang, system_info, [machine]}},
            receive
                {_Pid, Result} ->
                    io:format("~s~n", [Result])
            after 30000 ->
                io:format("timeout~n"),
                error(timeout)
            end
    end.
