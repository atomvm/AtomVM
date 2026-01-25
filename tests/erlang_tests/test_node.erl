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

-module(test_node).

-export([start/0]).

start() ->
    ok = test_node_no_distribution(),
    case has_setnode_creation() of
        true ->
            ok = test_node_distribution();
        false ->
            ok
    end,
    0.

test_node_no_distribution() ->
    nonode@nohost = node(),
    ok.

test_node_distribution() ->
    {NetKernelPid, MonitorRef} = spawn_opt(
        fun() ->
            receive
                quit -> ok
            end
        end,
        [monitor]
    ),
    register(net_kernel, NetKernelPid),
    true = erlang:setnode(test@test_node, 42),
    42 = get_creation(),
    test@test_node = node(),
    NetKernelPid ! quit,
    ok =
        receive
            {'DOWN', MonitorRef, process, NetKernelPid, normal} -> ok
        after 5000 -> timeout
        end,
    case node() of
        nonode@nohost ->
            ok;
        _Other ->
            % On BEAM, node may not be reset immediatly
            "BEAM" = erlang:system_info(machine),
            sleep(100),
            nonode@nohost = node()
    end,
    ok.

has_setnode_creation() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            true;
        "BEAM" ->
            OTPRelease = erlang:system_info(otp_release),
            OTPRelease >= "23"
    end.

get_creation() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            atomvm:get_creation();
        "BEAM" ->
            erts_internal:get_creation()
    end.

sleep(Ms) ->
    receive
    after Ms -> ok
    end.
