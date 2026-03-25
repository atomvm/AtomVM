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
    ok = test_node_distribution(),
    0.

test_node_no_distribution() ->
    nonode@nohost = node(),
    nonode@nohost = node(self()),
    nonode@nohost = node(make_ref()),
    ok = assert_badarg(fun() -> node(test) end),
    ok = assert_badarg(fun() -> node({test, nonode@nohost}) end),
    test@test_node = node(external_pid(67)),
    test@test_node = node(external_ref(67)),
    test@test_node = node(external_port(test@test_node, 67)),
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
    test@test_node = node(self()),
    test@test_node = node(make_ref()),
    test@test_node = node(external_port(test@test_node, 42)),
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

get_creation() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            atomvm:get_creation();
        "BEAM" ->
            erts_internal:get_creation()
    end.

external_pid(Creation) ->
    binary_to_term(
        <<131, 88, 119, 14, "test@test_node", 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, Creation>>
    ).

external_ref(Creation) ->
    binary_to_term(
        <<131, 90, 0, 1, 119, 14, "test@test_node", Creation:32, 1:32>>
    ).

external_port(Node, Creation) ->
    term_from_node(Node, fun(NodeBin) ->
        binary_to_term(<<131, 120, NodeBin/binary, 43:64, Creation:32>>)
    end).

term_from_node(Node, Fun) ->
    NodeBin = atom_to_binary(Node, utf8),
    Fun(<<119, (byte_size(NodeBin)), NodeBin/binary>>).

assert_badarg(Fun) ->
    try
        Fun(),
        unexpected_success
    catch
        error:badarg -> ok
    end.

sleep(Ms) ->
    receive
    after Ms -> ok
    end.
