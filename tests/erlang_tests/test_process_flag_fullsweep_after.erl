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

-module(test_process_flag_fullsweep_after).

-export([start/0]).

start() ->
    case get_otp_version() > 23 of
        true ->
            ok = test_process_flag_fullsweep_after(),
            ok = test_spawn_opt_fullsweep_after();
        false ->
            ok
    end,
    % Exercise the actual generational collector regardless of OTP version:
    % build and verify large live structures across many GCs, with both a
    % minor-GC-dominated and a full-sweep-only policy.
    ok = test_minor_gc_keeps_data_intact(),
    ok = test_fullsweep_after_one(),
    0.

% With a high fullsweep_after, collections are minor GCs and data is promoted
% to the old generation. Building a large list incrementally while keeping it
% live forces repeated promotion and old->young scanning. A corrupted tail
% (e.g. a cycle) would make length/3 or the comparison below loop forever or
% fail; intact data returns the expected sum.
test_minor_gc_keeps_data_intact() ->
    erlang:process_flag(fullsweep_after, 65535),
    L = build_list(5000, []),
    5000 = my_length(L, 0),
    ExpectedSum = (5000 * 5001) div 2,
    ExpectedSum = sum_list(L, 0),
    % A nested structure held live across further churn-induced GCs.
    % build_tree(Depth) has 2^Depth - 1 nodes (2^12 - 1 = 4095).
    Tree = build_tree(12),
    _ = build_list(3000, []),
    4095 = count_nodes(Tree),
    ok.

% fullsweep_after = 1 forces a full sweep on (almost) every collection, the
% opposite policy. Same data must survive identically.
test_fullsweep_after_one() ->
    erlang:process_flag(fullsweep_after, 1),
    L = build_list(5000, []),
    5000 = my_length(L, 0),
    ExpectedSum = (5000 * 5001) div 2,
    ExpectedSum = sum_list(L, 0),
    ok.

build_list(0, Acc) ->
    Acc;
build_list(N, Acc) ->
    % Allocate transient garbage to drive collections while Acc stays live.
    _ = make_garbage(50, []),
    build_list(N - 1, [N | Acc]).

% Self-contained garbage generator (avoids depending on estdlib's lists module,
% which is not linked in the test-erlang harness).
make_garbage(0, Acc) ->
    Acc;
make_garbage(N, Acc) ->
    make_garbage(N - 1, [{N, N * 2} | Acc]).

my_length([], Acc) ->
    Acc;
my_length([_ | T], Acc) ->
    my_length(T, Acc + 1).

sum_list([], Acc) ->
    Acc;
sum_list([H | T], Acc) ->
    sum_list(T, Acc + H).

build_tree(0) ->
    leaf;
build_tree(Depth) ->
    {node, build_tree(Depth - 1), build_tree(Depth - 1)}.

count_nodes(leaf) ->
    0;
count_nodes({node, L, R}) ->
    1 + count_nodes(L) + count_nodes(R).

test_process_flag_fullsweep_after() ->
    OldVal = erlang:process_flag(fullsweep_after, 10),
    10 = erlang:process_flag(fullsweep_after, 0),
    0 = erlang:process_flag(fullsweep_after, OldVal),
    ok = expect_badarg(fun() -> erlang:process_flag(fullsweep_after, -1) end),
    ok = expect_badarg(fun() -> erlang:process_flag(fullsweep_after, foo) end),
    ok.

test_spawn_opt_fullsweep_after() ->
    Parent = self(),
    spawn_opt(
        fun() ->
            {fullsweep_after, Val} = erlang:process_info(self(), fullsweep_after),
            Parent ! {fullsweep_after, Val}
        end,
        [{fullsweep_after, 42}]
    ),
    ok =
        receive
            {fullsweep_after, 42} -> ok
        after 5000 -> timeout
        end,
    ok = expect_badarg(fun() -> spawn_opt(fun() -> ok end, [{fullsweep_after, -1}]) end),
    ok = expect_badarg(fun() -> spawn_opt(fun() -> ok end, [{fullsweep_after, foo}]) end),
    ok.

expect_badarg(Fun) ->
    try
        Fun(),
        unexpected
    catch
        error:badarg -> ok
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" -> list_to_integer(erlang:system_info(otp_release));
        _ -> atomvm
    end.
