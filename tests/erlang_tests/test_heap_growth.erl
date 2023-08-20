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

-module(test_heap_growth).

-export([start/0]).

start() ->
    ok = test_grow_beyond_min_heap_size(),
    ok = test_bounded_free_strategy(false),
    ok = test_bounded_free_strategy(true),
    ok = test_minimum_strategy(),
    ok = test_fibonacci_strategy(),
    0.

test_grow_beyond_min_heap_size() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            % Heap size is set to minimum at first GC/Heap growth
            alloc_some_heap_words(10),
            {total_heap_size, 100} = process_info(self(), total_heap_size),
            Var = alloc_some_heap_words(200),
            {total_heap_size, X} = process_info(self(), total_heap_size),
            true = X > 100,
            % do something with Var to avoid compiler optimizations
            true = 200 =:= length(Var)
        end,
        [monitor, {min_heap_size, 100}]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    ok.

test_bounded_free_strategy(UseDefault) ->
    Opt =
        if
            UseDefault -> [];
            true -> [{atomvm_heap_growth, bounded_free}]
        end,
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            {total_heap_size, X1} = process_info(self(), total_heap_size),
            ok = test_growth_bounded(32),
            {total_heap_size, X2} = process_info(self(), total_heap_size),
            % Allocate again, this is when heap will be shrunk
            Var1 = alloc_some_heap_words(10),
            {total_heap_size, X3} = process_info(self(), total_heap_size),
            20 = erts_debug:flat_size(Var1),
            true = X3 < X2,
            true = X3 - X1 - erts_debug:flat_size(Var1) < 32
        end,
        [monitor | Opt]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    ok.

test_minimum_strategy() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            {total_heap_size, X1} = process_info(self(), total_heap_size),
            % Cannot really be 0 because we allocate more than just the list.
            ok = test_growth_bounded(10),
            {total_heap_size, X2} = process_info(self(), total_heap_size),
            % Allocate again, this is when heap will be shrunk
            Var1 = alloc_some_heap_words(10),
            {total_heap_size, X3} = process_info(self(), total_heap_size),
            20 = erts_debug:flat_size(Var1),
            true = X3 < X2,
            true = X3 - X1 - erts_debug:flat_size(Var1) =< 10
        end,
        [monitor, {atomvm_heap_growth, minimum}]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 500 -> timeout
        end,
    ok.

% This test is a little bit long on the CI
% It aims to test:
% - that heap sizes progress following fibonacci suite like progression
% - until a given size where they progress slower
% - and in both cases, memory is eventually reclaimed
test_fibonacci_strategy() ->
    % Test small increments follow fibonacci
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            MaxHeap = test_growth_fibonacci(50, 20000),
            % Allocate again until heap is shrunk
            NewHeap = allocate_until_heap_size_changes(MaxHeap),
            true = NewHeap < MaxHeap
        end,
        [monitor, link, {atomvm_heap_growth, fibonacci}]
    ),
    % Test large increments no longer follow fibonacci
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            MaxHeap = test_growth_fibonacci(50000, 10000000),
            % Allocate again until heap is shrunk
            NewHeap = allocate_until_heap_size_changes(MaxHeap),
            true = NewHeap < MaxHeap
        end,
        [monitor, link, {atomvm_heap_growth, fibonacci}]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 300000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, normal} -> ok
        after 300000 -> timeout
        end,
    ok.

alloc_some_heap_words(N) ->
    alloc_some_heap_words(N, []).

alloc_some_heap_words(0, Acc) -> Acc;
alloc_some_heap_words(N, Acc) -> alloc_some_heap_words(N - 1, [N | Acc]).

test_growth_bounded(Boundary) ->
    % Test growth is bounded
    Var1 = alloc_some_heap_words(10),
    {total_heap_size, X1} = process_info(self(), total_heap_size),
    Var2 = alloc_some_heap_words(10),
    {total_heap_size, X2} = process_info(self(), total_heap_size),
    Var3 = alloc_some_heap_words(10),
    {total_heap_size, X3} = process_info(self(), total_heap_size),
    true = X2 - X1 - erts_debug:flat_size(Var2) =< Boundary,
    true = X3 - X2 - erts_debug:flat_size(Var3) =< Boundary,
    true = X3 - X1 - erts_debug:flat_size(Var2) - erts_debug:flat_size(Var3) =< Boundary,
    20 = erts_debug:flat_size(Var1),
    ok.

test_growth_fibonacci(Increment, TargetMax) ->
    HeapSizes = collect_heap_sizes([], [], Increment, TargetMax),
    [MaxHeap | _] = HeapSizes,
    ok = test_fibonacci_heap_sizes(Increment, HeapSizes),
    MaxHeap.

collect_heap_sizes(Acc, Terms, Increment, TargetMax) ->
    {total_heap_size, S1} = process_info(self(), total_heap_size),
    % Heap should not shrink
    NewAcc =
        case Acc of
            [] -> [S1 | Acc];
            [S1 | _Tail] -> Acc;
            [SmallerSize | _Tail] when SmallerSize < S1 -> [S1 | Acc]
        end,
    if
        S1 >= TargetMax ->
            NewAcc;
        true ->
            collect_heap_sizes(
                NewAcc, [alloc_some_heap_words(Increment) | Terms], Increment, TargetMax
            )
    end.

test_fibonacci_heap_sizes(Increment, [HeapSize, Previous | Tail]) when HeapSize > 5709651 ->
    Diff = HeapSize - Previous,
    Delta = Diff - Previous div 5,
    true = Delta >= -4 andalso Delta =< 4,
    test_fibonacci_heap_sizes(Increment, [Previous | Tail]);
test_fibonacci_heap_sizes(Increment, [_HeapSize, _P1, P2 | _]) when 4 * Increment > P2 ->
    % * 4 because alloc_some_heap_words
    % - alloc_some_heap_words allocates approximatively twice the increment
    % - a number can be skipped because of this
    % Typically, with an increment of 2000, heaps are 17731,10958,6772,4185,2586
    % instead of: 17731,10958,4185,2586
    ok;
test_fibonacci_heap_sizes(Increment, [HeapSize, P1, P2 | Tail]) when P2 > 233 ->
    Diff = HeapSize - P1,
    Delta = Diff - P2,
    true = Delta >= -4 andalso Delta =< 4,
    test_fibonacci_heap_sizes(Increment, [P1, P2 | Tail]);
test_fibonacci_heap_sizes(_Increment, [610, 376, 233 | _]) ->
    ok;
test_fibonacci_heap_sizes(_Increment, HeapSizes) ->
    {unexpected, HeapSizes}.

allocate_until_heap_size_changes(Heap) ->
    {total_heap_size, S1} = process_info(self(), total_heap_size),
    if
        S1 =/= Heap ->
            S1;
        true ->
            alloc_some_heap_words(100),
            allocate_until_heap_size_changes(Heap)
    end.
