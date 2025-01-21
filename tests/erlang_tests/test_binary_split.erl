%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_binary_split).

-export([start/0, split_compare/3, split_compare/2, compare_bin/2, fail_split/1, id/1]).

start() ->
    ok = split_compare(<<"Hello:World">>, <<"Hello">>, <<"World">>),
    ok = split_compare(<<"Hello:::World:">>, <<"Hello">>, <<"::World:">>),
    ok = split_compare(<<"Test:">>, <<"Test">>, <<>>),
    ok = split_compare(<<":">>, <<>>, <<>>),
    ok = split_compare(<<>>, <<>>),
    ok = split_compare(<<"Test">>, <<>>),
    ok = split_compare2(<<"Test">>, <<>>),
    ok = split_compare2(<<"helloSEPARATORworld">>, <<"hello">>, <<"world">>),
    ok = fail_split(<<>>),
    ok = fail_split({1, 2}),
    ok = fail_split2({1, 2}),
    case erlang:system_info(machine) of
        "BEAM" -> ok;
        "ATOM" -> ok = memory_allocation_split()
    end,
    0.

split_compare(Bin, Part1) ->
    [A] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A).

split_compare(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<":">>),
    ok = compare_bin(Part1, A),
    ok = compare_bin(B, Part2).

split_compare2(Bin, Part1) ->
    [A] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A).

split_compare2(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<"SEPARATOR">>),
    ok = compare_bin(Part1, A),
    ok = compare_bin(B, Part2).

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    ok;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    B1 = binary:at(Bin2, Index),
    compare_bin(Bin1, Bin2, Index - 1).

fail_split(Separator) ->
    try binary:split(<<"TESTBIN">>, Separator) of
        _Any -> {unexpected, _Any}
    catch
        error:badarg -> ok;
        T:V -> {unexpected, {T, V}}
    end.

fail_split2(Bin) ->
    try binary:split(Bin, <<"TESTSEPARATOR">>) of
        _Any -> {unxpected, _Any}
    catch
        error:badarg -> ok;
        T:V -> {unxpected, {T, V}}
    end.

memory_allocation_split() ->
    Parent = self(),
    Hostname = <<"atomvm">>,
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            % Carefully designed lists to generate a crash on unix 64 bits
            % This binary is 63 bytes long, so it's stored on heap on 64 bits
            % binary:split should allocate sufficient bytes as subbinaries
            % have to be on heap as well
            HeapBin = list_to_binary([
                id(Hostname), <<"@atomvms3.object.stream.atomvms3.object.stream.atomvms3.o">>
            ]),
            List1 = binary:split(HeapBin, <<"@">>, [global]),
            Parent ! {self(), List1}
        end,
        [link, monitor, {atomvm_heap_growth, minimum}]
    ),
    ok =
        receive
            {Pid, List1} ->
                2 = length(List1),
                ok
        after 5000 -> timeout
        end,
    normal =
        receive
            {'DOWN', MonitorRef, process, Pid, Reason} -> Reason
        after 5000 -> timeout
        end,
    ok.

id(X) -> X.
