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

-module(test_binary_copy).

-export([start/0]).

start() ->
    ok = test_copy1(),
    ok = test_copy2(),
    ok = test_counts(),
    0.

test_copy1() ->
    <<>> = binary:copy(<<>>),
    <<"foo">> = binary:copy(<<"foo">>),
    ok.

test_copy2() ->
    <<>> = binary:copy(<<>>, 1),
    <<"foo">> = binary:copy(<<"foo">>, 1),
    <<>> = binary:copy(<<"foo">>, 0),
    <<"foofoo">> = binary:copy(<<"foo">>, 2),
    ok.

test_counts() ->
    WordBits = erlang:system_info(wordsize) * 8,
    Max = (1 bsl WordBits) - 1,
    foreach(
        fun(Count) -> <<>> = binary:copy(<<>>, Count) end,
        [1 bsl (WordBits - 4), 1 bsl (WordBits - 1), Max]
    ),
    foreach(
        fun(Count) ->
            foreach(
                fun(Bin) -> expect_error(badarg, fun() -> binary:copy(Bin, Count) end) end,
                [<<>>, <<0, 255, 128>>]
            )
        end,
        [-1, -(1 bsl 100), bad, [], 1.0, 1 bsl WordBits]
    ),
    foreach(
        fun(Bin) ->
            Limit = Max div (byte_size(Bin) * 8),
            expect_error(system_limit, fun() -> binary:copy(Bin, Limit) end),
            expect_error(system_limit, fun() -> binary:copy(Bin, Max) end)
        end,
        [<<1>>, <<0, 255, 128, 17, 3, 4, 5, 6>>]
    ),
    expect_error(badarg, fun() -> binary:copy(<<5:3>>, 0) end),
    <<0, 255, 128, 0, 255, 128, 0, 255, 128>> = binary:copy(<<0, 255, 128>>, 3),
    ok.

foreach(_Fun, []) ->
    ok;
foreach(Fun, [H | T]) ->
    Fun(H),
    foreach(Fun, T).

expect_error(Expected, Fun) ->
    Result =
        try Fun() of
            Value -> {returned, Value}
        catch
            error:Reason -> Reason
        end,
    Expected = Result,
    ok.
