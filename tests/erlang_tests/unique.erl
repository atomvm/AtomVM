%
% This file is part of AtomVM.
%
% Copyright 2024 Jakub Gonet <jakub.gonet@swmansion.com>
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

-module(unique).

-export([start/0, id/1]).

start() ->
    ok = unique_0(),
    ok = unique_positive(),
    ok = unique_monotonic(),
    ok = unique_positive_monotonic(),
    ok = unique_monotonic_processes(),
    ok = invalid_args(),

    Self = self(),
    N = length([
        spawn_opt(fun() -> Self ! {ok, unique_0()} end, []),
        spawn_opt(fun() -> Self ! {ok, unique_positive()} end, []),
        spawn_opt(fun() -> Self ! {ok, unique_monotonic()} end, []),
        spawn_opt(fun() -> Self ! {ok, unique_positive_monotonic()} end, [])
    ]),
    receive_messages(N),
    0.

invalid_args() ->
    ok = raises(badarg, fun() -> erlang:unique_integer(?MODULE:id(5)) end),
    ok = raises(badarg, fun() ->
        erlang:unique_integer(?MODULE:id([positive | monotonic]))
    end),
    ok.

raises(Reason, F) ->
    try F() of
        Result -> {unexpected, Result}
    catch
        error:Reason -> ok;
        E:R -> {unexpected, E, R}
    end.

id(X) ->
    X.

unique_0() ->
    A = erlang:unique_integer(),
    B = erlang:unique_integer(),
    C = erlang:unique_integer(),
    Valid = (A =/= B) and (A =/= C),
    true = Valid,
    ok.

unique_positive() ->
    A = erlang:unique_integer([positive]),
    B = erlang:unique_integer([positive]),
    C = erlang:unique_integer([positive]),
    Valid = (A > 0) and (B > 0) and (C > 0) and (A =/= B) and (B =/= C),
    true = Valid,
    ok.

unique_monotonic() ->
    A = erlang:unique_integer([monotonic]),
    B = erlang:unique_integer([monotonic]),
    C = erlang:unique_integer([monotonic]),
    Valid = (A < B) and (B < C),
    true = Valid,
    ok.

unique_positive_monotonic() ->
    A = erlang:unique_integer([positive, monotonic]),
    B = erlang:unique_integer([positive, monotonic]),
    C = erlang:unique_integer([positive, monotonic]),
    Valid = (A > 0) and (A < B) and (B < C),
    true = Valid,
    ok.

unique_monotonic_processes() ->
    Self = self(),
    spawn_opt(
        fun() ->
            n_times(50, fun() -> Self ! {a, erlang:unique_integer([monotonic])} end)
        end,
        []
    ),
    spawn_opt(
        fun() ->
            n_times(100, fun() -> Self ! {b, erlang:unique_integer([monotonic])} end)
        end,
        []
    ),
    Msgs = receive_messages(150),
    NumA = [Num || {a, Num} <- Msgs],
    NumB = [Num || {b, Num} <- Msgs],
    true = increasing(NumA),
    true = increasing(NumB),
    ok.

receive_messages(N) ->
    receive_messages(N, []).
receive_messages(0, Msgs) ->
    reverse(Msgs);
receive_messages(N, Msgs) ->
    receive
        Msg -> receive_messages(N - 1, [Msg | Msgs])
    end.

reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

n_times(0, _F) ->
    ok;
n_times(N, F) ->
    F(),
    n_times(N - 1, F).

increasing([_]) -> true;
increasing([A, B | T]) when A < B -> increasing([B | T]);
increasing(_) -> false.
