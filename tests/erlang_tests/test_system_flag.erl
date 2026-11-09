%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(test_system_flag).

-export([start/0, depth_a/1, depth_b/1]).

start() ->
    ok = test_schedulers_online(),
    ok = test_backtrace_depth(),
    0.

test_backtrace_depth() ->
    Previous = erlang:system_flag(backtrace_depth, 8),
    try
        %% Builds without stacktraces return undefined even for supplied traces.
        HasStacktraces = is_list(supplied_trace(error, [])),
        [
            begin
                8 = erlang:system_flag(backtrace_depth, Depth),
                Limit = max(1, min(64, Depth)),
                [
                    begin
                        Stack = frames(Count, modern),
                        Expected =
                            case HasStacktraces of
                                true -> prefix(Stack, Limit);
                                false -> undefined
                            end,
                        Expected = supplied_trace(Class, Stack),
                        Legacy = frames(Count, legacy),
                        Expected = supplied_trace(Class, Legacy)
                    end
                 || Count <- [0, 1, 3, 7, 8, 9, 65], Class <- [error, exit, throw]
                ],
                ExpectedLength =
                    case HasStacktraces of
                        true -> Limit;
                        false -> undefined
                    end,
                ExpectedLength = generated_trace(),
                Stored = min(64, Depth),
                Stored = erlang:system_flag(backtrace_depth, 8)
            end
         || Depth <- [0, 1, 3, 8, 64, 65, 1 bsl 26]
        ],
        [
            ok =
                try erlang:system_flag(backtrace_depth, Bad) of
                    _ -> unexpected
                catch
                    error:badarg -> ok
                end
         || Bad <- [-1, 1.0, bad, [], 1 bsl 100]
        ],
        %% Even discarded frames must be validated, including improper tails.
        8 = erlang:system_flag(backtrace_depth, 1),
        [
            badarg = apply(erlang, raise, [error, supplied, Stack])
         || Stack <- [
                [{?MODULE, supplied, 1, []}, bad],
                [{?MODULE, supplied, 1, []} | bad]
            ]
        ],
        1 = erlang:system_flag(backtrace_depth, 8),
        ok
    after
        erlang:system_flag(backtrace_depth, Previous)
    end.

frames(0, _) -> [];
frames(N, modern) -> [{?MODULE, supplied, N, []} | frames(N - 1, modern)];
frames(N, legacy) -> [{?MODULE, supplied, N} | frames(N - 1, legacy)].

prefix(_, 0) -> [];
prefix([], _) -> [];
prefix([Frame | Rest], N) -> [Frame | prefix(Rest, N - 1)].

supplied_trace(Class, Stack) ->
    try
        apply(erlang, raise, [Class, supplied, Stack])
    catch
        Class:supplied:Trace -> Trace
    end.

generated_trace() ->
    try
        depth_a(80)
    catch
        error:depth_error:Trace ->
            case Trace of
                undefined -> undefined;
                _ -> length(Trace)
            end
    end.

depth_a(0) -> error(depth_error);
depth_a(N) -> ?MODULE:depth_b(N - 1) + 1.
depth_b(N) -> ?MODULE:depth_a(N - 1) + 2.

test_schedulers_online() ->
    case erlang:system_info(schedulers) of
        % probably not an SMP build
        1 ->
            ok;
        Schedulers when is_integer(Schedulers) ->
            Online = erlang:system_info(schedulers_online),
            Online = erlang:system_flag(schedulers_online, 1),
            true = Online =< Schedulers,
            1 = erlang:system_info(schedulers_online),
            1 = erlang:system_flag(schedulers_online, 2),
            2 = erlang:system_info(schedulers_online),
            Overflow =
                if
                    Online < Schedulers -> Schedulers;
                    true -> Online
                end + 1,
            ok =
                try
                    erlang:system_flag(schedulers_online, Overflow),
                    expected_error
                catch
                    error:badarg -> ok
                end,
            2 = erlang:system_flag(schedulers_online, Online),
            Online = erlang:system_info(schedulers_online),
            ok =
                try
                    erlang:system_flag(schedulers_online, 0),
                    expected_error
                catch
                    error:badarg -> ok
                end,
            Online = erlang:system_info(schedulers_online),
            ok
    end.
