%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

-module(test_calendar).

-export([test/0]).

test() ->
    ok = test_gregorian_days(),
    ok = test_gregorian_seconds(),
    ok.

test_gregorian_days() ->
    0 = calendar:date_to_gregorian_days({0, 1, 1}),
    739089 = calendar:date_to_gregorian_days({2023, 7, 23}),

    expect_exception(fun() -> calendar:date_to_gregorian_days(not_a_date) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({not_a_year, 1, 1}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, not_a_month, 1}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, -1, 1}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, 13, 1}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, 1, not_a_day}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, 1, -1}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({0, 1, 32}) end),
    expect_exception(fun() -> calendar:date_to_gregorian_days({-1, 12, 31}) end),

    ok.

test_gregorian_seconds() ->
    0 = calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 0, 0}}),
    1 = calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 0, 1}}),
    61 = calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 1, 1}}),
    63857338267 = calendar:datetime_to_gregorian_seconds({{2023, 7, 23}, {13, 31, 7}}),

    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds(not_a_datetime) end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, not_a_time}) end),
    expect_exception(fun() ->
        calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {not_an_hour, 0, 0}})
    end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {-1, 0, 0}}) end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {-24, 0, 0}}) end),
    expect_exception(fun() ->
        calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, not_a_minute, 0}})
    end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, -1, 0}}) end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 60, 0}}) end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 0, -1}}) end),
    expect_exception(fun() -> calendar:datetime_to_gregorian_seconds({{0, 1, 1}, {0, 0, 60}}) end),

    ok.

expect_exception(F) ->
    try
        F(),
        fail
    catch
        _:_ ->
            ok
    end.
