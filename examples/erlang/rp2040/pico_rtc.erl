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

% This example demonstrates the usage of the Pico's hardware RTC.

-module(pico_rtc).
-export([start/0]).

-define(START_DATE, {{2022, 11, 16}, {22, 09, 00}}).

start() ->
    case atomvm:platform() of
        pico ->
            pico_rtc_loop();
        Other ->
            erlang:display({unexpected, Other})
    end.

pico_rtc_loop() ->
    Date = erlang:universaltime(),
    print_date(Date),
    receive
        ok -> ok
    after 5000 -> ok
    end,
    if
        Date < ?START_DATE ->
            pico:rtc_set_datetime(?START_DATE);
        true ->
            ok
    end,
    pico_rtc_loop().

print_date({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    % Only ~p and ~s seem to exist for now.
    YearStr = integer_to_list(Year),
    MonthStr = format_2_digits(Month),
    DayStr = format_2_digits(Day),
    HourStr = format_2_digits(Hour),
    MinStr = format_2_digits(Min),
    SecStr = format_2_digits(Sec),
    console:puts(
        lists:flatten([
            YearStr, "-", MonthStr, "-", DayStr, "T", HourStr, ":", MinStr, ":", SecStr, "\n"
        ])
    ).

format_2_digits(Num) when Num < 10 ->
    ["0", integer_to_list(Num)];
format_2_digits(Num) ->
    integer_to_list(Num).
