%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
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

%%-----------------------------------------------------------------------------
%% @doc A partial implementation of the Erlang/OTP calendar functions.
%%
%% This module provides an implementation of a subset of the functionality of
%% the Erlang/OTP calendar functions.
%%
%% All dates conform to the Gregorian calendar. This calendar was introduced by
%% Pope Gregory XIII in 1582 and was used in all Catholic countries from this year.
%% Protestant parts of Germany and the Netherlands adopted it in 1698, England followed
%% in 1752, and Russia in 1918 (the October revolution of 1917 took place in November
%% according to the Gregorian calendar).
%%
%% The Gregorian calendar in this module is extended back to year 0. For a given date,
%% the gregorian day is the number of days up to and including the date specified.
%% @end
%%-----------------------------------------------------------------------------
-module(calendar).

-export([
    date_to_gregorian_days/1,
    date_to_gregorian_days/3,
    datetime_to_gregorian_seconds/1,
    day_of_the_week/1,
    day_of_the_week/3,
    system_time_to_universal_time/2
]).

-export_type([datetime/0]).

%%-----------------------------------------------------------------------------
%% @doc Year cannot be abbreviated.
%%
%% For example, 93 denotes year 93, not 1993. The valid range depends on the
%% underlying operating system. The date tuple must denote a valid date.
%%
%% @end
%%-----------------------------------------------------------------------------

-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..59.
-type gregorian_days() :: integer().
-type day_of_week() :: 1..7.
-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second()}.
-type datetime() :: {date(), time()}.

%%-----------------------------------------------------------------------------
%% @equiv date_to_gregorian_days(Year, M, D)
%% @param   Date    the date to get the gregorian day count of
%% @returns Days    number of days
%% @end
%%-----------------------------------------------------------------------------
-spec date_to_gregorian_days(Date :: date()) -> Days :: gregorian_days().
date_to_gregorian_days({Y, M, D}) ->
    date_to_gregorian_days(Y, M, D).

%%-----------------------------------------------------------------------------
%% @param   Year    ending year
%% @param   M       ending month
%% @param   D       ending day
%% @returns Days    number of days
%% @doc     Computes the number of gregorian days starting with year 0 and ending at the specified date.
%% @end
%%-----------------------------------------------------------------------------
-spec date_to_gregorian_days(Year :: year(), M :: month(), D :: day()) -> gregorian_days().
date_to_gregorian_days(Year, M, D) when
    is_integer(Year) andalso Year >= 0 andalso
        is_integer(M) andalso M > 0 andalso M =< 12 andalso
        is_integer(D) andalso D > 0 andalso D =< 31
->
    Y =
        if
            M =< 2 -> Year - 1;
            true -> Year
        end,
    Era =
        if
            Y >= 0 -> Y div 400;
            true -> Y - 399 div 400
        end,
    YoE = Y - Era * 400,
    MO =
        if
            M > 2 -> -3;
            true -> 9
        end,
    DoY = (153 * (M + MO) + 2) div 5 + D - 1,
    DoE = YoE * 365 + YoE div 4 - YoE div 100 + DoY,
    Era * 146097 + DoE + 60.

%%-----------------------------------------------------------------------------
%% @param   DateTime the date and time to convert to seconds
%% @returns Seconds number of seconds
%% @doc     Computes the number of gregorian seconds starting with year 0 and ending
%% at the specified date and time.
%% @end
%%-----------------------------------------------------------------------------
-spec datetime_to_gregorian_seconds(DateTime :: datetime()) -> integer().
datetime_to_gregorian_seconds({Date, {Hour, Minute, Second}}) when
    is_integer(Hour) andalso Hour >= 0 andalso Hour =< 23 andalso
        is_integer(Minute) andalso Minute >= 0 andalso Minute =< 59 andalso
        is_integer(Second) andalso Second >= 0 andalso Second =< 59
->
    DateSeconds = date_to_gregorian_days(Date) * 24 * 60 * 60,
    DateSeconds + Hour * 60 * 60 + Minute * 60 + Second.

%%-----------------------------------------------------------------------------
%% @equiv day_of_the_week(Y, M, D)
%% @param   Date the date for which to retrieve the weekday
%% @returns Weekday day of the week
%% @doc     Computes the day of the week from the specified date tuple {Year, Month, Day}.
%%          Returns the day of the week as 1: Monday, 2: Tuesday, and so on.
%% @end
%%-----------------------------------------------------------------------------
%%          Returns the day of the week as 1: Monday, 2: Tuesday, and so on.
%% @end
%%-----------------------------------------------------------------------------
-spec day_of_the_week(Date :: date()) -> day_of_week().
day_of_the_week({Y, M, D}) ->
    day_of_the_week(Y, M, D).

%%-----------------------------------------------------------------------------
%% @param   Y year of the desired day
%% @param   M month of the desired day
%% @param   D year of the desired day
%% @returns Weekday day of the week
%% @doc     Computes the day of the week from the specified Year, Month, and Day.
%%          Returns the day of the week as 1: Monday, 2: Tuesday, and so on.
%% @end
%%-----------------------------------------------------------------------------
-spec day_of_the_week(Y :: year(), M :: month(), D :: day()) -> day_of_week().
day_of_the_week(Y, M, D) ->
    (date_to_gregorian_days(Y, M, D) + 5) rem 7 + 1.

%%-----------------------------------------------------------------------------
%% @param   Time the time, as an integer, in the specified unit
%% @param   TimeUnit the time unit
%% @returns DateTime The date and time (in UTC) converted from the specified time and time unit
%% @doc     Convert an integer time value to a date and time in UTC.
%% @end
%%-----------------------------------------------------------------------------
-spec system_time_to_universal_time(Time :: integer(), TimeUnit :: erlang:time_unit()) ->
    datetime().
system_time_to_universal_time(_Time, _TimeUnit) ->
    erlang:nif_error(undefined).
