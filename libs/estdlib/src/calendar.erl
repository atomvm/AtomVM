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

-module(calendar).

-export([date_to_gregorian_days/1, date_to_gregorian_days/3, day_of_the_week/1, day_of_the_week/3]).

-type year() :: integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type gregorian_days() :: integer().
-type day_of_week() :: 1..7.

-spec date_to_gregorian_days({year(), month(), day()}) -> gregorian_days().
date_to_gregorian_days({Y, M, D}) ->
    date_to_gregorian_days(Y, M, D).

-spec date_to_gregorian_days(year(), month(), day()) -> gregorian_days().
date_to_gregorian_days(Year, M, D) when M =< 12 andalso D =< 31 ->
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

-spec day_of_the_week({year(), month(), day()}) -> day_of_week().
day_of_the_week({Y, M, D}) ->
    day_of_the_week(Y, M, D).

-spec day_of_the_week(year(), month(), day()) -> day_of_week().
day_of_the_week(Y, M, D) ->
    (date_to_gregorian_days(Y, M, D) + 5) rem 7 + 1.
