-module(avm_calendar).

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
