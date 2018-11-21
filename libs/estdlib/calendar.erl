-module(calendar).

-export([day_of_the_week/1, day_of_the_week/3]).

day_of_the_week({Y, M, D}) ->
    day_of_the_week(Y, M, D).

day_of_the_week(Y, M, D) ->
    (unix_days(Y, M, D) + 4) rem 7.

unix_days(Year, M, D) when Year >= 0 ->
    Y =
        if
            M =< 2 -> Year - 1;
            true -> Year
        end,
    Era = Y div 400,
    YoE = Y - Era * 400,
    MO =
        if
            M > 2 -> -3;
            true -> 9
        end,
    DoY = (153 * (M + MO) + 2) div 5 + D - 1,
    DoE = YoE * 365 + YoE div 4 - YoE div 100 + DoY,
    Era * 146097 + DoE - 719468.
