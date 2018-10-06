-module(datetime).
-export([start/0]).

start() ->
    {Date, Time} = erlang:universaltime(),
    test_date(Date) + test_time(Time).

test_date({Y, M, D}) when Y >= 2018 andalso M >= 1 andalso M =< 12 andalso D >= 1 andalso D =< 31 ->
    1.

test_time({HOUR, MIN, SEC}) when HOUR >= 0 andalso HOUR < 24 andalso MIN >= 0 andalso MIN < 60 andalso SEC >= 0 andalso MIN < 60 ->
    2.
