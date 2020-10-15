-module(sleep).

-export([start/0, sleep_1/1, sleep_2/1, sleep_3/1, sleep_4/1]).

start() ->
    spawn(sleep, sleep_3, [self()]),
    spawn(sleep, sleep_1, [self()]),
    spawn(sleep, sleep_2, [self()]),
    sleep(400),
    FirstValue =
        receive
            Value1 ->
                Value1 * 8
        end,
    SecondValue =
        receive
            Value2 ->
                Value2 * 16
        end,
    ThirdValue =
        receive
            Value3 ->
                Value3 * 32
        end,
    FourthValue =
        receive
            Value4 ->
                Value4 * 64
        end,
    FirstValue + SecondValue + ThirdValue + FourthValue.

sleep_1(ParentPid) ->
    sleep(100),
    spawn(sleep, sleep_4, [ParentPid]),
    ParentPid ! 1.

sleep_2(ParentPid) ->
    sleep(200),
    ParentPid ! 2.

sleep_3(ParentPid) ->
    sleep(300),
    ParentPid ! 3.

sleep_4(ParentPid) ->
    sleep(250),
    ParentPid ! 4.

sleep(T) ->
    receive
    after T -> ok
    end.
