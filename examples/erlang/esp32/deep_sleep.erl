-module (deep_sleep).
-export([start/0]).

start() ->
    io:format("Wakeup cause: ~p~n", [esp:sleep_get_wakeup_cause()]),
    SleepMs = 5000,
    io:format("Sleeping for ~pms~n", [SleepMs]),
    esp:deep_sleep(SleepMs).
