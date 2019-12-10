-module(esp_random).
-export([start/0]).

-include("estdlib.hrl").

start() ->
    ?IO:format("Reset reason: ~p~n", [esp:reset_reason()]),
    loop().

loop() ->
    RandomLen = abs(atomvm:random()) rem 128,
    RandomBytes = atomvm:rand_bytes(RandomLen),
    ?IO:format("Random bytes: ~p~n", [RandomBytes]),
    ?TIMER:sleep(5000),
    case RandomLen of
        0 ->
            esp:restart();
        _ ->
            loop()
    end.
