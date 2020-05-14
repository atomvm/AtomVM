-module (dht_example).

-export([start/0]).

start() ->
    Pin = 21,
    {ok, DHT} = dht:start(Pin, dht11),
    loop(DHT).

loop(DHT) ->
    case dht:measure(DHT) of
        {ok, Measurement} ->
            {Temp, TempFractional, Hum, HumFractional} = Measurement,
            io:format("Temperature: ~p.~pC  Humidity: ~p.~p%~n", [Temp, TempFractional, Hum, HumFractional]);
        Error ->
            io:format("Error taking measurement: ~p~n", [Error])
    end,
    timer:sleep(30000),
    loop(DHT).
