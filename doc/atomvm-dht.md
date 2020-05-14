# DHT API

The DHT API can be used to drive common DHT11 and DHT22 temperature and humidity sensors.

DHT22 devices are reported to have higher resolution and range in both temperature and humidity readings.

Temperature and humity readings can be taken at intervals not less that 1 second apart for DHT11 devices, and 2 seonds apart for DHT22 devices.  The DHT API will ensure that any one DHT instance will not read at less than the
recommended interval.

### Sample Code

The following code illustrates use of the DHT API:

    %% start a DHT11 reader on Pin 22
    {ok, DHT} = dht:start(22),

    %% take a measurement and print the results
    case dht:measure(DHT) of
        {ok, Measurement} ->
            {Temp, TempFractional, Hum, HumFractional} = Measurement,
            io:format("Temperature: ~p.~pC  Humidity: ~p.~p%~n", [Temp, TempFractional, Hum, HumFractional]);
        Error ->
            io:format("Error taking measurement: ~p~n", [Error])
    end,
    ...

### Example Program

The 'dht_example` program illustrates use of the DHT API by taking a temperature and humidity reading every 30 seconds, and displaying the result on the console.

To run this example program, connect the positive (+) lead on the DHT device to +3.3v power on the ESP32, the negative lead (-) to a ground pin on the ESP32, and the data pin to GPIO pin 22 on the ESP32 device.

                    +------------+
                    |            |
                    |    DHT11   |
                    |      or    |
                    |    DHT22   |
                    |            |
                    |            |
                    | +  d     - |
                    +-+--+--+--+-+
                      |  |  |  |
                      |  |  |  |
                      |  |  |  |

Build the example program and upload

    shell$ make && .../tools/dev/flash.sh examples/erlang/esp32/dht_example.avm

Attach to the

    shell$ make monitor
    Toolchain path: /work/opt/xtensa-esp32-elf/bin/xtensa-esp32-elf-gcc
    WARNING: Toolchain version is not supported: crosstool-ng-1.22.0-95-ge082013a
    ...
    Found AVM partition: size: 1048576, address: 0x110000
    Booting file mapped at: 0x3f430000, size: 1048576
    Starting: dht_example.beam...
    ---
    Temperature: 21.3C  Humidity: 41.6%
    Temperature: 21.4C  Humidity: 41.4%
    Temperature: 21.3C  Humidity: 41.3%
    ...
