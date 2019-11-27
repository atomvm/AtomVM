-module(sht31).
-export([start/0, read/1]).

-define (SHT31_BASE_ADDR, 16#44).
-define (SHT31_MEAS_HIGH_REP, 16#2400).

start() ->
    I2C = i2c:open([{scl_io_num, 15}, {sda_io_num, 4}, {i2c_clock_hz, 1000000}]),
    loop(I2C).

loop(I2C) ->
    Val = read(I2C),
    erlang:display(Val),
    avm_timer:sleep(10000),
    loop(I2C).

read(I2C) ->
    Bin = read_sensor(I2C),
    parse_bin(Bin).

parse_bin(B) ->
    IntTemp = (binary:at(B, 0) bsl 8) bor binary:at(B, 1),
    Temp = float_temp(IntTemp, 0.01),
    IntHum = (binary:at(B, 3) bsl 8) bor binary:at(B, 4),
    Hum = float_hum(IntHum, 0.01),
    {ok, Temp, Hum}.

float_temp(IntTemp, S) ->
    (((4375 * IntTemp) bsr 14) - 4500) * S.

float_hum(IntHum, S) ->
    ((625 * IntHum) bsr 12) * S.

read_sensor(I2C) ->
    send_command(I2C, ?SHT31_MEAS_HIGH_REP),
    avm_timer:sleep(20),
    i2c:read_bytes(I2C, ?SHT31_BASE_ADDR, 6).

send_command(I2C, Command) ->
    i2c:begin_transmission(I2C, ?SHT31_BASE_ADDR),
    i2c:write_byte(I2C, (Command bsr 8) band 16#FF),
    i2c:write_byte(I2C, Command band 16#FF),
    i2c:end_transmission(I2C).
