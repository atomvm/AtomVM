%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_i2c).

-export([start/0]).

-define(BMP180_ADDRESS, 16#77).
-define(NACK_ADDRESS, 16#76).
-define(BMP180_CHIP_ID_REGISTER, 16#D0).
-define(BMP180_CHIP_ID, 16#55).
-define(BMP180_CALIBRATION_REGISTER, 16#AA).
-define(BMP180_CALIBRATION_SIZE, 22).
-define(BMP180_CONTROL_REGISTER, 16#F4).
-define(BMP180_DATA_REGISTER, 16#F6).
-define(BMP180_TEMPERATURE_COMMAND, 16#2E).
-define(BMP180_PRESSURE_COMMAND_OSS0, 16#34).

start() ->
    ok = run_case(false),
    ok = run_case(true),
    ok.

run_case(UseNif) ->
    I2C = i2c:open(config(UseNif)),
    try
        ok = test_einprogress_errors(I2C),
        ok = test_nack_errors(I2C),
        ok = test_chip_id(I2C),
        Calibration = test_calibration(I2C),
        SplitWriteTemp = test_split_transaction_temperature_read(I2C),
        DirectWriteTemp = test_direct_write_temperature_read(I2C),
        true = erlang:abs(SplitWriteTemp - DirectWriteTemp) =< 2,
        Pressure = test_register_write_pressure_read(I2C),
        ok = test_compensated_values(Calibration, DirectWriteTemp, Pressure)
    after
        ok = i2c:close(I2C)
    end.

config(UseNif) ->
    CommonConfig = [
        {sda, 21},
        {scl, 22},
        {clock_speed_hz, 100000},
        {peripheral, "i2c0"}
    ],
    case UseNif of
        true ->
            [{use_nif, true}, {send_timeout_ms, 1000} | CommonConfig];
        false ->
            CommonConfig
    end.

test_einprogress_errors(I2C) ->
    {OwnerPid, OwnerRef} = start_transaction_holder(I2C),
    try
        {error, {einprogress, OwnerPid}} = i2c:begin_transmission(I2C, ?BMP180_ADDRESS),
        {error, {einprogress, OwnerPid}} = i2c:write_byte(I2C, 16#00),
        {error, {einprogress, OwnerPid}} = i2c:write_bytes(I2C, <<16#00>>),
        {error, {einprogress, OwnerPid}} = i2c:end_transmission(I2C),
        {error, {einprogress, OwnerPid}} = i2c:read_bytes(I2C, ?BMP180_ADDRESS, 1),
        {error, {einprogress, OwnerPid}} = i2c:read_bytes(
            I2C, ?BMP180_ADDRESS, ?BMP180_CHIP_ID_REGISTER, 1
        ),
        {error, {einprogress, OwnerPid}} = i2c:write_bytes(I2C, ?BMP180_ADDRESS, <<16#00>>),
        {error, {einprogress, OwnerPid}} =
            i2c:write_bytes(
                I2C, ?BMP180_ADDRESS, ?BMP180_CONTROL_REGISTER, ?BMP180_TEMPERATURE_COMMAND
            ),
        ok
    after
        ok = stop_transaction_holder(OwnerPid, OwnerRef)
    end.

start_transaction_holder(I2C) ->
    Parent = self(),
    {Pid, Ref} = erlang:spawn_monitor(fun() -> transaction_holder(Parent, I2C) end),
    receive
        {Pid, ready} ->
            {Pid, Ref};
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error({transaction_holder_failed, Reason})
    after 5000 ->
        erlang:error(transaction_holder_start_timeout)
    end.

stop_transaction_holder(Pid, Ref) ->
    Pid ! {self(), finish},
    receive
        {Pid, finished} ->
            receive
                {'DOWN', Ref, process, Pid, normal} ->
                    ok
            after 5000 ->
                erlang:error(transaction_holder_stop_timeout)
            end;
        {'DOWN', Ref, process, Pid, normal} ->
            receive
                {Pid, finished} ->
                    ok
            after 0 ->
                ok
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            receive
                {Pid, finished} ->
                    ok
            after 0 ->
                ok
            end,
            erlang:error({transaction_holder_failed, Reason})
    after 5000 ->
        erlang:error(transaction_holder_stop_timeout)
    end.

transaction_holder(Parent, I2C) ->
    ok = i2c:begin_transmission(I2C, ?BMP180_ADDRESS),
    ok = i2c:write_byte(I2C, ?BMP180_CHIP_ID_REGISTER),
    Parent ! {self(), ready},
    receive
        {Parent, finish} ->
            ok = i2c:end_transmission(I2C),
            Parent ! {self(), finished}
    after 5000 ->
        erlang:error(transaction_holder_finish_timeout)
    end.

test_nack_errors(I2C) ->
    % 0x76 is adjacent to the BMP180's 0x77 address and is unused in the
    % current simulator diagram, so transactions there should NACK.
    {error, esp_fail} = i2c:read_bytes(I2C, ?NACK_ADDRESS, 1),
    ok = assert_chip_id_via_register_read(I2C),
    {error, esp_fail} = i2c:write_bytes(I2C, ?NACK_ADDRESS, <<16#00>>),
    ok = assert_chip_id_via_register_read(I2C),
    ok = i2c:begin_transmission(I2C, ?NACK_ADDRESS),
    ok = i2c:write_byte(I2C, 16#00),
    {error, esp_fail} = i2c:end_transmission(I2C),
    ok = assert_chip_id_via_pointer_read(I2C),
    ok.

test_chip_id(I2C) ->
    ok = assert_chip_id_via_register_read(I2C),
    ok = assert_chip_id_via_pointer_read(I2C),
    ok.

assert_chip_id_via_register_read(I2C) ->
    {ok, <<?BMP180_CHIP_ID>>} = i2c:read_bytes(I2C, ?BMP180_ADDRESS, ?BMP180_CHIP_ID_REGISTER, 1),
    ok.

assert_chip_id_via_pointer_read(I2C) ->
    ok = set_register_pointer(I2C, ?BMP180_CHIP_ID_REGISTER),
    {ok, <<?BMP180_CHIP_ID>>} = i2c:read_bytes(I2C, ?BMP180_ADDRESS, 1),
    ok.

test_calibration(I2C) ->
    {ok, Calibration} =
        i2c:read_bytes(
            I2C, ?BMP180_ADDRESS, ?BMP180_CALIBRATION_REGISTER, ?BMP180_CALIBRATION_SIZE
        ),
    true = byte_size(Calibration) =:= ?BMP180_CALIBRATION_SIZE,
    true = Calibration =/= <<0:(?BMP180_CALIBRATION_SIZE * 8)>>,
    decode_calibration(Calibration).

test_split_transaction_temperature_read(I2C) ->
    ok = i2c:begin_transmission(I2C, ?BMP180_ADDRESS),
    ok = i2c:write_byte(I2C, ?BMP180_CONTROL_REGISTER),
    ok = i2c:write_bytes(I2C, <<?BMP180_TEMPERATURE_COMMAND>>),
    ok = i2c:end_transmission(I2C),
    timer:sleep(10),
    {ok, <<Temp:16/big-unsigned-integer>>} = i2c:read_bytes(
        I2C, ?BMP180_ADDRESS, ?BMP180_DATA_REGISTER, 2
    ),
    true = Temp > 0,
    Temp.

test_direct_write_temperature_read(I2C) ->
    ok = i2c:write_bytes(
        I2C, ?BMP180_ADDRESS, <<?BMP180_CONTROL_REGISTER, ?BMP180_TEMPERATURE_COMMAND>>
    ),
    timer:sleep(10),
    ok = set_register_pointer(I2C, ?BMP180_DATA_REGISTER),
    {ok, <<Temp:16/big-unsigned-integer>>} = i2c:read_bytes(I2C, ?BMP180_ADDRESS, 2),
    true = Temp > 0,
    Temp.

test_register_write_pressure_read(I2C) ->
    ok = i2c:write_bytes(
        I2C, ?BMP180_ADDRESS, ?BMP180_CONTROL_REGISTER, ?BMP180_PRESSURE_COMMAND_OSS0
    ),
    timer:sleep(10),
    {ok, <<Msb, Lsb, Xlsb>>} = i2c:read_bytes(I2C, ?BMP180_ADDRESS, ?BMP180_DATA_REGISTER, 3),
    Pressure = ((Msb bsl 16) bor (Lsb bsl 8) bor Xlsb) bsr 8,
    true = Pressure > 0,
    Pressure.

set_register_pointer(I2C, Register) ->
    ok = i2c:begin_transmission(I2C, ?BMP180_ADDRESS),
    ok = i2c:write_byte(I2C, Register),
    ok = i2c:end_transmission(I2C).

decode_calibration(
    <<
        AC1:16/signed-big,
        AC2:16/signed-big,
        AC3:16/signed-big,
        AC4:16/unsigned-big,
        AC5:16/unsigned-big,
        AC6:16/unsigned-big,
        B1:16/signed-big,
        B2:16/signed-big,
        MB:16/signed-big,
        MC:16/signed-big,
        MD:16/signed-big
    >>
) ->
    #{
        ac1 => AC1,
        ac2 => AC2,
        ac3 => AC3,
        ac4 => AC4,
        ac5 => AC5,
        ac6 => AC6,
        b1 => B1,
        b2 => B2,
        mb => MB,
        mc => MC,
        md => MD
    }.

test_compensated_values(Calibration, TemperatureRaw, PressureRaw) ->
    Temperature = compensate_temperature(Calibration, TemperatureRaw),
    Pressure = compensate_pressure(Calibration, TemperatureRaw, PressureRaw),
    true = Temperature >= 200,
    true = Temperature =< 280,
    true = Pressure >= 100000,
    true = Pressure =< 103000,
    ok.

compensate_temperature(#{ac5 := AC5, ac6 := AC6, mc := MC, md := MD}, TemperatureRaw) ->
    X1 = ((TemperatureRaw - AC6) * AC5) bsr 15,
    X2 = (MC bsl 11) div (X1 + MD),
    B5 = X1 + X2,
    (B5 + 8) bsr 4.

compensate_pressure(Calibration, TemperatureRaw, PressureRaw) ->
    #{
        ac1 := AC1,
        ac2 := AC2,
        ac3 := AC3,
        ac4 := AC4,
        ac5 := AC5,
        ac6 := AC6,
        b1 := B1,
        b2 := B2,
        mc := MC,
        md := MD
    } = Calibration,
    Oss = 0,

    X1 = ((TemperatureRaw - AC6) * AC5) bsr 15,
    X2 = (MC bsl 11) div (X1 + MD),
    B5 = X1 + X2,
    B6 = B5 - 4000,

    X1_1 = (B2 * ((B6 * B6) bsr 12)) bsr 11,
    X2_1 = (AC2 * B6) bsr 11,
    X3_1 = X1_1 + X2_1,
    B3 = ((((AC1 * 4) + X3_1) bsl Oss) + 2) div 4,

    X1_2 = (AC3 * B6) bsr 13,
    X2_2 = (B1 * ((B6 * B6) bsr 12)) bsr 16,
    X3_2 = (X1_2 + X2_2 + 2) bsr 2,
    B4 = (AC4 * (X3_2 + 32768)) bsr 15,
    B7 = (PressureRaw - B3) * (50000 bsr Oss),

    Pressure0 =
        case B7 < 16#80000000 of
            true ->
                (B7 * 2) div B4;
            false ->
                (B7 div B4) * 2
        end,

    X1_3 = ((Pressure0 bsr 8) * (Pressure0 bsr 8) * 3038) bsr 16,
    X2_3 = (-7357 * Pressure0) bsr 16,
    Pressure0 + ((X1_3 + X2_3 + 3791) bsr 4).
