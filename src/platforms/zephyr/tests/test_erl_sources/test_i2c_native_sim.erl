%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_i2c_native_sim).

-export([start/0]).

-define(I2C_BUS, "i2c0").
-define(TEST_ADDRESS, 16#50).
-define(MISSING_ADDRESS, 16#51).

start() ->
    I2C = i2c:open([
        {peripheral, ?I2C_BUS},
        {clock_speed_hz, 100000},
        {send_timeout_ms, 100}
    ]),
    try
        run(I2C)
    after
        ok = i2c:close(I2C)
    end.

run(I2C) ->
    ok = i2c:write_bytes(I2C, ?TEST_ADDRESS, 16#10, <<16#CA, 16#FE>>),
    {ok, <<16#CA, 16#FE>>} = i2c:read_bytes(I2C, ?TEST_ADDRESS, 16#10, 2),

    ok = i2c:begin_transmission(I2C, ?TEST_ADDRESS),
    ok = i2c:write_byte(I2C, 16#10),
    ok = i2c:end_transmission(I2C),
    {ok, <<16#CA, 16#FE>>} = i2c:read_bytes(I2C, ?TEST_ADDRESS, 2),

    ok = i2c:write_bytes(I2C, ?TEST_ADDRESS, 16#12, 16#42),
    {ok, <<16#42>>} = i2c:read_bytes(I2C, ?TEST_ADDRESS, 16#12, 1),

    {error, eio} = i2c:read_bytes(I2C, ?MISSING_ADDRESS, 1),
    ok.
