%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_spi).
-export([start/0]).

start() ->
    {ok, {_Baudrate, SPI}} = spi:init(1, #{baudrate => 1000000}),

    %% Smoke test of the SPI path against the renode Micron_MT25Q flash. We
    %% don't validate the response bytes: renode's STM32SPI / STM32H7_SPI
    %% emulations don't faithfully model the JEDEC ID timing (one shifts the
    %% response by a byte, the other deasserts NSS between calls), so we just
    %% verify the peripheral round-trips a response of the expected size.
    1 = spi:transmit(SPI, <<16#9F>>, 1000),
    ChipInfo = erlang:system_info(stm32_chip_info),
    IsH7 =
        case ChipInfo of
            #{series := stm32h7} -> true;
            _ -> false
        end,
    erlang:display(ChipInfo),
    case IsH7 of
        true ->
            % Workaround for renode limitations in H7 spi emulation
            {ok, RxData} = spi:transmit_receive(SPI, <<0, 0, 0>>, 1000),
            3 = byte_size(RxData);
        false ->
            {ok, RxData} = spi:receive_(SPI, 3, 1000),
            3 = byte_size(RxData)
    end,
    {ok, TxRxData} = spi:transmit_receive(SPI, <<16#9F, 0, 0, 0>>, 1000),
    4 = byte_size(TxRxData),
    ready = spi:get_state(SPI),
    0 = spi:get_error(SPI),
    ok = spi:abort(SPI),

    %% --- per-device reconfiguration coverage ---
    %% Same config in a row is a no-op (cache hit on prescaler + CPOL/CPHA);
    %% changing either field drives HAL_SPI_Init. We can't read CFG1 from the
    %% harness, so for each call we assert the observable invariants: returns
    %% ok, peripheral stays in the ready state with no error, and a follow-up
    %% transmit still succeeds.
    ok = check_reconfig(SPI, #{}),
    ok = check_reconfig(SPI, #{clock_speed_hz => 4000000, mode => 0}),
    ok = check_reconfig(SPI, #{clock_speed_hz => 4000000, mode => 0}),
    ok = check_reconfig(SPI, #{clock_speed_hz => 1000000, mode => 3}),
    ok = check_reconfig(SPI, #{mode => 0}),
    ok = check_reconfig(SPI, #{clock_speed_hz => 1000000}),

    %% Invalid arguments must badarg without disturbing the peripheral.
    ok = expect_badarg(SPI, #{mode => 4}),
    ok = expect_badarg(SPI, #{mode => -1}),
    ok = expect_badarg(SPI, #{mode => not_an_integer}),
    ok = expect_badarg(SPI, #{clock_speed_hz => 0}),
    ok = expect_badarg(SPI, #{clock_speed_hz => -1}),
    ok = expect_badarg(SPI, #{clock_speed_hz => not_an_integer}),
    ok = expect_badarg(SPI, not_a_map),
    ready = spi:get_state(SPI),
    0 = spi:get_error(SPI),
    1 = spi:transmit(SPI, <<16#9F>>, 1000),

    ok = spi:deinit(SPI),
    %% After deinit the resource is invalid; apply_device_config must badarg.
    ok = expect_badarg(SPI, #{mode => 0}),
    ok = spi:deinit(SPI),
    erlang:display(spi_done).

%% @private
check_reconfig(SPI, Config) ->
    ok = spi:apply_device_config(SPI, Config),
    ready = spi:get_state(SPI),
    0 = spi:get_error(SPI),
    1 = spi:transmit(SPI, <<16#9F>>, 1000),
    ok = spi:abort(SPI),
    ok.

%% @private
expect_badarg(SPI, Config) ->
    try spi:apply_device_config(SPI, Config) of
        Result -> error({expected_badarg_got, Config, Result})
    catch
        error:badarg -> ok
    end.
