%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(spi).
-export([open/1, read_at/3, write_at/4]).

-type bus_config() :: [
    {miso_io_num, non_neg_integer()} |
    {mosi_io_num, non_neg_integer()} |
    {sclk_io_num, non_neg_integer()}
].
-type device_config() :: [
    {clock_speed_hz, non_neg_integer()} |
    {mode, non_neg_integer()} |
    {spi_cs_io_num, non_neg_integer()} |
    {address_len_bits, non_neg_integer()}
].
-type param() :: {bus_config, bus_config()} | {device_config, device_config()}.
-type params() :: [param()].
-type spi() :: pid().
-type address() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns process id of the driver.
%% @doc     Open a connection to the SPI driver
%%
%%          This function will open a connection to the SPI driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params::params()) -> spi().
open(Params) ->
    open_port({spawn, "spi"}, Params).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address from which to read
%% @returns `{ok, Value}' or `error'
%% @doc     Read a value from and address on the device.
%% @end
%%-----------------------------------------------------------------------------
-spec read_at(SPI::spi(), Address::address(), Len::non_neg_integer()) -> {ok, integer()} | error.
read_at(SPI, Address, Len) ->
    port:call(SPI, {read_at, Address, Len}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address to which to write
%% @returns `{ok, Value}' or `error'
%% @doc     Write a value to and address on the device.
%%
%%          The value returned from this function is dependent on the device and address.
%%          Consult the documentation for the device to understand expected return
%%          values from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec write_at(SPI::spi(), Address::address(), Len::non_neg_integer(), Data::integer()) -> {ok, integer()} | error.
write_at(SPI, Address, Len, Data) ->
    port:call(SPI, {write_at, Address bor 16#80, Len, Data}).
