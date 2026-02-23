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

%%-----------------------------------------------------------------------------
%% @doc SPI Hardware Abstraction Layer behavior
%%
%% This module defines the behavior that platform-specific SPI modules
%% must implement. It provides a common interface for SPI operations
%% across all supported platforms.
%% @end
%%-----------------------------------------------------------------------------
-module(spi_hal).

-type spi() :: port() | pid() | term().
%% Handle returned by `open/1'.

-type device_name() :: atom().
%% Name identifying an SPI device, as specified in the device configuration.

-type address() :: non_neg_integer().
%% SPI device address.

-type params() :: [term()] | map().
%% Initialization parameters for the SPI bus.

-type transaction() :: #{
    command => integer(),
    address => non_neg_integer(),
    write_data => binary(),
    write_bits => non_neg_integer(),
    read_bits => non_neg_integer()
}.
%% SPI transaction map.

-export_type([spi/0, device_name/0, address/0, params/0, transaction/0]).

-callback open(Params :: params()) -> spi().

-callback close(SPI :: spi()) -> ok.

-callback read_at(
    SPI :: spi(), DeviceName :: device_name(), Address :: address(), Len :: non_neg_integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.

-callback write_at(
    SPI :: spi(),
    DeviceName :: device_name(),
    Address :: address(),
    Len :: non_neg_integer(),
    Data :: integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.

-callback write(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    ok | {error, Reason :: term()}.

-callback write_read(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    {ok, ReadData :: binary()} | {error, Reason :: term()}.
