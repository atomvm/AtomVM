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
%% must implement. It provides a common interface for SPI (Serial
%% Peripheral Interface) operations across all supported platforms.
%%
%% Currently, only ESP32 provides an SPI implementation.
%%
%% <h3>Lifecycle</h3>
%%
%% An SPI bus is opened with `open/1' and closed with `close/1'. The
%% `open/1' function takes a configuration that includes both bus
%% parameters (pins, peripheral) and device configurations. Multiple
%% devices can share the same SPI bus.
%%
%% <h3>Device names</h3>
%%
%% Each device on the SPI bus is identified by a `device_name()' atom.
%% Device names are defined during `open/1' configuration and used in
%% all subsequent read/write operations. For example, if the
%% configuration includes `{my_sensor, [{cs, 5}]}', then `my_sensor'
%% is used as the device name in calls like
%% `spi:read_at(SPI, my_sensor, Address, Len)'.
%%
%% <h3>Simple read/write</h3>
%%
%% For simple address-based operations:
%%
%% <ul>
%% <li>`read_at/4' - Read data from a device at a given address</li>
%% <li>`write_at/5' - Write data to a device at a given address</li>
%% </ul>
%%
%% <h3>Transaction-based operations</h3>
%%
%% For more complex operations, use transaction maps with `write/3'
%% or `write_read/3':
%%
%% ```
%% Transaction = #{
%%     command => 16#42,
%%     address => 16#00,
%%     write_data => <<1, 2, 3>>,
%%     write_bits => 24,
%%     read_bits => 8
%% },
%% {ok, ReadData} = spi:write_read(SPI, my_device, Transaction).
%% '''
%%
%% <h3>Configuration parameters</h3>
%%
%% The `open/1' function accepts a proplist or map with bus and device
%% configuration. Bus parameters include:
%%
%% <ul>
%% <li>`{sclk, non_neg_integer()}' - SCLK (clock) pin number
%% (required)</li>
%% <li>`{miso, non_neg_integer()}' - MISO pin number (optional)</li>
%% <li>`{mosi, non_neg_integer()}' - MOSI pin number (optional)</li>
%% <li>`{peripheral, string() | binary()}' - SPI peripheral (e.g.
%% `"spi2"', `"spi3"', default: `"spi2"')</li>
%% </ul>
%%
%% Device parameters are specified as `{DeviceName, DeviceOpts}' entries:
%%
%% <ul>
%% <li>`{cs, non_neg_integer()}' - Chip Select pin number</li>
%% <li>`{clock_speed_hz, non_neg_integer()}' - Clock speed (default:
%% 1000000)</li>
%% <li>`{mode, 0..3}' - SPI mode (default: 0)</li>
%% <li>`{address_len_bits, 0..64}' - Address width in bits (default:
%% 8)</li>
%% <li>`{command_len_bits, 0..16}' - Command width in bits (default:
%% 0)</li>
%% </ul>
%%
%% <h3>Example</h3>
%%
%% ```
%% SPI = spi:open([
%%     {sclk, 18}, {miso, 19}, {mosi, 23},
%%     {my_device, [{cs, 5}, {clock_speed_hz, 1000000}]}
%% ]),
%% {ok, Value} = spi:read_at(SPI, my_device, 16#0F, 8),
%% spi:close(SPI).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(spi_hal).

-type spi() :: port() | pid() | term().
%% Handle returned by `open/1'.

-type device_name() :: atom().
%% Name identifying an SPI device, as specified in the device
%% configuration passed to `open/1'.

-type address() :: non_neg_integer().
%% SPI device address.

-type params() :: [term()] | map().
%% Initialization parameters for the SPI bus and its devices.
%% See the module documentation for common parameters.

-type transaction() :: #{
    command => integer(),
    address => non_neg_integer(),
    write_data => binary(),
    write_bits => non_neg_integer(),
    read_bits => non_neg_integer()
}.
%% SPI transaction map. Fields are all optional and depend on the
%% operation being performed.

-export_type([spi/0, device_name/0, address/0, params/0, transaction/0]).

% Open an SPI bus with the given configuration.
%
% The configuration includes both bus parameters (pins, peripheral)
% and device configurations. Returns a handle for subsequent
% operations.
-callback open(Params :: params()) -> spi().

% Close the SPI bus and release its resources.
-callback close(SPI :: spi()) -> ok.

% Read data from a device at the given address.
%
% Returns `{ok, Value}' where Value is an integer representing the
% read data, or `{error, Reason}'.
-callback read_at(
    SPI :: spi(), DeviceName :: device_name(), Address :: address(), Len :: non_neg_integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.

% Write data to a device at the given address.
%
% Returns `{ok, Value}' where Value is the response from the device,
% or `{error, Reason}'.
-callback write_at(
    SPI :: spi(),
    DeviceName :: device_name(),
    Address :: address(),
    Len :: non_neg_integer(),
    Data :: integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.

% Write data to a device using a transaction map.
%
% The transaction map specifies the command, address, and data to
% write. See the `transaction()' type for details.
-callback write(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    ok | {error, Reason :: term()}.

% Write data to and read data from a device in a single transaction.
%
% Performs a simultaneous write and read operation. Returns the read
% data as a binary.
-callback write_read(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    {ok, ReadData :: binary()} | {error, Reason :: term()}.
