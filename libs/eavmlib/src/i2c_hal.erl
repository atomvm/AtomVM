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
%% @doc I2C Hardware Abstraction Layer behavior
%%
%% This module defines the behavior that platform-specific I2C modules
%% must implement. It provides a common interface for I2C (Inter-Integrated
%% Circuit) operations across all supported platforms.
%%
%% Currently, only ESP32 provides an I2C implementation.
%%
%% <h3>Lifecycle</h3>
%%
%% An I2C bus is opened with `open/1' and closed with `close/1'. The
%% returned handle is passed to all subsequent operations.
%%
%% <h3>Reading data</h3>
%%
%% Data can be read from I2C devices using `read_bytes/3' or
%% `read_bytes/4':
%%
%% <ul>
%% <li>`read_bytes/3' reads a number of bytes directly from a device
%% at the given address.</li>
%% <li>`read_bytes/4' reads a number of bytes from a specific register
%% within a device.</li>
%% </ul>
%%
%% <h3>Writing data</h3>
%%
%% There are two approaches for writing data:
%%
%% <b>Direct writes:</b> `write_bytes/3' and `write_bytes/4' write data
%% to a device address, optionally specifying a register:
%%
%% ```
%% i2c:write_bytes(I2C, 16#68, 16#0D, <<16#FF>>).
%% '''
%%
%% <b>Transaction-based writes:</b> For more control, use
%% `begin_transmission/2', followed by one or more calls to
%% `write_byte/2' or `write_bytes/2', and finalize with
%% `end_transmission/1':
%%
%% ```
%% i2c:begin_transmission(I2C, 16#68),
%% i2c:write_byte(I2C, 16#0D),
%% i2c:write_bytes(I2C, <<16#FF, 16#00>>),
%% i2c:end_transmission(I2C).
%% '''
%%
%% <h3>Configuration parameters</h3>
%%
%% The `open/1' function accepts a proplist of configuration parameters.
%% Common parameters include:
%%
%% <ul>
%% <li>`{scl, pin()}' - SCL (clock) pin number</li>
%% <li>`{sda, pin()}' - SDA (data) pin number</li>
%% <li>`{clock_speed_hz, pos_integer()}' - Clock speed in Hz</li>
%% <li>`{peripheral, string() | binary()}' - I2C peripheral name (e.g.
%% `"i2c0"', `"i2c1"')</li>
%% </ul>
%%
%% <h3>Example</h3>
%%
%% ```
%% I2C = i2c:open([{scl, 22}, {sda, 21}, {clock_speed_hz, 100000}]),
%% {ok, Data} = i2c:read_bytes(I2C, 16#68, 16#75, 1),
%% i2c:write_bytes(I2C, 16#68, 16#6B, 0),
%% i2c:close(I2C).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(i2c_hal).

-type i2c() :: port() | pid() | term().
%% Handle returned by `open/1'.
%% On ESP32, this is either a port (port driver mode) or a resource
%% tuple (NIF mode).

-type address() :: 0..127.
%% 7-bit I2C device address (e.g. `16#68').

-type register() :: non_neg_integer().
%% Register address within an I2C device.

-type params() :: [term()].
%% Initialization parameters for the I2C bus.
%% See the module documentation for common parameters.

-export_type([i2c/0, address/0, register/0, params/0]).

% Open an I2C bus with the given configuration parameters.
%
% Returns a handle that must be passed to all subsequent I2C
% operations.
-callback open(Params :: params()) -> i2c().

% Close an I2C bus and release its resources.
-callback close(I2C :: i2c()) -> ok | {error, Reason :: term()}.

% Begin a write transaction to a device at the given address.
%
% After calling this function, use `write_byte/2' or `write_bytes/2'
% to send data, then call `end_transmission/1' to complete the
% transaction.
-callback begin_transmission(I2C :: i2c(), Address :: address()) ->
    ok | {error, Reason :: term()}.

% Write a single byte as part of a transaction.
%
% Must be called between `begin_transmission/2' and
% `end_transmission/1'.
-callback write_byte(I2C :: i2c(), Byte :: byte()) ->
    ok | {error, Reason :: term()}.

% Write binary data as part of a transaction.
%
% Must be called between `begin_transmission/2' and
% `end_transmission/1'.
-callback write_bytes(I2C :: i2c(), Bytes :: binary()) ->
    ok | {error, Reason :: term()}.

% End a write transaction started with `begin_transmission/2'.
%
% Sends all buffered data to the device.
-callback end_transmission(I2C :: i2c()) ->
    ok | {error, Reason :: term()}.

% Read a number of bytes from a device at the given address.
-callback read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.

% Read a number of bytes from a specific register within a device.
-callback read_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), Count :: non_neg_integer()
) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.

% Write data to a device at the given address.
%
% BinOrInt can be a binary or a single byte value.
-callback write_bytes(I2C :: i2c(), Address :: address(), BinOrInt :: binary() | byte()) ->
    ok | {error, Reason :: term()}.

% Write data to a specific register within a device.
%
% BinOrInt can be a binary or a single byte value.
-callback write_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), BinOrInt :: binary() | byte()
) ->
    ok | {error, Reason :: term()}.
