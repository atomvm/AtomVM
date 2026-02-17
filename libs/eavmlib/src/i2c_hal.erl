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
%% must implement. It provides a common interface for I2C operations
%% across all supported platforms.
%% @end
%%-----------------------------------------------------------------------------
-module(i2c_hal).

-type i2c() :: port() | pid() | term().
%% Handle returned by `open/1'.

-type address() :: non_neg_integer().
%% I2C device address.

-type register() :: non_neg_integer().
%% Register address within an I2C device.

-type params() :: [term()].
%% Initialization parameters for the I2C bus.

-export_type([i2c/0, address/0, register/0, params/0]).

-callback open(Params :: params()) -> i2c().

-callback close(I2C :: i2c()) -> ok | {error, Reason :: term()}.

-callback begin_transmission(I2C :: i2c(), Address :: address()) ->
    ok | {error, Reason :: term()}.

-callback write_byte(I2C :: i2c(), Byte :: byte()) ->
    ok | {error, Reason :: term()}.

-callback write_bytes(I2C :: i2c(), Bytes :: binary()) ->
    ok | {error, Reason :: term()}.

-callback end_transmission(I2C :: i2c()) ->
    ok | {error, Reason :: term()}.

-callback read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.

-callback read_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), Count :: non_neg_integer()
) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.

-callback write_bytes(I2C :: i2c(), Address :: address(), BinOrInt :: binary() | byte()) ->
    ok | {error, Reason :: term()}.

-callback write_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), BinOrInt :: binary() | integer()
) ->
    ok | {error, Reason :: term()}.
