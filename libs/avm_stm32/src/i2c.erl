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
%% @doc AtomVM I2C interface for STM32
%%
%% This module provides an interface to the I2C hardware on STM32 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API</b>
%% {@link init/1}, {@link deinit/1}, {@link write/4}, {@link read/4},
%% {@link mem_read/5}, {@link mem_write/5}.
%% These operate on a bare resource reference returned by {@link init/1}.
%%
%% <b>High-level API (`i2c_hal' behavior)</b>
%% {@link open/1}, {@link close/1}, {@link read_bytes/3}, {@link read_bytes/4},
%% {@link write_bytes/3}, {@link write_bytes/4},
%% {@link begin_transmission/2}, {@link write_byte/2}, {@link write_bytes/2},
%% {@link end_transmission/1}.
%% {@link open/1} handles pin setup and I2C initialization automatically.
%% @end
%%-----------------------------------------------------------------------------
-module(i2c).

-behaviour(i2c_hal).

%% High-level API (i2c_hal behaviour)
-export([
    open/1,
    close/1,
    begin_transmission/2,
    write_byte/2,
    end_transmission/1,
    read_bytes/3, read_bytes/4,
    write_bytes/2, write_bytes/3, write_bytes/4
]).

%% Low-level API (STM32 HAL)
-export([
    init/1,
    deinit/1,
    write/4,
    read/4,
    mem_read/5,
    mem_write/5
]).

-type pin() :: {gpio_bank(), 0..15}.
-type gpio_bank() :: a | b | c | d | e | f | g | h | i | j | k.
-type freq_hz() :: non_neg_integer().
-type peripheral() :: 1..4.
-type param() ::
    {scl, pin()}
    | {sda, pin()}
    | {clock_speed_hz, freq_hz()}
    | {peripheral, peripheral()}
    | {af, non_neg_integer()}
    | {send_timeout_ms, timeout()}.
-type params() :: [param()].
-type i2c_resource() :: reference().
-type i2c() :: pid().
-type address() :: non_neg_integer().
-type register_addr() :: non_neg_integer().

-export_type([
    i2c/0, i2c_resource/0, address/0, register_addr/0
]).

-define(DEFAULT_SEND_TIMEOUT_MS, 500).

%% ---------------------------------------------------------------------------
%% High-level API
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns I2C handle
%% @doc     Open a connection to the I2C driver
%%
%%          This function configures the GPIO pins for I2C alternate function
%%          with open-drain and pull-up, and initializes the I2C peripheral.
%%
%%          Supported parameters:
%%          <ul>
%%              <li>`{scl, {Bank, Pin}}' - the SCL pin (required)</li>
%%              <li>`{sda, {Bank, Pin}}' - the SDA pin (required)</li>
%%              <li>`{clock_speed_hz, Hz}' - clock speed in Hz (default: 100000)</li>
%%              <li>`{peripheral, 1..4}' - I2C peripheral number (default: 1)</li>
%%              <li>`{af, N}' - GPIO alternate function number (default: 4)</li>
%%              <li>`{send_timeout_ms, Ms | infinity}' - send timeout (default: 500)</li>
%%          </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> i2c().
open(Params) ->
    SendTimeoutMs = get_value(send_timeout_ms, Params, ?DEFAULT_SEND_TIMEOUT_MS),
    {ok, Resource} = ?MODULE:init(Params),
    erlang:spawn_opt(fun() -> loop(Resource, SendTimeoutMs, undefined) end, [link]).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @returns `ok'
%% @doc     Closes the connection to the I2C driver and frees resources.
%% @end
%%-----------------------------------------------------------------------------
-spec close(I2C :: i2c()) -> ok | {error, Reason :: term()}.
close(Pid) ->
    call(Pid, close).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @returns `ok' or `{error, Reason}'
%% @doc     Begin a transmission of I2C commands
%%
%%          This command is typically followed by one or more calls to
%%          `write_byte/2' and then a call to `end_transmission/1'
%% @end
%%-----------------------------------------------------------------------------
-spec begin_transmission(I2C :: i2c(), Address :: address()) -> ok | {error, Reason :: term()}.
begin_transmission(Pid, Address) ->
    call(Pid, {begin_transmission, Address}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a byte to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_byte(I2C :: i2c(), Byte :: byte()) -> ok | {error, Reason :: term()}.
write_byte(Pid, Byte) ->
    call(Pid, {write_byte, Byte}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Bytes value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a sequence of bytes to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2C :: i2c(), Bytes :: binary()) -> ok | {error, Reason :: term()}.
write_bytes(Pid, Bytes) ->
    call(Pid, {write_bytes_tx, Bytes}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @returns `ok' or `{error, Reason}'
%% @doc     End a transmission of I2C commands
%%
%%          This command is typically preceded by a call to `begin_transmission/2'
%%          and one or more calls to `write_byte/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec end_transmission(I2C :: i2c()) -> ok | {error, Reason :: term()}.
end_transmission(Pid) ->
    call(Pid, end_transmission).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @param   Count The number of bytes to read
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read a block of bytes from the I2C device.
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.
read_bytes(Pid, Address, Count) ->
    call(Pid, {read_bytes, Address, Count}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @param   Register The register address from which to read
%% @param   Count The number of bytes to read
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read a block of bytes from the I2C device starting at a specified
%%          register address.
%%
%%          This uses HAL_I2C_Mem_Read which performs an I2C combined
%%          write-then-read transaction (register address write followed by
%%          repeated start and read).
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(
    I2C :: i2c(), Address :: address(), Register :: register_addr(), Count :: non_neg_integer()
) -> {ok, binary()} | {error, Reason :: term()}.
read_bytes(Pid, Address, Register, Count) ->
    call(Pid, {read_bytes, Address, Register, Count}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @param   Data The binary or byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a block of bytes to the I2C device.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2C :: i2c(), Address :: address(), BinOrInt :: binary() | byte()) ->
    ok | {error, Reason :: term()}.
write_bytes(Pid, Address, Int) when is_integer(Int) ->
    write_bytes(Pid, Address, <<Int:8>>);
write_bytes(Pid, Address, Data) ->
    call(Pid, {write_bytes, Address, Data}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @param   Register The register address to which to write
%% @param   Data The binary or byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a block of bytes to the I2C device starting at a specified
%%          register address.
%%
%%          This uses HAL_I2C_Mem_Write which handles the register address
%%          and data in a single I2C transaction.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: register_addr(),
    BinOrInt :: binary() | integer()
) -> ok | {error, Reason :: term()}.
write_bytes(Pid, Address, Register, Int) when is_integer(Int) ->
    write_bytes(Pid, Address, Register, <<Int:8>>);
write_bytes(Pid, Address, Register, Data) ->
    call(Pid, {write_bytes, Address, Register, Data}).

%% ---------------------------------------------------------------------------
%% Low-level API (STM32 HAL NIFs)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Opts Initialization options proplist
%% @returns `{ok, Resource}'
%% @doc     Initialize the I2C peripheral.
%%
%%          Configures GPIO pins and initializes the I2C hardware.
%%          Options: `peripheral', `scl', `sda', `clock_speed_hz', `af'.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Opts :: params()) -> {ok, Resource :: i2c_resource()}.
init(_Opts) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @returns `ok'
%% @doc     Deinitialize the I2C peripheral.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: i2c_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Data Binary data to write
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Write to I2C device (master transmit).
%% @end
%%-----------------------------------------------------------------------------
-spec write(
    Resource :: i2c_resource(), Addr :: address(), Data :: binary(), TimeoutMs :: timeout()
) ->
    non_neg_integer() | {error, Reason :: term()}.
write(_Resource, _Addr, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Count Number of bytes to read
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read from I2C device (master receive).
%% @end
%%-----------------------------------------------------------------------------
-spec read(
    Resource :: i2c_resource(),
    Addr :: address(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    {ok, binary()} | {error, Reason :: term()}.
read(_Resource, _Addr, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Register Register address to read from
%% @param   Count Number of bytes to read
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read from a register of an I2C device (combined write+read).
%% @end
%%-----------------------------------------------------------------------------
-spec mem_read(
    Resource :: i2c_resource(),
    Addr :: address(),
    Register :: register_addr(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    {ok, binary()} | {error, Reason :: term()}.
mem_read(_Resource, _Addr, _Register, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Register Register address to write to
%% @param   Data Binary data to write
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Write to a register of an I2C device.
%% @end
%%-----------------------------------------------------------------------------
-spec mem_write(
    Resource :: i2c_resource(),
    Addr :: address(),
    Register :: register_addr(),
    Data :: binary(),
    TimeoutMs :: timeout()
) ->
    non_neg_integer() | {error, Reason :: term()}.
mem_write(_Resource, _Addr, _Register, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

%% @private
get_value(_Key, [], Default) -> Default;
get_value(Key, [{Key, Value} | _], _Default) -> Value;
get_value(Key, [_ | Rest], Default) -> get_value(Key, Rest, Default).

%% @private
call(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} -> Reply
    end.

%% @private
loop(Resource, SendTimeoutMs, TxState) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, SendTimeoutMs, TxState, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply, NewTxState} ->
                    From ! {Ref, Reply},
                    loop(Resource, SendTimeoutMs, NewTxState)
            end
    end.

%% @private
handle_request(Resource, _SendTimeoutMs, _TxState, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(_Resource, _SendTimeoutMs, undefined, {begin_transmission, Address}) ->
    {reply, ok, {Address, []}};
handle_request(_Resource, _SendTimeoutMs, {_Address, _Acc}, {begin_transmission, _NewAddress}) ->
    {reply, {error, transaction_already_in_progress}, {_Address, _Acc}};
handle_request(_Resource, _SendTimeoutMs, {Address, Acc}, {write_byte, Byte}) ->
    {reply, ok, {Address, [<<Byte:8>> | Acc]}};
handle_request(_Resource, _SendTimeoutMs, undefined, {write_byte, _Byte}) ->
    {reply, {error, no_transaction}, undefined};
handle_request(_Resource, _SendTimeoutMs, {Address, Acc}, {write_bytes_tx, Bytes}) ->
    {reply, ok, {Address, [Bytes | Acc]}};
handle_request(_Resource, _SendTimeoutMs, undefined, {write_bytes_tx, _Bytes}) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, {Address, Acc}, end_transmission) ->
    Data = erlang:iolist_to_binary(lists:reverse(Acc)),
    Result =
        case ?MODULE:write(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, undefined, end_transmission) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Count}) ->
    Result = ?MODULE:read(Resource, Address, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Register, Count}) ->
    Result = ?MODULE:mem_read(Resource, Address, Register, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Data}) ->
    Result =
        case ?MODULE:write(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Register, Data}) ->
    Result =
        case ?MODULE:mem_write(Resource, Address, Register, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState}.
