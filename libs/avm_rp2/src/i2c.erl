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
%% @doc AtomVM I2C interface for RP2 (Pico)
%%
%% This module provides an interface to the I2C hardware on RP2 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API</b>
%% {@link init/2}, {@link deinit/1}, {@link set_baudrate/2},
%% {@link write_blocking/4}, {@link read_blocking/4},
%% {@link write_timeout_us/5}, {@link read_timeout_us/5}.
%% These operate on a bare resource reference returned by {@link init/2}.
%% Pin muxing must be done separately via `gpio:set_function/2' and
%% `gpio:set_pin_pull/2'.
%%
%% <b>High-level API (`i2c_hal` behavior)</b>
%% {@link open/1}, {@link close/1}, {@link read_bytes/3}, {@link read_bytes/4},
%% {@link write_bytes/3}, {@link write_bytes/4},
%% {@link begin_transmission/2}, {@link write_byte/2}, {@link write_bytes/2},
%% {@link end_transmission/1}.
%% {@link open/1} handles pin setup automatically.
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

%% Low-level API (Pico SDK)
-export([
    init/2,
    deinit/1,
    set_baudrate/2,
    write_blocking/4,
    read_blocking/4,
    write_timeout_us/5,
    read_timeout_us/5
]).

-type pin() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type peripheral() :: 0 | 1.
-type param() ::
    {scl, pin()}
    | {sda, pin()}
    | {clock_speed_hz, freq_hz()}
    | {peripheral, peripheral()}
    | {send_timeout_ms, timeout()}.
-type params() :: [param()].
-type i2c_resource() :: reference().
-type i2c() :: pid().
-type address() :: non_neg_integer().
-type register_addr() :: non_neg_integer().

-export_type([
    i2c/0, i2c_resource/0, address/0, register_addr/0
]).

-define(DEFAULT_CLOCK_SPEED_HZ, 100000).
-define(DEFAULT_PERIPHERAL, 0).
-define(DEFAULT_SEND_TIMEOUT_MS, 500).

%% ---------------------------------------------------------------------------
%% High-level API (ESP32-compatible)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns I2C handle
%% @doc     Open a connection to the I2C driver
%%
%%          This function configures the GPIO pins for I2C function, enables
%%          internal pull-ups, and initializes the I2C peripheral.
%%
%%          Supported parameters:
%%          <ul>
%%              <li>`{scl, Pin}' - the SCL pin number (required)</li>
%%              <li>`{sda, Pin}' - the SDA pin number (required)</li>
%%              <li>`{clock_speed_hz, Hz}' - the I2C clock speed in Hz (default: 100000)</li>
%%              <li>`{peripheral, 0 | 1}' - the I2C peripheral to use (default: 0)</li>
%%              <li>`{send_timeout_ms, Ms | infinity}' - send timeout in milliseconds (default: 500)</li>
%%          </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> i2c().
open(Params) ->
    SCL = proplists:get_value(scl, Params),
    SDA = proplists:get_value(sda, Params),
    ClockSpeedHz = proplists:get_value(clock_speed_hz, Params, ?DEFAULT_CLOCK_SPEED_HZ),
    Peripheral = proplists:get_value(peripheral, Params, ?DEFAULT_PERIPHERAL),
    SendTimeoutMs = proplists:get_value(send_timeout_ms, Params, ?DEFAULT_SEND_TIMEOUT_MS),
    gpio:set_function(SCL, i2c),
    gpio:set_function(SDA, i2c),
    gpio:set_pin_pull(SCL, up),
    gpio:set_pin_pull(SDA, up),
    {ok, {_ActualBaudrate, Resource}} = ?MODULE:init(Peripheral, ClockSpeedHz),
    spawn_link(fun() -> loop(Resource, SendTimeoutMs, undefined) end).

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
%%          This performs a write of the register address with nostop (repeated
%%          start), followed by a read of the requested number of bytes.
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
%% Low-level API (Pico SDK)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Peripheral I2C peripheral number (0 or 1)
%% @param   Baudrate Baudrate in Hz (e.g. 100000 for 100kHz)
%% @returns `{ok, {ActualBaudrate, Resource}}'
%% @doc     Initialize the I2C HW block.
%%
%%          Pin muxing must be done separately via `gpio:set_function/2'
%%          and `gpio:set_pin_pull/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Peripheral :: peripheral(), Baudrate :: freq_hz()) ->
    {ok, {ActualBaudrate :: freq_hz(), Resource :: i2c_resource()}}.
init(_Peripheral, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @returns `ok'
%% @doc     Disable the I2C HW block.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: i2c_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @param   Baudrate Baudrate in Hz
%% @returns `{ok, ActualBaudrate}'
%% @doc     Set I2C baudrate.
%% @end
%%-----------------------------------------------------------------------------
-spec set_baudrate(Resource :: i2c_resource(), Baudrate :: freq_hz()) ->
    {ok, ActualBaudrate :: freq_hz()}.
set_baudrate(_Resource, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @param   Addr 7-bit I2C device address
%% @param   Data Binary data to write
%% @param   Nostop If true, master retains control of the bus (no Stop issued)
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Write to I2C device, blocking.
%% @end
%%-----------------------------------------------------------------------------
-spec write_blocking(
    Resource :: i2c_resource(), Addr :: address(), Data :: binary(), Nostop :: boolean()
) ->
    non_neg_integer() | {error, Reason :: term()}.
write_blocking(_Resource, _Addr, _Data, _Nostop) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @param   Addr 7-bit I2C device address
%% @param   Count Number of bytes to read
%% @param   Nostop If true, master retains control of the bus (no Stop issued)
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read from I2C device, blocking.
%% @end
%%-----------------------------------------------------------------------------
-spec read_blocking(
    Resource :: i2c_resource(), Addr :: address(), Count :: non_neg_integer(), Nostop :: boolean()
) ->
    {ok, binary()} | {error, Reason :: term()}.
read_blocking(_Resource, _Addr, _Count, _Nostop) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @param   Addr 7-bit I2C device address
%% @param   Data Binary data to write
%% @param   Nostop If true, master retains control of the bus (no Stop issued)
%% @param   TimeoutUs Timeout in microseconds
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Write to I2C device, with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec write_timeout_us(
    Resource :: i2c_resource(),
    Addr :: address(),
    Data :: binary(),
    Nostop :: boolean(),
    TimeoutUs :: non_neg_integer()
) ->
    non_neg_integer() | {error, Reason :: term()}.
write_timeout_us(_Resource, _Addr, _Data, _Nostop, _TimeoutUs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/2'
%% @param   Addr 7-bit I2C device address
%% @param   Count Number of bytes to read
%% @param   Nostop If true, master retains control of the bus (no Stop issued)
%% @param   TimeoutUs Timeout in microseconds
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read from I2C device, with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read_timeout_us(
    Resource :: i2c_resource(),
    Addr :: address(),
    Count :: non_neg_integer(),
    Nostop :: boolean(),
    TimeoutUs :: non_neg_integer()
) ->
    {ok, binary()} | {error, Reason :: term()}.
read_timeout_us(_Resource, _Addr, _Count, _Nostop, _TimeoutUs) ->
    erlang:nif_error(undefined).

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

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
        case do_write(Resource, Address, Data, false, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, undefined, end_transmission) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Count}) ->
    Result = do_read(Resource, Address, Count, false, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Register, Count}) ->
    Result =
        case do_write(Resource, Address, <<Register:8>>, true, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> do_read(Resource, Address, Count, false, SendTimeoutMs)
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Data}) ->
    Result =
        case do_write(Resource, Address, Data, false, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Register, Data}) ->
    Result =
        case do_write(Resource, Address, <<Register:8, Data/binary>>, false, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState}.

%% @private
do_read(Resource, Address, Count, Nostop, infinity) ->
    ?MODULE:read_blocking(Resource, Address, Count, Nostop);
do_read(Resource, Address, Count, Nostop, TimeoutMs) ->
    ?MODULE:read_timeout_us(Resource, Address, Count, Nostop, TimeoutMs * 1000).

%% @private
do_write(Resource, Address, Data, Nostop, infinity) ->
    ?MODULE:write_blocking(Resource, Address, Data, Nostop);
do_write(Resource, Address, Data, Nostop, TimeoutMs) ->
    ?MODULE:write_timeout_us(Resource, Address, Data, Nostop, TimeoutMs * 1000).
