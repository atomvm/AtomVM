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

%%-----------------------------------------------------------------------------
%% @doc AtomVM I2C interface for Zephyr.
%%
%% This module provides the common `i2c_hal' API for Zephyr platforms. Zephyr
%% owns pin muxing and bus setup through devicetree; applications select an I2C
%% bus with the optional `peripheral' parameter and may request a bus speed with
%% `clock_speed_hz'.
%%
%% The low-level API maps to AtomVM Zephyr I2C resource NIFs and operates on a
%% resource returned by {@link init/1}. Slave-mode functions are present for API
%% symmetry but return `{error, enotsup}' until target-mode support is added.
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

%% Low-level API (Zephyr I2C NIFs)
-export([
    init/1,
    deinit/1,
    master_transmit/4,
    master_receive/4,
    slave_transmit/3,
    slave_receive/3,
    mem_read/6,
    mem_write/6,
    is_device_ready/4
]).

-type freq_hz() :: pos_integer().
-type peripheral() :: non_neg_integer() | string() | binary().
-type mem_add_size() :: 8 | 16.
-type param() ::
    {clock_speed_hz, freq_hz()}
    | {peripheral, peripheral()}
    | {send_timeout_ms, timeout()}.
-type params() :: [param()].
-type i2c_resource() :: reference().
-type i2c() :: pid().
-type address() :: 0..127.
-type register_addr() :: non_neg_integer().

-export_type([
    i2c/0, i2c_resource/0, address/0, register_addr/0, mem_add_size/0
]).

-define(DEFAULT_SEND_TIMEOUT_MS, 500).

%% ---------------------------------------------------------------------------
%% High-level API
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns I2C handle
%% @doc     Open a connection to the I2C driver.
%%
%%          Supported parameters:
%%          <ul>
%%              <li>`{peripheral, N | Name}' - I2C bus index or Zephyr device
%%                  name, such as `0' or `"i2c0"'. Defaults to the devicetree
%%                  `atomvm,i2c' chosen node when present, otherwise `i2c0'.</li>
%%              <li>`{clock_speed_hz, Hz}' - requested I2C clock speed. Zephyr
%%                  maps this to one of its standard speed tiers.</li>
%%              <li>`{send_timeout_ms, Ms | infinity}' - send timeout stored in
%%                  the high-level server state. Zephyr controller drivers own
%%                  the actual synchronous transfer timeout.</li>
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
%% @doc     Close the connection to the I2C driver and free resources.
%% @end
%%-----------------------------------------------------------------------------
-spec close(I2C :: i2c()) -> ok | {error, Reason :: term()}.
close(Pid) ->
    call(Pid, close).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Address 7-bit I2C address of the device
%% @returns `ok' or `{error, Reason}'
%% @doc     Begin a transmission of I2C commands.
%% @end
%%-----------------------------------------------------------------------------
-spec begin_transmission(I2C :: i2c(), Address :: address()) -> ok | {error, Reason :: term()}.
begin_transmission(Pid, Address) ->
    call(Pid, {begin_transmission, Address}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a byte to the current transaction.
%% @end
%%-----------------------------------------------------------------------------
-spec write_byte(I2C :: i2c(), Byte :: byte()) -> ok | {error, Reason :: term()}.
write_byte(Pid, Byte) ->
    call(Pid, {write_byte, Byte}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @param   Bytes value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write bytes to the current transaction.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2C :: i2c(), Bytes :: binary()) -> ok | {error, Reason :: term()}.
write_bytes(Pid, Bytes) ->
    call(Pid, {write_bytes_tx, Bytes}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C handle created via `open/1'
%% @returns `ok' or `{error, Reason}'
%% @doc     End and send the current transaction.
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
%% @doc     Read a block of bytes from an I2C device.
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
%% @doc     Read a block of bytes from a device register.
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
%% @doc     Write a block of bytes to an I2C device.
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
%% @doc     Write a block of bytes to a device register.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: register_addr(),
    BinOrInt :: binary() | byte()
) -> ok | {error, Reason :: term()}.
write_bytes(Pid, Address, Register, Int) when is_integer(Int) ->
    write_bytes(Pid, Address, Register, <<Int:8>>);
write_bytes(Pid, Address, Register, Data) ->
    call(Pid, {write_bytes, Address, Register, Data}).

%% ---------------------------------------------------------------------------
%% Low-level API (Zephyr I2C NIFs)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Opts Initialization options proplist
%% @returns `{ok, Resource}' or `{error, Reason}'
%% @doc     Open a Zephyr I2C controller resource.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Opts :: params()) -> {ok, Resource :: i2c_resource()} | {error, Reason :: term()}.
init(_Opts) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @returns `ok'
%% @doc     Release an I2C resource.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: i2c_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Data Binary data to transmit
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Master transmit.
%% @end
%%-----------------------------------------------------------------------------
-spec master_transmit(
    Resource :: i2c_resource(), Addr :: address(), Data :: binary(), TimeoutMs :: timeout()
) ->
    non_neg_integer() | {error, Reason :: term()}.
master_transmit(_Resource, _Addr, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Count Number of bytes to receive
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Master receive.
%% @end
%%-----------------------------------------------------------------------------
-spec master_receive(
    Resource :: i2c_resource(),
    Addr :: address(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    {ok, binary()} | {error, Reason :: term()}.
master_receive(_Resource, _Addr, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Data Binary data to transmit
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Slave transmit. Currently returns `{error, enotsup}' on Zephyr.
%% @end
%%-----------------------------------------------------------------------------
-spec slave_transmit(
    Resource :: i2c_resource(), Data :: binary(), TimeoutMs :: timeout()
) ->
    non_neg_integer() | {error, Reason :: term()}.
slave_transmit(_Resource, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Count Number of bytes to receive
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Slave receive. Currently returns `{error, enotsup}' on Zephyr.
%% @end
%%-----------------------------------------------------------------------------
-spec slave_receive(
    Resource :: i2c_resource(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    {ok, binary()} | {error, Reason :: term()}.
slave_receive(_Resource, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   MemAddr Memory/register address to read from
%% @param   MemAddSize Memory address size: `8' for 8-bit or `16' for 16-bit
%% @param   Count Number of bytes to read
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Read from a device register.
%% @end
%%-----------------------------------------------------------------------------
-spec mem_read(
    Resource :: i2c_resource(),
    Addr :: address(),
    MemAddr :: register_addr(),
    MemAddSize :: mem_add_size(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    {ok, binary()} | {error, Reason :: term()}.
mem_read(_Resource, _Addr, _MemAddr, _MemAddSize, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   MemAddr Memory/register address to write to
%% @param   MemAddSize Memory address size: `8' for 8-bit or `16' for 16-bit
%% @param   Data Binary data to write
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Write to a device register.
%% @end
%%-----------------------------------------------------------------------------
-spec mem_write(
    Resource :: i2c_resource(),
    Addr :: address(),
    MemAddr :: register_addr(),
    MemAddSize :: mem_add_size(),
    Data :: binary(),
    TimeoutMs :: timeout()
) ->
    non_neg_integer() | {error, Reason :: term()}.
mem_write(_Resource, _Addr, _MemAddr, _MemAddSize, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource I2C resource returned by `init/1'
%% @param   Addr 7-bit I2C device address
%% @param   Trials Number of trials
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `ok' or `{error, Reason}'
%% @doc     Check if a device acknowledges on the bus.
%% @end
%%-----------------------------------------------------------------------------
-spec is_device_ready(
    Resource :: i2c_resource(),
    Addr :: address(),
    Trials :: non_neg_integer(),
    TimeoutMs :: timeout()
) ->
    ok | {error, Reason :: term()}.
is_device_ready(_Resource, _Addr, _Trials, _TimeoutMs) ->
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
    MRef = monitor(process, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} ->
            demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {server_died, Reason}}
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
        case ?MODULE:master_transmit(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, undefined, end_transmission) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Count}) ->
    Result = ?MODULE:master_receive(Resource, Address, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Register, Count}) ->
    MemAddSize = mem_add_size(Register),
    Result = ?MODULE:mem_read(Resource, Address, Register, MemAddSize, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Data}) ->
    Result =
        case ?MODULE:master_transmit(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Register, Data}) ->
    MemAddSize = mem_add_size(Register),
    Result =
        case ?MODULE:mem_write(Resource, Address, Register, MemAddSize, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(_Resource, _SendTimeoutMs, TxState, _Unknown) ->
    {reply, {error, badarg}, TxState}.

%% @private
mem_add_size(MemAddr) when MemAddr > 16#FF -> 16;
mem_add_size(_MemAddr) -> 8.
