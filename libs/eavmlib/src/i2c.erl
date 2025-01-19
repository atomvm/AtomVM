%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Davide Bettio <davide@uninstall.it>
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
%% @doc AtomVM I2c interface
%%
%% This module provides and interface into the AtomVM I2C driver.
%%
%% Use this module to communicate with devices connected to your ESP32
%% device via the 2-wire I2C interface.
%%
%% Using this interface, you can read or write data to an I2C device
%% at a given I2C address.  In addition, you may read from or write to
%% specific registers on the I2C device.
%% @end
%%-----------------------------------------------------------------------------
-module(i2c).
-export([
    open/1,
    close/1,
    begin_transmission/2,
    write_byte/2,
    end_transmission/1,
    read_bytes/3, read_bytes/4,
    write_bytes/2, write_bytes/3, write_bytes/4
]).

%% Nif implementation
-export([
    open_nif/1,
    close_nif/1,
    read_bytes_nif/2,
    write_bytes_nif/2,
    begin_transmission_nif/2,
    end_transmission_nif/1,
    enqueue_write_bytes_nif/2
]).

-define(I2C_RSRC, {'$i2c', _Resource, _Ref}).

-type pin() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type peripheral() :: string() | binary().
-type param() ::
    {scl, pin()}
    | {sda, pin()}
    | {clock_speed_hz, freq_hz()}
    | {peripheral, peripheral()}
    | {use_nif, boolean()}
    | {send_timeout_ms, non_neg_integer()}.
-type params() :: [param()].
-type i2c() :: port() | {'$i2c', term(), reference()}.
-type address() :: non_neg_integer().
-type register() :: non_neg_integer().

-export_type([
    i2c/0, address/0, register/0
]).

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns process id of the driver.
%% @doc     Open a connection to the I2C driver
%%
%%          This function will open a connection to the I2C driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> i2c().
open(Params) ->
    case proplists:get_value(use_nif, Params) of
        true ->
            ?MODULE:open_nif(migrate_config(Params));
        _ ->
            open_port({spawn, "i2c"}, migrate_config(Params))
    end.

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @returns `ok' atom
%% @doc     Closes the connection to the I2C driver
%%
%%          This function will close the connection to the I2C driver and
%%          free any resources in use by it.
%% @end
%%-----------------------------------------------------------------------------
-spec close(I2C :: i2c()) -> ok | {error, Reason :: term()}.
close(?I2C_RSRC = I2C) ->
    ?MODULE:close_nif(I2C);
close(I2C) ->
    port:call(I2C, {close}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @returns `ok' or `{error, Reason}'
%% @doc     Begin a transmission of I2C commands
%%
%%          This command is typically followed by one or more calls to
%%          `write_byte/2' and then a call to `end_transmission/1'
%% @end
%%-----------------------------------------------------------------------------
-spec begin_transmission(I2C :: i2c(), Address :: address()) -> ok | {error, Reason :: term()}.
begin_transmission(?I2C_RSRC = I2C, Address) ->
    ?MODULE:begin_transmission_nif(I2C, Address);
begin_transmission(I2C, Address) ->
    port:call(I2C, {begin_transmission, Address}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a byte to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_byte(I2C :: i2c(), Byte :: byte()) -> ok | {error, Reason :: term()}.
write_byte(?I2C_RSRC = I2C, Byte) when is_integer(Byte) andalso 0 =< Byte andalso Byte =< 255 ->
    write_bytes(I2C, <<Byte:8>>);
write_byte(I2C, Byte) when is_integer(Byte) andalso 0 =< Byte andalso Byte =< 255 ->
    port:call(I2C, {write_byte, Byte}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Bytes value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a sequence of bytes to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2C :: i2c(), Bytes :: binary()) -> ok | {error, Reason :: term()}.
write_bytes(?I2C_RSRC = I2C, Bytes) ->
    ?MODULE:enqueue_write_bytes_nif(I2C, Bytes);
write_bytes(I2C, Bytes) ->
    port:call(I2C, {write_bytes, Bytes}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @returns `ok' or `{error, Reason}'
%% @doc     End a transmission of I2C commands
%%
%%          This command is typically preceded by a call to `begin_transmission/2'
%%          and one or more calls to `write_byte/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec end_transmission(I2C :: i2c()) -> ok | {error, Reason :: term()}.
end_transmission(?I2C_RSRC = I2C) ->
    ?MODULE:end_transmission_nif(I2C);
end_transmission(I2C) ->
    port:call(I2C, {end_transmission}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Count The number of bytes to read
%% @returns `{ok, Data}' which includes the read binary data or `{error, Reason}'
%% @doc     Read a block of bytes from the I2C device.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, Data :: binary()} | {error, Reason :: term()}.
read_bytes(?I2C_RSRC = I2C, Address, Count) ->
    ?MODULE:read_bytes_nif(I2C, {Address, Count});
read_bytes(I2C, Address, Count) ->
    port:call(I2C, {read_bytes, Address, Count}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Register The register address in the device from which to read data
%% @param   Count The number of bytes to read
%% @returns `{ok, Data}' which includes the read binary data or `{error, Reason}'
%% @doc     Read a block of bytes from the I2C device starting at a specified
%%          register address
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), Count :: non_neg_integer()
) -> {ok, binary()} | {error, Reason :: term()}.
read_bytes(?I2C_RSRC = I2C, Address, Register, Count) ->
    ?MODULE:read_bytes_nif(I2C, {Address, Count, Register});
read_bytes(I2C, Address, Register, Count) ->
    port:call(I2C, {read_bytes, Address, Count, Register}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   BinOrInt The binary or byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a block of bytes to the I2C device.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2C :: i2c(), Address :: address(), BinOrInt :: binary() | byte()) ->
    ok | {error, Reason :: term()}.
write_bytes(?I2C_RSRC = I2C, Address, BinOrInt) ->
    ?MODULE:write_bytes_nif(I2C, {Address, BinOrInt});
write_bytes(I2C, Address, BinOrInt) ->
    port:call(I2C, {write_bytes, Address, BinOrInt}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Register The register address in the device to which to write data
%% @param   BinOrInt The binary or byte value to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write a block of bytes to the I2C device starting at a specified
%%          register address.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(
    I2C :: i2c(), Address :: address(), Register :: register(), BinOrInt :: binary() | integer()
) -> ok | {error, Reason :: term()}.
write_bytes(?I2C_RSRC = I2C, Address, Register, BinOrInt) ->
    ?MODULE:write_bytes_nif(I2C, {Address, BinOrInt, Register});
write_bytes(I2C, Address, Register, BinOrInt) ->
    port:call(I2C, {write_bytes, Address, BinOrInt, Register}).

%%
%% Internal operations
%%

%% @private
migrate_config([]) ->
    [];
migrate_config([{K, V} | T]) ->
    NewK = rename_key(K),
    warn_deprecated(K, NewK),
    NewV = migrate_value(NewK, V),
    [{NewK, NewV} | migrate_config(T)].

%% @private
migrate_value(peripheral, Peripheral) ->
    validate_peripheral(Peripheral);
migrate_value(_K, V) ->
    V.

%% @private
rename_key(Key) ->
    case Key of
        scl_io_num -> scl;
        sda_io_num -> sda;
        i2c_clock_hz -> clock_speed_hz;
        i2c_num -> peripheral;
        Any -> Any
    end.

%% @private
warn_deprecated(Key, Key) ->
    ok;
warn_deprecated(OldKey, NewKey) ->
    io:format("I2C: found deprecated ~p, use ~p instead!!!~n", [OldKey, NewKey]).

%% @private
validate_peripheral(I) when is_integer(I) ->
    io:format("I2C: deprecated integer peripheral is used.~n"),
    I;
validate_peripheral([$i, $2, $c | N] = Value) ->
    try list_to_integer(N) of
        % Internally integers are still used
        % TODO: change this as soon as ESP32 code is reworked
        I -> I
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(<<"i2c", N/binary>> = Value) ->
    try binary_to_integer(N) of
        I -> I
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(Value) ->
    throw({bardarg, {peripheral, Value}}).

%%
%% Nif implementation
%%

%% @hidden
open_nif(_Params) ->
    erlang:nif_error(undefined).

%% @hidden
close_nif(_I2C) ->
    erlang:nif_error(undefined).

%% @hidden
read_bytes_nif(_I2C, _Request) ->
    erlang:nif_error(undefined).

%% @hidden
write_bytes_nif(_I2C, _Request) ->
    erlang:nif_error(undefined).

%% @hidden
begin_transmission_nif(_I2C, _Address) ->
    erlang:nif_error(undefined).

%% @hidden
enqueue_write_bytes_nif(_I2C, _Bytes) ->
    erlang:nif_error(undefined).

%% @hidden
end_transmission_nif(_I2C) ->
    erlang:nif_error(undefined).
