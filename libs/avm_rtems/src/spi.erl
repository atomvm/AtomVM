%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
%% @doc SPI implementation for RTEMS using Linux-compatible SPI ioctls.
%%
%% This module implements the {@link spi_hal} behaviour. Transfers use
%% `SPI_IOC_MESSAGE' on a registered `/dev/spi-*' node. The i.MX7 BSP registers
%% the controller on first `open/1' via `spi_bus_register_imx'; SPARC erc32 and
%% ARM RealView have no SPI controller and `init/1' returns `{error, enotsup}'.
%%
%% Options:
%% <ul>
%% <li>`{bus_config, BusOpts}' - bus options:
%%   <ul>
%%     <li>`{peripheral, Path}' - device path (default `"/dev/spi-0"')</li>
%%     <li>`{fdt_alias, Alias}' - FDT alias such as `"spi0"' (imx7 only)</li>
%%   </ul>
%% </li>
%% <li>`{device_config, DeviceList}' - list of `{DeviceName, DeviceOpts}':
%%   <ul>
%%     <li>`{cs, Pin}' - Chip Select pin (`{Bank, Pin}' or FDT map)</li>
%%     <li>`{clock_speed_hz, Hz}' - clock speed (default: 1000000)</li>
%%     <li>`{mode, 0..3}' - SPI mode (default: 0)</li>
%%     <li>`{address_len_bits, Bits}' - address width (default: 8)</li>
%%     <li>`{command_len_bits, Bits}' - command width (default: 0)</li>
%%   </ul>
%% </li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(spi).

-behaviour(spi_hal).

%% High-level API (spi_hal behaviour)
-export([
    open/1,
    close/1,
    read_at/4,
    write_at/5,
    write/3,
    write_read/3
]).

%% Low-level NIF API
-export([
    init/1,
    deinit/1,
    apply_device_config/2,
    write_blocking/2,
    read_blocking/2,
    read_blocking/3,
    write_read_blocking/2
]).

-define(DEFAULT_CLOCK_SPEED_HZ, 1000000).
-define(DEFAULT_MODE, 0).
-define(DEFAULT_ADDRESS_LEN_BITS, 8).
-define(DEFAULT_COMMAND_LEN_BITS, 0).
-define(DEFAULT_PERIPHERAL, "/dev/spi-0").
-define(DEFAULT_FDT_ALIAS, "spi0").

-type spi_resource() :: reference().
-type spi() :: pid().
-type device_name() :: atom().
-type address() :: non_neg_integer().
-type transaction() :: spi_hal:transaction().

-export_type([spi/0, spi_resource/0, device_name/0, address/0, transaction/0]).

%% ---------------------------------------------------------------------------
%% High-level API
%% ---------------------------------------------------------------------------

-spec open(Params :: [term()] | map()) -> spi().
open(Params) ->
    BusConfig = get_value(bus_config, Params, []),
    Peripheral = get_value(
        peripheral, BusConfig, get_value(peripheral, Params, ?DEFAULT_PERIPHERAL)
    ),
    FdtAlias = get_value(fdt_alias, BusConfig, get_value(fdt_alias, Params, ?DEFAULT_FDT_ALIAS)),
    InitOpts = [{peripheral, Peripheral}, {fdt_alias, FdtAlias}],
    case ?MODULE:init(InitOpts) of
        {ok, Resource} ->
            RawDeviceConfigs = get_value(device_config, Params, []),
            Devices = setup_devices(RawDeviceConfigs),
            erlang:spawn_opt(fun() -> loop(Resource, Devices) end, [link]);
        {error, Reason} ->
            error(Reason)
    end.

-spec close(SPI :: spi()) -> ok.
close(Pid) ->
    call(Pid, close).

-spec read_at(
    SPI :: spi(), DeviceName :: device_name(), Address :: address(), Len :: non_neg_integer()
) ->
    {ok, integer()} | {error, term()}.
read_at(Pid, DeviceName, Address, Len) ->
    call(Pid, {read_at, DeviceName, Address, Len}).

-spec write_at(
    SPI :: spi(),
    DeviceName :: device_name(),
    Address :: address(),
    Len :: non_neg_integer(),
    Data :: integer()
) ->
    {ok, integer()} | {error, term()}.
write_at(Pid, DeviceName, Address, Len, Data) ->
    call(Pid, {write_at, DeviceName, Address bor 16#80, Len, Data}).

-spec write(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    ok | {error, term()}.
write(Pid, DeviceName, Transaction) ->
    call(Pid, {write, DeviceName, Transaction}).

-spec write_read(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    {ok, binary()} | {error, term()}.
write_read(Pid, DeviceName, Transaction) ->
    call(Pid, {write_read, DeviceName, Transaction}).

%% ---------------------------------------------------------------------------
%% Low-level NIFs
%% ---------------------------------------------------------------------------

-spec init(Opts :: [{atom(), term()}]) -> {ok, spi_resource()} | {error, term()}.
init(_Opts) ->
    erlang:nif_error(undefined).

-spec deinit(Resource :: spi_resource()) -> ok | {error, term()}.
deinit(_Resource) ->
    erlang:nif_error(undefined).

-spec apply_device_config(Resource :: spi_resource(), Config :: map()) -> ok | {error, term()}.
apply_device_config(_Resource, _Config) ->
    erlang:nif_error(undefined).

-spec write_blocking(Resource :: spi_resource(), Data :: binary()) ->
    non_neg_integer() | {error, term()}.
write_blocking(_Resource, _Data) ->
    erlang:nif_error(undefined).

-spec read_blocking(Resource :: spi_resource(), Len :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
read_blocking(Resource, Len) ->
    read_blocking(Resource, 0, Len).

-spec read_blocking(Resource :: spi_resource(), RepeatedByte :: byte(), Len :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
read_blocking(_Resource, _RepeatedByte, _Len) ->
    erlang:nif_error(undefined).

-spec write_read_blocking(Resource :: spi_resource(), Data :: binary()) ->
    {ok, binary()} | {error, term()}.
write_read_blocking(_Resource, _Data) ->
    erlang:nif_error(undefined).

%% ---------------------------------------------------------------------------
%% Internal loop and helpers
%% ---------------------------------------------------------------------------

call(Pid, Request) ->
    MRef = erlang:monitor(process, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {server_died, Reason}}
    end.

loop(Resource, Devices) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, Devices, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply} ->
                    From ! {Ref, Reply},
                    loop(Resource, Devices)
            end
    end.

handle_request(Resource, Devices, close) ->
    deselect_all(Devices),
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(_Resource, Devices, {Tag, DeviceName, _}) when
    (Tag =:= write orelse Tag =:= write_read), not is_map_key(DeviceName, Devices)
->
    {reply, {error, {unknown_device, DeviceName}}};
handle_request(_Resource, Devices, {Tag, DeviceName, _, _}) when
    Tag =:= read_at, not is_map_key(DeviceName, Devices)
->
    {reply, {error, {unknown_device, DeviceName}}};
handle_request(_Resource, Devices, {Tag, DeviceName, _, _, _}) when
    Tag =:= write_at, not is_map_key(DeviceName, Devices)
->
    {reply, {error, {unknown_device, DeviceName}}};
handle_request(Resource, Devices, {read_at, DeviceName, Address, LenBits}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
    AddrBin = <<Address:AddressLenBits/big-unsigned>>,
    AddrLen = byte_size(AddrBin),
    case ?MODULE:write_blocking(Resource, AddrBin) of
        AddrLen ->
            LenBytes = (LenBits + 7) div 8,
            case ?MODULE:read_blocking(Resource, 0, LenBytes) of
                {ok, Data} ->
                    deselect_device(DeviceInfo),
                    <<Value:LenBits/big-unsigned, _/bitstring>> = Data,
                    {reply, {ok, Value}};
                {error, _} = Error ->
                    deselect_device(DeviceInfo),
                    {reply, Error}
            end;
        {error, _} = Error ->
            deselect_device(DeviceInfo),
            {reply, Error}
    end;
handle_request(Resource, Devices, {write_at, DeviceName, Address, LenBits, Data}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
    TxBin = <<Address:AddressLenBits/big-unsigned, Data:LenBits/big-unsigned>>,
    case ?MODULE:write_read_blocking(Resource, TxBin) of
        {ok, RxBin} ->
            deselect_device(DeviceInfo),
            <<_:AddressLenBits, Value:LenBits/big-unsigned>> = RxBin,
            {reply, {ok, Value}};
        {error, _} = Error ->
            deselect_device(DeviceInfo),
            {reply, Error}
    end;
handle_request(Resource, Devices, {write, DeviceName, Transaction}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
    WriteData = maps:get(write_data, Transaction, <<>>),
    WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
    WriteBytes = (WriteBits + 7) div 8,
    TxPayload = binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))),
    TxBin = <<PrefixBin/binary, TxPayload/binary>>,
    Result =
        case byte_size(TxBin) of
            0 ->
                ok;
            Len ->
                case ?MODULE:write_blocking(Resource, TxBin) of
                    Len -> ok;
                    {error, _} = Err -> Err
                end
        end,
    deselect_device(DeviceInfo),
    {reply, Result};
handle_request(Resource, Devices, {write_read, DeviceName, Transaction}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
    PrefixLen = byte_size(PrefixBin),
    PrefixResult =
        case PrefixLen of
            0 ->
                ok;
            _ ->
                case ?MODULE:write_blocking(Resource, PrefixBin) of
                    PrefixLen -> ok;
                    {error, _} = PrefixErr -> PrefixErr
                end
        end,
    case PrefixResult of
        ok ->
            WriteData = maps:get(write_data, Transaction, <<>>),
            WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
            WriteBytes = (WriteBits + 7) div 8,
            ReadBits = maps:get(read_bits, Transaction, WriteBits),
            ReadBytes = (ReadBits + 7) div 8,
            DataLen = max(WriteBytes, ReadBytes),
            TxPayload = binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))),
            TxData = pad_binary(TxPayload, DataLen),
            Result =
                case ?MODULE:write_read_blocking(Resource, TxData) of
                    {ok, RxBin} ->
                        {ok, binary:part(RxBin, 0, ReadBytes)};
                    {error, _} = Error ->
                        Error
                end,
            deselect_device(DeviceInfo),
            {reply, Result};
        {error, _} = Error ->
            deselect_device(DeviceInfo),
            {reply, Error}
    end.

setup_devices(DeviceConfigMap) when is_map(DeviceConfigMap) ->
    setup_devices(maps:to_list(DeviceConfigMap));
setup_devices(DeviceConfigList) when is_list(DeviceConfigList) ->
    lists:foldl(fun setup_device/2, #{}, DeviceConfigList).

setup_device({Name, Config}, Acc) ->
    CS = get_value(cs, Config),
    ClockSpeedHz = get_value(clock_speed_hz, Config, ?DEFAULT_CLOCK_SPEED_HZ),
    Mode = get_value(mode, Config, ?DEFAULT_MODE),
    AddressLenBits = get_value(address_len_bits, Config, ?DEFAULT_ADDRESS_LEN_BITS),
    CommandLenBits = get_value(command_len_bits, Config, ?DEFAULT_COMMAND_LEN_BITS),
    case CS of
        undefined ->
            ok;
        _ ->
            _ = gpio:set_pin_mode(CS, output),
            _ = gpio:digital_write(CS, high)
    end,
    DeviceInfo = #{
        cs => CS,
        clock_speed_hz => ClockSpeedHz,
        mode => Mode,
        address_len_bits => AddressLenBits,
        command_len_bits => CommandLenBits
    },
    Acc#{Name => DeviceInfo}.

select_device(Resource, DeviceInfo) ->
    Config = #{
        clock_speed_hz => maps:get(clock_speed_hz, DeviceInfo),
        mode => maps:get(mode, DeviceInfo),
        bits_per_word => 8
    },
    _ = ?MODULE:apply_device_config(Resource, Config),
    case maps:get(cs, DeviceInfo) of
        undefined -> ok;
        CS -> _ = gpio:digital_write(CS, low)
    end.

deselect_device(DeviceInfo) ->
    case maps:get(cs, DeviceInfo) of
        undefined -> ok;
        CS -> _ = gpio:digital_write(CS, high)
    end.

deselect_all(Devices) ->
    maps:foreach(fun(_Name, DeviceInfo) -> deselect_device(DeviceInfo) end, Devices).

build_tx_prefix(DeviceInfo, Transaction) ->
    CommandLenBits = maps:get(command_len_bits, DeviceInfo),
    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
    Command = maps:get(command, Transaction, 0),
    Address = maps:get(address, Transaction, 0),
    case {CommandLenBits, AddressLenBits} of
        {0, 0} -> <<>>;
        {0, _} -> <<Address:AddressLenBits/big-unsigned>>;
        {_, 0} -> <<Command:CommandLenBits/big-unsigned>>;
        {_, _} -> <<Command:CommandLenBits/big-unsigned, Address:AddressLenBits/big-unsigned>>
    end.

pad_binary(Bin, Len) when byte_size(Bin) >= Len ->
    Bin;
pad_binary(Bin, Len) ->
    PadLen = Len - byte_size(Bin),
    <<Bin/binary, 0:(PadLen * 8)>>.

get_value(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_value(Key, List, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default).

get_value(Key, MapOrList) ->
    get_value(Key, MapOrList, undefined).
