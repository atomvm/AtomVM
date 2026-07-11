%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc AtomVM SPI interface for Zephyr.
%%
%% SPI signal pins are configured by devicetree. `peripheral' selects a
%% controller by index or device name. Device `cs' values are optional GPIO
%% pins used as software chip selects.
-module(spi).

-behaviour(spi_hal).

-export([open/1, close/1, read_at/4, write_at/5, write/3, write_read/3]).
-export([init/1, deinit/1, transceive/4]).

-type spi() :: pid().
-type spi_resource() :: reference().
-type device_name() :: atom().
-type transaction() :: map().

-define(DEFAULT_CLOCK_SPEED_HZ, 1000000).
-define(DEFAULT_MODE, 0).
-define(DEFAULT_ADDRESS_LEN_BITS, 8).
-define(DEFAULT_COMMAND_LEN_BITS, 0).

-spec open(list() | map()) -> spi() | {error, term()}.
open(Params) ->
    BusConfig = get_value(bus_config, Params, []),
    Peripheral = get_value(peripheral, BusConfig, undefined),
    InitOptions = case Peripheral of undefined -> []; _ -> [{peripheral, Peripheral}] end,
    case ?MODULE:init(InitOptions) of
        {ok, Resource} ->
            Devices = setup_devices(get_value(device_config, Params, [])),
            spawn_link(fun() -> loop(Resource, Devices) end);
        {error, _} = Error -> Error
    end.

-spec close(spi()) -> ok | {error, term()}.
close(Pid) -> call(Pid, close).

-spec read_at(spi(), device_name(), non_neg_integer(), non_neg_integer()) ->
    {ok, integer()} | {error, term()}.
read_at(Pid, Device, Address, Len) -> call(Pid, {read_at, Device, Address, Len}).

-spec write_at(spi(), device_name(), non_neg_integer(), non_neg_integer(), integer()) ->
    {ok, integer()} | {error, term()}.
write_at(Pid, Device, Address, Len, Data) ->
    call(Pid, {write_at, Device, Address bor 16#80, Len, Data}).

-spec write(spi(), device_name(), transaction()) -> ok | {error, term()}.
write(Pid, Device, Transaction) -> call(Pid, {write, Device, Transaction}).

-spec write_read(spi(), device_name(), transaction()) -> {ok, binary()} | {error, term()}.
write_read(Pid, Device, Transaction) -> call(Pid, {write_read, Device, Transaction}).

-spec init(list()) -> {ok, spi_resource()} | {error, term()}.
init(_Options) -> erlang:nif_error(undefined).

-spec deinit(spi_resource()) -> ok.
deinit(_Resource) -> erlang:nif_error(undefined).

-spec transceive(spi_resource(), pos_integer(), 0..3, binary()) ->
    {ok, binary()} | {error, term()}.
transceive(_Resource, _Frequency, _Mode, _Data) -> erlang:nif_error(undefined).

call(Pid, Request) ->
    MRef = monitor(process, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} -> demonitor(MRef, [flush]), Reply;
        {'DOWN', MRef, process, Pid, Reason} -> {error, {server_died, Reason}}
    end.

get_value(Key, Map, Default) when is_map(Map) -> maps:get(Key, Map, Default);
get_value(Key, List, Default) when is_list(List) -> proplists:get_value(Key, List, Default).

setup_devices(Config) when is_map(Config) -> setup_devices(maps:to_list(Config));
setup_devices(Config) when is_list(Config) -> lists:foldl(fun setup_device/2, #{}, Config).

setup_device({Name, Config}, Acc) when is_atom(Name) ->
    CS = get_value(cs, Config, undefined),
    case CS of
        undefined -> ok;
        _ ->
            ok = gpio:init(CS),
            ok = gpio:set_pin_mode(CS, output),
            ok = gpio:digital_write(CS, high)
    end,
    Acc#{Name => #{
        cs => CS,
        clock_speed_hz => get_value(clock_speed_hz, Config, ?DEFAULT_CLOCK_SPEED_HZ),
        mode => get_value(mode, Config, ?DEFAULT_MODE),
        address_len_bits => get_value(address_len_bits, Config, ?DEFAULT_ADDRESS_LEN_BITS),
        command_len_bits => get_value(command_len_bits, Config, ?DEFAULT_COMMAND_LEN_BITS)
    }}.

loop(Resource, Devices) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, Devices, Request) of
                {Reply, stop} -> From ! {Ref, Reply};
                Reply -> From ! {Ref, Reply}, loop(Resource, Devices)
            end
    end.

handle_request(Resource, _Devices, close) -> {?MODULE:deinit(Resource), stop};
handle_request(_Resource, Devices, {Tag, Device, _}) when
    (Tag =:= write orelse Tag =:= write_read), not is_map_key(Device, Devices)
-> {error, {unknown_device, Device}};
handle_request(_Resource, Devices, {read_at, Device, _, _}) when not is_map_key(Device, Devices) ->
    {error, {unknown_device, Device}};
handle_request(_Resource, Devices, {write_at, Device, _, _, _}) when not is_map_key(Device, Devices) ->
    {error, {unknown_device, Device}};
handle_request(Resource, Devices, {read_at, Device, Address, LenBits}) ->
    Info = maps:get(Device, Devices),
    AddressBits = maps:get(address_len_bits, Info),
    Prefix = <<Address:AddressBits/big-unsigned>>,
    ReadBytes = (LenBits + 7) div 8,
    case transfer(Resource, Info, <<Prefix/binary, 0:(ReadBytes * 8)>>) of
        {ok, <<_:AddressBits, Value:LenBits/big-unsigned, _/bitstring>>} -> {ok, Value};
        {error, _} = Error -> Error
    end;
handle_request(Resource, Devices, {write_at, Device, Address, LenBits, Data}) ->
    Info = maps:get(Device, Devices),
    AddressBits = maps:get(address_len_bits, Info),
    case transfer(Resource, Info, <<Address:AddressBits/big-unsigned, Data:LenBits/big-unsigned>>) of
        {ok, <<_:AddressBits, Value:LenBits/big-unsigned, _/bitstring>>} -> {ok, Value};
        {error, _} = Error -> Error
    end;
handle_request(Resource, Devices, {write, Device, Transaction}) ->
    Info = maps:get(Device, Devices),
    Tx = build_tx(Info, Transaction, false),
    case transfer(Resource, Info, Tx) of {ok, _} -> ok; Error -> Error end;
handle_request(Resource, Devices, {write_read, Device, Transaction}) ->
    Info = maps:get(Device, Devices),
    Prefix = build_prefix(Info, Transaction),
    WriteData = maps:get(write_data, Transaction, <<>>),
    WriteBytes = (maps:get(write_bits, Transaction, byte_size(WriteData) * 8) + 7) div 8,
    ReadBytes = (maps:get(read_bits, Transaction, WriteBytes * 8) + 7) div 8,
    DataBytes = max(WriteBytes, ReadBytes),
    Data = pad_binary(binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))), DataBytes),
    case transfer(Resource, Info, <<Prefix/binary, Data/binary>>) of
        {ok, Rx} -> {ok, binary:part(Rx, byte_size(Prefix), ReadBytes)};
        {error, _} = Error -> Error
    end.

transfer(Resource, Info, Tx) ->
    select(Info),
    Result = ?MODULE:transceive(Resource, maps:get(clock_speed_hz, Info), maps:get(mode, Info), Tx),
    deselect(Info),
    Result.

select(#{cs := undefined}) -> ok;
select(#{cs := CS}) -> gpio:digital_write(CS, low).
deselect(#{cs := undefined}) -> ok;
deselect(#{cs := CS}) -> gpio:digital_write(CS, high).

build_tx(Info, Transaction, _Read) ->
    Prefix = build_prefix(Info, Transaction),
    Data = maps:get(write_data, Transaction, <<>>),
    Bytes = (maps:get(write_bits, Transaction, byte_size(Data) * 8) + 7) div 8,
    <<Prefix/binary, (binary:part(Data, 0, min(Bytes, byte_size(Data))))/binary>>.

build_prefix(Info, Transaction) ->
    CommandBits = maps:get(command_len_bits, Info),
    AddressBits = maps:get(address_len_bits, Info),
    Command = maps:get(command, Transaction, 0),
    Address = maps:get(address, Transaction, 0),
    <<Command:CommandBits/big-unsigned, Address:AddressBits/big-unsigned>>.

pad_binary(Bin, Len) when byte_size(Bin) >= Len -> Bin;
pad_binary(Bin, Len) -> <<Bin/binary, 0:((Len - byte_size(Bin)) * 8)>>.
