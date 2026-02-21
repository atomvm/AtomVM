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
%% @doc AtomVM SPI interface for RP2 (Pico)
%%
%% This module provides an interface to the SPI hardware on RP2 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API</b>
%% {@link init/2}, {@link deinit/1}, {@link set_baudrate/2},
%% {@link set_format/4}, {@link write_blocking/2}, {@link read_blocking/3},
%% {@link write_read_blocking/2}.
%% These operate on a bare resource reference returned by {@link init/2}.
%% Pin muxing must be done separately via `gpio:set_function/2'.
%%
%% <b>High-level API (`spi_hal' behavior)</b>
%% {@link open/1}, {@link close/1}, {@link read_at/4}, {@link write_at/5},
%% {@link write/3}, {@link write_read/3}.
%% {@link open/1} handles pin setup and CS management automatically.
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

%% Low-level API (Pico SDK)
-export([
    init/2,
    deinit/1,
    set_baudrate/2,
    set_format/4,
    write_blocking/2,
    read_blocking/3,
    write_read_blocking/2
]).

-type freq_hz() :: non_neg_integer().
-type peripheral() :: 0 | 1.
-type spi_resource() :: reference().
-type spi() :: pid().
-type device_name() :: atom().
-type address() :: non_neg_integer().
-type transaction() :: #{
    command => integer(),
    address => non_neg_integer(),
    write_data => binary(),
    write_bits => non_neg_integer(),
    read_bits => non_neg_integer()
}.

-export_type([spi/0, spi_resource/0, device_name/0, address/0, transaction/0]).

-define(DEFAULT_CLOCK_SPEED_HZ, 1000000).
-define(DEFAULT_MODE, 0).
-define(DEFAULT_ADDRESS_LEN_BITS, 8).
-define(DEFAULT_COMMAND_LEN_BITS, 0).
-define(DEFAULT_PERIPHERAL, 0).

%% ---------------------------------------------------------------------------
%% High-level API (spi_hal behaviour)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns SPI handle (pid)
%% @doc     Open a connection to the SPI driver
%%
%%          This function configures the GPIO pins for SPI function,
%%          initializes the SPI peripheral, and sets up CS pins for
%%          software chip-select management.
%%
%%          Parameters use the same format as the ESP32 SPI driver:
%%          <ul>
%%              <li>`{bus_config, BusConfig}' - bus configuration (required)</li>
%%              <li>`{device_config, DeviceConfigs}' - device configurations</li>
%%          </ul>
%%
%%          Bus configuration:
%%          <ul>
%%              <li>`{sclk, Pin}' - the SCLK pin number (required)</li>
%%              <li>`{mosi, Pin}' - the MOSI pin number</li>
%%              <li>`{miso, Pin}' - the MISO pin number</li>
%%              <li>`{peripheral, 0 | 1}' - the SPI peripheral to use (default: 0)</li>
%%          </ul>
%%
%%          Device configuration (keyed by device name atom):
%%          <ul>
%%              <li>`{cs, Pin}' - the CS pin number</li>
%%              <li>`{clock_speed_hz, Hz}' - clock speed (default: 1000000)</li>
%%              <li>`{mode, 0..3}' - SPI mode (default: 0)</li>
%%              <li>`{address_len_bits, Bits}' - address width (default: 8)</li>
%%              <li>`{command_len_bits, Bits}' - command width (default: 0)</li>
%%          </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: [{atom(), term()}]) -> spi().
open(Params) ->
    BusConfig = get_value(bus_config, Params),
    DeviceConfigList = get_value(device_config, Params, []),
    SCLK = get_value(sclk, BusConfig),
    MOSI = get_value(mosi, BusConfig, undefined),
    MISO = get_value(miso, BusConfig, undefined),
    Peripheral = get_value(peripheral, BusConfig, ?DEFAULT_PERIPHERAL),
    gpio:set_function(SCLK, spi),
    maybe_set_spi_function(MOSI),
    maybe_set_spi_function(MISO),
    {ok, {_ActualBaud, Resource}} = ?MODULE:init(Peripheral, ?DEFAULT_CLOCK_SPEED_HZ),
    Devices = setup_devices(DeviceConfigList),
    spawn_link(fun() -> loop(Resource, Devices) end).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI handle created via `open/1'
%% @returns `ok'
%% @doc     Close the SPI driver and free resources.
%% @end
%%-----------------------------------------------------------------------------
-spec close(SPI :: spi()) -> ok.
close(Pid) ->
    call(Pid, close).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI handle created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Address SPI address from which to read
%% @param   Len number of bits to read
%% @returns `{ok, Value}' or `{error, Reason}'
%% @doc     Read a value from an address on the device.
%%
%%          Sends the address (address_len_bits wide), then reads Len bits.
%%          The read value is returned as a big-endian integer.
%% @end
%%-----------------------------------------------------------------------------
-spec read_at(
    SPI :: spi(), DeviceName :: device_name(), Address :: address(), Len :: non_neg_integer()
) ->
    {ok, integer()} | {error, term()}.
read_at(Pid, DeviceName, Address, Len) ->
    call(Pid, {read_at, DeviceName, Address, Len}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI handle created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Address SPI address to which to write
%% @param   Len number of bits to transfer
%% @param   Data value to write
%% @returns `{ok, Value}' or `{error, Reason}'
%% @doc     Write a value to an address on the device.
%%
%%          Sends the address with bit 7 set (write flag), followed by the
%%          data. Returns the value read back during the data phase.
%% @end
%%-----------------------------------------------------------------------------
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

%%-----------------------------------------------------------------------------
%% @param   SPI SPI handle created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Transaction transaction map
%% @returns `ok' or `{error, Reason}'
%% @doc     Write data to the SPI device using a transaction.
%%
%%          The transaction map may contain: `command', `address',
%%          `write_data', and `write_bits'.
%% @end
%%-----------------------------------------------------------------------------
-spec write(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    ok | {error, term()}.
write(Pid, DeviceName, Transaction) ->
    call(Pid, {write, DeviceName, Transaction}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI handle created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Transaction transaction map
%% @returns `{ok, ReadData}' or `{error, Reason}'
%% @doc     Write and simultaneously read from the SPI device.
%%
%%          The transaction map may contain: `command', `address',
%%          `write_data', `write_bits', and `read_bits'.
%%          Returns the first `ceil(read_bits / 8)' bytes read during the
%%          data phase.
%% @end
%%-----------------------------------------------------------------------------
-spec write_read(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    {ok, binary()} | {error, term()}.
write_read(Pid, DeviceName, Transaction) ->
    call(Pid, {write_read, DeviceName, Transaction}).

%% ---------------------------------------------------------------------------
%% Low-level API (Pico SDK)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Peripheral SPI peripheral number (0 or 1)
%% @param   Baudrate Baudrate in Hz
%% @returns `{ok, {ActualBaudrate, Resource}}'
%% @doc     Initialize the SPI HW block.
%%
%%          Pin muxing must be done separately via `gpio:set_function/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Peripheral :: peripheral(), Baudrate :: freq_hz()) ->
    {ok, {ActualBaudrate :: freq_hz(), Resource :: spi_resource()}}.
init(_Peripheral, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @returns `ok'
%% @doc     Disable the SPI HW block.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: spi_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   Baudrate Baudrate in Hz
%% @returns `{ok, ActualBaudrate}'
%% @doc     Set SPI baudrate.
%% @end
%%-----------------------------------------------------------------------------
-spec set_baudrate(Resource :: spi_resource(), Baudrate :: freq_hz()) ->
    {ok, ActualBaudrate :: freq_hz()}.
set_baudrate(_Resource, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   DataBits Number of data bits per transfer (4..16)
%% @param   CPOL Clock polarity (0 or 1)
%% @param   CPHA Clock phase (0 or 1)
%% @returns `ok'
%% @doc     Set SPI format.
%%
%%          SPI mode mapping: mode 0 = CPOL 0, CPHA 0; mode 1 = CPOL 0,
%%          CPHA 1; mode 2 = CPOL 1, CPHA 0; mode 3 = CPOL 1, CPHA 1.
%%          Data order is always MSB first.
%% @end
%%-----------------------------------------------------------------------------
-spec set_format(Resource :: spi_resource(), DataBits :: 4..16, CPOL :: 0 | 1, CPHA :: 0 | 1) -> ok.
set_format(_Resource, _DataBits, _CPOL, _CPHA) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   Data Binary data to write
%% @returns Number of bytes written
%% @doc     Write to SPI, blocking.
%% @end
%%-----------------------------------------------------------------------------
-spec write_blocking(Resource :: spi_resource(), Data :: binary()) -> non_neg_integer().
write_blocking(_Resource, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   RepeatedTxData Byte value to send on TX while reading
%% @param   Count Number of bytes to read
%% @returns `{ok, Data}'
%% @doc     Read from SPI, blocking.
%%
%%          `RepeatedTxData' is repeatedly sent on TX while reading data.
%% @end
%%-----------------------------------------------------------------------------
-spec read_blocking(
    Resource :: spi_resource(), RepeatedTxData :: byte(), Count :: non_neg_integer()
) ->
    {ok, binary()}.
read_blocking(_Resource, _RepeatedTxData, _Count) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   WriteData Binary data to write
%% @returns `{ok, ReadData}'
%% @doc     Simultaneous write and read from SPI, blocking.
%%
%%          WriteData is sent on TX while the same number of bytes is read
%%          into the returned binary.
%% @end
%%-----------------------------------------------------------------------------
-spec write_read_blocking(Resource :: spi_resource(), WriteData :: binary()) ->
    {ok, ReadData :: binary()}.
write_read_blocking(_Resource, _WriteData) ->
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
maybe_set_spi_function(undefined) -> ok;
maybe_set_spi_function(Pin) -> gpio:set_function(Pin, spi).

%% @private
get_value(Key, Map, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get_value(Key, List, Default) when is_list(List) ->
    proplists:get_value(Key, List, Default).

%% @private
get_value(Key, MapOrList) ->
    get_value(Key, MapOrList, undefined).

%% @private
setup_devices(DeviceConfigMap) when is_map(DeviceConfigMap) ->
    setup_devices(maps:to_list(DeviceConfigMap));
setup_devices(DeviceConfigList) when is_list(DeviceConfigList) ->
    lists:foldl(fun setup_device/2, #{}, DeviceConfigList).

%% @private
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
            gpio:set_function(CS, sio),
            gpio:set_pin_mode(CS, output),
            gpio:digital_write(CS, 1)
    end,
    DeviceInfo = #{
        cs => CS,
        clock_speed_hz => ClockSpeedHz,
        mode => Mode,
        address_len_bits => AddressLenBits,
        command_len_bits => CommandLenBits
    },
    Acc#{Name => DeviceInfo}.

%% @private
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

%% @private
handle_request(Resource, _Devices, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(Resource, Devices, {read_at, DeviceName, Address, LenBits}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
    ?MODULE:write_blocking(Resource, <<Address:AddressLenBits/big-unsigned>>),
    LenBytes = (LenBits + 7) div 8,
    {ok, Data} = ?MODULE:read_blocking(Resource, 0, LenBytes),
    deselect_device(DeviceInfo),
    <<Value:LenBits/big-unsigned, _/bitstring>> = Data,
    {reply, {ok, Value}};
handle_request(Resource, Devices, {write_at, DeviceName, Address, LenBits, Data}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
    TxBin = <<Address:AddressLenBits/big-unsigned, Data:LenBits/big-unsigned>>,
    {ok, RxBin} = ?MODULE:write_read_blocking(Resource, TxBin),
    deselect_device(DeviceInfo),
    <<_:AddressLenBits, Value:LenBits/big-unsigned>> = RxBin,
    {reply, {ok, Value}};
handle_request(Resource, Devices, {write, DeviceName, Transaction}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
    WriteData = maps:get(write_data, Transaction, <<>>),
    WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
    WriteBytes = (WriteBits + 7) div 8,
    TxData = binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))),
    ?MODULE:write_blocking(Resource, <<PrefixBin/binary, TxData/binary>>),
    deselect_device(DeviceInfo),
    {reply, ok};
handle_request(Resource, Devices, {write_read, DeviceName, Transaction}) ->
    DeviceInfo = maps:get(DeviceName, Devices),
    select_device(Resource, DeviceInfo),
    PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
    WriteData = maps:get(write_data, Transaction, <<>>),
    WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
    WriteBytes = (WriteBits + 7) div 8,
    ReadBits = maps:get(read_bits, Transaction, WriteBits),
    ReadBytes = (ReadBits + 7) div 8,
    case byte_size(PrefixBin) of
        0 -> ok;
        _ -> ?MODULE:write_blocking(Resource, PrefixBin)
    end,
    DataLen = max(WriteBytes, ReadBytes),
    TxData = pad_binary(binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))), DataLen),
    {ok, RxBin} = ?MODULE:write_read_blocking(Resource, TxData),
    deselect_device(DeviceInfo),
    ReadData = binary:part(RxBin, 0, ReadBytes),
    {reply, {ok, ReadData}}.

%% @private
select_device(Resource, DeviceInfo) ->
    ClockSpeedHz = maps:get(clock_speed_hz, DeviceInfo),
    Mode = maps:get(mode, DeviceInfo),
    {CPOL, CPHA} = mode_to_cpol_cpha(Mode),
    ?MODULE:set_baudrate(Resource, ClockSpeedHz),
    ?MODULE:set_format(Resource, 8, CPOL, CPHA),
    case maps:get(cs, DeviceInfo) of
        undefined -> ok;
        CS -> gpio:digital_write(CS, 0)
    end.

%% @private
deselect_device(DeviceInfo) ->
    case maps:get(cs, DeviceInfo) of
        undefined -> ok;
        CS -> gpio:digital_write(CS, 1)
    end.

%% @private
mode_to_cpol_cpha(0) -> {0, 0};
mode_to_cpol_cpha(1) -> {0, 1};
mode_to_cpol_cpha(2) -> {1, 0};
mode_to_cpol_cpha(3) -> {1, 1}.

%% @private
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

%% @private
pad_binary(Bin, Len) when byte_size(Bin) >= Len ->
    Bin;
pad_binary(Bin, Len) ->
    PadLen = Len - byte_size(Bin),
    <<Bin/binary, 0:(PadLen * 8)>>.
