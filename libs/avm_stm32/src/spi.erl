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
%% @doc AtomVM SPI interface for STM32
%%
%% This module provides an interface to the SPI hardware on STM32 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API (STM32 HAL)</b>
%% {@link init/2}, {@link deinit/1}, {@link transmit/3}, {@link receive_/3},
%% {@link transmit_receive/3}, {@link abort/1}, {@link get_state/1},
%% {@link get_error/1}.
%% These map directly to the corresponding HAL_SPI_* functions and operate on
%% a bare resource reference returned by {@link init/2}.
%% Pin muxing must be done separately via `gpio:set_pin_mode/2' with
%% `{af, AFNumber}' mode.
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

%% Low-level API (STM32 HAL)
-export([
    init/2,
    deinit/1,
    transmit/3,
    receive_/3,
    transmit_receive/3,
    abort/1,
    get_state/1,
    get_error/1
]).

-type freq_hz() :: non_neg_integer().
-type peripheral() :: 1..6.
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
-type spi_state() :: reset | ready | busy | busy_tx | busy_rx | busy_tx_rx | error | abort.

-type spi_config() :: #{
    mode => 0..1,
    direction => 0..2,
    data_size => 4..16,
    cpol => 0..1,
    cpha => 0..1,
    nss => 0..2,
    baudrate => non_neg_integer(),
    first_bit => 0..1,
    ti_mode => 0..1,
    crc_enable => 0..1,
    crc_poly => pos_integer(),
    nss_pulse => 0..1,
    crc_length => 0..2
}.

-export_type([
    spi/0, spi_resource/0, device_name/0, address/0, transaction/0, spi_state/0, spi_config/0
]).

-define(DEFAULT_CLOCK_SPEED_HZ, 1000000).
-define(DEFAULT_MODE, 0).
-define(DEFAULT_ADDRESS_LEN_BITS, 8).
-define(DEFAULT_COMMAND_LEN_BITS, 0).
-define(DEFAULT_PERIPHERAL, 1).
-define(DEFAULT_AF, 5).
-define(DEFAULT_SEND_TIMEOUT_MS, 1000).

%% ---------------------------------------------------------------------------
%% High-level API (spi_hal behaviour)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns SPI handle (pid)
%% @doc     Open a connection to the SPI driver
%%
%%          This function configures the GPIO pins for SPI alternate function,
%%          initializes the SPI peripheral, and sets up CS pins for
%%          software chip-select management.
%%
%%          Parameters follow a similar structure to other platforms:
%%          <ul>
%%              <li>`{bus_config, BusConfig}' - bus configuration (required)</li>
%%              <li>`{device_config, DeviceConfigs}' - device configurations</li>
%%          </ul>
%%
%%          Bus configuration:
%%          <ul>
%%              <li>`{sclk, Pin}' - the SCLK pin (required)</li>
%%              <li>`{mosi, Pin}' - the MOSI pin</li>
%%              <li>`{miso, Pin}' - the MISO pin</li>
%%              <li>`{peripheral, 1..6}' - the SPI peripheral to use (default: 1)</li>
%%              <li>`{af, N}' - GPIO alternate function number (default: 5)</li>
%%              <li>`{send_timeout_ms, Ms}' - timeout for SPI transfers in
%%              milliseconds (default: 1000)</li>
%%          </ul>
%%
%%          Device configuration (keyed by device name atom):
%%          <ul>
%%              <li>`{cs, Pin}' - the CS pin</li>
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
    case BusConfig of
        undefined ->
            erlang:error({bad_config, {missing_required_key, bus_config, open, 1}});
        _ ->
            ok
    end,
    SCLK = get_value(sclk, BusConfig),
    case SCLK of
        undefined ->
            erlang:error({bad_config, {missing_required_key, sclk, open, 1}});
        _ ->
            ok
    end,
    DeviceConfigList = get_value(device_config, Params, []),
    MOSI = get_value(mosi, BusConfig, undefined),
    MISO = get_value(miso, BusConfig, undefined),
    Peripheral = get_value(peripheral, BusConfig, ?DEFAULT_PERIPHERAL),
    AF = get_value(af, BusConfig, ?DEFAULT_AF),
    SendTimeoutMs = get_value(send_timeout_ms, BusConfig, ?DEFAULT_SEND_TIMEOUT_MS),
    maybe_set_af(SCLK, AF),
    maybe_set_af(MOSI, AF),
    maybe_set_af(MISO, AF),
    Config = #{baudrate => ?DEFAULT_CLOCK_SPEED_HZ},
    {ok, {_ActualBaud, Resource}} = ?MODULE:init(Peripheral, Config),
    Devices = setup_devices(DeviceConfigList),
    spawn_link(fun() -> loop(Resource, Devices, SendTimeoutMs) end).

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
%%          Sends the address with bit 7 forced high (16#80 ORed).
%%          This convention is used by some devices (e.g., LIS3DH) to
%%          distinguish writes from reads. For devices with different
%%          conventions, use {@link write/3} with a transaction map instead.
%%          Returns the value read back during the data phase.
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
    call(Pid, {write_at, DeviceName, Address, Len, Data}).

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
%% Low-level API (STM32 HAL)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Peripheral SPI peripheral number (1..6)
%% @param   Config SPI configuration map
%% @returns `{ok, {ActualBaudrate, Resource}}'
%% @doc     Initialize the SPI HW block (HAL_SPI_Init).
%%
%%          The Config map can contain the following keys (all optional,
%%          defaults shown):
%%          <ul>
%%              <li>`{mode, 0}' - SPI_MODE_MASTER (0) or SPI_MODE_SLAVE (1)</li>
%%              <li>`{direction, 0}' - SPI_DIRECTION_2LINES (0), SPI_DIRECTION_2LINES_RXONLY (1),
%%                 or SPI_DIRECTION_1LINE (2)</li>
%%              <li>`{data_size, 8}' - data size (4-16 bits)</li>
%%              <li>`{cpol, 0}' - clock polarity: 0 (low) or 1 (high)</li>
%%              <li>`{cpha, 0}' - clock phase: 0 (first edge) or 1 (second edge)</li>
%%              <li>`{nss, 1}' - NSS: SPI_NSS_SOFT (1), SPI_NSS_HARD_INPUT (0),
%%                 or SPI_NSS_HARD_OUTPUT (2)</li>
%%              <li>`{baudrate, 0}' - baudrate in Hz (0 = default, calculates prescaler)</li>
%%              <li>`{first_bit, 0}' - SPI_FIRSTBIT_MSB (0) or SPI_FIRSTBIT_LSB (1)</li>
%%              <li>`{ti_mode, 0}' - TI mode: disabled (0) or enabled (1)</li>
%%              <li>`{crc_enable, 0}' - CRC: disabled (0) or enabled (1)</li>
%%              <li>`{crc_poly, 0}' - CRC polynomial (default: 7)</li>
%%              <li>`{nss_pulse, 0}' - NSS pulse: disabled (0) or enabled (1)</li>
%%              <li>`{crc_length, 0}' - CRC length: aligned (0), 8-bit (1), or 16-bit (2)</li>
%%          </ul>
%%
%%          Pin muxing must be done separately via
%%          `gpio:set_pin_mode(Pin, {af, AFNumber})'.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Peripheral :: peripheral(), Config :: spi_config()) ->
    {ok, {ActualBaudrate :: freq_hz(), Resource :: spi_resource()}}.
init(_Peripheral, _Config) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @returns `ok'
%% @doc     Disable the SPI HW block (HAL_SPI_DeInit).
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: spi_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   Data Binary data to transmit
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written, or `{error, Reason}'
%% @doc     Transmit data (HAL_SPI_Transmit).
%% @end
%%-----------------------------------------------------------------------------
-spec transmit(Resource :: spi_resource(), Data :: binary(), TimeoutMs :: timeout()) ->
    non_neg_integer() | {error, term()}.
transmit(_Resource, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   Count Number of bytes to receive
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc     Receive data (HAL_SPI_Receive).
%% @end
%%-----------------------------------------------------------------------------
-spec receive_(Resource :: spi_resource(), Count :: non_neg_integer(), TimeoutMs :: timeout()) ->
    {ok, binary()} | {error, term()}.
receive_(_Resource, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @param   Data Binary data to transmit
%% @param   TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, ReadData}' or `{error, Reason}'
%% @doc     Simultaneous transmit and receive (HAL_SPI_TransmitReceive).
%%
%%          Data is sent on TX while the same number of bytes is read
%%          into the returned binary.
%% @end
%%-----------------------------------------------------------------------------
-spec transmit_receive(Resource :: spi_resource(), Data :: binary(), TimeoutMs :: timeout()) ->
    {ok, binary()} | {error, term()}.
transmit_receive(_Resource, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @returns `ok' or `{error, Reason}'
%% @doc     Abort ongoing SPI transfer (HAL_SPI_Abort).
%% @end
%%-----------------------------------------------------------------------------
-spec abort(Resource :: spi_resource()) -> ok | {error, term()}.
abort(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @returns SPI state atom
%% @doc     Get the SPI peripheral state (HAL_SPI_GetState).
%%
%%          Returns one of: `reset', `ready', `busy', `busy_tx', `busy_rx',
%%          `busy_tx_rx', `error', `abort'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_state(Resource :: spi_resource()) -> spi_state().
get_state(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource SPI resource returned by `init/2'
%% @returns Error code as integer
%% @doc     Get the SPI error code (HAL_SPI_GetError).
%%
%%          Returns a bitmask of HAL_SPI_ERROR_* values.
%%          0 means no error.
%% @end
%%-----------------------------------------------------------------------------
-spec get_error(Resource :: spi_resource()) -> non_neg_integer().
get_error(_Resource) ->
    erlang:nif_error(undefined).

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

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
maybe_set_af(undefined, _AF) -> ok;
maybe_set_af(Pin, AF) -> gpio:set_pin_mode(Pin, {af, AF}).

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
    if
        AddressLenBits rem 8 =/= 0 -> erlang:error({invalid_address_len_bits, AddressLenBits});
        CommandLenBits rem 8 =/= 0 -> erlang:error({invalid_command_len_bits, CommandLenBits});
        true -> ok
    end,
    case CS of
        undefined ->
            ok;
        _ ->
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
loop(Resource, Devices, SendTimeoutMs) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, Devices, SendTimeoutMs, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply} ->
                    From ! {Ref, Reply},
                    loop(Resource, Devices, SendTimeoutMs)
            end
    end.

%% @private
handle_request(Resource, _Devices, _SendTimeoutMs, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(Resource, Devices, SendTimeoutMs, {read_at, DeviceName, Address, LenBits}) ->
    case maps:find(DeviceName, Devices) of
        {ok, DeviceInfo} ->
            select_device(Resource, DeviceInfo),
            AddressLenBits = maps:get(address_len_bits, DeviceInfo),
            LenBytes = (LenBits + 7) div 8,
            TxBin = <<Address:AddressLenBits/big-unsigned, 0:(LenBytes * 8)>>,
            case ?MODULE:transmit_receive(Resource, TxBin, SendTimeoutMs) of
                {ok, RxBin} ->
                    deselect_device(DeviceInfo),
                    <<_:AddressLenBits, Value:LenBits/big-unsigned, _/bitstring>> = RxBin,
                    {reply, {ok, Value}};
                {error, _} = Error ->
                    deselect_device(DeviceInfo),
                    {reply, Error}
            end;
        error ->
            {reply, {error, {unknown_device, DeviceName}}}
    end;
handle_request(Resource, Devices, SendTimeoutMs, {write_at, DeviceName, Address, LenBits, Data}) ->
    case maps:find(DeviceName, Devices) of
        {ok, DeviceInfo} ->
            if
                LenBits rem 8 =/= 0 ->
                    {reply, {error, bad_bit_length}};
                true ->
                    select_device(Resource, DeviceInfo),
                    AddressLenBits = maps:get(address_len_bits, DeviceInfo),
                    AdjustedAddress =
                        case AddressLenBits of
                            8 -> Address bor 16#80;
                            _ -> Address bor (1 bsl (AddressLenBits - 1))
                        end,
                    TxBin =
                        <<AdjustedAddress:AddressLenBits/big-unsigned, Data:LenBits/big-unsigned>>,
                    case ?MODULE:transmit_receive(Resource, TxBin, SendTimeoutMs) of
                        {ok, RxBin} ->
                            deselect_device(DeviceInfo),
                            <<_:AddressLenBits, Value:LenBits/big-unsigned>> = RxBin,
                            {reply, {ok, Value}};
                        {error, _} = Error ->
                            deselect_device(DeviceInfo),
                            {reply, Error}
                    end
            end;
        error ->
            {reply, {error, {unknown_device, DeviceName}}}
    end;
handle_request(Resource, Devices, SendTimeoutMs, {write, DeviceName, Transaction}) ->
    case maps:find(DeviceName, Devices) of
        {ok, DeviceInfo} ->
            select_device(Resource, DeviceInfo),
            PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
            WriteData = maps:get(write_data, Transaction, <<>>),
            WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
            WriteBytes = (WriteBits + 7) div 8,
            TxData = binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))),
            case ?MODULE:transmit(Resource, <<PrefixBin/binary, TxData/binary>>, SendTimeoutMs) of
                {error, _} = Error ->
                    deselect_device(DeviceInfo),
                    {reply, Error};
                _N ->
                    deselect_device(DeviceInfo),
                    {reply, ok}
            end;
        error ->
            {reply, {error, {unknown_device, DeviceName}}}
    end;
handle_request(Resource, Devices, SendTimeoutMs, {write_read, DeviceName, Transaction}) ->
    case maps:find(DeviceName, Devices) of
        {ok, DeviceInfo} ->
            select_device(Resource, DeviceInfo),
            PrefixBin = build_tx_prefix(DeviceInfo, Transaction),
            PrefixLen = byte_size(PrefixBin),
            WriteData = maps:get(write_data, Transaction, <<>>),
            WriteBits = maps:get(write_bits, Transaction, byte_size(WriteData) * 8),
            WriteBytes = (WriteBits + 7) div 8,
            ReadBits = maps:get(read_bits, Transaction, WriteBits),
            ReadBytes = (ReadBits + 7) div 8,
            DataLen = max(WriteBytes, ReadBytes),
            TxData = pad_binary(
                binary:part(WriteData, 0, min(WriteBytes, byte_size(WriteData))), DataLen
            ),
            case
                ?MODULE:transmit_receive(
                    Resource, <<PrefixBin/binary, TxData/binary>>, SendTimeoutMs
                )
            of
                {ok, RxBin} ->
                    deselect_device(DeviceInfo),
                    ReadData = binary:part(RxBin, PrefixLen, ReadBytes),
                    {reply, {ok, ReadData}};
                {error, _} = Error ->
                    deselect_device(DeviceInfo),
                    {reply, Error}
            end;
        error ->
            {reply, {error, {unknown_device, DeviceName}}}
    end.

%% @private
select_device(_Resource, DeviceInfo) ->
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
