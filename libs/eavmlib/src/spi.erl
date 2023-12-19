%
% This file is part of AtomVM.
%
% Copyright 2018-2020 Davide Bettio <davide@uninstall.it>
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

%%
%% @doc
%% This module provides an interface into the
%% <a href="https://en.wikipedia.org/wiki/Serial_Peripheral_Interface">Serial Peripheral Interface</a> (SPI)
%% supported on many devices.
%%
%% This module currently support the SPI "leader" (historically known as the "master") interface, allowing
%% the leader to connect to one or more "follower" (historically known as "slave") devices.
%%
%% Users interact with this interface by creating an instance of the driver via the `open/1' function,
%% with returns an opaque reference to the driver instance.  The `open/1' function takes a complex map
%% structure, which configures the driver to connect to follower devices.  See the `open/1' documentation
%% for details about the structure of this configuration map.
%%
%% Subsequent read and write operations use the SPI instance returned from the `open/1' function.  Users
%% may read from a specific follower device at a specific address, write to the device at an address,
%% or simultaneously read from and write to the device in a single transaction.
%% @end
%%
-module(spi).

-export([open/1, close/1, read_at/4, write_at/5, write/3, write_read/3]).

-type peripheral() :: hspi | vspi | string() | binary().
-type bus_config() :: [
    {poci, non_neg_integer()}
    | {pico, non_neg_integer()}
    | {miso, non_neg_integer()}
    | {mosi, non_neg_integer()}
    | {sclk, non_neg_integer()}
    | {peripheral, peripheral()}
].
-type device_config() :: [
    {clock_speed_hz, non_neg_integer()}
    | {mode, 0..3}
    | {cs, non_neg_integer()}
    | {address_len_bits, 0..64}
    | {command_len_bits, 0..16}
].
-type device_name() :: atom().
-type params() :: [
    {bus_config, bus_config()}
    | {device_config, [{device_name(), device_config()}]}
].

-type spi() :: pid().
-type address() :: non_neg_integer().

-type transaction() :: #{
    command => integer(),
    address => non_neg_integer(),
    write_data => binary(),
    write_bits => non_neg_integer(),
    read_bits => non_neg_integer()
}.

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns process id of the driver.
%% @throws badarg
%%
%% @doc Open a connection to the SPI driver
%%
%% This function will open a connection to the SPI driver.
%%
%% Supply a set of parameters to initialize the driver.
%%
%% The parameters list must contain an SPI Bus configuration, together with
%% a properties list containing one or more device configurations.  This list must contain
%% atom keys as names, which are used to identify the device in the subsequent read and
%% write operations.  You may use any atom value of your choosing.
%%
%% The SPI Bus configuration is a properties list containing the following entries:
%%
%% <table>
%%   <tr> <th>Key</th> <th>Type</th> <th>Default</th> <th>Description</th> </tr>
%%   <tr> <td>`miso'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>MISO pin number</td></tr>
%%   <tr> <td>`mosi'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>MOSI pin number</td></tr>
%%   <tr> <td>`sclk'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>SCLK pin number</td></tr>
%%   <tr> <td>`peripheral'</td> <td>`hspi | vspi'</td> <td>`hspi'</td> <td>SPI Peripheral (ESP32 only)</td></tr>
%% </table>
%%
%% Each device configuration is a properties list containing the following entries:
%%
%% <table>
%%   <tr> <th>Key</th> <th>Type</th> <th>Default</th> <th>Description</th> </tr>
%%   <tr> <td>`clock_speed_hz'</td> <td>`non_neg_integer()'</td> <td>`1000000'</td> <td>Clock speed for the SPI device (in hz)</td></tr>
%%   <tr> <td>`mode'</td> <td>`0..3'</td> <td>`0'</td> <td>SPI device mode</td></tr>
%%   <tr> <td>`cs'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>SPI Chip Select pin number</td></tr>
%%   <tr> <td>`address_len_bits'</td> <td>`non_neg_integer()'</td> <td>8</td> <td>Number of bits in the device address</td></tr>
%% </table>
%%
%% Example:
%% <pre>
%% Params = [
%%    {bus_config, [
%%        {miso, 16},
%%        {mosi, 17},
%%        {sclk, 5}
%%    },
%%    {device_config, [
%%        {device1, [
%%            {cs, 18}
%%        ]},
%%        {device2, [
%%            {cs, 19}
%%        ]}
%%    ]}
%% ]
%% </pre>
%%
%% Note that `device1' and `device2' are atom names used to identify the device for read and write operations.
%%
%% This function raises an Erlang exception with a `badarg' reason, if initialization of the
%% SPI Bus or any device fails.
%%
%% The `write/3' and `write_read/3' functions in this module are designed to provide the maximum mount of
%% flexibility when interfacing with the SPI device.  The both make use of a map structure to encapsulate an SPI transaction.
%%
%% An SPI transaction may contain a command, and address, and/or a blob of data, each of which is optional and
%% each of which depends on how users interact with the device.  Consult the data sheet for your SPI device to
%% understand which fields should be used with your device.
%%
%% The fields of a transaction map are as follows:
%%
%% <table>
%%  <tr><th>Key</th><th>Value Type</th><th>Description</th></tr>
%%  <tr><td>`command'</td><td>`integer()' (16-bit)</td><td>SPI command.  The low-order `command_len_bits' are written to the device.</td></tr>
%%  <tr><td>`address'</td><td>`integer()' (64-bit)</td><td>Device address.  The low-order `address_len_bits' are written to the device.</td></tr>
%%  <tr><td>`write_data'</td><td>`binary()'</td><td>Data to write</td></tr>
%%  <tr><td>`write_bits'</td><td>`non_neg_integer()'</td><td>Number of bits to write from `write_data'.  If not included, then all bits will be written.</td></tr>
%%  <tr><td>`read_bits'</td><td>`non_neg_integer()'</td><td>Number of bits to read from the SPI device.  If not included, then the same number of bits will be read as were written.</td></tr>
%% </table>
%%
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> spi().
open(Params) ->
    open_port({spawn, "spi"}, validate_params(Params)).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @doc     Close the SPI driver.
%%
%% Close the SPI driver and free any resources in use by the driver.
%%
%% The SPI instance will no longer be valid and usable after this function has been called.
%% @end
%%-----------------------------------------------------------------------------
-spec close(SPI :: spi()) -> ok.
close(SPI) ->
    port:call(SPI, {close}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Address SPI Address from which to read
%% @param   Len in bytes to read
%% @returns `{ok, Value}' or `error'
%% @doc     Read a value from and address on the device.
%% @end
%%-----------------------------------------------------------------------------
-spec read_at(
    SPI :: spi(), DeviceName :: device_name(), Address :: address(), Len :: non_neg_integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.
read_at(SPI, DeviceName, Address, Len) ->
    port:call(SPI, {read_at, DeviceName, Address, Len}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   DeviceName device name from configuration
%% @param   Address SPI Address to which to write
%% @param   Len in bytes to read
%% @param   Data byte(s) to write
%% @returns `{ok, Value}' or `error'
%% @doc     Write a value to and address on the device.
%%
%%          The value returned from this function is dependent on the device and address.
%%          Consult the documentation for the device to understand expected return
%%          values from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec write_at(
    SPI :: spi(),
    DeviceName :: device_name(),
    Address :: address(),
    Len :: non_neg_integer(),
    Data :: integer()
) ->
    {ok, integer()} | {error, Reason :: term()}.
write_at(SPI, DeviceName, Address, Len, Data) ->
    port:call(SPI, {write_at, DeviceName, Address bor 16#80, Len, Data}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   DeviceName SPI device name (use key in `device_config')
%% @param   Transaction transaction map.
%% @returns `ok' or `{error, Reason}', if an error occurred.
%% @doc     Write data to the SPI device, using the instructions encoded in the supplied transaction.
%%
%% The supplied `Transaction' encodes information about how data is to be written to the selected SPI device.
%% See the description above for the fields that may be specified in this map.
%%
%% When a binary is supplied in the `write_data' field, the data is written to the
%% SPI device in the natural order of the binary.  For example, if the
%% input binary is `<<16#57, 16#BA>>', then the first
%% byte is `0x57' and the second byte is `0xBA'.
%%
%% The value of the `write_bits' field, if specified, must be less than or equal to `8 * byte_size(write_data)'.
%% If `write_bits' is less than `8 * byte_size(write_data)', only the first `write_bits' bits from `write_data' will be written.
%%
%% This function will return a tuple containing the `error' atom if an error occurred
%% writing to the SPI device at the specified address.
%% The returned reason term is implementation-defined.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec write(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    ok | {error, Reason :: term()}.
write(SPI, DeviceName, Transaction) when
    is_pid(SPI) andalso is_atom(DeviceName) andalso is_map(Transaction)
->
    port:call(SPI, {write, DeviceName, Transaction}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   DeviceName SPI device name (use key in `device_config')
%% @param   Transaction transaction.
%% @returns `{ok, binary()}' or `{error, Reason}', if an error occurred.
%% @doc     Write data to the SPI device, using the instructions encoded in the supplied transaction.
%% device, and simultaneously read data back from the device, returning the read data in a binary.
%%
%% The supplied `Transaction' encodes information about how data is to be written to the selected SPI device.
%% See the description above for the fields that may be specified in this map.
%%
%% When a binary is supplied in the `write_data' field, the data is written to the
%% SPI device in the natural order of the binary.  For example, if the
%% input binary is `<<16#57, 16#BA>>', then the first
%% byte is `0x57' and the second byte is `0xBA'.
%%
%% The value of the `write_bits' field, if specified, must be less than or equal to `8 * byte_size(write_data)'.
%% If `write_bits' is less than `8 * byte_size(write_data)', only the first `write_bits' bits from `write_data' will be written.
%%
%% The return value contains a sequence of bytes that have been read from the SPI device.  The number of
%% bytes returned will be `ceil(read_bits / 8)'.  Only the first `read_bits' will be populated.
%%
%% This function will return a tuple containing the `error' atom if an error occurred
%% writing to the SPI device at the specified address.
%% The returned reason term is implementation-defined.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec write_read(SPI :: spi(), DeviceName :: device_name(), Transaction :: transaction()) ->
    {ok, ReadData :: binary()} | {error, Reason :: term()}.
write_read(SPI, DeviceName, Transaction) when
    is_pid(SPI) andalso is_atom(DeviceName) andalso is_map(Transaction)
->
    port:call(SPI, {write_read, DeviceName, Transaction}).

%%
%% Internal operations
%%
%% Note.  The configuration parameters passed into the C spi_driver is normalized
%% to be a map which is isomorphic to the properties list type structure defined in
%% the type specification above.  In a future release, the properties list will be
%% replaced with a map, and in fact, technically users can use a map to initialize
%% the SPI driver, but only when all peripherals have been upgraded to support maps
%% will this be documented.
%%

%% @private
validate_params(Params) when is_map(Params) orelse is_list(Params) ->
    #{
        bus_config => validate_bus_config(get_value(bus_config, Params, undefined)),
        device_config => validate_device_config(get_value(device_config, Params, undefined))
    };
validate_params(Params) ->
    throw({error, {not_a_map_or_list, Params}}).

%% @private
validate_bus_config(MaybeOldBusConfig) when
    is_map(MaybeOldBusConfig) orelse is_list(MaybeOldBusConfig)
->
    BusConfig = migrate_deprecated(MaybeOldBusConfig),
    #{
        miso => validate_integer_entry(miso, BusConfig),
        mosi => validate_integer_entry(mosi, BusConfig),
        sclk => validate_integer_entry(sclk, BusConfig),
        peripheral => validate_peripheral(get_value(peripheral, BusConfig, hspi))
    };
validate_bus_config(undefined) ->
    throw({badarg, missing_bus_config});
validate_bus_config(BusConfig) ->
    throw({badarg, {not_a_map_or_list, BusConfig}}).

%% @private
migrate_deprecated(MaybeDeprecated) when is_map(MaybeDeprecated) ->
    Iter = maps:iterator(MaybeDeprecated),
    migrate_deprecated_iter(maps:next(Iter), #{});
migrate_deprecated(MaybeDeprecated) when is_list(MaybeDeprecated) ->
    lists:foldl(
        fun migrate_deprecated_fold/2,
        [],
        MaybeDeprecated
    );
migrate_deprecated(MaybeDeprecated) ->
    throw({bardarg, {not_a_map_or_list, MaybeDeprecated}}).

%% @private
migrate_deprecated_iter(none, Accum) ->
    Accum;
migrate_deprecated_iter({K, V, Iter}, Accum) ->
    {Status, NewK} = replace_key(K),
    warn_deprecated(Status, K, NewK),
    migrate_deprecated_iter(maps:next(Iter), Accum#{NewK => V}).

%% @private
migrate_deprecated_fold({K, V}, Accum) ->
    {Status, NewK} = replace_key(K),
    warn_deprecated(Status, K, NewK),
    [{NewK, V} | Accum];
migrate_deprecated_fold(E, Accum) ->
    [E | Accum].

%% @private
replace_key(Key) ->
    case Key of
        pico -> {rename, mosi};
        poci -> {rename, miso};
        miso_io_num -> {warning, miso};
        mosi_io_num -> {warning, mosi};
        sclk_io_num -> {warning, sclk};
        spi_cs_io_num -> {warning, cs};
        spi_clock_hz -> {warning, clock_speed_hz};
        spi_peripheral -> {warning, peripheral};
        Any -> {ok, Any}
    end.

warn_deprecated(warning, OldKey, NewKey) ->
    io:format("SPI: found deprecated ~p, use ~p instead!!!~n", [OldKey, NewKey]);
warn_deprecated(_Status, Key, Key) ->
    ok.

%% @private
validate_integer_entry(Key, Map) ->
    validate_integer_entry(Key, Map, undefined).

%% @private
validate_integer_entry(Key, Map, DefaultValue) when is_map(Map) orelse is_list(Map) ->
    validate_is_integer(Key, get_value(Key, Map, DefaultValue)).

%% @private
validate_is_integer(_Key, Int) when is_integer(Int) ->
    Int;
validate_is_integer(Key, Value) ->
    throw({badarg, {not_an_integer_value, {Key, Value}}}).

%% @private
validate_peripheral(hspi) ->
    io:format("SPI: deprecated peripheral name!!!~n"),
    hspi;
validate_peripheral(vspi) ->
    io:format("SPI: deprecated peripheral name!!!~n"),
    vspi;
validate_peripheral(PeripheralString) when is_list(PeripheralString) ->
    % Internally atoms are still used, so it is easier to convert them here
    % TODO: use just strings in the future
    erlang:list_to_atom(PeripheralString);
validate_peripheral(PeripheralBinString) when is_binary(PeripheralBinString) ->
    erlang:binary_to_atom(PeripheralBinString, latin1);
validate_peripheral(Value) ->
    throw({bardarg, {peripheral, Value}}).

%% @private
validate_device_config(DeviceConfig) when is_map(DeviceConfig) ->
    Iter = maps:iterator(DeviceConfig),
    validate_device_config_iter(maps:next(Iter), #{});
validate_device_config(DeviceConfig) when is_list(DeviceConfig) ->
    lists:foldl(
        fun validate_device_config_fold/2,
        #{},
        DeviceConfig
    );
validate_device_config(DeviceConfig) ->
    throw({bardarg, {not_a_map_or_list, DeviceConfig}}).

%% @private
validate_device_config_iter(none, Accum) ->
    Accum;
validate_device_config_iter({K, V, Iter}, Accum) when is_atom(K) ->
    Entries = validate_device_config_entries(V),
    validate_device_config_iter(maps:next(Iter), Accum#{K => Entries});
validate_device_config_iter({K, _V, _Iter}, _Accum) ->
    throw({bardarg, {not_an_atom, K}}).

validate_device_config_fold({K, V}, Accum) when is_atom(K) ->
    Entries = validate_device_config_entries(V),
    Accum#{K => Entries};
validate_device_config_fold({K, _V}, _Accum) ->
    throw({bardarg, {not_an_atom, K}});
validate_device_config_fold(E, _Accum) ->
    throw({bardarg, {not_proplist_entry, E}}).

%% @private
validate_device_config_entries(MaybeOldEntries) when
    is_map(MaybeOldEntries) orelse is_list(MaybeOldEntries)
->
    Entries = migrate_deprecated(MaybeOldEntries),
    #{
        clock_speed_hz => validate_integer_entry(clock_speed_hz, Entries),
        mode => validate_mode(get_value(mode, Entries, undefined)),
        cs => validate_integer_entry(cs, Entries, undefined),
        address_len_bits => validate_address_len_bits(
            get_value(address_len_bits, Entries, undefined)
        ),
        command_len_bits => validate_command_len_bits(get_value(command_len_bits, Entries, 0))
    };
validate_device_config_entries(Entries) ->
    throw({bardarg, {not_a_map_or_list, Entries}}).

%% @private
validate_mode(Mode) when is_integer(Mode) andalso 0 =< Mode andalso Mode =< 3 ->
    Mode;
validate_mode(Mode) ->
    throw({bardarg, {mode, Mode}}).

%% @private
validate_address_len_bits(Len) when is_integer(Len) andalso 0 =< Len andalso Len =< 64 ->
    Len;
validate_address_len_bits(Len) ->
    throw({bardarg, {address_len_bits, Len}}).

%% @private
validate_command_len_bits(Len) when is_integer(Len) andalso 0 =< Len andalso Len =< 16 ->
    Len;
validate_command_len_bits(Len) ->
    throw({bardarg, {command_len_bits, Len}}).

%% @private
get_value(Key, Map, DefaultValue) when is_map(Map) ->
    maps:get(Key, Map, DefaultValue);
get_value(Key, Proplist, DefaultValue) when is_list(Proplist) ->
    proplists:get_value(Key, Proplist, DefaultValue).
