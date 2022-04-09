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

-export([open/1, read_at/4, write_at/5]).

-type spi_peripheral() :: hspi | vspi.
-type bus_config() :: [
    {miso_io_num, non_neg_integer()} |
    {mosi_io_num, non_neg_integer()} |
    {sclk_io_num, non_neg_integer()} |
    {spi_peripheral, spi_peripheral()}
].
-type device_config() :: [
    {clock_speed_hz, non_neg_integer()} |
    {mode, 0..3} |
    {spi_cs_io_num, non_neg_integer()} |
    {address_len_bits, non_neg_integer()}
].
-type device_name() :: atom().
-type params() :: [
    {bus_config, bus_config()} |
    {device_config, [{device_name(), device_config()}]}
].

-type spi() :: pid().
-type address() :: non_neg_integer().


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
%%   <tr> <td>`miso_io_num'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>MISO pin number</td></tr>
%%   <tr> <td>`mosi_io_num'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>MOSI pin number</td></tr>
%%   <tr> <td>`sclk_io_num'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>SCLK pin number</td></tr>
%%   <tr> <td>`spi_peripheral'</td> <td>`hspi | vspi'</td> <td>`hspi'</td> <td>SPI Peripheral (ESP32 only)</td></tr>
%% </table>
%%
%% Each device configuration is a properties list containing the following entries:
%%
%% <table>
%%   <tr> <th>Key</th> <th>Type</th> <th>Default</th> <th>Description</th> </tr>
%%   <tr> <td>`clock_speed_hz'</td> <td>`non_neg_integer()'</td> <td>`1000000'</td> <td>Clock speed for the SPI device (in hz)</td></tr>
%%   <tr> <td>`mode'</td> <td>`0..3'</td> <td>`0'</td> <td>SPI device mode</td></tr>
%%   <tr> <td>`spi_cs_io_num'</td> <td>`non_neg_integer()'</td> <td>-</td> <td>SPI Chip Select pin number</td></tr>
%%   <tr> <td>`address_len_bits'</td> <td>`non_neg_integer()'</td> <td>8</td> <td>Number of bits in the device address</td></tr>
%% </table>
%%
%% Example:
%% <pre>
%% Params = [
%%    {bus_config, [
%%        {miso_io_num, 16},
%%        {mosi_io_num, 17},
%%        {sclk_io_num, 5}
%%    },
%%    {device_config, [
%%        {device1, [
%%            {spi_cs_io_num, 18}
%%        ]},
%%        {device2, [
%%            {spi_cs_io_num, 19}
%%        ]}
%%    ]}
%% ]
%% </pre>
%%
%% Note that `device1' and `device2' are atom names used to identify the device for read and write operations.
%%
%% This function raises an Erlang exception with a `badarg' reason, if initialization of the
%% SPI Bus or any device fails.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> spi().
open(Params) ->
    open_port({spawn, "spi"}, validate_params(Params)).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address from which to read
%% @returns `{ok, Value}' or `error'
%% @doc     Read a value from and address on the device.
%% @end
%%-----------------------------------------------------------------------------
-spec read_at(SPI :: spi(), DeviceName::device_name(), Address :: address(), Len :: non_neg_integer()) ->
    {ok, integer()} | error.
read_at(SPI, DeviceName, Address, Len) ->
    port:call(SPI, {read_at, DeviceName, Address, Len}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address to which to write
%% @returns `{ok, Value}' or `error'
%% @doc     Write a value to and address on the device.
%%
%%          The value returned from this function is dependent on the device and address.
%%          Consult the documentation for the device to understand expected return
%%          values from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec write_at(SPI :: spi(), DeviceName::device_name(), Address :: address(), Len :: non_neg_integer(), Data :: integer()) ->
    {ok, integer()} | error.
write_at(SPI, DeviceName, Address, Len, Data) ->
    port:call(SPI, {write_at, DeviceName, Address bor 16#80, Len, Data}).

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
validate_bus_config(BusConfig) when is_map(BusConfig) orelse is_list(BusConfig) ->
    #{
        miso_io_num => validate_integer_entry(miso_io_num, BusConfig),
        mosi_io_num => validate_integer_entry(mosi_io_num, BusConfig),
        sclk_io_num => validate_integer_entry(sclk_io_num, BusConfig),
        spi_peripheral => validate_spi_peripheral(get_value(spi_peripheral, BusConfig, hspi))
    };
validate_bus_config(undefined) ->
    throw({badarg, missing_bus_config});
validate_bus_config(BusConfig) ->
    throw({badarg, {not_a_map_or_list, BusConfig}}).

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
validate_spi_peripheral(hspi) -> hspi;
validate_spi_peripheral(vspi) -> vspi;
validate_spi_peripheral(Value) -> throw({bardarg, {spi_peripheral, Value}}).

%% @private
validate_device_config(DeviceConfig) when is_map(DeviceConfig) ->
    Iter = maps:iterator(DeviceConfig),
    validate_device_config_iter(maps:next(Iter), #{});
validate_device_config(DeviceConfig) when is_list(DeviceConfig) ->
    lists:foldl(
        fun validate_device_config_fold/2,
        DeviceConfig,
        #{}
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
validate_device_config_entries(Entries) when is_map(Entries) orelse is_list(Entries) ->
    #{
        spi_clock_hz => validate_integer_entry(spi_clock_hz, Entries),
        mode => validate_mode(get_value(mode, Entries, undefined)),
        spi_cs_io_num => validate_integer_entry(spi_cs_io_num, Entries, undefined),
        address_len_bits => validate_address_len_bits(get_value(address_len_bits, Entries, undefined))
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
get_value(Key, Map, DefaultValue) when is_map(Map) ->
    maps:get(Key, Map, DefaultValue);
get_value(Key, Proplist, DefaultValue) when is_list(Proplist) ->
    proplists:get_value(Key, Proplist, DefaultValue).
