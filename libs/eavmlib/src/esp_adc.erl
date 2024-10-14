%%
%% Copyright (c) 2020-2023 dushin.net
%% Copyright (c) 2022-2024 Winford <winford@object.stream>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc Analog to digital peripheral support.
%%
%% Use this module to take ADC (analog voltage) readings. Currently this driver
%% only supports the esp32 family of chips, but support for other platforms is
%% planned in the future. On an ESP32 device ADC unit 1 allows taking reading from
%% pins 32-39. ADC unit2 is disabled by default for the ESP32 classic, but when
%% enabled in the build configuration allows pins 0, 2, 4, 12-15, and 25-27 to be
%% used as long as WiFi is not required by the application. Unit 2 is disabled for
%% ESP32C3 due to its inaccurate results. ADC unit 2 is enabled for all other ESP32
%% series with more than one ADC unit; there is an arbitrator peripheral that allows
%% ADC unit 2 to be used while WiFI is active.  The pins available for ADC use vary
%% by device, check your datasheet for specific hardware support.
%%
%% There are two sets of APIs for interacting with the ADC hardware, only one set
%% of API may be used by an application.
%%
%% The core functionality is provided by the low level resource based nif functions.
%% To use the resource based nifs `esp_adc:init/0' and `esp_adc:deinit/1' will acquire and
%% release the adc `unit' resource needed for all other functions. A `channel' resource
%% used to take measurements from a pin can be obtained using `esp_adc:acquire/4', and
%% released using `esp_adc:release_channel/1'. ADC measurements are taken using `sample/3'.
%%
%% For convenience a gen_server managed set of APIs using pin numbers are also available.
%% A pin may be configured using `esp_adc:start/1,2', measurements are taken using
%% `esp_adc:read/1,2', pins can be released individually with `esp_adc:stop/1`, or the driver
%% can be stopped completely using `esp_adc:stop/0'.
%% @end
%%-----------------------------------------------------------------------------
-module(esp_adc).

-behaviour(gen_server).

%% Low level resource based nif functions
-export([acquire/4, init/0, release_channel/1, deinit/1, sample/3]).
%% Nif convenience functions
-export([acquire/2, sample/2]).

%% gen_server convenience functions
-export([start/0, start/1, start/2, read/1, read/2, stop/1, stop/0]).
%% gen_server internals
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2]).

-type adc_rsrc() :: {'$adc', Resource :: binary(), Ref :: reference()}.
-type adc_pin() :: non_neg_integer().
%% ADC capable pins vary by chipset. Consult your datasheet.
-type bit_width() :: bit_9 | bit_10 | bit_11 | bit_12 | bit_13 | bit_max.
%% The default `bit_max' will select the highest value supported by the chipset. Some models only support a single
%% fixed bit width.
-type attenuation() :: db_0 | db_2_5 | db_6 | db_11 | db_12.
%% The decibel gain determines the maximum safe voltage to be measured. Default is `db_11'. The specific range of
%% voltages supported by each setting varies by device. Typical voltage ranges are depicted in the table below:
%% <table>
%%   <tr> <th> Attenuation </th><th> Min Millivolts </th><th> Max Millivolts </th></tr>
%%   <tr> <td>`db_0'</td> <td>0-100</td> <td>750-950</td></tr>
%%   <tr> <td>`db_2_5'</td> <td>0-100</td> <td>1050-1250</td></tr>
%%   <tr> <td>`db_6'</td> <td>0-150</td> <td>1300-1750</td></tr>
%%   <tr> <td>`bd_11' | `db_12' </td> <td>0-150</td> <td>2450-2500</td></tr>
%% </table>
%% Consult the datasheet for your device to determine the exact voltage ranges supported by each gain setting.
%% The option `db_11' has been superseded by `db_12'. The option`db_11' and will be deprecated in a future release,
%% applications should be updated to use `db_12' (except for builds with ESP-IDF versions prior to v5.2). To Continue
%% to support older IDF version builds, the default will remain `db_11', which is the maximum tolerated voltage on
%% all builds, as `db_12' supported builds will automatically use `db_12' in place of `db_11', when `db_11' is
%% deprecated in all builds the default will be changed to `db_12'.

-type pin_options() :: [pin_option()].
-type pin_option() :: {bitwidth, Width :: bit_width()} | {atten, Decibels :: attenuation()}.
-type read_options() :: [read_option()].
-type read_option() :: raw | voltage | {samples, 1..100000}.
%% The value of the `samples' key is the number of samples to be taken and averaged when returning a measurement,
%% default is 64. For optimal stable readings use a 100nF ceramic capacitor input filter, for more info consult
%% Espressif's "ADC Calibration Driver" documentation.
%% The keys `raw' and `voltage' determine if these values are included in the results or returned as `undefined'.
-type raw_value() :: 0..511 | 0..1023 | 0..2047 | 0..4095 | 0..8191 | undefined.
%% The maximum analog value is determined by `bit_width()'.
-type voltage_reading() :: 0..3300 | undefined.
%% The maximum safe millivolt value that can be measured is determined by `attenuation()', this value should never
%% exceed the chips maximum input tolerance.
-type reading() :: {raw_value() | undefined, voltage_reading() | undefined}.

-define(ADC_RSRC, {'$adc', _Resource, _Ref}).
-define(DEFAULT_SAMPLES, 64).
-define(DEFAULT_READ_OPTIONS, [raw, voltage, {samples, ?DEFAULT_SAMPLES}]).
-define(DEFAULT_PIN_OPTIONS, [{bitwidth, bit_max}, {atten, db_11}]).

%%-----------------------------------------------------------------------------
%% @returns {ok, ADCUnit :: adc_rsrc()} | {error, Reason}
%% @doc     Nif to initialize the ADC unit hardware.
%%
%% The returned ADC unit handle resource must be supplied for all future ADC operations.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec init() -> {ok, ADCUnit :: adc_rsrc()} | {error, Reason :: term()}.
init() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   UnitResource returned from init/0
%% @returns ok | {error, Reason}
%% @doc     Nif to release the ADC unit resource returned from init/0.
%%
%% Stop the ADC driver and free the unit resource. All active ADC channels should
%% be released using `release_channel/1' to free each configured channel before
%% freeing the unit resource.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(UnitResource :: adc_rsrc()) -> ok | {error, Reason :: term()}.
deinit(_UnitResource) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Pin         Pin to configure as ADC
%% @param   UnitHandle  The unit handle returned from `init/0'
%% @equiv   acquire(Pin, UnitHandle, bit_max, db_11)
%% @returns {ok, Channel::adc_rsrc()} | {error, Reason}
%% @doc     Nif to initialize an ADC pin.
%%
%% Initializes an ADC pin and returns a channel handle resources.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec acquire(Pin :: adc_pin(), UnitHandle :: adc_rsrc()) ->
    {ok, Channel :: adc_rsrc()} | {error, Reason :: term()}.
acquire(Pin, UnitHandle) ->
    ?MODULE:acquire(Pin, UnitHandle, bit_max, db_11).

%%-----------------------------------------------------------------------------
%% @param   Pin         Pin to configure as ADC
%% @param   UnitHandle  The unit handle returned from `init/0'
%% @param   BitWidth    Resolution in bit to measure
%% @param   Attenuation Decibel gain for voltage range
%% @returns {ok, Channel::adc_rsrc()} | {error, Reason}
%% @doc     Nif to initialize an ADC pin.
%%
%% The BitWidth value `bit_max' may be used to automatically select the highest
%% sample rate supported by your ESP chip-set, or choose from a bit width supported
%% by the device.
%%
%% The Attenuation value can be used to adust the gain, and therefore safe
%% measurement range on voltage the exact range of voltages supported by each
%% db gain varies by chip, consult the data sheet for exact range of your model.
%% For more information see the `attenuation()' type specification.
%%
%% Use the returned `Channel' reference in subsequent ADC operations on
%% the same pin.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec acquire(
    Pin :: adc_pin(),
    UnitHandle :: adc_rsrc(),
    BitWidth :: bit_width(),
    Attenuation :: attenuation()
) -> {ok, Channel :: adc_rsrc()} | {error, Reason :: term()}.
acquire(_Pin, _UnitHandle, _BitWidth, _Attenuation) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   ChannelResource of the pin returned from acquire/4
%% @returns ok | {error, Reason}
%% @doc     Nif to deinitialize the specified ADC channel.
%%
%% In the case that an error is returned it is safe to "drop" the `ChannelResource'
%% handle from use. After there are no remaining processes with references to
%% the channel resource handle, the calibration profile and any remaining resources
%% associated with the channel will be released as part of the next garbage
%% collection event.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec release_channel(ChannelResource :: adc_rsrc()) -> ok | {error, Reason :: term()}.
release_channel(_ChannelResource) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   ChannelResource of the pin returned from acquire/4
%% @param   UnitResource of the pin returned from init/0
%% @returns {ok, {RawValue, MilliVolts}} | {error, Reason}
%% @equiv   sample(ChannelResource, UnitResource, [raw, voltage, {samples, 64}])
%% @doc     Nif to take a reading using default values from an ADC channel.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec sample(ChannelResource :: adc_rsrc(), UnitResource :: adc_rsrc()) ->
    {ok, reading()} | {error, Reason :: term()}.
sample(ChannelResource, UnitResource) ->
    ?MODULE:sample(ChannelResource, UnitResource, ?DEFAULT_READ_OPTIONS).

%%-----------------------------------------------------------------------------
%% @param   ChannelResource of the pin returned from acquire/4
%% @param   UnitResource of the pin returned from init/0
%% @param   ReadOptions extra list of options to override defaults.
%% @returns {ok, {RawValue, MilliVolts}} | {error, Reason}
%% @doc     Nif to take a reading from an ADC channel.
%%
%% The Options parameter may be used to specify the behavior of the read
%% operation.
%%
%% If the ReadOptions contains the atom `raw', then the raw value will be returned
%% in the first element of the returned tuple.  Otherwise, this element will be
%% the atom `undefined'.
%%
%% If the ReadOptions contains the atom `voltage', then the voltage value will be
%% returned in millivolts in the second element of the returned tuple.  Otherwise,
%% this element will be the atom `undefined'.
%%
%% You may specify the number of samples to be taken and averaged over using the
%% tuple `{samples, Samples::pos_integer()}'.
%%
%% If the error `Reason' is timeout and the adc channel is on unit 2 then WiFi is
%% likely enabled and adc2 readings may be blocked until there is less network
%% traffic. On and ESP32 classic the results for unit 2 will always be
%% `{error, timeout}' if wifi is enabled.
%%
%% This is a low level nif that cannot be used in an application that uses the
%% convenience functions.
%% @end
%%-----------------------------------------------------------------------------
-spec sample(
    ChannelResource :: adc_rsrc(), UnitResource :: adc_rsrc(), ReadOptions :: read_options()
) -> {ok, Result :: reading()} | {error, Reason :: term()}.
sample(_ChannelResource, _UnitResource, _ReadOptions) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns {ok, Pid}
%% @doc     Optionally initialize a gen_server managed ADC driver without a pin.
%%
%% Use of this function is optional, but may be desired if the drivers pid is needed,
%% or it is desireable to start the driver without configuring an initial ADC channel.
%%
%% Note: since only one instance of the driver is allowed it is registered with the
%% name `adc_driver', which also may be used to directly call the gen_server.
%%
%% This convenience function cannot be used in an application that uses the low level
%% nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec start() -> {ok, Pid :: pid()}.
start() ->
    Pid = get_adc_pid(),
    {ok, Pid}.

%%-----------------------------------------------------------------------------
%% @param   Pin         Pin to configure as ADC
%% @equiv   start(Pin, [{bitwidth, bit_max}, {atten, db_11}])
%% @returns ok | {error, Reason}
%% @doc     Initialize a gen_server managed ADC pin with default options.
%%
%% This convenience function configures an ADC pin with the default options for
%% use with the optional `gen_server' APIs.  Default options are:
%% `[{bitwidth, bit_max}, {atten, db_11}]'
%%
%% This function cannot be used in an application that uses the low level
%% nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin :: adc_pin()) -> ok | {error, Reason :: term()}.
start(Pin) ->
    start(Pin, ?DEFAULT_PIN_OPTIONS).

%%-----------------------------------------------------------------------------
%% @param   Pin         Pin to configure as ADC
%% @param   Options     List of options to override default settings
%% @returns ok | {error, Reason}
%% @doc     Initialize a gen_server managed ADC pin with the supplied options.
%%
%% This convenience function configures an ADC pin with the provided options to
%% override the default configuration: `[{bitwidth, bit_max}, {atten, db_11}]'.
%%
%% For more details about these options see the `attenuation()' and `bit_width()'
%% type specifications.
%%
%% This function cannot be used in an application that uses the low level nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin :: adc_pin(), Options :: pin_options()) -> ok | {error, Reason :: term()}.
start(Pin, Options) ->
    {Bits, Atten} = validate_pin_options(Options),
    gen_server:call(get_adc_pid(), {acquire, Pin, Bits, Atten}).

%%-----------------------------------------------------------------------------
%% @param   Pin the pin to be released
%% @returns ok | {error, Reason}
%% @doc     De-initialize the specified ADC pin.
%%
%% This convenience function is used to release a pin from the gen_server managed
%% ADC driver. If an error is returned the ADC channel will still be stopped and
%% release internal resources during the next VM garbage collection event, the pin
%% will immediately no longer be useable in any case.
%%
%% This function cannot be used in an application that uses the low level nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(Pin :: adc_pin()) -> ok | {error, Reason :: term()}.
stop(Pin) ->
    gen_server:call(adc_driver, {stop, Pin}).

%%-----------------------------------------------------------------------------
%% @returns ok | {error, Reason}
%% @doc     Stop the ADC driver and release all resources.
%%
%% This convenience function is used to completely stop the gen_server managed
%% ADC driver and release all resources.
%%
%% Note: if an error is returned, a full shutdown of the ADC peripheral should
%% still occur, and any remaining resources freed with next VM garbage collection
%% event. Regardless the gen_server will exit normally and the adc peripheral
%% will no longer be usable.
%%
%% This function cannot be used in an application that uses the low level nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    gen_server:call(adc_driver, stop).

%%-----------------------------------------------------------------------------
%% @param   Pin     The pin from which to take ADC measurement
%% @returns {ok, {RawValue, MilliVolts}} | {error, Reason}
%% @equiv   read(Pin, [raw, voltage, {samples, 64}])
%% @doc     Take a reading using default values from an ADC pin.
%%
%% This convenience function is used to take a measurement from a previously
%% started adc pin.
%%
%% This function cannot be used in an application that uses the low level
%% nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Pin :: adc_pin()) -> {ok, reading()} | {error, Reason :: term()}.
read(Pin) ->
    gen_server:call(adc_driver, {read, Pin, ?DEFAULT_READ_OPTIONS}).

%%-----------------------------------------------------------------------------
%% @param   Pin         The pin from which to take ADC measurement
%% @param   ReadOptions Extra options
%% @returns {ok, {RawValue, MilliVolts}} | {error, Reason}
%% @doc     Take a reading from an ADC pin using the supplied options.
%%
%% This convenience function is used to take a measurement from a previously
%% started adc pin, using the supplied read options parameter.
%%
%% The Options parameter may be used to specify the behavior of the read
%% operation.
%%
%% If the ReadOptions contains the atom `raw', then the raw value will be returned
%% in the first element of the returned tuple.  Otherwise, this element will be the
%% atom `undefined'.
%%
%% If the ReadOptions contains the atom `voltage', then the voltage value will be returned
%% in millivolts in the second element of the returned tuple.  Otherwise, this element will
%% be the atom `undefined'.
%%
%% You may specify the number of samples to be taken and averaged over using the tuple
%% `{samples, Samples::pos_integer()}', the default is `64'.
%%
%% If the error `Reason' is timeout and the adc channel is on unit 2 then WiFi is
%% likely enabled and adc2 readings may be blocked until there is less network
%% traffic. On and ESP32 classic the results for unit 2 will always be
%% `{error, timeout}' if wifi is enabled.
%%
%% This function cannot be used in an application that uses the low level nif APIs.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Pin :: adc_pin(), ReadOptions :: read_options()) ->
    {ok, Result :: reading()} | {error, Reason :: term()}.
read(Pin, ReadOptions) ->
    gen_server:call(adc_driver, {read, Pin, ReadOptions}).

%%
%% gen_server
%%

-record(state, {
    handle,
    pins = #{}
}).

%% @hidden
init([]) ->
    try ?MODULE:init() of
        {ok, UnitHandle} ->
            {ok, #state{handle = UnitHandle}};
        Error ->
            Error
    catch
        _E:Reason ->
            {error, Reason}
    end.

%% @hidden
handle_call(stop, _From, State) ->
    case do_stop_driver(State) of
        {ok, NewState} ->
            {stop, normal, ok, NewState};
        {Error, NewState} ->
            {stop, normal, {error, Error}, NewState}
    end;
handle_call({stop, Pin}, _From, State) ->
    {Result, NewState} = do_deinit_pin(Pin, State),
    {reply, Result, NewState};
handle_call({acquire, Pin, Bits, Atten}, _From, State) ->
    {Result, NewState} = do_config_pin({Pin, Bits, Atten}, State),
    {reply, Result, NewState};
handle_call({read, Pin, Options}, _From, State) ->
    {reply, do_take_reading(maps:get(Pin, State#state.pins), State#state.handle, Options), State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% private fun
%%

%% private
get_adc_pid() ->
    case erlang:whereis(adc_driver) of
        undefined ->
            case gen_server:start_link({local, adc_driver}, ?MODULE, [], []) of
                {ok, Pid} -> Pid;
                Err -> erlang:throw(Err)
            end;
        Pid when is_pid(Pid) ->
            Pid
    end.

% private
validate_pin_options(Options) ->
    Bits = proplists:get_value(bitwidth, Options, bit_max),
    Atten = proplists:get_value(atten, Options, db_11),
    {Bits, Atten}.

%private
do_config_pin({Pin, Bits, Atten}, State) ->
    try ?MODULE:acquire(Pin, State#state.handle, Bits, Atten) of
        {ok, ChanRsrc} ->
            {ok, State#state{pins = maps:put(Pin, ChanRsrc, State#state.pins)}};
        Error ->
            io:format("[~p] adc_driver: failed to acquire pin, error: ~p~n", [
                erlang:monotonic_time(millisecond), Error
            ]),
            {Error, State}
    catch
        _E:Reason ->
            {{error, Reason}, State}
    end.

% private
do_take_reading(ChannelHandle, UnitHandle, Options) ->
    try ?MODULE:sample(ChannelHandle, UnitHandle, Options) of
        Result -> Result
    catch
        _E:Reason ->
            {error, Reason}
    end.

% private
do_deinit_pin(Pin, State) ->
    try ?MODULE:release_channel(maps:get(Pin, State#state.pins)) of
        ok ->
            {ok, State#state{pins = maps:remove(Pin, State#state.pins)}};
        Error ->
            {Error, State#state{pins = maps:remove(Pin, State#state.pins)}}
    catch
        _E:Reason ->
            {{error, Reason}, State#state{pins = maps:remove(Pin, State#state.pins)}}
    end.

% private
do_stop_driver(State) ->
    Pins = maps:keys(State#state.pins),
    case Pins of
        [] ->
            NewState = State;
        _ ->
            NewState = stop_pins_from_list(Pins, State)
    end,
    try ?MODULE:deinit(NewState#state.handle) of
        ok ->
            {ok, NewState#state{handle = undefined}};
        Error ->
            {Error, NewState#state{handle = undefined}}
    catch
        _E:Reason ->
            {{error, Reason}, NewState#state{handle = undefined}}
    end.

% private
stop_pins_from_list([], State) ->
    {ok, State};
stop_pins_from_list([Pin | List], State) ->
    {ok, NewState} = do_deinit_pin(Pin, State),
    stop_pins_from_list(List, NewState).
