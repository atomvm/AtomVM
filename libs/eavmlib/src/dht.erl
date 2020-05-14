%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2020 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-----------------------------------------------------------------------------
%% @doc DHT11 and DHT22 support.
%%
%%      Use this module to read temperature and humidity readings from
%%      a DHT11 or DHT22 sensor.
%% @end
%%-----------------------------------------------------------------------------
-module(dht).

-export([
    start/1, start/2, stop/1,
    measure/1
]).
-export([read/1]). %% internal nif APIs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-type pin() :: non_neg_integer().
-type device() :: dht11 | dht22.
-type dht() :: pid().
-type temp() :: non_neg_integer().
-type temp_fractional() :: non_neg_integer().
-type hum() :: non_neg_integer().
-type hum_fractional() :: non_neg_integer().
-type measurement() :: {temp(), temp_fractional(), hum(), hum_fractional()}.

-define(MIN_TIME_BETWEEN_MEASUREMENTS_DHT11_MS, 1000).
-define(MIN_TIME_BETWEEN_MEASUREMENTS_DHT22_MS, 2000).

-record(state, {
    pin :: pin(),
    device :: device(),
    last_measurement = erlang:timestamp()
}).


%%-----------------------------------------------------------------------------
%% @param   Pin     pin from which to read DHT
%% @returns ok | {error, Reason}
%% @doc     Start a DHT.
%%
%%          Equivalent to start(Pin, dht11).
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::pin()) -> {ok, dht()} | {error, Reason::term()}.
start(Pin) ->
    start(Pin, dht11).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read DHT
%% @param   Options     extra options expressed as a
%% @returns ok | {error, Reason}
%% @doc     Start a DHT.
%%
%%
%% @end
%%-----------------------------------------------------------------------------
-spec start(Pin::pin(), Device::device()) -> {ok, dht()} | {error, Reason::term()}.
start(Pin, Device) ->
    gen_server:start(?MODULE, [Pin, Device], []).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Stop the specified DHT.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(DHT::dht()) -> ok.
stop(DHT) ->
    gen_server:call(DHT, stop).

%%-----------------------------------------------------------------------------
%% @param   Pin         pin from which to read DHT
%% @param   Options     extra options expressed as a
%% @returns ok | {error, Reason}
%% @doc     Take a measurement.
%%
%%          This function will return a measurement expressed as a 4-tuple
%%          of elements, including:
%%          <ol>
%%              <li>temperature whole number part</li>
%%              <li>temperature fractional part</li>
%%              <li>humidity whole number part</li>
%%              <li>humidity fractional part</li>
%%          </ol>
%%          Temperature is measured in degrees celcius.  Relative humidity
%%          is expressed as a percentage.
%%
%%          Measurements cannot be taken more frequently than once every
%%          second, for DHT11 devices, and every 2 seconds, for DHT22 devices.
%%          This operation will block until enough time has elapsed before
%%          taking the next measurement.
%% @end
%%-----------------------------------------------------------------------------
-spec measure(DHT::dht()) -> {ok, measurement()} | {error, Reason::term()}.
measure(DHT) ->
    gen_server:call(DHT, measure).

%% @hidden
-spec read(Pin::pin()) -> {ok, binary()} | {error, Reason::term()}.
read(_Pin) ->
    throw(nif_error).


%%
%% gen_server API
%%

%% @hidden
init([Pin, Device]) ->
    {ok, #state{pin=Pin, device=Device}}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(measure, _From, State) ->
    {LastMeasurmentTime, Response} = do_measure(State#state.pin, State#state.device, State#state.last_measurement),
    {reply, Response, State#state{last_measurement=LastMeasurmentTime}};
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

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% internal operations
%%

%% @private
do_measure(Pin, Device, LastMeasurement) ->
    maybe_sleep(Device, LastMeasurement),
    {erlang:timestamp(), do_measure(Pin, Device)}.

%% @private
do_measure(Pin, Device) ->
    case dht:read(Pin) of
        {ok, Measurement} ->
            <<A:8, B:8, C:8, D:8, Parity:8>> = Measurement,
            case (A + B + C + D) rem 256 of
                Parity ->
                    {ok, get_measurement({A, B, C, D}, Device)};
                _ ->
                    {error, {checksum_error, {A, B, C, D}, Parity}}
            end;
        Error ->
            Error
    end.

get_measurement({A, B, C, D}, dht11) ->
    {C, D, A, B};
get_measurement({A, B, C, D}, dht22) ->
    H = (A bsl 8) bor B,
    F = case C band 16#80 of 0 -> 1; 16#80 -> -1 end,
    T = ((C band 16#7F) bsl 8) bor D,
    {F * (T div 10), T rem 10, H div 10, H rem 10}.

maybe_sleep(Device, LastMeasurement) ->
    TimeSinceLastMeasurementMs = timestamp_util:delta_ms(erlang:timestamp(), LastMeasurement),
    MinTimeBetweenMeasurements = case Device of
        dht11 -> ?MIN_TIME_BETWEEN_MEASUREMENTS_DHT11_MS;
        dht22 -> ?MIN_TIME_BETWEEN_MEASUREMENTS_DHT22_MS
    end,
    case TimeSinceLastMeasurementMs < MinTimeBetweenMeasurements of
        true ->
            timer:sleep(MinTimeBetweenMeasurements - TimeSinceLastMeasurementMs);
        _ -> ok
    end.
