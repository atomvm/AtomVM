%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
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
%% @doc AtomVM UART interface for STM32
%%
%% This module provides an interface to the UART hardware on STM32 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API (STM32 HAL)</b>
%% {@link init/1}, {@link deinit/1}, {@link write/3}, {@link read/3},
%% {@link abort/1}, {@link get_state/1}, {@link get_error/1},
%% {@link halfduplex_init/1}, {@link halfduplex_enable_tx/1},
%% {@link halfduplex_enable_rx/1}.
%% These map directly to the corresponding HAL_UART_* functions.
%%
%% <b>High-level API (`uart_hal' behavior)</b>
%% {@link open/1}, {@link open/2}, {@link close/1}, {@link read/1},
%% {@link read/2}, {@link write/2}.
%% @end
%%-----------------------------------------------------------------------------
-module(uart).
-behaviour(uart_hal).

%% High-level API (uart_hal behaviour)
-export([open/1, open/2, close/1, read/1, read/2, write/2]).

%% Low-level API (STM32 HAL)
-export([
    init/1,
    deinit/1,
    write/3,
    read/3,
    abort/1,
    get_state/1,
    get_error/1,
    halfduplex_init/1,
    halfduplex_enable_tx/1,
    halfduplex_enable_rx/1
]).

-type peripheral() :: 1..9.
-type uart_resource() :: reference().
-type uart() :: pid().
-type uart_state() :: reset | ready | busy | busy_tx | busy_rx | busy_tx_rx | error | timeout.
-type pin() :: {Bank :: atom(), PinNum :: 0..15}.

-type uart_config() :: #{
    peripheral => peripheral(),
    tx => pin(),
    rx => pin(),
    speed => pos_integer(),
    data_bits => 7 | 8 | 9,
    stop_bits => 1 | 2,
    parity => none | odd | even,
    af => non_neg_integer()
}.

-type halfduplex_config() :: #{
    peripheral => peripheral(),
    tx => pin(),
    speed => pos_integer(),
    data_bits => 7 | 8 | 9,
    stop_bits => 1 | 2,
    parity => none | odd | even,
    af => non_neg_integer()
}.

-export_type([
    uart/0, uart_resource/0, peripheral/0, uart_state/0, pin/0, uart_config/0, halfduplex_config/0
]).

-define(DEFAULT_SPEED, 115200).
-define(DEFAULT_DATA_BITS, 8).
-define(DEFAULT_STOP_BITS, 1).
-define(DEFAULT_PARITY, none).
-define(DEFAULT_AF, 7).
-define(DEFAULT_PERIPHERAL, 1).
-define(DEFAULT_TIMEOUT_MS, 5000).

%%-----------------------------------------------------------------------------
%% High-level API (uart_hal behaviour)
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param Opts UART configuration options
%% @returns UART handle (pid)
%% @doc Open a UART connection with the given options.
%%
%% Options:
%% <ul>
%% <li>`{tx, Pin}' - TX pin (required for full-duplex)</li>
%% <li>`{rx, Pin}' - RX pin (required for full-duplex)</li>
%% <li>`{speed, 115200}' - Baud rate (default: 115200)</li>
%% <li>`{data_bits, 8}' - Data bits: 7, 8, or 9 (default: 8)</li>
%% <li>`{stop_bits, 1}' - Stop bits: 1 or 2 (default: 1)</li>
%% <li>`{parity, none}' - Parity: none, odd, or even (default: none)</li>
%% <li>`{peripheral, 1}' - UART peripheral number (default: 1)</li>
%% <li>`{af, 7}' - Alternate function number (default: 7)</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: [{atom(), term()}]) -> uart() | {error, term()}.
open(Opts) ->
    Config = parse_opts(Opts),
    case ?MODULE:init(Config) of
        {ok, Resource} ->
            spawn_link(fun() -> loop(Resource) end);
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param Name UART peripheral name (unused, for compatibility)
%% @param Opts UART configuration options
%% @returns UART handle (pid)
%% @doc Open a UART connection with the given options.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Opts :: [{atom(), term()}]) -> uart() | {error, term()}.
open(_Name, Opts) ->
    open(Opts).

%%-----------------------------------------------------------------------------
%% @param UART UART handle created via `open/1' or `open/2'
%% @returns `ok'
%% @doc Close the UART connection.
%% @end
%%-----------------------------------------------------------------------------
-spec close(UART :: uart()) -> ok.
close(Pid) when is_pid(Pid) ->
    call(Pid, close).

%%-----------------------------------------------------------------------------
%% @param UART UART handle created via `open/1' or `open/2'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read data from the UART with default timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read(UART :: uart()) -> {ok, binary()} | {error, term()}.
read(Pid) when is_pid(Pid) ->
    call(Pid, {read, ?DEFAULT_TIMEOUT_MS}).

%%-----------------------------------------------------------------------------
%% @param UART UART handle created via `open/1' or `open/2'
%% @param Timeout Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read data from the UART with the specified timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read(UART :: uart(), Timeout :: pos_integer()) -> {ok, binary()} | {error, term()}.
read(Pid, Timeout) when is_pid(Pid), is_integer(Timeout), Timeout > 0 ->
    call(Pid, {read, Timeout}).

%%-----------------------------------------------------------------------------
%% @param UART UART handle created via `open/1' or `open/2'
%% @param Data Data to write (binary or iolist)
%% @returns `ok' or `{error, Reason}'
%% @doc Write data to the UART.
%% @end
%%-----------------------------------------------------------------------------
-spec write(UART :: uart(), Data :: iodata()) -> ok | {error, term()}.
write(Pid, Data) when is_pid(Pid) ->
    call(Pid, {write, Data, ?DEFAULT_TIMEOUT_MS}).

%%-----------------------------------------------------------------------------
%% Low-level API (STM32 HAL)
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param Config UART configuration map
%% @returns `{ok, Resource}'
%% @doc Initialize the UART HW block (HAL_UART_Init).
%%
%% The Config map should contain:
%% <ul>
%% <li>`{tx, Pin}' - TX pin (required)</li>
%% <li>`{rx, Pin}' - RX pin (required)</li>
%% <li>`{peripheral, 1}' - UART peripheral number (default: 1)</li>
%% <li>`{speed, 115200}' - Baud rate (default: 115200)</li>
%% <li>`{data_bits, 8}' - Data bits: 7, 8, or 9 (default: 8)</li>
%% <li>`{stop_bits, 1}' - Stop bits: 1 or 2 (default: 1)</li>
%% <li>`{parity, none}' - Parity: none, odd, or even (default: none)</li>
%% <li>`{af, 7}' - Alternate function number (default: 7)</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec init(Config :: uart_config() | [{atom(), term()}]) -> {ok, uart_resource()}.
init(_Config) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @returns `ok'
%% @doc Disable the UART HW block (HAL_UART_DeInit).
%% @end
%%-----------------------------------------------------------------
-spec deinit(Resource :: uart_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @param Data Data to write
%% @param TimeoutMs Timeout in milliseconds or `infinity'
%% @returns Number of bytes written or `{error, Reason}'
%% @doc Write data (HAL_UART_Transmit).
%% @end
%%-----------------------------------------------------------------
-spec write(Resource :: uart_resource(), Data :: binary(), TimeoutMs :: timeout()) ->
    non_neg_integer() | {error, term()}.
write(_Resource, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @param Count Number of bytes to read
%% @param TimeoutMs Timeout in milliseconds or `infinity'
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read data (HAL_UART_Receive).
%% @end
%%-----------------------------------------------------------------
-spec read(Resource :: uart_resource(), Count :: non_neg_integer(), TimeoutMs :: timeout()) ->
    {ok, binary()} | {error, term()}.
read(_Resource, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @returns `ok' or `{error, Reason}'
%% @doc Abort ongoing UART transfer (HAL_UART_Abort).
%% @end
%%-----------------------------------------------------------------
-spec abort(Resource :: uart_resource()) -> ok | {error, term()}.
abort(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @returns UART state atom
%% @doc Get the UART state (HAL_UART_GetState).
%% @end
%%-----------------------------------------------------------------
-spec get_state(Resource :: uart_resource()) -> uart_state().
get_state(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource returned by `init/1'
%% @returns Error code as integer
%% @doc Get the UART error (HAL_UART_GetError).
%% @end
%%-----------------------------------------------------------------
-spec get_error(Resource :: uart_resource()) -> non_neg_integer().
get_error(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Config Half-duplex UART configuration
%% @returns `{ok, Resource}'
%% @doc Initialize UART in half-duplex mode (HAL_HalfDuplex_Init).
%% @end
%%-----------------------------------------------------------------
-spec halfduplex_init(Config :: halfduplex_config() | [{atom(), term()}]) -> {ok, uart_resource()}.
halfduplex_init(_Config) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource from `halfduplex_init/1'
%% @returns `ok' or `error'
%% @doc Enable transmitter (HAL_HalfDuplex_EnableTransmitter).
%% @end
%%-----------------------------------------------------------------
-spec halfduplex_enable_tx(Resource :: uart_resource()) -> ok | error.
halfduplex_enable_tx(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% @param Resource UART resource from `halfduplex_init/1'
%% @returns `ok' or `error'
%% @doc Enable receiver (HAL_HalfDuplex_EnableReceiver).
%% @end
%%-----------------------------------------------------------------
-spec halfduplex_enable_rx(Resource :: uart_resource()) -> ok | error.
halfduplex_enable_rx(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------
%% Internal helpers
%%-----------------------------------------------------------------

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

loop(Resource) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply} ->
                    From ! {Ref, Reply},
                    loop(Resource)
            end
    end.

handle_request(Resource, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(Resource, {read, Timeout}) ->
    Result = ?MODULE:read(Resource, 1, Timeout),
    {reply, Result};
handle_request(Resource, {write, Data, Timeout}) ->
    Bin = iolist_to_binary(Data),
    case ?MODULE:write(Resource, Bin, Timeout) of
        N when is_integer(N) -> {reply, ok};
        {error, _} = Error -> {reply, Error}
    end.

parse_opts(Opts) ->
    Tx = proplists:get_value(tx, Opts),
    Rx = proplists:get_value(rx, Opts),
    Tx =:= undefined andalso error({missing_required_option, tx}),
    Rx =:= undefined andalso error({missing_required_option, rx}),
    Speed = proplists:get_value(speed, Opts, ?DEFAULT_SPEED),
    DataBits = proplists:get_value(data_bits, Opts, ?DEFAULT_DATA_BITS),
    StopBits = proplists:get_value(stop_bits, Opts, ?DEFAULT_STOP_BITS),
    Parity = proplists:get_value(parity, Opts, ?DEFAULT_PARITY),
    Peripheral = proplists:get_value(peripheral, Opts, ?DEFAULT_PERIPHERAL),
    AF = proplists:get_value(af, Opts, ?DEFAULT_AF),
    [
        {peripheral, Peripheral},
        {tx, Tx},
        {rx, Rx},
        {af, AF},
        {speed, Speed},
        {data_bits, DataBits},
        {stop_bits, StopBits},
        {parity, parity_to_int(Parity)}
    ].

parity_to_int(none) -> 0;
parity_to_int(odd) -> 1;
parity_to_int(even) -> 2.
