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
%% @doc AtomVM UART interface for RP2 (Pico)
%%
%% This module provides an interface to the UART hardware on RP2 platforms.
%%
%% Two API levels are provided:
%%
%% <b>Low-level API</b>
%% {@link init/2}, {@link deinit/1}, {@link set_baudrate/2},
%% {@link set_format/4}, {@link set_hw_flow/3},
%% {@link write_blocking/2}, {@link read_blocking/2},
%% {@link is_readable/1}, {@link is_readable_within_us/2}.
%% These operate on a bare resource reference returned by {@link init/2}.
%% Pin muxing must be done separately via `gpio:set_function/2'.
%%
%% <b>High-level API (`uart_hal' behavior)</b>
%% {@link open/1}, {@link open/2}, {@link close/1},
%% {@link read/1}, {@link read/2}, {@link write/2}.
%% {@link open/1} handles pin setup automatically.
%% @end
%%-----------------------------------------------------------------------------
-module(uart).

-behaviour(uart_hal).

%% High-level API (uart_hal behaviour)
-export([
    open/1, open/2,
    close/1,
    read/1, read/2,
    write/2
]).

%% Low-level API (Pico SDK)
-export([
    init/2,
    deinit/1,
    set_baudrate/2,
    set_format/4,
    set_hw_flow/3,
    write_blocking/2,
    read_blocking/2,
    is_readable/1,
    is_readable_within_us/2
]).

-type pin() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type peripheral() :: 0 | 1.
-type param() ::
    {tx, pin()}
    | {rx, pin()}
    | {rts, pin()}
    | {cts, pin()}
    | {speed, pos_integer()}
    | {data_bits, 5..8}
    | {stop_bits, 1 | 2}
    | {parity, none | even | odd}
    | {flow_control, none | hardware}
    | {peripheral, peripheral() | string() | binary()}.
-type params() :: [param()].
-type uart_resource() :: reference().
-type uart() :: pid().

-export_type([uart/0, uart_resource/0]).

-define(DEFAULT_SPEED, 115200).
-define(DEFAULT_DATA_BITS, 8).
-define(DEFAULT_STOP_BITS, 1).
-define(DEFAULT_PARITY, none).
-define(DEFAULT_FLOW_CONTROL, none).
-define(DEFAULT_PERIPHERAL, 0).

%% ---------------------------------------------------------------------------
%% High-level API (uart_hal behaviour)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Name UART peripheral name (`"UART0"' or `"UART1"')
%% @param   Params Initialization parameters
%% @returns UART handle (pid)
%% @doc     Open a connection to the UART driver
%%
%%          This function provides compatibility with the ESP32 UART driver
%%          interface. The `Name' parameter is converted to a peripheral
%%          number and prepended to the parameters.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Params :: params()) -> uart().
open(Name, Params) ->
    open([{peripheral, Name} | Params]).

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns UART handle (pid)
%% @doc     Open a connection to the UART driver
%%
%%          This function configures the GPIO pins for UART function,
%%          initializes the UART peripheral, and configures data format
%%          and flow control.
%%
%%          Supported parameters:
%%          <ul>
%%              <li>`{tx, Pin}' - the TX pin number (required)</li>
%%              <li>`{rx, Pin}' - the RX pin number (required)</li>
%%              <li>`{rts, Pin}' - the RTS pin number (optional)</li>
%%              <li>`{cts, Pin}' - the CTS pin number (optional)</li>
%%              <li>`{speed, Baud}' - the baud rate (default: 115200)</li>
%%              <li>`{data_bits, 5..8}' - data bits per character (default: 8)</li>
%%              <li>`{stop_bits, 1 | 2}' - stop bits (default: 1)</li>
%%              <li>`{parity, none | even | odd}' - parity (default: none)</li>
%%              <li>`{flow_control, none | hardware}' - flow control (default: none)</li>
%%              <li>`{peripheral, 0 | 1 | "UART0" | "UART1"}' - UART peripheral (default: 0)</li>
%%          </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params :: params()) -> uart().
open(Params) ->
    TX = proplists:get_value(tx, Params),
    RX = proplists:get_value(rx, Params),
    RTS = proplists:get_value(rts, Params, undefined),
    CTS = proplists:get_value(cts, Params, undefined),
    Speed = proplists:get_value(speed, Params, ?DEFAULT_SPEED),
    DataBits = proplists:get_value(data_bits, Params, ?DEFAULT_DATA_BITS),
    StopBits = proplists:get_value(stop_bits, Params, ?DEFAULT_STOP_BITS),
    Parity = proplists:get_value(parity, Params, ?DEFAULT_PARITY),
    FlowControl = proplists:get_value(flow_control, Params, ?DEFAULT_FLOW_CONTROL),
    PeripheralParam = proplists:get_value(peripheral, Params, ?DEFAULT_PERIPHERAL),
    Peripheral = normalize_peripheral(PeripheralParam),
    gpio:set_function(TX, uart),
    gpio:set_function(RX, uart),
    maybe_set_uart_function(RTS),
    maybe_set_uart_function(CTS),
    {ok, {_ActualBaudrate, Resource}} = ?MODULE:init(Peripheral, Speed),
    ?MODULE:set_format(Resource, DataBits, StopBits, Parity),
    case FlowControl of
        hardware ->
            ?MODULE:set_hw_flow(Resource, CTS =/= undefined, RTS =/= undefined);
        none ->
            ok
    end,
    spawn_link(fun() -> loop(Resource) end).

%%-----------------------------------------------------------------------------
%% @param   UART UART handle created via `open/1'
%% @returns `ok'
%% @doc     Close the connection to the UART driver and free resources.
%% @end
%%-----------------------------------------------------------------------------
-spec close(UART :: uart()) -> ok | {error, Reason :: term()}.
close(Pid) ->
    call(Pid, close).

%%-----------------------------------------------------------------------------
%% @param   UART UART handle created via `open/1'
%% @returns `{ok, Data}' or `{error, timeout}'
%% @doc     Read currently available data from the UART FIFO.
%%
%%          Returns `{error, timeout}' immediately if no data is available.
%% @end
%%-----------------------------------------------------------------------------
-spec read(UART :: uart()) -> {ok, binary()} | {error, Reason :: term()}.
read(Pid) ->
    call(Pid, read).

%%-----------------------------------------------------------------------------
%% @param   UART UART handle created via `open/1'
%% @param   Timeout Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, timeout}'
%% @doc     Read data from the UART with a timeout.
%%
%%          Waits up to `Timeout' milliseconds for data to become available.
%%          Once data arrives, reads all currently available bytes from the
%%          FIFO.
%% @end
%%-----------------------------------------------------------------------------
-spec read(UART :: uart(), Timeout :: pos_integer()) ->
    {ok, binary()} | {error, Reason :: term()}.
read(Pid, Timeout) ->
    call(Pid, {read, Timeout}).

%%-----------------------------------------------------------------------------
%% @param   UART UART handle created via `open/1'
%% @param   Data iodata to write
%% @returns `ok'
%% @doc     Write data to the UART.
%% @end
%%-----------------------------------------------------------------------------
-spec write(UART :: uart(), Data :: iodata()) -> ok | {error, Reason :: term()}.
write(Pid, Data) ->
    case is_iolist(Data) of
        true -> call(Pid, {write, Data});
        false -> error(badarg)
    end.

%% ---------------------------------------------------------------------------
%% Low-level API (Pico SDK)
%% ---------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @param   Peripheral UART peripheral number (0 or 1)
%% @param   Baudrate Baudrate in Hz (e.g. 115200)
%% @returns `{ok, {ActualBaudrate, Resource}}'
%% @doc     Initialize the UART HW block.
%%
%%          Pin muxing must be done separately via `gpio:set_function/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Peripheral :: peripheral(), Baudrate :: freq_hz()) ->
    {ok, {ActualBaudrate :: freq_hz(), Resource :: uart_resource()}}.
init(_Peripheral, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @returns `ok'
%% @doc     Disable the UART HW block.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: uart_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   Baudrate Baudrate in Hz
%% @returns `{ok, ActualBaudrate}'
%% @doc     Set UART baudrate.
%% @end
%%-----------------------------------------------------------------------------
-spec set_baudrate(Resource :: uart_resource(), Baudrate :: freq_hz()) ->
    {ok, ActualBaudrate :: freq_hz()}.
set_baudrate(_Resource, _Baudrate) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   DataBits Number of data bits per character (5..8)
%% @param   StopBits Number of stop bits (1 or 2)
%% @param   Parity Parity setting (`none', `even', or `odd')
%% @returns `ok'
%% @doc     Set UART data format.
%% @end
%%-----------------------------------------------------------------------------
-spec set_format(
    Resource :: uart_resource(), DataBits :: 5..8, StopBits :: 1 | 2, Parity :: none | even | odd
) -> ok.
set_format(_Resource, _DataBits, _StopBits, _Parity) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   CTS Enable CTS flow control
%% @param   RTS Enable RTS flow control
%% @returns `ok'
%% @doc     Set UART hardware flow control.
%%
%%          The corresponding CTS/RTS pins must have been set to UART
%%          function via `gpio:set_function/2' before enabling.
%% @end
%%-----------------------------------------------------------------------------
-spec set_hw_flow(Resource :: uart_resource(), CTS :: boolean(), RTS :: boolean()) -> ok.
set_hw_flow(_Resource, _CTS, _RTS) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   Data Binary data to write
%% @returns `ok'
%% @doc     Write to UART, blocking until all data is sent.
%% @end
%%-----------------------------------------------------------------------------
-spec write_blocking(Resource :: uart_resource(), Data :: binary()) -> ok.
write_blocking(_Resource, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   Count Number of bytes to read
%% @returns `{ok, Data}'
%% @doc     Read from UART, blocking until `Count' bytes have been received.
%% @end
%%-----------------------------------------------------------------------------
-spec read_blocking(Resource :: uart_resource(), Count :: non_neg_integer()) ->
    {ok, binary()}.
read_blocking(_Resource, _Count) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @returns `true' if data is available, `false' otherwise
%% @doc     Check if UART has data available to read.
%% @end
%%-----------------------------------------------------------------------------
-spec is_readable(Resource :: uart_resource()) -> boolean().
is_readable(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource UART resource returned by `init/2'
%% @param   Us Maximum wait time in microseconds
%% @returns `true' if data became available, `false' on timeout
%% @doc     Wait for UART data with a microsecond timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec is_readable_within_us(Resource :: uart_resource(), Us :: non_neg_integer()) -> boolean().
is_readable_within_us(_Resource, _Us) ->
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

%% @private
handle_request(Resource, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(Resource, read) ->
    case ?MODULE:is_readable(Resource) of
        true ->
            Data = read_available(Resource),
            {reply, {ok, Data}};
        false ->
            {reply, {error, timeout}}
    end;
handle_request(Resource, {read, Timeout}) ->
    Deadline = erlang:system_time(millisecond) + Timeout,
    case poll_readable(Resource, Deadline) of
        true ->
            Data = read_available(Resource),
            {reply, {ok, Data}};
        false ->
            {reply, {error, timeout}}
    end;
handle_request(Resource, {write, Data}) ->
    Bin = erlang:iolist_to_binary(Data),
    ?MODULE:write_blocking(Resource, Bin),
    {reply, ok}.

%% @private
read_available(Resource) ->
    read_available(Resource, []).

%% @private
read_available(Resource, Acc) ->
    {ok, Byte} = ?MODULE:read_blocking(Resource, 1),
    case ?MODULE:is_readable(Resource) of
        true ->
            read_available(Resource, [Byte | Acc]);
        false ->
            erlang:iolist_to_binary(lists:reverse([Byte | Acc]))
    end.

%% @private
poll_readable(Resource, Deadline) ->
    case ?MODULE:is_readable_within_us(Resource, 1000) of
        true ->
            true;
        false ->
            case erlang:system_time(millisecond) >= Deadline of
                true -> false;
                false -> poll_readable(Resource, Deadline)
            end
    end.

%% @private
normalize_peripheral(N) when is_integer(N) -> N;
normalize_peripheral("UART0") -> 0;
normalize_peripheral("UART1") -> 1;
normalize_peripheral(<<"UART0">>) -> 0;
normalize_peripheral(<<"UART1">>) -> 1.

%% @private
maybe_set_uart_function(undefined) -> ok;
maybe_set_uart_function(Pin) -> gpio:set_function(Pin, uart).

%% @private
is_iolist([]) ->
    true;
is_iolist(B) when is_binary(B) ->
    true;
is_iolist(I) when is_integer(I) andalso 0 =< I andalso I =< 255 ->
    true;
is_iolist([H | T]) ->
    case is_iolist(H) of
        true ->
            is_iolist(T);
        false ->
            false
    end;
is_iolist(_) ->
    false.
