%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
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

-module(uart).
-export([open/1, open/2, close/1, read/1, read/2, write/2]).

-type peripheral() :: string() | binary().
% The peripheral `Name' may be one of: `"UART0"' | `"UART1"' | `"UART2"' | `<<"UART0">>' | `<<"UART1">>' | `<<"UART2">>'.

-type uart_opts() :: [
    {tx, Tx_pin :: integer()}
    | {rx, Rx_pin :: integer()}
    | {rts, Rts_pin :: integer()}
    | {cts, Cts_pin :: integer()}
    | {speed, Speed :: pos_integer()}
    | {data_bits, 5..8}
    | {stop_bits, 1 | 2}
    | {event_queue_len, Qlen :: pos_integer()}
    | {flow_control, none | hardware | software}
    | {parity, none | even | odd}
    | {peripheral, peripheral()}
    | []
].

%%-----------------------------------------------------------------------------
%% @param   Name the uart peripheral to be opened
%% @param   Opts uart configuration options
%% @returns Port of the driver.
%% @doc     Open a connection to the UART driver
%%
%%          This function will open a connection to the UART driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: peripheral(), Opts :: uart_opts()) ->
    Port :: port() | {error, _Reason :: term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%%-----------------------------------------------------------------------------
%% @param   Opts uart configuration options
%% @returns Port of the driver.
%% @doc     Open a connection to the UART driver default port
%%
%%          This function will open a connection to the UART driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: uart_opts()) -> Port :: port() | {error, _Reason :: term()}.
open(Opts) ->
    open_port({spawn, "uart"}, migrate_config(Opts)).

%%-----------------------------------------------------------------------------
%% @param   Port of the uart port to be closed
%% @returns ok.
%% @doc     Close a port connection to the UART driver
%%
%%          This function will close the given port connection to the UART driver.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Port :: port()) -> ok | {error, _Reason :: term()}.
close(Port) when is_port(Port) ->
    port:call(Port, close).

%%-----------------------------------------------------------------------------
%% @param   Port of the uart port to be read
%% @returns {ok, Data} or {error, Reason}
%% @doc     Read data from a UART port
%%
%%          This function will return any data that is available, or return
%%          a `{error, timeout}' tuple. The driver will sent the next available
%%          data from the UART driver to the process that made the last read.
%%          Example:
%%          ```
%%          Data = case uart:read(Uart) of
%%              {ok, Binary} -> Binary;
%%              {error, timeout} ->
%%                  receive
%%                      {ok, RecvBinary} -> RecvBinary;
%%                      Error -> error(Error)
%%                  end;
%%              Error -> error(Error)
%%          end,
%%          '''
%%          Any attempt by another (or the same process) to read from uart before the
%%          next uart payload is sent by the driver will result in `{error, ealready}'.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port()) -> {ok, Data :: iodata()} | {error, _Reason :: term()}.
read(Port) when is_port(Port) ->
    port:call(Port, read).

%%-----------------------------------------------------------------------------
%% @param   Port of the uart port to be read
%% @param   Timeout millisecond to wait for data to become available
%% @returns `{ok, Data}', or `{error, Reason}'
%% @doc     Read data from a UART port
%%
%%          This function will return any data that is available within the
%%          timeout period to the process. After the timeout has expired a new
%%          read command may be used regardless of whether the last read was
%%          sent a payload.
%%          Example:
%%          ```
%%          Data = case uart:read(Uart, 3000) of
%%              {ok, Bin} -> Bin;
%%              {error, timeout} -> <<"">>;
%%              Error -> error_handler_fun(Uart, Error)
%%          end,
%%          '''
%%          Any data sent to the esp32 over uart between reads with a timeout will
%%          be lost, so be sure this is what you want. Most applications will want
%%          a single process to read from UART and continue to listen until a payload
%%          is received, and likely pass the payload off for processing and
%%          immediately begin another read.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port(), Timeout :: pos_integer()) ->
    {ok, Data :: iodata()} | {error, _Reason :: term()}.
read(Port, Timeout) when is_port(Port) ->
    case port:call(Port, read, Timeout) of
        {error, timeout} ->
            port:call(Port, cancel_read),
            {error, timeout};
        Result ->
            Result
    end.

%%-----------------------------------------------------------------------------
%% @param   Port of the uart port to be written to
%% @param   Data to be written to the given uart port
%% @returns ok or {error, Reason}
%% @doc     Write data to a UART port
%%
%%          This function will write the given data to the UART port.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Port :: port(), Data :: iodata()) -> ok | {error, _Reason :: term()}.
write(Port, Data) ->
    case is_iolist(Data) andalso is_port(Port) of
        true ->
            port:call(Port, {write, Data});
        false ->
            error(badarg)
    end.

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

migrate_config([]) ->
    [];
migrate_config([{K, V} | T]) ->
    NewK = rename_key(K),
    warn_deprecated(K, NewK),
    NewV = migrate_value(NewK, V),
    [{NewK, NewV} | migrate_config(T)].

migrate_value(peripheral, Peripheral) ->
    validate_peripheral(Peripheral);
migrate_value(_K, V) ->
    V.

rename_key(Key) ->
    case Key of
        rx_pin -> rx;
        tx_pin -> tx;
        rts_pin -> rts;
        cts_pin -> cts;
        Any -> Any
    end.

warn_deprecated(Key, Key) ->
    ok;
warn_deprecated(OldKey, NewKey) ->
    io:format("UART: found deprecated ~p, use ~p instead!!!~n", [OldKey, NewKey]).

validate_peripheral(I) when is_integer(I) ->
    io:format("UART: deprecated integer peripheral is used.~n"),
    I;
validate_peripheral([$U, $A, $R, $T | N] = Value) ->
    try list_to_integer(N) of
        _ -> Value
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(<<"UART", N/binary>> = Value) ->
    try binary_to_integer(N) of
        _ -> Value
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(Value) ->
    throw({bardarg, {peripheral, Value}}).
