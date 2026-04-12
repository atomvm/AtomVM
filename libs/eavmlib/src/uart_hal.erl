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
%% @doc UART Hardware Abstraction Layer behavior
%%
%% This module defines the behavior that platform-specific UART modules
%% must implement. It provides a common interface for UART (Universal
%% Asynchronous Receiver-Transmitter) operations across all supported
%% platforms.
%%
%% ESP32, RP2, STM32 and generic unix platforms provide UART implementations.
%%
%% <h3>Lifecycle</h3>
%%
%% A UART port is opened with `open/1' or `open/2' and closed with
%% `close/1'. The `open/2' variant is a convenience that takes the
%% peripheral name as a separate argument. The returned handle is
%% passed to all subsequent operations.
%%
%% <h3>Reading and writing</h3>
%%
%% <ul>
%% <li>`read/1' - Non-blocking read. Returns `{ok, Data}' if data is
%% available, or `{error, timeout}' if no data is ready.</li>
%% <li>`read/2' - Blocking read with a timeout in milliseconds.
%% Waits up to the specified time for data to arrive.</li>
%% <li>`write/2' - Write data to the UART port.</li>
%% </ul>
%%
%% <h3>Configuration parameters</h3>
%%
%% The `open/1' function accepts a proplist of configuration
%% parameters. Common parameters include:
%%
%% <ul>
%% <li>`{tx, integer()}' - Transmit pin number</li>
%% <li>`{rx, integer()}' - Receive pin number</li>
%% <li>`{speed, pos_integer()}' - Baud rate</li>
%% <li>`{data_bits, 5..8}' - Number of data bits (default: 8)</li>
%% <li>`{stop_bits, 1 | 2}' - Number of stop bits (default: 1)</li>
%% <li>`{parity, none | even | odd}' - Parity mode</li>
%% <li>`{flow_control, none | hardware | software}' - Flow control
%% type</li>
%% <li>`{rts, integer()}' - RTS pin for hardware flow control</li>
%% <li>`{cts, integer()}' - CTS pin for hardware flow control</li>
%% <li>`{peripheral, string() | binary()}' - UART peripheral name
%% (e.g. `"UART0"', `"UART1"', `"UART2"')</li>
%% </ul>
%%
%% <h3>Example</h3>
%%
%% ```
%% UART = uart:open([{tx, 17}, {rx, 16}, {speed, 115200}]),
%% uart:write(UART, <<"Hello\r\n">>),
%% case uart:read(UART, 5000) of
%%     {ok, Data} -> io:format("Received: ~p~n", [Data]);
%%     {error, timeout} -> io:format("No response~n")
%% end,
%% uart:close(UART).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(uart_hal).

-type uart() :: port() | pid() | term().
%% Handle returned by `open/1' or `open/2'.

-type peripheral() :: string() | binary().
%% UART peripheral name (e.g. `"UART0"', `"UART1"').

-type params() :: [term()].
%% Initialization parameters for the UART port.
%% See the module documentation for common parameters.

-export_type([uart/0, peripheral/0, params/0]).

% Open a UART port with the given configuration parameters.
%
% Returns a handle for subsequent read/write operations.
-callback open(Params :: params()) -> uart() | {error, Reason :: term()}.

% Open a UART port on the specified peripheral.
%
% Convenience wrapper that adds `{peripheral, Name}' to the
% parameters and calls `open/1'.
-callback open(Name :: peripheral(), Params :: params()) -> uart() | {error, Reason :: term()}.

% Close a UART port and release its resources.
-callback close(UART :: uart()) -> ok | {error, Reason :: term()}.

% Non-blocking read from the UART port.
%
% Returns `{ok, Data}' if data is available, or `{error, timeout}'
% if no data is ready.
-callback read(UART :: uart()) -> {ok, Data :: iodata()} | {error, Reason :: term()}.

% Blocking read from the UART port with a timeout.
%
% Waits up to `Timeout' milliseconds for data to arrive. Returns
% `{error, timeout}' if no data arrives within the timeout period.
-callback read(UART :: uart(), Timeout :: pos_integer()) ->
    {ok, Data :: iodata()} | {error, Reason :: term()}.

% Write data to the UART port.
-callback write(UART :: uart(), Data :: iodata()) -> ok | {error, Reason :: term()}.
