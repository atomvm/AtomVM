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
%% @doc USB CDC implementation for STM32 using TinyUSB.
%%
%% This module implements the {@link uart_hal} behaviour over USB CDC ACM.
%%
%% Requires TinyUSB to be integrated into the STM32 build and
%% `AVM_USB_CDC_PORT_DRIVER_ENABLED' to be defined.
%%
%% Example:
%% ```
%% USB = usb_cdc:open([]),
%% ok = usb_cdc:write(USB, <<"hello">>),
%% case usb_cdc:read(USB, 1000) of
%%     {ok, Data} -> io:format("Got: ~p~n", [Data]);
%%     {error, timeout} -> io:format("No data~n")
%% end,
%% ok = usb_cdc:close(USB).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(usb_cdc).

-behaviour(uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).

%%-----------------------------------------------------------------------------
%% @param Name peripheral name (ignored)
%% @param Opts configuration options
%% @returns Port handle or error
%% @doc Open the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Opts :: list()) ->
    port() | {error, term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%%-----------------------------------------------------------------------------
%% @param Opts configuration options
%% @returns Port handle or error
%% @doc Open the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: list()) -> port() | {error, term()}.
open(Opts) ->
    open_port({spawn, "usb_cdc"}, Opts).

%%-----------------------------------------------------------------------------
%% @param Port handle returned by open
%% @returns ok or error
%% @doc Close the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Port :: port()) -> ok | {error, term()}.
close(Port) when is_port(Port) ->
    port:call(Port, close).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read data from USB CDC. Blocks until data is available.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port()) -> {ok, binary()} | {error, term()}.
read(Port) when is_port(Port) ->
    port:call(Port, read).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, timeout}' or `{error, Reason}'
%% @doc Read data from USB CDC with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port(), Timeout :: pos_integer()) ->
    {ok, binary()} | {error, term()}.
read(Port, Timeout) when is_port(Port) ->
    Self = self(),
    MonitorRef = monitor(port, Port),
    Port ! {'$call', {Self, MonitorRef}, read},
    Result =
        receive
            {MonitorRef, Reply} ->
                Reply;
            {'DOWN', MonitorRef, port, Port, normal} ->
                {error, noproc};
            {'DOWN', MonitorRef, port, Port, Reason} ->
                {error, Reason}
        after Timeout ->
            % Pass MonitorRef so the driver only clears its slot if it
            % still belongs to us; otherwise a cancel racing with a
            % just-completed read would wipe a freshly-installed reader.
            port:call(Port, {cancel_read, MonitorRef}),
            % Drop any reply that landed between the timeout and the
            % cancel so it doesn't sit in the mailbox forever.
            receive
                {MonitorRef, _LateReply} -> ok
            after 0 -> ok
            end,
            {error, timeout}
        end,
    demonitor(MonitorRef, [flush]),
    Result.

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Data to write
%% @returns ok or error
%% @doc Write data to USB CDC.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Port :: port(), Data :: iodata()) -> ok | {error, term()}.
write(Port, Data) when is_port(Port) ->
    port:call(Port, {write, Data}).
