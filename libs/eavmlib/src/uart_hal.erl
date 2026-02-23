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
%% must implement. It provides a common interface for UART operations
%% across all supported platforms.
%% @end
%%-----------------------------------------------------------------------------
-module(uart_hal).

-type uart() :: port() | pid() | term().
%% Handle returned by `open/1' or `open/2'.

-type peripheral() :: string() | binary().
%% UART peripheral name.

-type params() :: [term()].
%% Initialization parameters for the UART bus.

-export_type([uart/0, peripheral/0, params/0]).

-callback open(Params :: params()) -> uart() | {error, Reason :: term()}.

-callback open(Name :: peripheral(), Params :: params()) -> uart() | {error, Reason :: term()}.

-callback close(UART :: uart()) -> ok | {error, Reason :: term()}.

-callback read(UART :: uart()) -> {ok, Data :: iodata()} | {error, Reason :: term()}.

-callback read(UART :: uart(), Timeout :: pos_integer()) ->
    {ok, Data :: iodata()} | {error, Reason :: term()}.

-callback write(UART :: uart(), Data :: iodata()) -> ok | {error, Reason :: term()}.
