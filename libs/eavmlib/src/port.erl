%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
% Copyright 2021 Fred Dushin <fred@dushin.net>
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
%% @doc AtomVM port driver APIs
%%
%% This module contains functions that are intended to be used by drivers that
%% rely on a `port' interface rather than `nifs'.
%%
%% The port driver should be initialized with:
%% `open_port({spawn, "Name"}, Param)'
%% Where Name is an atom(), and is the name of the driver. The return from `open_port/2'
%% will be the Port that will be required for future `port:call/2' or `port:call/3' use.
%%
%% Examples:
%% ```open_port({spawn, "i2c"}, Param)'''
%% or
%% ```open_port({spawn, "spi"}, Params)'''
%% @end
%%-----------------------------------------------------------------------------
-module(port).

-export([call/2, call/3]).

%%-----------------------------------------------------------------------------
%% @param   Port Port to which to send messages
%% @param   Message the message to send
%% @returns term() | {error, Reason}.
%% @doc     Send a message to a given port driver.
%%
%% This function is used to send a message to an open port drivers port and will
%% return a term or `{error, Reason}'.
%% @end
%%-----------------------------------------------------------------------------
-spec call(Port :: port(), Message :: term()) -> term().
call(Port, Message) ->
    call(Port, Message, infinity).

%%-----------------------------------------------------------------------------
%% @param   Port Port to which to send messages
%% @param   Message the message to send
%% @param   Timeout the timeout value in milliseconds
%% @returns term() | {error, Reason}.
%% @doc     Send a message to a given port driver with a timeout.
%%
%% This function is used to send a message to an open port drivers port and will return
%% a term or `{error, Reason}', or`{error, timeout}' if the TimeoutMs is reached first.
%% @end
%%-----------------------------------------------------------------------------
-spec call(Port :: port(), Message :: term(), Timeout :: timeout()) -> term() | {error, timeout}.
call(Port, Message, Timeout) ->
    MonitorRef = monitor(port, Port),
    Port ! {'$call', {self(), MonitorRef}, Message},
    Result =
        receive
            {'DOWN', MonitorRef, port, Port, normal} ->
                {error, noproc};
            {'DOWN', MonitorRef, port, Port, Reason} ->
                {error, Reason};
            out_of_memory ->
                out_of_memory;
            {MonitorRef, Ret} ->
                Ret
        after Timeout ->
            {error, timeout}
        end,
    demonitor(MonitorRef, [flush]),
    Result.
