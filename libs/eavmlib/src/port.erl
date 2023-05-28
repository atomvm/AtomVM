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
%% rely on a `port' interface rather than `niffs'.
%%
%% The port driver should be initialized with:
%% `open_port({spawn, "Name"}, Param)'
%% Where Name is an atom(), and is the name of the driver. The return from `open_port/2'
%% will be the Pid that will be required for future `port:call/2' or `port:call/3' use.
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
%% @param   Pid Pid to which to send messages
%% @param   Message the message to send
%% @returns term() | {error, Reason}.
%% @doc     Send a message to a given port driver pid.
%%
%% This function is used to send a message to an open port divers pid and will
%% return a term or `{error, Reason}'.
%% @end
%%-----------------------------------------------------------------------------
-spec call(Pid :: pid(), Message :: term()) -> term() | {error, Reason :: term()}.
call(Pid, Message) ->
    case erlang:is_process_alive(Pid) of
        false ->
            {error, noproc};
        true ->
            Ref = erlang:make_ref(),
            Pid ! {self(), Ref, Message},
            receive
                out_of_memory -> out_of_memory;
                {Ref, Reply} -> Reply
            end
    end.

%%-----------------------------------------------------------------------------
%% @param   Pid Pid to which to send messages
%% @param   Message the message to send
%% @param   TimeoutMs the timeout value in milliseconds
%% @returns term() | {error, Reason}.
%% @doc     Send a message to a given port driver pid with a timeout.
%%
%% This function is used to send a message to an open port divers pid and will return
%% a term or `{error, Reason}', or`{error, timeout}' if the TimeoutMs is reached first.
%% @end
%%-----------------------------------------------------------------------------
-spec call(Pid :: pid(), Message :: term(), TimeoutMs :: non_neg_integer()) ->
    term() | {error, timeout}.
call(Pid, Message, TimeoutMs) ->
    case erlang:is_process_alive(Pid) of
        false ->
            {error, noproc};
        true ->
            Ref = erlang:make_ref(),
            Pid ! {self(), Ref, Message},
            receive
                out_of_memory -> out_of_memory;
                {Ref, Reply} -> Reply
            after TimeoutMs ->
                {error, timeout}
            end
    end.
