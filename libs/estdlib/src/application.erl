%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(application).
-export([get_env/2, get_env/3]).
-export_type([start_type/0]).

-type start_type() :: normal | {takeover, Node :: node()} | {failover, Node :: node()}.

%%-----------------------------------------------------------------------------
%% @param   Application application to get the parameter value of
%% @param   Parameter parameter to get the value of
%% @returns undefined
%% @doc     Retrieve the value of the configuration parameter `Parameter' for
%%          application `Application' or `undefined' if not found.
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(Application :: atom(), Parameter :: atom()) -> any().
get_env(Application, Parameter) ->
    get_env(Application, Parameter, undefined).

%%-----------------------------------------------------------------------------
%% @param   Application application to get the parameter value of
%% @param   Parameter parameter to get the value of
%% @param   Default default value if parameter is not found
%% @returns default value
%% @doc     Retrieve the value of the configuration parameter `Parameter' for
%%          application `Application' or `Default' if not found.
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(Application :: atom(), Parameter :: atom(), Default :: any()) -> any().
get_env(_Application, _Parameter, Default) -> Default.
