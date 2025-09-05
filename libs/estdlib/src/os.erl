%
% This file is part of AtomVM.
%
% Copyright 2025 Jakub Gonet <jakub.gonet@swmansion.com>
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
-module(os).

-export([getenv/1]).

%%-----------------------------------------------------------------------------
%% @param   Name name of the environment variable
%% @returns the value of environment variable or false if unset
%% @doc     Get an environment variable value if defined
%% @end
%%-----------------------------------------------------------------------------
-spec getenv(Name :: nonempty_string()) -> nonempty_string() | false.
getenv(_VarName) ->
    erlang:nif_error(undefined).
