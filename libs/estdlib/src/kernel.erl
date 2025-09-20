%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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
%% @doc Root supervisor for the kernel application.
%% @end
%%-----------------------------------------------------------------------------
-module(kernel).
-behaviour(application).
-behaviour(supervisor).

% application behavior
-export([
    start/2,
    stop/1
]).

% supervisor behavior
-export([
    init/1
]).

%% @doc Start kernel application
-spec start(StartType :: application:start_type(), StartArgs :: any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, []) ->
    supervisor:start_link({local, kernel_sup}, ?MODULE, []).

%% @hidden
stop(_State) ->
    ok.

%% @hidden
init([]) ->
    ChildrenSpec = [
        #{
            id => code_server,
            start => {code_server, start_link, []},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [code_server]
        }
    ],
    SupFlags = #{strategy => one_for_all},
    {ok, {SupFlags, ChildrenSpec}}.
