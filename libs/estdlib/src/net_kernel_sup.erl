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
%% @doc Supervisor for erlang distribution.
%%
%% This module matches OTP's undocumented erl_distribution module and serves
%% as the supervisor for everything erlang distribution related.
%% @end
%%-----------------------------------------------------------------------------
-module(net_kernel_sup).

-behaviour(supervisor).

% api to net_kernel
-export([
    start/1,
    stop/0
]).

% supervisor interface
-export([
    init/1
]).

% supervised callback
-export([
    start_link/1
]).

%% @hidden
-spec start(map()) -> {ok, pid()} | {error, any()}.
start(Options) ->
    ChildSpec = #{
        id => ?MODULE,
        start => {?MODULE, start_link, [Options]},
        restart => permanent,
        shutdown => 1000,
        type => supervisor,
        modules => [?MODULE]
    },
    supervisor:start_child(kernel_sup, ChildSpec).

%% @hidden
-spec stop() -> ok | {error, any()}.
stop() ->
    case supervisor:terminate_child(kernel_sup, ?MODULE) of
        ok ->
            supervisor:delete_child(kernel_sup, ?MODULE);
        Error ->
            Error
    end.

%% @hidden
start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

init(Options) ->
    ChildrenSpec = [
        #{
            id => erl_epmd,
            start => {erl_epmd, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [erl_epmd]
        },
        #{
            id => net_kernel,
            start => {net_kernel, start_link, Options},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [net_kernel]
        }
    ],
    SupFlags = #{strategy => one_for_all},
    {ok, {SupFlags, ChildrenSpec}}.
