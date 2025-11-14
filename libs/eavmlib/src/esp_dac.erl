%%
%% Copyright (c) 2025 schnittchen <schnittchen@das-labor.org>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(esp_dac).

-export([
    new_channel/2,
    oneshot_output_voltage/2,
    oneshot_del_channel/1,
    oneshot_new_channel_p/1
]).

-type dac_rsrc() :: {'$dac', Resource :: binary(), Ref :: reference()}.

-type oneshot_channel_opts() :: [{chan_id, 0 | 1}].

-spec new_channel(oneshot, Opts :: oneshot_channel_opts()) ->
    {ok, Channel :: dac_rsrc()} | {error, Reason :: term()}.
new_channel(oneshot, Opts) ->
    case Opts of
        [{chan_id, 0}] ->
            ?MODULE:oneshot_new_channel_p(0);
        [{chan_id, 1}] ->
            ?MODULE:oneshot_new_channel_p(1);
        _Else ->
            {error, badarg}
    end.

-spec oneshot_output_voltage(Channel :: dac_rsrc(), Level :: 0..255) ->
    ok | {error, Reason :: term()}.
oneshot_output_voltage(_res, _level) ->
    erlang:nif_error(undefined).

-spec oneshot_del_channel(Channel :: dac_rsrc()) -> ok | {error, Reason :: term()}.
oneshot_del_channel(_res) ->
    erlang:nif_error(undefined).

-spec oneshot_new_channel_p(0 | 1) -> {ok, Channel :: dac_rsrc()} | {error, Reason :: term()}.
oneshot_new_channel_p(_chan) ->
    erlang:nif_error(undefined).
