%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(esp_nvs).
-export([start/0]).

-record(state, {
    count = 0,
    timestamp = erlang:timestamp()
}).

start() ->
    Bin = esp:nvs_get_binary(?MODULE, starts),
    State =
        case Bin of
            undefined ->
                #state{};
            _ ->
                case erlang:binary_to_term(Bin) of
                    #state{count = Count} ->
                        #state{count = Count + 1};
                    _ ->
                        erlang:display({error, bad_value}),
                        #state{}
                end
        end,
    io:format("Saving count ~p to NVS...~nReset device to increment.~n", [State#state.count]),
    esp:nvs_set_binary(?MODULE, starts, erlang:term_to_binary(State)).
