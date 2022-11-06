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

-module(set_network_config).

-export([start/0]).

start() ->
    Ssid = <<"myssid">>,
    Psk = <<"mypsk">>,
    esp:nvs_set_binary(atomvm, sta_ssid, Ssid),
    erlang:display({atomvm, sta_ssid, Ssid}),
    esp:nvs_set_binary(atomvm, sta_psk, Psk),
    erlang:display({atomvm, sta_psk, <<"xxxxxx">>}),
    ok.
