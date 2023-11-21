%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(external_proplist_test).

-export([start/0, get_value/2]).

start() ->
    APropList = [{a, 1}, {b, 2}, {c, 3}, {d, 4}],
    get_value(APropList, c).

get_value([], _N) ->
    not_found;
get_value([{PN, PV} | _Tail], N) when PN == N ->
    PV;
get_value([_Head | Tail], N) ->
    get_value(Tail, N).
