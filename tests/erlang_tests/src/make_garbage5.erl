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

-module(make_garbage5).

-export([start/0, det/1]).

start() ->
    det({{2, 2, 3}, {1, 1, 3}, {2, 0, 1}}).

det({{A, B}, {C, D}}) ->
    A * D - B * C;
det({{A, B, C}, {D, E, F}, {G, H, I}}) ->
    A * det({{E, F}, {H, I}}) - B * det({{D, F}, {G, I}}) + C * det({{D, E}, {G, H}}).
