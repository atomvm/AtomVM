%
% This file is part of AtomVM.
%
% Copyright 2024 Jakub Gonet <jakub.gonet@swmansion.com>
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

% This test has been made after https://github.com/atomvm/AtomVM/issues/1379

-module(gc_safe_x_reg_write).
-export([start/0]).
-export([f/0, check/2]).

start() ->
    ?MODULE:check(?MODULE:f(), 0) - 21.

f() ->
    [
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0},
        {f, fun f/0}
    ].

check([], Count) ->
    Count;
check([{f, F} | T], Count) when is_function(F) ->
    check(T, Count + 1).
