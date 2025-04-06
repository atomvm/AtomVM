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

-module(float_bif).

-export([start/0]).
% For remote function calls
-export([id/1, check_one/1]).

-define(ID(Arg), ?MODULE:id(Arg)).

start() ->
    true = 1.0 =:= float(?ID(1)),
    true = 1.0 =:= float(?ID(1.0)),
    ok =
        try float(?ID("1")) of
            _ ->
                unreachable
        catch
            error:badarg ->
                ok
        end,
    ok = ?MODULE:check_one(?ID(1)),
    ok = ?MODULE:check_one(?ID(1.0)),
    error = ?MODULE:check_one(?ID(atom)),
    0.

id(X) ->
    X.

% Tests float/1 in guards
check_one(T) when float(T) =:= 1.0 ->
    ok;
check_one(_T) ->
    error.
