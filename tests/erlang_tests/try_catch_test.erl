%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(try_catch_test).

-export([start/0, fail/1, mayfail/1]).

start() ->
    mayfail(89) + mayfailsafe(42).

mayfail(Value) ->
    fail(Value) + 10.

mayfailsafe(Value) ->
    try mayfail(Value) + 5 of
        _Any -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1
    end.

fail(X) ->
    89 = X,
    100.
