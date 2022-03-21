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

-module(test_try_case_end).

-export([start/0, id/1, add_handle_badarith/2, try_add_plus_2/2, try_add/2]).

start() ->
    add_handle_badarith(id(-1), id(1)).

id(X) ->
    X.

add_handle_badarith(A, B) ->
    try try_add_plus_2(A, B) of
        Result -> Result
    catch
        error:badarith -> -16;
        error:{try_clause, N} -> 256 - N;
        _:_ -> 2048
    end.

try_add_plus_2(A, B) ->
    try_add(A, B) + 2.

try_add(A, B) ->
    try A + B of
        1 -> 0;
        2 -> 1
    catch
        error:badarg -> -1;
        _:_ -> -1024
    end.
