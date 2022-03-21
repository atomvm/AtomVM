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

-module(improper_literal).

-export([start/0, id/1, add/2, check/3]).

start() ->
    L1 = [1, 2, 3, 4 | 5],
    L2 = [1 | 2],
    bool_to_int(check(id(L1), 0, 5)) + bool_to_int(check(id(L2), 0, 2)) * 2.

id(I) ->
    I.

add(A, B) ->
    id(A) + id(B).

check(T, Last, 1) when not is_list(T) ->
    T > Last;
check([H | T], Last, N) ->
    if
        H > Last -> check(T, H, N - 1);
        H =< Last -> false
    end.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.
