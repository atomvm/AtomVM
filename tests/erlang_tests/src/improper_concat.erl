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

-module(improper_concat).

-export([start/0, id/1, add/2, check/3]).

start() ->
    L1 = [id(add(id(1), id(0))), id(add(id(1), id(1)))] ++ id(add(id(1), id(2))),
    L2 = [-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0] ++ L1,
    try_concat([id(1), add(id(1), id(1)) | id(3)], [4, id(5)]) * 4 +
        bool_to_int(check(L1, 0, 3)) + bool_to_int(check(L2, -11, 14)) * 2.

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

try_concat(A, B) ->
    try id(A) ++ id(B) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> 2048
    end.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.
