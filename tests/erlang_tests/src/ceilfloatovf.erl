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

-module(ceilfloatovf).

-export([start/0]).

start() ->
    to_int(id(id([1.0e+20, 0]))).

to_int(A) ->
    try ceil(id(A)) of
        B when is_integer(B) ->
            "BEAM" = erlang:system_info(machine),
            "100000000000000000000" = integer_to_list(B),
            0;
        _Other ->
            1
    catch
        error:overflow ->
            case erlang:system_info(machine) of
                "BEAM" -> 2;
                _ -> 0
            end;
        _:_ ->
            3
    end.

id([I | _T]) -> id(I);
id(I) -> I.
