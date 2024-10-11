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

-module(test_list_to_integer).

-export([start/0, sum_integers/2, append_0/1]).

start() ->
    sum_integers(append_0("10"), "-1") +
        safe_list_to_integer("--") - 5 +
        safe_list_to_integer(nan) - 5 +
        safe_list_to_integer("+10") - 10 +
        safe_list_to_integer("-") - 5 +
        safe_list_to_integer("+") - 5 +
        safe_list_to_integer("") - 5 +
        safe_list_to_integer("0a", 16) - 10 +
        safe_list_to_integer("-0a", 16) + 10 +
        safe_list_to_integer("1010", 2) - 10.

append_0(L) ->
    L ++ "0".

sum_integers(A, B) ->
    list_to_integer(A) + list_to_integer(B).

safe_list_to_integer(A) ->
    safe_list_to_integer(A, 10).
safe_list_to_integer(A, Base) ->
    try list_to_integer(A, Base) of
        AnyValue -> AnyValue
    catch
        error:badarg ->
            5;
        _:_ ->
            "error"
    end.
