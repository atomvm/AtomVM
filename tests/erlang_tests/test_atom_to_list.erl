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

-module(test_atom_to_list).

-export([start/0, f/1, g/1, h/1, compare_list/2]).

start() ->
    compare_list(f(hello_world), "hello" ++ g(world)) - h(15).

f(A) when is_binary(A) ->
    binaries_not_ok;
f(A) ->
    atom_to_list(A).

compare_list([], []) ->
    1;
compare_list([H_A | T_A], [H_B | T_B]) when H_A == H_B ->
    compare_list(T_A, T_B);
compare_list(_A, _B) ->
    0.

g(world) ->
    "_world";
g(A) when is_atom(A) ->
    "?".

h(A) ->
    try f(A) of
        _AnyVal -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.
