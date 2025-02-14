%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
% Copyright 2025 Davide Bettio <davide@uninstall.it>
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

-module(fun_call_bif).

-export([
    start/0,
    g/2,
    h/4
]).

start() ->
    Fun1 = fun erlang:'not'/1,
    A = ?MODULE:h(Fun1, true, false, 1) - 1,
    R1 = erlang:list_to_existing_atom("start"),
    Fun2 = fun erlang:list_to_existing_atom/1,
    B = ?MODULE:g(Fun2, atom_to_list(R1)),
    A + B.

g(Fun, R1) ->
    start = Fun(R1),
    0.

h(Fun, In, Exp, V) ->
    case Fun(In) of
        Exp -> V;
        _ -> 0
    end.
