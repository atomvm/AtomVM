%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(fail_apply).

-export([start/0, add/2]).

start() ->
    NoModule = try_apply(add(1, 3), list_to_atom("bar")),
    NoFunction = try_apply(?MODULE, add(4, 1)),
    Initial = NoModule + NoFunction,
    do_apply({?MODULE, add}, {erlang, list_to_integer}, ["1", "2", "3", "4", "5"], Initial).

do_apply(_, _, [], Sum) ->
    Sum;
do_apply({M1, F1} = MF1, {M2, F2} = MF2, [H | T], Sum) ->
    NewSum = M1:F1(M2:F2(H), Sum),
    do_apply(MF1, MF2, T, NewSum).

add(A, B) ->
    A + B.

try_apply(M, F) ->
    try
        M:F(),
        0
    catch
        _Class:_Reason -> 1
    end.
