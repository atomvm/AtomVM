%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_apply_last).

-export([start/0, add/2, do_apply0/2, do_apply1/3, do_apply2/4]).

start() ->
    NoModule = try_apply_last(list_to_atom("foo"), list_to_atom("bar")),
    NoFunction = try_apply_last(?MODULE, no_such_function),
    Initial = NoModule + NoFunction,
    do_apply_last({?MODULE, add}, {erlang, list_to_integer}, ["1", "2", "3", "4", "5"], Initial).

do_apply_last(_, _, [], Sum) ->
    Sum;
do_apply_last({M1, F1} = MF1, {M2, F2} = MF2, [H | T], Sum) ->
    NewSum = ?MODULE:do_apply2(M1, F1, ?MODULE:do_apply1(M2, F2, H), Sum),
    do_apply_last(MF1, MF2, T, NewSum).

do_apply0(M, F) ->
    pad_some_calls(),
    M:F().

do_apply1(M, F, A) ->
    pad_some_calls(),
    M:F(A).

do_apply2(M, F, A, B) ->
    pad_some_calls(),
    M:F(A, B).

add(A, B) ->
    A + B.

try_apply_last(M, F) ->
    try
        ?MODULE:do_apply0(M, F)
    catch
        _Class:_Reason -> 1
    end.

pad_some_calls() ->
    X = 1,
    Y = 2,
    Z = X + Y,
    Z.
