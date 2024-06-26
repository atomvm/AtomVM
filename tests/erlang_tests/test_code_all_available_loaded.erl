%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_code_all_available_loaded).

-export([start/0]).

start() ->
    ok = test_all_loaded(),
    ok = test_all_available(),
    0.

test_all_loaded() ->
    Loaded = code:all_loaded(),
    true = length(Loaded) > 0,
    true = has_key(?MODULE, Loaded),
    ok.

test_all_available() ->
    FunctionExists =
        case erlang:system_info(machine) of
            "BEAM" ->
                erlang:system_info(version) >= "11.";
            "ATOM" ->
                true
        end,
    test_all_available0(FunctionExists).

test_all_available0(false) ->
    ok;
test_all_available0(true) ->
    Loaded = code:all_loaded(),
    Available = code:all_available(),
    true = length(Available) >= length(Loaded),
    AvailableLoaded = [
        {binary_to_atom(iolist_to_binary(Module), utf8), File}
     || {Module, File, true} <- Available
    ],
    true = set_equal(Loaded, AvailableLoaded),
    ok.

has_key(_Module, []) -> false;
has_key(Module, [{Module, _} | _]) -> true;
has_key(Module, [_H | Tail]) -> has_key(Module, Tail).

set_equal(L1, L2) ->
    set_equal0(L1, L2, []).

set_equal0([], [], []) -> true;
set_equal0([H | T1], [H | T2], Acc) -> set_equal0(T1, T2 ++ Acc, []);
set_equal0([_H1 | _T1] = L1, [H2 | T2], Acc) -> set_equal0(L1, T2, [H2 | Acc]);
set_equal0(_L1, [], _Acc) -> false;
set_equal0([], _L2, _Acc) -> false.
