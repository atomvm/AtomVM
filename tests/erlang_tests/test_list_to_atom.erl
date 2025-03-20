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

-module(test_list_to_atom).

-export([start/0, f/1, g/1, h/1, i/1, id/1]).

start() ->
    ok = test_more_than_255_bytes(),
    f(i(h(g(2)))) + f(i(h(g(10)))) + i(10).

f(hello) ->
    1;
f(world) ->
    2;
f(test) ->
    4;
f(AnyAtom) when is_atom(AnyAtom) ->
    8;
f(_Any) ->
    16.

g(N) ->
    (N div 2) - 1.

h(0) ->
    "hello";
h(4) ->
    "this_will_be_a_new_atom";
h(_) ->
    [].

i(A) when is_binary(A) ->
    error;
i(A) ->
    try list_to_atom(A) of
        Value -> Value
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.

test_more_than_255_bytes() ->
    Suffix = duplicate("-", 252, ""),
    Str = "アトム" ++ Suffix,
    255 = length(Str),
    _ = list_to_atom(id(Str)),
    % biggest atom is 1020 bytes
    MoonStr = "🌑🌒🌓🌔🌕🌖🌗🌘",
    8 = length(MoonStr),
    Moons256Str = duplicate(MoonStr, 32, ""),
    256 = length(Moons256Str),
    "🌑" ++ Moons255Str = Moons256Str,
    _ = list_to_atom(id(Moons255Str)),

    ok =
        try
            _ = list_to_atom(id(Str ++ "o")),
            should_have_raised
        catch
            error:system_limit -> ok
        end.

id(X) ->
    X.

duplicate(_List, 0, Acc) -> Acc;
duplicate(List, N, Acc) -> duplicate(List, N - 1, List ++ Acc).
