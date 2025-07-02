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

-module(test_binary_to_atom).

-export([start/0, f/1, g/1, h/1, i/1]).

start() ->
    ok = test_more_than_255_bytes(),
    f(i(h(g(2)))) + f(i(h(g(10)))) + f(i(h(g(20)))).

f(not_an_atom) ->
    0;
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
    <<"hello">>;
h(4) ->
    <<"this_will_be_a_new_atom">>;
h(9) ->
    5;
h(_) ->
    [].

i(A) ->
    try binary_to_atom(A, latin1) of
        AnyAtom -> AnyAtom
    catch
        _:_ -> not_an_atom
    end.

test_more_than_255_bytes() ->
    Suffix = duplicate(<<"-">>, 252, <<>>),
    Str = <<"アトム"/utf8, Suffix/binary>>,
    261 = byte_size(Str),
    _LongAtom = binary_to_atom(id(Str), utf8),
    % biggest atom is 1020 bytes
    MoonStr = <<"🌑🌒🌓🌔🌕🌖🌗🌘"/utf8>>,
    32 = byte_size(MoonStr),
    Moons256Str = duplicate(MoonStr, 32, <<>>),
    1024 = byte_size(Moons256Str),
    <<"🌑"/utf8, Moons255Str/binary>> = Moons256Str,
    1020 = byte_size(Moons255Str),
    _LongestAtom = binary_to_atom(id(Moons255Str), utf8),
    ok =
        try
            _ = binary_to_atom(id(<<Str/binary, "o">>), utf8),
            should_have_raised
        catch
            error:system_limit -> ok
        end.

id(X) ->
    X.

duplicate(_Binary, 0, Acc) -> Acc;
duplicate(Binary, N, Acc) -> duplicate(Binary, N - 1, <<Binary/binary, Acc/binary>>).
