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

-module(test_set_tuple_element).

-export([start/0, test/2]).

-record(big, {
    a = foo,
    b = 1,
    c = {e, f},
    e = [3, 1, 4, 1, 5, 2, 6, 9, 5],
    f = [],
    g = undefined,
    h = [have, i, created, enough, elements, yet],
    i = [und, now, for, something, completely, different],
    j = {ha, ha}
}).

start() ->
    ok = test(thing1, {lets, try_this}),
    ok = test(thing1, {ha, ha}),
    ok = test(undefined, {ha, ha}),
    ok = test(undefined, {lets, try_this}),
    0.

test(G, J) ->
    Big = make_a_change(#big{}, G, J),
    case {Big#big.g, Big#big.j} of
        {G, J} ->
            ok;
        X ->
            erlang:display(X),
            fail
    end.

make_a_change(Big, Thing1, Thing2) ->
    Big#big{g = Thing1, j = Thing2}.
