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

-module(test_size).

-export([start/0, test_size/2, test_size_guard/2]).

start() ->
    gt = ?MODULE:test_size_guard({1, 2, 3}, 4),
    lt = ?MODULE:test_size_guard({1, 2, 3}, 2),
    eq = ?MODULE:test_size_guard({1, 2, 3}, 3),
    gt = ?MODULE:test_size_guard(<<1, 2, 3>>, 4),
    lt = ?MODULE:test_size_guard(<<1, 2, 3>>, 2),
    eq = ?MODULE:test_size_guard(<<1, 2, 3>>, 3),
    gt = ?MODULE:test_size_guard(<<3, 14, 1, 2, 3>>, 4),
    lt = ?MODULE:test_size_guard(<<3, 14, 1, 2, 3>>, 2),
    eq = ?MODULE:test_size_guard(<<3, 14, 1, 2, 3>>, 3),
    false = ?MODULE:test_size_guard(#{a => 1, b => 2, c => 3}, 4),
    false = ?MODULE:test_size_guard([1, 2, 3], 2),

    gt = ?MODULE:test_size({1, 2, 3}, 4),
    lt = ?MODULE:test_size({1, 2, 3}, 2),
    eq = ?MODULE:test_size({1, 2, 3}, 3),
    gt = ?MODULE:test_size(<<1, 2, 3>>, 4),
    lt = ?MODULE:test_size(<<1, 2, 3>>, 2),
    eq = ?MODULE:test_size(<<1, 2, 3>>, 3),
    gt = ?MODULE:test_size(<<3, 14, 1, 2, 3>>, 4),
    lt = ?MODULE:test_size(<<3, 14, 1, 2, 3>>, 2),
    eq = ?MODULE:test_size(<<3, 14, 1, 2, 3>>, 3),
    ok =
        try
            ?MODULE:test_size([1, 2, 3], 2),
            fail
        catch
            error:badarg ->
                ok
        end,
    ok =
        try
            ?MODULE:test_size(#{a => 1, b => 2, c => 3}, 4),
            fail
        catch
            error:badarg ->
                ok
        end,
    0.

% OTP-27 encodes this as a call to byte_size/1, passing it the matching state
% OTP-21 encodes this as a call to size/1, but encodes bs_context_to_binary
% first.
test_size_guard(<<3, 14, Elem/binary>>, S) when size(Elem) < S -> gt;
test_size_guard(<<3, 14, Elem/binary>>, S) when size(Elem) > S -> lt;
test_size_guard(<<3, 14, Elem/binary>>, S) when size(Elem) =:= S -> eq;
test_size_guard(Elem, S) when size(Elem) < S -> gt;
test_size_guard(Elem, S) when size(Elem) > S -> lt;
test_size_guard(Elem, S) when size(Elem) =:= S -> eq;
test_size_guard(_Elem, _S) -> false.

test_size(<<3, 14, Elem/binary>>, S) ->
    Size = size(Elem),
    if
        Size < S -> gt;
        Size > S -> lt;
        true -> eq
    end;
test_size(Elem, S) ->
    Size = size(Elem),
    if
        Size < S -> gt;
        Size > S -> lt;
        true -> eq
    end.
