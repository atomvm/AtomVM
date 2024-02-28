%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(complex_list_match_xregs).
-export([start/0, test_a/1, list_a/0, list_b/0, list_c/0]).

start() ->
    {ok, {"first", "list"}} = ?MODULE:test_a(?MODULE:list_a()),
    {x, $o} = ?MODULE:test_a(?MODULE:list_b()),
    ok_baz = ?MODULE:test_a(?MODULE:list_c()),
    0.

test_a(["This", "is", _, _, _, _, _, "baz", "baar"]) ->
    error_a;
test_a(["This", "is", _, _, "foo", _, _, "baz"]) ->
    ok_baz;
test_a(["This", "is", "baz", _, _, _, _, "baz"]) ->
    error_c;
test_a(["This", "is", "not" | _Tail]) ->
    error_d;
test_a(["This", "is", _, N, _, W]) ->
    {ok, {N, W}};
test_a(["This" | Tail]) ->
    {foo, "This", Tail};
test_a([[$F, X, $o | Tail], Tail]) ->
    {x, X};
test_a([[$F, Y, $o | _Tail1], _Tail2]) ->
    {x, Y};
test_a(List) ->
    {error, List}.

list_a() ->
    ["This", "is", "the", "first", "test", "list"].

list_b() ->
    [[$F, $o, $o, "bar"], "bar"].

list_c() ->
    ["This", "is", "aaa", "bbb", "foo", "ccc", "ddd", "baz"].
