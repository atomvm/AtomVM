%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_cmp_term).

-export([start/0]).

start() ->
    ok = test_equal(),
    ok = test_less_than(),
    ok = test_greater_than(),
    ok = test_exact_int_float(),
    ok = test_type_ordering(),
    ok = test_complex_terms(),
    ok = test_symmetry(),
    1.

test_equal() ->
    0 = erts_internal:cmp_term(0, 0),
    0 = erts_internal:cmp_term(42, 42),
    0 = erts_internal:cmp_term(-1, -1),
    0 = erts_internal:cmp_term(1.0, 1.0),
    0 = erts_internal:cmp_term(foo, foo),
    0 = erts_internal:cmp_term([], []),
    0 = erts_internal:cmp_term([1, 2, 3], [1, 2, 3]),
    0 = erts_internal:cmp_term({a, b}, {a, b}),
    0 = erts_internal:cmp_term(<<"hello">>, <<"hello">>),
    0 = erts_internal:cmp_term(#{a => 1}, #{a => 1}),
    ok.

test_less_than() ->
    -1 = erts_internal:cmp_term(1, 2),
    -1 = erts_internal:cmp_term(-1, 0),
    -1 = erts_internal:cmp_term(a, b),
    -1 = erts_internal:cmp_term(aa, ab),
    -1 = erts_internal:cmp_term([1], [2]),
    -1 = erts_internal:cmp_term({1}, {2}),
    -1 = erts_internal:cmp_term({1, 2}, {1, 3}),
    -1 = erts_internal:cmp_term(<<"a">>, <<"b">>),
    ok.

test_greater_than() ->
    1 = erts_internal:cmp_term(2, 1),
    1 = erts_internal:cmp_term(0, -1),
    1 = erts_internal:cmp_term(b, a),
    1 = erts_internal:cmp_term(ab, aa),
    1 = erts_internal:cmp_term([2], [1]),
    1 = erts_internal:cmp_term({2}, {1}),
    1 = erts_internal:cmp_term({1, 3}, {1, 2}),
    1 = erts_internal:cmp_term(<<"b">>, <<"a">>),
    ok.

test_exact_int_float() ->
    R1 = erts_internal:cmp_term(1, 1.0),
    true = (R1 =/= 0),
    R2 = erts_internal:cmp_term(0, 0.0),
    true = (R2 =/= 0),
    ok.

test_type_ordering() ->
    %% Standard Erlang term ordering: number < atom < ref < fun < port < pid < tuple < map < nil < list < bitstring
    -1 = erts_internal:cmp_term(1, a),
    1 = erts_internal:cmp_term(a, 1),
    -1 = erts_internal:cmp_term(a, {1}),
    1 = erts_internal:cmp_term({1}, a),
    -1 = erts_internal:cmp_term({1}, #{a => 1}),
    1 = erts_internal:cmp_term(#{a => 1}, {1}),
    -1 = erts_internal:cmp_term(#{}, []),
    1 = erts_internal:cmp_term([], #{}),
    -1 = erts_internal:cmp_term([], [1]),
    1 = erts_internal:cmp_term([1], []),
    -1 = erts_internal:cmp_term([1], <<"a">>),
    1 = erts_internal:cmp_term(<<"a">>, [1]),
    ok.

test_complex_terms() ->
    0 = erts_internal:cmp_term({[1, 2], #{a => b}}, {[1, 2], #{a => b}}),
    -1 = erts_internal:cmp_term({[1, 2], #{a => a}}, {[1, 2], #{a => b}}),
    -1 = erts_internal:cmp_term({1}, {1, 2}),
    1 = erts_internal:cmp_term({1, 2}, {1}),
    -1 = erts_internal:cmp_term([1, 2], [1, 3]),
    1 = erts_internal:cmp_term([1, 3], [1, 2]),
    Big1 = 1 bsl 64,
    Big2 = (1 bsl 64) + 1,
    -1 = erts_internal:cmp_term(Big1, Big2),
    1 = erts_internal:cmp_term(Big2, Big1),
    0 = erts_internal:cmp_term(Big1, Big1),
    ok.

test_symmetry() ->
    %% If cmp_term(A, B) = -1, then cmp_term(B, A) = 1 and vice versa
    Pairs = [
        {1, 2},
        {a, b},
        {[], [1]},
        {{1}, {1, 2}},
        {<<"a">>, <<"b">>},
        {1, a},
        {#{}, []}
    ],
    ok = check_symmetry(Pairs).

check_symmetry([]) ->
    ok;
check_symmetry([{A, B} | Rest]) ->
    R1 = erts_internal:cmp_term(A, B),
    R2 = erts_internal:cmp_term(B, A),
    true = (R1 =:= -R2),
    0 = erts_internal:cmp_term(A, A),
    0 = erts_internal:cmp_term(B, B),
    check_symmetry(Rest).
