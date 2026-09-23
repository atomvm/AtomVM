%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_bigint_normalization).

-export([start/0, id/1]).

start() ->
    ok = test_small_value_from_wide_segment(),
    ok = test_negative_small_value_from_wide_segment(),
    ok = test_zero_from_wide_segment(),
    ok = test_large_value_stays_big(),
    ok = test_normalized_value_in_containers(),
    ok = test_wide_small_big_ext(),
    0.

test_small_value_from_wide_segment() ->
    A = u72(id(<<42:72>>)),
    true = is_integer(A),
    true = A =:= 42,
    true = A == 42,
    42 = A,
    0 = A - 42,
    84 = A + 42,
    ok.

test_negative_small_value_from_wide_segment() ->
    B = s72(id(<<-42:72>>)),
    true = B =:= -42,
    true = B == -42,
    -42 = B,
    ok.

test_zero_from_wide_segment() ->
    Z = u72(id(<<0:72>>)),
    true = Z =:= 0,
    0 = Z,
    ok.

test_large_value_stays_big() ->
    C = u72(id(<<(1 bsl 70):72>>)),
    true = C =:= (1 bsl 70),
    false = C =:= 42,
    %% 1 bsl 62 exceeds the small integer range on every supported word size
    D = u72(id(<<(1 bsl 62):72>>)),
    true = D =:= (1 bsl 62),
    ok.

test_normalized_value_in_containers() ->
    A = u72(id(<<42:72>>)),
    true = lists:member(A, [41, 42, 43]),
    [42] = [A],
    {ok, 42} = {ok, A},
    true = [A] =:= [42],
    ok.

test_wide_small_big_ext() ->
    P = binary_to_term(id(<<131, 110, 9, 0, 42, 0, 0, 0, 0, 0, 0, 0, 0>>)),
    true = P =:= 42,
    42 = P,
    N = binary_to_term(id(<<131, 110, 9, 1, 42, 0, 0, 0, 0, 0, 0, 0, 0>>)),
    true = N =:= -42,
    -42 = N,
    Big = binary_to_term(id(<<131, 110, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64>>)),
    true = Big =:= (1 bsl 70),
    ok.

u72(Bin) ->
    <<X:72>> = Bin,
    X.

s72(Bin) ->
    <<X:72/signed>> = Bin,
    X.

id(X) -> X.
