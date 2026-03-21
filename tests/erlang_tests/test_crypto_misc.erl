%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_crypto_misc).
-export([start/0, get_bad/0]).

start() ->
    ok = test_hash_equals_equal(),
    ok = test_hash_equals_not_equal(),
    ok = test_hash_equals_empty(),
    ok = test_hash_equals_single_byte_equal(),
    ok = test_hash_equals_single_byte_not_equal(),
    ok = test_hash_equals_differs_at_start(),
    ok = test_hash_equals_differs_at_end(),
    ok = test_hash_equals_returns_bool(),
    ok = test_hash_equals_bad_first_arg(),
    ok = test_hash_equals_bad_second_arg(),
    ok = test_hash_equals_different_lengths(),
    0.

test_hash_equals_equal() ->
    true = crypto:hash_equals(
        <<16#0C, 16#60, 16#C8, 16#0F, 16#96, 16#1F, 16#0E, 16#71, 16#F3, 16#A9, 16#B5, 16#24, 16#AF,
            16#60, 16#12, 16#06, 16#2F, 16#E0, 16#37, 16#A6>>,
        <<16#0C, 16#60, 16#C8, 16#0F, 16#96, 16#1F, 16#0E, 16#71, 16#F3, 16#A9, 16#B5, 16#24, 16#AF,
            16#60, 16#12, 16#06, 16#2F, 16#E0, 16#37, 16#A6>>
    ),
    ok.

test_hash_equals_not_equal() ->
    false = crypto:hash_equals(
        <<16#0C, 16#60, 16#C8, 16#0F, 16#96, 16#1F, 16#0E, 16#71, 16#F3, 16#A9, 16#B5, 16#24, 16#AF,
            16#60, 16#12, 16#06, 16#2F, 16#E0, 16#37, 16#A6>>,
        <<16#EA, 16#6C, 16#01, 16#4D, 16#C7, 16#2D, 16#6F, 16#8C, 16#CD, 16#1E, 16#D9, 16#2A, 16#CE,
            16#1D, 16#41, 16#F0, 16#D8, 16#DE, 16#89, 16#57>>
    ),
    ok.

test_hash_equals_empty() ->
    true = crypto:hash_equals(<<>>, <<>>),
    ok.

test_hash_equals_single_byte_equal() ->
    true = crypto:hash_equals(<<16#ab>>, <<16#ab>>),
    ok.

test_hash_equals_single_byte_not_equal() ->
    false = crypto:hash_equals(<<16#ab>>, <<16#cd>>),
    ok.

test_hash_equals_differs_at_start() ->
    false = crypto:hash_equals(<<16#ff, 16#00, 16#00>>, <<16#00, 16#00, 16#00>>),
    ok.

test_hash_equals_differs_at_end() ->
    false = crypto:hash_equals(<<16#00, 16#00, 16#ff>>, <<16#00, 16#00, 16#00>>),
    ok.

test_hash_equals_returns_bool() ->
    R1 = crypto:hash_equals(<<"abc">>, <<"abc">>),
    R2 = crypto:hash_equals(<<"abc">>, <<"xyz">>),
    true = is_boolean(R1),
    true = is_boolean(R2),
    ok.

test_hash_equals_bad_first_arg() ->
    try
        crypto:hash_equals(?MODULE:get_bad(), <<"hello">>)
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_hash_equals_bad_second_arg() ->
    try
        crypto:hash_equals(<<"hello">>, ?MODULE:get_bad())
    catch
        error:_ ->
            exp_err
    end,
    ok.

test_hash_equals_different_lengths() ->
    try
        crypto:hash_equals(<<"hello">>, <<"hi">>)
    catch
        error:_ ->
            exp_err
    end,
    ok.

get_bad() -> foo.
