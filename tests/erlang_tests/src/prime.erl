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

-module(prime).

-export([start/0, is_prime/1, calculate_list/2]).

start() ->
    spawn_opt(prime, calculate_list, num_range(2, 100), []),
    spawn_opt(prime, calculate_list, num_range(100, 400), []),
    spawn_opt(prime, calculate_list, num_range(500, 1500), []),
    all_primes_test(2000) -
        all_primes_test(2000) +
        all_primes_test(2000) -
        all_primes_test(2000) +
        all_primes_test(2000).

num_range(A, Size) ->
    [A, A + Size].

is_prime(Num) ->
    test_prime(Num, 2).

test_prime(Num, I) when Num == I ->
    true;
test_prime(Num, I) ->
    if
        Num rem I == 0 ->
            false;
        true ->
            test_prime(Num, I + 1)
    end.

all_primes_test(UpTo) ->
    do_all_primes_test(2, UpTo, 2).

do_all_primes_test(N, UpTo, Last) when N == UpTo ->
    Last;
do_all_primes_test(N, UpTo, Last) ->
    case is_prime(N) of
        true ->
            do_all_primes_test(N + 1, UpTo, N);
        false ->
            do_all_primes_test(N + 1, UpTo, Last)
    end.

calculate_list(First, Last) when First < 2 ->
    calculate_list(First + 1, Last);
calculate_list(First, Last) when First == Last ->
    case is_prime(Last) of
        true ->
            Last;
        false ->
            []
    end;
calculate_list(First, Last) ->
    case is_prime(First) of
        true ->
            [First | calculate_list(First + 1, Last)];
        false ->
            calculate_list(First + 1, Last)
    end.
