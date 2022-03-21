%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
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

-module(test_list_to_tuple).

-export([start/0, is_prime/1, calculate_list/2]).

start() ->
    L = calculate_list(2, 10),
    [_A, _B, _C, _D | Empty] = L,
    try_checksum(L) + try_checksum(Empty) + try_checksum(calculate_list(2, 7)) +
        try_checksum(foo(Empty)).

checksum({A, B, C, D}) ->
    1 + A * 2 + B * 4 + C * 8 + D * 16;
checksum(T) when is_tuple(T) ->
    -tuple_size(T).

try_checksum(L) ->
    try list_to_tuple(L) of
        T -> checksum(T)
    catch
        error:badarg -> -50;
        _:_ -> -200
    end.

foo([]) ->
    42;
foo(L) ->
    L.

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

% Returns an improper list when Last is a prime
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
