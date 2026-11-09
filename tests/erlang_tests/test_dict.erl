%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(test_dict).

-export([start/0, put_int/1, stringize/1, factorial/1, id/1, the_get/1, the_erase/1]).

start() ->
    ok = test_get_keys(),
    ok = test_get_keys_gc(),
    ok = test_get_0_erase_0(),
    put_int(0),
    X = put_int(1),
    put_int(2),
    put(6, X),
    put_int(3),
    put_int(4),
    put_int(5),
    Y = stringize(factorial(8)),
    Z = stringize(factorial(9)),
    [H | _T] = stringize(erlang:list_to_integer(Z) - erlang:list_to_integer(Y)),
    _ = stringize(factorial(6)),
    _ = stringize(factorial(7)),
    _ = stringize(123456),
    _ = stringize(876543),
    W = erlang:list_to_integer(the_get(stringize((H - $0) - 2)) ++ the_erase(id(6))),
    _ = stringize(111222),
    _ = stringize(333222),
    "undefined" = erlang:atom_to_list(the_erase(6)),
    ok = test_put_get_erase(),
    W.

put_int(N) ->
    put(stringize(id(factorial(id(N)))), stringize(id(factorial(id(N))) * 2)).

stringize(I) when is_integer(I) ->
    erlang:integer_to_list(I) ++ "0";
stringize(_I) ->
    "".

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

id(X) when is_integer(X) ->
    X;
id(_X) ->
    0.

the_get(L) when is_list(L) ->
    get(L);
the_get(_X) ->
    "-1".

the_erase(N) when is_integer(N) ->
    erase(N);
the_erase(_X) ->
    "0".

test_put_get_erase() ->
    undefined = put({any_term}, 1),
    1 = get({any_term}),
    1 = apply(erlang, list_to_atom("put"), [{any_term}, 2]),
    2 = apply(erlang, list_to_atom("get"), [{any_term}]),
    2 = apply(erlang, list_to_atom("erase"), [{any_term}]),
    undefined = erase({any_term}),
    ok.

test_get_keys() ->
    [] = get_keys(),
    [] = get_keys(missing),
    put(1, integer),
    put(1.0, float),
    put(a, 1),
    put(b, 1.0),
    put(c, 1),
    Keys = get_keys(),
    5 = length(Keys),
    true = has_key(1, Keys),
    true = has_key(1.0, Keys),
    true = has_key(a, Keys),
    true = has_key(b, Keys),
    true = has_key(c, Keys),
    [1] = get_keys(integer),
    [1.0] = get_keys(float),
    IntegerKeys = get_keys(1),
    2 = length(IntegerKeys),
    true = has_key(a, IntegerKeys),
    true = has_key(c, IntegerKeys),
    [b] = get_keys(1.0),
    put(boxed, {<<"value">>, #{x => 1}}),
    [boxed] = get_keys({<<"value">>, #{x => 1}}),
    [] = get_keys({<<"value">>, #{x => 1.0}}),
    6 = length(erase()),
    [] = get_keys(),
    ok.

test_get_keys_gc() ->
    {Pid, Ref} = spawn_opt(
        fun() ->
            fill_dictionary(256),
            get_keys_gc(32)
        end,
        [monitor, {min_heap_size, 100}]
    ),
    %% Check GC correctness, not wall-clock performance under instrumentation.
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end,
    ok.

fill_dictionary(0) ->
    ok;
fill_dictionary(N) ->
    put({N, list_to_binary([N rem 256, $k])}, {N rem 2, list_to_binary([N rem 2, $v])}),
    fill_dictionary(N - 1).

get_keys_gc(0) ->
    256 = length(erase()),
    [] = get_keys(),
    ok;
get_keys_gc(N) ->
    % Shrink before allocating the result lists.  The boxed search value must
    % survive any collection inside get_keys/1, not just a collection before it.
    true = erlang:garbage_collect(),
    Keys = get_keys(),
    Value = {N rem 2, list_to_binary([N rem 2, $v])},
    Matching = get_keys(Value),
    [] = get_keys({2, list_to_binary([N rem 2, $v])}),
    % Keep both results live while allocating far beyond the minimum heap.
    Pressure = dictionary_pressure(1024, []),
    true = erlang:garbage_collect(),
    1024 = length(Pressure),
    256 = length(Keys),
    128 = length(Matching),
    ok = check_dictionary_keys(256, Keys, Matching, N rem 2),
    get_keys_gc(N - 1).

dictionary_pressure(0, Acc) -> Acc;
dictionary_pressure(N, Acc) -> dictionary_pressure(N - 1, [{N, N + 1} | Acc]).

check_dictionary_keys(0, _Keys, _Matching, _Parity) ->
    ok;
check_dictionary_keys(N, Keys, Matching, Parity) ->
    Key = {N, list_to_binary([N rem 256, $k])},
    true = has_key(Key, Keys),
    Expected = N rem 2 =:= Parity,
    Expected = has_key(Key, Matching),
    ExpectedValue = {N rem 2, list_to_binary([N rem 2, $v])},
    ExpectedValue = get(Key),
    check_dictionary_keys(N - 1, Keys, Matching, Parity).

has_key(_Key, []) -> false;
has_key(Key, [Key | _]) -> true;
has_key(Key, [_ | Rest]) -> has_key(Key, Rest).

test_get_0_erase_0() ->
    [] = get(),
    undefined = put({any_term}, 1),
    [{{any_term}, 1}] = get(),
    [{{any_term}, 1}] = erase(),
    [] = get(),
    [] = erase(),
    ok.
