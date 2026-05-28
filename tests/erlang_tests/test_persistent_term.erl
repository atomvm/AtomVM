%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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

-module(test_persistent_term).

-export([start/0]).

start() ->
    cleanup(),
    ok = test_get_put_erase(),
    % put_new/2 is OTP 28.4+ only
    case erlang:function_exported(persistent_term, put_new, 2) of
        true -> ok = test_put_new();
        false -> ok
    end,
    ok = test_complex_keys(),
    ok = test_fun_keys(),
    ok = test_info_and_get_all(),
    cleanup(),
    0.

test_get_put_erase() ->
    Key = {?MODULE, basic},
    assert_badarg(fun() -> persistent_term:get(Key) end),
    default = persistent_term:get(Key, default),
    false = persistent_term:erase(Key),

    Big = <<1:80/unit:8>>,
    OldValue = {old, [Big, <<"small">>]},
    ok = persistent_term:put(Key, OldValue),
    OldValue = persistent_term:get(Key),

    Retained = persistent_term:get(Key),
    ok = persistent_term:put(Key, {new, value}),
    {new, value} = persistent_term:get(Key),
    OldValue = Retained,

    true = persistent_term:erase(Key),
    false = persistent_term:erase(Key),
    missing = persistent_term:get(Key, missing),
    OldValue = Retained,
    ok.

test_put_new() ->
    Key = {?MODULE, put_new},
    ok = persistent_term:put_new(Key, first),
    ok = persistent_term:put_new(Key, first),
    first = persistent_term:get(Key),
    assert_badarg(fun() -> persistent_term:put_new(Key, second) end),
    first = persistent_term:get(Key),
    true = persistent_term:erase(Key),
    ok.

test_complex_keys() ->
    Key = {{?MODULE, complex}, [self(), <<"bin">>], #{a => 1, <<"b">> => {c, d}}},
    ok = persistent_term:put(Key, complex_value),
    complex_value = persistent_term:get(Key),
    true = persistent_term:erase(Key),
    complex_value = persistent_term:get(Key, complex_value),
    ok.

test_fun_keys() ->
    LocalFun = fun identity/1,
    ExternalFun = fun erlang:length/1,
    Key = {?MODULE, fun_key, LocalFun, ExternalFun},
    EquivalentKey = {?MODULE, fun_key, fun identity/1, fun erlang:length/1},
    ok = persistent_term:put(Key, fun_value),
    fun_value = persistent_term:get(EquivalentKey),
    true = persistent_term:erase(EquivalentKey),
    missing = persistent_term:get(Key, missing),
    ok.

test_info_and_get_all() ->
    Key1 = {?MODULE, info_1},
    Key2 = {?MODULE, info_2},
    #{count := Count0} = persistent_term:info(),

    ok = persistent_term:put(Key1, value1),
    ok = persistent_term:put(Key2, {value2, [1, 2, 3]}),

    #{count := Count1, memory := Memory1} = persistent_term:info(),
    true = Count1 >= Count0 + 2,
    true = is_integer(Memory1),
    true = Memory1 > 0,

    ok = persistent_term:put(Key1, {value1, replaced}),
    #{count := Count2, memory := Memory2} = persistent_term:info(),
    Count1 = Count2,
    true = Memory2 > Memory1,

    All = persistent_term:get(),
    true = lists:member({Key1, {value1, replaced}}, All),
    true = lists:member({Key2, {value2, [1, 2, 3]}}, All),

    true = persistent_term:erase(Key1),
    #{count := Count3, memory := Memory3} = persistent_term:info(),
    Count3 = Count2 - 1,
    %% AtomVM retains retired entries on a dead list until VM shutdown so
    %% previously returned terms stay valid without a global GC pass;
    %% OTP may reclaim that memory immediately.
    true = (Memory3 =< Memory2),
    true = persistent_term:erase(Key2),
    ok.

cleanup() ->
    _ = persistent_term:erase({?MODULE, basic}),
    _ = persistent_term:erase({?MODULE, put_new}),
    _ = persistent_term:erase({?MODULE, info_1}),
    _ = persistent_term:erase({?MODULE, info_2}),
    ok.

assert_badarg(Fun) ->
    {'EXIT', {badarg, _}} = (catch Fun()),
    ok.

identity(Value) ->
    Value.
