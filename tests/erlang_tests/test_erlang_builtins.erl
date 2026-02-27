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

-module(test_erlang_builtins).

-export([start/0, id/1]).

start() ->
    ok = test_abs(),
    ok = test_byte_size(),
    ok = test_element(),
    ok = test_error(),
    ok = test_error2(),
    ok = test_hd(),
    ok = test_is_atom(),
    ok = test_is_binary(),
    ok = test_is_boolean(),
    ok = test_is_float(),
    ok = test_is_function(),
    ok = test_is_function2(),
    ok = test_is_integer(),
    ok = test_is_list(),
    ok = test_is_number(),
    ok = test_is_pid(),
    ok = test_is_reference(),
    ok = test_is_tuple(),
    ok = test_length(),
    ok = test_list_to_float(),
    ok = test_node(),
    ok = test_round(),
    ok = test_self(),
    ok = test_setelement(),
    ok = test_size(),
    ok = test_throw(),
    ok = test_tl(),
    ok = test_trunc(),
    ok = test_tuple_size(),
    ok = test_tuple_to_list(),
    0.

id(X) -> X.

test_abs() ->
    3 = erlang:abs(id(-3)),
    3 = erlang:abs(id(3)),
    2.5 = erlang:abs(id(-2.5)),
    ok.

test_byte_size() ->
    5 = erlang:byte_size(id(<<"hello">>)),
    0 = erlang:byte_size(id(<<>>)),
    ok.

test_element() ->
    a = erlang:element(1, id({a, b, c})),
    c = erlang:element(3, id({a, b, c})),
    ok.

test_error() ->
    try
        erlang:error(id(test_reason))
    catch
        error:test_reason -> ok
    end.

test_error2() ->
    try
        erlang:error(id(test_reason), id([arg1]))
    catch
        error:test_reason -> ok
    end.

test_hd() ->
    1 = erlang:hd(id([1, 2, 3])),
    a = erlang:hd(id([a])),
    ok.

test_is_atom() ->
    true = erlang:is_atom(id(hello)),
    false = erlang:is_atom(id(42)),
    ok.

test_is_binary() ->
    true = erlang:is_binary(id(<<"hello">>)),
    false = erlang:is_binary(id(42)),
    ok.

test_is_boolean() ->
    true = erlang:is_boolean(id(true)),
    true = erlang:is_boolean(id(false)),
    false = erlang:is_boolean(id(hello)),
    ok.

test_is_float() ->
    true = erlang:is_float(id(1.0)),
    false = erlang:is_float(id(1)),
    ok.

test_is_function() ->
    true = erlang:is_function(id(fun id/1)),
    false = erlang:is_function(id(42)),
    ok.

test_is_function2() ->
    true = erlang:is_function(id(fun id/1), id(1)),
    false = erlang:is_function(id(fun id/1), id(2)),
    false = erlang:is_function(id(42), id(1)),
    ok.

test_is_integer() ->
    true = erlang:is_integer(id(42)),
    false = erlang:is_integer(id(1.0)),
    ok.

test_is_list() ->
    true = erlang:is_list(id([1, 2, 3])),
    true = erlang:is_list(id([])),
    false = erlang:is_list(id(42)),
    ok.

test_is_number() ->
    true = erlang:is_number(id(42)),
    true = erlang:is_number(id(1.0)),
    false = erlang:is_number(id(hello)),
    ok.

test_is_pid() ->
    true = erlang:is_pid(id(self())),
    false = erlang:is_pid(id(42)),
    ok.

test_is_reference() ->
    true = erlang:is_reference(id(make_ref())),
    false = erlang:is_reference(id(42)),
    ok.

test_is_tuple() ->
    true = erlang:is_tuple(id({a, b})),
    false = erlang:is_tuple(id(42)),
    ok.

test_length() ->
    3 = erlang:length(id([a, b, c])),
    0 = erlang:length(id([])),
    ok.

test_list_to_float() ->
    F = erlang:list_to_float(id("1.5")),
    true = erlang:is_float(F),
    true = (F > 1.4 andalso F < 1.6),
    ok.

test_node() ->
    Node = erlang:node(),
    true = erlang:is_atom(Node),
    ok.

test_round() ->
    2 = erlang:round(id(1.6)),
    1 = erlang:round(id(1.4)),
    -2 = erlang:round(id(-1.6)),
    5 = erlang:round(id(5)),
    ok.

test_self() ->
    Pid = erlang:self(),
    true = erlang:is_pid(Pid),
    ok.

test_setelement() ->
    {x, b, c} = erlang:setelement(1, id({a, b, c}), id(x)),
    {a, x, c} = erlang:setelement(2, id({a, b, c}), id(x)),
    ok.

test_size() ->
    3 = erlang:size(id({a, b, c})),
    5 = erlang:size(id(<<"hello">>)),
    ok.

test_throw() ->
    try
        erlang:throw(id(test_value))
    catch
        throw:test_value -> ok
    end.

test_tl() ->
    [2, 3] = erlang:tl(id([1, 2, 3])),
    [] = erlang:tl(id([a])),
    ok.

test_trunc() ->
    1 = erlang:trunc(id(1.9)),
    -1 = erlang:trunc(id(-1.9)),
    5 = erlang:trunc(id(5)),
    ok.

test_tuple_size() ->
    3 = erlang:tuple_size(id({a, b, c})),
    0 = erlang:tuple_size(id({})),
    ok.

test_tuple_to_list() ->
    [a, b, c] = erlang:tuple_to_list(id({a, b, c})),
    [] = erlang:tuple_to_list(id({})),
    ok.
