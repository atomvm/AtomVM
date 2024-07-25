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

-module(test_queue).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_from_to_list(),
    ok = test_new_and_is_queue(),
    ok = test_len_and_in(),
    ok = test_in_and_from_to_list(),
    ok = test_len_and_out(),
    ok = test_out_on_empty(),
    ok = test_out_r(),
    ok = test_in_r(),
    ok = test_member(),
    ok = test_basic_queue_flow(),
    ok = test_get(),
    ok = test_get_empty(),
    ok = test_get_r(),
    ok = test_get_r_empty(),
    ok = test_peek(),
    ok = test_peek_r(),
    ok = test_drop(),
    ok = test_drop_r(),
    ok = test_reverse(),
    ok = test_join(),
    ok = test_split(),
    ok = test_filter(),
    ok = test_filter_replace(),
    ok = test_filtermap(get_otp_version()),
    ok = test_filtermap_replace(get_otp_version()),
    ok = test_fold(get_otp_version()),
    ok = test_any(get_otp_version()),
    ok = test_all(get_otp_version()),
    ok = test_delete(get_otp_version()),
    ok = test_delete_with(get_otp_version()),
    ok = test_delete_with_r(get_otp_version()),
    ok = test_complete_flow(),
    ok.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" -> list_to_integer(erlang:system_info(otp_release));
        _ -> atomvm
    end.

test_from_to_list() ->
    L = [1, -1, 2, -3],
    Q = queue:from_list(L),
    ?ASSERT_MATCH(queue:to_list(Q), L),
    ok.

test_new_and_is_queue() ->
    Q = queue:new(),
    ?ASSERT_MATCH(queue:is_queue(Q), true),
    ?ASSERT_MATCH(queue:is_queue(<<"Foo">>), false),
    ok.

test_len_and_in() ->
    Q0 = queue:new(),
    ?ASSERT_MATCH(queue:len(Q0), 0),
    Q1 = queue:in(1, Q0),
    ?ASSERT_MATCH(queue:len(Q1), 1),
    ok.

test_len_and_out() ->
    Q0 = queue:new(),
    Q1 = queue:in(1, Q0),
    ?ASSERT_MATCH(queue:len(Q1), 1),
    {{value, 1}, Q2} = queue:out(Q1),
    ?ASSERT_MATCH(queue:len(Q2), 0),
    ok.

test_in_and_from_to_list() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:in(100, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 3, 4, 5, 100]),
    ok.

test_out_on_empty() ->
    Q0 = queue:new(),
    ?ASSERT_MATCH(queue:out(Q0), {empty, Q0}),
    ok.

test_in_r() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:in_r(100, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [100, 1, 2, 3, 4, 5]),
    ok.

test_out_r() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    {{value, 5}, Q1} = queue:out_r(Q),
    queue:to_list(Q1),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 3, 4]),
    ok.

test_member() ->
    L = [1, -1, 2, -3],
    Q = queue:from_list(L),

    ?ASSERT_MATCH(queue:member(1, Q), true),
    ?ASSERT_MATCH(queue:member(-1, Q), true),
    ?ASSERT_MATCH(queue:member(2, Q), true),
    ?ASSERT_MATCH(queue:member(-3, Q), true),
    ?ASSERT_MATCH(queue:member(20, Q), false),
    ?ASSERT_MATCH(queue:member(0, queue:new()), false),

    ok.

test_basic_queue_flow() ->
    Q0 = queue:new(),
    Q1 = queue:in(1, Q0),
    Q2 = queue:in(2, Q1),
    Q3 = queue:in(3, Q2),

    ?ASSERT_MATCH(queue:is_empty(Q3), false),
    ?ASSERT_MATCH(queue:len(Q3), 3),

    {V1, O1} = queue:out(Q3),
    ?ASSERT_MATCH(V1, {value, 1}),

    {V2, O2} = queue:out(O1),
    ?ASSERT_MATCH(V2, {value, 2}),

    {V3, O3} = queue:out(O2),
    ?ASSERT_MATCH(V3, {value, 3}),

    ?ASSERT_MATCH(queue:is_empty(O3), true),

    ok.

test_get() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:get(Q), 1),
    ok.

test_get_empty() ->
    Empty = queue:new(),
    %queue:get(Empty),
    ?ASSERT_ERROR(queue:get(Empty), empty),
    ok.

test_get_r() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:get_r(Q), 5),
    ok.

test_get_r_empty() ->
    Empty = queue:new(),
    ?ASSERT_ERROR(queue:get_r(Empty), empty),
    ok.

test_peek() ->
    ?ASSERT_MATCH(queue:peek(queue:new()), empty),
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:peek(Q), {value, 1}),
    ok.

test_peek_r() ->
    ?ASSERT_MATCH(queue:peek_r(queue:new()), empty),
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:peek_r(Q), {value, 5}),
    ok.

test_drop() ->
    Q0 = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:drop(Q0),
    ?ASSERT_MATCH(queue:to_list(Q1), [2, 3, 4, 5]),
    ok.

test_drop_r() ->
    Q0 = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:drop_r(Q0),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 3, 4]),
    ok.

test_reverse() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    QR = queue:reverse(Q),
    ?ASSERT_MATCH(queue:to_list(QR), [5, 4, 3, 2, 1]),
    ok.

test_join() ->
    Q1 = queue:from_list([1, 3]),
    Q2 = queue:from_list([2, 4]),
    ?ASSERT_MATCH(queue:to_list(queue:join(Q1, Q2)), [1, 3, 2, 4]),
    ok.

test_split() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),

    {Q1, Q2} = queue:split(2, Q),
    ?ASSERT_MATCH({queue:to_list(Q1), queue:to_list(Q2)}, {[1, 2], [3, 4, 5]}),

    {Q3, Q4} = queue:split(0, Q),
    ?ASSERT_MATCH({queue:to_list(Q3), queue:to_list(Q4)}, {[], [1, 2, 3, 4, 5]}),

    {Q5, Q6} = queue:split(5, Q),
    ?ASSERT_MATCH({queue:to_list(Q5), queue:to_list(Q6)}, {[1, 2, 3, 4, 5], []}),

    ok.

test_filter() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:filter(fun(E) -> E > 2 end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [3, 4, 5]),
    ok.

test_filter_replace() ->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:filter(fun(E) -> [E, E + 1] end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 2, 3, 3, 4, 4, 5, 5, 6]),
    ok.

test_filtermap(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:filtermap(fun(E) -> E > 2 end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [3, 4, 5]),
    ok;
test_filtermap(_) ->
    ok.

test_filtermap_replace(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:filtermap(fun(E) -> {true, E + 100} end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [101, 102, 103, 104, 105]),
    ok;
test_filtermap_replace(_) ->
    ok.

test_fold(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:fold(fun(X, Sum) -> X + Sum end, 0, Q), 15),
    ok;
test_fold(_) ->
    ok.

test_any(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:any(fun(E) -> E > 10 end, Q), false),
    ?ASSERT_MATCH(queue:any(fun(E) -> E > 3 end, Q), true),
    ok;
test_any(_) ->
    ok.

test_all(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    ?ASSERT_MATCH(queue:all(fun(E) -> E > 3 end, Q), false),
    ?ASSERT_MATCH(queue:all(fun(E) -> E > 0 end, Q), true),
    ok;
test_all(_) ->
    ok.

test_delete(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5]),
    Q1 = queue:delete(3, Q),
    ?ASSERT_MATCH(queue:member(3, Q1), false),
    ok;
test_delete(_) ->
    ok.

test_delete_with(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([100, 1, 2, 3, 4, 5]),
    Q1 = queue:delete_with(fun(E) -> E > 0 end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 3, 4, 5]),
    ok;
test_delete_with(_) ->
    ok.

test_delete_with_r(OTPVersion) when
    (is_integer(OTPVersion) andalso OTPVersion >= 24) orelse OTPVersion == atomvm
->
    Q = queue:from_list([1, 2, 3, 4, 5, 100]),
    Q1 = queue:delete_with_r(fun(E) -> E > 10 end, Q),
    ?ASSERT_MATCH(queue:to_list(Q1), [1, 2, 3, 4, 5]),
    ok;
test_delete_with_r(_) ->
    ok.

test_complete_flow() ->
    Q0 = queue:new(),
    Q1 = queue:in(-2, Q0),
    Q2 = queue:in(-1, Q1),
    Q3 = queue:in(0, Q2),
    Q4 = queue:in(1, Q3),
    Q5 = queue:in(2, Q4),

    ?ASSERT_MATCH(queue:len(Q5), 5),

    {V1, Q6} = queue:out(Q5),
    ?ASSERT_MATCH(V1, {value, -2}),

    Q7 = queue:in(3, Q6),
    Q8 = queue:in(4, Q7),

    {V2, Q9} = queue:out(Q8),
    ?ASSERT_MATCH(V2, {value, -1}),
    {V3, Q10} = queue:out(Q9),
    ?ASSERT_MATCH(V3, {value, 0}),

    Q11 = queue:in(5, Q10),

    ?ASSERT_MATCH(queue:to_list(Q11), [1, 2, 3, 4, 5]),

    ok.
