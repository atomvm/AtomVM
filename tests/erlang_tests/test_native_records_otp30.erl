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

%% Native record instructions introduced in OTP 30.
-module(test_native_records_otp30).
-export([start/0, id/1]).

-record #pair{a, b}.
-record #other{a, c}.

start() ->
    ok = test_local_alloc(),
    ok = test_local_match(),
    ok = test_local_update(),
    ok = test_external_match(),
    ok = test_anonymous_update(),
    ok = test_badfield(),
    ok = test_fail_label(),
    ok = test_tracked_type_other_name(),
    0.

id(X) -> X.

%% test_heap with a {records, N} allocation entry
test_local_alloc() ->
    L = make_pairs(200, []),
    [{#pair{a = 1, b = [1]}, #pair{a = {1}, b = 1}} | _] = L,
    ok = check_pairs(L, 1).

make_pairs(0, Acc) -> Acc;
make_pairs(I, Acc) -> make_pairs(I - 1, [{#pair{a = I, b = [I]}, #pair{a = {I}, b = I}} | Acc]).

check_pairs([], 201) -> ok;
check_pairs([{#pair{a = I, b = [I]}, #pair{a = {I}, b = I}} | T], I) -> check_pairs(T, I + 1).

%% get_record_elements_id with a local id
test_local_match() ->
    P = id(#pair{a = 1, b = 2}),
    #pair{a = A, b = B} = P,
    {1, 2} = {A, B},
    ok.

%% update_record_id with a local id
test_local_update() ->
    P0 = id(#pair{a = 1, b = 2}),
    P1 = P0#pair{b = 3},
    #pair{a = 1, b = 3} = P1,
    ok.

%% get_record_elements_id with an external id, including the fail path
test_external_match() ->
    V = test_native_records_ext:make_vector(1, 2, 3),
    {1, 2, 3} = vector_xyz(V),
    nomatch = vector_xyz(id(#pair{a = 1, b = 2})),
    nomatch = vector_xyz(id({vector, 1, 2, 3})),
    ok.

vector_xyz(#test_native_records_ext:vector{x = X, y = Y, z = Z}) -> {X, Y, Z};
vector_xyz(_) -> nomatch.

%% update_record_id with an anonymous ([]) id
test_anonymous_update() ->
    V0 = test_native_records_ext:make_vector(1, 2, 3),
    V1 = (id(V0))#_{y = 20},
    {1, 20, 3} = vector_xyz(V1),
    P = (id(#pair{a = 1, b = 2}))#_{a = 10},
    #pair{a = 10, b = 2} = P,
    ok.

test_badfield() ->
    V = test_native_records_ext:make_vector(1, 2, 3),
    BadField = {{test_native_records_ext, vector}, a},
    BadField =
        try
            #test_native_records_ext:vector{a = id(1)}
        catch
            error:{badfield, F1} -> F1
        end,
    BadField =
        try
            (id(V))#test_native_records_ext:vector{a = 1}
        catch
            error:{badfield, F2} -> F2
        end,
    BadField =
        try
            (id(V))#_{a = 1}
        catch
            error:{badfield, F3} -> F3
        end,
    ok.

%% put_record and update_record_id with a non-zero fail label
test_fail_label() ->
    [true] = ext_creation(),
    [true] = ext_update(test_native_records_ext:make_vector(1, 2, 3)),
    ok.

ext_creation() ->
    [
        try
            #test_native_records_ext:vector{a = 1}
        catch
            _:_ -> true
        end
    ].

ext_update(R) ->
    [
        try
            R#test_native_records_ext:vector{a = 1}
        catch
            _:_ -> true
        end
    ].

%% After is_native_record(other), a get_record_field with another record name
%% must raise badrecord rather than use the tracked layout.
test_tracked_type_other_name() ->
    O = id(#other{a = 7, c = 1}),
    {badrecord, O} =
        try
            wrong_name(O)
        catch
            error:Reason -> Reason
        end,
    ok.

wrong_name(X) when is_record(X, other) -> X#pair.a.
