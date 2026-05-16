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

-module(test_native_records).
-export([start/0, id/1, name/1]).

-import_record(test_native_records_ext, [vector]).

-record #pair{a, b}.
-record(#point{x = 0, y = 0, z = 0}).
-record(#empty{}).
-record(#with_list_default{items = [a, b, c]}).
-record(#with_complex_defaults{
    tuple_val = {alpha, {beta, gamma}},
    list_val = [one, two, {three, four}],
    big_val = 16#123456789ABCDEF0
}).

id(X) -> X.

start() ->
    ok = test_construct_and_access(),
    ok = test_defaults(),
    ok = test_update(),
    ok = test_is_record_folded(),
    ok = test_pattern_match(),
    ok = test_term_to_binary_roundtrip(),
    ok = test_is_record_1_runtime(),
    ok = test_is_record_2_runtime(),
    ok = test_is_record_opcodes(),
    ok = test_is_tuple_disjoint_from_record(),
    ok = test_get_record_field_runtime(),
    ok = test_pattern_match_runtime(),
    ok = test_term_ordering(),
    ok = test_term_ordering_multifield_atomvm_only(),
    ok = test_term_equality(),
    ok = test_records_in_collections(),
    ok = test_record_message_passing(),
    ok = test_empty_record(),
    ok = test_list_default(),
    ok = test_external_format_roundtrip_runtime(),
    ok = test_cross_module_construct_and_access(),
    ok = test_cross_module_pattern_match(),
    ok = test_cross_module_field_access_runtime(),
    ok = test_cross_module_defaults(),
    ok = test_otp_format_compatibility(),
    ok = test_otp_binary_decodes(),
    ok = test_unexported_record_from_outside(),
    ok = test_get_record_elements_multi(),
    ok = test_get_record_elements_no_partial_write(),
    ok = test_malformed_binary_decode(),
    ok = test_guard_get_record_field(),
    ok = test_gc_stress_with_records(),
    ok = test_empty_record_ordering(),
    ok = test_records_module_inspectors(),
    ok = test_put_record_defaults_gc_stress(),
    ok = test_records_get_definition_complex_defaults(),
    ok = test_records_get_definition_no_silent_default_loss(),
    0.

test_construct_and_access() ->
    P = #pair{a = 1, b = 2},
    1 = P#pair.a,
    2 = P#pair.b,
    ok.

test_defaults() ->
    Origin = #point{},
    0 = Origin#point.x,
    0 = Origin#point.y,
    0 = Origin#point.z,
    P = #point{x = 10, y = 20},
    10 = P#point.x,
    20 = P#point.y,
    0 = P#point.z,
    ok.

test_update() ->
    P0 = #point{x = 1, y = 2, z = 3},
    P1 = P0#point{y = 200},
    1 = P1#point.x,
    200 = P1#point.y,
    3 = P1#point.z,
    ok.

test_is_record_folded() ->
    P = #pair{a = 1, b = 2},
    true = is_record(P),
    true = is_record(P, pair),
    false = is_record(P, point),
    false = is_record({pair, 1, 2}),
    false = is_record(not_a_record),
    ok.

test_pattern_match() ->
    P = #pair{a = foo, b = bar},
    #pair{a = foo, b = bar} = P,
    a = name(#pair{a = 1, b = 2}),
    b = name(#point{}),
    ok.

name(#pair{}) -> a;
name(#point{}) -> b.

test_term_to_binary_roundtrip() ->
    P = #pair{a = 1, b = 2},
    Bin = erlang:term_to_binary(P),
    <<131, 67, _Rest/binary>> = Bin,
    P = erlang:binary_to_term(Bin),
    Q = #point{x = 10, y = 20, z = 30},
    Q = erlang:binary_to_term(erlang:term_to_binary(Q)),
    ok.

test_is_record_1_runtime() ->
    P = ?MODULE:id(#pair{a = 1, b = 2}),
    Q = ?MODULE:id(#point{}),
    T = ?MODULE:id({pair, 1, 2}),
    N = ?MODULE:id(42),
    L = ?MODULE:id([1, 2]),

    true = is_record(P),
    true = is_record(Q),
    false = is_record(T),
    false = is_record(N),
    false = is_record(L),

    ok =
        case is_record(?MODULE:id(P)) of
            true -> ok;
            false -> error(rec_check_fail)
        end,
    ok =
        case is_record(?MODULE:id(T)) of
            true -> error(tuple_misidentified);
            false -> ok
        end,
    ok.

test_is_record_2_runtime() ->
    P = ?MODULE:id(#pair{a = 1, b = 2}),
    T = ?MODULE:id({pair, 1, 2}),
    PairName = ?MODULE:id(pair),
    PointName = ?MODULE:id(point),

    true = is_record(P, PairName),
    false = is_record(P, PointName),

    true = is_record(T, PairName),
    false = is_record(T, PointName),

    false = is_record(?MODULE:id({}), PairName),
    false = is_record(?MODULE:id({}), PointName),

    false = is_record(?MODULE:id(42), PairName),
    ok.

%% Exercise the runtime type-test opcodes emitted by the OTP 29 compiler:
%% OP_IS_NATIVE_RECORD (function-head patterns and is_record/2 guards with a
%% literal record name) and OP_IS_ANY_NATIVE_RECORD (is_record/1 in guards).
%% Without these helpers, no built test BEAM emits either opcode.
test_is_record_opcodes() ->
    P = ?MODULE:id(#pair{a = 1, b = 2}),
    Q = ?MODULE:id(#point{x = 1, y = 2, z = 3}),
    Atom = ?MODULE:id(other),
    N = ?MODULE:id(42),

    pair = classify_is_record(P),
    point = classify_is_record(Q),
    other = classify_is_record(Atom),
    other = classify_is_record(N),

    pair = classify_pattern(P),
    point = classify_pattern(Q),
    other = classify_pattern(Atom),
    other = classify_pattern(N),

    yes = any_native_record(P),
    yes = any_native_record(Q),
    no = any_native_record(Atom),
    no = any_native_record(N),

    ok.

classify_is_record(X) when is_record(X, pair) -> pair;
classify_is_record(X) when is_record(X, point) -> point;
classify_is_record(_) -> other.

classify_pattern(#pair{}) -> pair;
classify_pattern(#point{}) -> point;
classify_pattern(_) -> other.

any_native_record(X) when is_record(X) -> yes;
any_native_record(_) -> no.

test_is_tuple_disjoint_from_record() ->
    R = ?MODULE:id(#pair{a = 1, b = 2}),
    T = ?MODULE:id({pair, 1, 2}),
    false = is_tuple(R),
    true = is_record(R),
    true = is_tuple(T),
    false = is_record(T),
    ok.

test_get_record_field_runtime() ->
    P = ?MODULE:id(#pair{a = 11, b = 22}),
    Q = ?MODULE:id(#point{x = 33, y = 44, z = 55}),
    11 = P#pair.a,
    22 = P#pair.b,
    33 = Q#point.x,
    44 = Q#point.y,
    55 = Q#point.z,
    Bad = ?MODULE:id({pair, 1, 2}),
    {badrecord, Bad} = try Bad#pair.a catch error:E1 -> E1 end,
    {badrecord, Q} = try Q#pair.a catch error:E2 -> E2 end,
    {badrecord, P} =
        try P#test_native_records_ext:vector.x catch error:E3 -> E3 end,
    ok.

test_pattern_match_runtime() ->
    P = ?MODULE:id(#pair{a = foo, b = bar}),
    #pair{a = A, b = B} = P,
    foo = A,
    bar = B,
    a = pick(?MODULE:id(#pair{a = 1, b = 2})),
    b = pick(?MODULE:id(#point{})),
    other = pick(?MODULE:id({pair, 1, 2})),
    ok.

pick(#pair{}) -> a;
pick(#point{}) -> b;
pick(_) -> other.

test_term_ordering() ->
    P = ?MODULE:id(#pair{a = 1, b = 2}),
    Q = ?MODULE:id(#point{x = 1, y = 2, z = 3}),
    T = ?MODULE:id({pair, 1, 2}),
    M = ?MODULE:id(#{a => 1}),
    L = ?MODULE:id([1, 2]),
    true = T < P,
    true = P < M,
    true = M < L,
    R1 = ?MODULE:id(#pair{a = 1, b = 2}),
    R2 = ?MODULE:id(#pair{a = 2, b = 2}),
    true = R1 < R2,
    true = P < Q,
    ok.

test_term_ordering_multifield_atomvm_only() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            %% See https://github.com/erlang/otp/pull/11107
            ok;
        "ATOM" ->
            R1 = ?MODULE:id(#pair{a = 1, b = 2}),
            R3 = ?MODULE:id(#pair{a = 1, b = 3}),
            true = R1 < R3,
            P1 = ?MODULE:id(#point{x = 1, y = 1, z = 1}),
            P2 = ?MODULE:id(#point{x = 1, y = 2, z = 1}),
            P3 = ?MODULE:id(#point{x = 1, y = 1, z = 2}),
            true = P1 < P2,
            true = P1 < P3,
            ok
    end.

test_term_equality() ->
    P1 = ?MODULE:id(#pair{a = 1, b = 2}),
    P2 = ?MODULE:id(#pair{a = 1, b = 2}),
    P3 = ?MODULE:id(#pair{a = 1, b = 3}),
    Q = ?MODULE:id(#point{x = 1, y = 2, z = 3}),
    T = ?MODULE:id({pair, 1, 2}),
    true = P1 =:= P2,
    true = P1 == P2,
    false = P1 =:= P3,
    false = P1 =:= Q,
    false = P1 =:= T,
    false = P1 == T,
    ok.

test_records_in_collections() ->
    ok = collection_list(),
    ok = collection_tuple(),
    ok = collection_map(),
    ok = collection_nested_record(),
    ok.

collection_map() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    M = ?MODULE:id(#{first => P, second => Q}),
    P = maps:get(first, M),
    Q = maps:get(second, M),
    ok.

collection_list() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    [LP, LQ, LP2] = ?MODULE:id([P, Q, P]),
    P = LP,
    Q = LQ,
    P = LP2,
    ok.

collection_tuple() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    {wrapper, TP, TQ} = ?MODULE:id({wrapper, P, Q}),
    P = TP,
    Q = TQ,
    ok.

collection_nested_record() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    Nested = ?MODULE:id(#pair{a = P, b = Q}),
    P = Nested#pair.a,
    Q = Nested#pair.b,
    ok.

test_record_message_passing() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    Self = self(),
    Pid = spawn_opt(
        fun() ->
            receive
                Msg -> Self ! {got, Msg}
            end
        end,
        []
    ),
    Pid ! {P, Q},
    receive
        {got, {RP, RQ}} ->
            P = RP,
            Q = RQ
    after 1000 ->
        error(timeout)
    end,
    ok.

test_empty_record() ->
    E = ?MODULE:id(#empty{}),
    true = is_record(E),
    true = is_record(E, empty),
    false = is_tuple(E),
    E2 = #empty{},
    true = E =:= E2,
    E = erlang:binary_to_term(erlang:term_to_binary(E)),
    ok.

test_list_default() ->
    R = ?MODULE:id(#with_list_default{}),
    [a, b, c] = R#with_list_default.items,
    ok.

test_cross_module_construct_and_access() ->
    _ = test_native_records_ext:module_info(module),
    V = ?MODULE:id(#test_native_records_ext:vector{x = 1, y = 2, z = 3}),
    1 = V#vector.x,
    2 = V#vector.y,
    3 = V#vector.z,
    ok.

test_cross_module_pattern_match() ->
    V = ?MODULE:id(#test_native_records_ext:vector{x = 10, y = 20, z = 30}),
    #test_native_records_ext:vector{x = X, y = Y, z = Z} = V,
    10 = X,
    20 = Y,
    30 = Z,
    ok.

test_cross_module_field_access_runtime() ->
    V = ?MODULE:id(test_native_records_ext:make_vector(7, 8, 9)),
    7 = V#test_native_records_ext:vector.x,
    8 = V#test_native_records_ext:vector.y,
    9 = V#test_native_records_ext:vector.z,
    ok.

test_cross_module_defaults() ->
    _ = test_native_records_ext:module_info(module),
    T = ?MODULE:id(#test_native_records_ext:tagged{}),
    ext_default_tag = T#test_native_records_ext:tagged.tag,
    [ext_one, ext_two, ext_three] =
        T#test_native_records_ext:tagged.items,
    ext_default_note = T#test_native_records_ext:tagged.note,
    T2 = ?MODULE:id(#test_native_records_ext:tagged{tag = overridden}),
    overridden = T2#test_native_records_ext:tagged.tag,
    [ext_one, ext_two, ext_three] =
        T2#test_native_records_ext:tagged.items,
    ext_default_note = T2#test_native_records_ext:tagged.note,
    ok.

test_external_format_roundtrip_runtime() ->
    P = #pair{a = 1, b = 2},
    Q = #point{x = 10, y = 20, z = 30},
    Structure = ?MODULE:id({records, [P, Q], #{key => P}}),
    Decoded = erlang:binary_to_term(erlang:term_to_binary(Structure)),
    Structure = Decoded,
    {records, [DP, DQ], #{key := DP2}} = Decoded,
    P = DP,
    Q = DQ,
    P = DP2,
    ok.

test_otp_format_compatibility() ->
    Pair = #pair{a = foo, b = bar},
    ExpectedPair = <<131, 67, 0,0,0,2, 0,
                     119, 19, "test_native_records",
                     119, 4, "pair",
                     119, 1, "a", 119, 1, "b",
                     119, 3, "foo", 119, 3, "bar">>,
    ExpectedPair = erlang:term_to_binary(Pair),

    Point = #point{x = 1, y = 2, z = 3},
    ExpectedPoint = <<131, 67, 0,0,0,3, 0,
                      119, 19, "test_native_records",
                      119, 5, "point",
                      119, 1, "x", 119, 1, "y", 119, 1, "z",
                      97, 1, 97, 2, 97, 3>>,
    ExpectedPoint = erlang:term_to_binary(Point),

    Empty = #empty{},
    ExpectedEmpty = <<131, 67, 0,0,0,0, 0,
                      119, 19, "test_native_records",
                      119, 5, "empty">>,
    ExpectedEmpty = erlang:term_to_binary(Empty),

    Vec = #test_native_records_ext:vector{x = 7, y = 8, z = 9},
    ExpectedVec = <<131, 67, 0,0,0,3, 1,
                    119, 23, "test_native_records_ext",
                    119, 6, "vector",
                    119, 1, "x", 119, 1, "y", 119, 1, "z",
                    97, 7, 97, 8, 97, 9>>,
    ExpectedVec = erlang:term_to_binary(Vec),
    ok.

test_otp_binary_decodes() ->
    PairBin =
        <<131, 67, 0, 0, 0, 2, 0, 119, 19, "test_native_records", 119, 4, "pair", 119, 1, "a", 119,
            1, "b", 119, 3, "foo", 119, 3, "bar">>,
    Pair = erlang:binary_to_term(PairBin),
    true = is_record(Pair, pair),
    foo = Pair#pair.a,
    bar = Pair#pair.b,

    PointBin =
        <<131, 67, 0, 0, 0, 3, 0, 119, 19, "test_native_records", 119, 5, "point", 119, 1, "x", 119,
            1, "y", 119, 1, "z", 97, 11, 97, 22, 97, 33>>,
    Pt = erlang:binary_to_term(PointBin),
    true = is_record(Pt, point),
    11 = Pt#point.x,
    22 = Pt#point.y,
    33 = Pt#point.z,

    EmptyBin = <<131, 67, 0, 0, 0, 0, 0, 119, 19, "test_native_records", 119, 5, "empty">>,
    E = erlang:binary_to_term(EmptyBin),
    true = is_record(E, empty),

    VecBin =
        <<131, 67, 0, 0, 0, 3, 1, 119, 23, "test_native_records_ext", 119, 6, "vector", 119, 1, "x",
            119, 1, "y", 119, 1, "z", 97, 1, 97, 2, 97, 3>>,
    V = erlang:binary_to_term(VecBin),
    true = is_record(V, vector),
    1 = V#vector.x,
    2 = V#vector.y,
    3 = V#vector.z,
    ok.

test_unexported_record_from_outside() ->
    U = ?MODULE:id(test_native_records_ext:make_unexported(11, 22)),
    UnexportedName = ?MODULE:id(unexported),
    PairName = ?MODULE:id(pair),
    true = is_record(U),
    true = is_record(U, UnexportedName),
    false = is_record(U, PairName),
    false = is_tuple(U),

    U2 = test_native_records_ext:make_unexported(11, 22),
    true = U =:= U2,
    UDiff = test_native_records_ext:make_unexported(11, 99),
    false = U =:= UDiff,

    UBack = erlang:binary_to_term(erlang:term_to_binary(U)),
    true = is_record(UBack, UnexportedName),
    true = U =:= UBack,

    Self = self(),
    Pid = spawn_opt(
        fun() ->
            receive
                Msg -> Self ! {got, Msg}
            end
        end,
        []
    ),
    Pid ! {wrap, [U, UDiff]},
    receive
        {got, {wrap, [RU, RUDiff]}} ->
            true = is_record(RU, UnexportedName),
            true = is_record(RUDiff, UnexportedName),
            true = U =:= RU,
            true = UDiff =:= RUDiff
    after 1000 ->
        error(timeout)
    end,
    ok.

test_get_record_elements_multi() ->
    P1 = ?MODULE:id(#point{x = 100, y = 200, z = 300}),
    #point{x = X1, y = Y1, z = Z1} = P1,
    100 = X1,
    200 = Y1,
    300 = Z1,

    #point{z = Z2, x = X2, y = Y2} = P1,
    100 = X2,
    200 = Y2,
    300 = Z2,

    #point{x = X3, z = Z3} = P1,
    100 = X3,
    300 = Z3,
    ok.

test_get_record_elements_no_partial_write() ->
    Bind = ?MODULE:id(99),
    Result =
        case ?MODULE:id(#point{x = 1, y = 2, z = 3}) of
            #pair{a = Bind} ->
                error({unexpected_match, Bind});
            _Other ->
                Bind
        end,
    99 = Result,

    Outer = ?MODULE:id(42),
    case ?MODULE:id(#point{x = 7, y = 8, z = 9}) of
        #pair{a = _A, b = _B} ->
            error(matched_wrong_def);
        #point{x = X, y = Y, z = Z} ->
            7 = X,
            8 = Y,
            9 = Z,
            42 = Outer
    end,
    ok.

test_malformed_binary_decode() ->
    ModName = <<"test_native_records">>,
    ModLen = byte_size(ModName),
    ModAtomBytes = <<119, ModLen, ModName/binary>>,
    PairName = <<"pair">>,
    PairAtomBytes = <<119, (byte_size(PairName)), PairName/binary>>,
    KeyA = <<119, 1, "a">>,
    KeyB = <<119, 1, "b">>,
    Truncated =
        <<131, 67, 0, 0, 0, 2, 0, ModAtomBytes/binary, PairAtomBytes/binary, KeyA/binary,
            KeyB/binary, 97, 1, 97>>,
    badarg = decode_expect_badarg(Truncated),
    BadModType =
        <<131, 67, 0, 0, 0, 2, 0, 97, 1, PairAtomBytes/binary, KeyA/binary, KeyB/binary, 97, 1, 97,
            2>>,
    badarg = decode_expect_badarg(BadModType),
    BadNameType =
        <<131, 67, 0, 0, 0, 2, 0, ModAtomBytes/binary, 97, 1, KeyA/binary, KeyB/binary, 97, 1, 97,
            2>>,
    badarg = decode_expect_badarg(BadNameType),

    case erlang:system_info(machine) of
        "BEAM" ->
            ok;
        "ATOM" ->
            BadExported =
                <<131, 67, 0, 0, 0, 2, 2, ModAtomBytes/binary, PairAtomBytes/binary, KeyA/binary,
                    KeyB/binary, 97, 1, 97, 2>>,
            badarg = decode_expect_badarg(BadExported),
            UnknownName = <<119, 7, "missing">>,
            BadUnknownDef =
                <<131, 67, 0, 0, 0, 0, 0, ModAtomBytes/binary, UnknownName/binary>>,
            badarg = decode_expect_badarg(BadUnknownDef),
            BadArity =
                <<131, 67, 0, 0, 0, 3, 0, ModAtomBytes/binary, PairAtomBytes/binary, KeyA/binary,
                    KeyB/binary, <<119, 1, "c">>/binary, 97, 1, 97, 2, 97, 3>>,
            badarg = decode_expect_badarg(BadArity),
            BadExportedFlag =
                <<131, 67, 0, 0, 0, 2, 1, ModAtomBytes/binary, PairAtomBytes/binary, KeyA/binary,
                    KeyB/binary, 97, 1, 97, 2>>,
            badarg = decode_expect_badarg(BadExportedFlag),
            BadKeyOrder =
                <<131, 67, 0, 0, 0, 2, 0, ModAtomBytes/binary, PairAtomBytes/binary, KeyB/binary,
                    KeyA/binary, 97, 1, 97, 2>>,
            badarg = decode_expect_badarg(BadKeyOrder),
            ok
    end,
    ok.

decode_expect_badarg(Bin) ->
    try erlang:binary_to_term(Bin) of
        _ -> {unexpected, decoded}
    catch
        error:badarg -> badarg
    end.

test_guard_get_record_field() ->
    other = first_a_one(?MODULE:id({not_a_record, 1, 2})),
    other = first_a_one(?MODULE:id(#point{x = 1, y = 2, z = 3})),
    other = first_a_one(?MODULE:id(#pair{a = 999, b = 1})),
    matched = first_a_one(?MODULE:id(#pair{a = 1, b = 2})),
    ok.

first_a_one(X) when X#pair.a =:= 1 -> matched;
first_a_one(_) -> other.

test_gc_stress_with_records() ->
    Records = build_record_list(64, []),
    erlang:garbage_collect(),
    ok = verify_records(Records, 0),
    Nested = [?MODULE:id({nest, R, [R, R], #{key => R}}) || R <- Records],
    erlang:garbage_collect(),
    ok = verify_nested(Nested, 0),
    ok.

build_record_list(0, Acc) ->
    lists:reverse(Acc);
build_record_list(N, Acc) ->
    R = ?MODULE:id(#point{x = N, y = N * 2, z = N * 3}),
    build_record_list(N - 1, [R | Acc]).

verify_records([], _) ->
    ok;
verify_records([R | Rest], Idx) ->
    Expected = 64 - Idx,
    Expected = R#point.x,
    ExpectedY = Expected * 2,
    ExpectedY = R#point.y,
    ExpectedZ = Expected * 3,
    ExpectedZ = R#point.z,
    verify_records(Rest, Idx + 1).

verify_nested([], _) ->
    ok;
verify_nested([{nest, R, [R1, R2], Map} | Rest], Idx) ->
    Expected = 64 - Idx,
    Expected = R#point.x,
    Expected = R1#point.x,
    Expected = R2#point.x,
    #{key := R3} = Map,
    Expected = R3#point.x,
    verify_nested(Rest, Idx + 1).

test_empty_record_ordering() ->
    E = ?MODULE:id(#empty{}),
    P = ?MODULE:id(#pair{a = 0, b = 0}),
    true = E < P,
    false = P < E,
    true = E =:= ?MODULE:id(#empty{}),
    true = ?MODULE:id({any, tuple}) < E,
    true = E < ?MODULE:id(#{any => map}),
    ok.

test_records_module_inspectors() ->
    P = ?MODULE:id(#pair{a = 11, b = 22}),
    V = ?MODULE:id(test_native_records_ext:make_vector(1, 2, 3)),
    E = ?MODULE:id(#empty{}),

    11 = records:get(a, P),
    22 = records:get(b, P),
    1 = records:get(x, V),
    badarg = catch_error(fun() -> records:get(missing, P) end),
    badarg = catch_error(fun() -> records:get(a, ?MODULE:id({pair, 1, 2})) end),
    badarg = catch_error(fun() -> records:get(<<"a">>, P) end),

    test_native_records = records:get_module(P),
    test_native_records_ext = records:get_module(V),
    badarg = catch_error(fun() -> records:get_module(?MODULE:id(42)) end),

    pair = records:get_name(P),
    vector = records:get_name(V),
    empty = records:get_name(E),
    badarg = catch_error(fun() -> records:get_name(?MODULE:id({not_a_rec})) end),

    [a, b] = records:get_field_names(P),
    [x, y, z] = records:get_field_names(V),
    [] = records:get_field_names(E),
    BadRec = ?MODULE:id({pair, 1, 2}),
    {badrecord, BadRec} = catch_error(fun() -> records:get_field_names(BadRec) end),

    false = records:is_exported(P),
    true = records:is_exported(V),
    false = records:is_exported(E),
    {badrecord, BadRec} = catch_error(fun() -> records:is_exported(BadRec) end),

    {#{is_exported := false}, PairFields} =
        records:get_definition(test_native_records, pair),
    [a, b] = PairFields,
    {#{is_exported := true}, VecFields} =
        records:get_definition(test_native_records_ext, vector),
    [{x, 0}, {y, 0}, {z, 0}] = VecFields,
    {#{is_exported := false}, [{items, [a, b, c]}]} =
        records:get_definition(test_native_records, with_list_default),
    {#{is_exported := false}, []} =
        records:get_definition(test_native_records, empty),
    badarg = catch_error(fun() -> records:get_definition(no_such_module, x) end),
    badarg = catch_error(fun() ->
        records:get_definition(test_native_records, no_such_record)
    end),
    badarg = catch_error(fun() -> records:get_definition(<<"foo">>, x) end),

    ok.

test_put_record_defaults_gc_stress() ->
    Records = build_complex_default_list(128, []),
    erlang:garbage_collect(),
    ok = verify_complex_defaults(Records),
    ok.

build_complex_default_list(0, Acc) ->
    Acc;
build_complex_default_list(N, Acc) ->
    R = ?MODULE:id(#with_complex_defaults{}),
    build_complex_default_list(N - 1, [R | Acc]).

verify_complex_defaults([]) ->
    ok;
verify_complex_defaults([R | Rest]) ->
    {alpha, {beta, gamma}} = R#with_complex_defaults.tuple_val,
    [one, two, {three, four}] = R#with_complex_defaults.list_val,
    16#123456789ABCDEF0 = R#with_complex_defaults.big_val,
    verify_complex_defaults(Rest).

test_records_get_definition_complex_defaults() ->
    {#{is_exported := false}, Fields} =
        records:get_definition(test_native_records, with_complex_defaults),
    [
        {tuple_val, {alpha, {beta, gamma}}},
        {list_val, [one, two, {three, four}]},
        {big_val, 16#123456789ABCDEF0}
    ] = Fields,
    ok.

test_records_get_definition_no_silent_default_loss() ->
    %% Regression for the silent default-decode loss: a field whose stored
    %% default decodes successfully must round-trip to a {Name, Default}
    %% tuple, never appear as a bare Name atom.
    {#{is_exported := false}, FieldsP} =
        records:get_definition(test_native_records, point),
    [{x, 0}, {y, 0}, {z, 0}] = FieldsP,
    {#{is_exported := false}, FieldsW} =
        records:get_definition(test_native_records, with_list_default),
    [{items, [a, b, c]}] = FieldsW,
    ok.

catch_error(Fun) ->
    try Fun() of
        Value -> {unexpected, Value}
    catch
        error:Reason -> Reason
    end.
