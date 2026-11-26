# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Finite public-API samples inspired by LynxTest, not translations of proofs."""

import base64
import hashlib
import re
import struct


def cases():
    selected = []

    def probe(source, tests, expression, **extra):
        if isinstance(tests, str):
            tests = [tests]
        selected.append({"lean_origins": [f"{source}.lean#{test}" for test in tests],
                         "expression": expression, **extra})

    def call(source, tests, function, *args, module="erlang", **extra):
        probe(source, tests, f"differential:call({module},'{function}',[{','.join(args)}])", **extra)

    e = "Modules/Erlang"
    for value in ["0", "1", "-7", "1 bsl 100", "1.5", "-1.5", "0.0", "-0.0",
                  "0.5", "-0.5", "5.0e-324", "-5.0e-324", "bad", "[]"]:
        for fn in ["abs", "round", "trunc", "floor", "ceil", "float"]:
            call(e, ["numeric_rounding", "numeric_conversion_boundaries",
                     "numeric_guards_integer_identity", "numeric_guards_invalid"], fn, value)
    for fn, label in [("+", "addition_numeric_types"), ("*", "multiplication_numeric_types"),
                      ("-", "arithmetic_boundaries"), ("/", "arithmetic_boundaries")]:
        for a, b in [("2", "3"), ("1.5", "1.0"), ("1", "1.5"), ("1.5", "-2"),
                     ("9007199254740993", "1.0"), ("1.0", "9007199254740993"),
                     ("5.0e-324", "0.5"), ("-1.5e-323", "0.5"),
                     ("-0.0", "-0.0"), ("-0.0", "0.0"), ("0", "-0.0"),
                     ("1.7976931348623157e308", "1.7976931348623157e308"),
                     ("1", "[]"), ("bad", "1.0")]:
            call(e, label, fn, a, b)
    for label, fn, pairs in [
        ("addition_rounding", "+", [("1.0", "1.1102230246251565e-16"),
                                     ("1.0000000000000002", "1.1102230246251565e-16"),
                                     ("1.9999999999999998", "1.1102230246251565e-16")]),
        ("addition_subnormals_and_zero", "+", [("5.0e-324", "5.0e-324"),
                                                ("2.225073858507201e-308", "5.0e-324")]),
        ("addition_overflow", "+", [("1.7976931348623157e308", "8.98846567431158e307")]),
        ("multiplication_rounding_and_underflow", "*", [("-1.5e-323", "0.5")]),
        ("multiplication_overflow", "*", [("1.7976931348623157e308", "2")]),
    ]:
        for a, b in pairs:
            call(e, label, fn, a, b)
    for fn in ["+", "-", "bnot"]:
        for value in ["0.0", "-0.0", "-10", "[]", "0.5"]:
            call(e, "arithmetic_invalid" if value == "[]" else "arithmetic_boundaries", fn, value)
    for fn, label in [("div", "div/2_signs"), ("rem", "remainder_signs_and_zero"),
                      ("band", "bitwise_and_signs"), ("bor", "bitwise_signs"),
                      ("bxor", "bitwise_signs"), ("bsl", "shifts_signed_values_and_counts"),
                      ("bsr", "shifts_signed_values_and_counts")]:
        for a, b in [("7", "3"), ("-7", "3"), ("7", "-3"), ("-7", "-3"),
                     ("7", "0"), ("-7", "100"), ("10", "-12"),
                     ("(1 bsl 100)+3", "-2"), ("1.0", "2"), ("2", "bad")]:
            call(e, label, fn, a, b)
    for fn in ["min", "max"]:
        for a, b in [("1", "1.0"), ("1.0", "1"), ("0.0", "-0.0"),
                     ("-0.0", "0.0"), ("a", "1"), ("1", "a")]:
            call(e, "min_max_equal_returns_first", fn, a, b)

    # New upstream exact list-subtraction model: duplicates remove only the
    # first exact match. Invalid tails remain OTP error-path observations.
    for left, right in [
        ("[]", "[]"), ("[1,2,1,3]", "[1,3]"), ("[1,2]", "[3,2,2]"),
        ("[1]", "[1.0]"), ("[1.0,1]", "[1]"), ("[1,1.0,1]", "[1.0,1]"),
        ("[0.0,-0.0,0]", "[-0.0]"), ("[{1},#{a=>1},<<5:3>>]", "[{1.0},#{a=>1.0}]"),
        ("[1 bsl 100,7,1 bsl 100]", "[1 bsl 100]"),
        ("[a|tail]", "[]"), ("bad", "[]"),
        ("[a,b|tail]", "[a]"), ("[a]", "[a|tail]"),
    ]:
        call(e, "list_subtraction", "--", left, right)
        if left != right:
            call(e, "list_subtraction", "--", right, left)

    # The new upstream zip/unzip example proves list/pair premises. These
    # finite runtime observations are example-inspired, not freshly proved laws.
    for left, right in [("[]", "[]"), ("[a]", "[7]"),
                        ("[1,1.0,{nested,<<5:3>>}]", "[z,-7,#{a=>9}]"),
                        ("lists:seq(1,33)", "lists:seq(101,133)")]:
        selected.append({"upstream_example": "examples/otp/lists.exs#unzip_zip",
                         "expression": f"A={left},B={right},Z=lists:zip(A,B),"
                                       "{Z,lists:unzip(Z),lists:unzip(Z)=:={A,B}}"})
    for pairs in ["[]", "[{a,7}]", "[{1,a},{1.0,b},{#{x=>7},<<5:3>>}]",
                  "[{a,1},{a,2},{b,3},{a,1}]"]:
        selected.append({"upstream_example": "examples/otp/lists.exs#zip_unzip",
                         "expression": f"P={pairs},{{A,B}}=lists:unzip(P),"
                                       "{A,B,lists:zip(A,B),lists:zip(A,B)=:=P}"})
    for left, right in [("[]", "[a]"), ("[a,b]", "[7]"), ("[a|tail]", "[7]"),
                        ("bad", "[]")]:
        selected.append({"upstream_example": "examples/otp/lists.exs#unzip_zip",
                         "expression": f"differential:call(lists,zip,[{left},{right}])"})
    for pairs in ["[{a,1},bad]", "[{a,1}|tail]", "[{a,1,extra}]", "bad"]:
        selected.append({"upstream_example": "examples/otp/lists.exs#zip_unzip",
                         "expression": f"differential:call(lists,unzip,[{pairs}])"})
    # Upstream sort and seq example laws; finite OTP observations, not proof provenance.
    for xs, ys, probes in [
        ("[]", "[]", "[a,1]"),
        ("[1]", "[]", "[1,1.0,2]"),
        ("[3,1,2,1,1.0,-1]", "[1,4,2,2]", "[1,1.0,2,3,4,99]"),
        ("[0.0,-0.0,1,1.0,a]", "[1.0,0.0]", "[0.0,-0.0,1,1.0,a,b]"),
        ("[{b,[2,1]},#{k=>1},<<5:3>>,{a,[]},#{k=>1.0},[1,2]]",
         "[#{k=>1},{b,[2,1]}]",
         "[{b,[2,1]},#{k=>1},#{k=>1.0},<<5:3>>,missing]"),
        ("[1 bsl 100,-(1 bsl 100),(1 bsl 255)-1,1 bsl 60,0,-(1 bsl 60),1 bsl 100]",
         "[1 bsl 100,42,-(1 bsl 255)+1]",
         "[1 bsl 100,-(1 bsl 100),(1 bsl 255)-1,42,99]"),
        ("lists:reverse(lists:seq(1,33))", "[0,34]", "[1,17,33,34,99]"),
    ]:
        selected.append({"upstream_example": "examples/otp/lists.exs#sort_contract_holds",
                         "expression": f"differential:list_relations({xs},{ys},[],{probes})"})
    for bad in ["bad", "[1|2]", "[3,2,1|tail]"]:
        selected.append({"upstream_example": "examples/otp/lists.exs#sort_contract_holds",
                         "expression": f"differential:call(lists,sort,[{bad}])"})
    for first, last, step, probes in [
        ("1", "10", "1", "[0,1,4,9,10]"),
        ("-5", "5", "3", "[0,1,2,3,4]"),
        ("10", "1", "-2", "[0,1,2,4,5]"),
        ("5", "-5", "-3", "[0,1,2,3,4]"),
        ("7", "7", "1", "[0,1]"),
        ("7", "7", "-3", "[0,1]"),
        ("7", "7", "0", "[0,1]"),
        ("5", "4", "1", "[0]"),
        ("5", "3", "2", "[0]"),
        ("5", "4", "2", "[0]"),
        ("10", "7", "5", "[0]"),
        ("3", "5", "-2", "[0]"),
        ("4", "5", "-2", "[0]"),
        ("7", "10", "-5", "[0]"),
        ("(1 bsl 100)-3", "(1 bsl 100)+3", "2", "[0,1,2,3,4]"),
        ("-(1 bsl 100)+4", "-(1 bsl 100)-2", "-3", "[0,1,2,3]"),
    ]:
        selected.append({"upstream_example": "examples/otp/lists.exs#sequence_formula",
                         "expression": f"differential:seq_contract({first},{last},{step},{probes})"})
    for args in [
        ["5", "3"], ["a", "5"], ["1", "b"],
        ["5", "2", "1"], ["5", "2", "2"],
        ["2", "5", "-1"], ["2", "5", "-2"],
        ["1", "5", "0"], ["a", "5", "1"],
        ["1", "5", "a"], ["1", "5", "1.0"],
    ]:
        selected.append({"upstream_example": "examples/otp/lists.exs#sequence_formula",
                         "expression": f"differential:call(lists,seq,[{','.join(args)}])"})

    # Upstream stdlib example laws for modules bundled in AtomVM libs/estdlib/src:
    # base64, binary hex, calendar, json, and queue.
    for data in ["<<>>", "<<0>>", "<<255>>", "<<1,2>>", "<<1,2,3>>",
                 "<<62,63,254,255>>", "<<\"hello\">>"]:
        selected.append({
            "upstream_example": "examples/otp/base64.exs#encoding_roundtrip",
            "expression": f"B = {data}, E = base64:encode(B), D = base64:decode(E), "
                          "{E, D, D =:= B, byte_size(E) =:= ((byte_size(B) + 2) div 3) * 4}",
        })
    for data in ["<<>>", "<<0>>", "<<1,2>>", "<<62,63,254,255>>"]:
        selected.append({
            "upstream_example": "examples/otp/base64.exs#encoding_roundtrip",
            "expression": f"B = {data}, S = base64:encode_to_string(B), "
                          "D = base64:decode_to_string(S), {S, D, list_to_binary(D) =:= B}",
        })
    for mode in ["standard", "urlsafe"]:
        for pad in ["true", "false"]:
            for data in ["<<>>", "<<251>>", "<<251,255>>", "<<251,255,191>>"]:
                selected.append({
                    "upstream_example": "examples/otp/base64.exs#encoding_roundtrip",
                    "expression": f"B = {data}, Opts = #{{mode => {mode}, padding => {pad}}}, "
                                  "E = base64:encode(B, Opts), D = base64:decode(E, Opts), {E, D, D =:= B}",
                })
    for expr in [
        "{base64:encode(\"hello\"), base64:decode(\"aGVsbG8=\"), "
        "base64:decode(<<\"SGVsbG8\">>, #{padding => false}), "
        "base64:decode_to_string(base64:encode_to_string(<<251,255>>, #{mode => urlsafe, padding => false}), #{mode => urlsafe, padding => false})}",
        "differential:call(base64,decode,[<<\"!!!!\">>])",
        "differential:call(base64,decode,[<<\"AA\">>,#{padding => false}])",
        "differential:call(base64,decode_to_string,[\"SGVsbG8=\"])",
        # Documented OTP error-reason differences (expected_failures.json:
        # AtomVM raises badarg uniformly per base64.erl @doc):
        "differential:call(base64,decode,[<<\"A\">>])",
        "differential:call(base64,decode,[<<\"AA\">>])",
        "differential:call(base64,decode,[<<\"AA=\">>])",
        "differential:call(base64,decode,[<<\"AA===\">>])",
        "differential:call(base64,decode,[<<\"-===\">>])",
        "differential:call(base64,decode,[<<1:1>>])",
        "differential:call(base64,decode,[1])",
        "differential:call(base64,decode_to_string,[\"!!!!\"])",
        "differential:call(base64,encode,[<<\"a\">>,bad])",
        "differential:call(base64,encode,[[a]])",
        "differential:call(base64,encode,[[<<\"a\">>|tail]])",
        "differential:call(base64,decode,[<<\"-\">>,#{mode => standard}])",
        "differential:call(base64,decode,[<<\"+\">>,#{mode => urlsafe}])",
        "differential:call(base64,decode,[<<\"====\">>])",
        "differential:call(base64,decode,[<<\"A===\">>])",
        # Documented base64.erl @doc behavioral differences vs OTP:
        # nested iolist() acceptance, out-of-range byte lists, whitespace rejection:
        "differential:call(base64,encode,[[<<\"hi\">>,[32,<<\"there\">>]]])",
        "differential:call(base64,decode,[[<<\"SGVs\">>,<<\"bG8=\">>]])",
        "differential:call(base64,encode,[[256]])",
        "differential:call(base64,decode,[<<\"SGVs bG8=\">>])",
    ]:
        selected.append({"upstream_example": "examples/otp/base64.exs#encoding_roundtrip",
                         "expression": expr})

    for data in ["<<>>", "<<0>>", "<<15,16,255>>", "<<\"AtomVM\">>"]:
        selected.append({
            "upstream_example": "examples/otp/binary.exs#hex_roundtrip",
            "expression": f"B = {data}, {{binary:encode_hex(B), "
                          "binary:decode_hex(binary:encode_hex(B)) =:= B, "
                          "binary:encode_hex(B, uppercase), "
                          "binary:decode_hex(binary:encode_hex(B, uppercase)) =:= B, "
                          "binary:encode_hex(B, lowercase), "
                          "binary:decode_hex(binary:encode_hex(B, lowercase)) =:= B}",
        })
    for expr in [
        "differential:call(binary,decode_hex,[<<>>])",
        "differential:call(binary,decode_hex,[<<\"00ff\">>])",
        "differential:call(binary,decode_hex,[<<\"00FF\">>])",
        "differential:call(binary,decode_hex,[<<\"0aA0fF\">>])",
        "differential:call(binary,decode_hex,[<<\"0\">>])",
        "differential:call(binary,decode_hex,[<<\"zz\">>])",
        "differential:call(binary,decode_hex,[<<\"0g\">>])",
        "differential:call(binary,decode_hex,[<<1:1>>])",
        "differential:call(binary,decode_hex,[bad])",
        "differential:call(binary,encode_hex,[<<1:1>>])",
        "differential:call(binary,encode_hex,[bad])",
        "differential:call(binary,encode_hex,[<<1>>,mixed])",
    ]:
        selected.append({"upstream_example": "examples/otp/binary.exs#hex_roundtrip",
                         "expression": expr})

    for year in ["0", "1", "399", "400", "1970", "2000", "2024", "2400"]:
        selected.append({
            "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
            "expression": f"Y = {year}, D1 = calendar:date_to_gregorian_days({{Y,1,1}}), "
                          "D2 = calendar:date_to_gregorian_days({Y+400,1,1}), "
                          "{D1, D2, D2 =:= D1 + 146097}",
        })
    for year, month, day in [(0, 1, 1), (1970, 1, 1), (2000, 2, 29), (2024, 2, 29), (2024, 12, 31)]:
        selected.append({
            "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
            "expression": f"{{calendar:date_to_gregorian_days({year},{month},{day}), "
                          f"calendar:day_of_the_week({{{year},{month},{day}}}), "
                          f"calendar:day_of_the_week({year},{month},{day}), "
                          f"calendar:datetime_to_gregorian_seconds({{{{{year},{month},{day}}},{{23,59,59}}}})}}",
        })
    for sec in ["0", "86400", "1700000000", "-86400"]:
        selected.append({
            "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
            "expression": f"differential:call(calendar,system_time_to_universal_time,[{sec},second])",
        })
    selected.append({
        "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
        "expression": "differential:call(calendar,datetime_to_gregorian_seconds,[{{2024,1,1},{24,0,0}}])",
    })
    # Documented OTP range-guard differences (expected_failures.json: AtomVM
    # calendar.erl enforces Year >= 0, M in 1..12, D in 1..31 per its -spec):
    for bad_date in ["{2024,13,1}", "{-1,1,1}", "{2024,0,1}", "{2024,1,32}"]:
        selected.append({
            "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
            "expression": f"differential:call(calendar,date_to_gregorian_days,[{bad_date}])",
        })
    selected.append({
        "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
        "expression": "differential:call(calendar,day_of_the_week,[{-1,1,1}])",
    })
    selected.append({
        "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
        "expression": "differential:call(calendar,datetime_to_gregorian_seconds,[{{-1,1,1},{0,0,0}}])",
    })
    # Documented negative sub-second truncation bug in calendar:system_time_to_universal_time/2:
    for unit in ["millisecond", "microsecond", "nanosecond", "native"]:
        selected.append({
            "upstream_example": "examples/otp/calendar.exs#date_to_gregorian_days_cycle",
            "expression": f"differential:call(calendar,system_time_to_universal_time,[-1,{unit}])",
        })
    # Unimplemented functions from the remaining laws in examples/otp/calendar.exs:
    for law_name, expr in [
        ("seconds_to_daystime_roundtrips", "differential:call(calendar,seconds_to_daystime,[86400])"),
        ("seconds_to_daystime_roundtrips", "differential:call(calendar,time_to_seconds,[{1,2,3}])"),
        ("seconds_to_time_roundtrips", "differential:call(calendar,seconds_to_time,[3723])"),
        ("is_leap_year_cycle", "differential:call(calendar,is_leap_year,[2024])"),
        ("is_leap_year_cycle", "differential:call(calendar,is_leap_year,[2424])"),
        ("gregorian_days_roundtrip", "differential:call(calendar,gregorian_days_to_date,[739251])"),
        ("gregorian_days_roundtrip", "differential:call(calendar,valid_date,[{2024,2,29}])"),
        ("iso_week_number_valid_date", "differential:call(calendar,iso_week_number,[{2024,1,1}])"),
    ]:
        selected.append({
            "upstream_example": f"examples/otp/calendar.exs#{law_name}",
            "expression": expr,
        })

    # Unimplemented stdlib modules from examples/otp/{array,gb_trees,rand}.exs
    # (expected_failures.json):
    for expr in [
        "A = array:new(), W = array:set(2, hello, A), "
        "{array:get(2, W), array:size(W), array:set(2, world, W) =:= array:set(2, world, A)}",
        "A = array:from_list([10,20,30]), "
        "{array:is_array(A), array:size(A), array:get(1, A), array:to_list(A)}",
        "differential:call(array,get,[0,array:new()])",
    ]:
        selected.append({
            "upstream_example": "examples/otp/array.exs#set_obeys_update_contract",
            "expression": expr,
        })
    for expr in [
        "T = gb_trees:from_orddict([{1,a},{2,b},{3,c}]), B = gb_trees:balance(T), "
        "{gb_trees:to_list(B) =:= gb_trees:to_list(T), "
        "gb_trees:lookup(2, B) =:= gb_trees:lookup(2, T), "
        "gb_trees:size(B) =:= gb_trees:size(T)}",
        "T = gb_trees:insert(b, 2, gb_trees:insert(a, 1, gb_trees:empty())), "
        "{gb_trees:size(T), gb_trees:to_list(T), gb_trees:lookup(a, T), gb_trees:lookup(z, T)}",
        "differential:call(gb_trees,get,[missing,gb_trees:empty()])",
    ]:
        selected.append({
            "upstream_example": "examples/otp/gb_trees.exs#balance_preserves_observations",
            "expression": expr,
        })
    for law_name, expr in [
        ("shuffle_permutes",
         "S0 = rand:seed_s(exsss, {1,2,3}), {L, S1} = rand:shuffle_s([1,2,3,4], S0), "
         "{lists:sort(L), element(1, S1) =:= element(1, S0)}"),
        ("shuffle_permutes",
         "S0 = rand:seed_s(exsss, {1,2,3}), {H, _} = S0, {L, _} = rand:shuffle_s([3,1,4,1,5], S0), "
         "{maps:get(type, H), length(L), lists:sort(L)}"),
        ("shuffle_short_consumes_nothing",
         "S0 = rand:seed_s(exsss, {1,2,3}), "
         "{rand:shuffle_s([], S0) =:= {[], S0}, rand:shuffle_s([only], S0) =:= {[only], S0}}"),
    ]:
        selected.append({
            "upstream_example": f"examples/otp/rand.exs#{law_name}",
            "expression": expr,
        })

    for term in [
        "null", "true", "false", "0", "-7", "42", "1 bsl 100", "-(1 bsl 100)",
        "0.0", "3.14", "-12.5", "1.0e20",
        "<<>>", "<<\"hello\">>", "<<\"a\\nb\\tc\\rd\\\\e\\\"f\">>",
        "<<\"\\x{00}\\x{1f}\\x{7f}\">>",
        "<<\"cafe\\x{0301} 😀 日本語\"/utf8>>",
        "<<\"0123456789abcdef0123456789abcdef\">>",
        "[]", "[1,true,null,<<\"x\">>,#{}]",
        "#{<<\"a\">> => 1, <<\"nested\">> => #{<<\"b\">> => [false, <<>>]}}",
    ]:
        selected.append({
            "upstream_example": "examples/otp/json.exs#json_roundtrip",
            "expression": f"V = {term}, E = iolist_to_binary(json:encode(V)), "
                          "D = json:decode(E), {E, D, D =:= V}",
        })
    selected.append({
        "upstream_example": "examples/otp/json.exs#json_roundtrip",
        "expression": "V = lists:foldl(fun(I,Acc)->[I,Acc] end,null,lists:seq(1,32)), "
                      "E = iolist_to_binary(json:encode(V)), {json:decode(E) =:= V}",
    })
    for raw in [
        "<<\" -0.0 \">>",
        "<<\"\\t\\n\\r [ 1 , -2.5 , true ]\\n\">>",
        r'<<"\"\\\\\\/\\b\\f\\n\\r\\t\"">>',
        r'<<"\"\\u0041\\uD83D\\uDE00\"">>',
        "<<>>", "<<\" \">>", "<<\"{\">>", "<<\"[1,\">>", "<<\"[1,]\">>",
        "<<\"\\\"unterminated\">>",
        r'<<"\"\\uD800\"">>',
        r'<<"\"\\uD800\\uD800\"">>',
        r'<<"\"\\uDC00\"">>',
        r'<<"\"\\uZZZZ\"">>',
        "<<\"01\">>", "<<\"truee\">>", "<<\"1 2\">>", "<<\"1e1000\">>", "<<\"[1 2]\">>",
    ]:
        selected.append({
            "upstream_example": "examples/otp/json.exs#json_roundtrip",
            "expression": f"differential:call(json,decode,[{raw}])",
        })
    selected.append({
        "upstream_example": "examples/otp/json.exs#json_roundtrip",
        "expression": "{continue, S1} = json:decode_start(<<\"{\\\"k\\\":\">>, ok, #{}), "
                      "{continue, S2} = json:decode_continue(<<\"[1,2\">>, S1), "
                      "json:decode_continue(<<\"]}\">>, S2)",
    })

    for q_expr in [
        "queue:new()",
        "queue:from_list([1])",
        "queue:from_list([1,2,3,4,5])",
        "{[3,2,1],[]}",
        "{[],[1,2,3]}",
        "{[3,2],[1]}",
    ]:
        selected.append({
            "upstream_example": "examples/otp/queue.exs#reverse_involution",
            "expression": f"Q = {q_expr}, R1 = queue:reverse(Q), R2 = queue:reverse(R1), "
                          "{queue:is_queue(Q), queue:to_list(R1), queue:to_list(R2) =:= queue:to_list(Q)}",
        })
        selected.append({
            "upstream_example": "examples/otp/queue.exs#reverse_exchanges_ends",
            "expression": f"Q = {q_expr}, R = queue:reverse(Q), "
                          "{queue:peek(Q), queue:peek_r(Q), "
                          "queue:peek(R) =:= queue:peek_r(Q), queue:peek_r(R) =:= queue:peek(Q), "
                          "lists:reverse(queue:to_list(Q)) =:= queue:to_list(R)}",
        })
        selected.append({
            "upstream_example": "examples/otp/queue.exs#front_insert_peek",
            "expression": f"Q = {q_expr}, Q2 = queue:in_r(tag,Q), P = queue:peek(Q2), "
                          "{{value,tag}, Q3} = queue:out(Q2), "
                          "{P =:= {value,tag}, queue:to_list(Q3) =:= queue:to_list(Q)}",
        })
        for split_n in (["0"] if q_expr == "queue:new()" else ["0", "1", "queue:len(Q)"]):
            selected.append({
                "upstream_example": "examples/otp/queue.exs#split_preserves_ordered_partition",
                "expression": f"Q = {q_expr}, N = {split_n}, {{Q1,Q2}} = queue:split(N,Q), "
                              "{queue:to_list(Q1), queue:to_list(Q2), queue:len(Q1) =:= N, "
                              "queue:to_list(Q1) ++ queue:to_list(Q2) =:= queue:to_list(Q), "
                              "queue:to_list(queue:join(Q1,Q2)) =:= queue:to_list(Q)}",
            })
    selected.append({
        "upstream_example": "examples/otp/queue.exs#split_preserves_ordered_partition",
        "expression": "Q = queue:from_list([1,2,3,4,5]), {Q1,Q2} = queue:split(2,Q), "
                      "{queue:to_list(Q1), queue:to_list(Q2), queue:member(3,Q), queue:member(9,Q), "
                      "queue:to_list(queue:filter(fun(X)->X rem 2 =:= 0 end,Q)), "
                      "queue:fold(fun(X,A)->X+A end,0,Q), "
                      "queue:any(fun(X)->X>4 end,Q), queue:all(fun(X)->X>0 end,Q), "
                      "queue:to_list(queue:delete(3,Q)), queue:to_list(queue:delete_r(3,Q))}",
    })
    for bad in [
        "differential:call(queue,out,[queue:new()])",
        "differential:call(queue,out_r,[queue:new()])",
        "differential:call(queue,get,[queue:new()])",
        "differential:call(queue,get_r,[queue:new()])",
        "differential:call(queue,drop,[queue:new()])",
        "differential:call(queue,drop_r,[queue:new()])",
        "differential:call(queue,split,[2,queue:from_list([1])])",
        "differential:call(queue,split,[-1,queue:from_list([1])])",
        "differential:call(queue,len,[bad])",
    ]:
        selected.append({"upstream_example": "examples/otp/queue.exs#split_preserves_ordered_partition",
                         "expression": bad})

    t = "Modules/Timers"
    for payload in ["tick", "{nested,#{1=>one,1.0=>float},<<5:3>>}"]:
        for mode, label, delay in [
            ("deliver", "timer_delivers", 0), ("deliver", "timer_delivers", 1),
            ("cancel", "cancel_twice", 60000), ("cancel", "cancel_stops_or_message_arrives", 0),
            ("expired", "cancel_twice", 0),
            ("dead_before", "cancel_after_destination_exits", 60000),
            ("dead_after", "cancel_after_destination_exits_later", 60000),
            ("time_left", "cancel_reports_time_left", 5000),
            ("time_left", "cancel_reports_time_left", 60000),
            ("no_time_left", "cancel_reports_no_time_left", 0),
            ("false_before_delivery", "cancel_false_before_delivery", 0),
            ("false_before_delivery_waited", "cancel_false_before_delivery", 1),
            ("false_before_delivery_target", "cancel_false_before_delivery", 1),
        ]:
            probe(t, label, f"differential:timer({mode},{delay},{payload})")
    call(t, "cancel_other_reference", "cancel_timer", "make_ref()")
    for value in ["timer", "0", "[]"]:
        call(t, "cancel_badarg", "cancel_timer", value)
    for time, destination in [("-1", "self()"), ("1.0", "self()"),
                              ("bad", "self()"), ("1 bsl 63", "self()"), ("10", "make_ref()")]:
        # Observe acceptance without comparing VM-specific references. Clean up
        # a timer if the target incorrectly accepts the invalid arguments.
        probe(t, "send_after_negative_time" if time == "-1" else "send_after_long_times",
              f"R=erlang:send_after({time},{destination},tick),"
              "{is_reference(R),erlang:cancel_timer(R)=/=false}")
    for mode, delay, options in [("deliver", 10, "[{abs,true},{abs,false}]"),
                                 ("deliver", 10, "[]"),
                                 ("cancel", 60000, "[{abs,true},{abs,false}]")]:
        probe(t, "send_after_options", f"differential:timer_options({mode},{delay},{options})")
    for opts in ["[abs]", "[{info,true}]", "[{abs,false},abs]", "[{abs,true},{info,true}]"]:
        probe(t, "send_after_options",
              f"R=erlang:send_after(10,self(),tick,{opts}),"
              "{is_reference(R),erlang:cancel_timer(R)=/=false}")
    # OTP extensions, not instances of Lynx's symbolic timer/options proofs.
    # Absolute inputs are offsets from each VM's own monotonic clock. Opposite
    # duplicate orders distinguish last-option-wins from first-option-wins.
    for options in ["[]", "[{abs,false}]", "[{abs,true},{abs,false}]",
                    "[{abs,false},{abs,false}]"]:
        for mode, delay in [("deliver", 0), ("deliver", 20), ("cancel", 60000)]:
            selected.append({"otp_extension": "send_after/4",
                             "expression": f"differential:timer_options({mode},{delay},{options})"})
    for options in ["[{abs,true}]", "[{abs,false},{abs,true}]", "[{abs,true},{abs,true}]"]:
        for mode, delay in [("abs_deliver", -20), ("abs_deliver", -1),
                            ("abs_deliver", 0), ("abs_deliver", 20),
                            ("abs_cancel", -20), ("abs_cancel", 0),
                            ("abs_cancel", 60000)]:
            selected.append({"otp_extension": "send_after/4",
                             "expression": f"differential:timer_options({mode},{delay},{options})"})
    for time, destination, options in [
        *(("60000", "self()", opts) for opts in [
            "bad", "[abs]", "[{info,true}]", "[{abs,0}]", "[{abs,bad}]",
            "[{abs}]", "[{abs,true,extra}]", "[{abs,false}|tail]",
            "[{abs,bad},{abs,false}]", "[{abs,false},{abs,bad}]",
            "[{info,true},{abs,false}]", "[{abs,false},{info,true}]",
        ]),
        ("-1", "self()", "[]"), ("-1", "self()", "[{abs,false}]"),
        ("1.0", "self()", "[]"), ("1.0", "self()", "[{abs,true}]"),
        ("bad", "self()", "[{abs,true}]"),
        ("60000", "make_ref()", "[]"), ("60000", "42", "[{abs,true}]"),
    ]:
        # An incorrectly accepted timer is canceled before returning a mismatch.
        selected.append({"otp_extension": "send_after/4",
                         "expression": f"R=erlang:send_after({time},{destination},tick,{options}),"
                                       "{is_reference(R),erlang:cancel_timer(R)=/=false}"})

    # Source forms are paired with dynamic apply and guard observations. Inspect
    # compiled instructions before claiming distinct paths on a new OTP release.
    for op in ["+", "-", "*", "/"]:
        for a, b in [("0", "0"), ("0", "7"), ("-7", "3"), ("3", "-7"),
                     ("-7", "-3"), ("1", "1.0"),
                     ("9007199254740993", "1.0"), ("1.0", "9007199254740993"),
                     ("-(1 bsl 63)", "-1"), ("(1 bsl 127)-1", "1"),
                     ("(1 bsl 255)-1", "0"), ("-0.0", "0.0"),
                     ("0.0", "-0.0"), ("-0.0", "-0.0"), ("5.0e-324", "0.5"),
                     ("1.7976931348623157e308", "2.0"), ("bad", "1"), ("1", "[]")]:
            probe(e, "arithmetic_boundaries", f"differential:execution_paths('{op}',{a},{b})")
    for op in ["div", "rem", "band", "bor", "bxor", "bsl", "bsr"]:
        for a, b in [("0", "0"), ("0", "7"), ("-7", "3"), ("3", "-7"),
                     ("-7", "-3"), ("1.0", "2"), ("bad", "1"), ("1", "[]"),
                     ("(1 bsl 100)+3", "63"), ("-(1 bsl 63)", "-1"),
                     ("(1 bsl 255)-1", "0"), ("(1 bsl 127)+1", "127")]:
            probe(e, "arithmetic_boundaries", f"differential:execution_paths('{op}',{a},{b})")
    for op in ["==", "/=", "=:=", "=/=", "<", "=<", ">", ">="]:
        for a, b in [("1", "1.0"), ("0.0", "-0.0"),
                     ("9007199254740993", "9007199254740992.0"),
                     ("-(1 bsl 63)-1", "-9223372036854775808.0"),
                     ("(1 bsl 255)-1", "1.0e77"), ("5.0e-324", "0"),
                     ("{1}", "{1.0}"), ("{z}", "{a,b}"),
                     ("#{1=>wanted}", "#{1.0=>wanted}"),
                     ("#{a=>1}", "#{a=>1.0}"), ("[1|a]", "[1|b]"),
                     ("[1]", "[1.0]"), ("<<5:3>>", "<<5:4>>"),
                     ("<<128>>", "<<1:1>>"), ("[]", "<<>>"),
                     ("a", "1"), ("{}", "{}"), ("#{a=>1,b=>2}", "#{a=>1,b=>3}")]:
            for left, right in ([(a, b)] if a == b else [(a, b), (b, a)]):
                probe("Term/Compare", "numeric_and_exact_comparison",
                      f"differential:execution_paths('{op}',{left},{right})")
    for key, value in [
        ("key", "#{key=>wanted}"), ("key", "#{key=>other}"), ("missing", "#{key=>wanted}"),
        ("1", "#{1=>wanted,1.0=>other}"), ("1.0", "#{1=>wanted,1.0=>other}"),
        ("1", "#{1.0=>wanted}"), ("1.0", "#{1=>wanted}"),
        ("0.0", "#{-0.0=>wanted}"), ("-0.0", "#{0.0=>wanted}"),
        ("{key,1}", "#{{key,1}=>wanted}"), ("{key,1}", "#{{key,1.0}=>wanted}"),
        ("#{v=>1}", "#{#{v=>1}=>wanted}"), ("#{v=>1}", "#{#{v=>1.0}=>wanted}"),
        ("<<5:3>>", "#{<<5:3>>=>wanted}"), ("<<5:4>>", "#{<<5:3>>=>wanted}"),
        ("key", "#{key=>-0.0}"), ("key", "bad"), ("key", "[]"),
    ]:
        probe("Modules/Maps", "update_lookup", f"differential:execution_paths(map_get,{key},{value})")
    for size in [0, 31, 32, 33]:
        value = f"maps:from_list([{{I,I}} || I <- lists:seq(1,{size})])"
        for key in ["1", "1.0", "missing"]:
            probe("Modules/Maps", "update_lookup", f"differential:execution_paths(map_get,{key},{value})")
    # Reuse body/dynamic/guard observations, keeping inputs and first results
    # live across a call and explicit GC. A successful unequal guard must reject
    # just like an invalid BIF, but body/dynamic results distinguish the two.
    structural_inputs = {
        "length": ("3", [
            ("empty", "[]"), ("three", "[wanted,{nested,7},<<5:3>>]"),
            ("two", "[a,b]"), ("improper", "[wanted|tail]"),
            ("nested_improper", "[a,b|tail]"), ("non_list", "{a,b}"),
            ("live", "[self(),make_ref(),fun()->alive end]"),
            ("runtime_list", "lists:seq(1,257)"),
        ]),
        "hd": ("wanted", [
            ("empty", "[]"), ("proper", "[wanted,{nested,7}]"),
            ("improper", "[wanted|tail]"), ("other", "[other]"),
            ("non_list", "{wanted}"),
            ("heap_head", "lists:reverse([tail,{nested,<<5:3>>}])"),
        ]),
        "tl": ("[wanted]", [
            ("empty", "[]"), ("proper", "[head,wanted]"),
            ("improper", "[head|wanted]"), ("singleton", "[head]"),
            ("non_list", "42"),
            ("heap_tail", "lists:reverse(differential:exception_argument([{nested,<<5:3>>},head]),tail)"),
        ]),
        "tuple_size": ("3", [
            ("empty", "{}"), ("three", "{wanted,<<5:3>>,[a|tail]}"),
            ("two", "{a,b}"), ("non_tuple", "[a,b,c]"),
            ("live", "list_to_tuple([self(),make_ref(),fun()->alive end])"),
        ]),
    }
    binary_inputs = [(f"bits_{bits}", f"<<5:{bits}>>") for bits in [0, 1, 7, 8, 9, 23, 24, 25]] + [
        ("heap_binary", "binary:copy(<<7>>,31)"),
        ("refc_binary", "binary:copy(<<7>>,257)"),
        ("aligned_slice", "binary:part(binary:copy(<<17>>,129),1,65)"),
        ("unaligned_slice", "begin Parent=differential:exception_argument("
         "<<7:3,(binary:copy(<<17>>,129))/binary,3:5>>),"
         "<<_:3,Slice:65/binary,_/bitstring>>=Parent,Slice end"),
        ("partial_slice", "begin Parent=differential:exception_argument("
         "binary:copy(<<17>>,129)),<<_:5,Slice:515/bitstring,_/bitstring>>=Parent,Slice end"),
        ("non_bitstring_atom", "bad"), ("non_bitstring_list", "[1,2,3]"),
    ]
    structural_inputs.update(byte_size=("3", binary_inputs), bit_size=("24", binary_inputs))
    for op, (target, inputs) in structural_inputs.items():
        origin = {"length": "length/1_two_elements", "hd": "hd/1_cons", "tl": "tl/1_cons",
                  "tuple_size": "tuple_access"}.get(op, "bitstring_guards")
        for label, value in inputs:
            path = f"differential:execution_paths({op},V,{target})"
            probe(e, origin, f"V=differential:exception_argument({value}),Before={path},"
                  f"true=erlang:garbage_collect(),After={path},{{Before,After}}",
                  structural_bif=op, structural_case=label)
    tuple_value = "{wanted,other,{nested,<<5:3>>}}"
    element_inputs = [(f"index_{index}", index, tuple_value)
                      for index in ["-1", "0", "1", "2", "3", "4", "1.0", "bad", "1 bsl 100"]] + [
        ("empty", "1", "{}"), ("non_tuple", "1", "[wanted,other]"),
        ("invalid_both", "0", "bad"),
    ] + [(f"runtime_{index}", str(index), "setelement(33,erlang:make_tuple(33,other),wanted)")
         for index in [32, 33, 34]]
    for label, index, value in element_inputs:
        path = "differential:execution_paths(element,I,V)"
        probe(e, "tuple_access", f"I=differential:exception_argument({index}),"
              f"V=differential:exception_argument({value}),Before={path},"
              f"true=erlang:garbage_collect(),After={path},{{Before,After}}",
              structural_bif="element", structural_case=label)
    for a in ["true", "false", "0"]:
        for b in ["true", "false", "bad"]:
            call(e, "xor/2_booleans", "xor", a, b)
    for size in ["0", "1", "3", "-1", "1.0", "bad"]:
        call(e, "make_tuple_replicates" if size in ["0", "1", "3"] else "make_tuple_bad_size",
             "make_tuple", size, "{nested,7}")
    for value in ["{}", "{a,7,{nested}}", "[]", "bad"]:
        call(e, ["tuple_access", "tuple_access_rejects_nontuples"], "tuple_size", value)
        for index in ["-1", "0", "1", "3", "4", "1.0"]:
            call(e, ["tuple_access", "tuple_access_invalid_indices"], "element", index, value)
            call(e, ["setelement_read", "setelement_bad_index"], "setelement", index, value, "new")
    for value in ["[]", "[a]", "[a,7]", "[a|tail]", "bad"]:
        for fn in ["hd", "tl", "length"]:
            call(e, ["hd/1_cons", "tl/1_cons", "length/1_two_elements", "list_guards_invalid"], fn, value)
    for width in range(18):
        for fn in ["is_binary", "is_bitstring", "bit_size", "byte_size", "size"]:
            call(e, ["bitstring_guards", "binary_guard_alignment", "all_partial_byte_sizes",
                     "empty_bitstring_has_zero_lastBits", "size/1_complete_bytes"], fn, f"<<255:{width}>>")
    for value in ["bad", "[]", "{}", "1"]:
        for fn in ["is_binary", "is_bitstring", "bit_size", "byte_size"]:
            call(e, "non_bitstrings_rejected", fn, value)
    for start, length in [(0, 3), (1, 2), (3, -2), (3, 0), (2, 2), (1, -2), (-1, 1)]:
        call(e, "binary_part/3_ranges", "binary_part", "<<1,2,3>>", str(start), str(length))
        call(e, "binary_part/3_ranges", "binary_part", "<<1,2,3>>", f"{{{start},{length}}}")
    call(e, "binary_part/3_ranges", "binary_part", "<<1,2,0:1>>", "0", "0")
    for a, b in [("a", "b"), ("1", "1.0"), ("0.0", "-0.0"),
                 ("#{key=>1}", "#{key=>1.0}")]:
        probe(e, ["process_dictionary_empty", "process_dictionary_put_replace_erase",
                  "process_dictionary_numeric_types_are_distinct", "process_dictionary_queries"],
              f"differential:dictionary({a},{b})")

    m = "Modules/Math"
    for value in ["0.0", "-0.0", "0.25", "-0.25", "0.9999999999999999", "-0.9999999999999999",
                  "5.0e-324", "-5.0e-324", "4503599627370496.0", "1.7976931348623157e308",
                  "7", "bad"]:
        for fn in ["floor", "ceil"]:
            call(m, ["floor_signed_zero", "ceil_signed_zero", "floor_unit_interval",
                     "ceil_negative_unit_interval", "large_values_are_fixed_and_integral"], fn, value, module="math")

    c = "Modules/Conversions"
    integers = ["+12", "-0", "007", "", "-", "1_000", " 1", "1 ", "1.0",
                "123456789012345678901234567890", "--1", "+-1", "0x10"]
    floats = ["1.0", "1e5", "1.e5", ".5", "1.0E+5", "1.0e400", "-0.0", "+1.5",
              "1.0e-400", "1.5e", "1.5e+", "1", "0.1e1_0", "1.7976931348623157e308",
              "1.7976931348623159e308", "2.4703282292062327e-324", "2.4703282292062328e-324", "00.10"]
    for ordinal, fn, texts in [(1, "binary_to_integer", integers), (2, "binary_to_float", floats)]:
        for text in texts:
            call(c, f"guard:{ordinal}", fn, f'<<"{text}">>')
    for base, texts in [("2", ["101101", "-101101", "+000101", "102"]),
                        ("10", ["+00012", "-0", "123456789012345678901234567890", "1_0"]),
                        ("16", ["fF", "-8000000000000001", "10000000000000000000000001", "0x10"]),
                        ("36", ["Zz", "+000Z", "-zZ", "zZ!"])]:
        for text in texts:
            call(c, "guard:1", "binary_to_integer", f'<<"{text}">>', base)
            call(c, "guard:1", "list_to_integer", f'"{text}"', base)
    for bytes_ in ["49,0", "49,0,50", "0,49", "43,49,0,50"]:
        for base in [None, "2", "10", "16", "36"]:
            args = [] if base is None else [base]
            call(c, "guard:1", "binary_to_integer", f"<<{bytes_}>>", *args)
            call(c, "guard:1", "list_to_integer", f"[{bytes_}]", *args)
    for base in ["1", "37", "-1", "1.0", "1 bsl 100"]:
        for fn, value in [("binary_to_integer", '<<"10">>'), ("list_to_integer", '"10"'),
                          ("integer_to_binary", "10"), ("integer_to_list", "10")]:
            call(c, "integer_binary_round_trip", fn, value, base)
    for base in ["2", "16", "36"]:
        for value in ["0", "-1", "(1 bsl 63)+1", "-((1 bsl 127)+35)"]:
            for fn in ["integer_to_binary", "integer_to_list"]:
                call(c, "integer_binary_round_trip", fn, value, base)
    for value in ["0.0", "-0.0", "5.0e-324", "1.7976931348623157e308", "100.0", "1000.0",
                  "1230.0", "12300.0", "0.0012", "0.00012", "0.1", "0.3", "123456.0",
                  "12345.678", "1.0e23", "9.999999999999999e22", "9007199254740991.0",
                  "9007199254740992.0", "-1230.0", "2.5e-7"]:
        probe(c, ["guard:3", "float_binary_round_trip"], f"differential:float_text({value},[short])")
    for ordinal, atom, encoding in [(4, "'héllo'", "utf8"), (5, "'café'", "latin1"), (6, "'π'", "latin1")]:
        call(c, f"guard:{ordinal}", "atom_to_binary", atom, encoding)
    for binary_expr in [
        "<<>>", "<<104,105>>", "<<192,128>>", "<<194,128>>", "<<224,159,128>>",
        "<<224,160,128>>", "<<237,160,128>>", "<<237,159,191>>",
        "<<240,143,128,128>>", "<<240,144,128,128>>", "<<244,143,191,191>>",
        "<<244,144,128,128>>", "<<245,128,128,128>>", "<<128>>", "<<195>>",
        "binary:copy(<<97>>,256)", "<<(binary:copy(<<97>>,255))/binary,255>>",
        "<<(binary:copy(<<97>>,254))/binary,255>>", "binary:copy(<<97>>,1021)",
        "binary:copy(<<195,169>>,255)", "binary:copy(<<240,159,152,128>>,255)",
        "binary:copy(<<195,169>>,300)",
    ]:
        for encoding in ["utf8", "unicode", "latin1"]:
            call(c, "guard:7", "binary_to_atom", binary_expr, encoding)
    for encoding in ["utf8", "unicode", "latin1"]:
        call(c, "guard:8", "binary_to_atom", "binary:copy(<<97>>,255)", encoding)
    for binary_expr in [
        "<<>>", "<<104,105>>", "<<194,128>>", "<<192,128>>", "<<255>>",
        "binary:copy(<<97>>,255)", "binary:copy(<<97>>,256)",
        "<<(binary:copy(<<97>>,255))/binary,255>>",
        "<<(binary:copy(<<97>>,254))/binary,255>>", "a", "<<128:7>>",
    ]:
        call(c, "guard:9", "binary_to_atom", binary_expr)
    for binary_expr in ["<<104>>", "<<>>", "a"]:
        call(c, "guard:10", "binary_to_atom", binary_expr, "ascii")
    for non_binary in ["a", "[104]", "1"]:
        for encoding in ["utf8", "latin1"]:
            call(c, "guard:11", "binary_to_atom", non_binary, encoding)
    for encoding in ["utf8", "unicode", "latin1"]:
        call(c, "guard:12", "binary_to_atom", "<<128:7>>", encoding)
    for value in ["{a,1}", "{}", "{1,1.0,<<5:3>>,[a|b],#{k=>1}}", "[]", "a", "1", "<<1,2>>", "#{a=>1}"]:
        call(c, "tuple_to_list_elements", "tuple_to_list", value)
    for value in ["0", "-7", "1 bsl 100", "-(1 bsl 255)"]:
        call(c, "integer_binary_round_trip", "integer_to_binary", value)
        probe(c, "integer_binary_round_trip",
              f"differential:call(erlang,binary_to_integer,[differential:call(erlang,integer_to_binary,[{value}])])")
    for value in ["[]", "<<>>", "[0,255,[1,<<2,3>>]|<<4,5>>]", "[[[]],<<7>>]",
                  "[1|2]", "[-1]", "[256]", "[<<1:1>>]", "<<1:1>>", "bad",
                  "0", "[<<>>,[],[[]]]", "[[1|<<2,3>>],[4,[]]|<<5,6>>]",
                  "[<<5:3>>,17,<<3:2>>]", "[[1|<<2,5:3>>],7]",
                  "[<<5:3>>,<<17:5>>]", "[1|<<5:3>>]", "[[[]]|1]",
                  "[1,[2,bad],3]", "[1,[2.0],3]", "[1,[1 bsl 100],3]",
                  "lists:foldl(fun(_,A)->[A,23] end,[17|<<19>>],lists:seq(1,64))",
                  "lists:foldl(fun(_,A)->[A,23] end,[17|19],lists:seq(1,64))",
                  "begin B=binary:copy(<<0,17,255>>,40),S=binary:part(B,1,65),[S,[S]|<<7>>] end"]:
        # The other APIs extend the model's iodata grammar samples: list-only
        # roots differ from iodata roots, and only list_to_bitstring accepts bits.
        for fn in ["iolist_to_binary", "iolist_size", "list_to_binary", "list_to_bitstring"]:
            call(c, "iodata_total_and_exact", fn, value)
    call(c, "binary_part_of_whole", "binary_part", "<<1,2>>", "0", "2")

    b = "Modules/Binary"
    for subject, pattern, options in [
        ('<<"a&b&c">>', '<<"&">>', "[]"), ('<<"a&b&c">>', '<<"&">>', "[global]"),
        ('<<"&a&&b&">>', '<<"&">>', "[global]"), ('<<"&a&&b&">>', '<<"&">>', "[global,trim]"),
        ('<<"&a&&b&">>', '<<"&">>', "[global,trim_all]"), ('<<"abc">>', '<<"&">>', "[global]"),
        ('<<>>', '<<"&">>', "[global]"), ('<<>>', '<<"&">>', "[global,trim]"),
        ('<<"a,b,c">>', '<<",">>', "[{scope,{2,3}}]"),
        ('<<"a,b,c">>', '<<",">>', "[{scope,{2,3}},global]"),
        ('<<"a,b,c">>', '<<",">>', "[{scope,{4,-3}},global]"),
        ('<<"abcab">>', '[<<"a">>,<<"ab">>]', "[global]"),
        ('<<"abc">>', '<<>>', "[]"), ('<<"abc">>', '[]', "[]"),
        ('<<"abc">>', '<<"b">>', "[{scope,{2,5}}]"), ('<<"abc">>', '<<"b">>', "[bad]"),
    ]:
        call(b, "guard:1", "split", subject, pattern, options, module="binary")
    call(b, "guard:2", "split", '<<"k=v=w">>', '<<"=">>', module="binary")
    for subject, pattern, options in [
        ('<<"a[b]">>', '<<"[">>', "[]"), ('<<"ab">>', '<<"[">>', "[]"),
        ('<<"abcab">>', '[<<"a">>,<<"ab">>]', "[]"),
        ('<<"abcab">>', '<<"ab">>', "[{scope,{1,4}}]"),
        ('<<"abcab">>', '<<"ab">>', "[{scope,{1,3}}]"),
    ]:
        call(b, "guard:5", "match", subject, pattern, options, module="binary")
    # Earlier matches beat longer later ones; at the same position the longest
    # fitting pattern wins, independent of pattern-list order and scope sign.
    for pattern in ['[<<"ab">>,<<"abc">>]', '[<<"abc">>,<<"ab">>]']:
        for options in ["[]", "[{scope,{3,3}}]", "[{scope,{6,-3}}]",
                        "[{scope,{3,2}}]", "[{scope,{3,0}}]", "[{scope,{8,0}}]"]:
            call(b, "guard:5", "match", '<<"zababcab">>', pattern, options, module="binary")
            call(b, "guard:1", "split", '<<"zababcab">>', pattern, options, module="binary")
            call(b, "guard:1", "split", '<<"zababcab">>', pattern, f"[global|{options}]", module="binary")
    for options in ["[{scope,{0,3}},{scope,{3,3}}]", "[{scope,{3,3}},{scope,{0,3}}]",
                    "[{scope,invalid},{scope,{3,3}}]", "[{scope,{0,99}},{scope,{3,3}}]",
                    "[{scope,{3,3}},{scope,invalid}]", "[{scope,{3,3}}|ignored]",
                    "[{scope,{3,3}},bad]", "[{scope,{3,3,0}}]", "[{scope,{3,1.0}}]",
                    "bad", "[bad]", "[{scope,{1 bsl 100,0}}]"]:
        for fn, label in [("match", "guard:5"), ("split", "guard:1")]:
            call(b, label, fn, '<<"zababcab">>', '<<"ab">>', options, module="binary")
    for pattern in ['[]', '[<<>>]', '[<<"ab">>,<<>>]', '[<<"ab">>,bad]',
                    '[<<"ab">>|tail]', '[<<1:1>>]', '[[<<"ab">>]]']:
        for subject in ['<<>>', '<<"zababcab">>']:
            for fn, label in [("match", "guard:5"), ("split", "guard:1")]:
                call(b, label, fn, subject, pattern, "[]", module="binary")
    call(b, "guard:6", "last", '<<"ab]">>', module="binary")
    call(b, "guard:7", "last", "<<>>", module="binary")
    for value in ["0", "1", "255", "256", "(1 bsl 64)+5", "1 bsl 255", "-1", "-(1 bsl 100)"]:
        call(b, "guard:3", "encode_unsigned", value, module="binary")
    for value in ["a", "0.0", "1.0", "[]", "<<1>>"]:
        call(b, "guard:4", "encode_unsigned", value, module="binary")

    l = "Modules/Lists"
    for value in ["[]", "[a,b]", "[a|improper]", "1"]:
        for tail in ["[]", "[c]", "tail"]:
            call(l, ["reverse_empty", "reverse_onto_tail", "reverse_improper", "reverse_non_list"],
                 "reverse", value, tail, module="lists")
    for value, items in [("b", "[a,b|improper]"), ("1", "[1.0]"), ("0.0", "[-0.0]"),
                         ("c", "[a|b]"), ("a", "foo"), ("b", "[a,b]"), ("b", "[]")]:
        call(l, ["member_found", "member_exact", "member_improper", "member_non_list"],
             "member", value, items, module="lists")
    for value in ["[]", "[a,b]", "[a|improper]", "[a,b|improper]", "bad"]:
        call(l, ["reverse_improper", "reverse_non_list"], "reverse", value, module="lists")
    for left, right in [
        ("[]", "tail"), ("[a,b]", "tail"), ("[1,1 bsl 100]", "[2,#{k=>v}]"),
        ("[a|improper]", "[]"), ("bad", "[1]"),
    ]:
        call(e, "append_reduces", "++", left, right)
    for value, items in [("a", "[a|improper]"), ("1", "[1.0,2]"), ("1", "[1,2|improper]")]:
        selected.append({"lean_origins": ["Modules/Lists.lean#member_found",
                                          "Modules/Lists.lean#member_improper",
                                          "Term/ShortCircuit.lean#isMember_exact",
                                          "Term/ShortCircuit.lean#isMember_agrees_with_chain"],
                         "expression": f"differential:call(lists,member,[{value},{items}])"})
    list_relation_origins = [
        "Modules/Lists.lean#reverse_empty",
        "Modules/Lists.lean#reverse_onto_tail",
        "Modules/Lists.lean#member_found",
        "Modules/Lists.lean#member_exact",
        "Modules/Erlang.lean#append_reduces",
        "Modules/Erlang.lean#list_subtraction",
        "Term/ShortCircuit.lean#isMember_exact",
        "Term/ShortCircuit.lean#isMember_agrees_with_chain",
        "Term/Compare.lean#exact_comparison_heads_first",
    ]
    for xs, ys, tail, probes in [
        ("[]", "[]", "[]", "[a,1]"),
        ("[]", "[1,2]", "[tail]", "[1,2,3]"),
        ("[1]", "[]", "improper_tail", "[1,1.0,2]"),
        ("[3,1,2,1,1.0,-1]", "[1,4,2,2]", "[9,8]", "[1,1.0,2,3,4,99]"),
        ("[0.0,-0.0,1,1.0,a]", "[1.0,0.0]", "[]", "[0.0,-0.0,1,1.0,a,b]"),
        ("[{b,[2,1]},#{k=>1},<<5:3>>,{a,[]},#{k=>1.0},[1,2]]",
         "[#{k=>1},{b,[2,1]}]",
         "{improper,tail}",
         "[{b,[2,1]},#{k=>1},#{k=>1.0},<<5:3>>,missing]"),
        ("[1 bsl 100,-(1 bsl 100),(1 bsl 255)-1,1 bsl 60,0,-(1 bsl 60),1 bsl 100]",
         "[1 bsl 100,42,-(1 bsl 255)+1]",
         "[0]",
         "[1 bsl 100,-(1 bsl 100),(1 bsl 255)-1,42,99]"),
    ]:
        selected.append({"lean_origins": list_relation_origins,
                         "expression": f"differential:list_relations({xs},{ys},{tail},{probes})"})

    s = "Modules/Sets"
    # Model-inspired public v2 behavior, not the model's version-1 coercion or
    # exhaustion. Normalize to_list results with exact keys, never lists:sort/1.
    for constructor in ["sets:new()", "sets:new([{version,2}])"]:
        probe(s, "creation", f"differential:set_elements({constructor})")
    for items in ["[]", "[a,a,b]", "[1]", "[1.0]", "[0.0]", "[-0.0]",
                  "[1,1.0,1,0.0,-0.0,0]",
                  "[{1},{1.0},[1],[1.0],#{1=>a},#{1.0=>a},<<5:3>>,1 bsl 100]"]:
        for options in ["", ",[{version,2}]"]:
            probe(s, ["creation", "size"],
                  f"differential:set_elements(sets:from_list({items}{options}))")
        for key in ["a", "1", "1.0", "-0.0", "0.0", "0", "{1.0}",
                    "[1.0]", "#{1.0=>a}", "1 bsl 100", "missing"]:
            probe(s, "membership",
                  f"sets:is_element({key},sets:from_list({items},[{{version,2}}]))")
    pairs = [
        ("[]", "[]"), ("[]", "[a,b]"), ("[a]", "[b,c]"),
        ("[a,b]", "[b,c,d]"), ("[a,b,a]", "[b,a]"), ("[a]", "[a,b,c]"),
        ("[1,0.0,{1},[1],#{1=>a}]", "[1.0,-0.0,{1.0},[1.0],#{1.0=>a}]"),
        ("[1 bsl 100,<<5:3>>,<<1,2>>]", "[1 bsl 100,<<1:1>>,a]")
    ]
    # Small heuristic boundaries and map sizes either side of 32/64. Overlaps
    # straddle subtraction's quarter-size shortcut and the 75% keep threshold.
    for size in [3, 4, 5, 31, 32, 33, 63, 64, 65]:
        left = f"lists:seq(1,{size})"
        for overlap in sorted({0, size // 4, size // 4 + 1, (size * 3) // 4, size}):
            right = f"lists:seq({size - overlap + 1},{size + 3})"
            pairs.append((left, right))
    for left, right in pairs:
        for a, b in ([(left, right)] if left == right else [(left, right), (right, left)]):
            probe(s, ["membership", "size"], f"differential:set_algebra({a},{b})")
    for items in ["[]", "[a]", "[1,1.0]", "[1 bsl 100,<<5:3>>]"]:
        for key in ["a", "1.0", "{nested,[1]}"]:
            probe(s, ["membership", "size"],
                  f"begin S=sets:from_list({items},[{{version,2}}]),"
                  f"Added=sets:add_element({key},S),"
                  f"{{differential:set_elements(Added),"
                  f"differential:set_elements(sets:add_element({key},Added)),"
                  f"differential:set_elements(sets:del_element({key},S)),"
                  f"differential:set_elements(sets:del_element({key},Added))}} end")
    for lists in ["[]", "[[]]", "[[a,b]]", "[[a,b],[b,c],[b,d]]",
                  "[[a,b],[],[a]]", "[[1,1.0],[1.0],[1.0,2]]"]:
        for operation in ["union", "intersection"]:
            probe(s, "membership",
                  f"differential:set_elements(sets:{operation}("
                  f"[sets:from_list(L,[{{version,2}}]) || L <- {lists}]))")
    # OTP decides input/option outcomes, including differences. No version-1
    # representation is compared and no model exhaustion is an expected error.
    for items in ["bad", "{}", "[a|bad]"]:
        for options in ["", ",[{version,2}]"]:
            call(s, "creation", "from_list", items + options, module="sets")
    for options in ["[]", "[{ignored,true}]", "[version]", "bad", "{}",
                    "[{version,3}]", "[{version,bad}]", "[{version,2}|bad]",
                    "[{version,2},{version,bad}]", "[{version,3},{version,2}]",
                    "[{ignored,true},{version,2}]", "[version,{version,2}]"]:
        for constructor in [f"sets:new({options})", f"sets:from_list([a,a,b],{options})"]:
            probe(s, "creation", f"differential:set_elements({constructor})")
    for invalid in ["bad", "[]", "{set}"]:
        call(s, "membership", "is_element", "a", invalid, module="sets")
        call(s, "size", "size", invalid, module="sets")
        for operation in ["union", "intersection", "subtract", "is_subset", "is_disjoint"]:
            for valid in ["sets:new([{version,2}])", "sets:from_list([a],[{version,2}])"]:
                for left, right in [(invalid, valid), (valid, invalid)]:
                    expression = f"differential:call(sets,{operation},[{left},{right}])"
                    if operation in ["union", "intersection", "subtract"]:
                        expression = f"differential:set_elements({expression})"
                    probe(s, "membership", expression)

    m = "Modules/Maps"
    for value in ["#{}", "#{a=>1,b=>2}", "#{1=>one,1.0=>float,0.0=>pos,-0.0=>neg}", "1", "[]", "{}"]:
        for key in ["a", "missing", "1", "1.0", "-0.0"]:
            call(m, ["new_and_get", "missing_key", "numeric_key_types_are_distinct", "bad_maps"],
                 "get", key, value, module="maps")
            call(m, ["put_overwrites", "get_after_put", "bad_maps"], "put", key, "new", value, module="maps")
            call(m, ["update_existing", "update_missing", "update_numeric_keys", "update_badmap"],
                 "update", key, "new", value, module="maps")
        probe(m, ["to_list_pairs", "enumeration_lookup"], f"differential:map_entries({value})")
        for mode in ["valid", "wrong_arity"]:
            probe(m, "fold_inputs", f"differential:map_fold({value},{mode})")
    call(m, "new_and_get", "new", module="maps")
    for a, b in [("#{a=>1}", "#{b=>2}"), ("#{a=>1}", "#{a=>2}"),
                 ("#{a=>2}", "#{a=>1}"), ("1", "#{}"), ("#{}", "1")]:
        call(m, ["merge_bindings", "bad_maps"], "merge", a, b, module="maps")
    for value in ["[]", "[{a,1},{a,2}]", "[{1,a},{1.0,b}]", "[{a,1}|1]", "[{a}]", "1"]:
        call(m, "from_list_pairs", "from_list", value, module="maps")
    for keys, val in [
        ("[]", "v"), ("[]", "0"), ("[a]", "{boxed,<<5:3>>,[1|2]}"),
        ("[b,a,b,c,a]", "1 bsl 100"), ("[1,1.0]", "tag"), ("[0,0.0,-0.0]", "zero"),
        ("[1,1.0,0,0.0,-0.0,1 bsl 100,-(1 bsl 100),1 bsl 100]", "mix"),
        ("[#{1=>a},#{1.0=>b},{1},{1.0},[1],[1.0],<<5:3>>,<<5:3>>,a]", "deep"),
        ("lists:seq(1,31)", "ok"), ("lists:seq(1,32)", "ok"),
        ("lists:seq(1,33)", "ok"), ("lists:seq(1,40)", "#{nested=>1}"),
        ("lists:seq(1,20)++lists:seq(10,35)", "overlap"),
        ("[a|b]", "v"), ("[a,b|1]", "v"), ("bad", "v"), ("0", "v"),
        ("{a,b}", "v"), ("#{a=>1}", "v"), ("<<1,2>>", "v"),
    ]:
        call(m, ["from_list_pairs", "numeric_key_types_are_distinct", "enumeration_lookup"],
             "from_keys", keys, val, module="maps")
    for value in ["none", "[0|#{}]"]:
        probe(m, "fold_inputs", f"differential:map_fold({value},valid)")
    # Extend fold_inputs with callback dispatch/exception behavior. Count and sum
    # are traversal-order independent, including beyond OTP's small-map limit.
    for value in ["#{}", "#{a=>7}", "#{a=>3,b=> -7,c=>11}",
                  "maps:from_list([{I,I*3-7} || I <- lists:seq(1,40)])", "none", "1"]:
        for mode in ["counted", "throw", "error", "exit", "non_function"]:
            probe(m, "fold_inputs", f"differential:map_fold({value},{mode})")
    # Public extensions of combine_inputs, not theorem instantiations. Record
    # callback arguments/multiplicity without asserting map traversal order.
    for left, right in [("#{}", "#{}"), ("#{a=>3}", "#{}"), ("#{}", "#{a=>11}"),
                        ("#{a=>3}", "#{b=>11}"), ("#{a=>3}", "#{a=>11,b=>7}"),
                        ("#{a=>11,b=>7}", "#{a=>3}"),
                        ("#{1=>integer,1.0=>float}", "#{1.0=>right,2=>only}"),
                        ("#{1=>left}", "#{1.0=>right}"),
                        ("#{<<5:3>>=>{boxed,[1,2]}}", "#{<<5:3>>=>{other,<<7:3>>}}")]:
        probe(m, "combine_inputs", f"differential:map_callback(merge_with,[{left},{right}],valid)")
    for size in [31, 32, 33, 40]:
        left = f"maps:from_list([{{I,{{left,I,list_to_binary([I,17])}}}} || I <- lists:seq(1,{size})])"
        right = f"maps:from_list([{{I,{{right,I,list_to_binary([I,29])}}}} || I <- lists:seq(2,{size+1})])"
        probe(m, "combine_inputs", f"differential:map_callback(merge_with,[{left},{right}],valid)")
        probe(m, "combine_inputs", f"differential:map_callback(filter,[{left}],valid)")
    for mode in ["throw", "error", "exit", "non_function", "wrong_arity"]:
        for left, right in [("#{}", "#{}"), ("#{a=>3}", "#{}"),
                            ("#{}", "#{a=>11}"), ("#{a=>3}", "#{b=>11}"),
                            ("#{a=>3}", "#{a=>11,b=>7}")]:
            probe(m, "combine_inputs", f"differential:map_callback(merge_with,[{left},{right}],{mode})")
    for left, right in [("bad_left", "#{}"), ("#{}", "bad_right"), ("bad_left", "bad_right")]:
        for mode in ["valid", "non_function", "wrong_arity"]:
            probe(m, "combine_inputs", f"differential:map_callback(merge_with,[{left},{right}],{mode})")
    for value in ["#{}", "#{keep=>ordinary,drop=>selected,skip=>ordinary}",
                  "#{1=>selected,1.0=>ordinary}", "#{keep=>{boxed,<<5:3>>}}"]:
        probe(m, "combine_inputs", f"differential:map_callback(filter,[{value}],valid)")
    for mode in ["throw", "error", "exit", "nonboolean", "non_function", "wrong_arity"]:
        for value in ["#{}", "#{keep=>selected}"]:
            probe(m, "combine_inputs", f"differential:map_callback(filter,[{value}],{mode})")
    for mode in ["valid", "non_function", "wrong_arity"]:
        probe(m, "combine_inputs", f"differential:map_callback(filter,[bad_map],{mode})")
    # PR #2355 adds intersection and filtermap. Reuse the same boundary/control
    # inputs, including both map-size branches, rather than just successful calls.
    for entry in list(selected):
        expression = entry["expression"]
        for old, new in [("merge_with", "intersect_with"), ("filter", "filtermap")]:
            prefix = f"differential:map_callback({old},"
            if expression.startswith(prefix):
                probe(m, "combine_inputs", expression.replace(prefix, f"differential:map_callback({new},", 1))
    for value in ["#{}", "#{keep=>7,drop=>11,change=>{boxed,<<5:3>>}}",
                  "#{1=>integer,1.0=>float}"]:
        probe(m, "combine_inputs", f"differential:map_callback(filtermap,[{value}],transform)")
    for left, right in [("#{}", "#{a=>11}"), ("#{a=>3}", "#{}"),
                        ("#{a=>3}", "#{a=>11,b=>7}"), ("#{a=>11,b=>7}", "#{a=>3}"),
                        ("#{1=>left}", "#{1.0=>right}"), ("bad_left", "bad_right")]:
        call(m, "combine_inputs", "intersect", left, right, module="maps")
    for value in ["#{}", "#{a=>error,1=>integer,1.0=>{boxed,<<5:3>>}}", "bad_map"]:
        for key in ["a", "missing", "1", "1.0"]:
            call(m, "new_and_get", "take", key, value, module="maps")
    for size in [31, 32, 33, 40]:
        value = f"maps:from_list([{{I,{{I,list_to_binary([I,17])}}}} || I <- lists:seq(1,{size})])"
        for key in ["1", str(size // 2), str(size), str(size + 1)]:
            call(m, "new_and_get", "take", key, value, module="maps")
    for keys in ["[]", "[a,a,missing]", "[1]", "[1.0,1]", "bad", "[a|bad]"]:
        for value in ["#{a=>error,1=>integer,1.0=>float}", "bad_map"]:
            for operation in ["with", "without"]:
                call(m, "enumeration_lookup", operation, keys, value, module="maps")
    for args in ["[a,#{a=>3}]", "[missing,#{a=>3}]", "[1,#{1.0=>float}]",
                 "[a,{initial,<<5:3>>},#{a=>3}]", "[missing,{initial,<<5:3>>},#{a=>3}]",
                 "[a,bad_map]", "[a,initial,bad_map]"]:
        for mode in ["valid", "throw", "error", "exit", "non_function", "wrong_arity"]:
            probe(m, "get_after_put", f"differential:map_callback(update_with,{args},{mode})")
    key_fun = "fun({K,_V})->K end"
    value_fun = "fun({_K,V})->{mapped,V} end"
    for values in ["[]", "[{b,3},{a,7},{b,11}]", "[{1,a},{1.0,b},{1,c}]", "bad", "[{a,3}|bad]"]:
        call(m, "from_list_pairs", "groups_from_list", key_fun, values, module="maps")
        call(m, "from_list_pairs", "groups_from_list", key_fun, value_fun, values, module="maps")
    for callback in ["bad", "fun()->wrong end", "fun(_)->throw(callback_failed) end",
                     "fun(_)->error(callback_failed) end", "fun(_)->exit(callback_failed) end"]:
        for values in ["[]", "[{a,3}]"]:
            call(m, "from_list_pairs", "groups_from_list", callback, values, module="maps")
            call(m, "from_list_pairs", "groups_from_list", key_fun, callback, values, module="maps")
    # Group callbacks have specified order; retain it rather than sorting effects.
    for operation in ["key_only", "key_and_value"]:
        args = "KeyFun,Values" if operation == "key_only" else "KeyFun,ValueFun,Values"
        probe(m, "from_list_pairs", "put(group_calls,[]), Values=[{b,3},{a,7},{b,11}], "
              "KeyFun=fun({K,_}=E)->put(group_calls,[{key,E}|get(group_calls)]),K end, "
              "ValueFun=fun({_,V}=E)->put(group_calls,[{value,E}|get(group_calls)]),{mapped,V} end, "
              f"Result=maps:groups_from_list({args}), {{Result,lists:reverse(erase(group_calls))}}")
    for order in ["undefined", "ordered", "reversed"]:
        for value in ["#{}", "#{a=>3,b=>7,c=>11}"]:
            iterator = f"maps:iterator({value},{order})"
            states = [iterator, f"maps:next({iterator})"]
            # Only ordered iteration has a portable next key after consumption.
            if value != "#{}" and order != "undefined":
                states.append(f"element(3,maps:next({iterator}))")
            for state in states:
                probe(m, "iterator_steps", f"differential:map_entries({state})")
                call(m, "iterator_steps", "is_iterator_valid", state, module="maps")
            if order != "undefined":
                probe(m, "iterator_steps", "maps:fold(fun(K,V,A)->[{K,V}|A] end,[]," + iterator + ")")
    # Custom total orders have no numeric-key ties. Observe consumer visits,
    # not comparator calls, whose order/count depends on the sorting algorithm.
    iterator_orders = [
        ("residue", "fun(A,B)->{A rem 3,A}=<{B rem 3,B} end"),
        ("descending", "fun(A,B)->{A rem 3,A}>={B rem 3,B} end"),
    ]
    for size in [0, 1, 31, 32, 33]:
        value = f"maps:from_list([{{K,7*K+3}} || K<-lists:seq(1,{size})])"
        for order_name, order in iterator_orders:
            for skip in sorted({0, 1, size, size + 1}):
                for consumer in ["next", "fold", "map", "filter", "filtermap", "foreach"]:
                    probe(m, "iterator_steps",
                          f"differential:map_iterator({value},{order},{skip},{consumer})",
                          iterator_scenario="consumer", iterator_size=size,
                          iterator_order=order_name, iterator_skip=skip,
                          iterator_consumer=consumer)
            probe(m, "iterator_steps",
                  f"M={value}, I=maps:iterator(M,{order}), "
                  f"Updated=maps:put(1,changed,maps:remove({size},M)), "
                  f"{{maps:to_list(I),maps:to_list(maps:iterator(Updated,{order}))}}",
                  iterator_scenario="snapshot", iterator_size=size, iterator_order=order_name)
    for size, value in [(0, "#{}"), (1, "#{a=>1}"), (3, "#{a=>1,b=>2,c=>3}")]:
        for order in ["bad", "fun()->true end", "fun(_)->true end", "fun(_,_,_)->true end"]:
            probe(m, "iterator_steps", f"maps:iterator({value},{order})",
                  iterator_scenario="invalid_order", iterator_size=size)
    for order in ["undefined", "ordered", "reversed", iterator_orders[0][1], "bad"]:
        probe(m, "iterator_steps", f"maps:iterator(bad_map,{order})",
              iterator_scenario="invalid_map")
    for size in [0, 1, 5]:
        value = f"maps:from_list([{{K,7*K+3}} || K<-lists:seq(1,{size})])"
        for mode in ["throw", "error", "exit"]:
            order = f"fun(_,_)->{mode}(comparator_failed) end"
            probe(m, "iterator_steps", f"differential:map_iterator({value},{order},0,next)",
                  iterator_scenario="comparator_exception", iterator_size=size,
                  iterator_exception=mode)
    for value in ["none", "bad", "[]", "#{a=>1}", "{a,3,none}", "{a,3,bad}"]:
        call(m, "iterator_steps", "is_iterator_valid", value, module="maps")
        probe(m, "iterator_steps", f"differential:map_entries({value})")
        for mode in ["valid", "wrong_arity"]:
            probe(m, "iterator_steps", f"differential:map_callback(filter,[{value}],{mode})")
    for value in ["#{}", "#{a=>1,b=>2}", "#{1=>one,1.0=>float}"]:
        for key in ["a", "1", "1.0"]:
            put = f"differential:call(maps,put,[{key},new,{value}])"
            update = f"differential:call(maps,update,[{key},new,{value}])"
            probe(m, "get_after_put", f"differential:call(maps,get,[{key},{put}])")
            probe(m, "update_lookup", f"differential:call(maps,get,[{key},{update}])")
            probe(m, "update_other_lookup", f"differential:call(maps,get,[b,{update}])")
    for a, b, c in [("#{a=>1}", "#{a=>2,b=>3}", "#{a=>4,c=>5}"), ("#{}", "#{1=>a}", "#{1.0=>b}")]:
        left = f"differential:call(maps,merge,[differential:call(maps,merge,[{a},{b}]),{c}])"
        right = f"differential:call(maps,merge,[{a},differential:call(maps,merge,[{b},{c}])])"
        probe(m, "merge_associative", f"{{{left},{right}}}")
    map_relation_origins = [
        "Modules/Maps.lean#mixed_same_key",
        "Modules/Maps.lean#enumeration_shadowed",
        "Modules/Maps.lean#from_literal_pairs",
        "Modules/Maps.lean#nonempty_map_equality",
        "Modules/Maps.lean#get_after_put",
        "Modules/Maps.lean#update_lookup",
        "Modules/Maps.lean#update_other_lookup",
        "Modules/Maps.lean#merge_bindings",
        "Modules/Erlang.lean#map_get/2_after_put",
        "Modules/Erlang.lean#map_size/1_shadowed_and_exact_keys",
        "Modules/Erlang.lean#semantic_comparison_operators",
        "Term/Compare.lean#association_list_order",
        "Term/Compare.lean#exact_map_equality_by_lookup",
        "Term/Compare.lean#exact_map_equality_lookup",
        "Term/Compare.lean#map_equality_lookup",
    ]
    for left_pairs, right_pairs, queries in [
        ("[]", "[]", "[a,1,1.0]"),
        ("[{a,1}]", "[]", "[a,b,1]"),
        ("[]", "[{a,1}]", "[a,b,1]"),
        ("[{a,1},{b,2},{a,3},{1,int},{1.0,float},{1,int_override}]",
         "[{b,2},{a,3},{1.0,float},{1,int_override}]",
         "[a,b,c,1,1.0]"),
        ("[{b,[]},{a,1},{a,99}]", "[{a,99},{b,[]}]", "[a,b,c]"),
        ("[{1,10},{2,20.0}]", "[{1,10.0},{2,20}]", "[1,1.0,2,2.0,3]"),
        ("[{1,a}]", "[{1.0,a}]", "[1,1.0,a]"),
        ("[{0.0,pos_zero}]", "[{-0.0,neg_zero}]", "[0.0,-0.0,0]"),
        ("[{a,0.0},{b,1}]", "[{a,-0.0},{b,1.0}]", "[a,b,c]"),
        ("[{a,1},{b,2},{c,3}]", "[{b,20},{d,4},{e,5}]", "[a,b,c,d,e,missing]"),
        ("[{b,0},{a,99}]", "[{c,0},{a,0}]", "[a,b,c]"),
        ("[{{k,[1,<<5:3>>]},#{inner=>[a,b]}},{[1,2],{tuple,1 bsl 60}}]",
         "[{{k,[1,<<5:3>>]},#{inner=>[a,b]}},{[1,2.0],{tuple,1 bsl 60}}]",
         "[{k,[1,<<5:3>>]},[1,2],[1,2.0],missing]"),
        ("[{1 bsl 60,-(1 bsl 60)},{1 bsl 100,(1 bsl 255)-1},{-(1 bsl 100),0}]",
         "[{1 bsl 60,-(1 bsl 60)},{1 bsl 100,(1 bsl 255)-2},{42,1 bsl 100}]",
         "[1 bsl 60,1 bsl 100,-(1 bsl 100),42,0]"),
    ]:
        selected.append({"lean_origins": map_relation_origins,
                         "expression": f"differential:map_relations({left_pairs},{right_pairs},{queries})"})
    for size in [31, 32, 33]:
        left_pairs = f"[{{I,I*2}} || I <- lists:seq(1,{size})]"
        right_same = f"[{{I,I*2}} || I <- lists:seq({size},1,-1)]"
        right_float = f"[{{I,case I of 17 -> 34.0; _ -> I*2 end}} || I <- lists:seq({size},1,-1)]"
        queries = f"[1,17,17.0,{size},{size + 1}]"
        selected.append({"lean_origins": map_relation_origins,
                         "expression": f"differential:map_relations({left_pairs},{right_same},{queries})"})
        selected.append({"lean_origins": map_relation_origins,
                         "expression": f"differential:map_relations({left_pairs},{right_float},{queries})"})
    for key, value in [
        ("k", "42"),
        ("{nested,[1,<<3:2>>]}", "#{inner=>1 bsl 100}"),
        ("1 bsl 100", "-(1 bsl 100)"),
    ]:
        probe(m, ["singleton_iteration", "from_literal_pairs"],
              f"differential:map_singleton_iteration({key},{value})")
    map_guard_origins = [
        "Modules/Maps.lean#is_map_guard",
        "Modules/Erlang.lean#map_guards_errors",
        "Modules/Erlang.lean#map_size/1_shadowed_and_exact_keys",
    ]
    for key in ["a", "1", "1.0", "1 bsl 100"]:
        for value in ["#{}", "#{a=>wanted,b=>2}", "#{1=>wanted,1.0=>other}",
                      "#{(1 bsl 100)=>wanted}", "[]", "1", "{a,wanted}"]:
            selected.append({"lean_origins": map_guard_origins,
                             "expression": f"differential:map_guards({key},{value})"})

    for source, names, pairs in [
        ("Term/Compare", ["numeric_and_exact_comparison", "signed_zero_comparison"],
         [("1", "1.0"), ("0.0", "-0.0"), ("-0.0", "0.0"), ("9007199254740993", "9007199254740992.0")]),
        ("Term/Compare", ["comparison_short_circuit", "comparison_keys_before_values", "nested_map_order"],
         [("{a,b}", "{z}"), ("#{a=>99,b=>0}", "#{a=>0,c=>0}"),
          ("#{1=>[]}", "#{1.0=>[]}"), ("[0|a]", "[0]"),
          ("#{#{a=>1}=>2}", "#{#{a=>2}=>1}")]),
        ("Term/Compare", ["binary_exact_kinds", "list_exact_kinds", "binary_exact_bytes",
                          "exact_comparison_heads_first"],
         [("<<>>", "[]"), ("<<128>>", "<<1:1>>"), ("[1|a]", "[1|b]"), ("<<1,2>>", "<<1,3>>"),
          ("[1,a]", "[2,b]")]),
        (e, ["tuple_equality", "map_equality", "float_equality_guards", "reflexive_operators",
             "bitstring_comparison", "ordered_operators"],
         [("{}", "{}"), ("{1}", "{1,[]}"), ("{1,a}", "{a,1}"),
          ("#{a=>1,b=>2}", "#{b=>2,a=>1}"), ("#{a=>1}", "#{a=>2}"), ("1.0", "1.0"),
          ("<<1,2>>", "<<1,3>>"), ("<<1:1>>", "<<1:2>>")]),
    ]:
        for a, b in pairs:
            probe(source, names, f"differential:compare({a},{b})")
            probe(source, names, f"differential:compare({b},{a})")

    # Same factory/code, different runtime captures: no generated fun names or
    # raw identities escape. Include numeric equality through nested captures.
    for a, b in [("10", "20"), ("1", "1.0"), ("0.0", "-0.0"),
                 ("9007199254740993", "9007199254740992.0"),
                 ("{1,a}", "{1.0,a}"), ("[1|a]", "[1.0|a]"),
                 ("#{a=>1}", "#{a=>1.0}"), ("#{1=>a}", "#{1.0=>a}"),
                 ("#{a=>99,b=>0}", "#{a=>0,c=>0}"),
                 ("<<128>>", "<<1:1>>"), ("[0|a]", "[0]"), ("{a,b}", "{z}")]:
        for left, right in [(a, b), (b, a), (a, a)]:
            probe("Term/Compare", "closure_captures_comparison",
                  f"differential:function_compare({left},{right})")
    # Same MFA does not mean the same identity: local and external captures
    # stay distinct as exact keys, including when another closure captures them.
    # Numeric captures additionally distinguish loose equality from key equality.
    for kind in ["named_zero", "named_one", "named_two", "captured_named", "numeric_capture"]:
        for padding in [0, 29, 30, 31]:  # Resulting maps have 2, 31, 32 and 33 keys.
            probe("Term/Compare", "captures_compare_as_outer" if kind == "numeric_capture"
                  else "local_and_external_functions_differ",
                  f"differential:function_identity({kind},{padding})")
    # Combine recursion, captures and escaping self references rather than
    # repeating capture-free recursion or nonrecursive function-key comparisons.
    for kind in ["tail", "non_tail"]:
        for lifetime in ["local", "gc", "message_gc", "creator_exit"]:
            for profile, captured, items in [
                ("empty", "{a,b}", "[]"),
                ("numeric", "{1,1.0}", "[4]"),
                ("compound", "{#{k=>[1,<<5:3>>]},{right,[-7|tail]}}", "[2,5,9]"),
            ]:
                selected.append({
                    "upstream_test": "test/lynx/core_to_leanj_test.exs#escaping local functions preserve captures, generated arguments, and self identity",
                    "recursive_closure_kind": kind,
                    "recursive_closure_lifetime": lifetime,
                    "recursive_closure_profile": profile,
                    "expression": f"differential:recursive_closure({kind},{lifetime},{captured},{items})",
                })
    for a, b in [("1", "make_ref()"), ("a", "make_ref()"),
                 ("make_ref()", "fun erlang:abs/1"), ("make_ref()", "fun()->ok end"),
                 ("fun erlang:abs/1", "self()"), ("fun()->ok end", "self()"),
                 ("self()", "{}"), ("{}", "#{}"), ("#{}", "[]"),
                 ("[]", "[0]"), ("[0]", "<<>>"),
                 ("''", "a"), ("a", "aa"), ("aa", "b"),
                 ("b", "'é'"), ("'é'", "'λ'"), ("'λ'", "'😀'")]:
        probe("Term/Compare", "ordered_terms", f"differential:compare({a},{b})")
        probe("Term/Compare", "ordered_terms", f"differential:compare({b},{a})")
    for value in ["make_ref()", "fun erlang:abs/1", "fun()->ok end", "self()", "'😀'"]:
        probe("Term/Compare", "ordered_terms",
              f"begin V={value},differential:compare(V,V) end")

    s = "Term/ShortCircuit"
    for op, names in [("or_else", ["orElse_short_circuits", "orElse_returns_right", "orElse_rejects_non_boolean",
                                    "orElse_keeps_left_requests"]),
                      ("and_also", ["andAlso_short_circuits", "andAlso_returns_right", "andAlso_rejects_non_boolean"])]:
        for left in ["true", "false", "1", "bad"]:
            for right in ["value", "throw"]:
                probe(s, names, f"differential:short_circuit({op},{left},{right})")
    for op in ["or_else", "and_also"]:
        for left in ["lhs_throw", "lhs_error"]:
            for right in ["value", "throw"]:
                probe(s, "orElse_propagates_left_error", f"differential:short_circuit({op},{left},{right})")
    for op in ["strict_and", "strict_or"]:
        for left in ["true", "false", "1", "bad"]:
            for right in ["value", "throw"]:
                probe(s, "and_rejects_non_boolean", f"differential:short_circuit({op},{left},{right})")
    for x in ["-1", "0", "5", "6", "10", "11", "0.0", "5.0", "bad"]:
        probe(s, "andAlso_chain_conjunction", f"differential:short_circuit_chain(right_body,{x})")
        probe(s, "andAlso_chain_conjunction", f"differential:short_circuit_chain(right_guard,{x})")
        probe(s, "andAlso_left_chain_conjunction", f"differential:short_circuit_chain(left_body,{x})")
        probe(s, "andAlso_left_chain_conjunction", f"differential:short_circuit_chain(left_guard,{x})")
    for triple in ["{false,bad,true}", "{false,throw,throw}", "{true,bad,throw}",
                   "{bad,false,throw}", "{true,true,throw}"]:
        probe(s, "andAlso_chain_conjunction", f"differential:short_circuit_chain(right_mixed,{triple})")
        probe(s, "andAlso_left_chain_conjunction", f"differential:short_circuit_chain(left_mixed,{triple})")

    p = "Modules/Process"
    for timeout in ["-1", "1.0", "bad", "4294967295", "4294967296",
                    "4294967297", "1 bsl 64", "1 bsl 100", "-(1 bsl 100)", "0", "1", "infinity"]:
        for messages in ["[before,wanted,after_message]", "[before]", "[]"]:
            if timeout in ["4294967295", "infinity"] and messages != "[before,wanted,after_message]":
                continue  # No deliberate long/infinite wait without a matching message.
            probe(p, ["timeout_validation_precedes_scan", "zero_timeout_scans_mailbox",
                      "zero_timeout_preserves_unmatched", "queued_match_precedes_timeout_choice"],
                  f"differential:receive_timeout({timeout},{messages})")
    for kind, mixed, rejected in [
        ("boolean", "[7,true,false,tail,false]", "[7,true,tail]"),
        ("tuple", "[bad,{},{tag,wrong},{tag,wanted},{last,wanted}]", "[bad,{},{tag,wrong}]"),
        ("map", "[bad,#{},#{key=>wrong},#{key=>wanted},#{key=>wanted,other=>last}]",
         "[bad,#{},#{key=>wrong}]"),
        ("division", "[0,bad,-4,5,4,3]", "[0,bad,-4,5]"),
        ("binary", "[bad,<<>>,<<1>>,<<2>>,<<2,3>>]", "[bad,<<>>,<<1>>]"),
        ("alternatives", "[{},bad,{tag,wrong},fallback,{tag,wanted}]", "[{},bad,{tag,wrong}]"),
    ]:
        for messages in [mixed, rejected, "[]"]:
            for timeout in ["0", "1"]:
                probe(p, ["guard_errors_and_false_preserve_messages", "zero_timeout_preserves_unmatched"],
                      f"differential:receive_guards({kind},{timeout},{messages})")
    for operation in ["operator", "function"]:
        for destination in ["self", "live", "dead", "named_self", "named_live", "missing",
                            "unregistered", "reference", "stale_alias", "{invalid,0}",
                            "{invalid,1 bsl 100}", "{invalid,1.5}", "{invalid,[]}",
                            "{invalid,[a]}", "{invalid,<<>>}", "{invalid,<<5:3>>}",
                            "{invalid,fun()->ok end}", "{invalid,{}}", "{invalid,{bad}}"]:
            for payload in ["ok", "{nested,[1,<<5:3>>],1 bsl 100}", "#{value=>[1,2,3],bytes=><<1,2,3>>}"]:
                probe(p, ["send_to_self", "send_to_missing_pid", "send_rejects_non_pid"],
                      f"differential:send_destination({operation},{destination},{payload})")
    for mode, origins in [
        ("dead", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_missing_pid"]),
        ("live", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_self"]),
        ("self", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_self"]),
        ("stale_alias", ["Modules/Process.lean#many_sends"]),
    ]:
        for count in [0, 1, 30]:
            selected.append({"lean_origins": origins,
                             "expression": f"differential:many_sends({mode},{count})"})
    for value in ["child", "#{a=>1}", "-0.0"]:
        probe(p, ["successive_references_are_distinct", "references_are_unique_across_processes",
                  "dictionary_preserves_reference_counter", "dictionary_preserves_mailbox",
                  "suspended_processes_keep_their_dictionaries"], f"differential:process_exchange({value})")
    for reason, args in [("boom", "[]"), ("custom_reason", '[1,<<"a">>]'),
                         ("{complex,1 bsl 100}", "none"), ("my_err", "not_a_list")]:
        probe(p, ["error_reason", "error_caught"], f"differential:error_pair({reason},{args})")
    for term in ["live_ref", "live_pid", "0", "1 bsl 100", "0.0", "true", "a",
                 "[]", "{}", "#{}", "<<>>", "fun erlang:abs/1", "fun()->ok end"]:
        probe(p, "reference_type_guard", f"differential:is_reference_paths({term})")
    probe(p, ["make_ref_preserves_process_state", "make_ref_is_effectful"],
          "differential:process_probe({refs,state_and_effect})")
    for n in [32, 64]:
        probe(p, "make_ref_fresh", f"differential:process_probe({{refs_fresh,{n}}})")
    for mode in ["preserve_unmatched", "exact_numeric"]:
        probe(p, "selective_receive_preserves_unmatched", f"differential:receive_selective({mode})")
    for mode in ["clause_priority_mailbox", "clause_priority_order"]:
        probe(p, "receive_bindings_and_clause_priority", f"differential:receive_selective({mode})")
    probe(p, "receive_body_exception_is_caught", "differential:receive_selective(receive_body_exception)")
    for messages in ["[]", "[before]"]:
        probe(p, "finite_timeout_progress", f"differential:receive_timeout(5,{messages})")
    for delay in [2, 5]:
        probe(p, ["timeout_can_expire_child", "finite_timeout_progress"],
              f"differential:process_probe({{child_timeout,{delay}}})")
    probe(p, "infinity_never_expires", "differential:process_probe(infinity_alive)")
    probe(p, "guard_reads_waiting_child_identity", "differential:process_probe(guard_child_self)")
    for mode in ["success_body", "success_body_after"]:
        probe(p, "try_success_body_is_outside_handler", f"differential:try_boundaries({mode})")
    for mode in ["handler_propagates", "handler_propagates_wildcard"]:
        probe(p, "try_handler_exception_propagates", f"differential:try_boundaries({mode})")
    probe(p, "blocking_is_not_an_exception", "differential:process_probe(blocking_not_exception)")
    for delay_mode in ["immediate", "delay_parent", "delay_child"]:
        probe(p, ["blocked_processes_resume", "handshake_every_run"],
              f"differential:process_probe({{handshake,{delay_mode}}})")
    probe(p, "two_senders_every_run", "differential:process_probe(two_senders)")
    probe(p, "terminated_child_is_not_revived", "differential:process_probe(terminated_child)")
    probe(p, "terminated_root_discards_late_messages", "differential:process_probe(terminated_root)")
    for kind in ["error", "throw", "exit"]:
        probe(p, "child_exception_is_isolated", f"differential:process_probe({{child_isolated,{kind}}})")
    for mode, name in [("normal", "monitor_reports_normal_exit"), ("raised", "monitor_reports_raised_reason"),
                       ("noproc", "monitor_missing_process"), ("flush", "demonitor_flushes_down")]:
        extra = {"stacktrace_dependent": True} if mode == "raised" else {}
        probe("Modules/Signals", name, f"differential:monitor_exit({mode})", **extra)
    for kind in ["process", "port"]:
        for state in ["self", "live", "dead", "named_self", "named_live", "missing"]:
            for options in ["two", "[]", "[{alias,demonitor}]", "[{alias,explicit_unalias}]",
                            "[{alias,reply_demonitor}]"]:
                probe("Modules/Signals", ["monitor_missing_process", "alias_ends_with_demonitor"],
                      f"differential:monitor_target({kind},{state},{options})")
    for state in ["self", "live", "dead", "named_self", "named_live", "missing"]:
        for options in ["bad", "0", "#{}", "[bad]", "[alias]", "[{alias}]",
                        "[{alias,bad}]", "[{alias,reply}]", "[{alias,demonitor,extra}]",
                        "[{alias,1}]", "[{alias,demonitor}|bad]", "[{alias,demonitor},bad]",
                        "[bad,{alias,explicit_unalias}]", "[{alias,bad},{alias,demonitor}]"]:
            probe("Modules/Signals", ["monitor_missing_process", "alias_ends_with_demonitor"],
                  f"differential:monitor_target(process,{state},{options})")
        for first in ["demonitor", "explicit_unalias", "reply_demonitor"]:
            for last in ["demonitor", "explicit_unalias", "reply_demonitor"]:
                probe("Modules/Signals", "alias_ends_with_demonitor",
                      f"differential:monitor_target(process,{state},[{{alias,{first}}},{{alias,{last}}}])")
    for mode in ["self", "pid", "alias", "reply_alias", "inactive_alias", "plain_ref",
                 "reply_then_exit", "exit_then_reply", "dead"]:
        for trap in (["false"] if mode == "dead" else ["false", "true"]):
            for reason in ["normal", "{shutdown,<<5:3>>}", "kill"]:
                entry = {
                    "expression": f"differential:async_exit({mode},{trap},{reason})",
                    "minimum_otp": 29, "async_exit_mode": mode,
                    "async_exit_trap": trap == "true", "async_exit_reason": reason,
                    "otp_api": "erlang:exit_signal/2",
                }
                if mode in ("self", "pid", "dead"):
                    site = "exit_signal_self_ends_before_timeout" if mode == "self" else "exit_signal_to_others"
                    entry["lean_origins"] = [f"Modules/Signals.lean#{site}"]
                # Alias semantics are OTP extensions: Lynx explicitly does not model them.
                selected.append(entry)
    for value in ["bad", "7", "[]", "{}", "fun()->ok end"]:
        selected.append({
            "expression": f"differential:call(erlang,exit_signal,[{value},reason])",
            "minimum_otp": 29, "async_exit_mode": "invalid", "otp_api": "erlang:exit_signal/2",
        })
    for mode in ["trapped", "untrapped", "unlinked"]:
        for reason in ["normal", "boom", "kill"]:
            probe("Modules/Signals", ["linked_crash_kills_parent", "linked_normal_exit_is_ignored",
                                      "trapped_exit_is_a_message"],
                  f"differential:linked_exit({mode},{reason})")
    for trap in ["true", "false"]:
        for linked in ["true", "false"]:
            for count in [0, 1, 16]:
                probe("Modules/Signals", ["linked_normal_exit_is_ignored", "trapped_exit_is_a_message"],
                      f"differential:self_link({trap},{linked},{count})")
    for owner in ["parent", "child"]:
        for trap in ["true", "false"]:
            for final in ["linked", "unlinked"]:
                for reason in ["normal", "boom"]:
                    for count in [0, 1, 16]:
                        probe("Modules/Signals", ["linked_crash_kills_parent", "linked_normal_exit_is_ignored",
                                                  "trapped_exit_is_a_message"],
                              f"differential:relink({owner},{trap},{final},{reason},{count})")
    # Extend linked-exit samples to unlink races; compare legal outcomes, not schedules.
    for reason in ["normal", "boom", "killed"]:
        for count in [1, 16, 64]:
            probe("Modules/Signals", ["linked_crash_kills_parent", "linked_normal_exit_is_ignored"],
                  f"differential:unlink_exit_race({reason},{count})")
    for a in ["false", "true"]:
        for b in ["false", "true"]:
            for final in ["keep", "local", "peer"]:
                for count in [1, 16, 64]:
                    probe("Modules/Signals", "linked_crash_kills_parent",
                          f"differential:crossed_unlink({a},{b},{final},{count})")
    for trap in ["true", "false"]:
        for reason in ["normal", "boom", "kill"]:
            probe("Modules/Signals", ["trapped_exit_is_a_message", "kill_cannot_be_trapped",
                                      "normal_exit_signal_is_ignored"],
                  f"differential:exit_signal({trap},{reason})")
    for mode in ["linked", "unlink_unfenced", "unlink_settled"]:
        for trap in ["true", "false"]:
            for reason in ["normal", "boom", "kill", "killed", "{shutdown,[1,<<5:3>>]}",
                           "#{reason=>{nested,1 bsl 100}}"]:
                probe("Modules/Signals", ["trapped_exit_is_a_message", "kill_cannot_be_trapped",
                                          "normal_exit_signal_is_ignored"],
                      f"differential:exit_signal({trap},{reason},{mode})")
    for arity in ["one", "two"]:
        for trap in ["true", "false"]:
            for linked in ["true", "false"]:
                for reason in ["normal", "boom", "kill", "killed", "{shutdown,[1,<<5:3>>]}",
                               "#{reason=>{nested,1 bsl 100}}"]:
                    probe("Modules/Signals", ["raise_classes", "trapped_exit_is_a_message",
                                              "kill_cannot_be_trapped"],
                          f"differential:self_exit({arity},{trap},{linked},{reason})")
    # Extend fixed trap settings to causally ordered flag changes, not reception races.
    for mode in ["before_linked", "before_explicit", "queued_explicit", "queued_self"]:
        for final in ["true", "false"]:
            for reason in ["normal", "boom", "kill", "killed", "{shutdown,[1,<<5:3>>]}",
                           "#{reason=>{nested,1 bsl 100}}"]:
                probe("Modules/Signals", ["trapped_exit_is_a_message", "kill_cannot_be_trapped",
                                          "normal_exit_signal_is_ignored"],
                      f"differential:trap_transition({mode},{final},{reason})")
    # Pending self-signals may be received on either side of the flag change.
    # Alias deactivation can instead discard an unreceived signal; never compare
    # the scheduling winner. The helper logs traces and checks allowed outcomes.
    for api, destination in [("exit", "pid"), ("exit_signal", "pid"), ("exit_signal", "alias")]:
        for timing in ["before", "received", "pending"]:
            for final in ["false", "true"]:
                for reason in ["normal", "{shutdown,<<5:3>>}", "kill"]:
                    entry = {
                        "expression": f"differential:exit_reception({api},{destination},{timing},{final},{reason}) =:= {{ok,allowed}}",
                        "assert_true": True,
                        "exit_reception_api": api, "exit_reception_destination": destination,
                        "exit_reception_timing": timing, "exit_reception_final": final == "true",
                        "exit_reception_reason": reason,
                    }
                    if api == "exit_signal":
                        entry["minimum_otp"] = 29
                    if destination == "pid":
                        # The final receive fences reception: this does NOT
                        # sample returning before an async self-signal arrives.
                        site = ("exit_self_trapped_is_handled_later" if api == "exit"
                                else "exit_signal_self_ends_before_timeout")
                        entry["lean_origins"] = [f"Modules/Signals.lean#{site}"]
                    selected.append(entry)
    for state in ["active", "active_message", "queued", "queued_twice", "queued_other", "consumed", "unknown"]:
        for options in ["[]", "[flush]", "[info]", "[flush,info]", "[info,flush]",
                        "[bad]", "[info|bad]", "[{flush,true}]"]:
            probe("Modules/Signals", ["demonitor_flushes_down", "demonitor_active_does_not_flush"],
                  f"differential:monitor_options({state},{options})")
    for options in ["[]", "[explicit_unalias]", "[reply]"]:
        probe("Modules/Signals", "alias_ends_with_demonitor",
              f"differential:alias_lifetime(standalone,{options})")
    for options in ["default", "bad", "0", "#{}", "{}", "[bad]", "[demonitor]",
                    "[reply_demonitor]", "[{alias,reply}]", "[reply|bad]",
                    "[explicit_unalias|bad]", "[reply|1]", "[reply,bad]", "[bad,reply]",
                    "[reply,bad,explicit_unalias]", "[explicit_unalias,bad,reply]",
                    "[explicit_unalias,explicit_unalias]", "[reply,reply]",
                    "[explicit_unalias,reply]", "[reply,explicit_unalias]",
                    "[reply,explicit_unalias,reply]", "[explicit_unalias,reply,explicit_unalias]",
                    "[reply,reply,explicit_unalias]"]:
        probe("Modules/Signals", "alias_ends_with_demonitor",
              f"differential:alias_lifetime(standalone,{options})")
    for value in ["bad", "0", "1 bsl 100", "1.0", "[]", "[a]", "{}", "#{}",
                  "<<>>", "<<5:3>>", "self()", "fun()->ok end", "make_ref()"]:
        call("Modules/Signals", "alias_ends_with_demonitor", "unalias", value)
    for mode in ["demonitor", "explicit_unalias", "reply_demonitor"]:
        for event in ["demonitor", "down", "unalias"]:
            probe("Modules/Signals", "alias_ends_with_demonitor",
                  f"differential:alias_lifetime({mode},{event})")
    for mode in ["plain", "demonitor", "explicit_unalias", "reply_demonitor"]:
        for action in ["keep", "demonitor", "unalias", "reply"]:
            if mode == "plain" and action == "reply":
                continue  # Only aliases are message destinations.
            for order in ["original_first", "replacement_first"]:
                probe("Modules/Signals", ["monitor_reports_normal_exit", "alias_ends_with_demonitor"],
                      f"differential:monitor_rebind({mode},{action},{order})")
    for mode in ["{standalone,explicit_unalias}", "{standalone,reply}", "{monitor,plain}",
                 "{monitor,demonitor}", "{monitor,explicit_unalias}", "{monitor,reply_demonitor}"]:
        for action in ["unalias", "demonitor", "{demonitor,[]}", "{demonitor,[flush]}",
                       "{demonitor,[info]}", "{demonitor,[flush,info]}", "{demonitor,[info,flush]}"]:
            probe("Modules/Signals", ["alias_ends_with_demonitor", "demonitor_flushes_down",
                                      "monitor_reports_normal_exit"],
                  f"differential:alias_owner({mode},{action})")
    for mode in ["plain", "demonitor", "explicit_unalias", "reply_demonitor"]:
        for state in ["active", "removed"]:
            for action in ["unalias", "demonitor", "{demonitor,[]}", "{demonitor,[flush]}",
                           "{demonitor,[info]}", "{demonitor,[flush,info]}", "{demonitor,[info,flush]}"]:
                probe("Modules/Signals", ["alias_ends_with_demonitor", "demonitor_flushes_down",
                                          "monitor_reports_normal_exit"],
                      f"differential:target_cancel({mode},{state},{action})")
    for first in ["plain", "demonitor", "explicit_unalias", "reply_demonitor"]:
        for second in ["plain", "demonitor", "explicit_unalias", "reply_demonitor"]:
            for action in ["keep", "demonitor", "unalias", "reply"]:
                if first == "plain" and action == "reply":
                    continue  # Plain monitor references are not message destinations.
                probe("Modules/Signals", ["monitor_reports_normal_exit", "alias_ends_with_demonitor"],
                      f"differential:monitor_pair({first},{second},{action})")
    for count in ["1", "8", "64"]:
        probe("Modules/Signals", ["monitor_reports_normal_exit", "kill_cannot_be_trapped"],
              f"differential:termination_queue({count})")
        for order in ["target_first", "observer_first"]:
            probe("Modules/Signals", "kill_cannot_be_trapped",
                  f"differential:termination_cascade({count},{order})")
    await_many_scenarios = [
        "both_in_order",
        "both_reverse_order",
        "both_expired_timer_race",
        "first_replies_second_times_out",
        "timeout_with_queued_and_late_replies",
        "first_crashes_second_late",
    ]
    for scenario in await_many_scenarios:
        selected.append({
            "upstream_example": "examples/elixir/task.exs#await_many_drains_replies",
            "expression": f"differential:await_many_drain({scenario})",
        })
        for alias_mode in ["reply_demonitor", "explicit_unalias", "demonitor"]:
            probe(
                "Modules/Signals",
                [
                    "alias_ends_with_demonitor",
                    "demonitor_flushes_down",
                ],
                f"differential:await_many_drain({scenario},{alias_mode})",
            )
    for mode in ["timeout", "message", "nested"]:
        probe(p, "timeout_handler_scope", f"differential:suspended_receive({mode})")

    for trap in ["true", "false"]:
        probe("Modules/Signals", "link_to_missing_process",
              f"differential:missing_link({trap})")
    probe("Modules/Signals", "liveness", "differential:process_liveness()")
    for state in ["live", "dead", "racing"]:
        for reason in ["normal", "{shutdown,<<5:3>>}"]:
            probe("Modules/Signals", "alive_may_leave_monitor_parked",
                  f"differential:liveness_monitor({state},{reason}) =:= {{ok,allowed}}",
                  assert_true=True, query_notification_family="monitor",
                  query_notification_state=state, query_notification_reason=reason)
    for query in ["alive", "info"]:
        for state, timings in [("dead", ["before", "received", "pending"]),
                               ("live", ["before", "received"])]:
            for timing in timings:
                for trap in (["false", "true"] if state == "dead" else ["true"]):
                    for reason in (["normal", "{shutdown,<<5:3>>}", "kill"]
                                   if state == "dead" else ["{shutdown,<<5:3>>}"]):
                        entry = {
                            "expression": f"differential:query_notifications({query},{state},{timing},{trap},{reason}) =:= {{ok,allowed}}",
                            "assert_true": True, "minimum_otp": 29,
                            "query_notification_family": "query", "query_notification_query": query,
                            "query_notification_state": state, "query_notification_timing": timing,
                            "query_notification_trap": trap == "true", "query_notification_reason": reason,
                        }
                        if query == "info" and state == "dead":
                            entry["lean_origins"] = ["Modules/Signals.lean#process_info_of_dead_does_not_flush"]
                        selected.append(entry)
    # Keep these sequences unfenced until the observation under test. Include
    # live/trapped/unlinked controls so constant answers cannot satisfy the batch.
    for site, scenarios in [
        ("process_info_after_kill_is_undefined",
         [f"{{kill_query,{named},{kill}}}" for named in ["false", "true"]
          for kill in ["false", "true"]]),
        ("trap_exit_after_normal_down", ["{normal_down,false}", "{normal_down,true}"]),
        ("stale_unlink_ack_keeps_new_link", ["{relink_kill,linked}", "{relink_kill,unlinked}"]),
    ]:
        for scenario in scenarios:
            for count in [1, 16, 64]:
                probe("Modules/Signals", site,
                      f"differential:signal_queue({scenario},{count})")
    for count in [1, 4, 16]:
        for mode in ["normal", "shutdown", "kill"]:
            probe("Modules/Signals", "link_exits_in_any_order",
                  f"differential:termination_fanout({mode},{count})",
                  termination_scenario="fanout", termination_mode=mode, peers=count)
        for state in ["live", "dead", "racing"]:
            probe("Modules/Signals", "queued_requests_answered_one_at_a_time",
                  f"differential:ending_requests({state},{count})",
                  termination_scenario="requests", termination_mode=state, peers=count)
    for value in ["bad", "1", "[]", "{}", "make_ref()", "fun()->ok end"]:
        call("Modules/Signals", "liveness", "is_process_alive", value)
        call("Modules/Signals", "liveness", "process_info", value, "registered_name")
    for cls in ["error", "exit", "throw", "bad", "1"]:
        for stack in ["[]", "[{differential,call,3,[]}]", "[{differential,call,[a,b,c],[]}]",
                      "bad", "[bad]", "[{differential,call,3,[]}|bad]", "[{1,call,3,[]}]"]:
            extra = (
                {"stacktrace_dependent": True}
                if cls in ("error", "exit", "throw")
                and stack in ("[]", "[{differential,call,3,[]}]", "[{differential,call,[a,b,c],[]}]")
                else {}
            )
            probe("Modules/Signals", "raise_classes",
                  f"differential:raise_exception({cls},#{{reason=>[1,2,3]}},{stack})",
                  **extra)
    for cls in ["error", "exit", "throw"]:
        for stack in ["[{differential,call,3}]", "[{differential,call,[a,b,c]}]",
                      "[{differential,call,bad}]", "[{fun erlang:abs/1,[-7]}]",
                      "[{fun erlang:abs/1,[-7],[]}]",
                      "[{differential,call,3},{fun erlang:abs/1,[-7]},"
                      "{differential,call,3,[{line,17}]}]",
                      "[{fun erlang:abs/1,[-7],[{line,17}|tail]}]"]:
            extra = {"stacktrace_dependent": True} if stack != "[{differential,call,bad}]" else {}
            probe("Modules/Signals", "raise_classes",
                  f"differential:raise_exception({cls},#{{reason=>[1,2,3]}},{stack})",
                  **extra)
    # Extend raise recipes to VM depth controls, which the model does not expose.
    for depth in ["default", "0", "1", "3", "8", "64", "65", "1 bsl 40"]:
        for count in [0, 1, 7, 8, 9, 65]:
            for style in ["{modern,error}", "{legacy,throw}"]:
                probe("Modules/Signals", "raise_classes",
                      f"differential:trace_depth({depth},{count},{style})",
                      stacktrace_dependent=True)
        for style in ["generated", "{modern,exit}", "{invalid_suffix,error}",
                      "{improper,throw}", "{child,{legacy,exit}}"]:
            extra = (
                {"stacktrace_dependent": True}
                if style not in ("{invalid_suffix,error}", "{improper,throw}")
                else {}
            )
            probe("Modules/Signals", "raise_classes",
                  f"differential:trace_depth({depth},80,{style})", **extra)
    for depth in ["-1", "1.0", "bad", "[]", "1 bsl 100"]:
        probe("Modules/Signals", "raise_classes",
              f"differential:trace_depth({depth},9,{{modern,error}})")
    for key, items in [("b", "[skipped,{}, {a,1},{b,first},{b,second}]"),
                       ("c", "[{a}]"),
                       ("missing", "[skipped,{a,1}]"), ("1", "[{1.0,float},{1,integer}]"),
                       ("1.0", "[{1,integer},{1.0,float}]"), ("0.0", "[{-0.0,negative}]"),
                       ("{1}", "[{{1.0},nested}]"), ("b", "[{b,first}|improper]"),
                       ("missing", "[{a,1}|improper]"), ("b", "[]"), ("b", "bad")]:
        for position in ["1", "2", "0", "-1", "1.0", "1 bsl 100"]:
            for fn in ["keyfind", "keymember"]:
                origins = ["Modules/Signals.lean#keyfind_finds_first_match"]
                if fn == "keymember":
                    origins.append("Modules/Lists.lean#keymember_finds_tuples")
                selected.append({"lean_origins": origins,
                                 "expression": f"differential:call(lists,'{fn}',[{key},{position},{items}])"})

    # Interior fields skip short tuples; map keys remain exact. The scalar
    # precision controls also expose OTP's lossy integer-key fast path, which
    # can select a candidate that ordinary == would reject.
    for key, decoy, stored in [
        ("9007199254740993", "9007199254740992.0", "9007199254740993"),
        ("9007199254740992.0", "9007199254740993", "9007199254740992"),
        ("-9007199254740993", "-9007199254740992.0", "-9007199254740993"),
        ("{[1],2}", "{[1],3}", "{[1.0],2.0}"),
        ("[1|2]", "[1|3]", "[1.0|2.0]"),
        ("#{1=>value}", "#{1.0=>value}", "#{1=>value}"),
        ("#{key=>1}", "#{key=>2}", "#{key=>1.0}"),
        ("#{key=>0.0}", "#{key=>1.0}", "#{key=> -0.0}"),
        ("#{#{1=>a}=>[2]}", "#{#{1.0=>a}=>[2]}", "#{#{1=>a}=>[2.0]}"),
    ]:
        extra = {"word_size_dependent": True} if "900719925474099" in key else {}
        for tail in ["[]", "improper"]:
            items = (f"[skipped,{{}},{{prefix}},{{prefix,{decoy},decoy}},"
                     f"{{prefix,{stored},first}},{{prefix,{key},second}}|{tail}]")
            for fn in ["keyfind", "keymember"]:
                call("Modules/Signals", "keyfind_finds_first_match", fn, key, "2", items, module="lists", **extra)
    # Without an exact fallback, keymember must expose the same disagreement.
    for key, stored in [("9007199254740993", "9007199254740992.0"),
                        ("9007199254740992.0", "9007199254740993"),
                        ("-9007199254740993", "-9007199254740992.0"),
                        ("-9007199254740992.0", "-9007199254740993")]:
        for fn in ["keyfind", "keymember"]:
            call("Modules/Signals", "keyfind_finds_first_match", fn, key, "2",
                 f"[skipped,{{}},{{prefix}},{{prefix,{stored},only}}]", module="lists",
                 word_size_dependent=True)

    # Raw Core, not source Erlang: computations nested in cons heads have
    # distinct hoisting rules. Keep provenance separate from Lean theorem sites.
    def core_step(tag, value, fail=False):
        action = "error" if fail else "return"
        return f"call 'differential':'core_step'('{tag}','{action}',{value})"

    def core_probe(scenario, body):
        selected.append({
            "upstream_test": "test/lynx/core_to_leanj_test.exs#a cons head runs what it computes first before the tail, and its last operation after",
            "core_order_scenario": scenario,
            "core_body": body,
            "expression": "differential:core_order(fun ?CORE_MODULE:entry/0)",
        })

    for shape, modes in [
        ("direct", ["success", "head_error", "tail_error", "both_error"]),
        ("nested_call", ["success", "head_error", "tail_error", "both_error", "prefix_error"]),
        ("let", ["success", "both_error", "prefix_error"]),
        ("sequence", ["success", "both_error"]),
        ("case", ["success", "both_error"]),
        ("shadowed_let", ["success", "both_error", "prefix_error"]),
        ("local_function", ["success", "both_error"]),
        ("try_head", ["success", "both_error"]),
    ]:
        for mode in modes:
            head_error = mode in ["head_error", "both_error"]
            g = core_step("g", "'seed'", mode == "prefix_error")
            f = core_step("f", "X", head_error)
            f_seed = core_step("f", "'seed'", head_error)
            f_nested = core_step("f", g, head_error)
            f_local = core_step("f", "apply 'local'/0()", head_error)
            h = core_step("h", "['tail']", mode in ["tail_error", "both_error"])
            head = {
                "direct": f_seed,
                "nested_call": f_nested,
                "let": f"let <X> = {g} in {f}",
                "sequence": f"do {g} {f_seed}",
                "case": f"case {g} of <X> when 'true' -> {f} end",
                "shadowed_let": f"let <X> = {g} in let <X> = {f} in X",
                "local_function": f"letrec 'local'/0 = fun () -> {g} in {f_local}",
                "try_head": f"try {g} of <X> -> {f} catch <C,R,S> -> primop 'raise'(S,R)"
            }[shape]
            core_probe(f"{shape}_{mode}", f"[{head}|{h}]")

    g = core_step("g", "'seed'")
    f = core_step("f", "X")
    h = core_step("h", "['tail']")
    case_miss = (f"case {g} of <'other'> when 'true' -> 'wrong' "
                 "<X> when 'true' -> primop 'match_fail'({'case_clause',X}) end")
    for scenario, body in [
        ("case_miss", f"[{case_miss}|{h}]"),
        ("case_miss_tail_error", f"[{case_miss}|{core_step('h', '[]', True)}]"),
        ("let_capture", "let <X> = 'outside' in "
                        f"[let <X> = {g} in {f}|{core_step('h', '[X]')}]"),
    ]:
        core_probe(scenario, body)
    for fail in [False, True]:
        values = "{" + core_step("g1", "7") + "," + core_step("g2", "9", fail) + "}"
        core_probe("tuple_argument_error" if fail else "tuple_arguments",
                   f"[{core_step('f', values)}|{h}]")
    for scenario, base, tail_error in [
        ("map_update", "~{'a'=>0}~", False),
        ("map_missing_key", "~{}~", False),
        ("map_missing_key_tail_error", "~{}~", True),
        ("map_bad_base", "'bad'", False),
    ]:
        tail = core_step("h", "['tail']", tail_error)
        # Core map updates require the base type check that source lowering
        # normally supplies; without it OTP rejects the generated BEAM.
        head = (f"let <V> = {core_step('v', '2')} in let <B> = "
                "case M of <~{}~> when 'true' -> M "
                "<Bad> when 'true' -> call 'erlang':'error'({'badmap',Bad}) end "
                "in ~{'a':=1,'b'=>V|B}~")
        core_probe(scenario, f"let <M> = call 'differential':'exception_argument'({base}) in "
                             f"[{head}|{tail}]")

    # Source Erlang binds the head first: a control against imposing the raw
    # Core cons ordering on ordinary source expressions.
    for mode in ["success", "both_error"]:
        action = "error" if mode == "both_error" else "return"
        selected.append({
            "upstream_test": "test/lynx/core_to_leanj_test.exs#calls nested in a cons head run after its tail, but their arguments before it",
            "core_order_scenario": f"source_{mode}",
            "expression": "differential:core_order(fun()->"
                          f"[differential:core_step(f,{action},differential:core_step(g,return,seed))"
                          f"|differential:core_step(h,{action},[tail])] end)",
        })

    # Annotated nonrecursive letrec labels share fallthrough continuations.
    # Values from before the match must survive calls on every path to the join.
    for path in ["core", "source"]:
        for scenario, value, failure in [
            ("first_match", "{'group',{'tag',{'probe','yes'}},41}", None),
            ("guard_false", "{'group',{'tag',{'probe','no'}},43}", None),
            ("guard_error", "{'group',{'tag',{'short'}},47}", None),
            ("later_match", "{'group',{'tag',7},53}", None),
            ("inner_pattern_miss", "{'group',{'other',99},59}", None),
            ("direct_match", "{'group','direct',61}", None),
            ("outer_pattern_miss", "'not_group'", None),
            ("first_body_error", "{'group',{'tag',{'probe','yes'}},41}", "body_first"),
            ("later_body_error", "{'group',{'tag',7},53}", "body_second"),
            ("direct_body_error", "{'group','direct',61}", "body_direct"),
            ("inner_error", "{'group',{'tag',{'probe','no'}},43}", "inner"),
            ("outer_error", "{'group',{'tag',{'short'}},47}", "outer"),
            ("outer_direct_error", "'not_group'", "outer"),
            ("input_error", "{'group','direct',61}", "input"),
            ("saved_error", "{'group','direct',61}", "saved"),
            ("join_error", "{'group',{'tag',7},53}", "join"),
        ]:
            def fallthrough_step(tag, result):
                if path == "core":
                    return core_step(tag, result, tag == failure)
                action = "error" if tag == failure else "return"
                return f"differential:core_step({tag},{action},{result})"

            capture = fallthrough_step("capture", "{'anchor',13}")
            input_ = fallthrough_step("input", value)
            saved = fallthrough_step("saved", "{Keep,97}")
            first = fallthrough_step("body_first", "{'first',V,Second}")
            later = fallthrough_step("body_second", "{'second',V2,Second}")
            direct = fallthrough_step("body_direct", "{'direct',Second}")
            inner = fallthrough_step("inner", "'seed'")
            outer = fallthrough_step("outer", "{'fallback',Input}")
            join = fallthrough_step("join", "'seed'")
            if path == "core":
                body = (
                    f"let <Keep> = {capture} in let <Input> = {input_} in let <Saved> = {saved} in "
                    f"let <Result> = (letrec 'outer'/0 = fun () -> {outer} in "
                    f"(letrec 'inner'/0 = fun () -> do {inner} apply 'outer'/0() in "
                    "case Input of <{'group',First,Second}> when 'true' -> case First of "
                    "<{'tag',V}> when call 'erlang':'=:='(call 'erlang':'element'(2,V),'yes') -> "
                    f"{first} <{{'tag',V2}}> when call 'erlang':'is_integer'(V2) -> {later} "
                    f"<'direct'> when 'true' -> {direct} "
                    "<Miss> when 'true' -> apply 'inner'/0() end "
                    "<Other> when 'true' -> apply 'outer'/0() end -| ['letrec_goto']) "
                    f"-| ['letrec_goto']) in do {join} {{Keep,Saved,Result}}"
                )
            else:
                body = (
                    f"Keep={capture},Input={input_},Saved={saved},Result=case Input of "
                    "{group,First,Second}->case First of "
                    f"{{tag,V}} when element(2,V)=:=yes->{first};"
                    f"{{tag,V2}} when is_integer(V2)->{later};direct->{direct};"
                    f"_->{inner},{outer} end;_->{outer} end,{join},{{Keep,Saved,Result}}"
                )
            entry = {
                "upstream_test": "test/lynx/core_to_leanj_test.exs#preserves nested compiler labels with immediate jumps",
                "fallthrough_scenario": scenario,
                "fallthrough_path": path,
                "expression": "differential:core_order(fun ?CORE_MODULE:entry/0)" if path == "core"
                              else f"differential:core_order(fun()->{body} end)",
            }
            if path == "core":
                entry["core_body"] = body
            selected.append(entry)

    # Observe construction itself, not already-evaluated segment arguments.
    # Source and raw Core may hoist a failing constructor differently.
    for path in ["source", "core"]:
        def step(tag, value, fail=False):
            if path == "core":
                return core_step(tag, value, fail)
            action = "error" if fail else "return"
            return f"differential:core_step({tag},{action},{value})"

        def segment(value, size="8", kind="integer", unit=1):
            if path == "core":
                width = "'undefined'" if kind == "utf8" else "'all'" if size is None else size
                scale = "'undefined'" if kind == "utf8" else str(unit)
                return f"#<{value}>({width},{scale},'{kind}',['unsigned','big'])"
            width = "" if size is None or kind == "utf8" else f":({size})"
            spec = kind if kind == "utf8" else f"{kind}-unit:{unit}"
            return f"({value}){width}/{spec}"

        def binary(*segments):
            contents = ",".join(segments)
            return f"#{{{contents}}}#" if path == "core" else f"<<{contents}>>"

        bodies = {}
        for scenario, value, size, failure in [
            ("segments_success", "5", "3", None),
            ("value_error", "5", "3", "v1"),
            ("size_error", "5", "3", "s1"),
            ("later_value_error", "5", "3", "v2"),
            ("later_size_error", "5", "3", "s2"),
            ("bad_value", "'bad'", "3", None),
            ("bad_size", "5", "-1", None),
            ("bad_value_later_error", "'bad'", "3", "v2"),
            ("bad_size_later_error", "5", "-1", "s2"),
            ("zero_bad_value", "'bad'", "0", None),
            ("zero_width", "5", "0", None),
        ]:
            bodies[scenario] = binary(
                segment(step("v1", value, failure == "v1"), step("s1", size, failure == "s1")),
                segment(step("v2", "19", failure == "v2"), step("s2", "5", failure == "s2")))

        for kind in ["binary", "utf8"]:
            value = "#{#<18>(8,1,'integer',['unsigned','big'])}#" if path == "core" else "<<18>>"
            first = (segment(step("v1", value), step("s1", "2"), "binary", 8)
                     if kind == "binary" else segment(step("v1", "16#D800"), kind="utf8"))
            for fail in [False, True]:
                name = "binary_short" if kind == "binary" else "utf_bad"
                bodies[name + ("_later_error" if fail else "")] = binary(
                    first, segment(step("v2", "19", fail), "5"))

        for scenario, prefix, kind, fail in [
            ("append_success", "bytes", "binary", False),
            ("append_bad_prefix", "'bad'", "binary", False),
            ("append_bad_prefix_later_error", "'bad'", "binary", True),
            ("append_unaligned_binary", "bits", "binary", False),
            ("append_unaligned_bitstring", "bits", "binary", False),
            ("append_shared_prefix", "bytes", "binary", False),
        ]:
            if prefix in ["bytes", "bits"]:
                prefix = binary(segment("5", "3")) if prefix == "bits" else binary(segment("18"), segment("52"))
            unit = 1 if scenario == "append_unaligned_bitstring" else 8
            suffix_size = "5" if scenario == "append_unaligned_bitstring" else "8"
            built = binary(segment("P", None, kind, unit), segment(step("v", "19", fail), suffix_size))
            result = "{P,B}" if scenario == "append_shared_prefix" else "B"
            bodies[scenario] = (
                f"let <P> = {step('p', prefix)} in let <B> = {built} in {result}"
                if path == "core" else f"P = {step('p', prefix)},B = {built},{result}")

        for scenario, wrapper, bad, tail_error in [
            ("nested_call_success", "call", False, False),
            ("nested_call_bad_value", "call", True, False),
            ("tuple_success", "tuple", False, False),
            ("tuple_bad_value", "tuple", True, False),
            ("cons_success", "cons", False, False),
            ("cons_bad_value", "cons", True, False),
            ("cons_bad_value_tail_error", "cons", True, True),
        ]:
            built = binary(segment(step("v", "'bad'" if bad else "5"), step("s", "8")))
            if wrapper == "call":
                bodies[scenario] = step("call", built)
            else:
                tail = step("tail", "['tail']" if path == "core" else "[tail]", tail_error)
                bodies[scenario] = f"[{built}|{tail}]" if wrapper == "cons" else f"{{{built},{tail}}}"

        for scenario, body in bodies.items():
            entry = {
                "upstream_test": "test/lynx/core_to_leanj_test.exs#binaries nested in arguments are constructed before the call",
                "binary_order_scenario": scenario,
                "binary_order_path": path,
                "expression": "differential:core_order(fun ?CORE_MODULE:entry/0)" if path == "core"
                              else f"differential:core_order(fun()->{body} end)",
            }
            if path == "core":
                entry["core_body"] = body
            selected.append(entry)

    # Real Core <> and <A,B> results, not one atom or tuple result. Source
    # controls cover the same exception boundaries without claiming Core arity.
    for path in ["core", "source"]:
        def try_step(tag, value):
            return core_step(tag, value) if path == "core" else f"differential:core_step({tag},return,{value})"

        for arity in [0, 2]:
            for mode in ["success", "protected_error", "protected_throw", "protected_exit",
                         "body_error", "body_throw", "body_exit",
                         "handler_error", "handler_throw", "handler_exit"]:
                stage, _, exception = mode.partition("_")
                protected = try_step("protected", "'seed'")
                if arity == 2:
                    left, right = try_step("left", "'left'"), try_step("right", "17")
                    protected = (f"do {protected} let <L> = {left} in let <R> = {right} in "
                                 if path == "core" else f"{protected},L={left},R={right},")
                else:
                    protected = f"do {protected} " if path == "core" else f"{protected},"
                result = "<L,R>" if arity else "<>"
                if path == "source":
                    result = "{L,R}" if arity else "ok"
                if stage in ["protected", "handler"]:
                    # Enter the handler first; its own exception must escape.
                    raised = exception if stage == "protected" else "throw"
                    protected += (f"call 'erlang':'{raised}'({{'protected',23}})" if path == "core"
                                  else f"erlang:{raised}({{protected,23}})")
                else:
                    protected += result
                body = try_step("body", "{B,A}" if arity else "'empty'")
                if stage == "body":
                    body = (f"do {body} call 'erlang':'{exception}'({{'body',31}})" if path == "core"
                            else f"{body},erlang:{exception}({{body,31}})")
                handler = try_step("handler", "{'caught',C,Reason}")
                if stage == "handler":
                    handler = (f"do {handler} call 'erlang':'{exception}'({{'handler',41}})" if path == "core"
                               else f"{handler},erlang:{exception}({{handler,41}})")
                body = (f"try {protected} of <{('A,B' if arity else '')}> -> {body} "
                        f"catch <C,Reason,S> -> {handler}" if path == "core" else
                        f"try begin {protected} end of {('{A,B}' if arity else 'ok')} -> {body} "
                        f"catch C:Reason -> {handler} end")
                entry = {
                    "upstream_test": "test/lynx/core_to_leanj_test.exs#translates zero and multiple Core try values",
                    "try_result_arity": arity,
                    "try_result_scenario": mode,
                    "try_result_path": path,
                    "expression": "differential:core_order(fun ?CORE_MODULE:entry/0)" if path == "core"
                                  else f"differential:core_order(fun()->{body} end)",
                }
                if path == "core":
                    entry["core_body"] = body
                selected.append(entry)

    # Transport actual zero/two results out of the try through enclosing Core
    # binds, matches and handlers. Bind before case: OTP's normal Core optimizer
    # assumes a non-values case scrutinee produces exactly one value.
    for arity in [0, 2]:
        for shape in ["let", "case", "nested"]:
            modes = ["success", "protected_throw"]
            if shape == "nested":
                modes += ["body_error", "handler_exit"]
            for mode in modes:
                result = "<B,A>" if arity else "<>"
                caught = "<'caught',Reason>" if arity else "<>"
                protected_step = core_step("protected", "'seed'")
                left_step, right_step = core_step("left", "'left'"), core_step("right", "17")
                protected = f"do {protected_step} "
                if arity:
                    protected += f"let <L> = {left_step} in let <R> = {right_step} in "
                protected += ("call 'erlang':'throw'({'protected',23})" if mode in
                              ["protected_throw", "handler_exit"] else "<L,R>" if arity else "<>")
                body_step, handler_step = core_step("body", "'seed'"), core_step("handler", "'seed'")
                body = f"do {body_step} " + (
                    "call 'erlang':'error'({'body',31})" if mode == "body_error" else result)
                handler = f"do {handler_step} " + (
                    "call 'erlang':'exit'({'handler',41})" if mode == "handler_exit" else caught)
                inner = (f"try {protected} of <{('A,B' if arity else '')}> -> {body} "
                         f"catch <C,Reason,S> -> {handler}")
                variables = "X,Y" if arity else ""
                value = "{X,Y}" if arity else "'empty'"
                if shape == "nested":
                    outer_handler = core_step("outer_handler", "{'escaped',OC,OR}")
                    body = (f"try {inner} of <{variables}> -> {core_step('outer', value)} "
                            f"catch <OC,OR,OS> -> {outer_handler}")
                else:
                    continuation = core_step("after", value)
                    if shape == "case":
                        matched = core_step("match", "'true'")
                        patterns = ("<17,'left'> when Flag -> " + continuation +
                                    " <'caught',OtherReason> when Flag -> " +
                                    core_step("after", "{'caught',OtherReason}") if arity else
                                    "<> when Flag -> " + continuation)
                        continuation = (f"let <Flag> = {matched} in case <{variables}> of {patterns} "
                                        f"<{variables}> when 'true' -> "
                                        "call 'erlang':'error'('bad_binding') end")
                    body = f"let <{variables}> = {inner} in {continuation}"
                selected.append({
                    "upstream_test": "test/lynx/core_to_leanj_test.exs#translates zero and multiple Core try values",
                    "try_result_arity": arity,
                    "try_result_scenario": f"{shape}_{mode}",
                    "try_result_path": "core",
                    "core_body": body,
                    "expression": "differential:core_order(fun ?CORE_MODULE:entry/0)",
                })

    # Reproduce lowered receive shapes, not just source-level nested receives.
    # Positive opaque timeouts retain real waits; dropped bodies end at removal.
    for path in ["core", "source"]:
        def receive_step(tag, value, fail=False):
            if path == "core":
                return core_step(tag, value, fail)
            action = "error" if fail else "return"
            return f"differential:core_step({tag},{action},{value})"

        def lowered_receive(name, action, patterns=()):
            wait = (
                "let <Expired> = primop 'recv_wait_timeout'(T) in case Expired of "
                f"<'true'> when 'true' -> {action} "
                f"<'false'> when 'true' -> apply '{name}'/0() end"
            )
            if patterns:
                clauses = " ".join(f"<{pattern}> when 'true' -> primop 'remove_message'()"
                                   for pattern in patterns)
                wait = (
                    "let <Available,Message> = primop 'recv_peek_message'() in "
                    f"case Available of <'true'> when 'true' -> case Message of {clauses} "
                    f"<Other> when 'true' -> do primop 'recv_next'() apply '{name}'/0() end "
                    f"<'false'> when 'true' -> {wait} end"
                )
            return (f"(letrec '{name}'/0 = fun () -> {wait} in apply '{name}'/0() "
                    "-| ['letrec_goto','no_inline'])")

        for family, modes in [
            ("sequence", ["success", "first_error", "second_error", "join_error",
                          "handler_error", "cleanup_error", "cleanup_overrides_error"]),
            ("discarded", ["first", "second", "timeout", "join_error", "timeout_error",
                           "timeout_join_error", "handler_error", "cleanup_error"]),
            ("rescan", ["success"]),
        ]:
            for mode in modes:
                capture = receive_step("capture", "{'anchor',13}")
                first = receive_step("first", "'one'", mode in ["first_error", "handler_error"])
                second = receive_step("second", "'two'", mode == "second_error")
                timeout = receive_step("timeout", "'ignored'", mode in ["timeout_error", "handler_error"])
                resumed = receive_step("resumed", "'selected'")
                join = receive_step("join", "{Keep,B}", mode in
                                    ["join_error", "timeout_join_error", "cleanup_overrides_error"])
                handler = receive_step("handler", "{'caught',C,R}", mode == "handler_error")
                cleanup = receive_step("cleanup", "'ok'", mode in
                                       ["cleanup_error", "cleanup_overrides_error"])
                # The float decoy must survive exact integer patterns. Two
                # matches in opposite orders expose mailbox-vs-clause priority.
                messages = "[sentinel,{take,7.0},{take,7},{take,9},tail]"
                if mode == "second":
                    messages = "[sentinel,{take,7.0},{take,9},{take,7},tail]"
                if family == "discarded" and mode in [
                    "timeout", "timeout_error", "timeout_join_error", "handler_error"
                ]:
                    messages = "[sentinel,{take,7.0},tail]"
                if path == "core":
                    if family == "sequence":
                        protected = (f"do {lowered_receive('first_wait', first)} "
                                     f"{lowered_receive('second_wait', second)}")
                    else:
                        scan = lowered_receive("scan", timeout, ["{'take',7}", "{'take',9}"])
                        protected = f"do {scan} {resumed}"
                        if family == "rescan":
                            skip = lowered_receive("skip_scan", first, ["'absent'"])
                            protected = f"do {skip} {protected}"
                    inner = f"try {protected} of <B> -> {join} catch <C,R,S> -> {handler}"
                    # Core has no source `after`: preserve both success and
                    # exceptional continuations explicitly, including reraising.
                    body = (f"let <T> = call 'differential':'exception_argument'(1) in "
                            f"let <Keep> = {capture} in try {inner} of <V> -> do {cleanup} V "
                            f"catch <AC,AR,AS> -> do {cleanup} primop 'raise'(AS,AR)")
                else:
                    if family == "sequence":
                        protected = f"receive after T -> {first} end,receive after T -> {second} end"
                    else:
                        protected = ("receive {take,7}->discarded_first;{take,9}->discarded_second "
                                     f"after T->{timeout} end,{resumed}")
                        if family == "rescan":
                            protected = f"receive absent->discarded after T->{first} end,{protected}"
                    body = (f"T=differential:exception_argument(1),Keep={capture},"
                            f"try (try begin {protected} end of B->{join} catch C:R->{handler} end) "
                            f"after {cleanup} end")
                entry = {
                    "upstream_test": "test/lynx/core_to_leanj_test.exs#" + (
                        "allocates distinct generated message binders for timeout-only receives"
                        if family == "sequence" else
                        "translates receive clauses whose unused bodies the compiler dropped"),
                    "receive_continuation_family": family,
                    "receive_continuation_scenario": mode,
                    "receive_continuation_path": path,
                    "expression": "differential:receive_continuation(" + (
                        "fun ?CORE_MODULE:entry/0" if path == "core" else f"fun()->{body} end") +
                        f",{messages})",
                }
                if path == "core":
                    entry["core_body"] = body
                selected.append(entry)

    # Upstream compiler regression, not a Lean theorem attribution. Keep the
    # same source call with either compiler-known or remotely returned operands.
    # Folded failures must retain preceding effects, but never run the body.
    effect_key = "'$lynx_exception_effects'"
    body = f"put({effect_key},[body|get({effect_key})]),matched"
    for scenario, fun, args in [
        ("atom_match", f"fun(a)->{body} end", ["a"]),
        ("atom_miss", f"fun(a)->{body} end", ["b"]),
        ("numeric_miss", f"fun(1)->{body} end", ["1.0"]),
        ("two_arg_miss", f"fun(a,2)->{body} end", ["a", "3"]),
        ("map_match", f"fun(#{{key:=7}})->{body} end", ["#{key=>7,extra=>9}"]),
        ("map_miss", f"fun(#{{key:=7}})->{body} end", ["#{other=>7}"]),
        ("guard_match", f"fun(X) when is_integer(X),X>0->{body} end", ["7"]),
        ("guard_false", f"fun(X) when is_integer(X),X>0->{body} end", ["-3"]),
        ("guard_error", f"fun(X) when element(2,X)=:=accepted->{body} end", ["{accepted}"]),
        ("later_clause", f"fun(a)->wrong_clause;(b)->{body} end", ["b"]),
        ("badfun", "not_fun", ["7"]),
        ("badarity", f"fun(_)->{body} end", ["7", "9"]),
        ("body_error", f"fun(a)->{body},error(body_failed) end", ["a"]),
        ("body_throw", f"fun(a)->{body},throw(body_failed) end", ["a"]),
        ("body_exit", f"fun(a)->{body},exit(body_failed) end", ["a"]),
    ]:
        for path in ["literal", "effectful_literal", "runtime"]:
            operands = [fun, *args]
            if path == "runtime":
                operands = [f"differential:exception_argument({value})" for value in operands]
            arguments = [f"begin put({effect_key},[arg{index}|get({effect_key})]),"
                         f"{value} end" for index, value in enumerate(operands[1:], 1)]
            setup = ""
            callee = f"({operands[0]})"
            handler = ""
            if scenario in ["badfun", "badarity"]:
                # An atom in direct callee position denotes a local function.
                setup = f"F={operands[0]},"
                callee = "F"
            if scenario == "badarity":
                # Only this case needs a fun-identity check. Keeping a reference
                # in other handlers can prevent the compiler folding the call.
                handler = ("error:{badarity,{BadFun,BadArgs}}->"
                           "{exception,error,{badarity,BadFun=:=F,BadArgs}};")
            if path == "literal":
                # Effects inside arguments inhibit Core folding on OTP 29.
                # Separate preceding effects from the pure literal call, and
                # retain effectful_literal as a third, distinct path control.
                setup += "".join(f"put({effect_key},[arg{index}|get({effect_key})]),"
                                 for index in range(1, len(args) + 1))
                arguments = args
            selected.append({
                "upstream_test": "test/lynx/core_to_leanj_test.exs#function clause failures raise function_clause alone",
                "exception_scenario": scenario,
                "exception_path": path,
                "expression": f"put({effect_key},[before]),{setup}"
                              f"Outcome=try {callee}({','.join(arguments)}) of V->{{ok,V}} catch "
                              f"{handler}ExcClass:ExcReason->{{exception,ExcClass,ExcReason}} end,"
                              f"{{Outcome,lists:reverse(erase({effect_key}))}}",
            })

    for kind in ["local_zero", "local_one", "local_two", "external_zero", "external_one",
                 "external_two", "bif_zero", "bif_one", "missing_export", "missing_module", "not_fun"]:
        for args in ["[]", "[7]", "[left,right]", "[a,b,c]", "[7|improper]", "bad"]:
            probe(e, ["dynamic_apply_2", "dynamic_apply_array", "dynamic_apply_captures",
                      "dynamic_apply_2_errors"],
                  f"differential:function_apply({kind},#{{capture=><<1,2,3>>}},{args})")
    for module, name in [("differential", "function_zero"), ("differential", "function_one"),
                         ("differential", "missing"), ("lynx_missing_functions", "missing"),
                         ("erlang", "abs"), ("1", "function_one"), ("differential", "1")]:
        for arity in ["0", "1", "255", "-1", "256", "1 bsl 100", "1.0", "bad"]:
            probe("Term/Dispatch", ["make_fun_resolves_by_name", "apply_calls_by_name"],
                  f"differential:function_make({module},{name},{arity},[7])")
        for args in ["[]", "[7]", "[left,right]", "[7|improper]", "bad"]:
            probe("Term/Dispatch", "apply_calls_by_name",
                  f"differential:function_named({module},{name},{args})")
    for target in ["local_zero", "local_capture", "make_fun_external",
                   "fun lists:reverse/1", "fun erlang:abs/1",
                   "fun differential:function_zero/0", "fun differential:function_two/2",
                   "not_a_fun", "0", "1.0", "[]", "{lists,reverse,1}", "#{}"]:
        probe("Term/Dispatch", "fun_info_mfa_reports_compiled_name",
              f"differential:fun_info_mfa({target})")
    for fun in ["fun differential:function_one/1", "fun(X)->X end", "not_fun"]:
        for arity in ["0", "1", "2", "255", "256", "-1", "1 bsl 100", "1.0"]:
            call(e, ["is_function/2_arity", "is_function/2_invalid_arity", "is_function/2_mismatch"],
                 "is_function", fun, arity)
    for kind in ["local", "external", "external_error", "external_throw", "external_exit",
                 "local_zero", "local_one", "external_one", "bif_zero", "bif_one",
                 "missing_export", "missing_module", "not_fun"]:
        extra = (
            {"stacktrace_dependent": True}
            if kind in ("external_error", "external_throw", "local_one",
                        "external_one", "bif_one", "missing_export",
                        "missing_module")
            else {}
        )
        for mode in ["monitored", "linked", "small_heap"]:
            probe("Term/Dispatch", ["spawned_dispatch", "spawn_exception_boundaries"],
                  f"differential:function_spawn({kind},{mode},#{{capture=>[1,2,3]}})",
                  **extra)

    construction = "Term/Bitstring/Construction"
    matching = "Term/Bitstring/Matching"
    for encoding, bits in [
        ("utf8", ["<<>>", "<<128>>", "<<192,128>>", "<<16#C1,16#BF>>",
                  "<<16#E0,16#80,16#80>>", "<<16#E0,16#9F,16#BF>>",
                  "<<237,160,128>>", "<<16#F0,16#80,16#80,16#80>>",
                  "<<16#F0,16#8F,16#BF,16#BF>>", "<<244,144,128,128>>",
                  "<<194,64:7>>", "<<97,255>>", "<<226,130,172>>",
                  "<<240,159,152,128>>", "<<239,187,191>>"]),
        ("utf16", ["<<>>", "<<0>>", "<<16#DC00:16>>", "<<16#D800:16>>", "<<16#D800:16,65:16>>",
                   "<<16#DC00:16,16#D800:16>>", "<<0,32:7>>", "<<16#D800:16,16#6E00:15>>",
                   "<<97:16,16#DC00:16>>", "<<16#D83D:16,16#DE00:16>>", "<<16#FEFF:16>>"]),
        ("utf16le", ["<<>>", "<<0>>", "<<16#DC00:16/little>>", "<<16#D800:16/little>>",
                     "<<16#D800:16/little,65:16/little>>", "<<16#DC00:16/little,16#D800:16/little>>",
                     "<<32,0:7>>", "<<0,216,0,110:7>>", "<<97:16/little,16#DC00:16/little>>",
                     "<<16#D83D:16/little,16#DE00:16/little>>", "<<16#FEFF:16/little>>"]),
        ("utf32", ["<<>>", "<<0,0,0>>", "<<16#D800:32>>", "<<16#110000:32>>", "<<64:31>>",
                   "<<97:32,255>>", "<<16#1F600:32>>", "<<16#FEFF:32>>"]),
        ("utf32le", ["<<>>", "<<0,0,0>>", "<<16#D800:32/little>>", "<<16#110000:32/little>>",
                     "<<64,0,0,0:7>>", "<<97:32/little,255>>", "<<16#1F600:32/little>>", "<<16#FEFF:32/little>>"]),
    ]:
        for offset in [0, 3]:
            for value in bits:
                malformed = ("utf16_rejects_malformed" if encoding.startswith("utf16") else
                             "utf32_rejects_malformed" if encoding.startswith("utf32") else
                             ["utf8_rejects_malformed", "match_utf8_rejects_overlong"])
                origins = ([malformed] if isinstance(malformed, str) else malformed) + [
                    "utf_rejects_incomplete_final_code_unit",
                    "utf_preserves_invalid_suffix",
                    "utf_unaligned_with_partial_suffix",
                ]
                probe(matching, origins,
                      f"differential:utf({encoding},<<5:{offset},({value})/bitstring>>,{offset})")
            for codepoint in [0, 127, 128, 0x7FF, 0x800, 0xD7FF, 0xD800, 0xDFFF,
                              0xE000, 0xFEFF, 0xFFFF, 0x10000, 0x20AC, 0x1F600,
                              0x10FFFF, 0x110000, -1]:
                probe(construction, ["utf_boundaries", "utf_characters", "utf_invalid_scalars",
                                     "utf_unaligned_concatenation"],
                      f"differential:utf_construct({encoding},{codepoint},{offset})")

    for encoding in ["utf8", "utf16", "utf16le", "utf32", "utf32le"]:
        for codepoint in [0, 0x7F, 0x80, 0x7FF, 0x800, 0xD7FF, 0xE000, 0xFEFF,
                          0xFFFF, 0x10000, 0x20AC, 0x1F600, 0x10FFFF]:
            c_origins = ["utf_boundaries", "utf_characters", "utf16_size", "utf32_preserves_scalar"]
            m_origins = ["utf_scalar_boundaries", "utf32_big_roundtrip",
                         "utf8_returns_scalar", "utf16_returns_scalar", "utf32_returns_scalar"]
            probe(construction, c_origins, f"differential:utf_roundtrip({encoding},{codepoint})")
            probe(matching, m_origins, f"differential:utf_roundtrip({encoding},{codepoint})")
        for nonint in ["a", "1.0", "<<65>>", "[65]", "{}"]:
            probe(construction, "utf_noninteger",
                  f"differential:utf_construct({encoding},{nonint},0)")
        for nonbits in ["a", "65", "[65]", "{<<65>>}"]:
            probe(matching, "utf_nonbitstring_inputs",
                  f"differential:utf_match_term({encoding},{nonbits})")

    # Whole, sized, unit-checked, and associative bitstring construction.
    for kind, val, size, origins in [
        ("whole_bitstring", "<<>>", "0", ["whole_bitstring_preserves_encoding"]),
        ("whole_bitstring", "<<5:3>>", "0", ["whole_bitstring_preserves_encoding"]),
        ("whole_bitstring", "<<16#AA,5:3>>", "0", ["whole_bitstring_preserves_encoding"]),
        ("whole_bitstring", "bad", "0", ["invalid_operands"]),
        ("whole_binary", "<<1,2,3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_binary", "<<5:3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_binary", "<<1,5:3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_binary", "123", "0", ["invalid_operands"]),
        ("whole_binary_unit16", "<<1,2,3,4>>", "0", ["whole_segment_checks_unit"]),
        ("whole_binary_unit16", "<<1,2,3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_binary_unit16", "<<5:3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_bitstring_unit3", "<<5:3>>", "0", ["whole_segment_checks_unit"]),
        ("whole_bitstring_unit3", "<<63:6>>", "0", ["whole_segment_checks_unit"]),
        ("whole_bitstring_unit3", "<<1>>", "0", ["whole_segment_checks_unit"]),
        ("whole_bitstring_unit256", "<<0:256>>", "0", ["whole_segment_checks_unit", "maximum_unit"]),
        ("whole_bitstring_unit256", "<<0:512>>", "0", ["whole_segment_checks_unit", "maximum_unit"]),
        ("whole_bitstring_unit256", "<<0:248>>", "0", ["whole_segment_checks_unit", "maximum_unit"]),
        ("whole_bitstring_unit256", "<<5:3>>", "0", ["whole_segment_checks_unit", "maximum_unit"]),
        ("sized_binary", "<<1,2,3>>", "2", ["sized_segment_preserves_prefix"]),
        ("sized_binary", "<<16#AA,5:3>>", "1", ["sized_segment_preserves_prefix"]),
        ("sized_binary", "<<1,2>>", "0", ["sized_segment_preserves_prefix", "zero_width"]),
        ("sized_binary", "<<5:3>>", "1", ["sized_segment_preserves_prefix"]),
        ("sized_binary", "<<1>>", "2", ["sized_segment_preserves_prefix"]),
        ("sized_binary", "<<1,2>>", "-1", ["negative_size"]),
        ("sized_binary", "bad", "0", ["zero_width", "invalid_operands"]),
        ("sized_binary_unit16", "<<1,2,3,4,5>>", "2", ["sized_segment_preserves_prefix"]),
        ("sized_binary_unit16", "<<1,2,3>>", "2", ["sized_segment_preserves_prefix"]),
        ("sized_bitstring", "<<16#AB,5:3>>", "5", ["sized_segment_preserves_prefix"]),
        ("sized_bitstring", "<<16#AB,5:3>>", "11", ["sized_segment_preserves_prefix"]),
        ("sized_bitstring", "<<16#AB,5:3>>", "12", ["sized_segment_preserves_prefix"]),
        ("sized_bitstring", "<<1,2>>", "0", ["zero_width"]),
        ("sized_bitstring", "<<1,2>>", "-1", ["negative_size"]),
        ("sized_bitstring", "<<1,2>>", "bad", ["invalid_operands"]),
        ("sized_bitstring", "<<1,2>>", "1 bsl 64", ["invalid_operands"]),
        ("sized_bitstring_unit3", "<<16#AB,5:3>>", "3", ["sized_segment_preserves_prefix"]),
        ("sized_bitstring_unit3", "<<16#AB,5:3>>", "4", ["sized_segment_preserves_prefix"]),
        ("concat_empty", "<<>>", "0", ["concatenation_empty"]),
        ("concat_empty", "<<5:3>>", "0", ["concatenation_empty"]),
        ("concat_empty", "<<1,2,3:5>>", "0", ["concatenation_empty"]),
        ("concat_assoc", "{<<5:3>>,<<16#AB:8>>,<<3:2>>}", "0",
         ["concatenation_preserves_bits", "concatenation_associative"]),
        ("concat_assoc", "{<<1:1>>,<<0:7>>,<<16#FF>>}", "0",
         ["concatenation_preserves_bits", "concatenation_associative"]),
        ("cross_byte_concat_unit3", "{<<5:3>>,16#1234,<<3:2>>}", "5",
         ["units_and_cross_byte_concatenation", "scaled_unit_truncation"]),
        ("cross_byte_concat_little_unit5", "{<<2:2>>,-17,<<1:3>>}", "3",
         ["units_and_cross_byte_concatenation", "scaled_unit_truncation"]),
    ]:
        probe(construction, origins, f"differential:bit_construct_binary({kind},{val},{size})")

    probe(matching, "build_from_bytes", "differential:bit_construct_binary(build_from_bytes,0,0)")

    # Integer bitstring construction across widths, signedness, endianness, units, and errors.
    for spec, val, size, origins in [
        ("unsigned_big", "0", "8", ["unsigned_byte_boundaries", "integer_segment_size"]),
        ("unsigned_big", "255", "8", ["unsigned_byte_boundaries", "integer_segment_preserves_bits"]),
        ("unsigned_big", "256", "8", ["unsigned_byte_boundaries"]),
        ("unsigned_big", "-1", "8", ["unsigned_byte_boundaries", "negative_values_and_truncation"]),
        ("unsigned_big", "-256", "8", ["unsigned_byte_boundaries", "negative_values_and_truncation"]),
        ("signed_big", "-128", "8", ["signed_byte_boundaries"]),
        ("signed_big", "127", "8", ["signed_byte_boundaries"]),
        ("signed_big", "128", "8", ["signed_byte_boundaries"]),
        ("signed_big", "-129", "8", ["signed_byte_boundaries"]),
        ("unsigned_big", "16#1234", "16", ["byte_aligned_endianness"]),
        ("unsigned_little", "16#1234", "16", ["byte_aligned_endianness"]),
        ("signed_big", "-32768", "16", ["multibyte_overflow_and_underflow"]),
        ("signed_little", "32768", "16", ["multibyte_overflow_and_underflow"]),
        ("signed_big", "-32769", "16", ["multibyte_overflow_and_underflow"]),
        ("unsigned_big", "65536", "16", ["multibyte_overflow_and_underflow"]),
        ("unsigned_big", "16#5A", "7", ["subbyte_endianness"]),
        ("unsigned_little", "16#5A", "7", ["subbyte_endianness"]),
        ("signed_big", "-5", "4", ["subbyte_endianness"]),
        ("signed_little", "-5", "4", ["subbyte_endianness"]),
        ("unsigned_big", "16#ABC", "12", ["big_and_little_partial_bytes"]),
        ("unsigned_little", "16#ABC", "12", ["big_and_little_partial_bytes"]),
        ("signed_big", "-2049", "12", ["big_and_little_partial_bytes", "partial_byte_overflow_and_underflow"]),
        ("signed_little", "2048", "12", ["big_and_little_partial_bytes", "partial_byte_overflow_and_underflow"]),
        ("unsigned_big", "8", "3", ["partial_byte_overflow_and_underflow"]),
        ("signed_big", "-5", "3", ["partial_byte_overflow_and_underflow"]),
        ("unsigned_little", "16#1FFF", "13", ["partial_byte_overflow_and_underflow"]),
        ("unsigned_big", "(1 bsl 100) + 16#ABCD", "13", ["large_integer_truncation"]),
        ("unsigned_little", "-(1 bsl 100) - 1", "13", ["large_integer_truncation", "negative_values_and_truncation"]),
        ("signed_big", "(1 bsl 128) - 1", "64", ["large_integer_truncation", "multibyte_overflow_and_underflow"]),
        ("unsigned_little", "1 bsl 200", "128", ["large_integer_truncation"]),
        ("unit3_unsigned_big", "16#DEAD", "5", ["scaled_unit_truncation", "units_and_cross_byte_concatenation"]),
        ("unit5_signed_little", "-(1 bsl 40) - 3", "3", ["scaled_unit_truncation", "units_and_cross_byte_concatenation"]),
        ("unit256_big", "(1 bsl 255) + 16#1234", "1", ["maximum_unit"]),
        ("unit256_little", "16#123456", "2", ["maximum_unit"]),
        ("unsigned_big", "123", "0", ["zero_width"]),
        ("signed_little", "1 bsl 100", "0", ["zero_width"]),
        ("unsigned_big", "bad", "0", ["zero_width", "invalid_operands"]),
        ("unsigned_big", "1", "-1", ["negative_size"]),
        ("unsigned_big", "bad", "-1", ["negative_size", "invalid_operands"]),
        ("unsigned_big", "1.5", "8", ["invalid_operands"]),
        ("unsigned_big", "bad", "8", ["invalid_operands"]),
        ("unsigned_big", "1", "bad", ["invalid_operands"]),
        ("unsigned_big", "1", "1 bsl 64", ["invalid_operands"]),
    ]:
        for offset in [0, 3]:
            probe(construction, origins,
                  f"differential:bit_construct_int({spec},{val},{size},{offset})")

    # Float16, float32, float64 construction and error paths.
    float_construct_cases = [
        # Float64 signed zeros, normals, extremes, integer conversions.
        ("big", "0.0", "64", ["float64_signed_zeros", "float64_size"]),
        ("big", "-0.0", "64", ["float64_signed_zeros", "float64_size"]),
        ("little", "{ieee64,1,0,0}", "64", ["float64_signed_zeros", "float64_unaligned_concatenation"]),
        ("little", "{ieee64,0,1023,0}", "64", ["float64_normal_values", "float64_unaligned_concatenation"]),
        ("big", "{ieee64,1,1024,1}", "64", ["float64_normal_values"]),
        ("big", "{ieee64,0,0,1}", "64", ["float64_extremes"]),
        ("little", "{ieee64,1,0,1}", "64", ["float64_extremes"]),
        ("big", "{ieee64,0,1,0}", "64", ["float64_extremes"]),
        ("big", "{ieee64,0,2046,(1 bsl 52)-1}", "64", ["float64_extremes"]),
        ("little", "{ieee64,1,2046,(1 bsl 52)-1}", "64", ["float64_extremes"]),
        ("big", "0", "64", ["float64_integer_conversion"]),
        ("big", "(1 bsl 53) + 1", "64", ["float64_integer_conversion"]),
        ("big", "-((1 bsl 53) + 1)", "64", ["float64_integer_conversion"]),
        ("big", "(1 bsl 255) - 1", "64", ["float64_integer_conversion"]),
        ("big", "-(1 bsl 255)", "64", ["float64_integer_conversion"]),
        # Runtime float widths (including unit:2).
        ("unit2_big", "{ieee64,0,1023,0}", "32", ["runtime_float_widths", "float64_size"]),
        ("unit2_little", "{ieee64,1,1020,0}", "16", ["runtime_float_widths", "float32_construction"]),
        ("unit2_big", "{ieee64,0,1023,0}", "8", ["runtime_float_widths", "float16_construction"]),
        ("big", "1.0", "-1", ["runtime_float_widths"]),
        ("big", "1.0", "size", ["runtime_float_widths"]),
        # Float16 OTP bs_construct_SUITE fp16/1 vectors.
        ("big", "{ieee64,0,999,16971005}", "16", ["float16_construction"]),
        ("little", "{ieee64,1,999,16971005}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1008,4494803592606853}", "16", ["float16_construction"]),
        ("little", "{ieee64,1,1008,4494803592606853}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1009,0}", "16", ["float16_construction"]),
        ("little", "{ieee64,1,1009,0}", "16", ["float16_construction"]),
        ("big", "65504", "16", ["float16_construction"]),
        ("little", "-65504", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1038,4499201580859392}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1022,4499201592118391}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,0,0}", "16", ["float16_construction"]),
        ("little", "{ieee64,1,0,0}", "16", ["float16_construction"]),
        ("big", "1", "16", ["float16_construction"]),
        ("little", "-1", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1023,4398035252105}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1021,1499733803991469}", "16", ["float16_construction"]),
        ("big", "1000000000", "16", ["float16_construction"]),
        ("little", "-1000000000", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1004,2674012278751232}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1001,1125899906842624}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1007,1741626418397184}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1023,2199560126464}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1023,2199023255552}", "16", ["float16_construction"]),
        ("big", "{ieee64,0,1023,2199027449856}", "16", ["float16_construction"]),
        # Float32 vectors.
        ("big", "{ieee64,0,0,0}", "32", ["float32_construction"]),
        ("big", "{ieee64,0,1020,0}", "32", ["float32_construction"]),
        ("little", "{ieee64,0,1020,0}", "32", ["float32_construction"]),
        ("big", "{ieee64,1,0,0}", "32", ["float32_construction"]),
        ("big", "{ieee64,0,2019,2221273467876764}", "32", ["float32_construction"]),
        ("little", "{ieee64,1,2019,2221273467876764}", "32", ["float32_construction"]),
        ("big", "{ieee64,0,856,3921383706114079}", "32", ["float32_construction"]),
        ("big", "{ieee64,0,1019,2702159776422298}", "32", ["float32_construction"]),
        ("big", "288230393331580929", "32", ["float32_construction"]),
        ("big", "-288230393331580929", "32", ["float32_construction"]),
        ("big", "576460786663161857", "32", ["float32_construction"]),
        ("big", "-576460786663161857", "32", ["float32_construction"]),
        ("big", "576460752303423487", "32", ["float32_construction"]),
        ("big", "1 bsl 200", "32", ["float32_construction"]),
        ("little", "-(1 bsl 200)", "32", ["float32_construction"]),
        # Narrow float & width errors (keep integers within AtomVM 256-bit limit).
        ("big", "one", "16", ["narrow_float_errors"]),
        ("little", "one", "64", ["narrow_float_errors"]),
        ("big", "1.0", "0", ["narrow_float_errors", "zero_width"]),
        ("big", "1.0", "8", ["narrow_float_errors"]),
        ("big", "1.0", "24", ["narrow_float_errors"]),
        ("big", "1.0", "128", ["narrow_float_errors"]),
    ]
    for spec, val, size, origins in float_construct_cases:
        extra = (
            {"word_size_dependent": True}
            if size == "32" and val in {"288230393331580929", "-288230393331580929"}
            else {}
        )
        for offset in [0, 3]:
            probe(construction, origins,
                  f"differential:float_construct({spec},{val},{size},{offset})", **extra)

    # Bitstring binary, tail, alignment, and literal matching.
    for spec, bits, size, origins in [
        ("binary_sized_rest", "<<1,2,3>>", "2", ["binary_consumes_exact_width"]),
        ("binary_sized_rest", "<<1,2,3:5>>", "2", ["binary_consumes_exact_width"]),
        ("binary_sized_rest", "<<1,2:7>>", "2", ["short_binary_input"]),
        ("binary_sized_rest", "<<>>", "1", ["short_binary_input"]),
        ("binary_sized_rest", "not_bits", "0", ["short_binary_input"]),
        ("binary_unit16_rest", "<<1,2,3,4,5>>", "2", ["binary_consumes_exact_width"]),
        ("binary_unit16_rest", "<<1,2,3>>", "2", ["short_binary_input"]),
        ("bitstring_sized_rest", "<<16#AB,5:3>>", "5", ["binary_consumes_exact_width"]),
        ("bitstring_sized_rest", "<<16#AB,5:3>>", "11", ["binary_consumes_exact_width"]),
        ("bitstring_sized_rest", "<<16#AB,5:3>>", "12", ["short_binary_input"]),
        ("bitstring_unit3_rest", "<<16#AB,5:3>>", "3", ["binary_consumes_exact_width"]),
        ("bitstring_unit3_rest", "<<16#AB,5:3>>", "4", ["short_binary_input"]),
        ("tail_binary_after3", "<<5:3,1,2>>", "0", ["tail_alignment"]),
        ("tail_binary_after3", "<<5:3>>", "0", ["tail_alignment"]),
        ("tail_binary_after3", "<<5:3,1:1>>", "0", ["tail_alignment"]),
        ("tail_bitstring_after3", "<<5:3,1:1>>", "0", ["tail_alignment"]),
        ("tail_binary_unit16_after4", "<<9:4,1,2,3,4>>", "0", ["tail_alignment"]),
        ("tail_binary_unit16_after4", "<<9:4,1,2,3>>", "0", ["tail_alignment"]),
        ("fixed_tail_bitstring16", "<<5:3,16#1234:16>>", "0", ["fixed_tail_sizes"]),
        ("fixed_tail_bitstring16", "<<5:3,16#123:15>>", "0", ["fixed_tail_sizes"]),
        ("fixed_tail_bitstring16", "<<5:3,16#12345:17>>", "0", ["fixed_tail_sizes"]),
        ("fixed_tail_binary2", "<<5:3,16#AB,16#CD>>", "0", ["fixed_tail_sizes"]),
        ("fixed_tail_binary2", "<<5:3,16#AB,7:7>>", "0", ["fixed_tail_sizes"]),
        ("finish_byte", "<<42>>", "0", ["finish_requires_no_remaining_bits"]),
        ("finish_byte", "<<42,0>>", "0", ["finish_requires_no_remaining_bits"]),
        ("finish_byte", "<<42,1:1>>", "0", ["finish_requires_no_remaining_bits"]),
        ("finish_byte", "<<>>", "0", ["finish_requires_no_remaining_bits"]),
        ("unaligned_binary2_rest", "<<5:3,16#AB,16#CD,3:2>>", "0", ["unaligned_binary_capture"]),
        ("unaligned_binary2_rest", "<<5:3,16#AB,16#CD>>", "0", ["unaligned_binary_capture"]),
        ("unaligned_binary2_rest", "<<5:3,16#AB,7:7>>", "0", ["short_binary_input", "unaligned_binary_capture"]),
        ("unaligned_bitstring16_rest", "<<5:3,16#12,16#34,1:1>>", "0", ["unaligned_binary_capture"]),
        ("unaligned_tail_binary", "<<5:3,16#AB,16#CD>>", "0", ["unaligned_capture_requires_binary_length"]),
        ("unaligned_tail_binary", "<<5:3>>", "0", ["unaligned_capture_requires_binary_length"]),
        ("unaligned_tail_binary", "<<5:3,16#AB,1:1>>", "0", ["unaligned_capture_requires_binary_length"]),
    ]:
        probe(matching, origins, f"differential:bit_match_binary({spec},{bits},{size})")

    # Integer matching: exact width, short, zero-width, signed, partial little-endian, dynamic size.
    for spec, bits, size, origins in [
        ("unsigned_big_rest", "<<110,117>>", "8", ["match_leading_byte", "integer_consumes_exact_width"]),
        ("unsigned_big_rest", "<<1,2,3>>", "16", ["match_leading_bytes", "integer_consumes_exact_width"]),
        ("unsigned_big_rest", "<<>>", "8", ["match_byte_empty", "short_integer_input"]),
        ("unsigned_big_rest", "<<255:7>>", "8", ["short_integer_input"]),
        ("unsigned_big_rest", "<<16#AB,5:3>>", "11", ["integer_consumes_exact_width"]),
        ("unsigned_big_rest", "<<16#AB,5:3>>", "12", ["short_integer_input"]),
        ("zero_width_tail", "<<>>", "0", ["integer_zero_width"]),
        ("zero_width_tail", "<<1,2>>", "0", ["integer_zero_width"]),
        ("zero_width_tail", "<<5:3>>", "0", ["integer_zero_width"]),
        ("zero_width_tail", "not_bits", "0", ["integer_zero_width", "integer_pattern_failures"]),
        ("signed_big_rest", "<<128>>", "8", ["integer_signed_boundaries"]),
        ("signed_big_rest", "<<127>>", "8", ["integer_signed_boundaries"]),
        ("signed_big_rest", "<<255>>", "8", ["integer_signed_boundaries"]),
        ("signed_little_rest", "<<0,128>>", "16", ["integer_signed_boundaries"]),
        ("signed_little_rest", "<<255,127>>", "16", ["integer_signed_boundaries"]),
        ("signed_big_rest", "<<4:3,1:1>>", "3", ["integer_signed_boundaries"]),
        ("signed_big_rest", "<<3:3,1:1>>", "3", ["integer_signed_boundaries"]),
        ("signed_big_rest", "<<16#8000000000000000:64>>", "64", ["integer_signed_boundaries"]),
        ("unsigned_little_rest", "<<16#BC,16#A:4,1:1>>", "12", ["integer_partial_little_endian"]),
        ("signed_little_rest", "<<16#BC,16#F:4>>", "12", ["integer_partial_little_endian"]),
        ("unaligned_unsigned_little_rest", "<<5:3,16#1234:13/little,3:2>>", "13", ["integer_partial_little_endian"]),
        ("unaligned_signed_little_rest", "<<5:3,(-7):11/signed-little,1:1>>", "11", ["integer_partial_little_endian"]),
        ("unsigned_big_exact", "<<1>>", "8", ["finish_requires_no_remaining_bits"]),
        ("unsigned_big_exact", "<<1,2>>", "8", ["finish_requires_no_remaining_bits", "integer_pattern_failures"]),
        ("unsigned_big_rest", "bad_input", "8", ["integer_pattern_failures"]),
        ("unsigned_big_rest", "<<1,2>>", "-1", ["integer_pattern_failures"]),
        ("unsigned_big_rest", "<<1,2>>", "bad", ["integer_pattern_failures"]),
        ("size_from_prev_binary", "<<2,16#AA,16#BB,5:3>>", "0", ["size_from_previous_segment"]),
        ("size_from_prev_binary", "<<0,5:3>>", "0", ["size_from_previous_segment"]),
        ("size_from_prev_binary", "<<3,1,2>>", "0", ["size_from_previous_segment"]),
        ("size_from_prev_unit2", "<<5:4,16#2AA:10,1:2>>", "0", ["size_from_previous_segment"]),
        ("size_from_prev_signed", "<<15:4,1,2>>", "0", ["size_from_previous_segment"]),
        ("consecutive_segments", "<<5:3,29:5,16#5A3:11/little,19:5,3:2>>", "0", ["consecutive_integer_segments"]),
    ]:
        probe(matching, origins, f"differential:bit_match_int({spec},{bits},{size})")

    # Float16, float32, float64 matching (finite values, signed zero, subnormals, nonfinite & invalid widths).
    for spec, bits, size, origins in [
        ("big", "<<60,0>>", "16", ["float_extraction_returns_float", "float16_matching"]),
        ("big", "<<0,0>>", "16", ["float16_matching"]),
        ("big", "<<128,0>>", "16", ["float16_matching"]),
        ("big", "<<0,1>>", "16", ["float16_matching"]),
        ("big", "<<128,1>>", "16", ["float16_matching"]),
        ("big", "<<3,255>>", "16", ["float16_matching"]),
        ("big", "<<4,0>>", "16", ["float16_matching"]),
        ("big", "<<123,255>>", "16", ["float16_matching"]),
        ("little", "<<255,251,7:3>>", "16", ["float16_matching"]),
        ("unaligned_big", "<<5:3,123,255,3:2>>", "16", ["float16_matching"]),
        ("big", "<<124,0>>", "16", ["float16_matching"]),
        ("big", "<<252,0>>", "16", ["float16_matching"]),
        ("big", "<<124,1>>", "16", ["float16_matching"]),
        ("big", "<<126,0>>", "16", ["float16_matching"]),
        ("big", "<<123,255:7>>", "16", ["float16_matching"]),
        ("big", "<<0,0,0,0>>", "32", ["float32_matching"]),
        ("big", "<<128,0,0,0>>", "32", ["float32_matching"]),
        ("big", "<<0,0,0,1>>", "32", ["float32_matching"]),
        ("big", "<<128,0,0,1>>", "32", ["float32_matching"]),
        ("big", "<<62,0,0,0>>", "32", ["float32_matching"]),
        ("big", "<<127,127,255,255>>", "32", ["float32_matching"]),
        ("little", "<<0,0,0,192,1:1>>", "32", ["float32_matching"]),
        ("unaligned_little", "<<5:3,0,0,128,63,3:2>>", "32", ["float32_matching"]),
        ("big", "<<127,128,0,0>>", "32", ["float32_matching"]),
        ("big", "<<255,128,0,0>>", "32", ["float32_matching"]),
        ("big", "<<127,192,0,0>>", "32", ["float32_matching"]),
        ("big", "<<127,128,0,1>>", "32", ["float32_matching"]),
        ("big", "<<62,0,0,0:7>>", "32", ["float32_matching"]),
        ("big", "<<0:64>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("big", "<<1:1,0:63>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("big", "<<0,0,0,0,0,0,0,1>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("big", "<<128,0,0,0,0,0,0,1>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("big", "<<0,15,255,255,255,255,255,255>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("unaligned_little", "<<5:3,1,0,0,0,0,0,0,128,3:5>>", "64", ["float64_signed_zero_and_subnormal"]),
        ("big", "<<0,16,0,0,0,0,0,0>>", "64", ["float64_normal_and_extreme"]),
        ("big", "<<63,240,0,0,0,0,0,0,9>>", "64", ["float64_normal_and_extreme"]),
        ("big", "<<127,239,255,255,255,255,255,255>>", "64", ["float64_normal_and_extreme"]),
        ("little", "<<255,255,255,255,255,255,239,255>>", "64", ["float64_normal_and_extreme"]),
        ("exact64", "<<63,240,0,0,0,0,0,0>>", "64", ["float64_normal_and_extreme", "finish_requires_no_remaining_bits"]),
        ("exact64", "<<63,240,0,0,0,0,0,0,1>>", "64", ["finish_requires_no_remaining_bits", "float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "<<127,240,0,0,0,0,0,0>>", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "<<255,240,0,0,0,0,0,0>>", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "<<127,248,0,0,0,0,0,0>>", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "<<127,240,0,0,0,0,0,1>>", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "<<63,240,0,0,0,0,0,0:7>>", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("big", "not_bits", "64", ["float64_rejects_nonfinite_and_short_inputs"]),
        ("unit2_big", "<<60,0,9>>", "8", ["runtime_float_widths", "float16_matching"]),
        ("unit2_big", "<<62,0,0,0>>", "16", ["runtime_float_widths", "float32_matching"]),
        ("unit2_big", "<<63,240,0,0,0,0,0,0>>", "32", ["runtime_float_widths", "float64_normal_and_extreme"]),
        ("unit4_little", "<<0,60>>", "4", ["runtime_float_widths"]),
        ("unit4_little", "<<0,0,0,62>>", "8", ["runtime_float_widths"]),
        ("unit4_little", "<<0,0,0,0,0,0,240,63>>", "16", ["runtime_float_widths"]),
        ("big", "<<0:128>>", "0", ["float_zero_and_invalid_widths"]),
        ("big", "<<0:128>>", "8", ["float_zero_and_invalid_widths"]),
        ("big", "<<0:128>>", "24", ["float_zero_and_invalid_widths"]),
        ("big", "<<0:128>>", "48", ["float_zero_and_invalid_widths"]),
        ("big", "<<0:128>>", "128", ["float_zero_and_invalid_widths"]),
        ("big", "<<0:128>>", "-1", ["float_zero_and_invalid_widths", "runtime_float_widths"]),
        ("big", "<<0:128>>", "bad", ["float_zero_and_invalid_widths", "runtime_float_widths"]),
    ]:
        probe(matching, origins, f"differential:bit_match_float({spec},{bits},{size})")

    # Literal bitstring matching: prefix, exact byte/bit, and sub-byte patterns.
    for kind, bits, origins in [
        ("prefix_binary", "<<\"abc\">>", ["literal_prefix_tail"]),
        ("prefix_binary", "<<\"abcdef\">>", ["literal_prefix_tail"]),
        ("prefix_binary", "<<\"abcd\",1:1>>", ["literal_prefix_tail", "tail_alignment"]),
        ("prefix_binary", "<<\"ab\">>", ["literal_prefix_tail"]),
        ("prefix_bitstring", "<<\"abcd\",1:1>>", ["literal_prefix_tail"]),
        ("prefix_partial", "<<16#A0>>", ["literal_prefix_tail", "partial_literal_pattern"]),
        ("prefix_partial", "<<5:3>>", ["literal_prefix_tail", "partial_literal_pattern"]),
        ("prefix_partial", "<<4:3,1>>", ["literal_prefix_tail", "partial_literal_pattern"]),
        ("prefix_partial", "<<>>", ["literal_prefix_tail"]),
        ("full_abc", "<<\"abc\">>", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_abc", "<<\"abcd\">>", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_abc", "<<\"abc\",0:1>>", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_abc", "<<\"ab\">>", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_abc", "<<\"abd\">>", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_abc", "abc", ["full_literal_pattern_requires_exact_bytes_and_length"]),
        ("full_partial_5_3", "<<5:3>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<16#A0:3>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<16#80:3>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<16#A0:4>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<16#A0:0>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<16#A0,0:3>>", ["partial_literal_pattern"]),
        ("full_partial_5_3", "<<>>", ["partial_literal_pattern"]),
        ("unaligned_partial_5_3", "<<5:3,5:3>>", ["partial_literal_pattern"]),
        ("unaligned_partial_5_3", "<<5:3,4:3>>", ["partial_literal_pattern"]),
        ("unaligned_partial_5_3", "<<5:3,5:4>>", ["partial_literal_pattern"]),
    ]:
        probe(matching, origins, f"differential:bit_match_literal({kind},{bits})")

    hashing = "Modules/Hashing"
    hashing_vectors = [
        "0", "1", "-1", "255", "256", "1000", "-1000", "134217727", "134217728",
        "-134217728", "-134217729", "4294967296", "576460752303423487",
        "-576460752303423488", "576460752303423488", "18446744073709551616",
        "-18446744073709551616", "18446744073709551615",
        "1267650600228229401496703205376", "-1267650600228229401496703205369",
        "123456789012345678901234567890",
        "{float_bits,0,0,0}", "{float_bits,1,0,0}", "{float_bits,0,1023,0}",
        "{float_bits,1,1023,2251799813685248}",
        "{float_bits,0,1024,2570632149304942}",
        "{float_bits,0,2019,2221273467876764}", "{float_bits,0,0,1}",
        "a", "open", "closed", "term", "bdd_bot", "bdd_top", "'hello world'",
        "{utf8_atom,<<99,97,102,195,169>>}", "{utf8_atom,<<207,128>>}",
        "'Elixir.Module.Types.Descr'",
        "'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'",
        "{utf8_atom,<<195,131,194,169>>}",
        "{utf8_atom,<<230,151,165,230,156,172,232,170,158>>}",
        "<<>>", "<<1>>", "<<97,98,99>>",
        "<<104,101,108,108,111,32,119,111,114,108,100,33>>",
        "<<104,101,108,108,111,32,119,111,114,108,100,33,33>>",
        "<<48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52>>",
        "<<1:1>>", "<<5:3>>", "<<1,2,3:4>>",
        "<<116,119,101,108,118,101,32,98,121,116,101,115,1:1>>",
        "[]", "[1]", "[1,2,3]", "[1,2,3,4]", "[1,2,3,4,5]",
        "[104,101,108,108,111]", "[a|b]", "[1|2]", "[1,2|3]", "[256,1,2]",
        "[1,a,2,3]", "[[1],[2]]", "[open]", "[term|term]",
        "[[]]", "[1,-1]", "[300,1,2,3,4,5]",
        "{}", "{a}", "{1,2}", "{a,{b,c},[1,2]}",
        "#{}", "#{a=>1}", "#{b=>2,a=>1}", "#{1=>a,[x]=>{y}}", "#{#{}=>#{a=>[]}}",
        "{map_seq_40}",
    ]
    for spec in hashing_vectors:
        probe(hashing, "guard:1", f"differential:phash2_probe({spec})")
    for size in ["1", "2", "7", "13", "97", "98", "1 bsl 27", "(1 bsl 32)-1", "1 bsl 32"]:
        probe(hashing, ["guard:1", "guard:3"], f"differential:phash2_range(a,{size})")
    for spec in ["0", "-1", "{float_bits,0,1024,2570632149304942}", "<<1,2,3:4>>", "{map_seq_40}"]:
        probe(hashing, ["guard:1", "guard:3"], f"differential:phash2_ranges({spec})")
    for spec, bad_range in [("a", "0"), ("a", "-1"), ("a", "-(1 bsl 32)"), ("{map_seq_40}", "0")]:
        probe(hashing, "guard:4", f"differential:phash2_range({spec},{bad_range})")
    for bad_range in ["(1 bsl 32)+1", "1 bsl 64", "1 bsl 100", "1.0", "bad", "[]"]:
        probe(hashing, "guard:5", f"differential:phash2_range(a,{bad_range})")
    for variant in ["shadowed_single", "insertion_order", "numeric_exact_keys",
                    "boundary_31_32_33", "large_40_history", "nested_maps_and_bitstrings"]:
        probe(hashing, ["guard:2", "shadowed_bindings_hash"],
              f"differential:phash2_map_relations({variant})")
    for variant in ["closures_shadowed_map", "closures_signed_zero_and_external",
                    "pids_and_refs", "composite_live_identities"]:
        probe(hashing, "capture_hash", f"differential:phash2_live_identities({variant})")

    # Remaining behavioral probes across Modules/Signals.lean, Term/Try.lean, and Modules/Erlang.lean.
    for expr_origin, expr in [
        ("success_binds_multiple_values", "try {7, 9} of {A, B} -> A + B catch _:_ -> caught end"),
        ("success_binds_no_values", "try ok of ok -> success catch _:_ -> caught end"),
        ("exception_bindings", "try erlang:throw(t) catch C:R -> {C, R} end"),
        ("exception_bindings", "try erlang:error(e) catch C:R -> {C, R} end"),
        ("exception_bindings", "try erlang:exit(x) catch C:R -> {C, R} end"),
        ("success_body_is_outside_handler", "try 7 of V -> throw(V) catch _:_ -> caught end"),
        ("handler_exception_propagates", "try error(protected) catch _:_ -> exit(handler) end"),
        ("outer_handler_catches_success_body",
         "try try 7 of V -> throw(V) catch _:_ -> inner end catch _:R -> R end"),
        ("unmatched_handler_reraises", "try throw(fail) catch error:R -> {caught, R} end"),
        ("unmatched_handler_reraises", "try error(fail) catch throw:R -> {caught, R} end"),
        ("unmatched_handler_reraises", "try exit(fail) catch error:R -> {caught, R} end"),
        ("reraise_preserves_class",
         "try throw(orig) catch C:_R:Stk -> try erlang:raise(C, replaced, Stk) catch C2:R2 -> {C2, R2} end end"),
        ("reraise_preserves_class",
         "try error(orig) catch C:_R:Stk -> try erlang:raise(C, replaced, Stk) catch C2:R2 -> {C2, R2} end end"),
        ("reraise_preserves_class",
         "try exit(orig) catch C:_R:Stk -> try erlang:raise(C, replaced, Stk) catch C2:R2 -> {C2, R2} end end"),
        ("protected_effects_survive_failure",
         "erase(try_key), try put(try_key, protected), throw(failure) "
         "catch C:R -> {C, R, get(try_key), erase(try_key)} end"),
        ("cleanup_runs_on_success_and_failure",
         "erase(try_cleanup), R = try 7 after put(try_cleanup, done) end, {R, erase(try_cleanup)}"),
        ("cleanup_runs_on_success_and_failure",
         "erase(try_cleanup), Res = (try try throw(failure) after put(try_cleanup, done) end "
         "catch C:R -> {C, R} end), {Res, erase(try_cleanup)}"),
        ("cleanup_failure_overrides_previous_outcome", "try 7 after exit(cleanup) end"),
        ("cleanup_failure_overrides_previous_outcome", "try throw(failure) after exit(cleanup) end"),
        ("dispatched_exception_is_caught",
         "F = fun() -> throw(dispatched) end, try F() catch C:R -> {C, R} end"),
        ("dispatched_success_body_exception_escapes",
         "F = fun() -> throw(dispatched) end, try ok of _ -> F() catch _:_ -> caught end"),
        ("resumed_receive_exception_is_caught",
         "self() ! 7, try receive V -> throw(V) after 1000 -> timeout end catch _:R -> R end"),
    ]:
        probe("Term/Try", expr_origin, expr)
    for expr_origin, expr in [
        ("iterator_shapes", "differential:call(erts_internal,mc_iterator,[#{}])"),
        ("iterator_shapes", "differential:call(erts_internal,mc_iterator,[[a]])"),
        ("iterator_shapes", "differential:call(erts_internal,mc_iterator,[a])"),
        ("iterator_entries", "#{{k, K} => V * 10 || K := V <- #{a => 1, b => 2}}"),
        ("refill_raises", "differential:call(erts_internal,mc_refill,[a])"),
        ("refill_raises", "differential:call(erts_internal,mc_refill,[[]])"),
        ("refill_raises", "differential:call(erts_internal,mc_refill,[none])"),
    ]:
        probe("Modules/ErtsInternal", expr_origin, expr)
    for expr_origin, expr in [
        ("entries_in_requested_order", "#{b := X, a := Y, b := Z} = #{a => 1, b => 2}, {X, Y, Z}"),
        ("entries_shadowed_key", "M = maps:from_list([{a, 1}, {a, 2}]), #{a := X} = M, X"),
        ("entries_missing_key", "try #{a := _, b := _} = #{} of _ -> matched catch error:{badmatch, _} -> badmatch end"),
        ("entries_missing_later", "try #{a := _, b := _} = #{a => 1} of _ -> matched catch error:{badmatch, _} -> badmatch end"),
        ("entries_empty", "#{} = #{a => 1}, ok"),
        ("entries_empty_nonmap", "try (fun(M) -> #{} = M end)(1) of _ -> matched catch error:{badmatch, 1} -> badmatch end"),
        ("entries_numeric_keys", "try #{1 := _} = #{1.0 => 1} of _ -> matched catch error:{badmatch, _} -> badmatch end"),
        ("entries_arity", "M = #{a => 1, b => 2, c => 3}, #{a := A, c := C} = M, length([A, C])"),
    ]:
        probe("Term/Maps", expr_origin, expr)
    for expr_origin, expr in [
        ("recursive_dispatch", "F = fun Self(0) -> 0; Self(N) -> Self(N - 1) end, F(3)"),
        ("pure_calls",
         "Add = fun(C) -> fun(X) -> C + X end end, F = Add(10), Bad = not_a_fun, "
         "{F(7), (try F() catch error:{badarity, {G, []}} -> {badarity, is_function(G, 1)} end), "
         "(try Bad() catch error:{badfun, not_a_fun} -> badfun end)}"),
        ("effectful_call", "is_pid((fun() -> self() end)())"),
        ("apply_call", "Inc = fun(X) -> X + 1 end, Forward = fun(X) -> Inc(X) end, Forward(1)"),
        ("sequential_dispatch", "F = fun() -> 7 end, {F(), F()}"),
        ("catch_dispatched_exception", "F = fun() -> throw(9) end, try F() catch throw:9 -> 42 end"),
        ("dispatched_state",
         "erase(dispatch_k), F = fun() -> put(dispatch_k, 1), 7 end, R = F(), {R, erase(dispatch_k)}"),
    ]:
        probe("Term/Dispatch", expr_origin, expr)
    for kind in ["tag_mismatch", "forged_then_real_down", "real_then_forged_down", "noproc_with_forged"]:
        probe("Modules/Signals", "demonitor_flushes_oldest_monitor_message",
              f"differential:demonitor_oldest_message({kind})")
    probe("Modules/Signals", "killed_root_keeps_environment", "differential:killed_linked_root(exit_atom)")
    probe("Modules/Signals", "killed_root_keeps_environment", "differential:killed_linked_root(error_reason)",
          stacktrace_dependent=True)

    for op, a, b in [("/", "0.0", "0.0"), ("/", "1.0", "0.0"), ("/", "-1.0", "0.0"),
                     ("+", "-1.7976931348623157e308", "-1.7976931348623157e308"),
                     ("-", "-1.7976931348623157e308", "1.7976931348623157e308"),
                     ("*", "1.7976931348623157e308", "-2.0"),
                     ("*", "1 bsl 255", "1.0e240"),
                     ("*", "-(1 bsl 255)", "1.0e240")]:
        probe(e, "nonfinite_float_results_rejected", f"differential:execution_paths('{op}',{a},{b})")

    non_numeric_terms = ["bad", "[]", "[1]", "{}", "#{}", "<<1>>", "fun erlang:abs/1"]
    for op, origin in [("+", "addition_rejects_nonnumeric"), ("*", "multiplication_rejects_nonnumeric")]:
        for num in ["0", "1", "1.5"]:
            for other in non_numeric_terms:
                probe(e, origin, f"differential:execution_paths('{op}',{num},{other})")
                probe(e, origin, f"differential:execution_paths('{op}',{other},{num})")
        probe(e, origin, f"differential:execution_paths('{op}',bad,[])")
        probe(e, origin, f"differential:execution_paths('{op}',[],bad)")

    for op in ["div", "rem", "band", "bor", "bxor", "bsl", "bsr"]:
        for n in ["0", "2"]:
            for non_int in ["0.0", "1.0", "bad", "[]", "<<1>>"]:
                probe(e, "integer_operations_reject_nonintegers", f"differential:execution_paths('{op}',{n},{non_int})")
                probe(e, "integer_operations_reject_nonintegers", f"differential:execution_paths('{op}',{non_int},{n})")

    for bits in [0, 1, 8, 28, 32, 59, 60, 64, 100]:
        for n in ["0", "1", f"(1 bsl {bits})-1", f"1 bsl {bits}", f"(1 bsl {bits})+7", "(1 bsl 130)+12345"]:
            probe(e, "word_mask", f"differential:word_mask_check({n},{bits})")
    for val in ["-1", "-7", "-8", "-9", "7", "-(1 bsl 100)", "-(1 bsl 100)-1", "(1 bsl 100)-1"]:
        for bits in ["0", "1", "2", "100", "200"]:
            probe(e, "right_shift_is_floor_division", f"differential:execution_paths('bsr',{val},{bits})")
    for n in ["0", "-1", "1", "-2", "(1 bsl 27)-1", "1 bsl 27", "-(1 bsl 27)",
              "(1 bsl 59)-1", "1 bsl 59", "-(1 bsl 59)", "(1 bsl 63)-1", "-(1 bsl 63)",
              "1 bsl 64", "(1 bsl 100)+7", "-(1 bsl 100)-7", "0.0", "bad"]:
        probe(e, "bnot/1_involution", f"differential:bnot_involution({n})")

    for n in ["-1", "0", "5", "10", "11", "0.0", "5.0", "bad"]:
        probe(e, "integer_range_guard", f"differential:integer_range_check({n},0,10)")
    for n, lo, hi in [("5", "10", "0"), ("bad", "10", "0"), ("5", "0.0", "10"),
                      ("5", "0", "10.0"), ("bad", "bad", "10"), ("bad", "0", "bad"),
                      ("-(1 bsl 100)-1", "-(1 bsl 100)", "1 bsl 100"),
                      ("-(1 bsl 100)", "-(1 bsl 100)", "1 bsl 100"),
                      ("1 bsl 59", "-(1 bsl 100)", "1 bsl 100"),
                      ("(1 bsl 63)-1", "-(1 bsl 100)", "1 bsl 100"),
                      ("1 bsl 100", "-(1 bsl 100)", "1 bsl 100"),
                      ("(1 bsl 100)+1", "-(1 bsl 100)", "1 bsl 100")]:
        probe(e, "integer_range_guard", f"differential:integer_range_check({n},{lo},{hi})")

    for extra in ["0", "a", "[]", "{}", "#{}", "<<>>", "fun()->ok end"]:
        probe(e, "local_node", f"differential:local_node_check(local,{extra})")
    for invalid in ["[]", "1", "a", "{}", "#{}", "<<>>", "fun()->ok end"]:
        probe(e, "local_node", f"differential:local_node_check(invalid,{invalid})")

    for n in ["0", "3", "-3", "100", "(1 bsl 53)+1", "1 bsl 60", "1 bsl 255", "-(1 bsl 255)"]:
        for flt in ["0.75", "0.5", "1.25"]:
            probe(e, "integer_float_product", f"differential:execution_paths('*',{n},{flt})")
            probe(e, "integer_float_product", f"differential:execution_paths('*',{flt},{n})")

    for kind in ["parent_child_sync", "child_immediate"]:
        probe(e, "spawn_schedules_child_or_parent_first", f"differential:spawn_behavior({kind})")
    probe(e, "completed_nested_processes_are_removed", "differential:nested_spawn_cleanup()")
    for kind in ["invalid_not_fun", "invalid_arity1", "invalid_arity2"]:
        probe(e, "spawn_rejects_invalid_fun", f"differential:spawn_behavior({kind})")
    for kind in ["unknown_export", "unknown_module"]:
        probe(e, "spawn_unknown_fun_fails_in_child", f"differential:spawn_behavior({kind})",
              stacktrace_dependent=True)
    for term in ["0", "1", "-1", "(1 bsl 27)-1", "1 bsl 27", "-(1 bsl 27)-1",
                 "(1 bsl 59)-1", "1 bsl 59", "(1 bsl 63)-1", "-(1 bsl 63)",
                 "1 bsl 64", "1 bsl 100", "-(1 bsl 255)",
                 "0.0", "-0.0", "1.5", "-1.5", "5.0e-324", "1.7976931348623157e308",
                 "true", "false", "a", "[]", "[1]", "{}", "#{1=>1}", "<<>>", "<<1:1>>",
                 "fun erlang:abs/1", "fun()->1 end", "live_pid", "live_ref"]:
        probe(e, ["numeric_guards_integer_identity", "numeric_guards_invalid"],
              f"differential:is_number_paths({term})")
    return selected


def _encode_atom(name):
    raw = name.encode("utf-8")
    return bytes([119, len(raw)]) + raw


def _encode_float32_ok(value):
    f32 = struct.unpack(">f", struct.pack(">f", value))[0]
    etf = bytes([131, 104, 2]) + _encode_atom("ok") + bytes([70]) + struct.pack(">d", f32)
    return base64.b64encode(etf).decode("ascii")


def _encode_exception(exc_class, reason):
    etf = (bytes([131, 104, 3])
           + _encode_atom("exception")
           + _encode_atom(exc_class)
           + _encode_atom(reason))
    return base64.b64encode(etf).decode("ascii")


def float32_cases():
    """Representation-aware IEEE-754 binary32 cases for AVM_USE_32BIT_FLOAT=ON builds."""
    return [
        {"expression": "differential:call(erlang,'+',[16777216.0,1.0])",
         "float32_expected": _encode_float32_ok(16777216.0)},
        {"expression": "differential:arithmetic(add,16777216.0,1.0)",
         "float32_expected": _encode_float32_ok(16777216.0)},
        {"expression": "differential:call(erlang,'+',[16777216.0,2.0])",
         "float32_expected": _encode_float32_ok(16777218.0)},
        {"expression": "differential:call(erlang,'+',[1.0,1.0e-8])",
         "float32_expected": _encode_float32_ok(1.0)},
        {"expression": "differential:call(erlang,'*',[1.5,-2])",
         "float32_expected": _encode_float32_ok(-3.0)},
        {"expression": "differential:call(erlang,'/',[1,2])",
         "float32_expected": _encode_float32_ok(0.5)},
        {"expression": "differential:call(erlang,abs,[-1.5])",
         "float32_expected": _encode_float32_ok(1.5)},
        {"expression": "differential:call(erlang,'*',[3.4e38,2.0])",
         "float32_expected": _encode_exception("error", "badarith")},
        {"expression": "differential:arithmetic(mul,3.4e38,2.0)",
         "float32_expected": _encode_exception("error", "badarith")},
        {"expression": "differential:call(erlang,'*',[1.0e-45,0.5])",
         "float32_expected": _encode_float32_ok(0.0)},
    ]


def inventory(checkout, selected, failures=None, unverified=()):
    """Inventory theorem/guard sites, never infer branch or proof coverage."""
    root = checkout / "Lean/LynxTest"
    if not root.is_dir():
        raise ValueError(f"Missing Lynx runtime tests: {root}")
    failed = {entry["case"]["module"] for entry in failures or ()}
    unknown = {entry["case"]["module"] for entry in unverified}
    rows = []
    known = set()
    for path in sorted(root.rglob("*.lean")):
        source = path.relative_to(root).as_posix()
        raw = path.read_bytes()
        guard = 0
        tests = []
        for line, text in enumerate(raw.decode().splitlines(), 1):
            match = re.match(r"^(?:(?:private|public)\s+)?theorem\s+(«[^»]+»|[\w'.]+)", text)
            if match:
                name = match[1].strip("«»")
            elif text.startswith("#guard "):
                guard += 1
                name = f"guard:{guard}"
            else:
                continue
            identifier = f"{source}#{name}"
            known.add(identifier)
            calls = [entry for entry in selected if identifier in entry.get("lean_origins", [])]
            tests.append({"test": name, "line": line, "status": "has_samples" if calls else "no_samples",
                          "cases": len(calls),
                          "passed": None if failures is None else sum(entry["module"] not in failed | unknown for entry in calls),
                          "failed": sum(entry["module"] in failed for entry in calls),
                          "unverified": sum(entry["module"] in unknown for entry in calls)})
        notes = {
            "ProofAudit.lean": "Lean kernel axiom/dependency audits; no Erlang execution equivalent.",
            "PublicApi.lean": "Lean API snapshots, #check and rewrite interfaces; not VM API coverage.",
            "Counterexample.lean": "Model schedule exploration and diagnostics; no fixed runtime race outcome is asserted.",
            "Purity.lean": "Symbolic purity, termination and execution-budget properties are not runtime tests.",
            "Modules/ErtsInternal.lean": "Public mc_iterator/1 non-map shapes, map comprehensions, and mc_refill/1 error paths cover 3/3 theorems without comparing VM-internal iterator structures.",
            "Modules/Process.lean": "Samples observe public process behavior, not all schedules, model PID paths, deadlock or exhaustion.",
            "Modules/Timers.lean": "Delivery/cancellation handshakes and OTP error paths; elapsed milliseconds and all-run model proofs are not cross-VM expectations.",
            "Modules/Signals.lean": "Samples normalize identities and error stack contents; signal/link/alias combinations remain incomplete.",
            "Term/Dispatch.lean": "Public recursive, captured, effectful, sequential, exception, and stateful dispatch samples cover 12/17 theorems; remaining 5 test synthetic depth-0/exhaustion and table-resolve internals.",
            "Term/Order.lean": "Fixed-point/admissibility proofs and divergence are not sampled as bounded successes.",
            "Term/Try.lean": "Public try/catch/after behavior samples cover 14/17 theorems; stacktrace_is_empty, exhaustion_is_not_caught, and blocking_is_not_caught are model-only expectations.",
            "Term/Maps.lean": "Public Erlang map pattern-matching samples cover 8/8 theorems; shadowed association-list internals are model representations.",
            "Term/Bitstring.lean": "Internal padding validity and proof objects are not observable VM heap contracts.",
            "Term/Bitstring/Construction.lean": "Public bitstring construction samples within AtomVM's 256-bit integer limit; ofBits representation lemmas are internal to Lean.",
            "Term/Bitstring/Matching.lean": "Public bitstring matching samples; extract_rewrites is a Lean simp lemma.",
        }
        rows.append({"source": source, "sha256": hashlib.sha256(raw).hexdigest(), "tests": tests,
                     "note": notes.get(source, "Finite public-behavior samples and invalid-input extensions, not universal theorem instances.")})
    attributed = {test for entry in selected for test in entry.get("lean_origins", [])}
    if attributed - known:
        raise ValueError(f"Lynx test attribution no longer exists: {sorted(attributed - known)}")
    return rows
