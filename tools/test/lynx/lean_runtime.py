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

    t = "Modules/Timers"
    for payload in ["tick", "{nested,#{1=>one,1.0=>float},<<5:3>>}"]:
        for mode, label, delay in [
            ("deliver", "timer_delivers", 0), ("deliver", "timer_delivers", 1),
            ("cancel", "cancel_twice", 60000), ("cancel", "cancel_stops_or_message_arrives", 0),
            ("expired", "cancel_twice", 0),
            ("dead_before", "cancel_after_destination_exits", 60000),
            ("dead_after", "cancel_after_destination_exits_later", 60000),
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
        call(b, "guard:3", "match", subject, pattern, options, module="binary")
    # Earlier matches beat longer later ones; at the same position the longest
    # fitting pattern wins, independent of pattern-list order and scope sign.
    for pattern in ['[<<"ab">>,<<"abc">>]', '[<<"abc">>,<<"ab">>]']:
        for options in ["[]", "[{scope,{3,3}}]", "[{scope,{6,-3}}]",
                        "[{scope,{3,2}}]", "[{scope,{3,0}}]", "[{scope,{8,0}}]"]:
            call(b, "guard:3", "match", '<<"zababcab">>', pattern, options, module="binary")
            call(b, "guard:1", "split", '<<"zababcab">>', pattern, options, module="binary")
            call(b, "guard:1", "split", '<<"zababcab">>', pattern, f"[global|{options}]", module="binary")
    for options in ["[{scope,{0,3}},{scope,{3,3}}]", "[{scope,{3,3}},{scope,{0,3}}]",
                    "[{scope,invalid},{scope,{3,3}}]", "[{scope,{0,99}},{scope,{3,3}}]",
                    "[{scope,{3,3}},{scope,invalid}]", "[{scope,{3,3}}|ignored]",
                    "[{scope,{3,3}},bad]", "[{scope,{3,3,0}}]", "[{scope,{3,1.0}}]",
                    "bad", "[bad]", "[{scope,{1 bsl 100,0}}]"]:
        for fn, label in [("match", "guard:3"), ("split", "guard:1")]:
            call(b, label, fn, '<<"zababcab">>', '<<"ab">>', options, module="binary")
    for pattern in ['[]', '[<<>>]', '[<<"ab">>,<<>>]', '[<<"ab">>,bad]',
                    '[<<"ab">>|tail]', '[<<1:1>>]', '[[<<"ab">>]]']:
        for subject in ['<<>>', '<<"zababcab">>']:
            for fn, label in [("match", "guard:3"), ("split", "guard:1")]:
                call(b, label, fn, subject, pattern, "[]", module="binary")
    call(b, "guard:4", "last", '<<"ab]">>', module="binary")
    call(b, "guard:5", "last", "<<>>", module="binary")

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
        ("dead", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_missing_pid",
                  "Modules/Signals.lean#ended_process_is_forced"]),
        ("live", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_self"]),
        ("self", ["Modules/Process.lean#many_sends", "Modules/Process.lean#send_to_self"]),
        ("stale_alias", ["Modules/Process.lean#many_sends",
                         "Modules/Signals.lean#inactive_reference_is_forced"]),
    ]:
        for count in [0, 1, 30]:
            selected.append({"lean_origins": origins,
                             "expression": f"differential:many_sends({mode},{count})"})
    for value in ["child", "#{a=>1}", "-0.0"]:
        probe(p, ["successive_references_are_distinct", "references_are_unique_across_processes",
                  "dictionary_preserves_reference_counter", "dictionary_preserves_mailbox",
                  "suspended_processes_keep_their_dictionaries"], f"differential:process_exchange({value})")
    for mode, name in [("normal", "monitor_reports_normal_exit"), ("raised", "monitor_reports_raised_reason"),
                       ("noproc", "monitor_missing_process"), ("flush", "demonitor_flushes_down")]:
        probe("Modules/Signals", name, f"differential:monitor_exit({mode})")
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
                                          "monitor_reports_normal_exit",
                                          "inactive_reference_is_forced", "active_reference_is_not_forced"],
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
                    "inactive_reference_is_forced",
                    "active_reference_is_not_forced",
                    "alias_ends_with_demonitor",
                    "demonitor_flushes_down",
                ],
                f"differential:await_many_drain({scenario},{alias_mode})",
            )
    for mode in ["timeout", "message", "nested"]:
        probe(p, "timeout_handler_scope", f"differential:suspended_receive({mode})")

    for trap in ["true", "false"]:
        probe("Modules/Signals", ["link_to_missing_process", "ended_process_is_forced"],
              f"differential:missing_link({trap})")
    probe("Modules/Signals", "liveness", "differential:process_liveness()")
    for value in ["bad", "1", "[]", "{}", "make_ref()", "fun()->ok end"]:
        call("Modules/Signals", "liveness", "is_process_alive", value)
        call("Modules/Signals", "liveness", "process_info", value, "registered_name")
    for cls in ["error", "exit", "throw", "bad", "1"]:
        for stack in ["[]", "[{differential,call,3,[]}]", "[{differential,call,[a,b,c],[]}]",
                      "bad", "[bad]", "[{differential,call,3,[]}|bad]", "[{1,call,3,[]}]"]:
            probe("Modules/Signals", "raise_classes",
                  f"differential:raise_exception({cls},#{{reason=>[1,2,3]}},{stack})")
    for cls in ["error", "exit", "throw"]:
        for stack in ["[{differential,call,3}]", "[{differential,call,[a,b,c]}]",
                      "[{differential,call,bad}]", "[{fun erlang:abs/1,[-7]}]",
                      "[{fun erlang:abs/1,[-7],[]}]",
                      "[{differential,call,3},{fun erlang:abs/1,[-7]},"
                      "{differential,call,3,[{line,17}]}]",
                      "[{fun erlang:abs/1,[-7],[{line,17}|tail]}]"]:
            probe("Modules/Signals", "raise_classes",
                  f"differential:raise_exception({cls},#{{reason=>[1,2,3]}},{stack})")
    # Extend raise recipes to VM depth controls, which the model does not expose.
    for depth in ["default", "0", "1", "3", "8", "64", "65", "1 bsl 40"]:
        for count in [0, 1, 7, 8, 9, 65]:
            for style in ["{modern,error}", "{legacy,throw}"]:
                probe("Modules/Signals", "raise_classes",
                      f"differential:trace_depth({depth},{count},{style})")
        for style in ["generated", "{modern,exit}", "{invalid_suffix,error}",
                      "{improper,throw}", "{child,{legacy,exit}}"]:
            probe("Modules/Signals", "raise_classes",
                  f"differential:trace_depth({depth},80,{style})")
    for depth in ["-1", "1.0", "bad", "[]", "1 bsl 100"]:
        probe("Modules/Signals", "raise_classes",
              f"differential:trace_depth({depth},9,{{modern,error}})")
    for key, items in [("b", "[skipped,{}, {a,1},{b,first},{b,second}]"),
                       ("missing", "[skipped,{a,1}]"), ("1", "[{1.0,float},{1,integer}]"),
                       ("1.0", "[{1,integer},{1.0,float}]"), ("0.0", "[{-0.0,negative}]"),
                       ("{1}", "[{{1.0},nested}]"), ("b", "[{b,first}|improper]"),
                       ("missing", "[{a,1}|improper]"), ("b", "[]"), ("b", "bad")]:
        for position in ["1", "2", "0", "-1", "1.0", "1 bsl 100"]:
            for fn in ["keyfind", "keymember"]:
                call("Modules/Signals", "keyfind_finds_first_match", fn, key, position, items, module="lists")

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

    for kind in ["local_zero", "local_one", "local_two", "external_zero", "external_one",
                 "external_two", "bif_zero", "bif_one", "missing_export", "missing_module", "not_fun"]:
        for args in ["[]", "[7]", "[left,right]", "[7|improper]", "bad"]:
            probe(e, ["dynamic_apply_2", "dynamic_apply_captures", "dynamic_apply_2_errors"],
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
    for fun in ["fun differential:function_one/1", "fun(X)->X end", "not_fun"]:
        for arity in ["0", "1", "2", "255", "256", "-1", "1 bsl 100", "1.0"]:
            call(e, ["is_function/2_arity", "is_function/2_invalid_arity", "is_function/2_mismatch"],
                 "is_function", fun, arity)
    for kind in ["local", "external", "external_error", "external_throw", "external_exit",
                 "local_zero", "local_one", "external_one", "bif_zero", "bif_one",
                 "missing_export", "missing_module", "not_fun"]:
        for mode in ["monitored", "linked", "small_heap"]:
            probe("Term/Dispatch", ["spawned_dispatch", "spawn_exception_boundaries"],
                  f"differential:function_spawn({kind},{mode},#{{capture=>[1,2,3]}})")

    construction = "Term/Bitstring/Construction"
    matching = "Term/Bitstring/Matching"
    for encoding, bits in [
        ("utf8", ["<<>>", "<<128>>", "<<192,128>>", "<<237,160,128>>", "<<244,144,128,128>>",
                  "<<194,64:7>>", "<<97,255>>", "<<226,130,172>>", "<<240,159,152,128>>", "<<239,187,191>>"]),
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
                probe(matching, ["utf16_rejects_malformed" if encoding.startswith("utf16") else
                                 "utf32_rejects_malformed" if encoding.startswith("utf32") else "utf8_rejects_malformed",
                                 "utf_rejects_incomplete_final_code_unit", "utf_preserves_invalid_suffix",
                                 "utf_unaligned_with_partial_suffix"],
                      f"differential:utf({encoding},<<5:{offset},({value})/bitstring>>,{offset})")
            for codepoint in [0, 127, 128, 0xD7FF, 0xD800, 0xDFFF, 0xE000, 0xFEFF, 0x20AC,
                              0x1F600, 0x10FFFF, 0x110000, -1]:
                probe(construction, ["utf_boundaries", "utf_characters", "utf_invalid_scalars",
                                     "utf_unaligned_concatenation"],
                      f"differential:utf_construct({encoding},{codepoint},{offset})")

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
            "Modules/ErtsInternal.lean": "Compiler/model iterator adapters are not portable ERTS contracts.",
            "Modules/Process.lean": "Samples observe public process behavior, not all schedules, model PID paths, deadlock or exhaustion.",
            "Modules/Timers.lean": "Delivery/cancellation handshakes and OTP error paths; elapsed milliseconds and all-run model proofs are not cross-VM expectations.",
            "Modules/Signals.lean": "Samples normalize identities and error stack contents; signal/link/alias combinations remain incomplete.",
            "Term/Dispatch.lean": "Synthetic function tables, dispatcher fuel and missing-entry exhaustion have no direct VM equivalent.",
            "Term/Order.lean": "Fixed-point/admissibility proofs and divergence are not sampled as bounded successes.",
            "Term/Try.lean": "Model empty stack traces and exhaustion are not VM expectations; translation-fixture mode tests try behavior separately.",
            "Term/Maps.lean": "Internal duplicate-binding association lists and extraction sentinels are not public map representations.",
            "Term/Bitstring.lean": "Internal padding validity and proof objects are not observable VM heap contracts.",
        }
        rows.append({"source": source, "sha256": hashlib.sha256(raw).hexdigest(), "tests": tests,
                     "note": notes.get(source, "Finite public-behavior samples and invalid-input extensions, not universal theorem instances.")})
    attributed = {test for entry in selected for test in entry.get("lean_origins", [])}
    if attributed - known:
        raise ValueError(f"Lynx test attribution no longer exists: {sorted(attributed - known)}")
    return rows
