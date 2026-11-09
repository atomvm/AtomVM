# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Finite public-API samples inspired by LynxTest, not translations of proofs."""

import hashlib
import re


def cases():
    selected = []

    def probe(source, tests, expression):
        if isinstance(tests, str):
            tests = [tests]
        selected.append({"lean_origins": [f"{source}.lean#{test}" for test in tests],
                         "expression": expression})

    def call(source, tests, function, *args, module="erlang"):
        probe(source, tests, f"differential:call({module},'{function}',[{','.join(args)}])")

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
                  "[1|2]", "[-1]", "[256]", "[<<1:1>>]", "<<1:1>>", "bad"]:
        call(c, "iodata_total_and_exact", "iolist_to_binary", value)
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

    for source, names, pairs in [
        ("Term/Compare", ["numeric_and_exact_comparison", "signed_zero_comparison"],
         [("1", "1.0"), ("0.0", "-0.0"), ("-0.0", "0.0"), ("9007199254740993", "9007199254740992.0")]),
        ("Term/Compare", ["comparison_short_circuit", "comparison_keys_before_values", "nested_map_order"],
         [("{a,b}", "{z}"), ("#{a=>99,b=>0}", "#{a=>0,c=>0}"),
          ("#{1=>[]}", "#{1.0=>[]}"), ("[0|a]", "[0]"),
          ("#{#{a=>1}=>2}", "#{#{a=>2}=>1}")]),
        ("Term/Compare", ["binary_exact_kinds", "list_exact_kinds", "binary_exact_bytes"],
         [("<<>>", "[]"), ("<<128>>", "<<1:1>>"), ("[1|a]", "[1|b]"), ("<<1,2>>", "<<1,3>>")]),
        (e, ["tuple_equality", "map_equality", "float_equality_guards", "reflexive_operators"],
         [("{}", "{}"), ("{1}", "{1,[]}"), ("{1,a}", "{a,1}"),
          ("#{a=>1,b=>2}", "#{b=>2,a=>1}"), ("#{a=>1}", "#{a=>2}"), ("1.0", "1.0")]),
    ]:
        for a, b in pairs:
            probe(source, names, f"differential:compare({a},{b})")
            probe(source, names, f"differential:compare({b},{a})")

    s = "Term/ShortCircuit"
    for op, names in [("or_else", ["orElse_short_circuits", "orElse_returns_right", "orElse_rejects_non_boolean",
                                    "orElse_keeps_left_requests"]),
                      ("and_also", ["andAlso_short_circuits", "andAlso_returns_right", "andAlso_rejects_non_boolean"])]:
        for left in ["true", "false", "1", "bad"]:
            for right in ["value", "throw"]:
                probe(s, names, f"differential:short_circuit({op},{left},{right})")

    p = "Modules/Process"
    for timeout in ["-1", "1.0", "bad", "4294967296", "0", "1", "infinity"]:
        for messages in ["[before,wanted,after_message]", "[before]"]:
            if timeout in ["4294967296", "infinity"] and messages == "[before]":
                continue  # No deliberate long/infinite wait without a matching message.
            probe(p, ["timeout_validation_precedes_scan", "zero_timeout_scans_mailbox",
                      "zero_timeout_preserves_unmatched", "queued_match_precedes_timeout_choice"],
                  f"differential:receive_timeout({timeout},{messages})")
    for value in ["child", "#{a=>1}", "-0.0"]:
        probe(p, ["successive_references_are_distinct", "references_are_unique_across_processes",
                  "dictionary_preserves_reference_counter", "dictionary_preserves_mailbox",
                  "suspended_processes_keep_their_dictionaries"], f"differential:process_exchange({value})")
    for mode, name in [("normal", "monitor_reports_normal_exit"), ("raised", "monitor_reports_raised_reason"),
                       ("noproc", "monitor_missing_process"), ("flush", "demonitor_flushes_down")]:
        probe("Modules/Signals", name, f"differential:monitor_exit({mode})")

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
    return selected


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
            match = re.match(r"^(?:private\s+)?theorem\s+(«[^»]+»|[\w'.]+)", text)
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
