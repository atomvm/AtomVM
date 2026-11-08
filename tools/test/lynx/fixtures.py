# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Calls into Lynx's translation fixtures, including observable error paths."""

from generated import erlang, float_boundaries


def cases():
    selected = []

    def add(module, function, *args, setup="", observe=None):
        call = f"{module}:{function}({','.join(args)})"
        if observe:
            call = f"({observe})({call})"
        selected.append({"fixture": [module, function, len(args)], "expression": setup + call})

    terms = ["0", "-1", "1.0", "-0.0", "ok", "[]", "[a]", "[a|b]",
             "{}", "{1}", "#{}", "#{a=>1}", "<<>>", "<<1>>", "<<1:1>>"]
    numbers = ["0", "-1", "1", "1.5", "-0.0", "5.0e-324", "1 bsl 60", "1 bsl 100", "bad"]
    for function in ["integers", "floats", "bitstrings", "atoms", "empty_list", "tuples", "maps",
                     "table", "pooled_list", "pooled_map", "pooled_binary", "pooled_nested"]:
        add("literal", function)
    for value in terms:
        add("literal", "computed_tuples", value)
        add("mutual", "identity", value)
    for key in ["a", "1", "1.0", "0.0", "-0.0", "#{key=>1}", "<<5:3>>"]:
        add("literal", "computed_maps", key, "{nested,7}")
    for index in ["0", "1", "16", "31", "32", "33", "1.0", "bad"]:
        add("literal", "table_lookup", index)
    for value in ["[]", "[5,-12,19]", "[1.5,2]", "[bad]", "[1|2]", "bad",
                  "[1 bsl 60, -(1 bsl 60), 7]", "[1 bsl 100,3,-(1 bsl 100)]"]:
        add("sum", "sum", value)
    add("sum", "sum_empty_ensures")
    for function in ["sum_singleton_requires", "sum_singleton_ensures"]:
        for value in numbers:
            add("sum", function, value)
    for function in ["odd", "even", "first", "second"]:
        for value in ["[]", "[a]", "[a,b]", "[a,b,c]", "[a,b,c,d]", "[a|bad]", "bad"]:
            add("mutual", function, value)
    for args in [("[]", "[a]", "[x]"), ("[1]", "[]", "[x]"), ("[1]", "[a]", "[]"),
                 ("[1,2]", "[a,b,c]", "[x,y]"), ("[1]", "[a|bad]", "[x]")]:
        add("mutual", "triples", *args)

    for function in ["captured_closures", "apply_calls", "inc", "qualified_capture"]:
        for value in numbers:
            add("functions", function, value)
    for function in ["zero", "zero_arity_calls"]:
        add("functions", function)
    for value in ["-7", "1 bsl 60", "#{a=>9}"]:
        for function in ["sequence_calls", "remember"]:
            add("functions", function, value, setup="put(last,previous), ",
                observe="fun(R)->{R,get(last)} end")
        add("functions", "make_store", value,
            observe=f"fun(F)->{{F(first),F(second),get({value})}} end")
    for value in numbers:
        add("functions", "make", value, observe="fun(F)->{F(1),F(-3)} end")
    for callback, args in [("fun functions:inc/1", "[9]"), ("fun functions:zero/0", "[]"),
                           ("fun(X,Y)->X-Y end", "[7,2]"), ("bad", "[]"),
                           ("fun functions:inc/1", "bad")]:
        add("functions", "explicit", callback, args)
    for module in ["functions", "1"]:
        add("functions", "calls_by_name", module, "9")
    add("functions", "references", observe="fun({A,B,M})->{A,B,map_size(M)} end")

    for function in ["grouped", "floats"]:
        for value in terms + ["a", "b", "[c|7]", "[c]", "[c,x]", "-1.5", "0.0",
                              "{0.1,[5.0e-324]}", "#{1.0=>1.5}", "#{1=>1.5}"]:
            add("patterns", function, value)
    for value in ["#{a=>0,b=>3}", "#{a=>1,b=>3}", "#{a=>2,b=>3}", "#{a=>0,b=>no}",
                  "#{a=>2}", "#{b=>3}", "#{}", "bad"]:
        add("patterns", "map_grouped", value)
    for value in ["{tag,[#{a=>#{b=>7}}]}", "{tag,[#{a=>#{b=>no}}]}",
                  "{tag,[#{a=>#{}}]}", "{tag,[]}", "bad"]:
        add("patterns", "map_nested", value)
    for k1, k2, value in [("a", "a", "#{a=>{ok,9}}"), ("a", "b", "#{a=>7,b=>{ok,9}}"),
                          ("a", "b", "#{a=>7,b=>9}"), ("a", "b", "#{a=>7}"),
                          ("1", "1.0", "#{1=>7,1.0=>{ok,9}}"),
                          ("0.0", "-0.0", "#{0.0=>7,-0.0=>{ok,9}}"), ("a", "b", "bad")]:
        add("patterns", "map_duplicate", k1, k2, value)
    for key, value in [("0", "#{1=>7}"), ("0.0", "#{1=>7}"), ("0", "#{1.0=>7}"),
                       ("0.0", "#{1.0=>7}"), ("-1", "#{0=>7}"), ("bad", "#{}"), ("0", "bad")]:
        add("patterns", "map_computed", key, value)
    for value in ["#{b=>2,a=>-1}", "#{b=>1,a=>-1}", "#{a=>0}", "#{a=>3}", "#{}", "bad"]:
        add("patterns", "map_fallthrough", "result", value)

    for function in ["integer", "float", "atom", "tuple", "map", "bitstring", "binary",
                     "pid", "reference", "function"]:
        for value in terms + ["self()", "make_ref()", "fun functions:inc/1", "fun()->ok end"]:
            # Runtime identities cannot be compared across VMs.
            observe = f"fun(V)->{{V=:=no,is_{function}(V)}} end" if function in ["pid", "reference", "function"] else None
            add("guard_patterns", function, value, observe=observe)
    for function, values in {
        "conjunction": ["{1,#{}}", "{1.0,#{}}", "{1,bad}", "bad"],
        "nested": ["{tag,[#{a=><<1>>}]}", "{tag,[#{a=><<1:1>>}]}", "{tag,[]}", "bad"],
        "conflict": ["{1}", "{1.0}", "bad"],
        "repeated": ["{1,1}", "{1,1.0}", "{1,2}", "{1.0,1.0}", "bad"],
        "negated": terms,
        "chain": ["0", "1", "2", "3", "4", "1.0"],
        "names": ["ok", "error", "<<\"x\">>", "<<\"xy\">>", "1", "bad"],
        "numbers": ["0", "1", "1.0", "2", "2.0", "3", "bad"],
    }.items():
        for value in values:
            add("guard_patterns", function, value)
    for left, right in [("{1}", "2"), ("{1}", "#{}"), ("{1}", "bad"), ("{1.0}", "2"), ("bad", "2")]:
        add("guard_patterns", "disjunction", left, right)
    for left, right in [("0", "bad"), ("1", "[]"), ("[]", "not_boolean"),
                        ("1", "bad"), ("[]", "[x]"), ("bad", "[x]")]:
        add("guard_patterns", "short_circuit", left, right)
    for left, right in [("1", "{yes}"), ("1.0", "{yes}"), ("1", "bad")]:
        add("guard_patterns", "outer", left, right)

    for function in ["caught", "scope", "handler_scope", "selective_catch", "guarded_catch",
                     "reraised", "after_effect", "after_raises", "success_clause", "nested"]:
        for value in ["-1", "0", "1", "2", "1.5", "1 bsl 60", "bad"]:
            observe = None
            if function == "caught":
                observe = "fun({C,R,S}) when is_list(S)->{C,R,S=/=[]}; (V)->V end"
            elif function == "after_effect":
                observe = "fun(V)->{V,get(key)} end"
            add("tries", function, value, observe=observe)
    for callback in ["fun()->7 end", "fun()->throw(reason) end", "fun()->error(reason) end",
                     "fun()->exit(reason) end", "bad"]:
        add("tries", "dynamic", callback)
    add("tries", "effectful")
    for value in ["[]", "[1,-4,8]", "[1,bad]", "[1|bad]", "bad"]:
        for callback in ["fun functions:inc/1", "fun(X)->{X} end", "bad"]:
            add("higher_order", "map", value, callback)
        add("higher_order", "proper_list", value)
        for function in ["map_returns_list_requires", "map_returns_list_ensures"]:
            add("higher_order", function, value, "fun(X)->X end")

    for function, inputs in {
        "comprehension_filtered": [("[]",), ("[{a,1},{2,3},skip,{b,4}]",), ("[{a,1}|bad]",), ("bad",)],
        "comprehension_nested": [("tag", "[]"), ("tag", "[[1,2],[],[a]]"), ("tag", "[[1],bad]")],
        "comprehension_lists": [("[]", "[a]"), ("[1]", "[]"), ("[1,2]", "[a,b,c]"), ("[1]", "bad")],
        "comprehension_lists3": [("[1,2]", "[a]", "[x,y]"), ("[1]", "[]", "[x]"), ("[1]", "[a]", "bad")],
        "comprehension_mixed3": [("[0,2]", "#{a=>1,b=>3}", "[x,y]"), ("[]", "#{}", "[]"), ("[0]", "bad", "[x]")],
        "comprehension_zip": [("[]", "[]"), ("[1,2]", "[a,b]"), ("[1]", "[a,b]"), ("[1,2]", "[a]"), ("bad", "[a]")],
        "map_generator": [("#{}",), ("#{a=>1,b=>2}",), ("bad",)],
        "map_generator_strict": [("#{}",), ("#{a=>ok,b=>ok}",), ("#{a=>no}",), ("bad",)],
        "map_comprehension": [("[]",), ("[{a,1},{a,2},{b,3}]",), ("[skip,{a,1}]",), ("bad",)],
        "map_to_map": [("#{}",), ("#{a=>1,b=>2}",), ("bad",)],
        "comprehension_list_map": [("[1,2]", "#{a=>1,b=>2}"), ("[]", "#{}"), ("[1]", "bad")],
        "comprehension_map_list": [("#{a=>1,b=>2}", "[x,y]"), ("#{}", "[]"), ("bad", "[x]")],
        "comprehension_filtered_maps": [("[1,bad,2,3]", "#{1=>a,2=>b,3=>c}"), ("[]", "#{}"), ("[1]", "bad")],
        "binary_generator": [("<<>>",), ("<<0,255,1>>",), ("<<1,5:3>>",), ("bad",)],
        "bits_generator": [("<<>>",), ("<<7:3,1:2,2:3,3:2,1:1>>",), ("bad",)],
        "binary_generator_strict": [("<<>>",), ("<<1,2,3,4>>",), ("<<1,2,3>>",), ("bad",)],
        "utf8_generator": [("<<>>",), ("<<65,226,130,172,240,159,152,128>>",), ("<<65,255>>",), ("<<226,130>>",), ("bad",)],
        "sized_generator": [("1", "<<1,2,3>>"), ("2", "<<1,2,3,4,5>>"), ("0", "<<>>"), ("0", "<<1>>"), ("-1", "<<1>>")],
        "zero_generator": [("<<>>",), ("<<1>>",)],
        "binary_comprehension": [("[]",), ("[1,2,15,16,-1]",), ("[bad]",), ("bad",)],
        "binary_to_binary": [("<<>>",), ("<<1,2,3,4,5>>",), ("bad",)],
        "binary_nested": [("<<1,2>>", "<<3:4,4:4,5:4>>"), ("<<>>", "<<1>>"), ("<<1>>", "bad")],
        "binary_zip": [("<<>>", "<<>>"), ("<<1,2>>", "<<3:4,4:4>>"), ("<<1>>", "<<3:4,4:4>>"), ("<<1,2>>", "<<3:4>>"), ("bad", "<<>>")],
        "binary_list": [("<<1,2>>", "[a,b,c]"), ("<<>>", "[]"), ("<<1>>", "bad")],
        "named": [("tag", "[]"), ("tag", "[1,2,a]"), ("tag", "[1|bad]")],
        "self_reference": [()],
    }.items():
        for args in inputs:
            observe = "fun lists:sort/1" if function in ["map_generator", "map_generator_strict", "comprehension_mixed3", "comprehension_list_map", "comprehension_filtered_maps"] else None
            add("named_locals", function, *args, observe=observe)

    add("binary_construction", "empty")
    for function in ["integer_default", "integer_mixed", "integer_invalid_size"]:
        for value in ["0", "255", "256", "-257", "1 bsl 100", "1.5", "bad"]:
            add("binary_construction", function, value)
    for function in ["integer_big", "integer_signed_big", "integer_little", "integer_signed_little", "integer_unit", "integer_max_unit"]:
        sizes = ["0", "1", "2", "-1", "1.0", "bad"] if function == "integer_max_unit" else ["0", "1", "7", "8", "9", "12", "63", "64", "65", "127", "128", "129", "-1", "1.0", "bad"]
        for size in sizes:
            for value in ["0", "-257", "1 bsl 100", "1.5", "bad"]:
                add("binary_construction", function, value, size)
    for function in ["float_default", "float_big", "float_little", "float_unit", "float_mixed", "float16", "float32"]:
        for value in numbers + ["0.0", "1.7976931348623157e308", "6546.0", "1.0e-6"]:
            add("binary_construction", function, value)
    for value in ["1.5", "6546.0", "-0.0", "bad"]:
        for size in ["0", "8", "12", "16", "32", "-1", "1.0", "bad"]:
            add("binary_construction", "float_sized", value, size)
    # Independently decoded encoding bytes are covered by differential:float_bits.
    # Here, reuse its boundary values to exercise Lynx's actual static opcodes too.
    for width in [16, 32]:
        values = {erlang(value) for value, bits, _offset, _endian in float_boundaries() if bits == width}
        for value in sorted(values):
            add("binary_construction", f"float{width}", value)
    for function in ["binary_all", "bitstring_all"]:
        for value in ["<<>>", "<<1,2>>", "<<5:3>>", "bad", "[1]"]:
            add("binary_construction", function, value)
    for function in ["binary_prefix", "bitstring_unit"]:
        for value in ["<<>>", "<<1,2>>", "<<1,2,5:3>>", "bad"]:
            for size in ["0", "1", "2", "3", "6", "-1", "1.0"]:
                add("binary_construction", function, value, size)
    for left, right in [("<<>>", "<<>>"), ("<<5:3>>", "<<1,2>>"), ("<<1>>", "<<1:1>>"), ("bad", "<<>>")]:
        add("binary_construction", "binary_mixed", left, right)

    codepoints = ["0", "16#7f", "16#80", "16#7ff", "16#800", "16#d7ff", "16#d800",
                  "16#dfff", "16#e000", "16#ffff", "16#10000", "16#10ffff", "16#110000", "-1", "1.0", "bad"]
    for function in ["utf8", "utf8_little", "utf16", "utf16_big", "utf16_little", "utf32", "utf32_big", "utf32_little", "utf_mixed"]:
        for value in codepoints:
            add("binary_construction", function, value)
    for function, values in {
        "empty": ["<<>>", "<<0>>", "bad"],
        "empty_or_byte": ["<<>>", "<<1,2>>", "<<1,5:3>>", "bad"],
        "literal": ["<<1,2,5:3>>", "<<1,2,4:3>>", "<<1,2,5:3,0:1>>"],
        "partial_literal": ["<<5:3>>", "<<4:3>>", "<<5:8>>"],
        "binary_literal": ["<<\"abc\">>", "<<\"abd\">>", "<<\"abc\",0>>"],
        "string_literal": ["<<\"abc\">>", "<<\"abd\">>", "<<\"abc\",0>>"],
        "signed_literal": ["<<-1:8/signed,16#123:24/little>>", "<<-1:8/signed,16#123:24/big>>", "<<255>>"],
        "integers": ["<<200:8,-257:12/signed-big,1023:12/signed-little>>", "<<255:8,-2048:12/signed-big,2047:12/signed-little>>", "<<0:31>>", "<<0:33>>", "bad"],
        "integer_sized": ["<<0,5:3>>", "<<12,-257:12/signed-little,5:3>>", "<<65,-1:65/signed-little,7:3>>", "<<12,0>>", "bad"],
        "binary_tail": ["<<3:3,1,2>>", "<<3:3,5:3>>", "<<>>", "bad"],
        "bitstring_tail": ["<<3:3,1,2>>", "<<3:3,5:3>>", "<<3:3>>", "<<1:1>>"],
        "binary_literal_tail": ["<<1:3,1,2>>", "<<1:3,5:3>>", "<<2:3,1,2>>"],
        "bitstring_literal_tail": ["<<1:3,1,2>>", "<<1:3,5:3>>", "<<2:3>>"],
        "nested": ["{tag,[#{bits=><<1,5:3>>}]}", "{tag,[#{bits=><<5:3>>}]}", "{tag,[#{}]}", "bad"],
        "guarded": ["<<10,1,2>>", "<<11,1,2>>", "<<11,5:3>>", "<<5:3>>"],
        "alias": ["<<1,2>>", "<<1,5:3>>", "<<5:3>>", "bad"],
    }.items():
        for value in values:
            add("bitstring_matching", function, value)
    for function in ["integer_external_size", "binary_fixed", "bitstring_fixed", "binary_prefix", "float_sized"]:
        for size in ["0", "1", "2", "3", "8", "16", "32", "-1", "1.0", "bad"]:
            for value in ["<<>>", "<<3:3,1,2>>", "<<1,2,5:3>>", "<<1.5:64/float,5:3>>", "bad"]:
                add("bitstring_matching", function, size, value)
    for value in ["0", "1", "255", "256", "-1", "1.0", "bad"]:
        add("bitstring_matching", "pinned", value, "<<1,5:3>>")
    for function, width in [("float16", 16), ("float32", 32), ("float64", 64)]:
        for value in ["0.0", "-0.0", "1.5", "5.0e-324", "1.0e-6", "6546.0"]:
            bits = f"<<{value}:{width}/float-big,{value}:{width}/float-little>>" if width != 16 else f"<<{value}:16/float,5:3>>"
            add("bitstring_matching", function, bits)
        for bits in ["<<>>", "<<1:1>>", "bad"]:
            add("bitstring_matching", function, bits)
        # No host float terms for infinities/NaNs: supply the raw wire patterns.
        for encoding in {16: [0x0001, 0x03ff, 0x0400, 0x7bff, 0x7c00, 0xfc00, 0x7e00],
                         32: [0x00000001, 0x007fffff, 0x00800000, 0x7f7fffff, 0x7f800000, 0xff800000, 0x7fc00000],
                         64: [1, 0x000fffffffffffff, 0x0010000000000000, 0x7fefffffffffffff, 0x7ff0000000000000, 0xfff0000000000000, 0x7ff8000000000000]}[width]:
            bits = f"<<{encoding}:{width}/big,{encoding}:{width}/little>>" if width != 16 else f"<<{encoding}:16,5:3>>"
            add("bitstring_matching", function, bits)
    for function, encoding in [("utf8", "utf8"), ("utf8_little", "utf8"), ("utf16", "utf16-big"),
                               ("utf16_big", "utf16-big"), ("utf16_little", "utf16-little"),
                               ("utf32", "utf32-big"), ("utf32_big", "utf32-big"), ("utf32_little", "utf32-little")]:
        for value in codepoints[:12]:
            if value not in ["16#d800", "16#dfff"]:
                add("bitstring_matching", function, f"<<{value}/{encoding},5:3>>")
        for bits in ["<<>>", "<<255>>", "<<16#d800:16>>", "<<16#110000:32>>", "<<1:1>>", "bad"]:
            add("bitstring_matching", function, bits)
    for bits in ["<<16#c0,16#80>>", "<<16#ed,16#a0,16#80>>", "<<16#f4,16#90,16#80,16#80>>", "<<226,130>>"]:
        add("bitstring_matching", "utf8", bits)
    for bits in ["<<3:3,16#20ac/utf8,16#1f600/utf16-little,16#10ffff/utf32-big,5:3>>", "<<3:3,255>>", "bad"]:
        add("bitstring_matching", "utf_mixed", bits)
    for bits in ["<<16#20ac/utf8,16#1f600/utf16-little,16#10ffff/utf32-big>>", "<<0>>"]:
        add("bitstring_matching", "utf_literal", bits)
    for value in ["16#20ac", "65", "1.0", "bad"]:
        add("bitstring_matching", "utf_pinned", value, "<<16#20ac/utf8,16#20ac/utf16-big,16#20ac/utf32-little>>")

    for function, args, messages in [
        ("plain", (), ["first", "second"]),
        ("selective", ("tag",), ["ignored", "{tag,9}", "{tag,10}"]),
        ("selective", ("tag",), ["stop"]),
        ("guarded", ("tag",), ["{tag,<<1>>}"]),
        ("guarded", ("tag",), ["{tag,7}"]),
        ("guarded", ("tag",), ["{tag,0}"]),
        ("guarded", ("tag",), ["{tag,<<1:1>>}"]),
        ("guarded", ("tag",), ["stop"]),
        ("self_guard", (), ["{not_self,1}", "{self(),9}"]),
        ("poll", (), []), ("poll", (), ["ignored", "{ok,9}"]),
        ("nested", (), ["{ok,tag}", "tag"]), ("nested", (), ["{ok,tag}"]),
        ("nested", (), ["stop"]),
        ("bindings", (), ["ignored", "{pair,1,2}"]), ("bindings", (), ["stop"]),
        ("repeated", (), ["{1,2}", "{3,3}"]), ("repeated", (), ["{1,1.0}"]),
        ("repeated", (), ["{0.0,-0.0}"]),
        ("alias", (), ["{pair,1,2}"]),
        ("lists", (), ["[1]"]), ("lists", (), ["[1,2,3]"]), ("lists", (), ["[1,2|tail]"]),
        ("alternatives", (), ["0", "-1"]), ("alternatives", (), ["0", "1"]),
        ("alternatives", (), ["bad"]), ("sequence", (), []),
    ]:
        # Check the remaining mailbox too: matching the wrong message can
        # otherwise be invisible when several messages have the same value.
        setup = "".join(f"self()!{message}, " for message in messages)
        add("receives", function, *args, setup=setup,
            observe="fun(R)->Drain=fun Loop()->receive V->[V|Loop()] after 0->[] end end, {R,Drain()} end")
    for function in ["timed", "sleep", "computed"]:
        for timeout in ["0", "1", "-1", "1.0", "bad"]:
            add("receives", function, timeout)
        if function != "sleep":
            message = "{ok,9}" if function == "timed" else "message"
            for timeout in ["0", "infinity", "bad"]:
                # computed(infinity) raises badarith rather than blocking.
                add("receives", function, timeout, setup=f"self()!{message}, ")

    # Compare zeros that crossed a module boundary, not compiler-folded terms.
    selected.append({
        "fixture": ["literal", "computed_tuples", 1],
        "expression": "P=element(2,literal:computed_tuples(0.0)), "
                      "N=element(2,literal:computed_tuples(-0.0)), "
                      "{P==N,P=:=N,map_size(#{P=>positive,N=>negative}),"
                      "#{#{value=>P}=>key}==#{#{value=>N}=>key}}",
    })
    # Preserve first occurrence order while avoiding duplicate boundary calls.
    return list({entry["expression"]: entry for entry in selected}.values())
