# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Public CLI integration tests; requires a built AtomVM and host OTP/Elixir."""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import lean_runtime


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = Path(__file__).with_name("validate.py")


@unittest.skipUnless((ROOT / "build/src/AtomVM").is_file(), "Build AtomVM first")
class ValidationTests(unittest.TestCase):
    def invoke(self, output, *args):
        result = subprocess.run(
            [sys.executable, str(SCRIPT), "--build", str(ROOT / "build"),
             "--output", str(output), *map(str, args)],
            capture_output=True, text=True, timeout=900,
        )
        self.assertTrue((output / "summary.json").is_file(), result.stdout + result.stderr)
        summary = json.loads((output / "summary.json").read_text())
        self.assertEqual(result.returncode,
                         int(summary["failed"] > 0 or summary["unverified"] > 0
                             or (summary["uncovered"] > 0 and "--replay" not in args)),
                         result.stdout + result.stderr)
        return summary

    def test_seed_and_replay_preserve_inputs_and_outcomes(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            first = work / "first"
            summary = self.invoke(first, "--seed", 42, "--generated", 32)
            again = work / "again"
            self.assertEqual(self.invoke(again, "--seed", 42, "--generated", 32), summary)
            self.assertEqual((first / "cases.json").read_text(), (again / "cases.json").read_text())
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", first / "cases.json"), summary)
            self.assertEqual((first / "cases.json").read_text(), (replay / "cases.json").read_text())
            # OTP hash-map iteration can change ETF byte order across VMs.
            # Check the decoded oracle terms, not incidental serialization order.
            check = subprocess.run([
                "elixir", "-e", """
                [a, b] = System.argv()
                read = fn p -> JSON.decode!(File.read!(p))["expected"] end
                decode = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
                true = Enum.all?(Enum.zip(read.(a), read.(b)), fn {x,y} -> decode.(x) === decode.(y) end)
                """, str(first / "manifest.json"), str(replay / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            other = work / "other"
            self.invoke(other, "--seed", 43, "--generated", 32)
            self.assertNotEqual((first / "cases.json").read_text(), (other / "cases.json").read_text())
            self.assertEqual(summary["passed"] + summary["failed"], 32)
            self.assertEqual(summary["laws"], 0)
            selected = json.loads((first / "cases.json").read_text())
            self.assertEqual({entry["operation"] for entry in selected},
                             {"compare", "maps", "bits", "arithmetic", "float_bits",
                              "map_sequence", "dictionary_sequence", "receive_sequence",
                              "binary_sequence", "lifecycle_sequence"})
            self.assertEqual(summary["passed"], 32)

    def test_timer_and_subtraction_observations_have_independent_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([{"expression": e} for e in [
                "differential:call(erlang,'--',[[1,2,1,3],[1,3]])",
                "differential:call(erlang,'--',[[1,1.0,1],[1.0,1]])",
                "differential:call(erlang,'--',[[{1},#{a=>1}],[{1.0},#{a=>1.0}]])",
                "differential:call(erlang,'--',[[a|tail],[]])",
                "differential:timer(deliver,0,{nested,7})",
                "differential:timer(deliver,1,<<5:3>>)",
                "differential:timer(cancel,60000,tick)",
                "differential:timer(cancel,0,tick)",
                "differential:timer(expired,0,tick)",
            ]]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 9, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Expectations are handwritten, not derived from the helper. Both
            # cancel outcomes must preserve unrelated mailbox contents, while
            # a completed timer must be absent on repeated cancellation.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, [2, 1]},
                  {:ok, [1]},
                  {:ok, [{1}, %{a: 1}]},
                  {:exception, :error, :badarg},
                  {:ok, {true, {:nested, 7}, [:retained]}},
                  {:ok, {true, <<5::3>>, [:retained]}},
                  {:ok, {true, true, false, [:retained]}},
                  {:ok, {true, true, false, [:retained]}},
                  {:ok, {:tick, false, false}}
                ] = values
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_send_after_options_have_independent_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            selected = [entry for entry in lean_runtime.cases()
                        if entry.get("otp_extension") == "send_after/4"]
            self.assertTrue(all("lean_origins" not in entry for entry in selected))
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            summary = self.invoke(output, "--replay", source)
            # Missing /4 support stays a visible failure, not an allowlisted pass.
            # This check remains valid once AtomVM implements the API.
            self.assertEqual(summary["passed"] + summary["failed"], 52)
            self.assertEqual(summary["unverified"], 0)
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                delivered = {:ok, {true, {:nested, %{1 => :one, 1.0 => :float}, <<5::3>>},
                                   true, false, false, [:retained]}}
                canceled = {:ok, {true, true, false, [:retained]}}
                # Four relative option forms; three absolute option forms.
                # Past/current deadlines deliver; cancel races have one normalized
                # valid result. Invalid entries must raise badarg, not undef.
                expected = List.duplicate([delivered, delivered, canceled], 4)
                           |> List.flatten()
                expected = expected ++ List.flatten(List.duplicate(
                  [delivered, delivered, delivered, delivered, canceled, canceled, canceled], 3))
                expected = expected ++ List.duplicate({:exception, :error, :badarg}, 19)
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_map_and_list_relation_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        upstream_sort = [c for c in all_cases
                         if c.get("upstream_example") == "examples/otp/lists.exs#sort_contract_holds"]
        upstream_seq = [c for c in all_cases
                        if c.get("upstream_example") == "examples/otp/lists.exs#sequence_formula"]
        self.assertGreater(len(upstream_sort), 0)
        self.assertGreater(len(upstream_seq), 0)
        self.assertTrue(all("lean_origins" not in c and not c.get("assert_true")
                            for c in upstream_sort + upstream_seq))
        model_relations = [c for c in all_cases
                           if any(c["expression"].startswith(prefix) for prefix in (
                               "differential:map_relations(",
                               "differential:map_singleton_iteration(",
                               "differential:map_guards(",
                               "differential:list_relations(",
                           )) and "upstream_example" not in c]
        self.assertGreater(len(model_relations), 0)
        self.assertTrue(all(c.get("lean_origins") and not c.get("assert_true")
                            for c in model_relations))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:map_relations([{a,1},{a,2},{1,10}],[{a,2},{1,10.0}],[a,1,1.0,missing])",
                "differential:map_singleton_iteration(k,42)",
                "differential:map_guards(a,#{a=>wanted,b=>2})",
                "differential:map_guards(a,[])",
                "differential:list_relations([3,1,2,1],[1,4],tail,[1,2,3,4,9])",
                "differential:seq_contract(1,7,3,[0,1,2,3])",
                "differential:seq_contract(5,4,1,[0])",
                "differential:call(lists,seq,[5,2,2])",
                "differential:call(lists,member,[a,[a|improper]])",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "relations"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 9, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                hit = fn k, v ->
                  {k, {:lookup, {:ok, v}, {:ok, v}, {:ok, v}, v, true, true},
                   {:put_rel, true, true}, {:update_rel, {:ok, true, true}},
                   {:remove_take_rel, true, {:ok, true, true}},
                   {:merge_rel, true, true, true, true, true}}
                end
                miss = fn k ->
                  {k, {:lookup, :error, {:exception, :error, {:badkey, k}},
                       {:exception, :error, {:badkey, k}}, :"$default", false, false},
                   {:put_rel, true, true},
                   {:update_rel, {:exception, :error, {:badkey, k}}},
                   {:remove_take_rel, true, {:error, true}},
                   {:merge_rel, true, true, true, true, true}}
                end
                expected_map = {:ok, {
                  {:from_list_eq_fold_put, true, true},
                  {:sizes, 2, 2, 2, 2, 1},
                  {:equality_vs_lookup, {false, false, true, true, true, true}},
                  {:compare, {true, false, false, true, false, true, false, true}},
                  {:entries, {2, %{:a => 2, 1 => 10}}, {2, %{:a => 2, 1 => 10.0}}},
                  {:per_key, [hit.(:a, 2), hit.(1, 10), miss.(1.0), miss.(:missing)]}
                }}
                expected_list = {:ok, {
                  {:lengths, 4, 2, 6, 4},
                  {:reverse_laws, true, true, true},
                  {:subtract_laws, true, true, true, true, [4, 1]},
                  {:sort_laws, [1, 1, 2, 3], true, true, true, true},
                  {:probes, [
                    {1, true, true, true, 2, true, true, true, true, 2, true},
                    {2, true, true, true, 1, true, true, true, true, 1, true},
                    {3, true, true, true, 1, true, true, true, true, 1, true},
                    {4, false, false, true, 0, true, true, true, true, 0, true},
                    {9, false, false, true, 0, true, true, true, true, 0, true}
                  ]}
                }}
                expected = [
                  expected_map,
                  {:ok, {true, true, true, true, :none, {0, %{}}, [k: 42]}},
                  {:ok, {{:map, 2}, :has_key, :nonempty, :matched_wanted,
                         {:ok, true}, {:ok, 2}, {:ok, true}, {:ok, :wanted}}},
                  {:ok, {:not_map, :no_key, :empty_or_not_map, :rejected,
                         {:ok, false}, {:exception, :error, {:badmap, []}},
                         {:exception, :error, {:badmap, []}},
                         {:exception, :error, {:badmap, []}}}},
                  expected_list,
                  {:ok, {[1, 4, 7], 3, 3, true, :not_applicable,
                         [{0, 1, true}, {1, 4, true}, {2, 7, true},
                          {3, :out_of_range, {:exception, :error, :function_clause}}]}},
                  {:ok, {[], 0, 0, true, true,
                         [{0, :out_of_range, {:exception, :error, :function_clause}}]}},
                  {:exception, :error, :badarg},
                  {:ok, true}
                ]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_binary_sequences_rebase_scopes_and_preserve_split_edges(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            short = {"bits": [0x6162, 16]}
            long = {"bits": [0x616263, 24]}
            patterns = [short, long]
            scope = {"tuple": ["scope", {"tuple": [3, 3]}]}
            negative = {"tuple": ["scope", {"tuple": [6, -3]}]}
            zero = {"tuple": ["scope", {"tuple": [0, 0]}]}
            steps = [
                ["match", patterns, []], ["match", patterns[::-1], [negative]],
                ["split", patterns, ["global", scope]],
                ["split", patterns, [negative, "global"]],
                ["match", patterns, [zero, scope]],
                ["match", patterns, [scope, zero]],
                ["match", patterns, [{"tuple": ["scope", {"tuple": [3, 2]}]}]],
                ["part", 1, 5], ["match", long, []],
                ["part", 5, -3], ["match", patterns, []],
                ["append", [97, 98]], ["split", patterns, ["global"]],
                ["split", patterns, ["global", "trim"]],
                ["part", -1, 1],
                ["match", patterns, [{"tuple": ["scope", {"tuple": [0, 6]}]}]],
                ["part", 5, 0], ["match", short, [zero]],
                ["split", short, ["global"]],
                ["split", short, ["global", "trim_all"]],
            ]
            trims = [
                ["split", {"bits": [124, 8]}, ["global", "trim"]],
                ["split", {"bits": [124, 8]}, ["global", "trim_all"]],
                ["split", {"bits": [124, 8]},
                 ["global", {"tuple": ["scope", {"tuple": [1, 4]}]}]],
            ]
            source.write_text(json.dumps([
                {"operation": "binary_sequence", "args": [list(subject),
                    [{"tuple": step} for step in operations]]}
                for subject, operations in [(b"zababcab", steps), (b"|a||b|", trims)]
            ]))
            output = work / "binary"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [first, second] = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                state = fn b -> {byte_size(b), b} end
                row = fn value, b -> {{:ok, value}, state.(b)} end
                original = "zababcab"
                expected = [row.({1,2}, original), row.({3,3}, original),
                  row.(["zab", "ab"], original), row.(["zab", "ab"], original),
                  row.({3,3}, original), row.(:nomatch, original), row.({3,2}, original),
                  row.("ababc", "ababc"), row.({2,3}, "ababc"),
                  row.("abc", "abc"), row.({0,3}, "abc"),
                  row.("abcab", "abcab"), row.(["", "", ""], "abcab"),
                  row.([], "abcab"),
                  {{:exception, :error, :badarg}, state.("abcab")},
                  {{:exception, :error, :badarg}, state.("abcab")},
                  row.("", ""), row.(:nomatch, ""), row.([""], ""), row.([], "")]
                true = first === {:ok, {state.(original), expected}}
                expected_trims = [row.(["", "a", "", "b"], "|a||b|"),
                  row.(["a", "b"], "|a||b|"), row.(["|a", "", "b|"], "|a||b|")]
                true = second === {:ok, {state.("|a||b|"), expected_trims}}
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_receive_sequences_preserve_order_and_resume_after_misses(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            tag_float = {"tuple": ["tag", 1.0]}
            tag_int = {"tuple": ["tag", 1]}
            tag_wrong = {"tuple": ["tag", "wrong"]}
            tag_wanted = {"tuple": ["tag", "wanted"]}
            empty_tuple, empty_map = {"tuple": []}, {"map": []}
            bits = {"bits": [5, 3]}
            inputs = [
                (["sentinel", 1.0, 1, tag_float, tag_int], [
                    ["exact", 1], ["exact", 1], ["send", 1], ["tag", "tag"],
                    ["exact", 1.0], ["exact", 1], ["any"],
                ]),
                ([0, "bad", empty_tuple, empty_map, tag_wrong, tag_wanted,
                  5, 4, "fallback", "wanted"], [
                    ["guard", "tuple"], ["guard", "division"],
                    ["clauses", "wanted"], ["clauses", "wanted"],
                    ["guard", "map"], ["any"],
                ]),
                ([], [["any"], ["exact", "missing"], ["send", bits],
                      ["exact", bits], ["any"]]),
            ]
            source.write_text(json.dumps([
                {"operation": "receive_sequence",
                 "args": [setup, [{"tuple": step} for step in steps]]}
                for setup, steps in inputs
            ]))
            output = work / "receives"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 3, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                wrap = fn rows -> {:ok, Enum.map(rows, &{:ok, &1})} end
                a = [:sentinel, 1.0, {:tag, 1.0}, {:tag, 1}]
                b = a ++ [1]
                c = [:sentinel, 1.0, {:tag, 1}, 1]
                d = [:sentinel, {:tag, 1}, 1]
                e = [:sentinel, {:tag, 1}]
                first = wrap.([
                  {:none, [:sentinel, 1.0, 1, {:tag, 1.0}, {:tag, 1}]},
                  {{:matched, 1}, a}, {:timeout, a}, {{:sent, 1}, b},
                  {{:matched, {:tag, 1.0}}, c}, {{:matched, 1.0}, d},
                  {{:matched, 1}, e}, {{:matched, :sentinel}, [{:tag, 1}]}])
                f = [0, :bad, {}, %{}, {:tag, :wrong}, 5, 4, :fallback, :wanted]
                g = [0, :bad, {}, %{}, {:tag, :wrong}, 5, :fallback, :wanted]
                h = [0, :bad, {}, %{}, {:tag, :wrong}, 5, :wanted]
                i = [0, :bad, {}, %{}, {:tag, :wrong}, 5]
                second = wrap.([
                  {:none, [0, :bad, {}, %{}, {:tag, :wrong}, {:tag, :wanted},
                           5, 4, :fallback, :wanted]},
                  {{:matched, {:tag, :wanted}}, f}, {{:matched, 4}, g},
                  {{:matched, :fallback}, h}, {{:matched, :wanted}, i},
                  {:timeout, i}, {{:matched, 0}, [:bad, {}, %{}, {:tag, :wrong}, 5]}])
                bits = <<5::3>>
                third = wrap.([{:none, []}, {:timeout, []}, {:timeout, []},
                               {{:sent, bits}, [bits]}, {{:matched, bits}, []}, {:timeout, []}])
                true = values === [first, second, third]
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_sequences_return_intermediate_results_and_exact_state(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            setup = [{"tuple": ["sentinel", "safe"]}]
            steps = [{"tuple": s} for s in [
                ["put", 1, "first"], ["put", 1.0, "second"],
                ["put", 1, "replaced"], ["erase", 1], ["get", 1.0],
                ["put", 1, "undefined"], ["selected", "undefined"],
                ["clear"], ["get", 1],
            ]]
            map_steps = steps[:7] + [{"tuple": s} for s in [
                ["update", "missing", "bad"], ["take", 1.0],
                ["take", 1.0], ["update", 1, "updated"],
            ]] + steps[7:]
            source.write_text(json.dumps([
                {"operation": "dictionary_sequence", "args": [setup, steps]},
                {"operation": "map_sequence", "args": [setup, map_steps]},
            ]))
            output = work / "sequences"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Hand-written expectations catch tracing mistakes even if both
            # runtimes agree on an incorrectly implemented probe.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [dictionary, map] = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                s0 = %{sentinel: :safe}
                s1 = %{1 => :first, :sentinel => :safe}
                s2 = Map.put(s1, 1.0, :second)
                s3 = %{s2 | 1 => :replaced}
                s4 = %{1.0 => :second, :sentinel => :safe}
                s5 = Map.put(s4, 1, :undefined)
                ds = fn m -> {map_size(m), m, map_size(m),
                              Map.new(Map.keys(m), &{&1, :present})} end
                ms = fn m -> {map_size(m), m} end
                expected_d = {{:ok, :undefined}, ds.(s1)}
                {:ok, {initial_d, trace_d}} = dictionary
                true = initial_d === ds.(s0)
                true = trace_d === [expected_d,
                  {{:ok, :undefined}, ds.(s2)}, {{:ok, :first}, ds.(s3)},
                  {{:ok, :replaced}, ds.(s4)}, {{:ok, :second}, ds.(s4)},
                  {{:ok, :undefined}, ds.(s5)},
                  {{:ok, {1, %{1 => :present}}}, ds.(s5)},
                  {{:ok, {3, s5}}, ds.(%{})}, {{:ok, :undefined}, ds.(%{})}]
                s6 = %{1 => :undefined, :sentinel => :safe}
                s7 = %{s6 | 1 => :updated}
                {:ok, {initial_m, trace_m}} = map
                true = initial_m === ms.(s0)
                true = trace_m === [{{:ok, s1}, ms.(s1)}, {{:ok, s2}, ms.(s2)},
                  {{:ok, s3}, ms.(s3)}, {{:ok, s4}, ms.(s4)},
                  {{:ok, {:ok, :second}}, ms.(s4)}, {{:ok, s5}, ms.(s5)},
                  {{:ok, {1, %{1 => :present}}}, ms.(s5)},
                  {{:exception, :error, {:badkey, :missing}}, ms.(s5)},
                  {{:ok, {:second, s6}}, ms.(s6)}, {{:ok, :error}, ms.(s6)},
                  {{:ok, s7}, ms.(s7)}, {{:ok, s7}, ms.(%{})},
                  {{:ok, :error}, ms.(%{})}]
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_float_encoding_smoke_cases_have_known_byte_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "floats"
            summary = self.invoke(output, "--float-encoding", "--generated", 6)
            self.assertEqual(summary["passed"] + summary["failed"], 6)
            selected = json.loads((output / "cases.json").read_text())
            self.assertEqual({entry["operation"] for entry in selected}, {"float_bits"})
            # Independent IEEE encodings: even tie, subnormal, near tie,
            # float32 even tie, underflow tie, and odd tie. Compare bytes,
            # not round-tripped floats (which cannot represent infinity in OTP).
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                encodings = Enum.map(values, fn value ->
                  {:ok, {_bits, encoded}} = :erlang.binary_to_term(Base.decode64!(value))
                  Base.encode16(encoded)
                end)
                ["6E64", "1100", "656E", "3F800000", "00000000", "3F800002"] = encodings
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_execution_paths_preserve_results_errors_and_guard_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:execution_paths('+',-7,3)",
                "differential:execution_paths('+',-7,7)",
                "differential:execution_paths('div',-7,3)",
                "differential:execution_paths('rem',-7,3)",
                "differential:execution_paths('/',0,0)",
                "differential:execution_paths('+',bad,1)",
                "differential:execution_paths('<',9007199254740993,9007199254740992.0)",
                "differential:execution_paths('<',9007199254740992.0,9007199254740993)",
                "differential:execution_paths('=:=',1,1.0)",
                "differential:execution_paths('+',-0.0,-0.0)",
                "differential:execution_paths(map_get,1,#{1=>wanted,1.0=>other})",
                "differential:execution_paths(map_get,1.0,#{1=>wanted,1.0=>other})",
                "differential:execution_paths(map_get,1.0,#{1=>wanted})",
                "differential:execution_paths(map_get,key,bad)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "paths"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 14, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                paths = fn value, guard -> {:ok,
                  [body: value, dynamic: value, guard: guard]} end
                map_paths = fn value, guard, pattern -> {:ok,
                  [body: value, dynamic: value, guard: guard, pattern: pattern]} end
                negative_zero = {:ok, -0.0, Base.decode16!("8000000000000000")}
                expected = [paths.({:ok, -4}, :rejected), paths.({:ok, 0}, :matched),
                  paths.({:ok, -2}, :rejected), paths.({:ok, -1}, :rejected),
                  paths.({:exception, :error, :badarith}, :rejected),
                  paths.({:exception, :error, :badarith}, :rejected),
                  paths.({:ok, false}, :rejected), paths.({:ok, true}, :matched),
                  paths.({:ok, false}, :rejected), paths.(negative_zero, :matched),
                  map_paths.({:ok, :wanted}, :matched, {:matched, :wanted}),
                  map_paths.({:ok, :other}, :rejected, {:matched, :other}),
                  map_paths.({:exception, :error, {:badkey, 1.0}}, :rejected, :no_match),
                  map_paths.({:exception, :error, {:badmap, :bad}}, :rejected, :no_match)]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_reference_failures_are_reported_without_stopping_other_cases(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([
                {"expression": "receive after infinity -> never end"},
                {"expression": "missing_lynx_module:missing()"},
                {"expression": "7-2"},
            ]))
            output = work / "references"
            summary = self.invoke(output, "--replay", source, "--timeout", 1)
            self.assertEqual(summary, {"laws": 0, "passed": 1, "failed": 0,
                                       "unverified": 2, "uncovered": 0})
            unverified = json.loads((output / "unverified.json").read_text())
            self.assertEqual([entry["message"] for entry in unverified],
                             ["OTP reference timed out", "OTP reference called an undefined function"])
            self.assertEqual(json.loads((output / "failures.json").read_text()), [])
            actuals = json.loads((output / "actuals.json").read_text())
            self.assertEqual([entry["status"] for entry in actuals], ["error", "ok", "ok"])
            self.assertIn("Timed out", actuals[0]["message"])
            self.assertEqual(unverified[0]["actual"], actuals[0])

    def test_true_assertions_reject_agreement_on_wrong_results(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([
                {"expression": expression, "assert_true": True}
                for expression in ["true", "false", "1", "{true}", "error(boom)"]
            ] + [{"expression": "false"}]))
            output = work / "assertions"
            summary = self.invoke(output, "--replay", source)
            self.assertEqual(summary, {"laws": 0, "passed": 2, "failed": 4,
                                       "unverified": 0, "uncovered": 0})
            failures = json.loads((output / "failures.json").read_text())
            self.assertEqual([f["case"]["expression"] for f in failures],
                             ["false", "1", "{true}", "error(boom)"])
            self.assertTrue(all(f["case"]["assert_true"] for f in failures))
            self.assertIn("must return true", (output / "comparison.log").read_text())
            self.assertEqual(self.invoke(work / "replay", "--replay", output / "failures.json"),
                             {"laws": 0, "passed": 0, "failed": 4,
                              "unverified": 0, "uncovered": 0})

    def test_lean_inventory_replay_reports_samples_not_proof_coverage(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            checkout = work / "lynx"
            tests = checkout / "Lean/LynxTest"
            tests.mkdir(parents=True)
            lean = tests / "Example.lean"
            lean.write_text("public theorem sampled : True := by trivial\n"
                            "private theorem unsampled : True := by trivial\n#guard true\n")
            origin = ["Example.lean#sampled"]
            source = work / "input.json"
            source.write_text(json.dumps([
                {"lean_origins": origin,
                 "expression": "differential:utf(utf8,<<16#E2,16#82,16#AC,255>>,0)"},
                {"lean_origins": origin,
                 "expression": "differential:short_circuit(or_else,true,throw)"},
                {"lean_origins": origin,
                 "expression": "differential:process_exchange({value,7})"},
                {"lean_origins": origin,
                 "expression": "differential:receive_timeout(-1,[wanted,other])"},
                {"lean_origins": origin, "expression": "missing_lynx_module:missing()"},
            ]))
            output = work / "full"
            summary = self.invoke(output, "--lean-runtime", checkout, "--replay", source)
            self.assertEqual(summary, {"laws": 0, "passed": 4, "failed": 0,
                                       "unverified": 1, "uncovered": 0})
            report = json.loads((output / "lean-coverage.json").read_text())
            self.assertEqual(report[0]["sha256"], hashlib.sha256(lean.read_bytes()).hexdigest())
            sampled, unsampled, guard = report[0]["tests"]
            self.assertEqual(sampled, {"test": "sampled", "line": 1, "status": "has_samples",
                                       "cases": 5, "passed": 4, "failed": 0, "unverified": 1})
            self.assertEqual([r["status"] for r in [unsampled, guard]], ["no_samples"] * 2)
            # Independent public-result oracles for Unicode, skipped effects,
            # child isolation, and a queued match before timeout validation.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                [utf, short, process, timeout] = Enum.map(Enum.take(values, 4), fn value ->
                  :erlang.binary_to_term(Base.decode64!(value))
                end)
                {:ok, {8364, <<255>>}} = utf
                {:ok, {true, :seen, :undefined}} = short
                {:ok, {true, true, true, true, true, true, :undefined,
                       {:value, 7}, :normal, [:pending]}} = process
                {:ok, {:matched, [:other]}} = timeout
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            subset = json.loads(source.read_text())[:1]
            source.write_text(json.dumps(subset))
            partial = work / "partial"
            self.assertEqual(self.invoke(partial, "--lean-runtime", checkout, "--replay", source)["passed"], 1)
            row = json.loads((partial / "lean-coverage.json").read_text())[0]["tests"][0]
            self.assertEqual((row["cases"], row["passed"], row["unverified"]), (1, 1, 0))
            subset[0]["lean_origins"] = ["Example.lean#renamed"]
            source.write_text(json.dumps(subset))
            rejected = subprocess.run([
                sys.executable, str(SCRIPT), "--lean-runtime", str(checkout),
                "--replay", str(source), "--output", str(work / "stale")
            ], capture_output=True, text=True, timeout=30)
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn("attribution no longer exists", rejected.stderr)

    def test_lifecycle_recipes_have_independent_result_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:linked_exit(trapped,kill)",
                "differential:linked_exit(untrapped,boom)",
                "differential:exit_signal(true,kill)",
                "differential:monitor_options(active_message,[flush,info])",
                "differential:monitor_options(queued_other,[flush,info])",
                "differential:alias_lifetime(reply_demonitor,demonitor)",
                "differential:receive_timeout(1 bsl 100,[])",
                "differential:suspended_receive(message)",
                "differential:termination_cascade(8,target_first)",
                "differential:missing_link(true)",
                "differential:missing_link(false)",
                "differential:process_liveness()",
                "differential:raise_exception(error,reason,[bad])",
                "differential:call(lists,keyfind,[1,1,[{1.0,float},{1,integer}]])",
                "differential:call(erlang,is_process_alive,[bad])",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 15, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                actual = Enum.map(values, &:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {:ok, {:kill, :kill}}},
                  {:ok, {:exit, :boom}},
                  {:ok, {:killed, :no_reply}},
                  {:ok, {true, [:other]}},
                  {:ok, {false, [{:down, :normal}]}},
                  {:ok, {false, false, []}},
                  {:ok, {{:exception, :error, :timeout_value}, []}},
                  {:ok, {:ok, {{:throw, :message_body}, true, :ran, [:retained]}}},
                  {:ok, {:killed, :killed, []}},
                  {:ok, {:ok, {true, :noproc, []}}},
                  {:ok, {:ok, {:exception, :error, :noproc}}},
                  {:ok, {:ok, {{true, true}, [], {true, {:registered_name, :lynx_liveness_child}},
                               {:undefined, []}, {false, :undefined}}}},
                  {:ok, {:returned, :badarg}},
                  {:ok, {1.0, :float}},
                  {:exception, :error, :badarg}
                ] = actual
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_map_callbacks_preserve_arguments_effects_and_exceptions(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:map_callback(merge_with,[#{a=>3},#{a=>11,b=>7}],valid)",
                "differential:map_callback(merge_with,[#{1=>left},#{1.0=>right}],valid)",
                "differential:map_callback(merge_with,[#{a=>3},#{b=>11}],throw)",
                "differential:map_callback(merge_with,[#{a=>3},#{a=>11}],error)",
                "differential:map_callback(filter,[#{keep=>ordinary,drop=>selected,skip=>ordinary}],valid)",
                "differential:map_callback(filter,[#{1=>selected,1.0=>ordinary}],valid)",
                "differential:map_callback(filter,[#{}],wrong_arity)",
                "differential:map_callback(filter,[#{keep=>selected}],exit)",
                "differential:map_callback(intersect_with,[#{a=>3},#{a=>11,b=>7}],valid)",
                "differential:map_callback(filtermap,[#{keep=>7,drop=>11,change=>9}],transform)",
                "differential:map_callback(update_with,[missing,initial,#{a=>3}],throw)",
                "differential:map_callback(update_with,[a,#{a=>3}],error)",
                "differential:map_callback(filter,[#{keep=>selected}],nonboolean)",
                "differential:map_callback(merge_with,[#{},#{}],wrong_arity)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 14, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {{:ok, %{a: {:a, 3, 11}, b: 7}}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{1 => :left, 1.0 => :right}}, []}},
                  {:ok, {{:ok, %{a: 3, b: 11}}, []}},
                  {:ok, {{:exception, :error, :callback_failed}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{keep: :ordinary, drop: :selected}},
                         [[:drop, :selected], [:keep, :ordinary], [:skip, :ordinary]]}},
                  {:ok, {{:ok, %{1 => :selected}}, [[1.0, :ordinary], [1, :selected]]}},
                  {:ok, {{:exception, :error, :badarg}, []}},
                  {:ok, {{:exception, :exit, :callback_failed}, [[:keep, :selected]]}},
                  {:ok, {{:ok, %{a: {:a, 3, 11}}}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{keep: 7, change: {:change, 9}}},
                         [[:change, 9], [:drop, 11], [:keep, 7]]}},
                  {:ok, {{:ok, %{a: 3, missing: :initial}}, []}},
                  {:ok, {{:exception, :error, :callback_failed}, [[3]]}},
                  {:ok, {{:exception, :error, {:case_clause, :not_boolean}}, [[:keep, :selected]]}},
                  {:ok, {{:exception, :error, :badarg}, []}}
                ] = actual
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_set_recipes_preserve_exact_elements_and_operand_direction(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:set_algebra([a,b],[b,c,d])",
                "differential:set_algebra([1],[1.0])",
                "differential:set_algebra([a],[a,b,c])",
                "differential:set_elements(sets:from_list([1,1.0,1],[{version,2}]))",
                "begin S=sets:from_list([1],[{version,2}]),"
                "{sets:is_element(1,S),sets:is_element(1.0,S),"
                "differential:set_elements(sets:del_element(1.0,S))} end",
                "differential:set_elements(sets:intersection([]))",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [overlap, numeric, subset, duplicates, membership, invalid] =
                  JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                {:ok, %{
                  left: {2, false, 2, %{a: :present, b: :present}},
                  right: {3, false, 3, %{b: :present, c: :present, d: :present}},
                  union: {4, false, 4, %{a: :present, b: :present, c: :present, d: :present}},
                  intersection: {1, false, 1, %{b: :present}},
                  left_only: {1, false, 1, %{a: :present}},
                  right_only: {2, false, 2, %{c: :present, d: :present}},
                  subset: {false, false}, disjoint: {false, false}, equal: {false, false},
                  relations: {true, true, true, true, true, true}
                }} = overlap
                {:ok, %{
                  union: {2, false, 2, %{1 => :present, 1.0 => :present}},
                  intersection: {0, true, 0, empty},
                  left_only: {1, false, 1, %{1 => :present}},
                  right_only: {1, false, 1, %{1.0 => :present}},
                  subset: {false, false}, disjoint: {true, true}, equal: {false, false},
                  relations: {true, true, true, true, true, true}
                }} = numeric
                0 = map_size(empty)
                {:ok, %{subset: {true, false}, left_only: {0, true, 0, empty},
                        right_only: {2, false, 2, %{b: :present, c: :present}}}} = subset
                {:ok, {2, false, 2, %{1 => :present, 1.0 => :present}}} = duplicates
                {:ok, {true, false, {1, false, 1, %{1 => :present}}}} = membership
                {:exception, :error, :function_clause} = invalid
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_function_recipes_have_independent_result_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:function_apply(local_one,captured,[7])",
                "differential:function_apply(bif_zero,captured,[7])",
                "differential:function_apply(missing_export,captured,[7])",
                "differential:function_make(differential,function_one,1,[7])",
                "differential:function_make(differential,function_one,256,[7])",
                "differential:function_named(differential,missing,[])",
                "differential:function_spawn(external,linked,payload)",
                "differential:function_spawn(local_one,monitored,payload)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 8, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {:ok, {:captured, 7}}},
                  {:ok, {:error, :badarity, true, true}},
                  {:ok, {:error, :badarity, true, true}},
                  {:ok, {true, {:module, :differential}, {:name, :function_one},
                         {:arity, 1}, {:ok, {:one, 7}}}},
                  {:exception, :error, :badarg},
                  {:ok, {:error, :undef}},
                  {:ok, {:ok, {:child, :normal, [{:reply, :payload}, {:exit, :normal}]}}},
                  {:ok, {:ok, {:child, {:badarity, true, true, true}, []}}}
                ] = actual
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_portable_hashing_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        hashing_cases = [
            c for c in all_cases
            if any(o.startswith("Modules/Hashing.lean#") for o in c.get("lean_origins", []))
        ]
        self.assertEqual(len(hashing_cases), 112)
        self.assertTrue(all(not c.get("assert_true") for c in hashing_cases))
        origins = {o for c in hashing_cases for o in c["lean_origins"]}
        self.assertEqual(
            origins,
            {
                "Modules/Hashing.lean#guard:1",
                "Modules/Hashing.lean#guard:2",
                "Modules/Hashing.lean#guard:3",
                "Modules/Hashing.lean#guard:4",
                "Modules/Hashing.lean#guard:5",
                "Modules/Hashing.lean#shadowed_bindings_hash",
                "Modules/Hashing.lean#capture_hash",
            },
        )
        selected_expressions = [
            "differential:phash2_probe(0)",
            "differential:phash2_probe(-1)",
            "differential:phash2_probe(134217728)",
            "differential:phash2_probe(-1267650600228229401496703205369)",
            "differential:phash2_probe({float_bits,0,0,0})",
            "differential:phash2_probe({float_bits,1,0,0})",
            "differential:phash2_probe({float_bits,0,0,1})",
            "differential:phash2_probe(a)",
            "differential:phash2_probe({utf8_atom,<<99,97,102,195,169>>})",
            "differential:phash2_probe({utf8_atom,<<230,151,165,230,156,172,232,170,158>>})",
            "differential:phash2_probe(<<>>)",
            "differential:phash2_probe(<<97,98,99>>)",
            "differential:phash2_probe(<<1,2,3:4>>)",
            "differential:phash2_probe([])",
            "differential:phash2_probe([104,101,108,108,111])",
            "differential:phash2_probe([1,2|3])",
            "differential:phash2_probe({a,{b,c},[1,2]})",
            "differential:phash2_probe(#{1=>a,[x]=>{y}})",
            "differential:phash2_probe({map_seq_40})",
            "differential:phash2_ranges(0)",
            "differential:phash2_range(a,7)",
            "differential:phash2_range(a,0)",
            "differential:phash2_range(a,(1 bsl 32)+1)",
            "differential:phash2_map_relations(shadowed_single)",
            "differential:phash2_live_identities(closures_shadowed_map)",
            "differential:phash2_live_identities(pids_and_refs)",
        ]
        by_expr = {c["expression"]: c for c in hashing_cases}
        self.assertTrue(all(expr in by_expr for expr in selected_expressions))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([by_expr[expr] for expr in selected_expressions]))
            output = work / "hashing"
            summary = self.invoke(output, "--replay", source)
            # Absent phash2/1,2 support stays an explicit target failure, never
            # allowlisted or marked unverified; remains valid once implemented.
            self.assertEqual(summary["passed"] + summary["failed"], 26)
            self.assertEqual(summary["unverified"], 0)
            check = subprocess.run([
                "elixir", "-e", """
                import Bitwise
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                probe = fn short, full ->
                  {:ok, {short, full, true, true, 0, rem(full, 7), true, rem(full, 1_000_003), true}}
                end
                full0 = 3_175_731_469
                ranges0 = Enum.map([1, 2, 7, 13, 255, 256, 65_537,
                                    (1 <<< 27) - 1, 1 <<< 27, (1 <<< 32) - 1, 1 <<< 32],
                                   &rem(full0, &1))
                expected = [
                  probe.(88_723_725, 3_175_731_469),
                  probe.(44_071_773, 1_117_813_597),
                  probe.(12_354_923, 2_562_491_755),
                  probe.(17_713_402, 3_373_156_602),
                  probe.(20_875_736, 423_528_920),
                  probe.(20_875_736, 423_528_920),
                  probe.(107_647_591, 3_731_526_247),
                  probe.(97, 97),
                  probe.(432_201, 432_201),
                  probe.(67_592_990, 67_592_990),
                  probe.(13_708_901, 147_926_629),
                  probe.(98_228_475, 1_306_188_027),
                  probe.(11_286_013, 2_292_987_389),
                  probe.(113_427_502, 3_468_870_702),
                  probe.(81_920_127, 753_008_767),
                  probe.(8_851_781, 4_169_601_349),
                  probe.(93_309_890, 1_569_704_898),
                  probe.(87_590_499, 2_637_727_331),
                  probe.(55_048_502, 2_202_532_150),
                  {:ok, {ranges0, true}},
                  {:ok, 6},
                  {:exception, :error, :badarg},
                  {:exception, :error, :badarg},
                  {:ok, {59_617_982, 1_401_795_262, true, true, true, true, true}},
                  {:ok, {true, true, true, true, true, true, true}},
                  {:ok, {true, true, true, true, true, true, true}}
                ]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_lifecycle_sequences_preserve_fenced_state_and_allowed_races(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            case_a = "differential:lifecycle_sequence(" + ",".join([
                "{false,true,none,none}",
                "[" + ",".join([
                    "{register_slot,reg_a,alpha}",
                    "{register_slot,reg_b,alpha}",
                    "{register_slot,reg_a,beta}",
                    "{whereis_slot,reg_a}",
                    "{unregister_slot,reg_a}",
                    "{peer_link_action,alpha,unlink}",
                    "{link_slot,missing}",
                    "{trap_exit,true}",
                    "{link_slot,missing}",
                    "{fenced_exit_signal,custom_signal}",
                    "{trap_exit,false}",
                    "{drain_mailbox}",
                ]) + "]",
            ]) + ")"
            case_b = "differential:lifecycle_sequence(" + ",".join([
                "{true,false,reg_a,explicit_unalias}",
                "[" + ",".join([
                    "{alias_send,a1,alpha,hello_a1}",
                    "{monitor_slot,m1,reg_a,reply_demonitor}",
                    "{alias_send,m1,alpha,one_shot}",
                    "{demonitor_slot,m1,info}",
                    "{monitor_slot,m2,beta,demonitor}",
                    "{monitor_slot,m1,alpha,bad_opt}",
                    "{kill_child,beta,shutdown}",
                    "{monitor_dead_queued,m1,explicit_unalias}",
                    "{demonitor_slot,m1,flush_info}",
                    "{alias_unalias,m1}",
                    "{spawn_slot,beta}",
                    "{link_slot,alpha}",
                    "{trap_exit,false}",
                    "{kill_child,alpha,boom}",
                    "{race_unlink_exit,beta,boom}",
                    "{race_demonitor_exit,explicit_unalias,flush,shutdown}",
                    "{drain_mailbox}",
                ]) + "]",
            ]) + ")"
            source.write_text(json.dumps([{"expression": case_a}, {"expression": case_b}]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [a, b] = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                s_a0 = {false, [:alpha], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a1 = {false, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a5 = {false, [:alpha], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a6 = {false, [], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a8 = {true, [], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                {:ok, {:ok, {^s_a0, [
                  {{:register_slot, :reg_a, :alpha, {:ok, true}, :alpha}, ^s_a1},
                  {{:register_slot, :reg_b, :alpha, {:exception, :error, :badarg}, :undefined}, ^s_a1},
                  {{:register_slot, :reg_a, :beta, {:exception, :error, :badarg}, :alpha}, ^s_a1},
                  {{:whereis_slot, :reg_a, :alpha}, ^s_a1},
                  {{:unregister_slot, :reg_a, {:ok, true}, :undefined}, ^s_a5},
                  {{:peer_link_action, :alpha, :unlink, true, false, false, false}, ^s_a6},
                  {{:link_slot, :missing, {:exception, :error, :noproc}}, ^s_a6},
                  {{:trap_exit, true, {:ok, false}, true}, ^s_a8},
                  {{:link_slot, :missing, {:ok, :noproc}}, ^s_a8},
                  {{:fenced_exit_signal, :custom_signal, :queued}, ^s_a8},
                  {{:trap_exit, false, {:ok, true}, false}, ^s_a6},
                  {{:drain_mailbox, [{:exit_msg, {:helper, 2}, :custom_signal}]}, ^s_a6}
                ]}}} = a
                s_b0 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {{:explicit_unalias, true}, :none}}
                s_b2 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, true, true}, :none}, {{:explicit_unalias, true}, :none}}
                s_b3 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, false, false}, :none}, {{:explicit_unalias, true}, :none}}
                s_b5 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, false, false}, {:demonitor, true, true}}, {{:explicit_unalias, true}, :none}}
                s_b6 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, {:demonitor, true, true}}, {{:explicit_unalias, true}, :none}}
                s_b7 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {:none, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b8 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {{:explicit_unalias, false, true}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b10 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b11 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b12 = {true, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b13 = {false, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b14 = {false, [], {:undefined, :undefined}, {{:dead, false}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b15 = {false, [], {:undefined, :undefined}, {{:dead, false}, {:dead, false}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                {:ok, {:ok, {^s_b0, [
                  {{:alias_send, :a1, :alpha, {:delivered, :hello_a1}}, ^s_b0},
                  {{:monitor_slot, :m1, :reg_a, :reply_demonitor, :ok}, ^s_b2},
                  {{:alias_send, :m1, :alpha, {:delivered, :one_shot}}, ^s_b3},
                  {{:demonitor_slot, :m1, :info, {:ok, false}}, ^s_b3},
                  {{:monitor_slot, :m2, :beta, :demonitor, :ok}, ^s_b5},
                  {{:monitor_slot, :m1, :alpha, :bad_opt, {:exception, :error, :badarg}}, ^s_b6},
                  {{:kill_child, :beta, :shutdown, {:ok, :shutdown, :none, [{:m2, {true, :shutdown}}]}}, ^s_b7},
                  {{:monitor_dead_queued, :m1, :explicit_unalias, :queued}, ^s_b8},
                  {{:demonitor_slot, :m1, :flush_info, {:ok, false}}, ^s_b8},
                  {{:alias_unalias, :m1, {:ok, true}}, ^s_b10},
                  {{:spawn_slot, :beta, :spawned, []}, ^s_b11},
                  {{:link_slot, :alpha, {:ok, true}, true, true, true}, ^s_b12},
                  {{:trap_exit, false, {:ok, true}, false}, ^s_b13},
                  {{:kill_child, :alpha, :boom, {:ok, :boom, {:observer_linked_exit, :boom}, []}}, ^s_b14},
                  {{:race_unlink_exit, :beta, :boom, true}, ^s_b15},
                  {{:race_demonitor_exit, :explicit_unalias, :flush, :shutdown, true}, ^s_b15},
                  {{:drain_mailbox, []}, ^s_b15}
                ]}}} = b
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    @unittest.skipUnless(os.getenv("LYNX_CHECKOUT"), "Set LYNX_CHECKOUT to test fixture coverage")
    def test_lynx_exports_have_cases_and_partial_replay_is_scoped(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            full = work / "full"
            summary = self.invoke(full, "--lynx", os.environ["LYNX_CHECKOUT"],
                                  "--generated", 0, "--timeout", 1)
            self.assertGreater(summary["laws"], 0)
            self.assertEqual(summary["uncovered"], 0)
            self.assertFalse((full / "lists.beam").exists())
            coverage = json.loads((full / "coverage.json").read_text())
            self.assertGreater(len(coverage), 100)
            self.assertTrue(all(row["cases"] > 0 for row in coverage))
            selected = json.loads((full / "cases.json").read_text())
            law_samples = [c for c in selected if c.get("assert_true")]
            self.assertEqual(len(law_samples), 19)
            self.assertEqual(sum(c["fixture"] == ["sum", "sum_singleton_ensures", 1]
                                 for c in law_samples), 5)
            self.assertEqual(sum(c["fixture"] == ["sum", "sum_empty_ensures", 0]
                                 for c in law_samples), 1)
            self.assertEqual(sum(c["fixture"][0] == "protocols" for c in law_samples), 13)
            manifest = json.loads((full / "manifest.json").read_text())
            self.assertTrue(manifest["support"])
            self.assertTrue(all(name.startswith("Elixir.Lynx.Support.")
                                for name in manifest["support"]))
            self.assertTrue(all((full / f"{name}.beam").is_file()
                                for name in manifest["support"]))
            protocol_calls = [c for c in selected if c["fixture"][0] == "protocols"]
            self.assertEqual(len(protocol_calls), 78)
            failures = json.loads((full / "failures.json").read_text())
            self.assertFalse(any(f["case"].get("assert_true") for f in failures))
            # A partial replay must not claim full fixture coverage merely
            # because its one selected call matched.
            source = work / "one.json"
            source.write_text(json.dumps([{"fixture": ["literal", "integers", 0],
                                          "expression": "literal:integers()"}]))
            partial = work / "partial"
            subset = self.invoke(partial, "--lynx", os.environ["LYNX_CHECKOUT"], "--replay", source)
            self.assertEqual(subset["passed"], 1)
            self.assertEqual(subset["failed"], 0)
            self.assertEqual(subset["uncovered"], len(coverage) - 1)
            self.assertEqual(subset["unverified"], 0)

    def test_wrong_target_result_is_saved_shrunk_and_replayable(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            build = work / "build"
            (build / "src").mkdir(parents=True)
            (build / "tools/packbeam").mkdir(parents=True)
            (build / "libs").mkdir()
            (build / "CMakeCache.txt").write_text("AVM_DISABLE_JIT:BOOL=ON\nAVM_ENABLE_PRECOMPILED:BOOL=OFF\n")
            (build / "libs/atomvmlib.avm").touch()
            (build / "tools/packbeam/PackBEAM").symlink_to(ROOT / "build/tools/packbeam/PackBEAM")
            # A successful process emitting the wrong serialized term must still fail.
            runtime = build / "src/AtomVM"
            runtime.write_text("#!/bin/sh\necho 'LYNX_RESULT [131,100,0,4,110,111,112,101]'\n")
            runtime.chmod(0o755)
            source = work / "input.json"
            source.write_text(json.dumps([{"operation": "compare", "args": [100, -7]}]))
            output = work / "failure"
            summary = self.invoke(output, "--build", build, "--replay", source, "--shrink")
            self.assertEqual(summary, {"laws": 0, "passed": 0, "failed": 1,
                                       "unverified": 0, "uncovered": 0})
            failures = json.loads((output / "failures.json").read_text())
            self.assertEqual(failures[0]["case"]["args"], [100, -7])
            reduced = json.loads((output / "minimized.json").read_text())
            self.assertEqual(reduced[0]["args"], [0, 0])
            self.assertEqual(
                self.invoke(work / "replayed", "--build", build, "--replay", output / "minimized.json"),
                summary,
            )
            setup = [{"tuple": ["sentinel", 17]}, {"tuple": [1.0, "second"]}]
            steps = [{"tuple": s} for s in [
                ["put", 1, "first"], ["put", 1, "replaced"],
                ["erase", 1], ["get", 1.0], ["put", 1, "reinserted"],
            ]]
            for operation in ["map_sequence", "dictionary_sequence", "receive_sequence", "binary_sequence", "lifecycle_sequence"]:
                if operation == "receive_sequence":
                    setup = ["sentinel", 1.0, {"tuple": ["tag", 1]}]
                    steps = [{"tuple": s} for s in [
                        ["exact", 1], ["send", 1], ["tag", "tag"], ["exact", 1], ["any"],
                    ]]
                elif operation == "binary_sequence":
                    setup = list(b"zababcab")
                    steps = [{"tuple": s} for s in [
                        ["part", 1, 5], ["part", 5, -3], ["append", [97, 98]],
                        ["match", {"bits": [0x6162, 16]}, []],
                    ]]
                elif operation == "lifecycle_sequence":
                    setup = {"tuple": ["false", "true", "none", "none"]}
                    steps = [{"tuple": s} for s in [
                        ["register_slot", "reg_a", "alpha"],
                        ["whereis_slot", "reg_a"],
                        ["peer_link_action", "alpha", "unlink"],
                        ["kill_child", "alpha", "shutdown"],
                    ]]
                source.write_text(json.dumps([
                    {"seed": 123, "index": 7, "operation": operation, "args": [setup, steps]}
                ]))
                output = work / operation
                self.assertEqual(self.invoke(output, "--build", build, "--replay", source, "--shrink"),
                                 summary)
                reduced = json.loads((output / "minimized.json").read_text())[0]
                self.assertEqual(reduced["args"][0], setup)
                self.assertEqual(len(reduced["args"][1]), 1)
                self.assertIn(reduced["args"][1][0], steps)
                self.assertEqual((reduced["seed"], reduced["index"]), (123, 7))
                self.assertEqual(self.invoke(work / (operation + "-replay"), "--build", build,
                                             "--replay", output / "minimized.json"), summary)
            runtime.write_text("#!/bin/sh\necho 'LYNX_RESULT [131]'\n")
            malformed = work / "malformed"
            self.assertEqual(self.invoke(malformed, "--build", build, "--replay", source), summary)
            failure = json.loads((malformed / "failures.json").read_text())[0]
            self.assertEqual(failure["actual"]["status"], "error")
            self.assertIn("Invalid serialized result", failure["actual"]["message"])
            sys.path.insert(0, str(Path(__file__).resolve().parent))
            import base64
            import validate
            ok_encoded = base64.b64encode(bytes([131, 104, 2, 119, 2, *b"ok", 97, 1])).decode("ascii")
            exc_encoded = base64.b64encode(bytes([131, 104, 3, 119, 9, *b"exception", 119, 5, *b"error", 119, 6, *b"badarg"])).decode("ascii")
            orig = {"case": {"operation": "lifecycle_sequence"}, "expected": ok_encoded, "actual": {"status": "ok", "value": ok_encoded}}
            self.assertTrue(validate.same_failure_contract(orig, dict(orig)))
            self.assertFalse(validate.same_failure_contract(orig, {**orig, "expected": exc_encoded}))
            self.assertFalse(validate.same_failure_contract(orig, {**orig, "case": {"operation": "compare"}}))


if __name__ == "__main__":
    unittest.main()
