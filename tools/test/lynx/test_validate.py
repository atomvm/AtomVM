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
                             {"compare", "maps", "bits", "arithmetic", "float_bits"})

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
            self.assertEqual(len(law_samples), 5)
            self.assertTrue(all(c["fixture"] == ["sum", "sum_singleton_ensures", 1]
                                for c in law_samples))
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
            runtime.write_text("#!/bin/sh\necho 'LYNX_RESULT [131]'\n")
            malformed = work / "malformed"
            self.assertEqual(self.invoke(malformed, "--build", build, "--replay", source), summary)
            failure = json.loads((malformed / "failures.json").read_text())[0]
            self.assertEqual(failure["actual"]["status"], "error")
            self.assertIn("Invalid serialized result", failure["actual"]["message"])


if __name__ == "__main__":
    unittest.main()
