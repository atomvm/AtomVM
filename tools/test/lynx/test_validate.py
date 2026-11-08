# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Public CLI integration tests; requires a built AtomVM and host OTP/Elixir."""

import json
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
            capture_output=True, text=True, timeout=180,
        )
        self.assertTrue((output / "summary.json").is_file(), result.stdout + result.stderr)
        summary = json.loads((output / "summary.json").read_text())
        self.assertEqual(result.returncode, int(summary["failed"] > 0), result.stdout + result.stderr)
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
            self.assertEqual(summary, {"laws": 0, "passed": 0, "failed": 1})
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
