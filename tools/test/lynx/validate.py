#!/usr/bin/env python3
# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Host-driven OTP/AtomVM differential testing, with optional Lynx proofs."""

import argparse
import base64
import hashlib
import json
import math
import os
import platform
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path

# Keep local runs from leaving imported bytecode in the source checkout.
sys.dont_write_bytecode = True
from fixtures import cases
from generated import candidates, expression, generate


def command(argv, cwd, log, timeout=600):
    # Kill the whole group on timeout, including children started by Mix/Lake.
    with subprocess.Popen(argv, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                          text=True, start_new_session=True) as process:
        try:
            output, _ = process.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            output, _ = process.communicate()
            log.write_text(output)
            raise RuntimeError(f"Timed out after {timeout}s: {argv[0]}\n{output}") from None
    log.write_text(output)
    if process.returncode:
        raise RuntimeError(f"{argv[0]} failed ({process.returncode}):\n{output}")
    return output


def validate(args, work):
    build = args.build.resolve()
    lynx = args.lynx.resolve() if args.lynx else None
    for executable in ["erl", "erlc", "elixir"] + (["mix", "lake"] if lynx else []):
        if not shutil.which(executable):
            raise RuntimeError(f"Required executable not on PATH: {executable}")
    if lynx and not (lynx / "mix.exs").is_file():
        raise RuntimeError(f"Not a Lynx checkout: {lynx}")
    for path in [build / "src/AtomVM", build / "tools/packbeam/PackBEAM", build / "CMakeCache.txt"]:
        if not path.is_file():
            raise RuntimeError(f"Build AtomVM first; missing {path}")
    config = {}
    for line in (build / "CMakeCache.txt").read_text().splitlines():
        if "=" in line and ":" in line.split("=", 1)[0]:
            key, value = line.split("=", 1)
            config[key.split(":", 1)[0]] = value
    native = config.get("AVM_DISABLE_JIT") == "OFF" or config.get("AVM_ENABLE_PRECOMPILED") == "ON"
    host_arch = {"arm64": "aarch64"}.get(platform.machine(), platform.machine())
    arch = config.get("AVM_JIT_TARGET_ARCH", host_arch) if native else ""
    library = build / "libs" / (f"atomvmlib-{arch}.avm" if native else "atomvmlib.avm")
    if not library.is_file():
        raise RuntimeError(f"Build the matching runtime library first: {library}")
    (work / "run.json").write_text(json.dumps({
        "seed": args.seed,
        "runtime": str(build / "src/AtomVM"),
        "runtime_sha256": hashlib.sha256((build / "src/AtomVM").read_bytes()).hexdigest(),
        "library": str(library),
        "library_sha256": hashlib.sha256(library.read_bytes()).hexdigest(),
    }))
    if args.replay:
        selected = json.loads(args.replay.read_text())
        # Accept either cases.json/minimized.json or the richer failures.json.
        selected = [entry.get("case", entry) for entry in selected]
        for entry in selected:
            if "operation" in entry:
                entry["expression"] = expression(entry)
    else:
        selected = cases() if lynx else []
        selected += generate(args.seed, args.generated, args.float_encoding)
    if not selected:
        raise RuntimeError("No cases selected; use --generated, --replay, or --lynx")
    print(f"Seed {args.seed}; {len(selected)} cases; build {build}", flush=True)
    entries = []
    for index, entry in enumerate(selected):
        module = f"lynx_probe_{index}"
        entries.append({**entry, "module": module})
        (work / f"{module}.erl").write_text(
            f"-module({module}).\n-export([actual/0,start/0]).\n"
            f"actual() -> try {{ok,begin {entry['expression']} end}} "
            "catch Class:Reason -> {exception,Class,Reason} end.\n"
            'start() -> io:format("LYNX_RESULT ~w~n",'
            "[binary_to_list(term_to_binary(actual()))]), ok.\n"
        )
    (work / "cases.json").write_text(json.dumps(entries))
    shutil.copyfile(Path(__file__).with_name("differential.erl"), work / "differential.erl")
    command(
        ["erlc", "-o", str(work), str(work / "differential.erl")]
        + [str(work / f"{e['module']}.erl") for e in entries],
        work, work / "compile.log",
    )
    helper = str(Path(__file__).with_suffix(".exs").resolve())
    host = ["mix", "run"] if lynx else ["elixir"]
    print("Computing isolated OTP references" + (" and checking Lynx laws..." if lynx else "..."), flush=True)
    command(host + [helper, "prepare" if lynx else "prepare_generated", str(work),
                    str(int(args.timeout * 1000))], lynx or work, work / "proofs.log")
    manifest = json.loads((work / "manifest.json").read_text())
    print(f"Checked {manifest['laws']} laws; running {len(entries)} cases ({arch or 'interpreter'}).", flush=True)
    if lynx:
        print("Dependency: packaged OTP lists.beam; bundled lists:sum/1 is not tested.", flush=True)
    fixture_beams = [work / "differential.beam"] + [work / f"{name}.beam" for name in manifest["fixtures"]]
    if lynx:
        fixture_beams.append(work / "lists.beam")
    drivers = [work / f"{entry['module']}.beam" for entry in entries]
    if native:
        native_dir = work / "native"
        native_dir.mkdir()
        command(
            ["erl", "-pa", str(build / "libs/jit/src/beams"), "-noshell",
             "-s", "jit_precompile", "-s", "init", "stop", "--", arch, str(native_dir) + "/"]
            + [str(p) for p in fixture_beams + drivers],
            work, work / "precompile.log",
        )
        fixture_beams = [native_dir / p.name for p in fixture_beams]
        drivers = [native_dir / p.name for p in drivers]
    actuals = []
    for entry, driver in zip(entries, drivers):
        pack = work / "case.avm"
        command(
            [str(build / "tools/packbeam/PackBEAM"), "create", str(pack), str(driver)]
            + [str(p) for p in fixture_beams],
            work, work / "pack.log",
        )
        try:
            output = command(
                [str(build / "src/AtomVM"), str(pack), str(library)],
                work, work / f"{entry['module']}.log", timeout=args.timeout,
            )
            lines = [line[len("LYNX_RESULT "):] for line in output.splitlines() if line.startswith("LYNX_RESULT ")]
            if len(lines) != 1:
                raise RuntimeError(f"Expected one result, got {len(lines)}: {output}")
            encoded = bytes(json.loads(lines[0]))
            actuals.append({"status": "ok", "value": base64.b64encode(encoded).decode()})
        except (RuntimeError, ValueError, TypeError) as error:
            actuals.append({"status": "error", "message": str(error)})
        if len(actuals) % 20 == 0:
            print(f"Executed {len(actuals)}/{len(entries)} cases", flush=True)
    (work / "actuals.json").write_text(json.dumps(actuals))
    # Compare deserialized terms on OTP, not using AtomVM's comparator.
    compare_host = ["mix", "run", "--no-compile"] if lynx else ["elixir"]
    print(command(compare_host + [helper, "compare", str(work)], lynx or work, work / "comparison.log"), end="")
    summary = json.loads((work / "summary.json").read_text())
    if summary["failed"]:
        failures = json.loads((work / "failures.json").read_text())
        if args.shrink:
            shrink(args, work, failures[0])
    if summary["failed"] or summary["unverified"] or (summary["uncovered"] and not args.replay):
        raise RuntimeError(
            f"{summary['failed']} differential failures, {summary['unverified']} unverified cases, "
            f"{summary['uncovered']} uncovered exports; see {work / 'summary.json'}"
        )


def shrink(args, work, failure):
    current = failure["case"]
    if "operation" not in current:
        print("Shrinking is available for generated cases only.")
        return
    print("Reducing the first failure (at most 30 trials)...", flush=True)
    trial = work / "shrink-trial"
    source = work / "minimized.json"
    attempts = 0
    while attempts < 30:
        improved = False
        for candidate in candidates(current):
            if attempts >= 30:
                break
            attempts += 1
            source.write_text(json.dumps([candidate]))
            command_line = [sys.executable, str(Path(__file__).resolve()), "--build", str(args.build.resolve()),
                            "--replay", str(source), "--timeout", str(args.timeout), "--output", str(trial)]
            try:
                command(command_line, work, work / "shrink.log")
            except RuntimeError:
                # Compilation/reference failures are not evidence of a reduced VM mismatch.
                if (trial / "failures.json").exists():
                    reduced = json.loads((trial / "failures.json").read_text())
                    if reduced and reduced[0]["actual"]["status"] == failure["actual"]["status"]:
                        current = candidate
                        improved = True
            finally:
                shutil.rmtree(trial, ignore_errors=True)
            if improved:
                break
        if not improved:
            break
    source.write_text(json.dumps([current]))
    print(f"Reproducer after {attempts} reduction trials: {current['expression']}", flush=True)
    print(f"Replay with --replay {source}", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--lynx", type=Path, help="Also check fixture laws/calls from this Lynx checkout")
    parser.add_argument("--seed", type=int, default=1, help="Reproducible generator seed (default: 1)")
    parser.add_argument("--generated", type=int, default=1000, help="Generated cases (default: 1000; 0 for fixtures only)")
    parser.add_argument("--float-encoding", action="store_true", help="Focus generated cases on float16/32 encoding boundaries")
    parser.add_argument("--replay", type=Path, help="Replay cases.json, failures.json, or minimized.json instead of generating")
    parser.add_argument("--shrink", action="store_true", help="Try up to 30 reductions of the first generated failure")
    parser.add_argument(
        "--build", type=Path, default=Path(__file__).resolve().parents[3] / "build",
        help="Existing native AtomVM CMake build",
    )
    parser.add_argument("--output", type=Path, help="Keep logs/binaries in a new or empty directory")
    parser.add_argument("--timeout", type=float, default=5, help="Seconds per OTP/AtomVM case (default: 5)")
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or args.timeout <= 0:
        parser.error("--timeout must be positive and finite")
    if args.generated < 0:
        parser.error("--generated must not be negative")
    work = None
    try:
        if args.output:
            work = args.output.resolve()
            work.mkdir(parents=True, exist_ok=True)
            if any(work.iterdir()):
                parser.error("--output must be a new or empty directory")
            print(f"Keeping results in {work}", flush=True)
            validate(args, work)
        else:
            work = Path(tempfile.mkdtemp(prefix="atomvm-differential-"))
            validate(args, work)
            shutil.rmtree(work)
    except (OSError, RuntimeError, ValueError, KeyError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        if work:
            print(f"Retained results in {work}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
