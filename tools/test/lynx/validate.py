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
from generated import candidates, expression, generate


def cases():
    selected = []

    def add(module, function, *args):
        expression = f"{module}:{function}({','.join(args)})"
        selected.append(expression)

    for function in ["integers", "floats", "bitstrings", "tuples", "maps", "pooled_nested"]:
        add("literal", function)
    add("literal", "computed_maps", "1.0", "{nested,7}")
    for value in ["[]", "[5,-12,19]", "[1.5,2]", "[bad]", "[1|2]"]:
        add("sum", "sum", value)
    add("sum", "sum_empty_ensures")
    for value in ["-(1 bsl 60)", "0", "1 bsl 60"]:
        add("sum", "sum_singleton_requires", value)
        add("sum", "sum_singleton_ensures", value)
    for value in ["1.5", "not_an_integer"]:
        add("sum", "sum_singleton_requires", value)
    for function in ["odd", "even", "first", "second"]:
        add("mutual", function, "[a,b,c]")
    add("mutual", "triples", "[1,2]", "[a]", "[x,y]")
    for function in ["captured_closures", "apply_calls", "sequence_calls", "qualified_capture"]:
        add("functions", function, "-7")
    add("functions", "calls_by_name", "functions", "9")
    selected.append("{A,B,M}=functions:references(), {A,B,map_size(M)}")
    for value in ["1", "1.0", "0.0", "-0.0", "-1.5", "{0.1,[5.0e-324]}"]:
        add("patterns", "floats", value)
    selected.append(
        "P=element(2,literal:computed_tuples(0.0)), "
        "N=element(2,literal:computed_tuples(-0.0)), "
        "{P==N,P=:=N,map_size(#{P=>positive,N=>negative}),"
        "#{#{value=>P}=>key}==#{#{value=>N}=>key}}"
    )
    add("patterns", "map_duplicate", "a", "a", "#{a=>{ok,9}}")
    add("patterns", "map_nested", "{tag,[#{a=>#{b=>7}}]}")
    for value in ["#{a=>0,b=>3}", "#{a=>2,b=>3}", "#{a=>0,b=>no}", "#{}"]:
        add("patterns", "map_grouped", value)
    for function, value in [("integer", "1.0"), ("binary", "<<1:1>>"), ("bitstring", "<<1:1>>")]:
        add("guard_patterns", function, value)
    for left, right in [("0", "bad"), ("1", "[]"), ("[]", "not_boolean"), ("1", "bad")]:
        add("guard_patterns", "short_circuit", left, right)
    for function in ["scope", "handler_scope", "reraised", "after_effect", "after_raises", "success_clause", "nested"]:
        for value in ["1", "bad"]:
            add("tries", function, value)
    selected.append("{C,R,S}=tries:caught(bad), {C,R,is_list(S)}")
    add("tries", "effectful")
    add("higher_order", "map", "[1,-4,8]", "fun functions:inc/1")
    add("higher_order", "proper_list", "[1|2]")
    for function, args in [
        ("comprehension_filtered", ["[{a,1},{2,3},skip,{b,4}]"]),
        ("comprehension_lists", ["[1,2]", "[a,b,c]"]),
        ("comprehension_lists3", ["[1,2]", "[a]", "[x,y]"]),
        ("comprehension_zip", ["[1]", "[a,b]"]),
        ("map_comprehension", ["[{a,1},{a,2},{b,3}]"]),
        ("comprehension_map_list", ["#{a=>1,b=>2}", "[x,y]"]),
        ("bits_generator", ["<<7:3,1:2,2:3,3:2>>"]),
        ("binary_generator_strict", ["<<1,2,3>>"]),
        ("binary_generator", ["<<65,226,130,172>>"]),
        ("utf8_generator", ["<<65,226,130,172>>"]),
        ("binary_comprehension", ["[1,2,15]"]),
        ("binary_zip", ["<<1,2>>", "<<3:4,4:4>>"]),
        ("self_reference", []),
    ]:
        add("named_locals", function, *args)
    selected.append("lists:sort(named_locals:map_generator(#{b=>2,a=>1}))")
    for function in ["integer_big", "integer_signed_big", "integer_little", "integer_signed_little"]:
        for size in ["0", "12", "-1"]:
            add("binary_construction", function, "-257", size)
    for function in ["float16", "float32", "float_mixed"]:
        add("binary_construction", function, "-2.25")
    for function in ["utf8", "utf16_little", "utf32_big", "utf_mixed"]:
        for value in ["16#1f600", "16#d800"]:
            add("binary_construction", function, value)
    for function, args in [
        ("partial_literal", ["<<5:3>>"]),
        ("integers", ["<<200:8,-257:12/signed-big,1023:12/signed-little>>"]),
        ("integer_sized", ["<<12:8,-257:12/signed-little,5:3>>"]),
        ("float16", ["<<16#3e00:16>>"]),
        ("float32", ["<<1.5:32/float-big,-2.25:32/float-little>>"]),
        ("binary_fixed", ["2", "<<3:3,1,2>>"]),
        ("utf8", ["<<255>>"]),
        ("utf8", ["<<226,130,172,1:1>>"]),
        ("utf16_little", ["<<16#3d,16#d8,0,16#de>>"]),
    ]:
        add("bitstring_matching", function, *args)
    selected.extend([
        "self()!ignored, self()!{tag,9}, {receives:selective(tag),receives:plain()}",
        "self()!{tag,<<1>>}, receives:guarded(tag)",
        "self()!{self(),9}, receives:self_guard()",
        "receives:poll()",
        "self()!{ok,9}, receives:poll()",
        "receives:timed(1)",
        "receives:timed(bad)",
        "self()!{ok,tag}, self()!tag, receives:nested()",
        "self()!{1,2}, self()!{3,3}, {receives:repeated(),receives:plain()}",
    ])
    return selected


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
        selected = ([{"expression": value} for value in cases()] if lynx else [])
        selected += generate(args.seed, args.generated)
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
        raise RuntimeError(f"{summary['failed']} differential failures; replay {work / 'failures.json'}")


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
