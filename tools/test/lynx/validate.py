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
import lean_runtime


class CommandTimeoutError(RuntimeError):
    def __init__(self, executable, timeout, output):
        super().__init__(f"Timed out after {timeout}s: {executable}\n{output}")
        self.output = output
        self.timeout = timeout


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
            output = output or ""
            log.write_text(output)
            raise CommandTimeoutError(argv[0], timeout, output) from None
    log.write_text(output)
    if process.returncode:
        raise RuntimeError(f"{argv[0]} failed ({process.returncode}):\n{output}")
    return output


def parse_cmake_cache(cache_path):
    config = {}
    for line in cache_path.read_text().splitlines():
        if "=" in line and ":" in line.split("=", 1)[0]:
            key, value = line.split("=", 1)
            config[key.split(":", 1)[0]] = value
    return config


def resolve_target(build, host_build_arg):
    cache_path = build / "CMakeCache.txt"
    if not cache_path.is_file():
        raise RuntimeError(f"Build AtomVM first; missing {cache_path}")
    config = parse_cmake_cache(cache_path)
    is_emscripten = (
        (build / "src/AtomVM.mjs").is_file()
        or config.get("CMAKE_SYSTEM_NAME") == "Emscripten"
    )
    if is_emscripten:
        platform_name = "emscripten"
        runtime_path = build / "src/AtomVM.mjs"
        wasm_path = build / "src/AtomVM.wasm"
        runtime_argv = ["node", str(runtime_path)]
        host_build = (host_build_arg or (Path(__file__).resolve().parents[3] / "build")).resolve()
        packbeam = host_build / "tools/packbeam/PackBEAM"
        word_size = 4
        for required in [runtime_path, wasm_path, packbeam]:
            if not required.is_file():
                raise RuntimeError(f"Build AtomVM first; missing {required}")
    else:
        platform_name = "generic_unix"
        runtime_path = build / "src/AtomVM"
        wasm_path = None
        runtime_argv = [str(runtime_path)]
        host_build = (host_build_arg or build).resolve()
        packbeam = (
            build / "tools/packbeam/PackBEAM"
            if (build / "tools/packbeam/PackBEAM").is_file()
            else host_build / "tools/packbeam/PackBEAM"
        )
        word_size = int(config.get("CMAKE_SIZEOF_VOID_P") or 8)
        for required in [runtime_path, packbeam]:
            if not required.is_file():
                raise RuntimeError(f"Build AtomVM first; missing {required}")

    jit = config.get("AVM_DISABLE_JIT", "ON") == "OFF"
    precompiled = config.get("AVM_ENABLE_PRECOMPILED", "OFF") == "ON"
    float32_mode = config.get("AVM_USE_32BIT_FLOAT", "OFF") == "ON"
    float_bits = 32 if float32_mode else 64
    if jit and precompiled:
        execution_mode = "jit+precompiled"
    elif jit:
        execution_mode = "on_demand_jit"
    elif precompiled:
        execution_mode = "precompiled"
    else:
        execution_mode = "interpreter"

    native = jit or precompiled
    host_arch = {"arm64": "aarch64"}.get(platform.machine(), platform.machine())
    arch = (
        "wasm32" if platform_name == "emscripten"
        else (config.get("AVM_JIT_TARGET_ARCH", host_arch) if native else "")
    )
    if platform_name == "emscripten":
        candidates = [
            build / "libs/atomvmlib-emscripten.avm",
            host_build / "libs/atomvmlib-emscripten.avm",
            host_build / "libs/atomvmlib.avm",
        ]
        library = next((path for path in candidates if path.is_file()), None)
        if library is None:
            raise RuntimeError(f"Build the matching Emscripten runtime library first: {candidates[1]}")
    else:
        library = build / "libs" / (f"atomvmlib-{arch}.avm" if native else "atomvmlib.avm")
        if not library.is_file():
            raise RuntimeError(f"Build the matching runtime library first: {library}")

    return {
        "platform": platform_name,
        "runtime_path": runtime_path,
        "wasm_path": wasm_path,
        "runtime_argv": runtime_argv,
        "host_build": host_build,
        "packbeam": packbeam,
        "word_size": word_size,
        "float_bits": float_bits,
        "jit": jit,
        "precompiled": precompiled,
        "float32_mode": float32_mode,
        "execution_mode": execution_mode,
        "native": native,
        "arch": arch,
        "target_arch": arch or config.get("CMAKE_SYSTEM_PROCESSOR", host_arch),
        "library": library,
    }


def validate(args, work):
    build = args.build.resolve()
    lynx = args.lynx.resolve() if args.lynx else None
    if lynx and not (lynx / "mix.exs").is_file():
        raise RuntimeError(f"Not a Lynx checkout: {lynx}")
    target = resolve_target(build, args.host_build)
    required_tools = ["erl", "erlc", "elixir"] + (["mix", "lake"] if lynx else [])
    if target["platform"] == "emscripten":
        required_tools.append("node")
    for executable in required_tools:
        if not shutil.which(executable):
            raise RuntimeError(f"Required executable not on PATH: {executable}")

    library = target["library"]
    run_info = {
        "seed": args.seed,
        "execution_mode": target["execution_mode"],
        "target": {
            "execution_mode": target["execution_mode"],
            "platform": target["platform"],
            "arch": target["target_arch"],
            "word_size": target["word_size"],
            "float_bits": target["float_bits"],
            "avm_disable_jit": not target["jit"],
            "avm_enable_precompiled": target["precompiled"],
            "avm_use_32bit_float": target["float32_mode"],
        },
        "runtime": str(target["runtime_path"]),
        "runtime_sha256": hashlib.sha256(target["runtime_path"].read_bytes()).hexdigest(),
        "library": str(library),
        "library_sha256": hashlib.sha256(library.read_bytes()).hexdigest(),
    }
    if target["wasm_path"] is not None:
        run_info["wasm"] = str(target["wasm_path"])
        run_info["wasm_sha256"] = hashlib.sha256(target["wasm_path"].read_bytes()).hexdigest()
    (work / "run.json").write_text(json.dumps(run_info))
    if args.replay:
        selected = json.loads(args.replay.read_text())
        # Accept either cases.json/minimized.json or the richer failures.json.
        selected = [entry.get("case", entry) for entry in selected]
        for entry in selected:
            if "operation" in entry:
                entry["expression"] = expression(entry)
    else:
        selected = cases() if lynx else []
        if args.lean_runtime:
            selected += lean_runtime.cases()
        if args.float32:
            selected += lean_runtime.float32_cases()
        selected += generate(args.seed, args.generated, args.float_encoding)
    if not selected:
        raise RuntimeError("No cases selected; use --generated, --replay, --lynx, --lean-runtime, or --float32")
    print(f"Seed {args.seed}; {len(selected)} cases; build {build}", flush=True)
    entries = []
    for index, entry in enumerate(selected):
        module = f"lynx_probe_{index}"
        entries.append({**entry, "module": module})
        (work / f"{module}.erl").write_text(
            f"-module({module}).\n-export([actual/0,start/0]).\n"
            f"actual() -> try {{ok,begin {entry['expression']} end}} "
            "catch Class:Reason -> {exception,Class,Reason} end.\n"
            "start() -> Result = actual(), "
            "erlang:display_string(stdout, <<\"LYNX_SEMANTIC_DONE\\n\">>), "
            "differential:print_result(Result), ok.\n"
        )
    if args.lean_runtime:
        report = lean_runtime.inventory(args.lean_runtime.resolve(), entries)
        (work / "lean-coverage.json").write_text(json.dumps(report, indent=2))
        print("Lean-inspired public-API samples; no Lean proofs or schedule coverage implied.", flush=True)
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
    arch = target["arch"]
    mode_label = f"{target['execution_mode']}/{arch}" if arch else target["execution_mode"]
    print(f"Checked {manifest['laws']} laws; running {len(entries)} cases ({mode_label}).", flush=True)
    support = manifest.get("support", [])
    fixture_beams = [work / "differential.beam"] + [
        work / f"{name}.beam" for name in manifest["fixtures"] + support]
    libraries = [library]
    precompile_only = target["precompiled"] and not target["jit"]
    if precompile_only:
        # The aggregate native archive still embeds bytecode eavmlib modules.
        # Prefer its matching native archive before the aggregate is searched.
        eavmlib = build / "libs/eavmlib/src" / f"eavmlib-{arch}.avm"
        if not eavmlib.is_file():
            raise RuntimeError(f"Build the matching eavmlib library first: {eavmlib}")
        libraries.insert(0, eavmlib)
        run_info.update(eavmlib=str(eavmlib),
                        eavmlib_sha256=hashlib.sha256(eavmlib.read_bytes()).hexdigest())
        (work / "run.json").write_text(json.dumps(run_info))
    if support:
        # Protocol implementations are fixture code; Elixir runtime dependencies
        # must still come from AtomVM's library rather than the host Elixir.
        elixir_library = build / "libs/exavmlib/lib/exavmlib.avm"
        if not elixir_library.is_file():
            elixir_library = target["host_build"] / "libs/exavmlib/lib/exavmlib.avm"
        if not elixir_library.is_file():
            raise RuntimeError(f"Build the bundled Elixir library first: {elixir_library}")
        libraries.append(elixir_library)
        run_info.update(elixir_library=str(elixir_library),
                        elixir_library_sha256=hashlib.sha256(elixir_library.read_bytes()).hexdigest())
        (work / "run.json").write_text(json.dumps(run_info))
    drivers = [work / f"{entry['module']}.beam" for entry in entries]
    if precompile_only:
        native_dir = work / "native"
        native_dir.mkdir()
        elixir_beams = []
        if support:
            elixir_dir = work / "elixir"
            elixir_dir.mkdir()
            command([str(target["packbeam"]), "extract", "--out",
                     str(elixir_dir), str(elixir_library)], work, work / "extract-elixir.log")
            elixir_beams = sorted(elixir_dir.glob("*.beam"))
        command(
            ["erl", "-pa", str(build / "libs/jit/src/beams"), "-noshell",
             "-s", "jit_precompile", "-s", "init", "stop", "--", arch, str(native_dir) + "/"]
            + [str(p) for p in fixture_beams + drivers + elixir_beams],
            work, work / "precompile.log",
        )
        fixture_beams = [native_dir / p.name for p in fixture_beams]
        drivers = [native_dir / p.name for p in drivers]
        if support:
            native_elixir = work / f"exavmlib-{arch}.avm"
            command([str(target["packbeam"]), "create", "--lib", str(native_elixir)]
                    + [str(native_dir / p.name) for p in elixir_beams],
                    work, work / "pack-elixir.log")
            libraries.pop()  # Replace the bytecode-only exavmlib added above.
            libraries.insert(0, native_elixir)
            run_info.update(native_elixir_library=str(native_elixir),
                            native_elixir_library_sha256=hashlib.sha256(native_elixir.read_bytes()).hexdigest())
            (work / "run.json").write_text(json.dumps(run_info))
    actuals = []
    for entry, driver in zip(entries, drivers):
        pack = work / "case.avm"
        log_path = work / f"{entry['module']}.log"
        command(
            [str(target["packbeam"]), "create", str(pack), str(driver)]
            + [str(p) for p in fixture_beams],
            work, work / "pack.log",
        )
        try:
            output = command(
                target["runtime_argv"] + [str(pack)] + [str(p) for p in libraries],
                work, log_path, timeout=args.timeout,
            )
            lines = [line[len("LYNX_RESULT "):] for line in output.splitlines() if line.startswith("LYNX_RESULT ")]
            if len(lines) != 1:
                raise RuntimeError(f"Expected one result, got {len(lines)}: {output}")
            encoded = bytes(json.loads(lines[0]))
            actuals.append({
                "status": "ok",
                "stage": "complete",
                "timeout": False,
                "value": base64.b64encode(encoded).decode(),
            })
        except CommandTimeoutError as error:
            stage = "transport" if "LYNX_SEMANTIC_DONE" in error.output.splitlines() else "semantic"
            actuals.append({"status": "error", "stage": stage, "timeout": True, "message": str(error)})
        except (RuntimeError, ValueError, TypeError) as error:
            log_text = log_path.read_text() if log_path.is_file() else ""
            stage = "transport" if "LYNX_SEMANTIC_DONE" in log_text.splitlines() else "semantic"
            actuals.append({"status": "error", "stage": stage, "timeout": False, "message": str(error)})
        if len(actuals) % 20 == 0:
            print(f"Executed {len(actuals)}/{len(entries)} cases", flush=True)
    (work / "actuals.json").write_text(json.dumps(actuals))
    # Compare deserialized terms on OTP, not using AtomVM's comparator.
    compare_host = ["mix", "run", "--no-compile"] if lynx else ["elixir"]
    print(command(compare_host + [helper, "compare", str(work)], lynx or work, work / "comparison.log"), end="")
    summary = json.loads((work / "summary.json").read_text())
    if args.lean_runtime:
        report = lean_runtime.inventory(
            args.lean_runtime.resolve(), entries,
            json.loads((work / "failures.json").read_text()),
            json.loads((work / "unverified.json").read_text()),
        )
        (work / "lean-coverage.json").write_text(json.dumps(report, indent=2))
    if summary["failed"]:
        failures = json.loads((work / "failures.json").read_text())
        if args.shrink:
            shrink(args, work, failures[0])
    if summary["failed"] or summary["unverified"] or (summary["uncovered"] and not args.replay):
        raise RuntimeError(
            f"{summary['failed']} differential failures, {summary['unverified']} unverified cases, "
            f"{summary['uncovered']} uncovered exports; see {work / 'summary.json'}"
        )


def etf_result_contract(encoded):
    if not encoded:
        return ("none",)
    try:
        data = base64.b64decode(encoded)
    except Exception:
        return ("invalid",)
    if len(data) < 3 or data[0] != 131:
        return ("invalid",)
    if data[1] != 104:
        return ("term",)
    arity = data[2]
    pos = 3
    atoms = []
    for _ in range(min(arity, 2)):
        if pos >= len(data):
            break
        tag = data[pos]
        if tag == 119 and pos + 2 <= len(data):
            length = data[pos + 1]
            pos += 2
        elif tag in (100, 118) and pos + 3 <= len(data):
            length = (data[pos + 1] << 8) | data[pos + 2]
            pos += 3
        else:
            break
        atoms.append(data[pos:pos + length].decode("utf-8", errors="replace"))
        pos += length
    if atoms and atoms[0] == "ok" and arity == 2:
        return ("ok",)
    if len(atoms) >= 2 and atoms[0] == "exception" and arity == 3:
        return ("exception", atoms[1])
    return ("tuple", arity)


def same_failure_contract(original, reduced):
    if original["case"].get("operation") != reduced["case"].get("operation"):
        return False
    if etf_result_contract(original.get("expected")) != etf_result_contract(reduced.get("expected")):
        return False
    orig_actual = original.get("actual", {})
    red_actual = reduced.get("actual", {})
    if orig_actual.get("status") != red_actual.get("status"):
        return False
    if orig_actual.get("stage") != red_actual.get("stage"):
        return False
    if orig_actual.get("timeout") != red_actual.get("timeout"):
        return False
    if orig_actual.get("status") == "ok":
        if etf_result_contract(orig_actual.get("value")) != etf_result_contract(red_actual.get("value")):
            return False
    return True


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
            if args.host_build:
                command_line.extend(["--host-build", str(args.host_build.resolve())])
            if args.float32:
                command_line.append("--float32")
            try:
                command(command_line, work, work / "shrink.log")
            except RuntimeError:
                # Compilation/reference/unverified failures are not evidence of the reduced VM mismatch.
                unverified = json.loads((trial / "unverified.json").read_text()) if (trial / "unverified.json").exists() else []
                if not unverified and (trial / "failures.json").exists():
                    reduced = json.loads((trial / "failures.json").read_text())
                    if reduced and same_failure_contract(failure, reduced[0]):
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
    parser.add_argument("--lean-runtime", type=Path,
                        help="Also sample public behaviors from this checkout's Lean/LynxTest inventory")
    parser.add_argument("--seed", type=int, default=1, help="Reproducible generator seed (default: 1)")
    parser.add_argument("--generated", type=int, default=1000, help="Generated cases (default: 1000; 0 disables generation)")
    parser.add_argument("--float-encoding", action="store_true", help="Focus generated cases on float16/32 encoding boundaries")
    parser.add_argument("--float32", action="store_true", help="Include IEEE-754 binary32 cases with float32 expectations")
    parser.add_argument("--replay", type=Path, help="Replay cases.json, failures.json, or minimized.json instead of generating")
    parser.add_argument("--shrink", action="store_true", help="Try up to 30 reductions of the first generated failure")
    parser.add_argument(
        "--build", type=Path, default=Path(__file__).resolve().parents[3] / "build",
        help="Existing AtomVM CMake build (generic_unix or emscripten)",
    )
    parser.add_argument(
        "--host-build", type=Path, default=None,
        help="Host AtomVM build providing PackBEAM and pre-built target libraries for cross targets",
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
