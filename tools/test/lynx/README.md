<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Host-driven differential testing

From the AtomVM checkout, use a freshly built native AtomVM:

```sh
python3 tools/test/lynx/validate.py --build build --seed 1 --generated 1000 \
  --output /tmp/atomvm-differential-1 --shrink
```

Requires Python 3, `erl`, `erlc`, and `elixir` on PATH. Lynx, Lean, Mix, and
AtomVM's `spawn_executable` support are **not required** for generated tests.
The runner installs nothing, builds no AtomVM targets, and edits no repository
sources. Validation currently uses OTP 29. A nonzero exit means a mismatch,
runtime error/timeout, or a failed prerequisite/reference/proof check.

## Generated inputs

The default is 1,000 cases, reproducible with `--seed`. Ten fixed boundary cases
precede seeded exploration so short runs still exercise known dangerous edges.
This generates inputs to four operation families, not arbitrary Erlang programs:

- Numeric comparison: all eight equality/order operators, signed zeros,
  subnormal/extreme finite floats, and integer/float pairs around precision and
  representation boundaries, including 2^53 and 2^63.
- Integer arithmetic, division/remainder, shifts, bitwise operations, and floating
  division. Generated integer results stay within AtomVM's documented 256-bit
  magnitude; divide-by-zero errors are compared, not silently discarded.
- Maps: scalar, tuple, map, and partial-bitstring keys; duplicate and signed-zero
  keys; insertion, lookup, removal, update, and comparison; sizes near 32 entries.
- Bitstrings: signed big/little-endian construction and matching at varied bit
  offsets and widths through 255 bits, including zero-width and invalid-size
  controls. Full bitstrings and decoded results are compared.

Inputs cross a module boundary before operations execute, preventing OTP's
compiler from precomputing the expressions being tested. OTP and AtomVM use the
same compiled probe/operation BEAMs. Each reference runs in a fresh OTP process;
each target case runs in a separate AtomVM process with a timeout. Serialized
results are compared **on OTP**, so AtomVM's own comparator cannot hide a bug.
Exceptions compare class/reason, not stack frames. Standalone generated tests
use AtomVM's bundled standard library, without packaging OTP's `lists.beam`.

## Replay, shrinking, and evidence

Use `--output` with a new or empty directory to retain generated sources/BEAMs,
case definitions, OTP references, runtime logs, summaries, and failure records.
`run.json` records the runtime/library paths and SHA-256 hashes. Without
`--output`, successful runs clean their temporary directory; **failed runs keep
it and print its path**.

Replay all cases, just failures, or a reduced case, without regenerating inputs:

```sh
python3 tools/test/lynx/validate.py --build build \
  --replay /tmp/atomvm-differential-1/failures.json
```

`--shrink` attempts at most 30 reductions of the first generated failure and
writes `minimized.json`. It simplifies numbers and nested keys and reruns each
candidate against both real runtimes; compilation/oracle errors are not accepted
as reductions. It preserves the distinction between value mismatches and target
errors. This is a bounded greedy reduction, **not a guarantee of a globally
minimal case or the same underlying root cause**. Original failures remain intact.
Replay files are trusted local input: they contain Erlang expressions to execute.

## Optional Lynx checks

Also check laws and selected calls from an existing
[Lynx](https://github.com/josevalim/lynx) checkout:

```sh
python3 tools/test/lynx/validate.py --lynx ../lynx --build build --generated 0
```

With `--lynx`, `mix` and `lake` must also be on PATH with the Elixir/Lean versions
required by that checkout. Mix/Lake may build their normal caches. The fixture
API currently targets Lynx revision `e3d8c68281058d21a31aa5ac8e6a7e0b83db4aeb`.

This additional mode:

1. Compiles Lynx's existing Erlang translation fixtures once, with debug info.
2. Freshly verifies the fixtures that declare laws, rejecting proof errors or
   cached verification results. These are proof-backed fixtures; the other
   fixtures provide differential tests, not proofs.
3. Runs selected calls in isolated OTP processes, then runs the same compiled
   fixture binaries in separate AtomVM processes. Each call has a timeout.
4. Serializes actual results and compares them on OTP, so AtomVM's own equality
   implementation cannot mask mismatches. Exceptions compare class and reason,
   not stack frames. Cases returning references compare observable properties,
   not runtime-specific identities.

The 125 selected fixture calls cover numeric literals, signed zero, nested maps, recursion,
closures, exceptions, comprehensions, bitstrings, UTF, and selective receive.
Every selected case must match; failures and timeouts produce a nonzero exit.
The known zero-width generator hangs and host executable-process ports are not
in this selected corpus. This is not exhaustive fixture coverage or a proof of
the AtomVM implementation. Lean's unbounded integers also differ from AtomVM's
finite integer capacity.

**Lynx-mode dependency choice:** the package includes OTP's `lists.beam`, because Lynx's
`sum` fixture calls `lists:sum/1`, which AtomVM's bundled module lacks. This
checks the VM with that dependency, not full compatibility of the bundled
standard library. Interpreter execution uses the exact proof-input fixture
binaries. For a JIT/precompiled build, the script transforms those same binaries
to native-only code using that build's JIT compiler. It selects the matching
native runtime library automatically. Cross-target/emulator runs are not supported.

Use `--timeout` to adjust the default five-second bound per runtime call. Host
compile/proof/compare commands have a ten-minute bound. Both modes support
interpreter and native-only JIT/precompiled execution with the matching runtime
library. Neither covers on-demand JIT compilation, scheduler interleavings, GC
stress, or embedded platforms. No CI workflow or Lynx dependency is added to the
normal AtomVM build/tests. Run `--help` for options.

The runner's CLI integration tests check seeded replay and deliberately wrong
target output, including reduction and replay of the retained failure:

```sh
python3 -B -m unittest discover -s tools/test/lynx -p test_validate.py -v
```
