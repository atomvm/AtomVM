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

Requires Python 3.9+, `erl`, `erlc`, and `elixir` on PATH. Lynx, Lean, Mix, and
AtomVM's `spawn_executable` support are **not required** for generated tests.
The runner installs nothing, builds no AtomVM targets, and edits no repository
sources. Validation currently uses OTP 29. A nonzero exit means a mismatch,
runtime error/timeout, or a failed prerequisite/reference/proof check.

## Generated inputs

The default is 1,000 cases, reproducible with `--seed`. Sixteen fixed boundary cases
precede seeded exploration so short runs still exercise known dangerous edges.
This generates inputs to five operation families, not arbitrary Erlang programs:

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
- Float encoding: float16/32 halfway values and their adjacent double values,
  subnormal/normal and overflow boundaries, signed zero, both signs and byte
  orders, and aligned/unaligned segments. Full bitstrings and extracted payload
  bytes are compared, without decoding floats that could hide rounding errors.

Inputs cross a module boundary before operations execute, preventing OTP's
compiler from precomputing the expressions being tested. OTP and AtomVM use the
same compiled probe/operation BEAMs. Each reference runs in a fresh OTP process;
each target case runs in a separate AtomVM process with a timeout. Serialized
results are compared **on OTP**, so AtomVM's own comparator cannot hide a bug.
Exceptions compare class/reason, not stack frames. Standalone generated tests
use AtomVM's bundled standard library, without packaging OTP's `lists.beam`.

## Focused float encoding checks

```sh
python3 tools/test/lynx/validate.py --build build --float-encoding \
  --seed 1 --generated 256 --output /tmp/atomvm-float-encoding
```

This mode starts with 189 fixed encoding/error cases, then samples those values
with seeded offsets and byte orders. Use at least 189 cases for the entire fixed
matrix; a six-case smoke run includes float16's `6546.0`, `1.0e-6`, and
`6546.0001`, plus float32 even/odd and underflow ties. These detect the known
float16 rounding defect fixed by [PR #2457](https://github.com/atomvm/AtomVM/pull/2457).
The regular mixed generator includes those six smoke cases too.

Both modes use runtime arguments, including a dynamic float-segment width.
Overflow is checked as infinity's **encoded bytes**, not by constructing an OTP
infinity term. Invalid widths compare exception class/reason. Encoding failures
are neither skipped nor allowlisted: an unfixed runtime should exit nonzero.
`--shrink` reduces the float value while preserving width, offset, and byte order.

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

Seeds reproduce a given generator revision; use saved case files to compare
builds across generator changes. `--replay` overrides generated-case selection,
including `--float-encoding`.

`--shrink` attempts at most 30 reductions of the first generated failure and
writes `minimized.json`. It simplifies numbers and nested keys and reruns each
candidate against both real runtimes; compilation/oracle errors are not accepted
as reductions. It preserves the distinction between value mismatches and target
errors. This is a bounded greedy reduction, **not a guarantee of a globally
minimal case or the same underlying root cause**. Original failures remain intact.
Replay files are trusted local input: they contain Erlang expressions to execute.

## Lean runtime test inventory

Sample additional public behaviors inspired by `Lean/LynxTest`, without running
Lean, Mix, or proofs:

```sh
python3 tools/test/lynx/validate.py --lean-runtime ../lynx --build build \
  --generated 0 --output /tmp/atomvm-lean-runtime
```

At the pinned Lynx revision below, this selects **2,067 deterministic-result cases**:
numeric BIFs, rounding/conversions/formatting, tuple/list/bitstring APIs, binary
search/split, maps, process dictionaries, comparison, effectful short circuit,
timeout validation with retained mailboxes, monitored process/reference/dictionary
handshakes, lifecycle combinations, and UTF-8/16/32 construction/matching. UTF cases
include malformed, incomplete, unaligned, and valid codepoints with invalid suffixes. Child identities
are compared within each run, never serialized as cross-runtime expectations.
This mode uses **AtomVM's bundled standard library**, not OTP's `lists.beam`.
Missing functions and differing error precedence remain failures, not skips.
OTP is the execution oracle, not the Lean model: for example, tested OTP 29
accepts an already queued receive match before checking an invalid timeout.
That differs from the model test named `timeout_validation_precedes_scan`.

The lifecycle/receive cases cover linked normal/abnormal exits,
unlinking, trapped exits, untrappable `kill`, demonitor option validation and
active/queued/consumed monitor states, standalone and monitor aliases, pending
monitors while both the target and observer terminate, and empty-mailbox timeout
errors at and beyond the 32-bit boundary. Positive timer waits without a possible
sender exercise exception/cleanup scopes after suspension; no elapsed duration
or cross-sender message ordering is compared. Linked crashes are contained in
monitored workers. Queued-DOWN flush recipes requeue a received real DOWN so
mailbox presence is deterministic rather than inferred from a delay.
Monitor target-type cases compare both arities, all three alias options, and
self/live/dead PIDs versus registered process names for process and port monitors.
They distinguish direct wrong-type `badarg` from a registered name's `noproc` DOWN,
including retained explicit aliases. These cases do not create live ports; the
permanent echo-driver regression separately covers live/dead port targets.
Thirty registered-name rebinding cases keep monitors on both the original and
replacement processes, terminate them in both orders, and check that the original
process's termination does not unregister its replacement. Plain monitors and all
alias modes cover keeping, demonitoring, unaliasing, and replying to the original
reference without changing the replacement monitor. Exit reasons distinguish
the two processes; no cross-sender arrival order or raw identity is compared.
Twelve self-link cases check repeated `link(self())` and `unlink(self())` with
and without existing child links and trapped exits, including zero-iteration
controls, explicit GC, and retained mailbox messages. A self-link must be a
successful no-op, not an extra entry in `process_info/2`'s link list.
Forty-eight relinking cases exercise pending unlink acknowledgements from both
endpoints, with duplicate active links, zero/one/16 iterations, trapped and
untrapped exits, normal and abnormal reasons, and a final linked/unlinked state.
FIFO handshakes check both endpoints after earlier signals and acknowledgements;
the final exit checks whether the restored link actually propagates termination.
Nine unlink-race cases sample normal and abnormal peer termination over one/16/64
iterations. Each checks an allowed-outcome predicate: an abnormal linked exit may
kill the worker before unlink completes, but must not kill it after `unlink/1`
returns. The recipes compare neither scheduling order nor outcome frequencies;
repetition increases the chance of exposing a race, not exhaustive schedule coverage.
Thirty-six crossed-unlink cases run unlink/relink combinations at both endpoints,
then fence peer operations and their acknowledgements before checking agreement.
Mixed combinations accept either settled link state, not a particular winner.
Fresh linking from either endpoint additionally checks that an old pending unlink
does not suppress the new link. These extend the model-inspired link samples;
they are not an exhaustive schedule exploration or translated Lean proofs.
Forty-eight self-exit cases distinguish catchable `exit/1` exceptions from
`exit(self(), Reason)` signals, with both trap settings, linked/unlinked observers,
and normal, kill, killed, and structured reasons. They check catch/cleanup effects,
trapped self-messages, termination before return, and monitor/link exit reasons.
The child uses a small heap; raw process identities and cross-signal arrival order
are not compared. These extend the model-inspired exception and signal samples.
Thirty-six explicit-exit cases test linked targets and targets just unlinked by
the sender, with both trap settings and the same six exit reasons. Explicit
`exit/2` signals must still arrive after unlinking, unlike linked-exit propagation.
Fenced cases check both endpoints' link lists before sending; unfenced cases omit
the peer handshake without assuming an unlink acknowledgement is still pending.
Monitor and linked-exit notifications are awaited independently. The original six
never-linked explicit-exit cases remain as controls.
Forty-eight trap-flag transition cases change `trap_exit` before linked or explicit
exits arrive, or after explicit/self-exit signals become queued messages. Both
final flag settings and the same six reasons are covered, including unchanged-flag
controls and the previous flag returned by `process_flag/2`. Same-sender markers
fence signal reception before queued-message tests change the flag; the actual
EXIT remains unconsumed, not reconstructed or requeued. Turning trapping off must
not reinterpret an existing message as a fatal signal. An unfenced self-exit is
not treated as deterministic queued-message coverage. These cases extend the
model-inspired signal samples rather than translating a Lean proof.
Forty-two ownership cases pass standalone aliases, plain monitor references, and
all three monitor-alias types to a third process distinct from the monitored
target. Foreign `unalias/1` and both
`demonitor` arities must not cancel the owner's state or the caller's independent
monitor. Flush options are checked against distinct caller and owner mailboxes.
OTP explicitly permits `badarg` for cheaply detected foreign monitors; those
recipes check the documented allowed outcome and corresponding mailbox effects,
not identical ownership-check policies. Cross-process alias messages check normal
and one-shot delivery. The owner then removes its alias and checks whether its
monitor remains active; the caller's independent monitor observes real termination.
Mailbox contents are normalized without comparing cross-sender arrival order.

The function/apply/spawn subset contains 209 cases: captured local closures,
external funs and BIFs, dynamic `apply/2,3`, improper argument lists, wrong arities,
undefined exports/modules, `make_fun/3` and `is_function/2` boundaries, and child
success/error/throw/exit with monitors, links, and small heaps. Error payloads
retain within-run fun/argument equality without serializing fun identities or
compiler-generated names. Undefined-call recipes catch `undef` explicitly so
an expected child/call failure is verified, not classified as a missing oracle.

An additional 170 cases cover already-dead links with and without trapped exits,
live/dead process and registered-name queries, invalid liveness arguments,
`raise/3` classes and stack validation, and `keyfind/3`/`keymember/3` first-match,
numeric-equality, tuple-position, and improper-list behavior. Actual OTP results
remain the oracle: the model's exception for an invalid `raise/3` stack is not
used in place of OTP's returned `badarg` atom. Malformed external stack traces
and raw internal re-raise traces are distinct paths.

Another 21 cases exercise legacy MFA/fun frame normalization and already-modern
frames across all three exception classes, including mixed stacks and supplied
locations. Frame normalization preserves the caller's input and adds an empty
location to legacy frames; modern frames retain their supplied locations.

An additional 141 cases extend the raise recipes to VM backtrace-depth controls
(which the Lean model does not expose): defaults, zero depth, truncation, empty
traces, modern and legacy frames, discarded malformed suffixes, generated
non-tail call chains, child visibility, upper-bound clamping, and invalid flags.
Configured recipes restore the previous VM-wide setting even on exceptions.

Permanent dictionary-query and scoped-split GC-pressure regressions also live in
the ordinary Erlang library/runtime test suites; they are not Lean proof samples.

`lean-coverage.json` inventories unindented theorem/private-theorem declarations
and `#guard ` sites in all 23 test files, recording source hashes, lines, sample
counts, and matched/failed/unverified counts. It is a lexical inventory, not a
Lean parser: `#check`, examples, and other macros are not counted. `lean_origins`
links samples to their inspiration, **not exact theorem instantiations**; some
samples extend to invalid inputs outside the theorem premises. Do not interpret
the number of linked declarations as proof, branch, or complete API coverage.
Unselected declarations remain `no_samples`, not implicit passes; they do not
make the run fail. Renamed/missing attributed declarations do fail validation.

Kernel audits, API snapshots, purity/fixed-point/exhaustion proofs, internal
model representations, all-schedule claims, and deadlock diagnostics do not have
ordinary deterministic VM-test equivalents. Their inventory notes explain those
boundaries. Lifecycle samples are not exhaustive signal/scheduler coverage;
distributed processes and ports are outside these recipes. Many executable
behaviors still need more recipes. Standalone runs check **zero fresh laws**.

The flag is additive with `--lynx` and generated cases. All modes use AtomVM's
bundled standard library. A replay selects only saved cases; include
`--lean-runtime ../lynx` to regenerate its scoped
source inventory. Like other modes, real mismatches or incomplete references
produce a nonzero exit and retain evidence.

## Optional Lynx checks

Also check laws and a boundary-driven call corpus from an existing
[Lynx](https://github.com/josevalim/lynx) checkout:

```sh
python3 tools/test/lynx/validate.py --lynx ../lynx --build build --generated 0 \
  --output /tmp/atomvm-lynx-fixtures
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

The corpus contains **2,131 calls covering all 191 exports in 12 fixture modules**
at the pinned revision. It includes:

- Literal pooling, numeric/map-key boundaries, recursive base/step/error cases,
  asymmetric nested traversals, closure captures, dynamic calls, and observable
  process-dictionary effects.
- Every type guard, with matching and nonmatching term types; repeated variables,
  map-pattern fallthrough, and short-circuit success/error paths.
- Exception scope, rethrowing, cleanup effects, success-clause failures, and
  callback return/throw/error/exit paths. Stack contents are not compared.
- Empty, valid, improper, mismatched-length, filtered, zipped, and strict
  comprehensions over lists, maps, binaries, and partial bitstrings. Unspecified
  map iteration order is normalized, not asserted as a portable guarantee.
- Signed/endian/unit bitstring construction and matching, dynamic and invalid
  sizes, numeric representation boundaries, float16/32 rounding boundaries,
  float16/32/64 decoding of normal/subnormal and infinity/NaN wire patterns,
  UTF encoding boundaries, surrogates, invalid/truncated sequences, and tails.
- Selective receive, guards, aliases, repeated variables, nested receives, and
  valid/invalid timeouts. Queued messages are deterministic; both the selected
  result and remaining mailbox are checked, without asserting elapsed timing.

`coverage.json` inventories the **actual compiled exports**, with selected,
matched, failed, and unverified call counts per export. A full fixture run exits
nonzero if an export lacks a recipe, so updating Lynx cannot silently leave new
exports untested. This is **direct-call coverage, not branch or instruction
coverage**, and does not prove the VM correct. Some branches are unreachable or
cannot be selected through the public fixture API. Lean's unbounded integers
also differ from AtomVM's finite integer capacity.

OTP reference timeouts and undefined calls are recorded in `unverified.json`,
separate from AtomVM differential failures in `failures.json`. Neither counts as
a pass, and either produces a nonzero exit. Other cases continue, and bounded
target execution is still recorded for unverified cases. In the tested OTP 29,
both `sized_generator(0, Bits)` and `zero_generator(Bits)` time out for empty and
nonempty inputs: four explicit unverified cases, not four AtomVM mismatches.
Consequently even a runtime with all known fixes does not yield an all-green
full fixture run on that OTP. No failures are allowlisted.

To rerun fixture failures, include `--lynx` so their modules are packaged:

```sh
python3 tools/test/lynx/validate.py --lynx ../lynx --build build \
  --replay /tmp/atomvm-lynx-fixtures/failures.json
```

Partial replays still report uncovered exports, but these do not cause a failing
exit: only the selected cases are being validated. `unverified.json` is also
replayable. Host executable-process ports are not part of this corpus.

**Lynx-mode dependencies:** fixtures use AtomVM's bundled standard library,
including `lists:sum/1`; no OTP library modules are substituted into the target.
Interpreter execution uses the exact proof-input fixture binaries. For a
JIT/precompiled build, the script transforms those same binaries
to native-only code using that build's JIT compiler. It selects the matching
native runtime library automatically. Cross-target/emulator runs are not supported.

Use `--timeout` to adjust the default five-second bound per runtime call. Host
compile/proof/compare commands have a ten-minute bound. Both modes support
interpreter and native-only JIT/precompiled execution with the matching runtime
library. Neither covers on-demand JIT compilation, scheduler interleavings, GC
stress, or embedded platforms. No CI workflow or Lynx dependency is added to the
normal AtomVM build/tests. Run `--help` for options.

The runner's CLI integration tests check seeded replay, independent encoding byte
oracles, deliberately wrong/malformed target output, reduction and replay, and
continuation after reference timeouts/undefined calls. They also check scoped Lean
sample inventories, unverified counts, stale source attribution rejection, and
independent Unicode/short-circuit/process/receive result oracles:

```sh
python3 -B -m unittest discover -s tools/test/lynx -p test_validate.py -v
```

Set `LYNX_CHECKOUT=/path/to/lynx` to enable the additional full-fixture/export
inventory and partial-replay integration test. It requires Mix/Lean on PATH and
takes several minutes; ordinary integration tests do not require Lynx.
