<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Expanding Lynx validation on AtomVM

Grow coverage by observable behavior, proof premises, and execution path, not
just by adding cases. Keep this plan current as batches land; keep commands and
runner semantics in [README.md](README.md).

## Current baseline

- The supported fixture revision is Lynx
  `ff3aad6f39e196ce539a73e1f51f8c3a1b69f68c`, requiring Lean 4.36.0; see the
  latest upstream refresh below.
- `fixtures.py` selects 2,351 calls with metadata for all 203 exports in 13
  translation-fixture modules. This is direct-call coverage, not branch coverage.
- `lean_runtime.py` selects 6,227 public-behavior recipes inspired by Lean tests
  and upstream examples (`369` of `478` Lean test sites across all 28 files). Source
  attribution is not an assertion of theorem premises or proof coverage.
- `--lynx` freshly checks nine fixture laws. Nineteen finite instances require
  the proved postcondition to be true on both VMs: five integer singleton sums,
  the empty sum, and thirteen protocol postconditions. Other matching
  differential results alone do not establish a property.
- Generated testing currently covers ten operation families: comparisons,
  arithmetic, maps, integer bitstrings, float encoding, map/dictionary
  sequences, selective-receive sequences, binary scope sequences, and
  process-lifecycle sequences. It does
  not generate arbitrary Erlang programs or enumerate scheduler interleavings.
- Dictionary queries, invalid receive timeouts, suspended receives, signal and
  alias combinations, and dictionary/scoped-split GC regressions already have
  coverage. Expand specific missing combinations rather than restarting these
  families. Short float formatting is outside the planned fixes.

The supported checkout has 28 Lean test files, including new concurrency audits
and `Modules/PersistentTerm.lean`. New model files do not imply runtime support:
`persistent_term` and `erlang:exit_signal/2` remain separately observed API gaps.
Fixture validation does not establish complete runtime-model support.

## Optimized Core evaluation order

Add 34 observations (2026-11-26), inspired by upstream Lynx commit
`32173b2475af4c469b25110704eeac9e59286bea`: 32 supplied Core bodies and two
ordinary Erlang-source controls. No new theorem attribution or runtime fix.

- The runner wraps each `core_body` in a companion `entry/0` module, invokes
  `erlc +from_core` with normal optimizations, then packages its BEAM for both
  VMs, including native precompilation. Driver expressions use `?CORE_MODULE`;
  saved-case/failure replays regenerate companions after filtering/reindexing.
- Compare values, exact error class/reason and ordered effects for direct and
  nested cons-head calls, `let`, sequence, case, shadowing/capture, local
  functions, `try`, tuple arguments and mixed associative/exact map updates.
  Prefix, head and tail failures distinguish dropped/reordered effects and
  exception precedence. Handwritten CLI oracles cover all 34 observations.
- OTP 29 assembly confirms direct Core cons tails run before their final head
  call; nested-call arguments run before the tail. A `try` head runs wholly
  after the tail. The source controls instead evaluate the head before the
  tail. Do not generalize raw Core rules to ordinary source Erlang.
- Default Core optimization simplifies the shadowed-let body before SSA, so
  that compiled case runs `g,h,f`, unlike upstream's direct translation of raw
  Core (`h,g,f`). Saved optimized Core records this distinction; these tests
  validate BEAM execution after compilation, not the Lynx translator itself.
- Hand-written map Core includes the base-type check normally supplied by
  source lowering. Omitting it makes OTP's BEAM validator reject the unchecked
  associative update; that invalid input is not a runtime differential case.
- Focused interpreter (`build`) and AArch64 precompiled
  (`.amp/in/build-native`, on-demand JIT disabled) initially matched 33/34 against
  OTP 29.1.3 (ERTS 17.1.2), with zero unverified cases. Fresh compilation and
  reference generation reproduce the OTP 29.1.2 results unchanged; evidence is
  under `.amp/in/artifacts/lynx-otp2913/` (interpreter/toolchain/harness) and
  `.amp/in/artifacts/lynx-core-order/otp2913-precompiled/` (native).
  All ordering traces agreed. One missing exact-map key raised `error:badarg`
  instead of OTP's `error:{badkey,a}`. Emitted assembly executes
  `put_map_assoc`, the tail effect, then `put_map_exact`; both opcode handlers
  explicitly raised `badarg` on a missing key. Ordinary Erlang
  `M=differential:exception_argument(#{}),M#{a:=1,b=>2}` reproduced the mismatch.
- Before the runtime correction, the failure stayed visible in `failures.json`
  and produced a nonzero runner exit; no expected-failure manifest entry was
  added. The CLI test requires all
  other scenarios to match and remains valid after a separate opcode fix.
  A single-failure native replay reproduces the same mismatch with a renamed
  companion; a reordered mixed Core/source replay matches 3/3. The complete
  harness CLI suite passes: 30 passed, 3 optional environment-gated skips
  (33 discovered). Python 3.9 grammar and `git diff --check` pass.
- All 6,193 prior recipes and metadata are unchanged. Total recipes are 6,227;
  Lean attribution remains 369/478 sites in 28 files. Evidence is under
  `.amp/in/artifacts/lynx-core-order/`, including Core/assembly, per-mode inputs,
  references, actual results, failure replays and build hashes. The full runtime
  corpus and embedded targets were not rerun for this batch.
- Separate runtime correction: both `put_map_exact` handlers now raise
  `{badkey, Key}`, retaining the key through exception allocation. Added `badkey`
  to the JIT's synchronized fixed-atom prefix. The existing Erlang `test_map`
  now checks exact class/reason for multi-key updates, empty maps, integer/float
  key distinction and a dynamically allocated compound key. It passed on OTP
  29.1.3 and failed on both unchanged AtomVM engines with `badarg`; after the
  fix it passes in interpreter, AArch64 precompiled and on-demand JIT execution.
  All 34 Core observations now match in all three modes. The JIT replay uses
  `--timeout 60 --jobs 2`: the default five seconds expires while compiling the
  large `differential` support module, before semantic completion. All five
  interpreter map test modules and the full harness (30 passed, 3 skipped)
  pass. Regression code generation also succeeds for x86_64, armv6m,
  armv6m+thumb2, arm32, riscv32, riscv64, xtensa and wasm32; those are not runtime
  execution claims. Fix, test and changelog are outside `tools/test/lynx` so
  they can be committed independently. Evidence:
  `.amp/in/artifacts/map-exact-exception/`.

## Compiler-folded versus runtime exceptions

Add 45 public observations (2026-11-26), inspired by upstream Lynx commit
`8558502245de2a5c725c8802446b8198e84b39bb` and its
`function clause failures raise function_clause alone` regression. Compiler-known
failed anonymous calls may become a literal Core `match_fail` tuple containing
arguments; the observable error reason must still be the atom `function_clause`.
This is upstream compiler-test provenance, not new Lean-site attribution.

- Fifteen scenarios cover atom, exact numeric, two-argument and map patterns;
  successful and false/erroring guards; a later matching clause; badfun and
  badarity controls; and matched-body `error`, `throw` and `exit`.
- Each scenario uses pure literal, effectful literal, and opaque runtime operands.
  A remote identity helper keeps dynamic funs and arguments opaque. OTP 29 does
  not fold failures with effects inside arguments, so pure literal calls record
  preceding effects separately; the effectful-literal path checks argument
  evaluation. Keep all three instead of assuming source literals imply folding.
- Optimized Core for the actual emitted literal drivers contains
  `{function_clause,b}` and `{function_clause,a,3}`; matching runtime drivers
  retain `call_fun/1,2`. Do not assert these incidental compiler shapes in the
  permanent behavioral tests.
- Compare exact class/reason and ordered effect traces. Only badarity's embedded
  fun identity is represented by a same-runtime equality check; its arguments
  remain exact. Handwritten public CLI expectations check all 45 results,
  including once-only argument/body effects and no body entry on clause failure.
- Focused interpreter (`build`) and AArch64 precompiled
  (`.amp/in/build-native`, on-demand JIT disabled) replays both match OTP 29 on
  all 45 cases, without failures or unverified cases. Inventory remains
  `369/478` sites in 28 files. No runtime fixes or expected-failure edits.
  The complete harness CLI suite passes: 29 passed, 3 optional environment-gated
  tests skipped (32 discovered); `git diff --check` is clean.
- Evidence: `.amp/in/artifacts/lynx-folded-exceptions/` contains replay inputs,
  final per-mode sources/manifests/results/build hashes, optimized Core and
  assembly, and CLI test output. This batch does not rerun the full runtime
  corpus, on-demand JIT or embedded targets.

## Signal-queue ordering fixes

Fix the two observed ordering differences (2026-11-26), without changing the
relink protocol or adding expected failures:

- Interpreter, precompiled and native-handler signal loops stop servicing a
  detached batch once a fatal signal marks the process killed. Remaining signals
  are requeued for teardown, which answers pending process-info requests only
  after removing the process from the process table and unregistering its name.
  Thus `undefined` cannot be followed by `is_process_alive(Pid) =:= true`.
- Termination handles local links before monitor notifications, independent of
  their creation order. A normal linked exit is handled before the child's
  `DOWN`, so enabling `trap_exit` afterward cannot expose that earlier exit.

Add public regressions to the existing `test_process_info` and `test_monitor`
modules: named/unnamed children, scalar/list process-info requests, liveness
before waiting for `DOWN`, and both trapping states with monitor-before-link
creation. Both tests failed against the original runtime. An intermediate
query-handler-only fix was rejected: precompiled execution caught `undefined`
followed by live liveness. The final fix defers the reply until actual teardown.
The lifecycle CLI test now requires both fixed families to match OTP; only the
separate relink family may remain a reported differential failure.

Final executed validation against OTP 29:

- Full interpreter corpus: 6,148 recipes plus 32 generated cases, seed
  `20261126`: 5,987 matches, 193 existing failures, zero unverified. The unchanged
  manifest accepts exactly 193/193, with zero unexpected failures or passes;
  all 24 signal-queue cases match.
- AArch64 precompiled: a fresh sequential subset matches all 24 cases. The first
  parallel run matches 23 and times out on named kill/query/64, with no value
  mismatches. Repeated direct executions reproduce that timeout. A control built
  with all four changed runtime files restored to baseline `94b3ece47` also times
  out on the same case (attempt 85), establishing a pre-existing timeout, not its cause.
  Thread samples show sleeping schedulers; debugger inspection of a fixed-build
  hang finds a waiting process with a nonempty outer mailbox. This is a separate
  unresolved finding, not proof of a shared cause with the earlier relink timeout.
- Interpreter core: both affected modules pass ten repetitions; all 483 core
  tests and mailbox/enif/struct tests pass. The two precompiled core modules each
  return zero on three runs. The native C test harness build is independently
  blocked by its existing missing `AVM_JIT_TARGET_ARCH_DIR` configuration; the
  native modules were packed and executed directly instead.
- CLI suite: 28 passed, three optional configurations skipped. Changed-line C
  formatting, Erlang formatting and whitespace checks pass.

Evidence: `.amp/in/artifacts/lynx-signal-fixes/`; final results have the `final-`
prefix and are distinct from rejected/intermediate runs. The original relink
and newly reproduced native kill/query timeouts remain unresolved, with no
allowlist additions or timeout increases. No full native corpus, fresh fixture
laws, on-demand JIT or embedded run is claimed. Runtime fixes, Erlang regressions
and individual changelog entries are committed separately with `-s` as
`0ffe3cfbd` (post-kill queries) and `b3abc9dfa` (exit-before-DOWN ordering).
A three-way cherry-pick check confirms the second commit applies independently
on the pre-fix baseline, without the first. Targeted core tests pass again after
committing. Lynx coverage and these validation notes are kept in a separate
commit from both runtime fixes. Nothing was pushed.

## Signal-queue differential boundaries

Initial coverage-only results, before the ordering fixes above.

Add 24 observations (2026-11-26), extending the runtime corpus from 6,124 to
6,148 recipes and attribution from 366 to 369 of 478 sites. Eight scenarios run
for 1/16/64 iterations, through a monitored worker:

- Kill then immediately query `process_info(Pid, registered_name)`, without a
  `DOWN`/liveness fence in between. Named and unnamed children have live controls;
  observations retain the live before-state, query result, subsequent liveness,
  termination reason and final name cleanup. The initial query before the kill
  ensures the child has handled a request, unlike the earlier spawn-then-kill
  exploratory control that passed.
- After a linked child's normal `DOWN`, enable `trap_exit` and poll for a late
  normal `EXIT`. A control traps before the child exits and must retain that
  `EXIT`; an unrelated sentinel must survive selective receives.
- Link, unlink, relink and kill without peer queries or acknowledgments between
  those operations. Require `EXIT(killed)` before the child's `DOWN`; a final
  unlink control must suppress it. Receive `DOWN` first, then check the queued
  `EXIT`, so assertions do not depend on wall-clock sleeps. Repetition creates
  scheduling opportunities but does not prove an unlink ack was pending.

The three new source sites are `process_info_after_kill_is_undefined`,
`trap_exit_after_normal_down`, and `stale_unlink_ack_keeps_new_link`. No proofs,
VM changes or allowlist entries were added. Existing runtime expressions remain
unchanged. The lifecycle CLI oracle test now independently specifies all 24
new reference results, retains strict checks for its 15 old observations, and
allows new differences to remain reported failures rather than freezing a
schedule-dependent failure count or requiring bugs to persist.

Executed against OTP 29 with the existing interpreter and AArch64 precompiled
builds (not on-demand JIT or embedded):

- Interpreter: 17 matched, seven failed, zero unverified. Six kill/query cases
  observe stale live information; the 64-iteration untrapped-normal case catches
  a late `EXIT(normal)` at iteration 7.
- Precompiled: 16 matched, eight failed, zero unverified. The same six kill/query
  cases and late normal exit differ. One 64-iteration linked relink case also
  hits the five-second semantic timeout. Three fresh sequential replays of that
  case with a 30-second guard all match; the initial timeout is retained and
  remains unexplained, not a diagnosed relink defect or dismissed as flakiness.
- All 24 OTP reference results are identical between the two mode runs. Live,
  pre-trapped, and final-unlinked controls match in both modes.
- Public CLI suite: 28 passed, three optional configurations skipped. The
  extended lifecycle test checks all 24 independent OTP oracles while preserving
  new target mismatches as failures. Erlang formatting, Python syntax, inventory
  attribution and whitespace checks pass.

These new failures are not covered by the unchanged 193-failure manifest: the
expanded runtime corpus is not an all-green CI baseline. The preceding full
runtime/fixture results below predate this batch; no new full-corpus run or
fresh law check is claimed. Evidence: `.amp/in/artifacts/lynx-signal-queue/`,
including replay inputs, both full subset runs, decoded differences and relink
replays. No runtime cause or fix is claimed from these observations alone.

## Latest upstream refresh

Rebaseline to `ff3aad6f39e196ce539a73e1f51f8c3a1b69f68c` (2026-11-26),
then add function-identity differential coverage:

- Upstream rewrote history. The previous `6968d72` checkout is preserved at
  local branch `backup/main-before-upstream-20261126` in the sibling repository.
  Its functional equivalent is `4e9147b`; 195 actual incoming commits were
  screened, rather than counting rewritten commits as new behavior. Descriptor
  example files were removed, so these baseline trees are not byte-identical.
- Remove attribution to the three deleted Signals sites
  `inactive_reference_is_forced`, `active_reference_is_not_forced`, and
  `ended_process_is_forced`. Preserve all 6,104 previous expressions, including
  all 82 recipes with obsolete attribution, and keep their valid other origins.
  Do not invent replacement proof premises or discard useful differential cases.
- Add 20 `function_identity/2` observations: local and external same-MFA
  functions at arities 0/1/2, `make_fun/3` equivalents, same-factory closures
  capturing local/external functions, and integer/float captures. Compare loose
  and exact equality, function type, application, exact membership versus loose
  key search, map-key coexistence/replacement/removal/take with originals retained,
  and dictionary writes/reads/erase. A monitored worker copies captures and
  explicitly collects with maps and dictionary keys live. Map sizes 2/31/32/33
  straddle OTP's flat-map boundary. No raw identities or opaque ordering escape.
- Attribute the new cases to `local_and_external_functions_differ` and
  `captures_compare_as_outer`. The inventory validates at 366/478 sites in
  28 files, up from 364 surviving attributed sites after removing the deleted
  three. These are finite samples, not theorem or branch coverage.

Validation on OTP 29 / Elixir 1.20.5 / isolated Lean 4.36.0:

- Lynx `mix test.all`: 172 passed (two laws, 170 tests), plus the Lean test build.
- Full translated fixtures: nine freshly checked laws, 2,347 matched, zero
  differential failures, four unverified zero-width-generator OTP references,
  and zero uncovered exports (203 exports in 13 modules). These four unresolved
  references still make the fixture command exit nonzero; they are not skips
  or successful comparisons.
- Full interpreter runtime corpus, seed `20261126`, 6,124 recipes plus 32
  generated cases: 5,963 matched, 193 known failures, zero unverified. The unchanged
  expected-failure manifest accepts exactly 193/193, with no unexpected failures
  or passes. Failures remain recorded rather than counted as matches.
- New function-identity subset: 20/20 match in both interpreter and aarch64
  precompiled execution, zero failures or unverified references. A public CLI
  regression also checks independently specified observations, not merely VM
  agreement. No full native, on-demand JIT, 32-bit, or embedded rerun is claimed.
- Public CLI suite: 28 passed, three optional configurations skipped. The full
  fixture command above was run separately. Erlang formatting, Python syntax,
  and whitespace checks pass.

Evidence: `.amp/in/artifacts/lynx-identity-baseline/` (runtime manifests, target
logs, fixture results, CLI and fresh upstream build logs). No AtomVM runtime or
standard-library behavior was changed, and no new proof machinery was added.

## Earlier upstream refresh

The clean sibling Lynx checkout was fast-forwarded through 60 commits from
`b9be5b8` to `2aa1467`. Useful executable changes and their coverage are:

- `3e5a28a`, `bbbe752`, `11a34d2`: consolidated protocol dispatch, recursive
  implementation calls, and cutoff handling. Add 78 calls covering all twelve
  exports of the new `protocols` fixture, including structs versus maps,
  fallback, undefined protocols, improper inputs, and thirteen law instances.
  Package Lynx's fourteen executable `Elixir.Lynx.Support.*` modules, using the
  consolidated modules loaded by Mix. Standard-library dependencies still come
  from AtomVM's bundled `exavmlib`, never host Elixir library beams.
- `5b681d8`: exact list subtraction. Add 25 observations covering first-match
  deletion, duplicates, integer/float distinctions, signed zeros, nested values,
  large integers, swapped operands, and improper-list/error paths.
- `46921a2`: zip/unzip round-trip example. Add sixteen observations with
  asymmetric lists, preserved order and duplicates, nested terms, length 33,
  and invalid inputs. These are labeled `upstream_example`, not attributed to
  nonexistent Lean runtime-test sites or freshly checked fixture laws.
- `17e3b47`, `02f2368`, `0883352`: timer semantics and remaining-time reporting.
  Add 28 public observations: delivery, cancellation races, repeated/expired
  cancellation, unknown references, destination death before/after creation,
  invalid arguments, and `send_after/4` options. Reference-tagged messages and
  monitor handshakes avoid short timing deadlines; remaining times are checked
  for range, not exact millisecond equality. A successful cancellation remains
  distinct from `false`. No scheduler interleavings or all-run proofs transfer.
- The empty-sum postcondition now joins singleton sums as a true assertion.
  Higher-order callbacks still need a valid exposed law input and remain deferred.

Type-descriptor proofs, symbolic environment/purity lemmas, protocol cutoffs,
partial-order scheduler reductions, and the new all-run statement of concurrent
laws are model/proof tooling, not additional AtomVM compatibility requirements.
The new exact-equality hashing lemma is useful future coverage, but `phash2`
remains a public API gap rather than an allowlisted success.

Lynx's own `mix test.all` passes: 165 checks (two laws and 163 tests), plus the
Lean test build. Source attribution validates against all 26 Lean test files.
Validation uses OTP 29 / Elixir 1.20.5 / Lean 4.35.0 and rebuilt host runtimes:

- Full fixtures: nine freshly checked laws, 2,347 matched, zero failures, four
  unverified zero-width-generator references, and zero uncovered exports. The
  four unresolved references still make this run exit nonzero.
- Protocol-only replay: all 78 match in both interpreter and aarch64 precompiled
  execution. The 191 unselected exports are reported, as expected for a replay.
- Full runtime interpreter corpus: 4,108 matched, 33 failed, zero unverified.
  All 4,072 original observations retain their previous decoded results,
  including the four known `[short]` float-format mismatches. The remaining
  29 failures come from the 69 new observations; 40 new cases match OTP.
- New runtime observations in aarch64 precompiled execution: 40 matched,
  29 failed, zero unverified. Both modes flag the same 29 new expressions.
  The full expanded native corpus was not rerun. All 25 subtraction cases match.
  Twelve zip/unzip failures expose missing `lists:unzip/1`. Seventeen timer differences comprise
  four destination-death cleanup cases, three nonreference cancellation cases,
  five invalid `send_after/3` arguments, and five missing `send_after/4` cases.
  Successful delivery/cancellation and expiry cases match. These are retained
  compatibility failures, not an all-green result or runtime fixes.

Native validation must search native eavmlib and transformed exavmlib before
`atomvmlib-<arch>.avm`: that aggregate still embeds bytecode copies of these
modules. The runner now loads the matching built eavmlib archive first and
precompiles the bundled exavmlib into the evidence directory when protocols are
selected. Library hashes are recorded. Initial runs without this packaging
aborted on timer calls and protocol exceptions; corrected runs above replace
those setup failures. No host standard-library modules are substituted.
This covers native-only precompiled code, not on-demand JIT or embedded targets.

All sixteen CLI integration tests pass: fifteen enabled by default and the
optional full-fixture/partial-replay test run separately with `LYNX_CHECKOUT`.
The new handwritten oracle checks cover subtraction results and timer mailbox,
cancellation, and expiry observations. Erlfmt 1.7.0, Mix formatting, Python syntax,
and `git diff --check` pass. Evidence is retained under
`.amp/in/artifacts/lynx-latest-*`: upstream tests, full interpreter corpora,
focused native replays, CLI tests, baseline comparisons, manifests and hashes.
No failures were hidden and no AtomVM runtime implementation was changed in
that upstream refresh.

### Timer argument validation follow-up

`timer_manager:send_after/3` now rejects invalid times and destinations in the
caller, before starting the manager or creating a timer worker. `erlang:send_after/3`
raises `badarg` while `timer:send_after/2,3` catches `badarg` and returns
`{error, badarg}` to match OTP. Destinations must be local PIDs
or atoms; nonreference cancellation arguments now raise `badarg`, while unknown
references still return `false`.

The supported delay range is explicitly 0 through 4294967295 milliseconds,
matching AtomVM's uint32 receive timeout. Modern OTP accepts larger delays;
rejecting those unsupported delays synchronously does not implement OTP's
extended timer range. Boundary tests cover zero, the maximum, and maximum + 1.
Public-API regressions also cover invalid types, foreign-node PIDs, registered
names, and unknown references. The new regression fails on the original code
and passes after the fix. The timer-manager, timer, gen_server, gen_statem, and
supervisor test modules all pass. The timer-manager boundary regressions also
pass in native-only precompiled execution, and the independent timer/subtraction
CLI oracle test passes. Erlfmt and `git diff --check` pass.

A replay of all 69 new observations in both interpreter and aarch64 precompiled
execution now matches 48 cases, with 21 failures and zero unverified references:
all eight argument-validation failures are fixed. The remaining failures are
12 missing-unzip cases, four destination-death cleanup cases, and five missing
`send_after/4` cases. The full 4,141-case corpus was not rerun after this fix.
Evidence is retained under `.amp/in/artifacts/timer-validation-*`.

### Destination-death cleanup follow-up

Timer workers now monitor PID destinations for both `send_after` and
`start_timer`. Destination death ends the worker and removes the timer using
the existing completion path, including replying `false` to a pending cancel.
Cancellation also checks PID liveness so a queued cancel cannot return remaining
time merely because the destination's `DOWN` has not been processed yet.
Registered-name timers are not tied to the current name holder and remain active
when that process dies.

Public regressions cover death before/after creation with zero-, one-, and
60000-millisecond delays, repeated cancellation, automatic removal without an
explicit cancel, and live registered-name timer controls. The regression failed
on the unchanged runtime. The timer-manager, timer, gen_server, gen_statem, and
supervisor modules pass after the change; timer-manager regressions also pass
in aarch64 precompiled execution.

Finite stress validation passes 1,000 ordered death cases and 500 two-canceller
races on OTP, interpreter AtomVM, and precompiled AtomVM. A separate forced
ordering queues cancellation before the worker installs its destination monitor:
a monitor-only variant incorrectly returns 60000, while the full fix returns
`false` in both AtomVM modes. These checks do not enumerate all schedules.

The 69-case focused replay now matches 52 cases in both AtomVM modes, with
17 failures and zero unverified references. All four destination-death failures
are fixed; the remaining failures are twelve missing-unzip and five missing
`send_after/4` observations. The full runtime corpus was not rerun. Evidence is
retained under `.amp/in/artifacts/timer-destination-death-*`.

### Expanded `send_after/4` differential cases

Replaced the five shallow option probes with 52 OTP-extension observations
(47 net new cases, 4,188 runtime cases total). These are explicitly labeled
`otp_extension`, not attributed to Lynx's symbolic timer proofs:

- Default and explicit relative options; duplicate options in both orders and
  repeated identical options. OTP 29 uses the last valid `{abs, Boolean}` value.
- Relative delivery at zero and 20 milliseconds; absolute past, current, and
  future deadlines; future and expired cancellation with repeated cancellation.
- Twelve malformed-option forms, including improper lists, nonboolean values,
  wrong tuple arities, unknown options, and invalid entries before/after a valid
  option. Later valid options do not hide an invalid earlier entry.
- Negative relative times, noninteger times in both modes, and invalid
  destinations, retaining OTP's `badarg` separately from missing API `undef`.

Absolute timestamps are constructed from each VM's monotonic clock. Past
cases age a timestamp actually observed during the VM's lifetime: subtracting
from a newly started OTP VM can precede `system_info(start_time)` and correctly
raise `badarg`. Delivery observations check the unchanged nested payload,
not-before-deadline timing, preservation of unrelated mailbox contents, and
repeated cancellation. Cancellation normalizes scheduler-dependent expiry
races and validates remaining-time bounds instead of comparing exact clock
values. The outer runner bounds non-delivery; no tight upper latency bound or
all-schedules guarantee is asserted.

The public CLI integration test independently checks handwritten OTP results
for all 52 cases. The CLI suite passes sixteen tests, with the optional Lynx
fixture integration test skipped because `LYNX_CHECKOUT` was not set. Focused
interpreter and aarch64 precompiled replays each
match 52 of 116 cases, with 64 failures and zero unverified references:
all 52 `/4` cases currently fail with `undef`, and the twelve existing unzip
failures remain. No runtime implementation changed and no failures are skipped.
The full 4,188-case corpus was not rerun. Evidence is retained under
`.amp/in/artifacts/timer-options/focused-{interpreter,native}`.

### `send_after/4` and `lists:unzip/1` follow-ups

Subsequent runtime fixes in `dbab35d88` (`send_after/4` with monotonic
deadlines) and `541596657` (`lists:unzip/1`) closed all 64 failures from the
116-case focused replay above. Both host-interpreter and aarch64 precompiled
replays matched all 116 OTP references, with zero failures and zero unverified
cases (`.amp/in/artifacts/unzip-{interpreter,native}`).

### Upstream fast-forward to `b6ac6ea`

Reviewed all 33 upstream commits from `2aa1467d4c28c4bffd5f5d205ebb197e261f85a4`
to `b6ac6ea905fa5b387e33cd0b4466a6abcd9d31de`:

- `protocols.erl` (and its generated `.json` and `.laws.lean`) only renamed
  `Lynx.Term.exactCompare_atoms` to `Lynx.Term.compareExact_atoms` inside the
  Lean proof string of `size_undefined` (`a882793`). No Erlang fixture clauses,
  exports (`203` exports across `13` modules), or `@law` preconditions/postconditions
  (`9` freshly checked laws, `19` `assert_true` cases) changed; `higher_order.erl`
  is unchanged.
- `Lean/LynxTest` site inventory across the 26 files changed from `426` to `428`
  sites (`200` sampled, zero stale attributions): `Modules/Process.lean` (`51 -> 49`)
  removed `handshake_every_schedule` and `sends_execute` and replaced
  `arbitrarily_many_sends` with `many_sends`; `Modules/Signals.lean` (`16 -> 20`)
  added four Lean model/partial-order-reduction theorems (`killed_root_keeps_environment`,
  `inactive_reference_is_forced`, `active_reference_is_not_forced`,
  `ended_process_is_forced`) whose public Erlang behaviors remain covered by the
  existing `Modules/Signals` and `Modules/Process` recipes.
- Remaining commits (`b839c75`..`b6ac6ea`) are Lean model/proof/translation updates
  (`signals in transit`, sleep-set/`forced` partial-order reduction, `lynx_kernel`
  replacing `lynx_cbv`, explicit fuel on `Term.compare`/`Term.compareExact`,
  extracted `prelude`/`postlude` files in `examples/elixir/descr/*.lean`, and an
  Elixir `Task.await_many/2` drain example in `examples/elixir/task.exs` where
  AtomVM `exavmlib` has no `Task` module).
- Verification: all 23 CLI integration tests in `test_validate.py` pass against
  `/Users/petermm/OSScontrib/lynx` at `b6ac6ea905fa5b387e33cd0b4466a6abcd9d31de`
  with `ATOMVM_JIT_BUILD=.amp/in/build-on-demand` (the 22 non-Lynx tests ran in
  one pass and `test_lynx_exports_have_cases_and_partial_replay_is_scoped` passed
  with Lean `4.35.0` and `LYNX_CHECKOUT=/Users/petermm/OSScontrib/lynx`), and
  `lean_runtime.inventory()` reports `200/428` sampled sites with zero missing
  attributions.
- Follow-up coverage expansion for `0956a05` / `97b8177` / `5f1ff5a`: adds 36
  cases (`4,413 -> 4,449`) across `differential:await_many_drain/1,2` (6
  `examples/elixir/task.exs#await_many_drains_replies` multi-worker drain
  scenarios plus 18 `Modules/Signals` variants across `reply_demonitor`,
  `explicit_unalias`, and `demonitor`) and `differential:many_sends/2` (12 cases
  for `0`, `1`, and `30` sends to `dead`, `live`, `self`, and `stale_alias`),
  bringing `lean_runtime.inventory()` coverage to `204/428` sites
  (`Modules/Process.lean` `15/49`, `Modules/Signals.lean` `18/20`). All 36
  cases match OTP 29 across interpreter (`build`), `aarch64` precompiled
  (`.amp/in/build-native`), and `aarch64` on-demand JIT
  (`.amp/in/build-on-demand`), and `test_validate.py` adds
  `test_await_many_and_many_sends_observations_have_independent_oracles`.

## Coverage batches

Current priority is more OTP differential coverage of existing fixtures and
public behavior. New proof-to-runtime integration in batch 2 is deferred.

### 1. Reconcile the Lynx revision and inventory

- [x] Compare the pinned revision with the newer checkout, including Erlang
  fixture sources, callback identity changes, generated translations, laws, and
  public runtime models. Do not silently move the supported revision.
- [x] Validate any revision update as its own change: fresh proof checks, compiled
  export inventory, existing corpus, source-attribution checks, and documented
  counts. Preserve the user's checkout and unrelated local changes.
- [x] Classify remaining sites as executable gaps, already represented behavior,
  model/proof-only contracts, or unsupported public APIs requiring separate work.
  Inspect the recipes before interpreting `no_samples` as missing behavior.
- [x] Recheck the four zero-width-generator OTP reference timeouts. Keep unresolved
  references explicit and replayable; never count them as passes or hide them
  with an allowlist. Record the OTP version if behavior changes.

Done when the supported revision and observed results agree, and the next
executable gaps are named without claiming branch or universal coverage.

Completed on 2026-11-10 with OTP 29 (ERTS 17.1.1, 64-bit), Elixir 1.20.5,
Lean 4.35.0, and a rebuilt host interpreter/library:

- Fresh fixture checks: four laws; 2,127 matched, zero mismatches, four unverified
  references; all 191 compiled exports in 12 modules have selected recipes.
- Runtime corpus (2,946 cases before the map-callback extension): 2,942 matched,
  four mismatches, zero unverified references.
  All four mismatches are `[short]` float formatting (`12300.0`, `0.00012`,
  `1.0e23`, `9.999999999999999e22`), left unchanged per scope. Both full modes
  therefore still exit nonzero; neither result is an all-green claim.
- All four zero-width generators still exceed the five-second OTP reference
  bound, on both empty and nonempty inputs. Target calls also time out. Their
  records remain replayable in `unverified.json`, not counted as matches.
- `mix test.all`: 151 passed (two laws and 149 tests), plus the Lean test build,
  including the new hashing/sets modules. The AtomVM fixture checks separately
  reject cached law results; passing model proofs does not prove VM behavior.
- CLI checks: eight passed, one optional full-fixture test skipped. The full
  fixture run above and a separate one-call replay exercised that integration:
  the replay matched and correctly reported 190 unselected exports without
  failing for partial coverage. Source attribution validates against all 25 files.

Local evidence is retained under `.amp/in/artifacts/lynx-rebaseline/`: corpus
sources/manifests, summaries, failures, unverified records, source hashes, build
and toolchain logs, upstream tests, API probes, and partial replay. The Lynx
checkout remains unchanged. No runtime fixes or new permanent recipes were
added in this batch.

#### Inventory review

The revision comparison found no changes to executable Erlang fixture clauses or
exports. `higher_order.erl` changes only its proof, following the new
module/name/arity function identities and compiler-generated callback ordering.
Generated translations and iterator adapters also changed; copying their
internal representations into VM expectations would not be portable.

At the batch-1 baseline, remaining work fell into four categories (the map
callback portion has since been sampled below):

- **Executable gaps:** `Modules/Maps.combine_inputs`, public iterator observations,
  map lookup/equality relations, portable hashing vectors, and version-2 set
  algebra. Hashing's five guard sites and Sets' four theorem sites currently have
  no attributed recipes; the latter includes deliberately unmodeled behavior,
  not four missing portable tests. Empty-sum assertions and callbacks satisfying
  the higher-order law's actual precondition remain batch 2.
- **Already represented behavior:** `Term/Try` has no attributed runtime recipes,
  but translation fixtures exercise try/of/catch/after. Construction/matching,
  short-circuit, map-pattern, and process sites likewise overlap existing fixture
  or runtime families. `no_samples` is not enough to establish a missing case;
  attribution counts are not a union of behavioral coverage across modes.
- **Model/proof-only contracts:** kernel audits, API snapshots, purity/fuel and
  fixed-point proofs, synthetic dispatch identities, raw heap padding,
  duplicate-binding map representations, and all-schedule/deadlock properties
  are not deterministic cross-VM expectations. Public observations can sometimes
  be sampled, without transferring the theorem's universal guarantee.
- **Confirmed public API gaps:** focused calls to `erlang:phash2/1,2` and
  `maps:intersect_with/3` return `undef` on the rebuilt AtomVM, while OTP succeeds.
  These are separate implementation work, not allowlisted passes. Sampled
  `maps:merge_with/3`, `maps:filter/2`, and explicit version-2 set construction,
  union and size with distinct integer/float elements match OTP. These few probes
  do not establish complete support for those families.

PR #2355 has since supplied `maps:intersect_with/3` and other map helpers;
`phash2/1,2` remain missing. See batch 3 for the expanded map observations.

The sets model ignores a requested version 1 and returns model exhaustion for
some unsupported representations. Neither behavior should become an AtomVM
compatibility requirement; public invalid-input behavior must use OTP as oracle.

### 2. Connect more proved properties to runtime assertions

- [x] Make the existing empty-sum postcondition an `assert_true` case, in addition
  to the five singleton instances. Test rejection when both VMs agree on false.
- [ ] Sample the higher-order map law with proper lists and callbacks that
  actually satisfy its requires predicate. Check the predicate on each VM before
  asserting the postcondition. A newly constructed identity closure is not
  automatically the same function value as a callback permitted by the law.
- [x] Add model-inspired map lookup/equality and list properties where the
  observable relation is useful. Label them separately from freshly checked
  fixture laws; do not fabricate proof provenance for a runtime assertion.
- [x] Cover empty, asymmetric multi-element, nested, and representable large
  integer inputs. Keep invalid inputs outside law premises as error-path tests.

Owners: `fixtures.py`, `validate.exs`, `test_validate.py`; small upstream Lynx
fixture changes may be needed to expose valid law inputs. Done when proof errors,
false preconditions, and false/nonboolean/exceptional postconditions fail clearly,
including replay, rather than being accepted as matching OTP.

Batch 2 model-inspired map/list relation expansion added 113 net new recipes
(`4,188 -> 4,301`, with 133 updated or added entries in the focused replay):

- Provenance remains split explicitly: 37 new `upstream_example` cases cover
  `examples/otp/lists.exs#sort_contract_holds` and
  `examples/otp/lists.exs#sequence_formula` (`sequence_formula` empty/ascending/descending/singleton/large-integer
  contracts and out-of-premise error paths), while 96 Lean-attributed map,
  list, guard, short-circuit membership, and comparison cases point strictly to
  verified `Lean/LynxTest` declarations. None fabricate `assert_true` proof
  provenance.
- Lean inventory validation covers 193 of 426 test sites across all 26 files
  (`Modules/Maps.lean` 26/26, `Modules/Lists.lean` 8/8, `Modules/Erlang.lean`
  60/76, `Term/Compare.lean` 15/16, `Term/ShortCircuit.lean` 9/13).
- Observations span empty, singleton, shadowed duplicate-binding, numeric exact
  vs semantic (`1`/`1.0`, `0.0`/`-0.0`), asymmetric multi-element, nested
  composite, representable large integers up to `(1 bsl 255) - 1`, map size
  boundaries (31, 32, 33), improper tails, and invalid-operand error paths.
- Focused 133-case replays on OTP 29 match 127 cases with 6 failures and zero
  unverified references in both host-interpreter and aarch64 precompiled
  execution:
  - Four `lists:seq/3` empty-step-range divergences (`seq_contract(5,4,2,[0])`,
    `seq_contract(10,7,5,[0])`, `seq_contract(4,5,-2,[0])`, and
    `seq_contract(7,10,-5,[0])`), where `libs/estdlib/src/lists.erl` returns
    `[First]` instead of `[]` when `abs(Step) > 1` and
    `0 < abs(Last - First) < abs(Step)`.
  - Two `lists:reverse/1` invalid-input exception differences (`[a|improper]`
    and `bad`), where AtomVM raises `badarg` and OTP 29 raises
    `function_clause`.
- Seventeen enabled CLI integration tests pass (18 total, with the optional
  `LYNX_CHECKOUT` fixture test skipped), including handwritten OTP oracles for
  `map_relations`, `map_singleton_iteration`, `map_guards`, `list_relations`,
  `seq_contract`, and error paths. No runtime code was modified; exposed
  failures remain visible and replayable under `.amp/in/artifacts/batch2-relations/`.

Subsequent `lists:seq/3` empty-step-range follow-up: `libs/estdlib/src/lists.erl`
now returns `[]` whenever `Incr > 0 andalso To < From` or
`Incr < 0 andalso To > From` after `badarg` range validation, instead of only
when `From =:= To + Incr`. The expanded `test_lists` regression fails before
the change and passes afterward on OTP 29, the host interpreter, and aarch64
precompiled execution. Replaying the 133 focused Batch 2 cases now matches 131
OTP references with 2 failures and zero unverified cases in both modes
(`.amp/in/artifacts/batch2-relations/seq-fix-{interpreter,native}`), leaving
only the two documented `lists:reverse/1` NIF `badarg` versus OTP
`function_clause` differences.

### 3. Expand public map combination and iterator behavior

- [x] Cover `maps:merge_with/3` and `filter/2`:
  empty/disjoint/overlapping maps, asymmetric callback arguments,
  integer-versus-float keys, callback arity/errors, and invalid operands.
- [x] Add `maps:intersect_with/3` after importing PR #2355's runtime support.
- [x] Check callback effects only where invocation is semantically required.
  Normalize unordered effects without erasing argument order or duplicate calls.
- [x] Add public iterator observations for empty, partially consumed,
  exhausted, and malformed iterators. Do not compare internal iterator layouts or
  invoke `erts_internal` model adapters as portable VM contracts.
- [x] Extend map-comprehension observations beyond the existing fixture recipes.
- [x] Include map sizes around 32 entries and callbacks retaining boxed values
  across allocation. First check which operations AtomVM exposes; missing APIs
  remain reported failures and get separate implementation decisions.

Map-callback portion added on 2026-11-10: 70 recipes attributed to
`Modules/Maps.combine_inputs`, including public-API extensions beyond its
theorem premises. Observations preserve callback arguments and multiplicity,
normalizing only traversal order. Sizes 31, 32, 33, and 40 use runtime-created
boxed values; this is not an additional forced-GC regression.

The initial focused host-interpreter run matched 51 cases and found 19 mismatches:
16 combiner-validation/error-precedence cases in `merge_with/3`, one nonboolean
filter-result case, and two filter callback-versus-map validation cases.
That initial coverage batch did not fix runtime behavior. Its evidence remains
under `.amp/in/artifacts/lynx-map-callbacks/`.

PR #2355 was then cherry-picked as five signed-off commits, preserving earlier
fold regressions alongside the imported iterator tests. It supplied missing
map APIs and resolved the three filter mismatches. Another 261 recipes now cover
intersection/filtermap, take, update_with, with/without, grouping and iterator
validation, bringing map-attributed coverage to 512 cases. Group callback order
is observed without sorting; unordered map callbacks still retain arguments and
multiplicity. Iterator tests consume public iterators without comparing layouts.

Before the remaining fix, 508 map cases matched 492 times with 16 combiner
validation failures; four additional ordered-tail observations were added
afterward. The ordinary library regression also failed on AtomVM and passed
on OTP. A narrow `merge_with/3` fix now validates arity/type before map operands,
even without common keys; valid-map iteration and callback order are unchanged.
Full corpus results in batch 1 predate these extensions. New evidence is under
`.amp/in/artifacts/maps-pr2355/`.

Final host-interpreter checks: all 512 map cases match; the full runtime corpus
has 3,273 matches and only the four retained short-format failures. Failure-only
replay matches all 16 formerly failing combiner cases. The ordinary map suite
passes on OTP and AtomVM, including the new regression. All 483 core test modules
and nine CLI tests pass, with one optional fixture test skipped. A separate full
fixture run freshly checks four laws and matches 2,127 cases with zero mismatches
and the same four unverified OTP references. No native/JIT, embedded, or 32-bit
execution is claimed for this integration.

Owners: `lean_runtime.py`, `differential.erl`, `test_validate.py`. Done when tests
distinguish swapped callback arguments, loose key matching, unwanted callback
execution, and skipped entries, without assuming traversal order.

#### Map-comprehension extension

Added 142 differential calls to seven existing `named_locals` exports in
`fixtures.py`, without changing Lynx sources, runtime code, or proof assertions:

- List-to-map comprehensions: nonadjacent duplicate keys retain the last value;
  mismatching tuple patterns are skipped; improper tails are rejected. Keys
  include integer/float distinctions, signed zeros, nested maps, improper lists,
  partial bitstrings, and boxed integers.
- Map generators and nested map/list comprehensions: observe exact bindings and
  multiplicity without depending on enumeration order. Empty outer generators
  and failing guards suppress invalid inner generators; invalid outer generators
  still fail. Filtered results retain duplicate list contributions.
- Strict map generators: all-match controls and exactly one nonmatching entry,
  avoiding dependence on which failing map entry is visited first.
- Map-to-map collisions: observe the exact output key set and validate each
  chosen binding against the input, not an unspecified winning source key.
- Runtime-created maps with boxed values at sizes 0, 1, 2, 31, 32, 33, 63, 64,
  and 65, including overwrites and collision-heavy outputs.

The focused host-interpreter run matches all 142 OTP references, with zero
mismatches or unverified references. Its seven selected exports are not a claim
of full fixture coverage. Evidence is under
`.amp/in/artifacts/lynx-map-comprehensions-focused-v2/`.

The full 2,273-call interpreter run on OTP 29 (ERTS 17.1.1) matches 2,269 cases,
with zero mismatches, the same four unverified zero-width-generator references,
and zero uncovered exports. It still exits nonzero for those unverified cases.
Nine CLI tests pass; the optional full-fixture test is skipped, with the full run
executed separately. Seven independently specified result checks and negative
controls for observation normalization also pass. Full-run evidence is under
`.amp/in/artifacts/lynx-map-comprehensions-full/`; companion logs use the
`lynx-map-comprehensions-` prefix. No native/JIT, embedded, or 32-bit execution is
claimed for this extension.

### 4. Add portable hashing coverage

- [x] Inventory AtomVM support for `erlang:phash2/1,2` before implementing recipes.
  Treat absent support as an API gap, not a skipped successful case.
- [x] Translate observable examples from `Modules/Hashing.lean`: signed and large
  integers, signed zeros, Unicode atoms, proper/improper lists, nested tuples/maps,
  byte binaries, partial bitstrings, and larger maps.
- [x] Exercise default range, 1, non-power-of-two ranges, 2^32, and invalid ranges.
  Compare actual OTP results and independently check selected fixed vectors.
- [x] Normalize model duplicate-binding maps to their effective public contents.
  Do not copy synthetic PID/reference/function identities into cross-VM hash
  expectations. Verify any live-identity hash properties within each run.

Owners: `lean_runtime.py`, `differential.erl`, `test_validate.py`; runtime support,
if needed, is separate. Done when a wrong hash or range reduction is detected,
not merely a result of the expected type.

Batch 4 added on 2026-11-14 against OTP 29: 112 portable hashing recipes
(`4,301 -> 4,413` runtime cases total), covering all 78 unique vectors from
`Modules/Hashing.lean` (signed and large integers up to 128-bit/decimal bignums,
positive/negative zero and subnormal floats, ASCII/Latin-1/Unicode atoms, byte
binaries and partial bitstrings, proper/improper lists, nested tuples/maps, and
the 40-entry map), valid range reductions (`1`, `2`, `7`, `13`, `97`, `98`,
`255`, `256`, `65537`, `1000003`, `2^27 - 1`, `2^27`, `2^32 - 1`, `2^32`),
invalid range rejections (`0`, negative, `> 2^32`, float, atom, list), shadowed
and reordered duplicate-binding maps across size boundaries (`31/32/33`, `40`)
and exact numeric/partial-bitstring keys, and within-run closure/PID/reference
identity checks (without serializing runtime-specific hash numbers).

Source attribution validates all 7 `Modules/Hashing.lean` sites (`guard:1..5`,
`shadowed_bindings_hash`, and `capture_hash`), bringing inventory site coverage
to 200 of 426 across all 26 files (200 of 428 at `b6ac6ea`). Both host-interpreter (`build`) and `aarch64`
precompiled (`.amp/in/build-native`) 112-case replays match 0 and report 112
failures with 0 unverified references: OTP verifies 102 `{ok, ...}` results and
10 `error:badarg` exceptions, while AtomVM reports `{exception, error, undef}` on
every case because `erlang:phash2/1,2` is unimplemented (`TODO` in
`src/libAtomVM/ets_multimap.c`). Decoded expected and target results agree across
modes, and all 113 native probe/helper `.beam` files contain `avmN` and no `Code`
chunk. Nineteen enabled CLI tests pass (20 total, with one optional
`LYNX_CHECKOUT` fixture test skipped), including handwritten Elixir/OTP vector,
range-reduction, and identity oracles in
`test_portable_hashing_observations_have_independent_oracles`. No AtomVM runtime
code was modified. Evidence is under `.amp/in/artifacts/batch4-hashing/`
(`hashing-cases.json`, `focused-interpreter/`, and `focused-native/`).

### 5. Add version-2 set algebra

- [x] Cover construction, duplicate elimination, exact membership, size, union,
  intersection, subtraction, subset, and disjointness through supported public
  APIs. Include empty/disjoint/overlapping sets and numeric type distinctions.
- [x] Request version 2 explicitly where representation matters. Observe sets by
  membership, size, and normalized elements rather than incidental map layout.
- [x] Check invalid options/inputs against OTP separately from model examples.
  The model intentionally ignores requested version 1 and leaves some inputs
  unmodeled; neither choice is an AtomVM compatibility requirement.
- [x] Check algebraic relations on bounded inputs with asymmetric controls, while
  keeping model-inspired properties distinct from freshly checked fixture laws.

Owners: `lean_runtime.py`, `differential.erl`, `test_validate.py`. Done when tests
catch loose numeric membership, lost elements, duplicate counts, and reversed
subtraction, without treating model exhaustion as an Erlang exception.

Added on 2026-11-11: 291 OTP differential recipes attributed to `membership`,
`size`, and `creation`. The `unmodeled` theorem is deliberately not sampled.
Cases cover both operand orders; nested numeric keys, signed zeros, boxed
integers and bitstrings; duplicate additions and deletions; multi-set
union/intersection; and sizes 3/4/5, 31/32/33, and 63/64/65 with asymmetric
overlap ratios around the intersection/subtraction heuristics. Observations retain public size,
emptiness, enumeration count, and exact-key element membership. Concrete
operation results accompany commutativity, absorption, partition, disjointness,
and cardinality relations; these are not newly proved fixture laws.

Before the empty-option follow-up below, focused interpreter validation with
OTP 29 (ERTS 17.1.1, 64-bit) matches 273
cases, reports 18 differences, and leaves zero unverified references. All
explicit-v2 algebra cases match. The differences are retained as failures:

- Six option calls succeed on OTP but fail with `function_clause` on AtomVM:
  `new/1` and `from_list/2` with `[]`, `[{ignored,true}]`, or
  `[{version,2}|bad]`.
- Six invalid-option calls report OTP `case_clause` versus AtomVM
  `function_clause`: `[version]`, `[{version,3}]`, and `[{version,bad}]`
  for both constructors.
- Six invalid-right-operand calls return an empty set or `true` on OTP but
  fail with `function_clause` on AtomVM: `subtract/2` and `is_subset/2` with
  an empty left set and `bad`, `[]`, or `{set}` on the right.

These results distinguish supported-input gaps from out-of-contract error and
short-circuit differences; they do not add version-1 support or change runtime
behavior. Ten CLI integration tests pass, including independent concrete
oracles for exact elements, duplicate counts, subset direction, and asymmetric
subtraction; one optional full-fixture test is skipped. Focused evidence and
replayable failures are under `.amp/in/artifacts/lynx-set-algebra-final/`.

Before that follow-up, the full 3,568-case interpreter run matches 3,546 cases, reports the same 18
set differences plus the four unchanged short-float-formatting mismatches,
and leaves zero unverified references. It checks zero fresh laws and exits
nonzero; no differences are allowlisted or counted as passes. Full evidence is
under `.amp/in/artifacts/lynx-set-algebra-full/`.

Empty-option follow-up on 2026-11-11: `new([])` and `from_list(List, [])`
now delegate to the default constructors; other options are unchanged. The
ordinary `test_sets` regression fails on the original runtime at `new([])` and
passes after rebuilding the library, on both AtomVM and OTP. It also checks
empty-list construction, duplicate elimination, integer/float membership, and
invalid-list rejection with empty options. Replaying all 291 set cases matches
275, retains 16 differences, and leaves zero unverified references. Comparing
failure sets confirms exactly the two empty-option failures were resolved and
none were added. The full runtime corpus was not rerun for this localized fix.
Evidence is under `.amp/in/artifacts/sets-empty-options-differential/` and
the sibling `sets-empty-options-*.log` files.

Remaining-difference follow-up on 2026-11-11: constructors now use the shared
property-list parser; unsupported v1 requests explicitly retain their prior
`function_clause` rejection. Invalid versions match OTP's `case_clause`
reasons. `subtract/2` and `is_subset/2` keep the map/map fast paths and add
OTP's general filter/fold fallbacks, which skip right-operand inspection only
when the left set is empty. Thirty-eight added recipes check first-option
precedence and invalid operands against both empty and nonempty valid sets.
All 329 set cases match OTP with zero failures or unverified references,
including all 16 previously failing expressions. Expanded `test_sets` cases
fail before this change and pass afterward on AtomVM and OTP; separate target
checks confirm v1 constructors still reject. Ten CLI tests pass (one optional
fixture test skipped), and erlfmt checks pass. A subsequent full interpreter
run matched 3,602 of 3,606 cases, with only the four known short-float failures
and zero unverified cases. Evidence is in `.amp/in/artifacts/lynx-set-final-full/`,
`.amp/in/artifacts/sets-edge-options-differential/`, and sibling logs.

### 6. Generate bounded operation sequences

- [x] Generate reproducible dictionary/map updates and queries, saving complete
  initial bindings and operations as replay input. Shrink by deleting operations
  without modifying setup, literal keys/values, seed, or index.
- [x] Observe every map/dictionary return value or exception and intermediate
  state, including overwrites, missing entries, unrelated sentinels, exact keys,
  stored `undefined`, and growth/shrinkage across 32 entries.
- [x] Generate selective-receive sequences with initial mailbox setup, self-sends,
  exact/tuple-tag/guard/multiple-clause/catch-all receives, and zero-timeout
  misses. Observe ordered remaining mailboxes by running each prefix in a fresh
  isolated worker and draining only at its end; never drain/requeue between
  operations. Bound each sequence to 30 operations and preserve initial queued
  messages and surviving operation payloads during shrinking.
- [x] Extend to binary scope combinations: nested parts, appends, overlapping
  patterns, signed/duplicate scopes, and split trimming. Compare every result,
  exception, and intermediate binary; preserve setup and literal steps in reduction.
- [x] For lifecycle extensions, use same-sender fences and explicit handshakes.
  Sample documented allowed outcomes for races; do not compare cross-sender
  arrival order, elapsed time, or outcome frequencies as correctness requirements.
- [x] Bound map/dictionary sequences to 72 operations and at most 41 bindings,
  reusing bounded numeric/key generation. Retain failing seeds and reduced cases;
  do not claim exhaustive schedules or general programs.

Owners: `generated.py`, `differential.erl`, `validate.py`, `test_validate.py`.
Done when a saved failure replays unchanged and shrinking preserves the tested
contract rather than producing a different setup or oracle failure.

Map/dictionary batch validated on 2026-11-11 with OTP 29 and the host
interpreter: seed `20261111` matches all 1,000 mixed cases, including 144 map
and 143 dictionary sequences with 17,036 operations, zero failures, and zero
unverified references. Decoded traces confirm sentinel preservation and both
32- and 33-entry states for near-boundary setups. Eleven enabled CLI tests
pass (one optional fixture test skipped), including same-seed regeneration,
replay, independent intermediate-state oracles, and injected failures reduced
and replayed with unchanged setup. erlfmt and whitespace checks pass.
Evidence is under `.amp/in/artifacts/lynx-sequences-seed-20261111/`,
`lynx-sequences-observations.json`, and `lynx-sequences-tests-confirmed.log`.
The full 3,606-case runtime recheck with chunked output subsequently matched
3,602 cases, retaining exactly the same four short-float formatting failures
and zero unverified references. Evidence is under
`.amp/in/artifacts/lynx-sequences-runtime/`. This run predates the new receive
sequence probe; it closes validation of the output and map/dictionary batch.

Selective-receive batch validated on 2026-11-11 with OTP 29 and the host
interpreter: seed `20261112` matches all 1,000 mixed cases, including 126 receive
sequences with 3,292 logical operations and 30 empty initial mailboxes. There
are zero failures or unverified references. Twelve enabled CLI tests pass
(one optional fixture test skipped), including hand-written ordered-mailbox
oracles for exact numeric matches, misses followed by sends, guard rejection,
clause competition, and empty queues. Injected receive failures shrink and
replay with unchanged initial messages, seed, and surviving payloads. Host
Erlang tools used `ERL_FLAGS='+S 2:2'`; no schedule-exhaustion claim is implied.
erlfmt and whitespace checks pass. No runtime changes were needed.
Evidence is under `.amp/in/artifacts/lynx-receive-sequences-seed-20261112/`,
`lynx-receive-sequences-observations.json`, and `lynx-receive-sequences-tests.log`.

Binary scope batch validated on 2026-11-11 against OTP 29: seed `20261113`
matches all 1,000 mixed cases in both the host interpreter and aarch64 precompiled
execution, including 112 binary sequences with 3,497 operations. Inputs and decoded
target results agree across both modes, with zero failures or unverified cases.
Each sequence has at most 32 operations; setup sizes include 0, 7, 31, 32, 63, 64,
65, and 96 bytes. Decoded traces show 1,078 successful matches, 378 misses, 1,344
successful splits, and 249 expected rejections. Thirteen enabled CLI tests pass
(one optional fixture test skipped), including hand-written scope/split/state
oracles, seed/replay checks, and injected failures reduced and replayed with
unchanged initial bytes, seed, and surviving operation payloads. All 1,001 native
probe/helper binaries have `avmN` and no `Code` chunk. This uses the matching native
library and disables on-demand JIT; it is not a GC/lifetime or schedule stress test.
No runtime changes were needed. erlfmt and whitespace checks pass. Evidence is
under `.amp/in/artifacts/lynx-binary-sequences-seed-20261113/`,
`lynx-binary-sequences-native/`, and the sibling `lynx-binary-sequences-*.log`,
`lynx-binary-sequences-observations.json`, and mode/chunk comparison files.

Lifecycle sequence batch validated on 2026-11-13 against OTP 29: generated
coverage now spans ten operation families, adding stateful `lifecycle_sequence`
runs with same-sender fences, explicit worker handshakes, sanitized symbolic
identities (no raw PIDs or references serialized), and allowed-outcome predicates
for unlink/exit and demonitor/exit races. Boundary 0 immediately exposed an
AtomVM runtime defect where `erlang:register/2` allowed an already-registered
PID or port to be registered under additional names (`{ok, true}` instead of
OTP's `error:badarg`); `--shrink` reduced the 15-step failing sequence in 12
trials to `[{register_slot, reg_a, alpha}, {register_slot, reg_b, alpha}]`,
replaying unchanged with `same_failure_contract` checking both OTP and AtomVM
contract preservation. Fixing `globalcontext_register_process` and
`nif_erlang_register_2` (with an added check in `tests/erlang_tests/test_process_info.erl`)
resolved all failures.

After the fix, seed `20261114` matches all 1,000 mixed cases in both host
interpreter and `aarch64` precompiled execution (`0` failures, `0` unverified
references), including 101 `lifecycle_sequence` cases spanning 1,561 operations
across all 19 step kinds (including 208 expected exceptions, 77 unlink-exit
race checks with 58 live races and 19 `already_dead` slots, and 95 demonitor-exit
race checks). All 1,000 decoded native results equal their interpreter
baselines, and all 1,001 native probe/helper `.beam` files contain `avmN` and no
`Code` chunk. Eighteen enabled CLI tests pass (one optional fixture test
skipped), `test-erlang` passes, and `erlfmt` checks pass. Evidence is under
`.amp/in/artifacts/batch6-lifecycle-sequences/` (`smoke-42-before-fix/`,
`register-duplicate-pid-shrink/`, `register-duplicate-pid-shrink-replay/`,
`interpreter-1000/`, and `native-1000/`).

### 7. Expand execution-path coverage and separate transport cost

- [x] Exercise arithmetic, comparison, and map-lookup boundary cases through
  direct, dynamic, guard, and applicable pattern paths. Inspect representative
  BEAM disassembly: source forms need not take distinct paths across OTP versions.
- [x] Run interpreter and native/precompiled modes with matching runtime libraries.
- [x] Extend to on-demand JIT and representative 32-bit/embedded/emulated execution
  in separate runner work; those modes are not supported by the current CLI.
- [x] Use a matching-word-size OTP oracle for representation-dependent recipes,
  especially numeric key-search compatibility. Define float32 expectations
  separately rather than treating host binary64 output as universally applicable.
- [x] Keep permanent GC/lifecycle regressions in ordinary AtomVM test suites and
  run them under instrumentation. Do not add short internal deadlines to tests
  intended to check correctness rather than speed.
- [x] Resolve the large-result transport bottleneck separately from split
  semantics. The 2,560-result probe computed the correct result; decimal byte-list
  formatting dominated its timeout. Sequence diagnostics reproduced the issue:
  a 70,796-byte serialized trace took 26.13 seconds with a whole-list decimal
  writer, versus 0.15 seconds with 256-byte chunks, including computation and
  serialization. Chunking preserves the byte-list wire format and decoded OTP
  reference term; no runtime heap policy changes. CLI regression checks retain
  malformed-output detection and replay.

Done when results identify the actual execution mode, target configuration,
runtime/library hashes, and whether a timeout was semantic, oracle, or transport
work. Cross-compilation alone does not count as target execution.

Precompiled replay validated on 2026-11-11 against 64-bit OTP 29: a fresh
Release aarch64 build uses `AVM_ENABLE_PRECOMPILED=ON`, `AVM_DISABLE_JIT=ON`,
and the matching `atomvmlib-aarch64.avm`. All 1,000 saved cases from each of
seeds `20261111` and `20261112` match OTP. The saved 3,606-case runtime corpus
matches 3,602 cases, retaining exactly the four known short-float formatting
mismatches, with zero unverified references. All 5,606 decoded native results
also equal their saved interpreter results; the replay inputs are unchanged.
Every probe/helper has an `avmN` chunk and no `Code` chunk, excluding bytecode
fallback for those modules. Some bundled library modules remain interpreted;
this does not validate on-demand JIT, scheduler interleavings, or other targets.
No runtime changes were needed. Configuration, runtime/library SHA-256 hashes,
counts, and evidence paths are in `.amp/in/artifacts/lynx-native-validation.json`;
the three replay directories are `lynx-native-runtime/`, `lynx-native-map-dict/`,
and `lynx-native-receive/` beneath `.amp/in/artifacts/`.

Execution-path batch validated on 2026-11-11 with 64-bit OTP 29: all 466 added
cases match OTP in interpreter and aarch64 precompiled modes, with zero failures
or unverified references. Eleven arithmetic operators, eight comparisons, and
map lookup each exercise both matching and rejected guards. Body/dynamic results
include float bits and exception class/reason; map patterns separately report
matched values or no match. This preserves the different contracts of each path.
Fourteen enabled CLI tests pass (one optional fixture test skipped), including
independent precision-boundary, signed-zero, arithmetic-error, and map-key oracles.
erlfmt and whitespace checks pass. No runtime changes were needed.

Disassembly of the actual helper confirms dynamic `erlang:apply/3`, comparison
BIFs versus guard tests, arithmetic GC-BIF failure labels of zero versus nonzero,
direct float-register division versus guard GC-BIF division, and map-pattern
`get_map_elements` versus lookup BIFs. Original helper `Code` chunks agree across
the two runs; all 467 transformed native probe/helper files contain `avmN` and no
`Code` chunk. The matching precompiled library is used with on-demand JIT disabled;
not every bundled library module is native. Recheck instruction selection on a
new OTP compiler.
Evidence is under `.amp/in/artifacts/lynx-execution-paths-interpreter/`,
`lynx-execution-paths-native/`, `lynx-execution-paths-tests.log`,
`lynx-execution-paths-disassembly.txt`, `lynx-execution-paths-instruction-check.txt`,
and `lynx-execution-paths-observations.json` (including runtime/library hashes).

The subsequent full 4,072-case runtime recheck on 2026-11-11 matches 4,068 cases
in each mode, retaining exactly the four known short-float formatting mismatches
(`12300.0`, `0.00012`, `1.0e23`, `9.999999999999999e22`, all with `[short]`).
There are zero new mismatches, target errors/timeouts, or unverified references.
All 4,072 decoded target results agree across interpreter and aarch64 precompiled
execution; the original 3,606 results also equal the saved interpreter baseline.
All 4,073 native probe/helper files have `avmN` and no `Code` chunk. Both CLI runs
exit nonzero for the four mismatches; none are skipped or allowlisted. No runtime
changes were needed. Evidence is under `.amp/in/artifacts/lynx-paths-full-interpreter/`
and `lynx-paths-full-native/`, with `lynx-paths-full-mode-comparison.txt`,
`lynx-paths-full-native-chunks.txt`, and `lynx-paths-full-validation.json` recording
the comparisons, configuration, source revision, runtime/library hashes, and
failure expressions. This remains precompiled validation, not on-demand JIT.

Batch 7 execution-path, representation, and ASan verification (2026-11-14):

- Runner & oracle classification (`validate.py`, `validate.exs`, `lean_runtime.py`):
  `run.json` now records `execution_mode` (`interpreter`, `on_demand_jit`,
  `precompiled`, or `jit+precompiled`), `target` (`platform`, `arch`, `word_size`,
  `float_bits`, `avm_disable_jit`, `avm_enable_precompiled`, `avm_use_32bit_float`,
  `avm_create_stacktraces`),
  and SHA-256 hashes for the runtime, optional `.wasm` module, and runtime
  libraries. Generated `lynx_probe_*.erl` drivers emit `LYNX_SEMANTIC_DONE\n`
  via `erlang:display_string/2` after `actual()` completes so `failures.json`,
  `unverified.json`, and `actuals.json` distinguish `oracle`, `semantic`, and
  `transport` failures/timeouts.
- On-demand JIT (`AVM_DISABLE_JIT=OFF`, `AVM_ENABLE_PRECOMPILED=OFF`,
  `.amp/in/build-on-demand`): `validate.py` preserves bytecode `Code`-only probes
  and helpers (no `avmN` chunk, no `native/` dir) when precompilation is off so
  `code_server` compiles them at runtime via `jit_trap_and_load` (`Compilation of ...`
  logged). All 24 multi-family cases pass against OTP 29 (`failed: 0`,
  `unverified: 0`), `runtime_sha256` `852c95ada7ab0d356edf869d8988d266c4725a5b17f370f9730dd46967b5c5eb`,
  `library_sha256` `c6bc067cdac52e52da5207a87fbded22fe1e17edf993fddc8ac424fcf3950355`;
  evidence in `.amp/in/artifacts/batch7-execution/on-demand-jit/`.
- 32-bit Emscripten / Node.js execution (`wasm32`, `.amp/in/build-wasm32`,
  `--host-build build`, `word_size = 4`): standalone Erlang tests
  `test_lists_keyfind` (`0`) and `test_bigintegers_ordering` (`7`) pass on Node.js
  (`.amp/in/artifacts/batch7-execution/wasm32-erlang-tests.log`). In the 116-case
  32-bit slice (`.amp/in/artifacts/batch7-execution/wasm32/`), all 96 portable cases
  (including 84 numeric/position `lists:keyfind/3` & `lists:keymember/3` boundary
  cases) match OTP (`failed: 0`), while the 20 64-bit-immediate `lists:keyfind/3`
  & `lists:keymember/3` precision-boundary cases (`word_size_dependent: true`) are
  marked `unverified` with `stage: "oracle"` because host OTP runs with
  `otp_word_size = 8`. Runtime `AtomVM.mjs` SHA-256
  `e744bba1d2fcc5dc66dbf819caed6f7542545ab10e1b475d008eab2b2cf1a93c`, `AtomVM.wasm`
  `6ed25c80477b733b4cc31fe53b6a5219607fd868d567cb63a747247102985006`, library
  `atomvmlib-emscripten.avm` `0803c224114dec9b41501f9304664a25fe4ffe2bfdeb830f05364501c8802b3a`.
- Single-precision floats (`AVM_USE_32BIT_FLOAT=ON`, `.amp/in/build-float32`,
  `float_bits = 32`): `--float32` supplies 10 IEEE-754 `binary32` boundary cases
  across `differential:call/3` and `differential:arithmetic/3` (covering binary32
  rounding ties, overflow to `badarith`, and subnormal underflow to `0.0`), all
  10/10 passing (`.amp/in/artifacts/batch7-execution/float32/`, `runtime_sha256`
  `ace335bf6655cb58655d667d7d45823814e468b174d201e88f6ca91d49c37e81`,
  `library_sha256` `acd81e90e925b0b296058ca066f42b0985e06a1feaecc6d4f3825d2ca6377418`).
  Unannotated `binary64` float results are marked `unverified` (`stage: "oracle"`,
  verified in `.amp/in/artifacts/batch7-execution/float32-oracle-guard/`, 2 passed,
  3 unverified, 0 failed). Note: constant-folded `binary64` literals in BEAM `LitT`
  chunks exceeding `binary32` max (`> 3.4028235e38`) currently convert to `+Inf` in
  `src/libAtomVM/external_term.c:780` (`NEW_FLOAT_EXT`) on `AVM_USE_32BIT_FLOAT=ON`,
  so runtime `float32` probes cross module boundaries (`differential:call/3`,
  `differential:arithmetic/3`).
- Permanent GC/lifecycle regressions and ASan instrumentation: verified
  existing ordinary test suites without adding short internal deadlines. Under
  both `build` and Debug + AddressSanitizer (`.amp/in/build-ci-no-stack`), `test-erlang` (`test_monitor`, `test_dict`,
  `test_process_info`, `test_is_process_alive`, `test_lists_keyfind`,
  `test_bigintegers_ordering`, `test_binary_split`, `test_binary_match`,
  `test_binary_part`) passed (`Success.`), `test_eavmlib.avm` passed all 10 modules
  (including `test_timer_manager`), and `test_estdlib.avm` passed 37/38 modules
  (including `test_spawn`, `test_timer`, `test_serial_dist_socat`) with zero ASan
  findings; `test_net_kernel` failed `test_autoconnect_to_beam` (`nxdomain`
  resolving host shortname `otp_188@localsome` in this environment).
- Disabled stacktraces (`AVM_CREATE_STACKTRACES=OFF`, `.amp/in/build-ci-no-stack`):
  `validate.py` records `"avm_create_stacktraces": false` in `run.json`, and
  `validate.exs` classifies the 169 `stacktrace_dependent` recipes (`monitor_exit(raised)`,
  valid-stack `raise_exception/3`, valid-raise/generated `trace_depth/3`, and
  uncaught-error `function_spawn/3` cases where `catch C:R:S` / exit reasons
  yield `undefined` instead of a stack list) as `unverified` (`stage: "oracle"`).
  Re-comparing the 555-case exception/signal/spawn slice against `.amp/in/build-ci-no-stack`
  gave 386 passed, 0 failed, and 169 unverified (`stage: "oracle"`), and
  `--seed 20261114 --generated 32` passed 32/32 (`0` failed, `0` unverified);
  `.github/workflows/lynx-differential.yaml` adds the `linux-x86_64-no-stacktraces`
  matrix leg for CI coverage.
- CLI integration suite: all 23 enabled tests in `test_validate.py` pass under
  `ATOMVM_JIT_BUILD=.amp/in/build-on-demand` (1 optional `LYNX_CHECKOUT` fixture
  test skipped), covering on-demand JIT bytecode preservation, `oracle`/`semantic`/
  `transport` stage & timeout classification, 32-bit word-size / `--float32` /
  `AVM_CREATE_STACKTRACES=OFF` oracle guards, and `wasm32`/`--host-build` target
  resolution.

Batch 8 remaining behavioral sites in `Modules/Erlang.lean`, `Term/ShortCircuit.lean`, and `Modules/Signals.lean` (2026-11-15):

- Covers all 21 remaining behavioral theorem sites (`Term/ShortCircuit.lean`: `13/13`,
  `Modules/Signals.lean`: `20/20`, `Modules/Erlang.lean`: `75/76` leaving only
  `private theorem floatOne_toRat`), adding 524 runtime cases (`5,085 -> 5,609`,
  `277 -> 298` of `428` Lean sites; `stacktrace_dependent`: `169 -> 172`).
- Differential validation of the 579 cases matching those 21 origins uncovered two
  genuine AtomVM defects (fixed separately and tested in `test_is_integer_3` and
  `test_spawn`): `maybe_call_native` in `src/libAtomVM/opcodesswitch.h` and
  `src/libAtomVM/jit.c` omitted `case 3:` for `BIFFunctionType` (breaking dynamic
  `apply(erlang, is_integer, [X, Lo, Hi])`), and `erlang:spawn_link/1` /
  `erlang:spawn_monitor/1` in `libs/estdlib/src/erlang.erl` lacked an `is_function(..., 0)`
  guard raising `error:badarg` for wrong-arity funs. Nonfinite/product float probes
  stay within AtomVM's 256-bit integer limit (`1 bsl 255`).
- Verified 579/579 passing on host interpreter (`build`) and `aarch64` precompiled JIT
  (`.amp/in/build-native`), 576/576 portable cases passing on `.amp/in/build-ci-no-stack`
  (ASan + no stacktraces, 3 `stacktrace_dependent` omitted via `--portable-only`), 13/13
  focused cases on `.amp/in/build-on-demand`, 17/17 portable cases on `.amp/in/build-wasm32`,
  and 27 enabled CLI tests (`29` total, 2 skipped) under `ATOMVM_JIT_BUILD=.amp/in/build-on-demand`.

Upstream refresh to `6968d72` (`1925266`, `d74890a`, `6968d72`) and Batch 9 BIF coverage (2026-11-15):

- Pinned upstream Lynx at `6968d72cb99211eb9cab973e067f24a18cacfb3e` (`438` total
  `Lean/LynxTest` sites across 26 files), updated shifted `Modules/Binary.lean`
  `guard:3..5 -> guard:5..7` attributions caused by the new `unsignedCases` guards,
  and added 112 differential cases (`5,609 -> 5,721`, `306` of `438` Lean sites:
  `Modules/Conversions.lean` `17/17`, `Modules/Lists.lean` `9/9`) for
  `erlang:binary_to_atom/1,2`, `erlang:tuple_to_list/1`, and `lists:keymember/3`
  short tuples (`binary:encode_unsigned/1` remains unimplemented in AtomVM and
  `Modules/Binary.lean#guard:3..4` remain unsampled).
- Differential checking exposed an AtomVM `binary_to_atom/2` discrepancy on
  255 valid ASCII characters followed by an invalid UTF-8 byte (`0xFF`), where
  `src/libAtomVM/bif.c` returned `badarg` instead of OTP/Lynx `system_limit`.
  Fixed `binary_to_atom` in `src/libAtomVM/bif.c` to enforce the 255-codepoint
  atom limit before decoding trailing bytes (and `badarg` on oversized
  `binary_to_existing_atom` lookups, plus freeing temporary `latin1 -> utf8`
  buffers), with regression checks in `tests/erlang_tests/test_binary_to_atom.erl`.
- Verified focused replay (`300/300` passing), full interpreter CI run (`5,753` cases
  with `--seed 20261114 --generated 32`: `5,632` matched OTP, `121/121` expected
  failures, `0` unexpected failures/passes), `./build/tests/test-erlang`, and
  `test_validate.py` (`29` tests: `26` passed, `3` env-gated optional skipped).

Batch 10 — Remaining `Lean/modules.json` BIF differential coverage (2026-11-15):

- Added 78 differential cases in `lean_runtime.py` (`5,721 -> 5,799`, `309` of `438`
  Lean sites across 26 files: `Modules/Binary.lean` reaches `7/7`,
  `Term/Dispatch.lean` reaches `5/17`) covering the four remaining BIFs in Lynx's
  `Lean/modules.json` outside `erts_internal`:
  - `erlang:is_number/1` (`32` cases via `differential:is_number_paths/1`: qualified,
    inline, `apply/3`, and guard paths across small/large ints, floats, booleans,
    compounds, funs, and live pids/refs; `32/32` pass).
  - `maps:from_keys/2` (`20` cases via `differential:call(maps,'from_keys',...)`:
    empty, duplicate keys, numeric exact keys, nested keys, shared boxed values,
    `31/32/33/40` size boundaries, and `badarg` invalid/improper lists; `20/20` pass).
  - `binary:encode_unsigned/1` (`13` cases attributed to `Modules/Binary.lean#guard:3..4`,
    tracked in `expected_failures.json` as `binary_encode_unsigned` due to `error:undef`).
  - `erlang:fun_info_mfa/1` (`13` cases attributed to `Term/Dispatch.lean#fun_info_mfa_reports_compiled_name`,
    tracked in `expected_failures.json` as `fun_info_mfa` due to `error:undef`).
- Verified full interpreter CI run (`5,831` cases with `--seed 20261114 --generated 32`
  and `--expected-failures tools/test/lynx/expected_failures.json`: `5,684` matched OTP,
  `147/147` expected failures, `0` unexpected failures/passes, `0` unverified) and
  `test_validate.py` (`29` tests: `26` passed, `3` env-gated optional skipped).

Batch 11 — Complete `Modules/Timers.lean` differential coverage (2026-11-15):

- Added 19 differential cases in `lean_runtime.py` (`5,799 -> 5,818`, `313` of `438`
  Lean sites across 26 files: `Modules/Timers.lean` reaches `13/13`) covering the
  four remaining sites in `Modules/Timers.lean`:
  - `cancel_reports_time_left` (`4` cases via `differential:timer(time_left,5000|60000,...)`:
    confirms positive remaining milliseconds `0 < Remaining <= Delay` and non-delivery).
  - `cancel_reports_no_time_left` (`2` cases via `differential:timer(no_time_left,0,...)`:
    confirms `0` or `false` on immediate 0ms cancel with non-delivery vs delivery).
  - `cancel_false_before_delivery` (`6` cases across `false_before_delivery`,
    `false_before_delivery_waited`, and cross-process `false_before_delivery_target`:
    allows non-blocking `after 0` peek to miss when `cancel_timer/1` returns `false`
    while verifying eventual delivery).
  - `send_after_options` (`7` cases via `differential:timer_options(...)` at `10`ms /
    `60000`ms and malformed option `badarg` checks without altering the 52
    `otp_extension` entries).
- Verified focused replay (`19/19` passing), full interpreter CI run (`5,850` cases
  with `--seed 20261114 --generated 32` and `--expected-failures tools/test/lynx/expected_failures.json`:
  `5,703` matched OTP, `147/147` expected failures, `0` unexpected failures/passes,
  `0` unverified), and `test_validate.py` (`29` tests: `26` passed, `3` env-gated
  optional skipped).

Batch 12 — Expand `Modules/Process.lean` public-behavior coverage (2026-11-15):

- Added 45 differential cases in `lean_runtime.py` (`5,818 -> 5,863`, `335` of `438`
  Lean sites across 26 files: `Modules/Process.lean` reaches `37/49`) covering 22
  testable sites:
  - `error_reason`, `error_caught` (`differential:error_pair/2` on `erlang:error/2`
    uncaught and caught paths including non-list arg terms).
  - `reference_type_guard`, `make_ref_preserves_process_state`, `make_ref_fresh`,
    `make_ref_is_effectful` (`differential:is_reference_paths/1` and `process_probe`
    ref state/effect/batch-uniqueness checks).
  - `selective_receive_preserves_unmatched`, `receive_bindings_and_clause_priority`,
    `receive_body_exception_is_caught`, `finite_timeout_progress`,
    `timeout_can_expire_child`, `infinity_never_expires`,
    `guard_reads_waiting_child_identity` (`differential:receive_selective/1`,
    `receive_timeout(5, ...)`, and `process_probe` child timeout/infinity/self-guard
    probes).
  - `try_success_body_is_outside_handler`, `try_handler_exception_propagates`,
    `blocking_is_not_an_exception` (`differential:try_boundaries/1` and blocked child
    wake check).
  - `blocked_processes_resume`, `handshake_every_run`, `two_senders_every_run`,
    `terminated_child_is_not_revived`, `terminated_root_discards_late_messages`,
    `child_exception_is_isolated` (deterministic `process_probe` handshakes, sorted
    two-sender completion, dead child/root non-revival, and unlinked child `error`/
    `throw`/`exit` isolation).
  - Remaining 12 uncovered `Modules/Process.lean` sites are pure Lean symbolic
    scheduler traces, `¬ Result.IsReadOnly` guard purity proofs, unbounded deadlock
    theorems, or meta-theorems matching the `Modules/Process.lean` inventory note.
- Verified focused replay (`45/45` passing), full interpreter CI run (`5,895` cases
  with `--seed 20261114 --generated 32` and `--expected-failures tools/test/lynx/expected_failures.json`:
  `5,748` matched OTP, `147/147` expected failures, `0` unexpected failures/passes,
  `0` unverified), and `test_validate.py` (`29` tests: `26` passed, `3` env-gated
  optional skipped).

Batch 13 — Add `upstream_example` coverage for all `examples/otp/*.exs` stdlib laws (`array`, `base64`, `binary` hex, `calendar`, `gb_trees`, `json`, `queue`, `rand`) (2026-11-15):

- Added 197 `upstream_example` differential cases in `lean_runtime.py`
  (`5,863 -> 6,060`, retaining `335` of `438` Lean sites across all 26 files)
  covering all eight remaining `examples/otp/*.exs` modules (and documenting all
  non-law / unimplemented OTP gaps in `tools/test/lynx/expected_failures.json`):
  - `examples/otp/base64.exs#encoding_roundtrip` (`50` cases): `encode/1,2` and
    `decode/1,2` roundtrips + `((Len + 2) div 3) * 4` length law across lengths
    mod 3, `standard`/`urlsafe` alphabets, `padding => true|false`,
    `encode_to_string`/`decode_to_string`, flat string input, agreed error
    checks, 15 `base64_invalid_error_reason` cases (uniform `badarg` vs OTP
    `function_clause` / `missing_padding` / `{case_clause, eq}` / `badarith`),
    and 4 `base64_iolist_whitespace_and_byte_range` cases (nested `iolist()`
    acceptance, out-of-range byte lists, and whitespace rejection per `@doc`).
  - `examples/otp/binary.exs#hex_roundtrip` (`16` cases): `encode_hex/1,2`
    (`uppercase`/`lowercase`) roundtrips, mixed-case `decode_hex/1`, and
    odd-length/non-hex/bitstring/invalid-case error paths.
  - `examples/otp/calendar.exs` (`36` cases across all 6 laws): `28` cases under
    `date_to_gregorian_days_cycle` (400-year `146,097`-day Gregorian cycle law
    for `Year >= 0`, leap-day/year-end boundaries, `day_of_the_week/1,3`,
    `datetime_to_gregorian_seconds/1`, `system_time_to_universal_time/2`,
    invalid-time `function_clause`, 6 `calendar_date_range_guards` cases for
    `Year >= 0, M in 1..12, D in 1..31`, and 4
    `calendar_negative_subsecond_system_time` truncation bug cases for `-1` in
    `millisecond`/`microsecond`/`nanosecond`/`native`), plus 8
    `calendar_unimplemented_functions` cases across
    `seconds_to_daystime_roundtrips` (`2`), `seconds_to_time_roundtrips` (`1`),
    `is_leap_year_cycle` (`2`), `gregorian_days_roundtrip` (`2`), and
    `iso_week_number_valid_date` (`1`).
  - `examples/otp/json.exs#json_roundtrip` (`42` cases): `encode/1`/`decode/1`
    roundtrips across booleans/null, small and boxed integers, floats, control
    and multi-byte UTF-8 strings, SWAR-width strings, depth-32 nested structures,
    whitespace/`-0.0`/escape/surrogate-pair decoding, streaming
    `decode_start/3` + `decode_continue/2`, and malformed-JSON error checks
    including `<<"1 2">>`, `<<"1e1000">>`, and lone high surrogates.
  - `examples/otp/queue.exs#{reverse_involution, reverse_exchanges_ends, front_insert_peek, split_preserves_ordered_partition}`
    (`44` cases): all four queue laws across empty, singleton, multi-element,
    front-loaded `{[3,2,1],[]}`, back-loaded `{[],[1,2,3]}`, and split
    `{[3,2],[1]}` representations, plus `filter`/`fold`/`any`/`all`/`delete`/
    `delete_r` and empty/bounds `badarg` checks.
  - Unimplemented `examples/otp/*.exs` modules documented in
    `expected_failures.json`: `array_module_unimplemented` (`3` cases for
    `examples/otp/array.exs#set_obeys_update_contract`),
    `gb_trees_module_unimplemented` (`3` cases for
    `examples/otp/gb_trees.exs#balance_preserves_observations`), and
    `rand_module_unimplemented` (`3` cases across
    `examples/otp/rand.exs#{shuffle_permutes, shuffle_short_consumes_nothing}`).
- Follow-up fix in `libs/estdlib/src/json.erl`: `decode/1` no longer strips
  trailing whitespace before reporting `{invalid_byte, Byte}` on trailing
  garbage (`<<"1 2">>`), default float decoding converts `binary_to_float`
  `badarg` (e.g. `1e1000`) directly to `{unexpected_sequence, Bin}` without an
  extra closure indirection and `decode_continue(end_of_input, ...)` re-raises
  `{unexpected_sequence, _}`, and `parse_surrogate_low` raises `unexpected_end`
  when a high surrogate is not followed by `\uXXXX`, matching OTP and
  `test_json.erl`'s regression table; retained `test_decode_otp_error_reasons/0`
  in `tests/libs/estdlib/test_json.erl`. Remaining non-law / unimplemented
  differences (`193` cases total across `14` areas) are documented in
  `tools/test/lynx/expected_failures.json`.
  - Verified `test_json`, full interpreter CI run (`6,092` cases with
  `--seed 20261114 --generated 32` and
  `--expected-failures tools/test/lynx/expected_failures.json`: `5,899` matched
  OTP, `193/193` expected failures, `0` unexpected failures/passes, `0`
  unverified), and `test_validate.py` (`30` tests including
  `test_upstream_stdlib_example_observations_have_independent_oracles`: `27`
  passed, `3` env-gated optional skipped).

Batch 14 — Cover testable `Term/Try.lean` `try/catch/after` theorems in `lean_runtime.py` (2026-11-16):

- Added `22` standalone Erlang `try/catch/after` probes in `lean_runtime.py`
  (`6,060 -> 6,082`, lifting attributed Lean coverage to `349` of `438` sites
  across all 26 files; `Term/Try.lean` reaches `14/17`) covering all `14`
  testable BEAM semantics in `Term/Try.lean` (`success_binds_multiple_values`,
  `success_binds_no_values`, `exception_bindings` across `throw`/`error`/`exit`,
  `success_body_is_outside_handler`, `handler_exception_propagates`,
  `outer_handler_catches_success_body`, `unmatched_handler_reraises`,
  `reraise_preserves_class` via `erlang:raise/3`,
  `protected_effects_survive_failure`, `cleanup_runs_on_success_and_failure`,
  `cleanup_failure_overrides_previous_outcome`, `dispatched_exception_is_caught`,
  `dispatched_success_body_exception_escapes`, and
  `resumed_receive_exception_is_caught`), leaving only the `3` model-only
  theorems (`stacktrace_is_empty`, `exhaustion_is_not_caught`,
  `blocking_is_not_caught`).
- Verified `lean_runtime.inventory()` against `/Users/petermm/OSScontrib/lynx`
  (`349/438`), `test_validate.py` (`27` passed, `3` env-gated optional skipped),
  and full interpreter CI run (`6,114` cases with `--seed 20261114 --generated 32`
  and `--expected-failures tools/test/lynx/expected_failures.json`: `5,921`
  matched OTP, `193/193` expected failures, `0` unexpected failures/passes, `0`
  unverified).

Batch 15 — Cover remaining testable `Modules/ErtsInternal.lean`, `Term/Maps.lean`, and `Term/Dispatch.lean` theorems (2026-11-16):

- Added `22` standalone Erlang probes in `lean_runtime.py` (`6,082 -> 6,104`,
  lifting attributed Lean coverage to `367` of `438` sites across all 26
  files):
  - `Modules/ErtsInternal.lean` (`0/3 -> 3/3`, `7` cases):
    `iterator_shapes` (`mc_iterator` on `#{}`, `[a]`, `a`), `iterator_entries`
    (map comprehension `#{{k, K} => V * 10 || K := V <- #{a => 1, b => 2}}`),
    and `refill_raises` (`mc_refill` on `a`, `[]`, `none`).
  - `Term/Maps.lean` (`0/8 -> 8/8`, `8` cases): public Erlang map
    pattern-matching across all 8 theorems (`entries_in_requested_order`,
    `entries_shadowed_key` via `maps:from_list/1`, `entries_missing_key`,
    `entries_missing_later`, `entries_empty`, `entries_empty_nonmap`,
    `entries_numeric_keys`, `entries_arity`).
  - `Term/Dispatch.lean` (`5/17 -> 12/17`, `7` cases): `recursive_dispatch`,
    `pure_calls` (closure capture, `{badarity, ...}`, `{badfun, ...}`),
    `effectful_call`, `apply_call`, `sequential_dispatch`,
    `catch_dispatched_exception`, and `dispatched_state`, leaving only the `5`
    depth-0/exhaustion/table-lookup Lean model theorems (`pure_dispatch_without_depth`,
    `exhaustion_is_not_an_exception`, `pure_spawn_without_depth`,
    `recursive_spawn_exhausts`, `resolve_finds_named_entries`).
- Verified `lean_runtime.inventory()` against `/Users/petermm/OSScontrib/lynx`
  (`367/438`), `test_validate.py` (`27` passed, `3` env-gated optional skipped),
  and full interpreter CI run (`6,136` cases with `--seed 20261114 --generated 32`
  and `--expected-failures tools/test/lynx/expected_failures.json`: `5,943`
  matched OTP, `193/193` expected failures, `0` unexpected failures/passes, `0`
  unverified).

## Rules for each batch

- Add only cases that distinguish a plausible wrong implementation. Prefer
  boundaries and asymmetric controls to a large Cartesian product of easy inputs.
- Run the relevant CLI integration checks and real OTP/AtomVM scenarios; fixture
  law assertions additionally need fresh `--lynx` proof checks. Preserve source,
  seed, results, replay files, toolchain/configuration, and hashes as evidence.
- Report matched, mismatched, unverified, and unselected cases separately.
  Normalize only unspecified behavior; never normalize away a semantic defect.
- Keep runtime fixes, coverage additions, and runner/protocol changes in logical
  commits. Reproduce defects before fixing them and retain a public regression.
- Do not modify general numeric comparison or short formatting to accommodate an
  unrelated API quirk. Keep integers within the documented target capacity.
- Update this plan and README counts when batches land. A completed batch is not
  proof of VM correctness, full API/branch coverage, or all-schedule validation.
