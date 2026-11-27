<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Lynx coverage TODOs

Status as of 2026-11-27, against Lynx
`ff3aad6f39e196ce539a73e1f51f8c3a1b69f68c`. The supported-behavior priorities
below follow an audit of the current recipes and unsampled sites. The original
six-item shortlist from screening 195 incoming commits is retained afterwards;
persistent terms and hashing are not the only remaining work.
See [PLAN.md](PLAN.md) for implementation details and validation results, and
[README.md](README.md) for runner usage. Completed items describe the targeted
batches, not exhaustive coverage or proof of every scheduler interleaving.

## Supported-behavior priorities

Prefer these batches before adding APIs. Keep runtime corrections and standalone
Erlang regressions separate from Lynx coverage.

### 1. Custom map-iterator ordering and consumers

- [x] Add custom deterministic total orders through `next`, `fold`, `map`,
  `filter`, `filtermap` and `foreach`, extending the earlier predefined-order
  recipes and basic standalone comparator regression.
- [x] Cover partial/exhausted iterators, empty/singleton and 31/32/33-entry maps,
  and retained iterators after updates to the original map value.
- [x] Cover invalid ordering arguments and comparator exceptions. Compare exact
  consumer visits, not comparator call order/count or unspecified key ties.
- [x] Fix invalid-order exception behavior separately, with an Erlang regression.

**Resolved difference:** OTP 29.1.3 raises `error:badarg`, but unchanged AtomVM
raised `error:function_clause` for a bad atom or functions of arity 0/1/3 as the
order, with empty/singleton/multi-entry maps. Non-map `iterator/2` inputs also
incorrectly raised `badmap`; `iterator/1` must retain that exception. The new
standalone regression passes OTP, fails unchanged AtomVM, and passes the fixed
runtime in interpreter, AArch64 precompiled and on-demand JIT modes.

Added 240 permanent differential recipes, with independent expected-value and
filtered/reordered replay checks. All match OTP in interpreter, AArch64
precompiled and on-demand JIT modes. See
[PLAN.md](PLAN.md#custom-map-iterator-ordering-and-consumers) for scope and
validation; no expected failures were added.

### 2. Structural BIF execution paths

- [x] Extend body/dynamic-call/guard comparisons to `length/1`, `hd/1`, `tl/1`,
  `element/2`, `tuple_size/1`, `byte_size/1` and `bit_size/1`.
- [x] Distinguish body/dynamic exceptions from guard rejection using opaque
  arguments. Include empty/improper lists, invalid tuples and index boundaries,
  partial bitstrings, heap/sub-binaries and live values across calls/GC.
- [x] Inspect emitted BEAM before claiming distinct opcode paths; different
  source forms can compile identically. Avoid duplicating existing map guards.

Added 70 permanent recipes with independent expected values and mixed/reordered
saved replay, extending the existing arithmetic/comparison/map-get observer.
Each compares three paths before and after explicit GC. Body and guard BIF
instructions have different failure targets; dynamic calls use `erlang:apply/3`.
These are dispatch/failure contexts, not dedicated structural opcodes. All 70
match OTP in interpreter, AArch64 precompiled and on-demand JIT modes, without
runtime fixes or expected failures. See
[PLAN.md](PLAN.md#structural-bif-execution-paths).

### 3. Exit reception across trap-flag changes

- [x] Combine unfenced trap-flag transitions with `exit_signal/2`, legacy
  `exit/2` controls and alias deactivation. Existing transition cases fence
  reception; the older asynchronous-exit batch holds the flag fixed.
- [x] Use isolated monitored workers, exact controls and allowed outcome sets.
  Distinguish an already converted EXIT message from a pending signal; never
  require a particular scheduling winner or claim every allowed outcome occurred.

Added 54 recipes: 36 deterministic controls and 18 unfenced samples, both flag
transitions and normal/structured/kill reasons. Require the allowed-outcome
predicate to be true on each VM, retain actual traces, and independently check
those traces and filtered/reordered replay. The two exploratory legacy self-exit
trace differences are admitted by the source-checked contract: these are **not
confirmed bugs**. Legacy self-suicide is selected at send time; queued messages
must survive later flag changes or alias removal. See
[PLAN.md](PLAN.md#exit-reception-across-trap-flag-changes).

The final receive deliberately establishes reception before a surviving worker
reports. This does not cover returning before any reception (the separate
`exit_signal_self_is_handled_later` model example), all schedules or all allowed
outcomes. The separate unfenced-completion batch below samples the first gap.

### 4. Liveness/dead-query fast paths and queued notifications

- [x] Target `alive_may_leave_monitor_parked` and
  `process_info_of_dead_does_not_flush`: local queries need not establish that
  the caller drained its own incoming notifications.
- [x] Add exact live/dead controls, allowed notification traces, retained mailbox
  sentinels and monitor cleanup. Do not repeat the existing kill/query,
  trap-after-DOWN, unlink/relink/kill or ending-process request batches.

Added 46 asserted recipes: six monitor/liveness cases and 40 query/notification
cases, with independent raw-trace tables and filtered/reordered replay. An OTP
survey observes all four liveness/DOWN combinations, including `{true, noproc}`;
none is required as a race winner. Known-dead queries use self-sent async signals
and a separate prepared target, with exact query-before-signal and
signal-received-before-query controls, plus held-live controls. This isolates
caller reception from target-death timing rather than reproducing the model's
killer/query program literally. A final receive still forces fatal reception;
returning without one and all-schedule coverage remain outside this batch. See
[PLAN.md](PLAN.md#livenessdead-target-queries-with-pending-notifications).

### 5. Compiler-lowered receive continuations

- [x] Sample upstream's consecutive timeout-only receive loops and receives
  with dropped unused clause bodies, pairing valid lowered Core with source
  controls and inspecting emitted operations.
- [x] Preserve skipped messages across waits and check exception/after
  continuations, exact integer-vs-float selection, mailbox-vs-clause priority,
  captured values, once-only effects and cleanup overriding an earlier error.

The batch adds 32 recipes: 16 lowered-Core cases and 16 source controls.
Positive opaque timeouts retain actual waits. A rescan control first skips the
whole mailbox and times out, then consumes a previously skipped match. Independent
expected tables check each VM's results, effect trace and remaining mailbox;
filtered/reordered replay checks Core companion reindexing. See
[PLAN.md](PLAN.md#lowered-receive-continuations).

An optimized-away `receive after 0` does not establish signal reception.
Receive-marker instructions alone also do not establish an optimized mailbox-skip
path: the interpreter currently treats them as no-ops apart from register clearing.
This batch does not claim a new mailbox-skip optimization or all compiler shapes.

### 6. Unfenced self-exit completion

- [x] Separate a post-function-return report from the worker's final DOWN, with
  no explicit worker-side receive, mailbox query or acknowledgement on the
  unfenced path. Add PID self-signals only; leave aliases/flag transitions in
  their existing batches.
- [x] Pair async cases with legacy self-`exit/2` and real receive-fenced async
  controls, both trap settings and normal/structured/kill reasons.
- [x] Use exact outcomes for the 15 deterministic recipes and strict allowed
  combinations for the three fatal unfenced async recipes; retain raw traces
  and independently validate report content/count, exit reason and monitor cleanup.

The batch adds 18 asserted recipes. An unfenced fatal signal can stop the worker
before its report, after its report, or lose to normal process completion. A normal
DOWN is not by itself a passing observation: it must have the exact return report,
except for the legacy untrapped-normal control, which must never report. Both OTP
and AtomVM actually completed normally after unfenced self-`kill` in the isolated
sweep; receive-fenced self-`kill` terminates with `killed` and no report. The report
proves the callback returned, not that a signal was pending at that moment or
that the worker stayed alive afterwards. See
[PLAN.md](PLAN.md#unfenced-self-exit-completion).

### 7. Seeded queue histories (follow-up)

- [x] Extend isolated queue observations into deterministic and seeded histories
  that cross empty/singleton boundaries and exercise front/rear rebalancing.
- [x] Mix insertion/removal at both ends, reverse, split and asymmetric joins;
  compare exact contents and returns, not internal queue representations.
- [x] Include first/last exact deletion, membership, invalid split recovery,
  empty-operation exceptions and saved immutable queues across operations/GC.
- [x] Check both VMs against an independent ordered-list oracle and exercise
  filtered/reordered replay and setup-preserving shrinking.

This is an additional supported-behavior gap found after the original audit,
not an unsampled Lean site or an API addition. Four boundary sequences extend
normal seeded generation; the earlier 44 fixed queue observations remain intact.
All 148 current histories (4,498 operations) and nine-case reordered replay
match OTP and the independent list model in interpreter, AArch64 precompiled
and on-demand JIT modes. No runtime fix or expected-failure entry was needed.
See [PLAN.md](PLAN.md#seeded-queue-sequences) for the observer contract and validation.

### 8. Queue callbacks (follow-up)

- [x] Cover `filter/2` list expansion, `filtermap/2`, noncommutative `fold/3`,
  short-circuit `any/2` and `all/2`, and first/last predicate deletion.
- [x] Use equivalent queues from `from_list`, repeated `in` and repeated `in_r`;
  include empty/singleton, invalid callbacks, non-boolean results, each exception
  class after preceding effects, retained original queues and explicit GC.
- [x] Independently check both VMs' results and callback effects, including raw
  unordered predicate visits and exact multiplicities, plus reordered replay.
- [x] Keep the discovered `lists:any/all` error correction and standalone Erlang
  regression separate from the Lynx coverage.

Adds 153 permanent recipes (total 7,082) without changing attribution. The initial
six mismatches were non-boolean predicate results: `any` raised `badarg`, while
`all` returned `false`, instead of OTP's `{case_clause, Result}`. The separate
library regression checks exact exceptions, effect prefixes and short circuit.
All 153 cases and 14-case reordered replay now match OTP and the independent
model in interpreter, AArch64 precompiled and on-demand JIT modes, without new
expected failures. See [PLAN.md](PLAN.md#queue-callback-combinations) for scope
and validation.

**Separate compiler follow-up:** appending the observer to the large existing
helper exposed an AArch64 out-of-range ADR in `jump_to_continuation/2`.
The observer is instead a separate compiled/packaged module; the prior helper
is unchanged. This keeps the coverage usable but does not fix the compiler limit.
Retain `.amp/in/artifacts/lynx-queue-callbacks/native/precompile.log` and its
combined helper source/BEAM as the reproducer for that separate investigation.

### 9. Set callbacks (follow-up)

- [x] Cover supported v2 `fold/3`, `filter/2`, `map/2` and `filtermap/2` through
  `from_list`, repeated additions and deletion/reinsertion histories.
- [x] Include exact numeric keys, mapping collisions, duplicates, empty/singleton
  and 31/32/33-element sets, invalid callbacks, non-boolean predicate results,
  each exception class after preceding effects, and original sets retained across GC.
- [x] Independently check exact result membership, cardinality, callback counts,
  raw visit multiplicities and fold accumulator threading. Accept unspecified
  traversal orders and exception prefixes; include filtered/reordered replay.
- [x] Correct `sets:filter/2`'s non-boolean predicate exception separately, with
  an ordinary Erlang regression in `test_sets.erl`, outside the Lynx coverage.

Adds 140 permanent differential recipes, preserving all 7,082 previous cases
and metadata (total 7,222). Initial interpreter, AArch64 precompiled and on-demand
JIT sweeps each matched 137/140, isolating one filter exception defect across
three construction histories, not a visit-order difference. The separate library
correction now raises OTP's `bad_filter` for non-boolean results while preserving
predicate-thrown exceptions and `maps:filter/2`'s behavior. Interpreter and
precompiled reruns match 140/140 with 16/16 replays; JIT reruns match 38/38 filter
recipes and a 16/16 mixed replay. The other 102 JIT recipes passed before the
filter-only fix and were not rerun afterward. The new CLI regression passes.
Ordinary `test_sets`/`test_maps` pass on OTP 27.3.4.12, 28.3.1 and 29.1.3 and in
all three AtomVM modes. No expected-failure entries were added.
See [PLAN.md](PLAN.md#set-callback-combinations) for execution-mode results and scope.

### 10. Ordered list folds and mapfolds (follow-up)

- [x] Cover `foldl/3`, `foldr/3`, `mapfoldl/3` and `mapfoldr/3` with exact
  callback order and every incoming accumulator, not just final sums.
- [x] Check mapped output order separately from right-to-left invocation order;
  include accumulator replacement, duplicate/exact numeric and compound elements,
  boxed values, captured data, and original inputs retained across explicit GC.
- [x] Include error/throw/exit on the first, second and final callback, unused
  failing callbacks, malformed mapfold returns, invalid callbacks and improper tails.
- [x] Check independent ordered contracts and filtered/reordered replay; reject
  identically wrong reports with reversed visits, broken threading or reversed output.
- [x] Correct callback validation and malformed-list exception differences
  separately, with ordinary Erlang regressions in `test_lists.erl`, outside Lynx.

Adds 128 recipes (29 per fold, 35 per mapfold), preserving all 7,222 previous
recipes and metadata; total 7,350, attribution unchanged at 377/478 sites.
Initial interpreter, AArch64 precompiled and on-demand JIT runs each matched
98/128, exposing 24 callback-validation differences, five `foldr/3` malformed-input
exceptions and one `foldl/3` non-list-entry exception. The separate library fix
adds entry guards and proper recursive exception boundaries without changing
`reverse/1` or translating callback exceptions. Interpreter, precompiled and
on-demand JIT reruns each match 128/128 with 15/15 reordered replays.
Full list/queue/set/map ordinary
tests pass on OTP 27.3.4.12, 28.3.1 and 29.1.3 and in all three AtomVM modes.
A 10,000-element validation of all four operations also passes in every mode.
No expected-failure entries were added. See
[PLAN.md](PLAN.md#ordered-list-folds-and-mapfolds) for mode/replay results and limits.

### 11. Ordered unary list callbacks (follow-up)

- [x] Cover `map/2`, `foreach/2`, `flatmap/2`, `filter/2`, `filtermap/2`,
  `search/2` and `dropwhile/2` with exact output, callback visits/indices,
  captured values and original inputs retained across explicit GC.
- [x] Check invalid callbacks, empty/proper/improper/non-list inputs,
  first/second/final callback exceptions and return-value validation.
- [x] Distinguish flatmap empty/single/multiple expansion and deferred validation
  from incorrectly preempting a later callback exception.
- [x] Check search/dropwhile first/middle/final/absent stops, exact numeric matches,
  and improper suffixes that must remain untouched after a short-circuit return.
- [x] Add independent sequential contracts, positive/negative model controls
  and filtered/reordered replay.
- [x] Correct the observed callback-validation and map-entry exceptions, and
  search's acceptance of non-boolean predicate results, with separate ordinary
  Erlang regressions rather than expected-failure entries.

Adds 268 recipes, preserving all 7,350 preceding cases and metadata (total 7,618;
attribution remains 377/478). Initial interpreter, AArch64 precompiled and
on-demand JIT runs each matched 239/268, exposing 22 callback-validation cases,
six search predicate-result failures and one map non-list-entry failure; their
21-case reordered replays matched 19/21. A separate library correction and
ordinary Erlang regressions resolve those differences, preserving callback
exceptions, short-circuiting and deferred flatmap validation. Fresh runs in all
three modes each match 268/268 and their reordered replays match 21/21, with
independent checks of both VMs. OTP 27.3.4.12, 28.3.1 and 29.1.3 each satisfy
all 268 contracts and pass the ordinary regressions. The harness passes all 46
enabled methods (three optional skips). No expected-failure entries were added. See
[PLAN.md](PLAN.md#ordered-unary-list-callbacks) for execution-mode/replay results,
independent checks and limits.

### 12. Paired-list zipwith callbacks (follow-up)

- [x] Cover `lists:zipwith/3` with asymmetric paired inputs, exact argument and
  result order, duplicates, numeric distinctions and arbitrary returned terms.
- [x] Check unequal lengths in both directions, non-list inputs and improper
  tails on either/both sides, preserving effects from the matching prefix.
- [x] Distinguish invalid functions and arities 0/1/3 on empty, matching and
  unmatched inputs. Do not import unary APIs' entry-validation contract.
- [x] Check first/second/final callback errors/throws/exits, unused failing
  callbacks and exceptions that precede a later length/tail failure.
- [x] Retain both inputs, captures and boxed results across explicit GC;
  independently check both VMs and exercise filtered/reordered replay.

Adds 95 recipes, preserving all 7,618 preceding cases and metadata (total 7,713;
fixture calls/exports and Lean attribution unchanged). OTP 27.3.4.12, 28.3.1
and 29.1.3 each satisfy all 95 independent contracts. A positive model control
passes; five controls reject identically wrong reports with swapped arguments,
reversed visits/output or eager input/callback validation. Interpreter,
precompiled and on-demand JIT each match 95/95, with 14/14 reordered replays and
identical complete observations. Independent checks pass for both VMs in all six
runs. The full 50-method harness passes (47 passed, three optional skips).
No library/runtime fix or expected-failure entry is included. See
[PLAN.md](PLAN.md#paired-list-zipwith-callbacks) for execution-mode results,
independent checks and limits.

### Comparator-driven sort, usort and merge

- [x] Add 148 recipes with non-strict ascending/descending comparators, stable
  equal-key ordering, first usort representatives and left-before-right merge
  ties. Preserve all 7,713 previous recipes and metadata (total 7,861).
- [x] Cover invalid callbacks/arities, non-boolean returns, constant
  errors/throws/exits and no-comparison controls, without asserting algorithm-
  dependent invocation counts, pair order or argument-dependent failures.
- [x] Retain original inputs, captures and boxed results across explicit GC;
  check both VMs against independent ranks and exercise reordered replay.
- [x] Check all 148 contracts on OTP 27/28/29 and reject six identically wrong
  report pairs with a passing positive model control.
- [x] Fix stable ties in `sort/2` and `merge/3`, with ordinary Erlang regressions
  separate from this coverage. Initial runs exposed six sort and eight merge
  tie mismatches. Keep natural/key sorting stable with non-strict comparators.
- [x] Match OTP's callback-validation boundaries for `sort/2` and `merge/3`,
  with separate ordinary Erlang regressions. Initial runs exposed six sort
  and fifteen merge validation mismatches. Keep usort entry validation and
  first representatives independent of the sorting correction; fix default
  usort's integer/float representative choice too.

Initial runs matched 113/148 and 22/30 reordered cases in all three modes.
A separate library/ordinary-regression/changelog correction resolves the
observed differences without an allowlist. Sort, merge, validation and default-
usort regression groups each fail against the old library and pass afterward;
existing keysort/ukeysort controls remain green. Ordinary lists/queue/sets/maps
suites and independent 10,000-item controls pass on OTP 27/28/29 and all three
AtomVM modes. Fresh interpreter, precompiled and JIT runs each match 148/148,
with 30/30 reordered replays and independent checks on both VMs. The full
51-method CLI suite passes (48 passed, three optional skips). See
[PLAN.md](PLAN.md#comparator-driven-sort-usort-and-merge) for the complete
validation results, independent contracts and scope.

### Keyed-list equality, mutation and sorting

- [x] Add 344 differential recipes for `keydelete`, `keyreplace`, `keystore`,
  `keytake`, `keysort` and `ukeysort`, preserving all 7,861 preceding recipes
  and metadata (total 8,205; fixture counts and Lean attribution unchanged).
- [x] Cover first numeric-equivalent matches, lone equivalent float keys,
  duplicate representatives, 2^53 boundaries, positions one/two, malformed
  elements, invalid arguments and reached/unreached improper tails.
- [x] Retain inputs/replacements/captures across GC and check both VMs against
  independent first-match/rank contracts, including reordered replay.
- [x] Validate all contracts on OTP 27/28/29; reject seven identically wrong
  report pairs, with a passing positive control.
- [x] Fix numeric equality separately in `keydelete`, `keyreplace` and
  `keystore`, with ordinary Erlang regressions for lone equivalent floats,
  earlier float/later integer matches, nested keys and untouched improper tails.
- [x] Fix `ukeysort` duplicate removal separately, retaining the first
  integer/float-equivalent key representative; add ordinary Erlang regressions,
  including the large-number precision boundary.
- [x] Match positive-integer index validation in the five failing APIs, with
  ordinary Erlang regressions. `keytake` retains its existing correct guard.
- [x] Match sort entry exception reasons for non-list and one-cell improper
  inputs separately, with ordinary Erlang regressions and lazy singleton controls.

Initial interpreter and AArch64 precompiled runs each matched 260/344 and
exposed 84 differences: 12 each in keydelete/keyreplace/keystore, 20 in keysort
and 28 in ukeysort. All 56 keytake observations matched. Each mode's 50-case
reordered replay matched 39 and differed on 11, as did the on-demand JIT subset.

A separate library/ordinary-regression/changelog correction resolves these
compatibility groups without changing any recipes, independent expectations
or allowlists. Fresh interpreter and precompiled runs match 344/344 and their
50-case replays pass, including the matching on-demand JIT subset. Ordinary
lists/queue/sets/maps suites and 10,000-item keyed-list controls pass on OTP
27/28/29 and all three AtomVM modes. The full CLI suite passes (49 passed,
three optional skips). See
[PLAN.md](PLAN.md#keyed-list-equality-mutation-and-sorting) for final JIT subset,
CLI results and validation scope; the full on-demand JIT matrix remains unrun.

### Audit scope and exclusions

At audit time the baseline had 6,469 runtime recipes, 2,351 fixture recipes
covering 203 exports, and attribution for 373/478 sites in 28 Lean files. The 105
unattributed sites classify as 71 model/proof contracts, 19 blocked persistent-term
sites, 11 represented/overlapping sites and four partially sampled signal
behaviors. Attribution is not branch coverage: all Maps sites already had labels,
yet custom-order differential combinations were missing. The iterator batch above
adds 240 recipes; the structural BIF batch adds another 70 (total 6,779),
without changing attribution or fixture counts.

Tagged monitors explicitly raise `unsupported`; priority exits/monitors and
distribution require separate support work, not an assumption that the APIs are
already implemented. Existing expected failures are compatibility work rather
than newly missing tests. No full corpus, fresh Lean laws, JIT or embedded-target
validation was run for this audit.

Local evidence: `.amp/in/artifacts/lynx-coverage-audit/` contains the regenerated
inventory/classification, exploratory probes, and focused interpreter/precompiled
map results. These are audit records, not new expected-failure entries.

## 1. Function identity through real consumers

- [x] Distinguish local and external functions for the same MFA, including
  `erlang:make_fun/3`, application and function type.
- [x] Exercise equality, map/dictionary keys and collection consumers, including
  captured identities and copy/GC boundaries.

## 2. Queued liveness and termination requests

- [x] Kill then immediately query `process_info/2`, without a completion fence.
- [x] Enable trapping after a linked child's normal `DOWN`; reject a late normal
  `EXIT`.
- [x] Exercise unfenced unlink/relink/kill with linked and unlinked controls.
  Repetition creates scheduling opportunities; it does not prove an unlink
  acknowledgment was pending.
- [x] Fan-out termination and requests to ending processes: 18 observations with
  1/4/16 peers, normal/shutdown/kill fan-out and live/dead/racing request controls.
  Check eventual replies, per-recipient causal order and allowed outcomes, not
  an arbitrary cross-sender winner or simultaneous delivery. See the
  [batch details](PLAN.md#fan-out-termination-and-ending-process-requests).

The targeted batch exposed two ordering differences, fixed separately. Native
validation also exposed a JIT-only handoff bug, fixed with a deterministic
Erlang regression test.

## 3. Persistent terms

- [ ] Add explicit differential coverage for `persistent_term:get/1`, `get/2`,
  `put/2`, `put_new/2` and `erase/1`.
- [ ] Cover missing keys/defaults, overwrite and repeated writes, erase results,
  exact keys, retained values after replacement/GC, cross-process visibility and
  persistence after the writer exits.

**Blocked on runtime support:** all five probed entry points returned `undef`
on AtomVM. Keep missing APIs visible rather than counting unsupported results
as matches. Capability-gate newer APIs on the OTP oracle. Serialize writes for
exact traces; use allowed outcomes for competing writers/erase.

## 4. Async `erlang:exit_signal/2`

- [x] Implement the new API with standalone Erlang regressions, independently
  of helpers that call `exit/2`, and add 56 differential observations covering
  normal/abnormal/kill signals, self-signal processing, PID/alias destinations,
  reply-alias ordering, inactive references and invalid/dead destinations.

The oracle requires OTP 29 or newer; older references are unverified, not
matches. Fatal calls run in monitored workers. The implementation supports local
processes, ports and aliases; distributed destinations and priority options in
`exit_signal/3` remain outside this batch. See the
[batch details](PLAN.md#asynchronous-exit-signals).

## 5. Compiler-folded exceptions and optimized Core evaluation order

- [x] Compare compiler-known and runtime-argument function-clause failures.
- [x] Add raw-Core compilation/replay and effect/exception-order observations,
  alongside ordinary-source controls.
- [x] Expand into binary construction, Core try-result binding and exception
  boundaries, escaping recursive closures and nested fallthrough continuations.

These execute compiled BEAM on AtomVM, not Lynx's direct Core translator.
Source and raw Core need not have identical evaluation order or emitted layout.

## 6. Bloom-filter integration workload

- [ ] Exercise real hashing through bit positions, integer bitsets, membership
  and union; compare exact bitsets as well as absence of false negatives.
- [ ] Cover empty inputs, duplicate insertions, asymmetric unions, zero hashes
  and bit/word boundaries, with filter width at most 256 bits.

**Blocked by missing `phash2/1,2`.** Do not substitute a stub hash and claim
coverage of the upstream integration. Keep exhaustive query counting small;
finite samples do not establish a statistical false-positive distribution.
