<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Lynx coverage TODOs

Status as of 2026-11-26, against Lynx
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

- [ ] Combine unfenced trap-flag transitions with `exit_signal/2`, legacy
  `exit/2` controls and alias deactivation. Existing transition cases fence
  reception; the asynchronous-exit batch holds the flag fixed.
- [ ] Use isolated monitored workers, exact controls and allowed outcome sets.
  Distinguish an already converted EXIT message from a pending signal; never
  require a particular scheduling winner or claim every allowed outcome occurred.

Two exploratory legacy self-exit traces differed from OTP when compared exactly.
These are **not confirmed bugs**: reception timing may legitimately differ.
Build a valid allowed-outcome observer before drawing runtime conclusions.

### 4. Liveness/dead-query fast paths and queued notifications

- [ ] Target `alive_may_leave_monitor_parked` and
  `process_info_of_dead_does_not_flush`: local queries need not establish that
  the caller drained its own incoming notifications.
- [ ] Add exact live/dead controls, allowed notification traces, retained mailbox
  sentinels and monitor cleanup. Do not repeat the existing kill/query,
  trap-after-DOWN, unlink/relink/kill or ending-process request batches.

Source-audited candidate only; no new defect or runtime result is claimed.

### 5. Compiler-lowered receive continuations (lower priority)

- [ ] Investigate upstream's consecutive timeout-only receive loops and receives
  with dropped unused clause bodies, pairing valid lowered Core with source
  controls and checking emitted operations before adding cases.
- [ ] Preserve skipped messages across waits and verify exception/after
  continuations. Existing source fixtures already cover nested receives and
  suspended exception scopes; avoid duplicating those observations.

An optimized-away `receive after 0` does not establish signal reception.
Receive-marker instructions alone also do not establish an optimized mailbox-skip
path: the interpreter currently treats them as no-ops apart from register clearing.

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
