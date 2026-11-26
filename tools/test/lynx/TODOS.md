<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Lynx coverage TODOs

Original priorities from screening 195 incoming Lynx commits after updating to
`ff3aad6f39e196ce539a73e1f51f8c3a1b69f68c`. Status as of 2026-11-26.
See [PLAN.md](PLAN.md) for implementation details and validation results, and
[README.md](README.md) for runner usage. Completed items describe the targeted
batches, not exhaustive coverage or proof of every scheduler interleaving.

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
