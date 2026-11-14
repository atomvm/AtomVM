<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Host-driven differential testing

See [PLAN.md](PLAN.md) for the coverage baseline, remaining gaps, and next batches.

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

The default is 1,000 cases, reproducible with `--seed`. Thirty-six boundary
cases precede seeded exploration (arranged so the first 32 cover all ten
operation families) so short runs still exercise known dangerous edges. This
generates inputs to ten operation families, not arbitrary Erlang programs:

- Numeric comparison: all eight equality/order operators, signed zeros,
  subnormal/extreme finite floats, and integer/float pairs around precision and
  representation boundaries, including 2^53 and 2^63.
- Integer arithmetic, division/remainder, shifts, bitwise operations, and floating
  division. Generated integer results stay within AtomVM's documented 256-bit
  magnitude; divide-by-zero errors are compared, not silently discarded.
- Maps: scalar, tuple, map, and partial-bitstring keys; duplicate and signed-zero
  keys; insertion, lookup, removal, update, and comparison; sizes near 32 entries.
- Map and process-dictionary sequences: an explicit initial binding list and
  at most 72 operations, with every returned value/exception and intermediate
  state compared. Include overwrites, missing entries, deletion/reinsertion,
  clearing, stored `undefined`, exact-value key selection, unrelated sentinels,
  and repeated growth/shrinkage across 32 entries. Keys include integer/float
  and signed-zero pairs, compound terms, and seeded partial bitstrings. Map
  sequences also exercise successful/rejected updates and takes. Enumeration
  order is ignored, but exact-key bindings and counts are retained.
- Selective-receive sequences: empty or mixed initial mailboxes, self-sends,
  exact/tuple-tag matches, guarded receives, multiple clauses, and catch-all
  receives. Include misses followed by new sends, duplicates, exact numeric and
  compound terms, guard errors/false results, and competing clause matches.
  Each sequence has at most 30 operations; all receives use timeout zero.
  Each prefix runs uninterrupted in a fresh monitored worker, then returns its
  last result and drains the remaining messages in order. This observes every
  intermediate mailbox without a drain/requeue snapshot resetting receive
  scanning between operations. Only the worker sends to itself; no cross-sender
  ordering, wall-clock timing, or scheduler outcome is used as an oracle.
- Binary scope sequences: at most 32 operations starting from explicit byte lists
  of up to 96 bytes. Search/split overlapping patterns in both orders, with paired
  positive/negative scopes, duplicate scopes, zero-width ranges, and optional
  global/trim/trim-all flags. Nested parts and appends change the subject so later
  coordinates must be relative to the current binary. Every result/exception and
  intermediate binary is compared; failed operations retain the previous state.
  Shrinking deletes operations without changing setup or literal payloads.
- Process-lifecycle sequences: stateful setups and sequences of 6 to 24
  operations (four deterministic boundary sequences up to 21 steps) across two
  worker slots, two registered-name slots, two monitor slots, and two alias slots.
  Steps cover `register/2`, `unregister/1`, `whereis/1`, local and peer links and
  unlinks, dead-process links under both `trap_exit` settings, fenced queued
  `'EXIT'` signals preserved across `trap_exit = false` transitions, monitor and
  demonitor options (`default`, `flush`, `info`, `flush_info`, and invalid
  options), standalone and monitor aliases, fenced cross-process/self alias
  sends, controlled child exits and respawns, and bounded unlink/exit and
  demonitor/exit race steps validating documented allowed-outcome predicates.
  Every step summary and intermediate snapshot is compared using symbolic slot
  labels rather than raw PIDs or references.
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
Byte-list output is emitted in bounded chunks, retaining the same wire format
without formatting the entire serialized trace as one giant list.

Seed `20261113` matches all 1,000 mixed cases against OTP 29 in both host
interpreter and aarch64 precompiled execution, including 112 binary sequences
with 3,497 operations. There are zero failures or unverified references. Thirteen
enabled CLI tests pass (one optional fixture test skipped), including independent
scope/split/state oracles and shrinking/replay with unchanged binary setup.

Seed `20261114` (after fixing `erlang:register/2` to reject processes and ports
already registered under another name) matches all 1,000 mixed cases against OTP 29
in both host interpreter and aarch64 precompiled execution, including 101
`lifecycle_sequence` cases with 1,561 operations. There are zero failures or
unverified references, with pre-fix boundary failure and minimized 2-step reproducer
artifacts preserved. Eighteen enabled CLI tests pass (one optional fixture test
skipped), including independent fenced lifecycle/race oracles and contract-preserving
shrinking/replay.
See [PLAN.md](PLAN.md) for evidence and execution-mode limitations.

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
as reductions. Sequence reduction deletes chunks of operations while keeping
initial bindings/mailbox, remaining operation payloads, seed, and index unchanged.
Keys/values are literal terms, not references to previous operation results;
an operation rejected after deletion is still observed, and execution continues.
Deleting a send may turn a later receive into an observed zero-timeout miss.
It preserves the distinction between value mismatches and target
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

At the pinned Lynx revision below, this selects **4,413 deterministic-result cases**:
numeric BIFs, rounding/conversions/formatting, tuple/list/bitstring APIs, binary
search/split, maps, version-2 set algebra, portable hashing (`erlang:phash2/1,2`),
process dictionaries, comparison, effectful short circuit, timeout validation
with retained mailboxes, monitored process/reference/dictionary handshakes, lifecycle
combinations, and UTF-8/16/32 construction/matching. UTF cases include malformed,
incomplete, unaligned, and valid codepoints with invalid suffixes. Child and
runtime-specific process/reference/closure hash identities are compared within each
run, never serialized as cross-runtime expectations.
This mode uses **AtomVM's bundled standard library**, not OTP's `lists.beam`.
Missing functions and differing error precedence remain failures, not skips.
OTP is the execution oracle, not the Lean model: for example, tested OTP 29
accepts an already queued receive match before checking an invalid timeout.
That differs from the model test named `timeout_validation_precedes_scan`.

The latest upstream refresh adds 69 cases: 25 exact list-subtraction samples,
16 example-inspired zip/unzip observations, and 28 timer observations. Timers use
message/monitor handshakes and validate remaining-time ranges rather than exact
elapsed milliseconds. Invalid arguments and missing APIs remain failures, not
skips. Zip/unzip examples are labeled separately from Lean test attribution.
See [the upstream refresh](PLAN.md#latest-upstream-refresh) for commit selection,
results, and model-only changes deliberately not transferred to VM requirements.
The full expanded interpreter corpus matches 4,108 cases, with 33 failures and
zero unverified references. The failures are four existing short-float mismatches
plus 29 new timer/unzip differences; all 4,072 old decoded results are unchanged.
A focused aarch64 precompiled replay flags the same 29 new cases (40 matches).
The expanded full native corpus was not rerun. No failures are allowlisted.
The subsequent [timer validation fix](PLAN.md#timer-argument-validation-follow-up)
removes eight argument-validation failures in focused interpreter/native replays;
the full-corpus figures above are the pre-fix baseline.
[Destination-death cleanup](PLAN.md#destination-death-cleanup-follow-up) fixes
four more observations for PID timers while retaining registered-name timers.
That focused replay matches 52 of 69 cases in both modes; the remaining
17 failures are missing unzip and `send_after/4` support.

The subsequent [`send_after/4` coverage expansion](PLAN.md#expanded-send_after4-differential-cases)
replaces five option probes with 52 OTP-extension cases, increasing the runtime
corpus to **4,188**. It covers relative and absolute delivery, past/current/future
monotonic deadlines, duplicate-option precedence, malformed options, invalid
arguments, and cancellation. Each VM constructs its own absolute timestamps;
past cases age a known-valid timestamp rather than preceding a fresh VM's start
time. Delivery checks include a not-before-deadline observation, not a tight
upper latency bound. Handwritten OTP expectations check all 52 cases independently.
The expanded focused replay matches 52 of 116 cases in both execution modes,
with 64 failures (52 missing `/4`, twelve missing unzip) and zero unverified
references. This is coverage only: no runtime API implementation is included,
and the full 4,188-case corpus has not been rerun.

Subsequent runtime fixes in `dbab35d88` (`send_after/4`) and `541596657`
(`lists:unzip/1`) resolve all 64 focused failures: both host-interpreter and
aarch64 precompiled replays match 116 of 116 cases against OTP 29.

The subsequent Batch 2 model-inspired map and list relation expansion adds 113
net new recipes (**4,301** runtime cases total; 133 updated or added entries in
the focused replay). Thirty-seven new `upstream_example` cases cover
`examples/otp/lists.exs#sort_contract_holds` and `sequence_formula`, and 96
Lean-attributed cases cover lookup/equality/merge/filter/guard relations,
singleton map iteration, list reversal/subtraction/sorting/membership
multiplicities, short-circuit improper-tail membership, map sizes 31/32/33,
nested terms, large integers through `(1 bsl 255) - 1`, and invalid-operand
error paths without fabricating proof provenance. Lean source attribution checks
validate 193 of 426 test sites across all 26 files. Seventeen enabled CLI tests
pass (one optional fixture test skipped), including handwritten OTP oracles for
the new relational probes. Focused 133-case replays match 127 cases with 6
failures and zero unverified references in both host-interpreter and aarch64
precompiled modes: four empty-step-range `lists:seq/3` divergences
(`seq_contract(5,4,2,[0])`, `(10,7,5,[0])`, `(4,5,-2,[0])`, `(7,10,-5,[0])`)
where AtomVM returns `[First]` instead of `[]`, and two `lists:reverse/1`
invalid-input exception differences (`badarg` vs OTP `function_clause`). The
full 4,301-case corpus has not been rerun; failures remain visible and
replayable under `.amp/in/artifacts/batch2-relations/`.

A subsequent `lists:seq/3` fix in `libs/estdlib/src/lists.erl` resolves all
four empty-step-range `seq_contract` failures: focused 133-case interpreter and
aarch64 precompiled replays now match 131 of 133 cases against OTP 29
(`.amp/in/artifacts/batch2-relations/seq-fix-{interpreter,native}`), retaining
only the two documented `lists:reverse/1` invalid-input exception differences.

Batch 4 adds 112 portable hashing recipes (`4,301 -> 4,413` runtime cases total)
attributed to all seven sites in `Modules/Hashing.lean` (200 of 428 Lean test
sites covered across all 26 files): all 78 unique vectors from
`Modules/Hashing.lean` (signed/large integers, signed zeros, subnormal float,
ASCII/Latin-1/Unicode atoms, byte binaries, partial bitstrings, proper/improper
lists, nested tuples/maps, and the 40-entry map), range reductions through `2^32`,
invalid zero/negative/oversized/non-integer ranges, normalized duplicate-binding
map equivalences across boundaries `31/32/33` and `40`, and within-run
closure/PID/reference properties. Both host-interpreter (`build`) and aarch64
precompiled (`.amp/in/build-native`) focused replays verify all 112 OTP 29
references (102 `{ok, ...}` results, 10 `error:badarg` exceptions) and retain
all 112 `error:undef` failures on AtomVM (`0` passed, `112` failed, `0`
unverified) because `erlang:phash2/1,2` is not yet implemented. Nineteen
enabled CLI integration tests pass (20 total; one optional fixture test skipped),
including independent handwritten Elixir/OTP hash and identity oracles. Evidence
is under `.amp/in/artifacts/batch4-hashing/`.

The execution-path extension adds 466 cases for 20 arithmetic, comparison, and
map-lookup operations. Each captures direct-body and dynamic `apply/3` outcomes
separately, plus guard match/rejection; map lookups also observe a map pattern.
Arithmetic guards match a zero result, comparison guards test the relation, and
map guards match the value `wanted`. Errors in guards reject the clause rather
than escaping as body exceptions. Floating results include their encoded bits
so signed zero and rounding differences cannot hide behind numeric equality.

OTP 29 disassembly confirms dynamic `erlang:apply/3`, comparison BIFs versus
guard test instructions, body/guard arithmetic failure labels, float-register
division versus guard GC-BIF division, and map-pattern `get_map_elements` versus
lookup BIFs. Compiler rewrites mean different source operators are not necessarily
different instructions; recheck disassembly when changing OTP versions.
All 466 added cases match OTP in both interpreter and aarch64 precompiled modes,
with zero failures or unverified references. Fourteen enabled CLI tests pass
(one optional fixture test skipped). The subsequent full 4,072-case run in each
mode matches 4,068 cases, retaining exactly the four known short-float formatting
mismatches, with zero unverified references or target timeouts. All decoded
results agree across modes, and the original 3,606 cases retain their saved
interpreter results. Both full runs exit nonzero for the four mismatches; no
failures are allowlisted. Earlier full-run results below are historical baselines.
Evidence and execution-mode limitations are in [PLAN.md](PLAN.md).

Before adding the 70 map-callback cases below, the 2026-11-10 host-interpreter
rebaseline matched 2,942 cases and retained four `[short]` float-formatting
mismatches, with zero unverified references. Short
formatting remains outside the planned fixes; those cases are still failures,
so the full runtime corpus exits nonzero. Hashing model sites do not yet have
selected recipes; the inventory must not be read as complete coverage. The set
extension below records option/input differences and their fixes.

An additional 116 integer-text recipes extend decimal model samples to binary
and list parsing/formatting in bases 2, 10, 16, and 36. They check signs, leading
zeros, mixed-case digits, invalid digits/suffixes/bases, boxed and large integer
values, and NUL bytes before or after valid digits. Both default and explicit
base parsers must reject the entire NUL-containing input rather than accepting
a prefix; list and binary results are checked directly, not only by round trips.

Seventy-five term-order recipes compare same-code closures with runtime captures,
including numeric types, signed zero, precision boundaries, nested containers,
and exact map keys. Both operand orders and equal-capture controls are checked.
Type-order samples include live references, local/external functions, and the
current PID, plus Unicode atoms and container boundaries; only comparison
booleans escape, never raw identities or generated function names. The source
inventory includes public as well as private and unqualified theorem sites.

Thirty map-fold callback cases extend the model's input-validation samples:
tuple accumulators count entries and sum asymmetric values, while callbacks
throw, error, or exit with a fixed reason. Empty maps and exhausted iterators
must not invoke callbacks; non-functions must still be rejected. Single-entry,
three-entry, and 40-entry maps exercise dispatch without asserting traversal
order, alongside invalid-map controls. These are public-API extensions, not
additional Lean proofs.

Seventy map-combination callback cases extend the newer model's `combine_inputs`
samples to `maps:merge_with/3` and `maps:filter/2`. They check empty, disjoint,
and overlapping maps; asymmetric callback arguments; exact numeric keys and
partial-bitstring keys; boxed values; sizes 31, 32, 33, and 40; callback
throw/error/exit; nonboolean predicates; invalid callbacks and operands.
Results include every callback's arguments, sorted without removing duplicates,
so unspecified traversal order is ignored but argument order, skipped entries,
extra calls, and effects before exceptions remain observable. Exceptional
callbacks have at most one eligible entry, avoiding an unspecified first key.
These are model-inspired public-API extensions, not fresh theorem checks.

The initial focused interpreter run matched 51 of these 70 cases and reported 19
mismatches: missing combiner validation in `merge_with/3` (including error
precedence), silently accepting a nonboolean filter result, and filter callback
validation taking lower precedence than a bad map. Importing PR #2355 resolved
the three filter failures and supplied `intersect_with/3` and other map helpers.
A separate combiner-validation fix rejects invalid functions before map operands,
including when there are no overlapping keys. No failures were allowlisted.

Another 261 recipes exercise the imported APIs: `intersect/2`, `intersect_with/3`,
`filtermap/2`, `take/2`, `update_with/3,4`, `with/2`, `without/2`,
`groups_from_list/2,3`, and exported iterator validation. They include exact keys,
duplicate requested/grouped keys, the stored atom `error` versus a missing key,
callback-free insertion of an initial value, callback exceptions, improper
lists, and error precedence. Group callback effects preserve their actual order.
Iterator observations cover default/ordered/reversed iteration, returned
key/value triples, partially consumed ordered iterators, exhaustion, and malformed
inputs. Results observe entries and specified order, not cross-VM iterator
layouts. These additions bring the map-attributed selection to 512 cases; they
remain finite public-behavior extensions, not new proved laws or exhaustive
map/comprehension coverage.

After integration and the combiner fix, all 512 map cases match OTP. The full
3,277-case interpreter run matches 3,273 cases and retains only the four known
`[short]` formatting failures; it checks zero fresh laws. The full fixture run
still checks four fresh laws, matches 2,127 calls, and reports four unresolved
OTP generator references with no differential failures. The ordinary map suite,
483 core test modules, and nine CLI tests pass; one optional fixture integration
test is skipped (the full fixture corpus is run separately).

Another 329 set recipes cover construction/deduplication, exact membership,
size/emptiness, add/delete, equality, binary and multi-set union/intersection,
subtraction in both directions, subset, and disjointness. Version 2 is explicit
in algebra operations. Inputs include integer/float and signed-zero distinctions,
nested keys, boxed integers, bitstrings, and asymmetric overlaps at sizes
3/4/5, 31/32/33, and 63/64/65. Public `to_list/1` elements are normalized to
an exact-key membership map plus size, emptiness, and enumeration count; no
set layout or unspecified traversal order is compared. Concrete outputs accompany
bounded commutativity, absorption, partition, disjointness, and cardinality
observations. Attribution to the model's `membership`, `size`, and `creation`
sites does not check theorem premises or the deliberately `unmodeled` site.

Before the empty-option fix below, the focused interpreter run on OTP 29
(ERTS 17.1.1, 64-bit) matches 273 cases,
reports 18 differences, and leaves zero unverified references. All explicit-v2
algebra cases match. Twelve differences concern constructor options: OTP accepts
`[]`, `[{ignored,true}]`, and `[{version,2}|bad]`, while AtomVM rejects them;
`[version]`, `[{version,3}]`, and `[{version,bad}]` produce different exception
reasons. Six more differences concern `subtract/2` and `is_subset/2` with an
empty left set and an invalid right operand (`bad`, `[]`, or `{set}`): OTP
returns an empty set or `true`, while AtomVM raises `function_clause`. These
are diagnostics, not newly blessed target behavior; invalid-input results do
not imply version-1 support is required. No runtime fixes or new proofs are
part of this extension. Ten CLI tests pass, including independent concrete set
oracles; one optional full-fixture test is skipped.

Before that fix, the full 3,568-case interpreter run matches 3,546 cases, with the 18 set
differences above and four unchanged `[short]` float-formatting mismatches.
There are zero unverified references and zero fresh laws. It still exits
nonzero; failures remain visible and replayable rather than being allowlisted.

The empty-option follow-up makes `sets:new([])` delegate to `new/0` and
`sets:from_list(List, [])` delegate to `from_list/1`, preserving default version 2.
The same 291-case set replay now matches 275 cases, retains the other 16
differences, and has zero unverified references or new differences. The ordinary
set tests fail before the fix and pass afterward on AtomVM and OTP, including
deduplication, exact numeric membership, empty input, and invalid-list rejection.
Other option handling is unchanged. The full runtime corpus was not rerun after
this localized fix.

The remaining-option/invalid-operand follow-up resolves the other 16 set
differences. Constructors use `proplists:get_value/3` for ignored options,
first-option precedence, improper tails, and OTP-style invalid-version errors.
Version 1 remains unsupported and still raises `function_clause`. Subtraction
and subset retain their map fast paths and add OTP's filter/fold fallbacks:
empty left sets do not inspect the right operand, while nonempty sets still
reject invalid right operands. Another 38 differential controls check duplicate
option precedence and invalid operands on both sides of empty/nonempty sets.
All 329 set cases match OTP, with zero failures or unverified references.
The expanded ordinary set suite passes on AtomVM and OTP; ten CLI integration
tests pass, with one optional fixture test skipped. Formatting checks pass.
A subsequent full 3,606-case interpreter run matches 3,602 cases, with only
the four unchanged short-float formatting mismatches and zero unverified
references. Evidence is under `.amp/in/artifacts/lynx-set-final-full/`.

Another 44 key-search cases check interior tuple positions, skipped short tuples,
first matches before improper tails, nested numeric equality, and exact map keys
with loosely equal values. Scalar precision-boundary cases check an OTP
compatibility quirk: OTP 29's integer-key fast path in `keyfind/3`
and `keymember/3` converts small integer keys to double when testing float fields.
On 64-bit OTP, integer key 9007199254740993 matches float 9007199254740992.0,
although `==` rejects that pair; swapping the key/field types or nesting the key
does not match. AtomVM mirrors that immediate-integer special case only in key
lookup, without weakening general or nested comparison. On 32-bit targets these
large integer keys are boxed and do not take that path; compare against an OTP
oracle with matching word size for these representation-dependent recipes.

The iodata grammar matrix has 96 cases across `iolist_to_binary/1`,
`iolist_size/1`, `list_to_binary/1`, and `list_to_bitstring/1` (86 additions to
the original conversion samples). It checks binary versus list roots, nested
binary tails, partial-bitstring elements/tails that only `list_to_bitstring/1`
accepts, and invalid nested bytes, floats, atoms, and improper integer tails.
Two 64-level input shapes exercise deep traversal and a buried invalid tail;
reused slices of a runtime-created large binary check exact bytes and size.
These public-API extensions do not imply additional Lean proofs or GC-pressure
coverage. In particular, byte-only APIs must reject partial input segments even
when concatenating those segments would produce a complete byte.

Another 88 binary search/split cases exercise overlapping patterns in both list
orders, earliest-match precedence, longest fitting matches, scope endpoints and
negative lengths, and first versus global splitting. Duplicate scopes must all
be valid even though the last one determines the search range; valid improper
option prefixes retain OTP's ignored-tail behavior. Invalid pattern lists are
checked on empty and nonempty subjects so an early match or empty input cannot
hide an invalid later pattern. These extend the model's binary API samples.

The lifecycle/receive cases cover linked normal/abnormal exits,
unlinking, trapped exits, untrappable `kill`, demonitor option validation and
active/queued/consumed monitor states, standalone and monitor aliases, pending
monitors while both the target and observer terminate, and empty-mailbox timeout
errors at and beyond the 32-bit boundary. Positive timer waits without a possible
sender exercise exception/cleanup scopes after suspension; no elapsed duration
or cross-sender message ordering is compared. Linked crashes are contained in
monitored workers. Queued-DOWN flush recipes requeue a received real DOWN so
mailbox presence is deterministic rather than inferred from a delay.
Thirty-six selective-receive cases check boolean, tuple, map, division, binary,
and alternative guard sequences against mixed, entirely rejected, and empty
mailboxes. Errors inside guards must reject rather than consume messages or
escape the receive. A second receive checks that retained candidates are scanned
again in order before a newly appended match, preserving all rejected messages.
Zero-timeout controls and positive waits with no matching sender cover both
immediate scans and timer suspension without asserting an elapsed duration.
Another 114 local-send cases compare `!` with runtime-dispatched `erlang:send/2`
across self/live/dead PIDs, registered and missing/unregistered names, ordinary
references, removed aliases, and invalid destination terms. Atom, nested
tuple/list/partial-bitstring/bignum, and map/binary payloads check exact return
values and delivery while preserving an unrelated mailbox sentinel. Receiver
acknowledgements and real DOWN notifications establish completion; process
identities are not serialized. Live ports and distributed destinations are
outside this batch.
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
Fifty-six target-caller cases pass the owner's monitor reference to the monitored
process itself. Active incoming monitors and owner-removed controls cover all four
monitor kinds and seven cancellation actions. Owner install/removal signals are
fenced by a message from the same owner before the target attempts cancellation.
These cases compare raw return/error results as well as caller/owner mailboxes,
alias delivery, and the owner's remaining monitor state. In particular, flushing
must still scan the caller's mailbox without removing an incoming monitor.
Sixty paired-monitor cases give the same owner two independently created monitors
of one target, with every pairing of plain and the three monitor-alias kinds.
Keeping, demonitoring, unaliasing, or replying through the first reference must
leave the second monitor intact. Reply delivery is fenced before termination;
real DOWN notifications, post-termination alias delivery, and final reference
states distinguish monitor cancellation from alias removal. Plain references are
not used as reply destinations, and cross-reference DOWN order is not compared.
Another 138 monitor-option cases cover malformed lists and alias tuples, invalid
alias modes, invalid entries before or after valid options, and all nine ordered
pairs of repeated alias options. Direct and registered self/live targets, dead targets, and missing
names check that validation is not bypassed by a target shortcut. Repeated options
check last-option-wins behavior through the remaining alias state, rather than
only accepting a returned reference. Tagged and priority monitors are outside
this option-validation batch.
Thirty-six standalone-alias cases add the `alias/0` default, invalid `alias/1`
options and improper tails, repeated `reply`/`explicit_unalias` options, and
`unalias/1` argument validation. Repeated options check whether exactly one or
both messages arrive, together with removal results, not just reference creation.
These cases also distinguish non-reference errors from an ordinary non-alias
reference returning false. Existing monitor-alias lifetime controls fence
post-event delivery before removing a surviving explicit alias; sending alone
does not establish delivery. Priority aliases remain outside this batch.

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
and `#guard ` sites in all 25 test files, recording source hashes, lines, sample
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
API currently targets Lynx revision `b6ac6ea905fa5b387e33cd0b4466a6abcd9d31de`,
updated from `2aa1467d4c28c4bffd5f5d205ebb197e261f85a4`; see
[the latest refresh](PLAN.md#latest-upstream-refresh) for current results.
The current full fixture run checks nine laws and matches 2,347 calls, with no
differential failures or uncovered exports and the same four unverified OTP
references. All 78 protocol calls match in interpreter and aarch64 precompiled
execution. Lynx's own suite reports 165 passed checks plus its Lean test build.

The following rebaseline results are historical. On OTP 29
(ERTS 17.1.1), Elixir 1.20.5, and Lean 4.35.0, the original rebaseline checked
four laws; its 2,131-call interpreter run matched 2,127 references, with zero
differential failures, four unresolved OTP references, and no uncovered exports.
Lynx's own `mix test.all` also passes. This supports the fixture integration,
not every public API in the expanded model: see [the inventory review](PLAN.md#inventory-review)
for confirmed API gaps and unrepresented behavior.

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

Five existing `sum_singleton` samples additionally require the proved
postcondition to return **true on both OTP and AtomVM**, not just equal results.
Inputs are 0, −1, 1, 2^60, and 2^100; each first checks the law's integer
precondition. The empty-sum postcondition and thirteen protocol postconditions
are also asserted true, for nineteen runtime assertions total. Protocol samples
check applicable premises before invoking their postconditions. Noninteger sum
inputs remain differential/error-path samples outside the law's premise. These
laws already exist in Lynx; no new theorem is added.

These cases carry `"assert_true": true` in `cases.json`. False, nonboolean, or
exception results fail even if both runtimes agree; the marker survives failure
replay. Include `--lynx ../lynx` when replaying these fixture calls to package
the fixtures and freshly check their laws. The assertion marker alone does not
invoke Lean or establish a proof. This checks finite runtime instances of a
proved model property, not correctness of the VM or the translation/compiler.

The corpus contains **2,351 calls covering all 203 exports in 13 fixture modules**
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
- Consolidated Elixir protocols: list/map/struct dispatch, Any fallback,
  recursive implementation calls, unsupported types, and law postconditions.

The map-comprehension extension adds 142 OTP differential recipes across seven
existing `named_locals` exports, without adding proofs or runtime assertions.
It checks last-write-wins list collisions, skipped patterns, improper tails,
exact numeric and boxed keys, strict generators, lazy inner-generator validation,
and nested/filtered generators. Runtime-created maps span sizes 0, 1, 2, 31, 32,
33, 63, 64, and 65. Unordered generator results retain exact bindings and item
counts; colliding map-to-map results check the output key set and validity of
chosen bindings, without assuming a winning source key or traversal order.

All 142 added cases match OTP 29. That historical interpreter run matched 2,269 of
2,273 cases, with zero mismatches and the same four unverified zero-width-generator
references; those unresolved cases still make the full run exit nonzero.

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
The protocol fixture packages Lynx's executable `Elixir.Lynx.Support.*` modules
(including consolidated protocols) and additionally loads AtomVM's bundled
`exavmlib.avm`. Build that library before running `--lynx`. Its path and hash are
recorded in `run.json`; host Elixir standard-library beams are not packaged.
Interpreter execution uses the exact proof-input fixture binaries. For a
precompiled build (`AVM_ENABLE_PRECOMPILED=ON`, `AVM_DISABLE_JIT=ON`), the script
transforms those same binaries
to native-only code using that build's JIT compiler, including the bundled
exavmlib when protocols are selected. It selects the matching native runtime
library and eavmlib archive automatically, loading native libraries before
bytecode copies embedded in the aggregate archive. Build the matching eavmlib
target too (for example, `eavmlib_aarch64`); extra library hashes are recorded.
Emscripten/Node (`wasm32`) is supported with `--host-build`; bare-metal boards
remain unsupported.

Use `--timeout` to adjust the default five-second bound per runtime call. Host
compile/proof/compare commands have a ten-minute bound. `run.json` records the
resolved `execution_mode` (`interpreter`, `on_demand_jit`, `precompiled`, or
`jit+precompiled`), `target` (`platform`, `arch`, `word_size`, `float_bits`, and
CMake feature flags), and SHA-256 hashes for the runtime, optional `.wasm`
module, and libraries. `failures.json`, `unverified.json`, and `actuals.json`
classify errors and timeouts by stage (`oracle`, `semantic`, or `transport`,
separated by `LYNX_SEMANTIC_DONE\n` after `actual()` completes).

Additional target and representation modes:

- On-demand JIT (`AVM_DISABLE_JIT=OFF`, `AVM_ENABLE_PRECOMPILED=OFF`): keep
  probe/helper BEAMs in bytecode form (`--timeout 30` recommended for cold JIT
  compilation) so `code_server` compiles them on demand at runtime.
- Emscripten / `wasm32` (`src/platforms/emscripten` with `-DAVM_EMSCRIPTEN_ENV=node`):
  pass `--build <wasm-build> --host-build <host-build>` so `node` executes
  `AtomVM.mjs` while `<host-build>` supplies `PackBEAM` and
  `atomvmlib-emscripten.avm`; 64-bit-immediate `word_size_dependent` recipes
  (`lists:keyfind/3` / `lists:keymember/3` at `±9007199254740993`) are marked
  `unverified` (`stage: "oracle"`) against a 64-bit host OTP oracle.
- Single-precision floats (`AVM_USE_32BIT_FLOAT=ON`): pass `--float32` to add
  10 IEEE-754 `binary32` runtime arithmetic/rounding/overflow/underflow cases
  with explicit `float32_expected` ETF oracles; unannotated `binary64` float
  results are marked `unverified` (`stage: "oracle"`).

Scheduler interleavings, GC stress, and bare-metal embedded boards remain outside
the CLI runner. `.github/workflows/lynx-differential.yaml` pins upstream Lynx
`b6ac6ea905fa5b387e33cd0b4466a6abcd9d31de` in a dedicated workflow without adding
a Lynx dependency to the normal AtomVM build/test workflows. Run `--help` for
options.

The 2026-11-11 aarch64 precompiled replay matches OTP 29 for both saved
1,000-case seeded batches (`20261111` and `20261112`). The saved runtime corpus
matches 3,602 of 3,606 cases, with exactly the four known short-float formatting
mismatches and zero unverified references. All 5,606 decoded native results equal
their interpreter baselines. Probe/helper binaries contain native code and no
BEAM `Code` chunk; on-demand JIT is disabled. This does not imply native execution
of every bundled library module. See [PLAN.md](PLAN.md) and
`.amp/in/artifacts/lynx-native-validation.json` for configuration and evidence.

The runner's CLI integration tests check seeded replay, independent encoding byte
oracles, deliberately wrong/malformed target output, reduction and replay, and
continuation after reference timeouts/undefined calls. They also check scoped Lean
sample inventories, unverified counts, stale source attribution rejection,
on-demand JIT compilation, `oracle`/`semantic`/`transport` stage classification,
32-bit word-size / `--float32` representation guards, and independent
Unicode/short-circuit/process/receive/hashing result oracles:

```sh
python3 -B -m unittest discover -s tools/test/lynx -p test_validate.py -v
```

Twenty-two enabled CLI tests pass under `ATOMVM_JIT_BUILD=.amp/in/build-on-demand`
(1 optional `LYNX_CHECKOUT` fixture test skipped). Set `LYNX_CHECKOUT=/path/to/lynx`
to enable the additional full-fixture/export inventory and partial-replay integration
test. It requires Mix/Lean on PATH and takes several minutes; ordinary integration
tests do not require Lynx.
