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
  `b9be5b8e6e4a1330685ef60ff150903e90670d4c`, rebaselined from
  `e3d8c68281058d21a31aa5ac8e6a7e0b83db4aeb` in batch 1.
- `fixtures.py` selects 2,273 calls with metadata for all 191 exports in 12
  translation-fixture modules. This is direct-call coverage, not branch coverage.
- `lean_runtime.py` selects 3,606 public-behavior recipes inspired by Lean tests.
  Source attribution is not an assertion of theorem premises or proof coverage.
- `--lynx` freshly checks fixture laws. Five integer singleton-sum instances also
  require the proved postcondition to be true on both VMs. Other matching
  differential results alone do not establish a property.
- Generated testing currently covers eight operation families: comparisons,
  arithmetic, maps, integer bitstrings, float encoding, map/dictionary
  sequences, and selective-receive sequences. It does not generate arbitrary
  Erlang programs or enumerate scheduler interleavings.
- Dictionary queries, invalid receive timeouts, suspended receives, signal and
  alias combinations, and dictionary/scoped-split GC regressions already have
  coverage. Expand specific missing combinations rather than restarting these
  families. Short float formatting is outside the planned fixes.

The supported checkout has 25 Lean test files, including newly added
`Modules/Hashing.lean` and `Modules/Sets.lean`, versus the previous 23-file
baseline. Hashing still has no attributed recipes; batch 5 adds finite set
samples without interpreting the deliberately unmodeled theorem. Batch 1's
validation establishes fixture integration, not complete runtime-model support.

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

- [ ] Make the existing empty-sum postcondition an `assert_true` case, in addition
  to the five singleton instances. Test rejection when both VMs agree on false.
- [ ] Sample the higher-order map law with proper lists and callbacks that
  actually satisfy its requires predicate. Check the predicate on each VM before
  asserting the postcondition. A newly constructed identity closure is not
  automatically the same function value as a callback permitted by the law.
- [ ] Add model-inspired map lookup/equality and list properties where the
  observable relation is useful. Label them separately from freshly checked
  fixture laws; do not fabricate proof provenance for a runtime assertion.
- [ ] Cover empty, asymmetric multi-element, nested, and representable large
  integer inputs. Keep invalid inputs outside law premises as error-path tests.

Owners: `fixtures.py`, `validate.exs`, `test_validate.py`; small upstream Lynx
fixture changes may be needed to expose valid law inputs. Done when proof errors,
false preconditions, and false/nonboolean/exceptional postconditions fail clearly,
including replay, rather than being accepted as matching OTP.

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

- [ ] Inventory AtomVM support for `erlang:phash2/1,2` before implementing recipes.
  Treat absent support as an API gap, not a skipped successful case.
- [ ] Translate observable examples from `Modules/Hashing.lean`: signed and large
  integers, signed zeros, Unicode atoms, proper/improper lists, nested tuples/maps,
  byte binaries, partial bitstrings, and larger maps.
- [ ] Exercise default range, 1, non-power-of-two ranges, 2^32, and invalid ranges.
  Compare actual OTP results and independently check selected fixed vectors.
- [ ] Normalize model duplicate-binding maps to their effective public contents.
  Do not copy synthetic PID/reference/function identities into cross-VM hash
  expectations. Verify any live-identity hash properties within each run.

Owners: `lean_runtime.py`, `differential.erl`, `test_validate.py`; runtime support,
if needed, is separate. Done when a wrong hash or range reduction is detected,
not merely a result of the expected type.

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
- [ ] For lifecycle extensions, use same-sender fences and explicit handshakes.
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

### 7. Expand execution-path coverage and separate transport cost

- [ ] Exercise fixed boundary cases through the BIF and opcode paths where both
  exist. Inspect representative BEAM disassembly: equivalent source expressions
  need not exercise the same path across OTP versions.
- [x] Run interpreter and native/precompiled modes with matching runtime libraries.
- [ ] Extend to on-demand JIT and representative 32-bit/embedded/emulated execution
  in separate runner work; those modes are not supported by the current CLI.
- [ ] Use a matching-word-size OTP oracle for representation-dependent recipes,
  especially numeric key-search compatibility. Define float32 expectations
  separately rather than treating host binary64 output as universally applicable.
- [ ] Keep permanent GC/lifecycle regressions in ordinary AtomVM test suites and
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
