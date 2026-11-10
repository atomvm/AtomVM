<!--
Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Expanding Lynx validation on AtomVM

Grow coverage by observable behavior, proof premises, and execution path, not
just by adding cases. Keep this plan current as batches land; keep commands and
runner semantics in [README.md](README.md).

## Baseline — 2026-11-10

- The supported fixture revision is Lynx
  `b9be5b8e6e4a1330685ef60ff150903e90670d4c`, rebaselined from
  `e3d8c68281058d21a31aa5ac8e6a7e0b83db4aeb` in batch 1.
- `fixtures.py` selects 2,131 calls with metadata for all 191 exports in 12
  translation-fixture modules. This is direct-call coverage, not branch coverage.
- `lean_runtime.py` selects 3,277 public-behavior recipes inspired by Lean tests.
  Source attribution is not an assertion of theorem premises or proof coverage.
- `--lynx` freshly checks fixture laws. Five integer singleton-sum instances also
  require the proved postcondition to be true on both VMs. Other matching
  differential results alone do not establish a property.
- Generated testing currently covers five operation families: comparisons,
  arithmetic, maps, integer bitstrings, and float encoding. It does not generate
  arbitrary Erlang programs or enumerate scheduler interleavings.
- Dictionary queries, invalid receive timeouts, suspended receives, signal and
  alias combinations, and dictionary/scoped-split GC regressions already have
  coverage. Expand specific missing combinations rather than restarting these
  families. Short float formatting is outside the planned fixes.

The supported checkout has 25 Lean test files, including newly added
`Modules/Hashing.lean` and `Modules/Sets.lean`, versus the previous 23-file
baseline. Both new modules currently have no attributed recipes. Batch 1's
validation establishes fixture integration, not complete runtime-model support.

## Next batches, in order

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
- [ ] Extend map-comprehension observations beyond the existing fixture recipes.
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

- [ ] Cover construction, duplicate elimination, exact membership, size, union,
  intersection, subtraction, subset, and disjointness through supported public
  APIs. Include empty/disjoint/overlapping sets and numeric type distinctions.
- [ ] Request version 2 explicitly where representation matters. Observe sets by
  membership, size, and normalized elements rather than incidental map layout.
- [ ] Check invalid options/inputs against OTP separately from model examples.
  The model intentionally ignores requested version 1 and leaves some inputs
  unmodeled; neither choice is an AtomVM compatibility requirement.
- [ ] Check algebraic relations on bounded inputs with asymmetric controls, while
  keeping model-inspired properties distinct from freshly checked fixture laws.

Owners: `lean_runtime.py`, `differential.erl`, `test_validate.py`. Done when tests
catch loose numeric membership, lost elements, duplicate counts, and reversed
subtraction, without treating model exhaustion as an Erlang exception.

### 6. Generate bounded operation sequences

- [ ] Extend beyond independent scalar calls to reproducible dictionary/map
  updates and queries, binary scope combinations, and selective-receive sequences.
  Save the complete sequence as replay input and shrink without breaking setup.
- [ ] Use observed mailbox/dictionary state and returned values as the oracle.
  Include rejected messages, overwrites, missing entries, and unrelated sentinels.
- [ ] For lifecycle extensions, use same-sender fences and explicit handshakes.
  Sample documented allowed outcomes for races; do not compare cross-sender
  arrival order, elapsed time, or outcome frequencies as correctness requirements.
- [ ] Bound sequence length, allocations, and integer magnitude. Retain failing
  seeds and reduced cases; do not claim exhaustive schedules or general programs.

Owners: `generated.py`, `differential.erl`, `validate.py`, `test_validate.py`.
Done when a saved failure replays unchanged and shrinking preserves the tested
contract rather than producing a different setup or oracle failure.

### 7. Expand execution-path coverage and separate transport cost

- [ ] Exercise fixed boundary cases through the BIF and opcode paths where both
  exist. Inspect representative BEAM disassembly: equivalent source expressions
  need not exercise the same path across OTP versions.
- [ ] Run interpreter and native/precompiled modes with matching runtime libraries.
  Extend to on-demand JIT and representative 32-bit/embedded/emulated execution
  in separate runner work; those modes are not supported by the current CLI.
- [ ] Use a matching-word-size OTP oracle for representation-dependent recipes,
  especially numeric key-search compatibility. Define float32 expectations
  separately rather than treating host binary64 output as universally applicable.
- [ ] Keep permanent GC/lifecycle regressions in ordinary AtomVM test suites and
  run them under instrumentation. Do not add short internal deadlines to tests
  intended to check correctness rather than speed.
- [ ] Resolve the large-result transport bottleneck separately from split
  semantics. The 2,560-result probe computed the correct result; decimal byte-list
  formatting dominated its timeout. Measure computation, serialization,
  formatting, and output separately before changing the protocol or heap policy.
  Any transport change must preserve malformed-output detection and replay.

Done when results identify the actual execution mode, target configuration,
runtime/library hashes, and whether a timeout was semantic, oracle, or transport
work. Cross-compilation alone does not count as target execution.

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
