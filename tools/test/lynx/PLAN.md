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
- `lean_runtime.py` selects 16,990 public-behavior recipes inspired by Lean tests
  and upstream examples (`377` of `478` Lean test sites across all 28 files). Source
  attribution is not an assertion of theorem premises or proof coverage.
- `--lynx` freshly checks nine fixture laws. Nineteen finite instances require
  the proved postcondition to be true on both VMs: five integer singleton sums,
  the empty sum, and thirteen protocol postconditions. Other matching
  differential results alone do not establish a property.
- Generated testing currently covers eleven operation families: comparisons,
  arithmetic, maps, integer bitstrings, float encoding, map/dictionary
  sequences, selective-receive sequences, binary scope sequences, queue
  sequences, and process-lifecycle sequences. It does
  not generate arbitrary Erlang programs or enumerate scheduler interleavings.
- Dictionary queries, invalid receive timeouts, suspended receives, signal and
  alias combinations, and dictionary/scoped-split GC regressions already have
  coverage. Expand specific missing combinations rather than restarting these
  families. Short float formatting is outside the planned fixes.

The supported checkout has 28 Lean test files, including new concurrency audits
and `Modules/PersistentTerm.lean`. New model files do not imply runtime support:
`persistent_term` remains an observed API gap. Local `erlang:exit_signal/2` is
implemented with the separate regression and differential batch below.
Fixture validation does not establish complete runtime-model support.

## Integer and atom conversions (`erlang`)

Add 1,371 direct recipes for all 17 exported integer and atom conversion
functions in `erlang` (`integer_to_list/1,2`, `integer_to_binary/1,2`,
`list_to_integer/1,2`, `binary_to_integer/1,2`, `atom_to_list/1`,
`atom_to_binary/1,2`, `list_to_atom/1`, `list_to_existing_atom/1`,
`binary_to_atom/1,2` and `binary_to_existing_atom/1,2`) via standalone helper
`int_atom_conv:observe/5`, preserving all 15,619 preceding recipes and metadata;
total 16,990. Fixture counts and Lean attribution are unchanged; no new APIs or
Lean proofs are added.

- Check signed and unsigned integer formatting and parsing across bases `2..36`,
  leading signs/zeros (`+0`, `-0`, 300 leading zeros), `INT64_MIN`/`INT64_MAX`
  and `INT64_MIN * Base` boundaries, multi-chunk bigints up to `±(2^256 - 1)`,
  ASCII digit/punctuation boundaries, embedded NUL, improper lists, partial
  bitstrings, invalid bases, and `bit_slice` binary inputs (`0..7`).
- Check atom conversions across `utf8`, `unicode`, and `latin1`, empty/ASCII/
  NUL-containing/Latin-1 high-byte/UTF-8 multi-byte boundaries (`0..0x10FFFF`),
  32/40/63/64/255-char Latin-1 and 255-emoji (`1020`-byte) atoms, `latin1`
  non-representable code-point errors, 255/256-character limit vs element and
  improper-tail validation precedence, and disjoint pre-existing vs
  never-created atoms.
- Exercise ETF-copied inputs, bit-sliced binary inputs (`0..7`), repeated
  list/binary builders (`rep_list`, `rep_bin`), and dynamic atoms (`rep_atom`)
  with operands, offsets, arguments, captures and results retained across
  explicit GC before and after each call.
- `INT_ATOM_MODEL` implements base-`2..36` integer conversion, byte-list
  validation, RFC 3629 UTF-8/Latin-1 conversion, and atom ETF inspection
  directly in pure Elixir without calling any tested `erlang` conversion BIF/NIF
  or `:unicode`, checking complete observations on both VMs.
- **Resolved runtime defects (kept cleanly separable):** probing exposed four C
  runtime bugs: (1) `int64_parse_ascii_buf` in `src/libAtomVM/utils.c` read
  past `buf[0]` for sign-only `"+"`/`"-"` inputs (returning `0` instead of
  `badarg` in bases `2..9, 11..36` and reading `buf[1]` OOB in base 10);
  (2) `list_to_atom` in `src/libAtomVM/bif.c` ran unbounded `term_list_length`
  before checking the 255-character limit and used `SYSTEM_LIMIT_ATOM` for
  `list_to_existing_atom/1` as well as trailing elements beyond 255 chars;
  (3) `nif_erlang_atom_to_binary` in `src/libAtomVM/nifs.c` sized heap
  allocation from UTF-8 `atom_len` instead of decoded `encoded_len` (risking
  heap under-allocation when `atom_len >= 64` and `encoded_len < 64`) and
  decoded with a fixed length `2` after only checking partial UTF-8;
  (4) `intn_parse` in `src/libAtomVM/intn.c` negated signed `INT64_MIN` chunk
  values (`parsed_chunk = -parsed_chunk`) and checked `out_len` instead of
  `new_out_len` on overflow. Extended `test_binary_to_integer_2`,
  `test_list_to_integer`, `test_list_to_atom`, `test_list_to_existing_atom`,
  and `test_atom_to_binary` (verified against OTP 29 and before-fix failures
  on `build` and `.amp/in/build-native`) alongside a `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 1,371/1,371 against OTP.
Reordered on-demand JIT replay matches 104/104, covering all 17 functions,
sign-only inputs, `INT64_MIN` bigints, 255/256 atom boundaries, Latin-1 heap
boundaries, and all eight bit offsets `0..7`, with zero failures or unverified
cases. Independent contracts pass on all three VMs, complete native and JIT
observations equal interpreter results, and fifteen local wrong-result controls
are rejected. Observer-only controls match all 1,371 OTP reference observations
on OTP `27.3.4.12`, `28.3.1` and `29.1.3`. Twelve ordinary suites
(`test_binary_to_integer`, `test_binary_to_integer_2`, `test_list_to_integer`,
`test_integer_to_binary`, `test_integer_to_list`, `test_list_to_atom`,
`test_list_to_existing_atom`, `test_binary_to_atom`,
`test_binary_to_existing_atom`, `test_atom_to_binary`, `test_atom_to_list` and
`test_utf8_atoms`) pass on OTP 27/28/29 and all three AtomVM modes, and
`./build/tests/test-erlang` prints `Success.`. Targeted CLI tests
(`test_int_atom_conversions_and_replay`, `test_unicode_preserves_encodings_errors_and_replay`,
and `test_remaining_behavioral_probes_have_independent_oracles`) pass (`Ran 3 tests in 326.438s -> OK`).

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-int-atom/`.
Values outside AtomVM's 256-bit `intn` range remain excluded alongside the full
runtime corpus, full CLI/JIT matrices, complete older-host harness, Lean fixture
laws and embedded/32-bit/float32 targets.

## Unicode conversions (`unicode`)

Add 571 direct recipes for all 5 exported functions in `unicode`
(`characters_to_list/1,2` and `characters_to_binary/1,2,3`) via standalone
helper `unicode_conv:observe/5`, preserving all 15,048 preceding recipes and
metadata; total 15,619. Fixture counts and Lean attribution are unchanged; no
new APIs or Lean proofs are added.

- Check valid UTF-8, `unicode`, and `latin1` conversions across ASCII, Latin-1
  (`0..255`), 2/3/4-byte UTF-8 boundaries (`0x7F`, `0x80`, `0x7FF`, `0x800`,
  `0xD7FF`, `0xE000`, `0xFFFF`, `0x10000`, `0x10FFFF`), empty chardata, nested
  lists, and improper binary tails (`[$a | <<"b">>]`).
- Check invalid UTF-8 decoding (stray continuations, overlong 2/3/4-byte forms,
  UTF-16 surrogates, `> 0x10FFFF`), truncated UTF-8 (`incomplete` vs `error`
  classification, including trailing empty lists/binaries preserving
  `incomplete` vs trailing non-empty data), out-of-range integer elements
  (negatives, surrogates `0xD800..0xDFFF`, `> 0x10FFFF`, `latin1` `> 255`,
  bigints), non-chardata roots/elements/tails, and invalid encoding atoms.
- Exercise copied, nested `deep`, and bit-sliced binary leaves (`0..7`) with
  operands, offsets, encodings, captures and results retained across explicit GC
  before and after each call.
- `UNICODE_MODEL` implements RFC 3629 UTF-8 encoding/decoding, Latin-1 checks,
  and recursive chardata traversal directly in pure Elixir without calling
  `:unicode`, checking complete observations on both VMs.
- **Resolved runtime defects (kept cleanly separable):** probing exposed bugs in
  `src/libAtomVM/interop.c` (plus a defensive `term_is_binary` guard in
  `src/libAtomVM/otp_crypto.c`): (1) `latin1` `characters_to_list/2` did not
  reject code points `> 255` in size calculation, causing uninitialized garbage
  output and heap overflow; (2) `latin1` `characters_to_binary/2,3` accepted
  integers `> 255`; (3) UTF-8/unicode conversions accepted UTF-16 surrogates
  (`0xD800..0xDFFF`) as list elements; (4) `interop_chardata_fold` raised
  `badarg` on bigint elements (`term_is_integer` vs `term_is_any_integer`)
  instead of `{error, ...}`; (5) trailing empty chardata (`[]`, `<<>>`,
  `[[], <<>>]`) after truncated UTF-8 flipped `{incomplete, ...}` to
  `{error, ...}`. Added `test_latin1_range_surrogates_bigints_and_trailing_empty/0`
  to `tests/erlang_tests/test_unicode.erl` (verified to fail before the C fixes
  and pass afterward across interpreter, AArch64 precompiled, on-demand JIT and
  OTP 29) alongside a `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 571/571 against OTP.
Reordered on-demand JIT replay matches 94/94, covering all five exports,
encodings, error/incomplete paths, surrogates, bigints, bit offsets `0..7`, and
`deep` nesting, with zero failures or unverified cases. Independent contracts
pass on all three VMs, complete native and JIT observations equal interpreter
results, and fifteen local wrong-result controls are rejected. Observer-only
controls match all 571 OTP reference observations on OTP `27.3.4.12`, `28.3.1`
and `29.1.3`. Nine ordinary suites (`test_unicode`, `test_ets`,
`test_binary_to_term`, `test_crc32`, `test_list_to_binary`,
`test_list_to_bitstring`, `test_binary_part`, `test_split_binary` and
`test_binary_eq`) pass on OTP 27/28/29 and all three AtomVM modes, and
`build/tests/test-erlang` prints `Success.`. Targeted CLI checks cover the new
batch/replay, `ets_table` batch/replay and behavioral oracles.

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-unicode/`.
OTP-vs-AtomVM documented differences (`latin1, latin1` negative integers in
`test_unicode.erl`, split multi-byte UTF-8 sequences across separate list
binaries, and OTP UTF-16/UTF-32 encodings) remain outside this batch alongside
the full runtime corpus, full CLI/JIT matrices, complete older-host harness,
Lean fixture laws and embedded/32-bit/float32 targets.

## ETS tables (`ets`)

Add 237 direct recipes for all 15 exported functions in `ets` (`new/2`,
`insert/2`, `insert_new/2`, `lookup/2`, `lookup_element/3,4`, `member/2`,
`update_element/3,4`, `update_counter/3,4`, `take/2`, `delete/1,2`, and
`delete_object/2`) via standalone helper `ets_tables:ets_table/4`, preserving
all 14,811 preceding recipes and metadata; total 15,048. Keeping the observer in
`tools/test/lynx/ets_tables.erl` avoids exceeding AArch64 `jit_aarch64_asm`'s
`±1 MiB` `adr` branch range on `differential.erl`. Fixture counts and Lean
attribution are unchanged; no new APIs or Lean proofs are added.

- Check `ets:new/2` option validation, invalid/non-atom names, improper lists,
  option precedence across `[public,private]`, `[private,protected]`,
  `[public,protected]`, `[set,bag]`, `[bag,set]`, `[bag,duplicate_bag]`,
  `[duplicate_bag,bag]`, and `[{keypos,1},{keypos,2}]`, `named_table`
  collisions, `delete/1`, and recreation.
- Check `set`, `bag`, and `duplicate_bag` CRUD semantics across `keypos` `1..3`,
  atomic multi-tuple `insert_new/2` rollback and trailing element validation,
  `update_element/3,4`, `update_counter/3,4` threshold clamping and bad op lists,
  `lookup_element/3,4` missing/default/bounds behavior, `take/2`, `delete/2`,
  `delete_object/2`, post-delete access, and invalid table handles.
- Check exact vs numeric keys (`1` vs `1.0`, `0.0` vs `-0.0`, `>64`-bit
  bigints, maps, improper lists, bitstrings, and bit-sliced binaries at bit
  offsets `0..7`), cross-process `private`/`protected`/`public` access, and
  table cleanup on owner exit, retaining operands, options, modes and captures
  across explicit GC before and after step execution.
- `ETS_MODEL` implements a pure-Elixir ETS table state machine directly without
  calling `:ets`, checking complete observations on both VMs.
- **Resolved runtime defects (kept cleanly separable):** probing exposed three
  bugs in `src/libAtomVM/ets.c` and `src/libAtomVM/nifs.c`: (1) `apply_op`
  used `assert(term_is_tuple(op))`, crashing the VM instead of raising
  `badarg` on non-tuple entries inside `update_counter/3,4` operation lists;
  (2) `lookup_or_default` only checked `tuple_arity(default) <= key_index`
  when inserting a missing key instead of checking before lookup; (3)
  `nif_ets_new` accepted unknown options and improper lists and applied wrong
  conflict precedence (`public > private`, `duplicate_bag > bag`). Added
  `test_option_validation_and_bad_counter_ops/0` to
  `tests/erlang_tests/test_ets.erl` (verified to fail before the C fixes and
  pass afterward on interpreter, AArch64 precompiled, on-demand JIT and OTP 29)
  alongside a `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 237/237 against OTP.
Reordered on-demand JIT replay matches 92/92, covering all 15 operations, all
three table types, option validation/precedence, `named_table`, `-0.0`, bit
offsets `0..7`, `update_counter` bad op lists, cross-process access, and
owner-exit cleanup, with zero failures or unverified cases. Independent
contracts pass on all three VMs, complete native and JIT observations equal
interpreter results, and sixteen local wrong-result controls are rejected.
Observer-only controls match all 237 OTP reference observations on OTP
`27.3.4.12`, `28.3.1` and `29.1.3`. Nine ordinary suites (`test_ets`,
`test_binary_to_term`, `test_crc32`, `maps_nifs`, `test_list_to_binary`,
`test_list_to_bitstring`, `test_binary_part`, `test_split_binary` and
`test_binary_eq`) pass on OTP 27/28/29 and all three AtomVM modes, and
`build/tests/test-erlang` prints `Success.`. Targeted CLI checks cover the new
batch/replay, `binary_etf` batch/replay and behavioral oracles.

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-ets/`. Default
tuples shorter than `keypos` when the key already exists are gated under OTP 29
in `test_ets.erl` due to OTP 27/28 vs 29 divergence (`otp/issues/10603`). Full
runtime corpus, full CLI/JIT matrices, complete older-host harness, Lean
fixture laws, integer overflow beyond `avm_int_t` in `update_counter`, and
embedded/32-bit/float32 targets remain outside this batch.

## External term format (`term_to_binary` and `binary_to_term`)

Add 836 direct recipes for `erlang:term_to_binary/1,2` and
`erlang:binary_to_term/1,2`, preserving all 13,975 preceding recipes and
metadata; total 14,811. Fixture counts and Lean attribution are unchanged; no
new APIs or Lean proofs are added.

- Check external term decoding across `SMALL_INTEGER_EXT`, `INTEGER_EXT`,
  `SMALL_BIG_EXT` (positive and negative `> 64`-bit integers), `NEW_FLOAT_EXT`
  (including subnormals and negative zero sign-bit preservation via
  `b2t_float_bits`), `ATOM_UTF8_EXT`/`SMALL_ATOM_UTF8_EXT`, `NIL_EXT`,
  `STRING_EXT`, `LIST_EXT` (proper and improper tails), `SMALL_TUPLE_EXT`,
  `LARGE_TUPLE_EXT`, `BINARY_EXT`, `BIT_BINARY_EXT` (widths `1..8`, dirty tail
  bit masking, and embedded `[used]` offset advancement), `MAP_EXT` (numeric
  key distinction `1` vs `1.0` and duplicate-key rejection), and trailing bytes.
- Check `binary_to_term/2` option handling (`[]`, `[used]`, `[safe]`,
  `[used,safe]`, `[safe,used]`, `[used,safe,used]`, `[safe,safe]`, and strict
  rejection of invalid/improper option lists), `[safe]` existing-vs-unknown
  atom rejection (`lynx_etf_never_atom_1`/`lynx_etf_never_atom_2`), invalid
  UTF-8 atoms, malformed `BIT_BINARY_EXT`, truncated/malformed headers, and
  non-byte-aligned bitstrings.
- Check canonical wire output (`t2b_wire`) across scalar/compound terms and
  `term_to_binary/2` option validation (`[]`, `[deterministic]`,
  `[{minor_version,0..2}]`, `[compressed]`, `[{compressed,0..9}]`, and invalid
  or improper option lists), plus full round-trip invariants (`roundtrip_2`)
  across deep mixed improper-list/tuple/map trees (`deep` mode) and bitstring
  widths `1..7`.
- Exercise copied inputs and larger-parent binary/bitstring leaves sliced at bit
  offsets `0..7` with operands, offsets, arguments, captures and results retained
  across explicit GC before and after each call.
- `BINARY_ETF_MODEL` implements pure-Elixir ETF wire encoding/decoding, option
  validation and deep-tree modeling directly without calling
  `:erlang.binary_to_term/1,2` or `:erlang.term_to_binary/1,2` to compute
  contracts, checking complete observations on both VMs.
- **Resolved runtime defects (kept cleanly separable):** probing exposed two OTP
  29 compatibility bugs: (1) `nif_erlang_binary_to_term` in
  `src/libAtomVM/nifs.c` used `interop_proplist_get_value_default`, accepting
  unknown options, `{opt,bool}` tuples, improper option lists and non-list tail
  garbage instead of raising `badarg`; (2) `calculate_simple_heap_usage`
  (`NEW_FLOAT_EXT`) in `src/libAtomVM/external_term.c` accepted IEEE-754
  non-finite exponent `0x7FF` payloads (`+inf`, `-inf`, quiet/signaling NaN)
  instead of raising `badarg`. Added `test_options_and_non_finite_floats/0` to
  `tests/erlang_tests/test_binary_to_term.erl` (verified to fail before the C
  fix and pass afterward across interpreter, AArch64 precompiled, on-demand JIT
  and OTP 29) alongside a `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 836/836 against OTP.
Reordered on-demand JIT replay matches 94/94, covering all five operations,
option validation, non-finite floats, `-0.0`, `[safe]` existing and unknown
atoms, `[used]`, bit offsets `0..7`, partial bitstrings `1..7` and `deep`
round-trips, with zero failures or unverified cases. Independent contracts pass
on all three VMs, complete native and JIT observations equal interpreter
results, and fifteen local wrong-result controls are rejected. Fresh
observer-only controls match all 836 OTP reference observations on OTP
`27.3.4.12`, `28.3.1` and `29.1.3`. Nine ordinary suites (`test_binary_to_term`,
`test_crc32`, `test_binary_part`, `test_split_binary`, `test_binary_copy`,
`test_binary_replace`, `test_list_to_binary`, `test_list_to_bitstring` and
`test_binary_eq`) pass on OTP 27/28/29 and all three AtomVM modes, and
`build/tests/test-erlang` prints `Success.`. Targeted CLI checks cover the new
batch/replay, `binary_crc32` batch/replay and behavioral oracles.

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-binary-etf/`.
Zero-length `BIT_BINARY_EXT` (`<<131,77,0,0,0,0,0>>`) is left to
`test_binary_to_term.erl`'s ERTS version guard due to OTP 27.3.4.12/28.3.1 vs
29.1.3 divergence (`CVE-2026-54890`). Full runtime corpus, full CLI/JIT
matrices, complete older-host harness, Lean fixture laws and
embedded/32-bit/float32 targets remain outside this batch.

## CRC32 checksums and combination

Add 428 direct recipes for supported `erlang:crc32/1`, `erlang:crc32/2` and
`erlang:crc32_combine/3`, preserving all 13,547 preceding recipes and metadata;
total 13,975. Fixture counts and Lean attribution are unchanged; no APIs or Lean
proofs are added.

- Check exact IEEE 802.3 CRC32 checksums across empty binaries/iolists (`<<>>`,
  `[]`, `[[]]`, `[<<>>]`, `[[]|<<>>]`), single/all-256 byte values, standard
  vector `<<"123456789">>`, nested/improper-tail iolists and 63/64/65/257-byte
  inputs.
- Check incremental chaining (`crc32/2`) across seeds `0`, `1`, `12345`,
  `2^31` and `2^32 - 1`, including empty iolists and split-vs-whole
  equivalence.
- Check `crc32_combine/3` across `len2 = 0`, small/power-of-two/65536 lengths,
  `2^31 - 1`, `2^31` and `2^32 - 1`, and split-message algebraic equivalence
  (`crc32_combine(crc32(A), crc32(B), iolist_size(B))`).
- Exercise ETF-copied inputs and larger-parent binary leaves sliced at bit
  offsets `0..7` with operands, offsets, arguments, captures and results retained
  across explicit GC before and after each call.
- Reject partial bitstrings, out-of-range byte elements, invalid improper tails,
  non-iodata roots, and negative, non-integer or `>= 2^32` CRC/length bounds.
- `BINARY_CRC32_MODEL` implements IEEE 802.3 (`0xEDB88320`) bit-by-bit,
  recursive iodata flattening and GF(2) matrix-squaring `crc32_combine`
  directly without calling `erlang:crc32*` or `zlib`, checking complete
  observations on both VMs.
- **Resolved runtime defects (kept in a separate commit/group):** initial probing
  exposed two `WITH_ZLIB` bugs in `src/libAtomVM/nifs.c`: (1) `nif_erlang_crc32`
  passed `NULL, 0` to zlib's `crc32` for empty iolists, resetting `OldCrc` to
  `0` instead of preserving it; (2) `nif_erlang_crc32_combine_3` validated
  `size2` with `term_is_int64` under `WITH_ZLIB`, accepting `>= 2^32` instead of
  raising `badarg` like OTP 27/28/29 and AtomVM's non-zlib path. Added
  `test_crc32_empty_iolist_and_combine_bounds/0` to
  `tests/erlang_tests/test_crc32.erl` (verified to fail before the C fix and
  pass afterward on interpreter, AArch64 precompiled, on-demand JIT and OTP 29)
  alongside the `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 428/428 against OTP.
Reordered on-demand JIT replay matches 109/109, explicitly covering all four
operations, empty-iolist `crc32/2`, `2^31`/`2^32 - 1`/`2^32` combine bounds,
split-combine identities and all eight bit offsets, with zero failures or
unverified cases. Independent contracts pass on both VMs, complete native/JIT
observations equal interpreter results, and thirteen local wrong-result controls
are rejected. Fresh observer-only controls match all 428 OTP reference
observations on OTP 27.3.4.12/28.3.1/29.1.3. Eight ordinary suites
(`test_crc32`, `test_list_to_binary`, `test_list_to_bitstring`,
`test_binary_to_list`, `test_bitstring_to_list`, `test_binary_part`,
`test_split_binary` and `test_binary_eq`) pass on OTP 27/28/29 and all three
AtomVM modes. Targeted CLI checks cover the new batch/replay, `binary_iodata`
batch/replay and behavioral oracles.

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-binary-crc32/`.
Full runtime corpus, full CLI/JIT matrices, complete older-host harness, Lean
fixture laws, non-zlib (`#ifndef WITH_ZLIB`) builds and embedded/32-bit/float32
targets were not run. No allocation-failure, performance, binary-size or leak
measurements are claimed.

## Iodata and bitstring-list conversion

Add 840 direct recipes for supported `binary:list_to_bin/1`,
`erlang:list_to_binary/1`, `erlang:iolist_to_binary/1`, `erlang:iolist_size/1`
and `erlang:list_to_bitstring/1`, preserving all 12,707 preceding recipes and
metadata; total 13,547. Fixture counts and Lean attribution are unchanged; no
APIs, runtime fixes or Lean proofs are added.

- Check root grammar distinctions: bare binaries (including empty, 1/4/63/64/65/
  257-byte and bit-sliced binaries) are accepted by `iolist_to_binary/1` and
  `iolist_size/1` and rejected with `badarg` by `list_to_bin/1`,
  `list_to_binary/1` and `list_to_bitstring/1`.
- Check proper, nested, empty and depth-33/65 nested lists across all 256 byte
  values and output lengths crossing 63/64/65/257 bytes.
- Check improper binary tails (accepted by all five APIs), improper
  partial-bitstring tails (accepted only by `list_to_bitstring/1`), and invalid
  integer/atom tails (rejected by all five APIs).
- Check bit-level concatenation in `list_to_bitstring/1` across every
  trailing-bit width 1–7 (zero, all-ones and asymmetric values), byte-aligned
  complement sums, and multi-fragment cross-byte packing, while verifying all
  four byte-oriented APIs reject partial bitstrings in head, nested or tail
  positions.
- Exercise ETF-copied inputs and larger-parent binary/bitstring leaf extraction
  at bit offsets 0–7 without calling any of the five APIs to construct
  operands; retain inputs, offsets, captures and results across explicit GC
  before and after each conversion.
- Reject out-of-range integers (`-1`, `256`, `1 bsl 64`, `-(1 bsl 64)`), floats,
  atoms, tuples and invalid roots.
- `BINARY_IODATA_MODEL` flattens recursive lists into bit lists directly
  without calling any conversion/size API and validates complete observations on
  both VMs.

Interpreter and AArch64 precompiled runs each match 840/840 against OTP.
Reordered on-demand JIT replay matches 158/158, explicitly covering all five
operations, bare binaries, improper binary and bitstring tails, all eight bit
offsets, all seven partial widths, cross-byte bit packing and deep nesting,
with zero failures or unverified cases. Independent contracts pass on both VMs,
complete native/JIT observations equal interpreter results, and thirteen local
wrong-result controls are rejected. Fresh observer-only controls match all 840
OTP reference observations on OTP 27.3.4.12/28.3.1/29.1.3. Eight ordinary
binary/iodata suites (`test_list_to_binary`, `test_list_to_bitstring`,
`test_binary_to_list`, `test_bitstring_to_list`, `test_binary_part`,
`test_split_binary`, `test_sub_binaries` and `test_binary_eq`) pass on OTP
27/28/29 and all three AtomVM modes. Targeted CLI checks cover the new
batch/replay, `binary_slice` batch/replay and behavioral oracles.

No semantic discrepancy was found: runtime/library code, ordinary Erlang tests,
changelog and expected-failure manifest remain unchanged. Inputs, sources,
manifests, results, reproducible checks and source hashes are saved under
`.amp/in/artifacts/lynx-binary-iodata/`. Full runtime corpus, full CLI/JIT
matrices, complete older-host harness, Lean fixture laws and
embedded/32-bit/float32 targets were not run. No allocation-failure,
performance, binary-size or leak measurements are claimed.

## Binary slicing and splitting

Add 834 direct recipes for supported `binary:part/3`, `erlang:binary_part/2,3`
(body and guard paths) and `erlang:split_binary/2`, preserving all 11,873
preceding recipes and metadata; total 12,707. Fixture counts and Lean
attribution are unchanged; no APIs or Lean proofs are added.

- Check positive and negative lengths across empty, 1/4/16/63/64/65/257-byte
  subjects, crossing `SUB_BINARY_MIN` (16) and heap/refc (`REFC_BINARY_MIN`, 64)
  boundaries. Exercise exact matches, non-matching binary targets, and
  guard-rejected failures for both `binary_part/2` and `binary_part/3`.
- Check `split_binary/2` across full binaries and partial bitstrings of every
  trailing-bit width 1–7 (zero, all-ones, and asymmetric tails standalone and
  after 4/63/64/65/257 full bytes), verifying that trailing bits remain in the
  second tuple element.
- Exercise ETF-copied inputs and larger-parent extraction at bit offsets 0–7
  without using `binary:part` or `split_binary` to construct operands; retain
  inputs, offsets, arguments, captures and results across explicit GC before and
  after each call.
- Check invalid out-of-bounds, negative, non-integer, boxed (`1 bsl 64` /
  `1 bsl 60`), malformed range tuple, partial-bitstring `binary_part` and
  non-bitstring root error paths.
- `BINARY_SLICE_MODEL` extracts and slices/splits individual bit lists directly,
  calling neither `part` nor `split_binary`, and validates complete observations
  on both VMs.
- **Resolved runtime defect (kept in a separate commit/group):** initial heap
  probing exposed that `nif_binary_part_3` and `bif_erlang_binary_part_3` (also
  used by `bif_erlang_binary_part_2`) passed raw signed `len` to
  `term_sub_binary_heap_size` instead of normalized `slice.len`. When `len < 0`
  on 49–63-byte heap binaries, `(size_t)len` wrapped above `REFC_BINARY_MIN`,
  under-allocating process heap space (8 words instead of up to 11), corrupting
  adjacent terms and crashing (`SIGSEGV`) on both interpreter and AArch64 native
  builds. Added `test_negative_length_heap_binary/0` to
  `tests/erlang_tests/test_binary_part.erl` (verified to fail before the C fix
  and pass afterward across interpreter, AArch64 precompiled, on-demand JIT and
  OTP 29) alongside the `CHANGELOG.md` entry.

Interpreter and AArch64 precompiled runs each match 834/834 against OTP.
Reordered on-demand JIT replay matches 108/108, explicitly covering all six
operations, negative-length 63-byte heap binaries, guards, all eight bit offsets
and all seven partial widths, with zero failures or unverified cases. Independent
contracts pass on both VMs, complete native/JIT observations equal interpreter
results, and thirteen local wrong-result controls are rejected. Fresh
observer-only controls match all 834 OTP reference observations on OTP
27.3.4.12/28.3.1/29.1.3. Eleven ordinary binary suites (`test_binary_part`,
`test_split_binary`, `test_sub_binaries`, `test_binary_eq`, `test_binary_copy`,
`test_binary_replace`, `test_binary_split`, `test_binary_match`,
`test_binary_longest_common_prefix`, `test_binary_to_list` and
`test_bitstring_to_list`) pass on OTP 27/28/29 and all three AtomVM modes.
Targeted CLI checks cover the new batch/replay, `binary_list` batch/replay and
behavioral oracles.

Inputs, before-fix failure logs, sources, manifests, results, reproducible
checks and source hashes are saved under `.amp/in/artifacts/lynx-binary-slice/`.
Full runtime corpus, full CLI/JIT matrices, complete older-host harness, Lean
fixture laws and embedded/32-bit/float32 targets were not run. No
allocation-failure, performance, binary-size or leak measurements are claimed.

## Binary-to-list conversion

Add 680 direct recipes for supported `erlang:binary_to_list/1` and
`erlang:bitstring_to_list/1`, preserving all 11,193 preceding recipes and metadata;
total 11,873. Fixture counts and Lean attribution are unchanged; no APIs or Lean
proofs are added. Existing incidental conversion calls in helpers are not counted
as direct coverage of the conversion contract.

- Check exact unsigned byte lists, including all 256 values, empty/singleton
  and asymmetric inputs, and 63/64/65/257-byte lengths.
- Check every trailing-bit width 1–7, with zero/all-one/asymmetric tails alone
  and after full bytes. `bitstring_to_list/1` must return full-byte integers
  followed by one final partial-bitstring element; `binary_to_list/1` must
  reject the same partial input with `badarg`.
- Exercise ETF-copied inputs and larger-parent extraction at bit offsets 0–7,
  including byte-aligned slices of partial inputs. Reuse the access observer,
  retaining inputs, offsets, captures and complete results over GC before/after
  each conversion. These are construction paths, not asserted internal layouts;
  GC inside the NIF's allocation check is not instrumented or guaranteed.
- Reject atoms, numbers, tuples and proper/improper list roots rather than
  accepting an iodata input or treating it as an empty bitstring.
- `BINARY_LIST_MODEL` reads individual bits, groups them in eights and sums
  full-byte values; it constructs the final short bitstring directly. It calls
  neither API nor its inverse, and checks complete observations on both VMs,
  not just length, round trips or cross-VM equality.
- A CLI test runs all 680 plus 36 reversed/strided recipes, preserving metadata
  and independently checking both VMs.

Interpreter and AArch64 precompiled runs each match 680/680 against OTP. Reordered
on-demand JIT replay matches 54/54, explicitly covering every bit offset and tail
width, with a 240-second cold-compilation budget. There are zero failures or
unverified cases. Independent contracts pass on both VMs and complete native/JIT
observations equal interpreter results. Thirteen local wrong-result controls
reject signed/reversed bytes, nonempty empty-results, dropped/padded/wrong-width
or shifted partial tails, improper result lists, accepted invalid inputs and
wrong exception reasons. The targeted CLI checks cover the new batch/replay,
existing access and hex batches/replay, and behavioral oracles.

Fresh observer-only controls match all 680 OTP reference observations on OTP
27.3.4.12/28.3.1/29.1.3; this compiles only the unchanged observer and `call/3`,
not the unrelated OTP-29-only parts of the helper. Four existing ordinary suites
(`test_binary_to_list`, `test_bitstring_to_list`, `test_list_to_binary` and
`test_list_to_bitstring`) pass on those hosts and all three AtomVM modes.
No semantic discrepancy was found: runtime/library code, ordinary Erlang tests,
changelog, expected-failure manifest and runner configuration remain unchanged.

Inputs, sources, manifests, results, configuration, reproducible checks and source
hashes are saved under `.amp/in/artifacts/lynx-binary-list/`. Full runtime corpus,
full CLI/JIT matrices, complete older-host harness, Lean fixture laws and
embedded/32-bit/float32 targets were not run. No allocation-failure, performance,
binary-size or leak measurements are claimed.

## Hex conversion

Add 726 recipes for supported `binary:encode_hex/1,2` and `decode_hex/1`,
preserving all 10,467 preceding recipes and metadata; total 11,193. Fixture counts
and Lean attribution are unchanged. This expands the 16 existing upstream-example
recipes rather than replacing them; no APIs or Lean proofs are added.

- Check exact default/uppercase/lowercase outputs for all 256 byte values,
  leading zeroes, high bytes, asymmetric inputs and 63/64/65/257-byte lengths.
  Decode uppercase/lowercase/mixed-case strings to exact bytes independently
  of encoder round trips.
- Reject every non-hex byte in both nibble positions, moving invalid pairs
  before/between/after valid pairs. Include signed pairs, odd lengths,
  whitespace, non-ASCII bytes, nonbinary/partial-bitstring sources and invalid
  case options, including on empty inputs.
- Reuse the access observer's ETF copying, larger-parent slices and three-bit
  offset extraction, retaining complete operands, mode, captures and results
  across GC. No hex conversion prepares an observed runtime operand.
- `BINARY_HEX_MODEL` uses literal digit tables and ordinary byte-list pairs,
  not the tested APIs or integer parsing. Check complete observations against
  each VM's explicit contract, including the exception difference below.
- A CLI test runs all 726 plus 32 reversed/strided recipes and preserves metadata.

The unchanged library produced 705 matches and 21 differences: 20 signed-pair
acceptances and one pre-existing partial-bitstring exception difference. The
ordinary `test_binary` regression also failed on unchanged AtomVM. A separate
library fix replaces permissive two-character integer parsing with strict
hex-digit decoding; signs now raise `badarg` instead of becoming byte values,
including truncated negative integers. The ordinary regression retains exact
mixed-case/zero-byte controls and rejects signed pairs alone and in longer input.

Do not change AtomVM's deliberate nonbinary-input `badarg` validation to mimic
OTP's leaked `{badmatch, Input}` for partial bitstrings whose rounded byte size
is even (`<<48,5:3>>` here). This raw exception was verified on OTP
27.3.4.12/28.3.1/29.1.3 and traced to OTP's `decode_hex1` pattern match. Register
exactly this expression as `binary_hex_partial_bitstring`, keeping the true
failure in results. The corpus manifest now selects 194 cases across 15 areas;
these are inventory counts, not a new full-corpus execution claim.

After the fix, interpreter and AArch64 precompiled runs each have 725 exact OTP
matches plus the one known exception difference. Reordered JIT replay has 46
matches plus the same difference, including all 20 formerly failing signed-pair
recipes. Strict saved-result classification has zero unexpected failures/passes
and zero unverified cases. Independent contracts pass on both VMs; complete
native/JIT observations equal interpreter results. Thirteen local wrong-result
controls reject wrong case/defaults, dropped zeroes, swapped nibbles, wrong output
types, invalid-pair acceptance, truncation, skipped empty-input mode validation
and copying OTP's exception into AtomVM. Targeted CLI checks cover the new batch,
old access recipes, upstream stdlib examples, behavioral oracles and strict
expected-failure classification.

Fresh observer-only host controls match all 726 OTP reference observations on
OTP 27/28/29. Ordinary `test_binary` and ten runtime binary suites pass on those
hosts and all three AtomVM modes. The older-host controls compile only the
unchanged observer and `call/3`, avoiding unrelated OTP-29-only helper guards;
this is not full older-OTP harness validation. Host build targets succeed; the
native build's unrelated all-target rule references a missing RISC-V archive,
so validation uses the required `AtomVM`, `atomvmlib` and `etest` targets.

Evidence, manifests, results, configuration, source hashes and reproducible
checks are under `.amp/in/artifacts/lynx-hex/`. Full runtime corpus, full CLI/JIT
matrices, fixture Lean laws and embedded/32-bit/float32 targets were not run.
No allocation-failure, performance, binary-size or leak measurements are claimed.

## Binary access and copying

Add 368 recipes for supported `binary:at/2`, `first/1`, `last/1` and `copy/1,2`,
preserving all 10,099 preceding recipes and metadata; total 10,467. Fixture counts
and Lean attribution are unchanged; no APIs or Lean proofs are added.

- Check exact unsigned high bytes, empty/singleton/asymmetric subjects,
  63/64/65/257-byte lengths and negative/start/middle/end/out-of-range indices.
- Check default copying and counts 0/1/2/3, invalid argument types, partial
  bitstrings even at zero count, negative/boxed/unsigned/over-word counts and
  guaranteed oversized-result boundaries. Large count literals require a
  matching-word-size OTP oracle; this batch targets 64-bit hosts.
- Exercise ETF-copied operands, larger-parent slices and byte-sized segments
  extracted at a three-bit offset. Retain complete inputs, arguments, captures
  and results across GC before/after each API. Construct operands without
  calling `binary:copy`; construction paths do not assert internal layouts.
- Independently index/repeat ordinary byte lists with `BINARY_ACCESS_MODEL`,
  checking complete observations on both VMs rather than only equality.
- A CLI integration test runs all 368 plus 34 reversed/strided recipes, checking
  independent contracts and saved metadata.

Empty-source probes on unchanged AtomVM exhausted a two-second CPU budget for
negative and large immediate counts; valid boxed/unsigned counts incorrectly
raised `badarg`. The expanded ordinary `test_binary_copy` passed OTP 29 but
failed unchanged AtomVM on a boxed-count control. Baseline probes used empty
sources only: no dangerous nonempty huge allocations were attempted. A separate
runtime fix validates unsigned machine-word counts before the empty-source fast
path and checks OTP's bit-size limit before multiplication/allocation. Following
OTP 29.1.3's `do_binary_copy`, a nonempty source rejects counts greater than or
**equal to** the unsigned-word maximum divided by source bit size, raising
`system_limit`. Ordinary Erlang regressions derive these boundaries from word
size and cover exact repetition bytes, invalid counts and partial sources.

The fixed batch matches 368/368 in interpreter and AArch64 precompiled modes;
45 reversed/strided JIT recipes, including all large-count controls, match with
a 240-second cold-compilation budget. No failures or unverified references.
Independent contracts pass on both VMs and complete native/JIT observations
equal interpreter results. Both targeted CLI tests pass. Twelve local wrong-result
controls reject signed high bytes, wrong indices/endpoints, ignored zero counts,
reversed copies, invalid-source/count acceptance, rejected valid boxed counts,
wrong oversized-result exceptions and wrapped result sizes. The four isolated
empty-source count probes finish with correct outcomes after the fix.

Ten ordinary binary suites pass on OTP 27.3.4.12/28.3.1/29.1.3 and interpreter,
AArch64 precompiled and on-demand JIT. This is not full older-OTP harness or
full JIT differential validation. Source, inputs, results, build/configuration
metadata and reproducible checks are saved under
`.amp/in/artifacts/lynx-binary-access/`. Full runtime corpus, full CLI/JIT
matrices, fixture Lean laws and embedded/32-bit/float32 targets were not run.
No allocation-failure, performance, binary-size or leak measurements are claimed.

## Longest common binary prefix

Add 252 recipes for supported `binary:longest_common_prefix/1`, preserving all
9,847 preceding recipes and metadata exactly; total 10,099. The batch adds
204 valid-input observations and 48 invalid-input observations. Fixture counts
and Lean attribution are unchanged; no APIs or Lean proofs are added.

- Check singleton/identical inputs, shorter binaries in either order, and
  three-input controls where a later input shortens the first pair's prefix.
  Reordered input controls must preserve the prefix length.
- Check full lengths 0/1/63/64/65/257, and mismatches at the first/middle/final
  byte, including 62/63/64/256-byte prefixes. Use asymmetric byte contents,
  embedded zero/high bytes, runtime-created larger-parent slices and byte-sized
  segments extracted at a three-bit offset.
- Preserve malformed entries and improper tails while preparing construction
  paths. Check invalid first entries, entries after an empty binary, entries
  after two binaries already disagree, and after a still-nonempty prefix. Check
  invalid roots, empty input lists and partial bitstrings separately from valid
  empty binaries.
- Copy inputs through ETF and retain complete operands/captures across explicit
  GC before and after the API call. Construction paths do not assert internal
  binary representations or force GC inside the allocation-free NIF.
- `BINARY_PREFIX_MODEL` validates every entry and the proper-list tail before
  counting equal byte columns across all inputs. It does not reproduce the
  NIF's pairwise scan against the first binary or call the API under test.
  Require complete decoded observations from both VMs to satisfy this contract.
- A targeted CLI test runs the full batch and 36 reversed/strided recipes,
  preserving saved metadata and checking both VMs against independent results.

The full batch matches 252/252 in interpreter and AArch64 precompiled execution;
reversed/strided on-demand JIT replay matches 36/36 with a 240-second
cold-compilation budget. Both VMs satisfy independent contracts, and complete
native/JIT observations equal their interpreter counterparts. Both targeted CLI
tests pass. Ten local wrong-result controls reject singleton-zero/off-by-one
results, ignored later operands, confusion between empty binaries and empty
input lists, invalid data hidden behind zero prefixes, partial-bitstring
acceptance and wrong exception reasons. No semantic discrepancy was observed:
runtime/library code, ordinary Erlang regressions, changelog, runner configuration
and allowlists remain unchanged.

Fresh observer-only host controls match all 252 complete observations on OTP
27.3.4.12, 28.3.1 and 29.1.3. These compile the unchanged observer and `call/3`
extracted from the helper, avoiding its unrelated OTP-29-only guards; they do
not establish full older-OTP harness compatibility.

Saved inputs, manifests, results and reproducible checks are under
`.amp/in/artifacts/lynx-binary-prefix/`. Full runtime corpus, full CLI/JIT
matrices, full older-OTP harness, fixture Lean laws, ordinary Erlang suites and
embedded/32-bit/float32 targets are outside this batch. No performance,
binary-size, leak or allocation-failure measurements are claimed.

## Binary replacement

Add 157 recipes for supported `binary:replace/3,4`: binary subjects, single
binary patterns, binary replacements and empty/`global` option lists. Preserve
all 9,690 preceding recipes and metadata exactly; total 9,847. Fixture counts
and Lean attribution are unchanged; no APIs or Lean proofs are added.

- Check first-only versus global replacement, adjacent/non-overlapping matches,
  overlap candidates, preserved suffixes and replacements containing the pattern
  without rescanning the output.
- Check growing/shrinking/deleting matches, empty subjects, missing and oversized
  patterns, embedded zero/high bytes and a 70-byte replacement.
- Copy inputs through ETF, create slices of larger runtime binaries and extract
  byte-sized segments at a three-bit offset. Check subject sizes 0/1/63/64/65/257
  and retain exact results, operands and captures across explicit GC. These are
  construction paths, not assertions about internal binary representation.
- Check invalid operands, partial bitstrings, empty patterns and malformed
  options, including improper tails after accepted prefixes and repeated global
  options. Pattern lists, compiled patterns, scoped replacement and
  `insert_replaced` remain outside the supported contract.
- `BINARY_REPLACE_MODEL` uses a byte-list transducer, not binary search/split/
  replacement or a round trip. It independently checks complete observations on
  both VMs. A CLI test executes all cases plus 23 reversed/strided recipes and
  checks saved metadata and the independent contract.

The initial interpreter run matches 145/157, with twelve semantic mismatches:

- Ten empty-subject cases: OTP returns the empty binary with a nonempty pattern,
  while AtomVM incorrectly raises `badarg`.
- Two nonempty-subject cases with `[global|tail]` or `[global,global|tail]`:
  OTP raises `badarg`, while AtomVM ignores the improper tail and replaces bytes.
  Empty-subject malformed-option controls already raise correctly.

A separate runtime/regression group changes only `nif_binary_replace` and the
ordinary `test_binary_replace` suite, with its changelog entry. Validate the
option tail before early return, and reject only the empty pattern, not an empty
subject. Invalid operands/options remain errors even on empty/no-match inputs.
The extended ordinary suite passes OTP and fails unchanged AtomVM; the initial
differential matrix separately records both failure groups.

After rebuilding all three runtimes, all 157 cases match OTP in interpreter and
AArch64 precompiled modes. Reversed/strided on-demand JIT replay includes every
previously failing recipe and matches 33/33, with a 240-second cold-compilation
budget. Both VMs satisfy independent contracts; complete native/JIT observations
match their interpreter counterparts. Nine local wrong-result controls reject
empty-subject errors, ignored global options, global-by-default, overlapping
replacement, rescanned output, dropped suffixes, accepted improper options,
accepted empty patterns and wrong exception reasons. Both targeted CLI tests pass.
No expected-failure entries are added.

Seven ordinary binary suites (replace, match, split, part, copy, longest common
prefix and equality) pass on OTP 27.3.4.12/28.3.1/29.1.3 and interpreter,
AArch64 precompiled and on-demand JIT execution. These older-host runs validate
ordinary suites, not the full Lynx harness. Equality retains its registered
expected result of 1; the other six suites return 0.

Evidence, saved inputs/results and reproducible checks are under
`.amp/in/artifacts/lynx-binary-replace/`. This batch does not rerun the full
runtime corpus, full CLI suite, full JIT matrix, full older-OTP harness,
fixture Lean laws, whole ordinary suite or embedded/32-bit/float32 targets.
No binary-size, performance or allocation-failure measurements are claimed.

## Proplist normalization and map conversion

Add 246 supported-API recipes: 22 for `property/1`, 77 for `property/2`,
30 each for `compact/1` and `unfold/1`, 66 for `to_map/1` and 21 for
`from_map/1`. Preserve all 9,444 preceding recipes and metadata exactly;
total 9,690. Fixture counts and Lean attribution are unchanged; no APIs or
Lean proofs are added.

- Collapse only `{Atom,true}` to atom shorthand. Preserve non-atom keys,
  false/non-boolean values, malformed tuples and unrelated nested terms.
- Check first-entry map precedence, malformed tuples suppressing later values,
  and absent bindings versus bindings whose value is `undefined`.
- Preserve exact numeric/compound keys, signed zeros, partial bitstrings,
  bignums and boxed payloads. Maps at 31/32/33/65 entries and longer list
  controls use distinct values so misplaced values cannot hide behind equal payloads.
- Check complete exception class/reason for non-list and improper generators,
  including `compact/1`'s `{bad_generator,Tail}` versus `function_clause`, and
  `from_map/1`'s `{badmap,Input}`.
- Copy arguments through ETF and retain raw results, inputs and captures across
  explicit GC. Observe `from_map/1` as entry count plus exact bindings, without
  assuming list order or using `proplists:to_map/1` as a round-trip oracle.
- `PROPLIST_TRANSFORM_MODEL` calculates shallow normalization and scans map
  entries left-to-right, independently of the library's right-fold conversion.
  It checks both VMs against complete expected observations.

Final asymmetric-matrix runs match 246/246 in interpreter and AArch64
precompiled execution, and the reversed/strided on-demand JIT subset matches
36/36. Both VMs satisfy independent contracts, complete decoded observations
agree across modes, and both targeted CLI tests pass. Twelve local wrong-result
controls reject incorrect normalization, first-entry precedence, suppression,
exact keys, entry counts/bindings and exception reasons. No discrepancy was
observed: runtime/library code, ordinary Erlang tests, changelog and allowlists
remain unchanged.

Initial JIT replay records four 120-second timeouts while compiling the shared
observer module, not semantic mismatches. Successful initial observer compilations
include 55–75-second timings. The final 36-case replay passes with a 240-second
per-case budget. Original timeout logs remain saved.

Fresh observer-only host controls match all 246 observations on OTP 27.3.4.12,
28.3.1 and 29.1.3. They compile the unchanged observer and `call/3` extracted
from the helper, avoiding its unrelated OTP-29-only guards. These controls do
not establish full older-OTP harness compatibility.

Saved inputs, results and reproducible checks are under
`.amp/in/artifacts/lynx-proplist-transform/`; `final/` holds the asymmetric
matrix. This batch does not rerun the full runtime corpus, full CLI suite,
full JIT matrix, full older-OTP harness, fixture Lean laws, ordinary Erlang
suites or embedded/32-bit/float32 targets.

## Proplist lookup and precedence

Add 352 supported-API recipes, 44 each for `get_value/2`, `get_value/3`,
`get_bool/2`, `is_defined/2`, `lookup/2`, `get_all_values/2`, `lookup_all/2`
and `delete/2`. Preserve all 9,092 preceding recipes and metadata exactly;
total 9,444. Fixture counts and Lean attribution are unchanged; no APIs or
Lean proofs are added.

- Check atom shorthand, first-entry precedence, duplicate keys, missing
  defaults and malformed tuple entries, including zero/one/three-element tuples.
- Distinguish exact integer/float, signed-zero and compound keys. Observe
  complete entries versus values, all-match deletion and ordered collections.
- Separate queries that stop at a match from collectors that reach invalid
  tails, with improper and non-list inputs and first/middle/last/missing controls.
- Copy input/key/default through ETF; preserve full outcomes, inputs and
  captures across explicit GC, including nested maps and partial bitstrings.
- `PROPLIST_LOOKUP_MODEL` classifies the finite prefix and unmatched tail, then
  calculates expectations without calling `proplists`. Do not infer shared
  semantics between first-entry queries and full-traversal consumers.

**Resolved difference:** unchanged AtomVM skips matching tuples whose arity
is not two in `get_value/2,3`, instead of returning the default as OTP does.
Eight recipes expose a later value being returned or a previously unvisited
improper tail raising `function_clause`. Add a matching-tuple guard after the
normal pair/atom clauses; do not prevalidate the list or alter other consumers.
The separate ordinary regression fails against the unchanged library and passes
with the correction, including nonmatching numeric-key and empty-tuple controls.
Library fix, ordinary regression and changelog are separate from Lynx files.

Interpreter and AArch64 precompiled runs match 352/352 against OTP 29.1.3;
the reversed/strided on-demand JIT subset matches 51/51. Both VMs satisfy
independent contracts in all three saved runs, and complete observations agree
across modes. Both targeted CLI tests pass, including full coverage and
metadata-preserving replay. Ten local wrong-result controls are rejected,
including last-match precedence, numeric key equality, lost defaults,
incomplete deletion, reversed collectors and incorrect tail validation.

The complete ordinary proplists/lists/maps/sets/queue suites pass in interpreter,
precompiled and on-demand JIT modes, and fresh host builds pass OTP 27.3.4.12,
28.3.1 and 29.1.3. These older-host runs cover the ordinary suites, not the full
differential harness.

Saved inputs, before/after results and reproducible checks are under
`.amp/in/artifacts/lynx-proplist-lookup/`. This batch does not rerun the full
runtime corpus, full CLI suite, full differential JIT matrix, fixture Lean
laws or embedded/32-bit/float32 targets.

## List construction without flattening

Add 209 supported-API recipes: 110 for `lists:join/2` and 99 for
`lists:duplicate/2`. Preserve all 8,883 preceding recipes and metadata exactly;
total 9,092. Fixture counts and Lean attribution are unchanged; no APIs or
Lean proofs are added.

- Insert separators as individual terms, including empty, proper and improper
  lists. Distinguish nested elements from malformed outer lists, and check
  empty/singleton inputs and 17/33/65-element traversal controls.
- Repeat whole elements at counts 0/1/2/17/33/65, including lists, tuples,
  exact numeric map keys, bignums, partial bitstrings and signed zeros.
- Reject negative counts (including a negative bignum), floats and non-numeric
  terms with exact exception class/reason. Successful counts are bounded;
  this does not test allocation exhaustion or oversized positive counts.
- Copy both arguments through ETF and retain complete arguments, captures and
  outcomes across explicit GC. Strict comparisons retain term identity.
- `LIST_CONSTRUCTION_MODEL` calculates expected results by indexed placement,
  without calling either tested API. Both VMs must satisfy the contracts.
  Seven local wrong-result controls are rejected: flattened separator/element,
  zero-count off-by-one, numeric coercion, lost zero sign, wrong exception and
  accepted improper outer list.

Interpreter and AArch64 precompiled runs match 209/209 against OTP 29.1.3;
the reversed/strided on-demand JIT subset matches 30/30. Complete decoded
observations agree across these modes, and both targeted CLI tests pass,
including full construction and metadata-preserving replay. No discrepancy
was found: runtime/library code, ordinary Erlang tests, changelog and allowlists
remain unchanged.

Saved inputs, results and reproducible independent checks are under
`.amp/in/artifacts/lynx-list-construction/`. This batch does not rerun the full
runtime corpus, full CLI suite, full JIT matrix, older-OTP controls, fixture
Lean laws or embedded/32-bit/float32 targets.

## List-selection identity and traversal

Add 209 supported-API recipes: 66 each for `lists:min/1` and `lists:max/1`,
and 77 for `lists:delete/2`. Preserve all 8,674 preceding recipes and metadata
exactly; total 8,883. Fixture counts and Lean attribution are unchanged; no
APIs or Lean proofs are added.

- Extrema retain the first ordering-equivalent representative. Check reversed
  and repeated integer/float ties, signed zeros, positive/negative 2^53
  boundaries, nested list/tuple/map values, exact map keys and bitstrings.
- Delete only the first exact match, preserving later duplicates and ignoring
  numerically equal but non-exact terms. Distinguish reached invalid tails
  from untouched suffixes, including atom/integer/map/bitstring tails.
- Reject malformed outer lists while accepting improper lists as elements.
  Add 65-item controls with first/middle/last/missing deletion targets.
- Copy input/key terms through ETF and retain full inputs, captures and results
  across explicit GC. Compare complete terms with strict equality, not just
  numeric equality or successful execution.
- `LIST_SELECTION_MODEL` chooses the first globally extremal candidate rather
  than reproducing the running-accumulator implementation. Its deletion model
  checks exact first matches without prevalidating an untouched suffix. Neither
  expectation calculation calls the tested list APIs.

Interpreter and AArch64 precompiled runs match 209/209; the reordered on-demand
JIT subset matches 30/30. Both VMs satisfy the independent contracts in all three
saved runs, and complete decoded observations agree across modes. Both targeted
CLI tests pass, including full selection and the metadata-preserving replay.
No defect is observed: runtime/library code, ordinary Erlang tests, changelog
and allowlists remain unchanged.

Fresh OTP 27.3.4.12, 28.3.1 and 29.1.3 host runs also match all 209 observations.
These controls compile the unchanged selection observer extracted from the
helper: the full helper contains an unrelated OTP-29-only `is_integer/3` guard
and cannot compile on OTP 27. This is not a full older-OTP harness validation.

Saved inputs, result logs and reproducible checks are under
`.amp/in/artifacts/lynx-list-selection/`. This batch does not rerun the full
runtime corpus, full CLI suite, full JIT matrix, fixture Lean laws or embedded
targets.

## Deep-list flattening and concatenation

Add 185 supported-API recipes: 37 for `flatten/1`, 22 for `append/1`, and 42
each for `append/2`, source `++` and dynamically applied `erlang:'++'/2`.
Preserve all 8,489 preceding recipes and metadata exactly; total 8,674. Fixture
counts and Lean attribution are unchanged; no APIs or Lean proofs are added.

- Contrast recursive flattening with one-level append. Check empty sublists,
  asymmetric nesting, atomic tuple/map/bitstring leaves and numeric identity.
- Check arbitrary final tails, singleton `append/1`, malformed intermediate
  lists, improper outer lists and the precedence of outer-list failure over
  an invalid prefix. Concatenation accepts any tail but requires a proper prefix.
- Copy inputs out of the literal pool through ETF, retain inputs/captures and
  complete outcomes across explicit GC, and exercise flatten's copying and
  reusable-tail paths. Nesting depths 7/8/9/17/33 retain pending siblings;
  wider 8/33/65-element structures distinguish traversal order and empty nodes.
- `DEEP_LIST_MODEL` computes results structurally without the APIs under test
  or `++`. It checks both OTP and AtomVM's complete observations, including
  exception class/reason and GC-retained data. CLI coverage includes full
  selection and a 27-case reversed/strided replay with unchanged metadata.

The initial interpreter run matched 169/185: all 16 differences were flatten
`badarg` versus OTP `function_clause` on non-list roots or improper tails.
All concatenation paths matched. A separate correction in `nifs.c`, ordinary
Erlang regressions and changelog fixes those exceptions and traversal-stack
cleanup; the differential cases and expectations are unchanged by the fix.
Both ordinary flatten regression groups fail against the saved old runtime
and pass afterward. No allowlist changes are used.

A 100-iteration successful/failing depth-33 stress run confirms the cleanup:
macOS `leaks` reports 200 flatten allocations (64,000 bytes) before and none
after. Total reported leaks drop from 258/65,856 bytes to 58/1,856 bytes; the
unrelated existing allocations remain and are not fixed or called leak-free.

Interpreter and AArch64 precompiled runs each match 185/185; the reordered
on-demand JIT subset matches 27/27. Independent contracts pass on both VMs in
all three runs, and complete decoded observations agree across modes. Both
targeted CLI tests pass, including the structural model and interpreter replay.
Ordinary lists/queue/sets/maps suites pass on all three AtomVM modes and OTP
27.3.4.12, 28.3.1 and 29.1.3. Saved inputs, logs, before-runtime snapshot and
reproducers are under `.amp/in/artifacts/lynx-deep-lists/`. Full runtime corpus,
full CLI suite, full JIT matrix and embedded targets are not rerun for this batch;
allocation-failure cleanup is inspected but not fault-injected.

## Positional-list traversal and validation

Add 284 supported-API recipes for `nth/2`, `nthtail/2`, `sublist/2,3` and
`split/2`. Preserve all 8,205 prior recipes and metadata exactly; total 8,489.
Fixture counts and Lean attribution are unchanged. These are OTP differential
extensions, not new Lean proofs or APIs. No observer helper or runner change
is needed: direct `differential:call/3` calls preserve results and exception
class/reason without comparing stack traces.

- Distinguish zero/one-based positions, end/overrun behavior, truncation,
  invalid numeric/non-numeric indices and large integers.
- Use empty, proper, improper and non-list inputs. Stop before, at and beyond
  improper tails, including zero-length slices with invalid initial inputs.
- Retain boxed maps, bitstrings and large integers in results, and exercise
  33-item boundaries. A CLI integration test checks the full matrix, reordered
  replay metadata and 15 handwritten boundary expectations independent of OTP.

The original interpreter run matched 278/284, exposing six differences in
three library behaviors: `nth/2` atom-index errors, `nthtail/2` returning a
reached improper suffix, and `sublist/2` validating non-list input for a
zero-length request. Keep the correction in `lists.erl`, its ordinary Erlang
regressions and changelog independently selectable from this coverage/notes.
Each ordinary regression group fails against the saved before-library and
passes against the rebuilt library. No allowlist or oracle changes are used.

Final interpreter and AArch64 precompiled runs each match 284/284. The reordered
on-demand JIT subset matches 22/22; complete decoded observations agree across
all selected modes. Both targeted CLI tests pass, including the 22-case reordered
interpreter replay and handwritten boundaries. Ordinary
lists/queue/sets/maps suites pass on all three AtomVM modes and installed OTP
27.3.4.12, 28.3.1 and 29.1.3. Evidence and saved inputs are under
`.amp/in/artifacts/lynx-list-positions/`. The full runtime corpus, full CLI suite,
full JIT matrix and embedded targets are not rerun for this batch.

## Keyed-list equality, mutation and sorting

Add 344 supported-API recipes (2026-11-27): 56 each for `keydelete/3` and
`keytake/3`, 62 each for `keyreplace/4` and `keystore/4`, and 54 each for
`keysort/2` and `ukeysort/2`. All 7,861 preceding recipes and their metadata are
preserved exactly; total 8,205. Fixture coverage remains 2,351 calls across 203
exports, and attribution remains 377/478 Lean sites across 28 files. No APIs,
Lean proofs or library/runtime fixes are added in this batch.

- Check first numeric-equivalent matches, replace-or-append behavior, returned
  removed elements, stable key sorting and first unique representatives, with
  distinct payloads at key positions one and two. Exercise integer/float ties,
  the 2^53 precision boundary, absent keys, duplicates and mixed term keys.
- Distinguish an earlier float match from a later exact integer, and a lone
  equivalent float from no match at all. The latter catches an incorrect append
  by keystore, not just replacement of the wrong duplicate.
- Cover non-tuples/short tuples, invalid positions and replacement terms,
  empty/singleton controls, non-list inputs and both reached and unreached
  improper tails. Mutations skip malformed elements; sorts return singleton
  inputs untouched but require valid key elements when comparing a pair.
- Retain original inputs, keys, replacement values and boxed captures across
  explicit GC before and after calls. Compare exception class/reason, not
  stacktraces or implementation-dependent comparison sequences.
- Independently derive mutation results from the first matching input index,
  and sorting results from pairwise stable ranks and first equivalence-class
  representatives. Do not use the keyed-list APIs under test as the model.
  Check saved OTP references and AtomVM observations separately, including
  metadata-preserving reordered replay; known differences must remain failures.

### Initial validation and identified compatibility groups

OTP 27.3.4.12, 28.3.1 and 29.1.3 each satisfy **344/344** independent contracts.
A positive control passes. Seven identically corrupted oracle/actual report
pairs are rejected: exact rather than numeric matching, lost improper suffix,
unstable key ordering, last unique representative, eager singleton key
validation, accepted invalid position and lost captured terms.

Initial interpreter and AArch64 precompiled runs each matched **260/344**, with
**84 differences**, zero unverified references and no timeouts. Their 50-case
reordered replays each match **39/50**, with 11 differences. Per API, keydelete,
keyreplace and keystore each differ on 12 recipes, keysort on 20 and ukeysort
on 28; **all 56 keytake cases match**. These are finite case counts, not 84
independent bugs. These exposed four compatibility groups, subsequently fixed
with separate library changes and ordinary Erlang regressions below:

- First-match numeric equality in keydelete/keyreplace/keystore: 12 mismatches,
  including untouched lone equivalent keys and keystore's incorrect append.
- First numeric-equivalent ukeysort representative: eight mismatches, including
  the large-integer/float boundary. Stable keysort positive controls pass.
- Positive-integer index guards in those five APIs: 40 mismatches. OTP rejects
  zero/negative/float/atom positions even on an empty or singleton list.
  Keytake already validates the position correctly.
- Keysort/ukeysort entry exception reasons: 24 mismatches. OTP returns
  `error:{case_clause, Input}` for non-list and one-cell improper inputs, but
  AtomVM reports function_clause. Longer valid-key improper-tail controls and
  malformed-element skips/short-circuit behavior match.

The on-demand AArch64 JIT runs the same 50-case reordered subset, matching
**39/50** with the same 11 differences and no timeouts/unverified references.
All complete decoded observations agree across interpreter/precompiled full
runs and all three reordered subsets. Both VMs are checked independently in
all five runs. This validates only 50 of the 344 cases under on-demand JIT,
not the full batch or every native execution path.

The full 52-method CLI run on the 336-case development matrix passes all 48
previously enabled methods and skips the three optional checks. Only the new
keyed-list method fails, in both full/replay subtests. Re-running the changed
checks on the final 344-case matrix preserves the two genuine contract
failures (344 full, 50 replay); the updated baseline-count/independent-oracle
check passes. Python 3.9 grammar, Erlang formatting and whitespace checks pass.

The initial integration assertion remained red on the genuine differences;
no expected-failure entries, skip markers or accepted wrong counts were added.
Library/runtime files, ordinary Erlang tests and changelog were unchanged in
the coverage batch. Initial evidence, toolchain/build hashes, inputs, raw
observations, cross-version/model controls and readable differences are under
`.amp/in/artifacts/lynx-list-keys/final/`. The earlier 336-case development runs
are retained one directory above; the final batch adds eight lone-float-key
controls. This batch does not rerun the entire 8,205-recipe runtime corpus.

### Separate library correction and ordinary Erlang regressions

`lists:keydelete/3` and the shared keyreplace/keystore worker now compare keys
using numeric equality, preserving the first match and untouched improper
suffixes while skipping malformed elements through guard failure. Public
keydelete/keyreplace/keystore/keysort/ukeysort validate positive integer
positions before traversing inputs. Keytake retains its correct implementation.
Keysort explicitly distinguishes empty/singleton/pair inputs, preserving lazy
singleton handling and OTP's case_clause entry errors. Ukeysort removes
numeric-equivalent duplicates while retaining the first sorted representative.
No general sort algorithm or global numeric-comparison behavior is changed.

The correction is confined to `libs/estdlib/src/lists.erl`, its changelog entry
and ordinary `tests/libs/estdlib/test_lists.erl` regressions, independent of
Lynx helpers. Existing public test exports stay unchanged. Extended per-API
cases cover earlier equivalent keys, lone floats, nested tuple keys, second
positions, exact large-number distinctions and improper-tail boundaries.
Shared validation groups cover invalid positions/replacements, lazy singleton
controls and malformed sort entry errors. Each affected regression group fails
against the saved old library and passes with the fix; existing keysort and
keytake controls pass before and after. Only the disposable isolation build
uses `+export_all`, not the maintained tests.

All ordinary lists/queue/sets/maps suites pass on OTP 27.3.4.12, 28.3.1, 29.1.3
and interpreter, precompiled and on-demand JIT AtomVM. The precompiled run
uses native ordinary modules and matching eavmlib first; JIT logs confirm
compilation of the raw ordinary modules. A pack containing the fixed local
lists source and ordinary tests compiled by OTP 27 also passes on AtomVM.
Independent 10,000-item keyed sort/unique/mutation controls pass on all six
runtimes, checking boxed inputs/results across GC and deriving expected stable
ranks and first matches from original indices rather than the APIs under test.

Fresh interpreter and AArch64 precompiled runs each match **344/344** and
**50/50** reordered cases. The same on-demand AArch64 JIT subset passes
**50/50**, with zero failures/timeouts/unverified references. Both VMs satisfy
the unchanged independent contracts in all five runs, and complete decoded
observations agree across modes/replay reindexing. SHA-256 checks confirm
recipes, model expectations, helper and runner are byte-for-byte unchanged
from the failing coverage batch. Full JIT batch, complete runtime corpus and
embedded/32-bit/float32 targets were not run for this correction.

The full 52-method CLI suite now passes: **49 enabled methods passed, three
optional configurations skipped**, including both full/replay keyed-list
subtests. Erlang formatting, Python 3.9 grammar and whitespace checks pass.
No expectations, cases or allowlists were changed to make the suite green.

Fix evidence is under `.amp/in/artifacts/keyed-list-fix/`, including before/
after isolation logs, ordinary/cross-OTP/large-control logs, source hashes,
reproducer sources and a patch of only library/ordinary-regression/changelog
changes. Generated scratch packs/beams are disposable, not maintained tests.

## Comparator-driven sort, usort and merge

Add 148 supported-API observations (2026-11-27): 46 each for `lists:sort/2`
and `usort/2`, and 56 for `merge/3`. Preserve all 7,713 preceding recipes and
metadata; total 7,861, attribution unchanged at 377/478 Lean sites and fixture
coverage unchanged at 2,351 calls across 203 exports. No APIs or proofs are added.

- Use valid non-strict key comparators (`=<` and `>=`), ascending/descending
  inputs, distinguishable equal-key elements, numeric-equivalent integer/float
  keys, empty/singleton controls and a 17-element mixed ordering case.
- Require stable sort results, the first representative of each comparator
  equivalence group for usort, and left-before-right merge ties. Merge inputs
  are already sorted in the chosen direction; their concatenation defines the
  independent equal-key precedence.
- Exercise invalid callbacks and arities 1/3, constant error/throw/exit reasons,
  non-boolean atom/tuple returns, and controls where no comparison is needed.
  OTP validates callbacks differently: sort invokes an invalid callback when
  at least two elements are present (`badfun`/`badarity`), whereas usort and
  merge reject invalid callbacks at entry (`function_clause`).
- Explicit GC inside comparisons and before reporting retains original inputs,
  boxed tagged results and captured maps, integers and partial bitstrings.
  Report a capture/argument-retention invariant, not pair visits or call counts.
  Normalize badarity's embedded function to an identity check, omitting its
  algorithm-dependent compared arguments. No ordering/count of comparator
  invocations or argument-dependent exception prefix is asserted.

`list_ordering.erl` is a standalone observer. The independent Elixir contract
ranks each item by strictly preceding keys plus earlier equivalent keys; it
calls no sorting/merging API and does not reproduce either implementation's
algorithm. Both VMs must satisfy it, so agreement on a wrong stable result is
not accepted. A positive control passes; six identically wrong report pairs
are rejected (unstable sort, last usort representative, right-first merge tie,
lost capture, failed GC invariant and eager sort callback validation).
OTP 27.3.4.12, 28.3.1 and 29.1.3 each satisfy all 148 contracts with the observer
freshly compiled under that version. Cross-OTP saved results are checked using
Elixir 29; these are OTP-only controls, not additional AtomVM runs.

Initial interpreter, AArch64 precompiled and on-demand JIT runs each matched
**113/148**, with **35 mismatches**, zero timeouts and zero unverified references.
Sort matched 34/46, usort 46/46 and merge 33/56. The differences were six unstable
sort cases, eight right-before-left merge tie cases, six sort callback-validation
cases and fifteen merge callback-validation cases. Reordered 30-case replays
matched **22/30**, exposing the same eight selected differences after module
reindexing. Complete observations agreed across modes; independent checks
validated OTP and identified the same AtomVM failures in all six runs.
The initial 51-method CLI suite passed all 47 preceding enabled methods, skipped
three optional configurations and failed only the new comparator method's two
subtests. That process used an exploratory 17-case subset; the final richer
30-case replay was separately rerun. No expected-failure entry was added.

### Separate library correction and ordinary Erlang regressions

The shared merge worker now tests the left item against the right and selects
it when the non-strict comparator returns true. Natural sorting/merging and
keysort use non-strict comparisons too, preserving stable default and key
ordering. Duplicate removal retains the first equivalent item, preserving
custom usort results after the sorting correction and correcting default
usort's integer/float representative choice.

Sort validates callbacks on empty/singleton lists but invokes them when a pair
is present. Merge validates arity two at entry even when one list is empty.
Usort retains its entry validation independently of sort. Callback exceptions
and non-boolean returns are not caught or translated by the library.

The correction lives in `libs/estdlib/src/lists.erl`, with changelog and ordinary
regressions in `tests/libs/estdlib/test_lists.erl`, separate from this Lynx batch.
Sort, merge, validation and default-usort regression groups each fail against
the unchanged library and pass afterward; existing keysort/ukeysort controls
remain green. The maintained tests use only public APIs and keep their exports
unchanged; isolation uses `+export_all` only in a disposable validation build.
Full ordinary lists, queue, sets and maps suites pass on OTP 27.3.4.12, 28.3.1,
29.1.3 and all three AtomVM modes. An OTP-27-compiled local library/test pack
also passes on AtomVM. Independent 10,000-item stable sort/usort/merge controls
pass on all three OTP hosts and AtomVM modes, with periodic GC and retained
inputs/captures.

Fresh interpreter, AArch64 precompiled and on-demand JIT runs each match
**148/148**, with **30/30** reordered replays, zero failures and zero unverified
references. Both VMs satisfy the independent contracts in all six runs, with
identical complete observations across modes and replay reindexing. The full
51-method CLI suite now passes: **48 passed, three optional skips**, including
the comparator integration test's full and final 30-case replay. Erlang
formatting, Python 3.9 grammar and whitespace checks pass. No cases, model
expectations or expected-failure entries were changed to obtain these results.

Evidence: `.amp/in/artifacts/lynx-list-ordering/` retains the initial failures,
inputs/replay, independent/cross-OTP/mutation controls, inventory and recipe
preservation hashes. `.amp/in/artifacts/list-ordering-fix/` records the separate
library correction, ordinary before/after controls and fresh validation.
Cold JIT uses `--timeout 120` for per-process compilation of the existing large
helper. The complete runtime corpus, fresh Lean proofs, embedded/32-bit/float32
targets, arbitrary comparator laws, malformed lists, strict/non-transitive
comparators and exhaustive input combinations are outside this batch.

## Paired-list zipwith callbacks

Add 95 supported-API observations (2026-11-27) for `lists:zipwith/3`, preserving
all 7,618 preceding recipes and metadata. Total 7,713; attribution remains
377/478 Lean sites, fixtures 2,351 calls across 203 exports. This is additional
supported-behavior coverage, not a new API or fresh Lean proof batch. Existing
zip/unzip round-trip observations remain unchanged.

- Observe exact paired arguments, invocation indices, output order, original
  inputs and captures. Asymmetric lists contain duplicates, integer/float
  distinctions, maps, boxed integers and partial bitstrings, with empty/singleton
  and 17-element controls. A pair-returning callback checks zip-style output.
- Check arbitrary callback returns, including non-list atoms, maps, tuples,
  partial bitstrings and improper lists. A later callback exception must not be
  preempted by treating earlier output as a flatmap chunk requiring validation.
- Check errors/throws/exits on the first, second and final pair, unused failing
  callbacks, unequal lengths in both directions and malformed inputs on either
  or both sides. Preserve exact matching-prefix effects before failure; a callback
  exception can win before a later length/tail failure, or remain unreached.
- Check invalid callbacks and arities 0/1/3 on empty, matching and unmatched
  inputs. Unlike the unary entry guards, OTP's `zipwith/3` delegates to the
  fail-mode worker, invoking callbacks on available pairs before checking the
  rest. Nonempty pairs expose `badfun`/`badarity`; empty or unmatched inputs raise
  `function_clause` without callback effects. Source and executable controls
  agree on OTP 27.3.4.12, 28.3.1 and 29.1.3.
- Run explicit GC inside callbacks and before reporting. Retain both original
  lists, captured data and returned boxed values, including improper terms.
  Normalize only the function identity embedded in badarity, retaining its
  same-runtime identity check and exact argument list.

`list_zipwith.erl` is a standalone observer, rather than enlarging the existing
large helper. An independent Elixir two-list model derives both VMs' results,
exception boundaries and ordered traces without calling zip/zipwith or this
observer. A positive control passes; five negative controls reject identically
wrong reports with swapped arguments, reversed visits, reversed output, eager
length validation or eager callback validation. The 14-case filtered/reordered
replay checks saved parameters and generated-module reindexing.

Interpreter, AArch64 precompiled and on-demand JIT runs each match **95/95**,
with **14/14** reordered replays, zero failures and zero unverified references.
Independent result/trace checks pass for both VMs in all six runs. Complete
observations agree across execution modes for both full and reordered inputs.
Cold JIT uses `--timeout 120` for per-process compilation of the existing large
helper.
Each of OTP **27.3.4.12, 28.3.1 and 29.1.3** also satisfies all 95 independent
contracts using an observer freshly compiled with that OTP. Cross-version
execution uses Erlang directly, then Elixir 29 to decode/check saved results.
The full 50-method CLI harness passes: **47 passed, three optional skips**,
including the new all-cases/replay integration test. Erlang formatting, Python
3.9 grammar and whitespace checks pass.

Evidence: `.amp/in/artifacts/lynx-list-zipwith/` contains saved inputs/replay,
per-mode generated sources/manifests/results and build hashes, independent
checks, cross-OTP controls, positive/negative model checks, regenerated inventory
and matching prior-recipe preservation hashes. No library/runtime correction or
expected-failure entry is included. The complete runtime corpus, embedded,
32-bit/float32 targets, fresh Lean proofs, trim/pad APIs and exhaustive
callback/input combinations are outside this batch.

## Ordered unary list callbacks

Add 268 supported-API observations (2026-11-27): 31 each for `map/2` and
`foreach/2`, 42 for `flatmap/2`, 37 each for `filter/2` and `filtermap/2`, and
45 each for `search/2` and `dropwhile/2`. Preserve all 7,350 previous recipes and
metadata; total 7,618, attribution unchanged at 377/478 sites. Fixture coverage
remains 2,351 calls across 203 exports. No APIs or Lean proofs are added.

- Observe exact callback visits, invocation indices and captured data, results,
  original inputs and explicit GC inside callbacks and before reporting.
  Asymmetric inputs include integer/float distinctions, duplicates, maps, boxed
  integers, partial bitstrings, empty/singleton and 17-element lists.
- Check first/second/final callback errors, throws and exits; unused failing
  callbacks; invalid functions/arities; initial non-list and recursive improper
  tails. Retain exception class/reason, normalizing only badarity's embedded
  function to same-runtime identity while preserving its argument list.
- Check filtering, all three filtermap return forms, and flatmap empty/single/
  multiple-element expansion. Map accepts arbitrary returned terms; foreach
  ignores them. Invalid predicate/flatmap returns follow preceding effects.
- Search/dropwhile controls stop at the first, middle or final element, or never
  stop. The callback raises if called beyond the designated stop. Proper and
  improper inputs distinguish a retained untouched suffix from an incorrectly
  scanned tail. Exact predicates distinguish `1` from `1.0` and duplicate matches.
- Flatmap checks non-list/improper returned chunks and invalid early chunks
  followed by later callback errors/throws/exits. Deferred append validation must
  not preempt those later exceptions. These are source-checked observations of
  OTP 27/28/29, not universal claims about every implementation/version.

`list_callbacks.erl` is a standalone observer, avoiding enlargement of the large
existing helper. An independent Elixir sequential model computes both VMs'
expected results/traces without calling these seven APIs or the observer.
Its separate list constructor and append validator model early returns and
post-callback validation independently of the runtime traversal. A positive
control passes; four negative controls reject identically wrong reports with
reversed visits, reversed output, early flatmap validation or a scanned
short-circuit tail. Filtered/reordered replay retains recipe metadata and
reindexes the generated modules.

### Initial library differences — correction kept separate

Before correction, interpreter, AArch64 precompiled and on-demand JIT runs each
matched **239/268**, with **29 failures** and zero unverified references. Exact
failing recipes agreed across modes; no additional JIT-only failure was found.
Cold JIT uses `--timeout 120`
for per-process compilation of the existing large helper.
OTP **27.3.4.12, 28.3.1 and 29.1.3** each satisfy all 268 independent contracts,
using an observer freshly compiled by that OTP and an Erlang-only launcher for
cross-version controls.

1. **22 callback-validation cases:** `map/2` exposes `badfun`/`badarity` on
   nonempty inputs instead of OTP's `function_clause` (four cases).
   `foreach/2`, `flatmap/2` and `search/2` also accept bad callbacks on empty
   inputs, returning `ok`, `[]` and `false` respectively, and expose
   `badfun`/`badarity` on nonempty inputs (six each). Invalid-callback/non-list
   combinations already match OTP's `function_clause`.
2. **Six search predicate-result cases:** non-boolean returns are treated as
   false, continuing to the end and returning `false`, instead of OTP's
   `{case_clause, Result}` at the offending callback. These also have extra
   callback effects, unlike the validation-only differences.
3. **One map non-list entry:** a valid callback with `not_a_list` raises
   `function_clause` rather than OTP's `{case_clause,not_a_list}`. Reached
   improper tails retain `function_clause` and must remain distinct.

All 268 input/capture snapshots agree between VMs. Trace differences are confined
to the six invalid search predicate results; the other 23 failures concern only
exception outcomes. Valid traversal/output, short-circuit controls, callback
exception propagation, malformed filter/filtermap results and flatmap deferred
validation match. The initial coverage included no library/runtime correction
or expected-failure entry. Keep the separate correction below and ordinary
Erlang regressions outside the Lynx coverage files; do not broadly catch/relabel
callback exceptions.

Before correction, all three 21-case filtered/reordered replays matched **19/21**, reproducing the
same two sampled validation differences after reindexing. Independent result/
trace checks reproduce those failures; saved recipes retain their metadata.
Six direct calls without this observer reproduce the exception differences,
with four passing controls for exact search, retained improper dropwhile
suffixes, deferred flatmap failure and filter's invalid-result exception.

The initial full CLI suite executed 49 tests: all **45 other enabled tests passed**,
three optional configurations were skipped, and the new unary-callback test was
red in its full and replay subtests on the real library differences. Erlang
formatting, Python 3.9 grammar and whitespace checks passed.

### Separate unary library correction and ordinary Erlang regressions

`map/2`, `foreach/2`, `flatmap/2` and `search/2` now validate arity-one callbacks
at their public entries. `map/2` distinguishes its initial case check from
recursive malformed tails, and `search/2` accepts only boolean predicate results.
Private workers retain traversal, short-circuiting and deferred flatmap append
validation. No callback exception catch/relabel or unrelated API change is needed.

Three ordinary regressions extend `test_lists.erl`, outside Lynx: invalid
callbacks on empty/proper/improper/non-list inputs; entry/tail exceptions and
callback-raised errors/throws/exits after preceding effects and GC; and invalid
search returns stopping before later effects, with exact numeric/improper-suffix
search and deferred flatmap controls. All three regression groups independently
fail on the unchanged library and pass after correction. The test driver uses
`foldl/3` rather than depending on `foreach/2`, which is itself under test.
Library source, ordinary tests and changelog remain separate from the Lynx
observer, recipes, harness and notes.

Full ordinary `test_lists`, `test_queue`, `test_sets` and `test_maps` pass on OTP
27.3.4.12, 28.3.1 and 29.1.3, and in AtomVM interpreter, AArch64 precompiled and
on-demand JIT modes. Fixed local lists source and ordinary tests compiled with
OTP 27 also pass in the interpreter. A 10,000-element validation of all four
changed APIs passes in every AtomVM mode, checking exact outputs, every visit,
count, independently calculated sum and explicit GC. Fresh interpreter,
precompiled and on-demand JIT differential runs each match **268/268**, with
**21/21** reordered replays, zero failures/unverified references and independent
result/trace checks of both VMs. Complete observations are identical across modes
for both inputs. The full 49-method CLI harness passes: **46 passed, three optional
skips**. No expected-failure entries were added.

Correction evidence: `.amp/in/artifacts/list-callback-fix/` retains final per-mode
runs, independent checks, ordinary before/after logs, cross-OTP and large-list
controls, validated-source hashes, a separate library/ordinary-regression patch
and small reproducers. `VALIDATION.md` records commands and the corrected JIT
bootstrap archive selection; disposable scratch packs and beams were removed.

Baseline evidence: `.amp/in/artifacts/lynx-list-callbacks/` contains saved inputs/replay,
per-mode sources, manifests, actual results, build hashes and independent checks,
cross-OTP controls, positive/negative model controls, failure classification,
direct public-call controls, regenerated inventory and the preservation hash.
The complete runtime corpus, embedded/32-bit/float32 targets, fresh Lean proofs
and exhaustive callback/input combinations are outside this batch.

## Ordered list folds and mapfolds

Add 128 supported-API observations (2026-11-27): 29 each for `foldl/3` and
`foldr/3`, 35 each for `mapfoldl/3` and `mapfoldr/3`. These are supported-behavior
extensions, not fresh Lean proofs or newly attributed sites. All 7,222 previous
recipes and metadata are unchanged (total 7,350; attribution remains 377/478).

- Use asymmetric inputs, duplicates, exact integer/float distinctions, compound
  elements, boxed integers and partial bitstrings, empty/singleton and 17-item lists.
- Return exact ordered callback traces with every incoming accumulator and
  captured value. Noncommutative nested accumulators distinguish direction,
  argument swaps and stale inputs. A replacement accumulator changes its shape
  after the second callback. Mapping depends on the incoming accumulator, and
  mapped result order is checked independently of callback execution order.
- Exercise error/throw/exit on the first, second and final invocation, callbacks
  that would fail only after an empty/singleton input has ended, malformed
  mapfold returns, non-functions, wrong arities, improper tails and non-list inputs.
  An improper input with a failing callback distinguishes left-side effects
  from right-side traversal failing before any invocation.
- Collect explicitly inside each callback and before returning the observation;
  retain captured data, intermediate accumulators, effects and original inputs.

`list_folds.erl` is a separate observer, avoiding growth of the existing large
helper. It calls the public API via `apply/3`, catches exact exception class/reason,
returns exact traces and does not use the fold APIs to construct its inputs.
Only badarity's embedded function is represented by same-runtime identity;
its argument list remains exact. An independent Elixir ordered-equation model
checks both VMs without calling any of these four fold APIs or the observer.
Three negative controls reject identically wrong OTP/AtomVM results with reversed
visits, an incorrect intermediate accumulator and reversed mapped output; an
unchanged positive control passes. Filtered/reordered replay checks recipe
metadata retention and reindexing.

### Initial library differences — correction kept separate

Initially, interpreter, AArch64 precompiled and on-demand JIT runs each matched
**98/128**, with **30 failures** and zero unverified references. Cold JIT uses `--timeout 120`
for the existing large helper's per-process compilation. All valid-input traversal,
accumulator and mapped result checks agree; malformed mapfold results also match.
The initial failures were:

1. **24 callback-validation cases:** all four entry points lacked OTP's arity-two
   guard. Empty lists wrongly accepted invalid callbacks; nonempty lists could raise
   `badfun`/`badarity` instead of `function_clause`. In `foldr/3`, reversing a
   malformed input could also preempt the invalid-callback check with `badarg`.
2. **Five `foldr/3` malformed-input cases:** valid callbacks with improper lists
   or a non-list raised `badarg` from the delegated reverse, versus OTP's
   `function_clause`. Neither VM invoked a callback; this was not an effect-order bug.
3. **One `foldl/3` non-list entry:** OTP's guarded entry checks the initial list
   with a case and raises `{case_clause,not_a_list}`; AtomVM raised
   `function_clause`. An improper tail reached recursively remains
   `function_clause`, so these boundaries must not be conflated.

OTP **27.3.4.12, 28.3.1 and 29.1.3** each match all 128 independent contracts,
using an observer compiled by the corresponding OTP. The model initially assumed
all non-list entries raised `function_clause`; actual OTP output and source
identified `foldl/3`'s distinct entry boundary, and the model was corrected.
These are observed compatibility differences, not inferred from source alone.
The initial coverage contained no library/runtime correction or expected-failure
entry. Keep the separate correction below outside the coverage files; do not catch
and relabel arbitrary callback exceptions or change `lists:reverse/1` globally.

Before correction, all three modes' 15-case filtered/reordered replays each
matched 12 and reproduced three of the same failures. Exact callback traces, retained inputs and captures
agree on all 128 full-run cases and all 15 replay cases in every mode; the
mismatches are exception outcomes, not invocation/GC differences. Four direct
public calls without the observer reproduce the validation/input differences.
The initial full-batch CLI test was red on the real library differences, as was
its replay subtest. Five existing CLI tests passed: set callbacks, queue callbacks,
map callbacks, seed/replay and independent behavioral oracles.

### Separate library correction and ordinary Erlang regressions

All four public entry points now validate arity-two callbacks before processing
any list. `foldl/3` distinguishes its initial case check from recursive tail
matching. `foldr/3` traverses directly rather than delegating to `reverse/1`, so
malformed inputs naturally raise `function_clause` before invoking callbacks.
The mapfold workers retain their established traversal/result construction.
No callback exception translation or global `reverse/1` change is needed.

Two ordinary regressions extend `test_lists.erl`: callback validation on empty,
proper, improper and non-list inputs, and exact entry/tail exceptions and
ordered preceding effects. User errors (`badarg`, `function_clause`, and a
structured `case_clause`), throws and exits propagate unchanged after preceding
callback work and explicit GC. The new regression fails on the unchanged library.
Fix, ordinary tests and changelog are outside the Lynx coverage files.

Full `test_lists`, `test_queue`, `test_sets` and `test_maps` pass on OTP 27.3.4.12,
28.3.1 and 29.1.3, and in AtomVM interpreter, AArch64 precompiled and on-demand
JIT execution. An additional interpreter control uses the fixed local library
and ordinary tests compiled by OTP 27. A separate target control confirms
`reverse/1`'s existing malformed-input behavior is unchanged. A 10,000-element
validation of all four operations passes in all three AtomVM modes, checking
count, independently calculated sum, first/last visits, mapped positions and GC.
Fresh interpreter, precompiled and on-demand JIT differential runs each match
**128/128**, with **15/15** reordered replays, including independent expected-result
and trace checks. The complete CLI suite passes **45 tests**, with three optional
environment-gated skips (48 total). No expected-failure entries were added.
The full runtime corpus was not rerun; embedded/32-bit/float32 execution, all
input shapes, fresh Lean proofs and exhaustive callback combinations are not claimed.

Evidence: `.amp/in/artifacts/lynx-list-folds/` contains saved input/replay,
per-mode sources, manifests, logs and build hashes, independent checks,
cross-OTP controls, negative/positive model controls, direct public-call controls,
regenerated inventory and the previous-recipes preservation hash.
`.amp/in/artifacts/list-fold-fix/` contains the failing baseline, fixed per-mode
runs and independent checks, ordinary/cross-OTP and 10,000-element validation,
CLI suite log, build logs and standalone reproducers.

## Set callback combinations

Add 140 supported-v2 callback recipes (2026-11-27), without new APIs, fresh Lean
laws or new source attribution. Preserve all 7,082 earlier recipes and metadata;
total 7,222, still 377/478 attributed sites and 2,351 fixture calls across 203 exports.

- `fold/3`: 29 cases, with an exact-key accumulator seeded with a partial
  bitstring, nonzero count and empty membership map. Check the final accumulated
  members/count and every raw accumulator input, not a shared traversal order.
- `filter/2` and `filtermap/2`: 38 cases each, including keep/drop/transform
  results, true/false controls, invalid callbacks and non-boolean results.
- `map/2`: 35 cases, including identity, constant mappings and two distinct tagged
  inputs mapping to one result, while integer `1` and float `1.0` remain distinct.
- All four operations exercise `from_list`, repeated additions and full
  deletion/reinsertion histories. Inputs include duplicates, compound exact keys,
  boxed integers and partial bitstrings, empty/singleton and 31/32/33-element sets,
  wrong arities/non-functions, and error/throw/exit after preceding effects.

`set_callbacks.erl` is a separate coherent observer, compiled/packaged alongside
the existing helpers. It reuses `differential:set_elements/1` for public result
normalization: size, emptiness, enumeration count and exact-key membership. The
original set remains observable after GC inside each callback and after the
operation. Raw `LYNX_SET_TRACE` records retain callback visits and fold inputs;
returned traces compare count and validity, never arbitrary visit order. A
failure on the second invocation admits any two distinct input elements, not
identical prefixes across VMs.

The CLI regression checks both VMs against an independent list/exact-key-map
model that invokes neither `sets` nor observer APIs. It checks all raw effects,
not just the helper's returned boolean, and repeats a 16-case filtered/reordered
replay. Independent negative controls reject identically forged OTP/AtomVM
results that lose the float key, duplicate raw visits despite a true validity
boolean, and broken fold accumulator threading. A positive control reverses
only AtomVM's raw visits (rethreading fold inputs correctly) and is accepted.

### Exposed library difference — kept separate

Before the library correction, interpreter, AArch64 precompiled and on-demand
JIT sweeps each matched **137/140**,
with zero unverified references. All three failures are `sets:filter/2` in
`invalid_return` mode, across the three construction histories. OTP raises
`error:{bad_filter,not_boolean}` after the second predicate invocation; AtomVM
raises `error:{case_clause,not_boolean}`. Every OTP result matches the independent
model; all 280 raw traces and accumulator checks pass in each full run,
so this is not a claimed visitation-order mismatch.

AtomVM delegated this operation to `maps:filter/2`, whose explicit true/false
case produces `case_clause`. OTP's v2 set implementation instead evaluates the
predicate as a comprehension filter. Direct OTP 27.3.4.12, 28.3.1 and 29.1.3
controls all raise `bad_filter` for `not_boolean`, `[]` and `{true,x}` after two
calls, retaining the original three-element set. The initial full-batch CLI
regression was red on this real exception-contract difference; no expected-failure
entry was added. Four existing CLI tests passed: map callbacks, queue callbacks,
seed/replay and independent behavioral oracles.

### Separate library correction and ordinary Erlang regression

`sets:filter/2` now uses a list-comprehension predicate over `maps:keys/1`, then
reconstructs the set with `maps:from_keys/2`. This naturally raises `bad_filter`
without catching/relabeling user exceptions or changing `maps:filter/2`.
The fix, ordinary `test_sets.erl` regression and changelog are outside the Lynx
coverage files so they can be committed independently.

The new regression fails on the unchanged library and passes after correction.
It checks three non-boolean results after preceding callback work, stopping at
the failure without assuming traversal order, original set preservation,
callback-raised `case_clause`, throw/exit propagation, no callback on empty sets,
and exact integer/float membership in the successful result.

Full `test_sets:test()` and `test_maps:test()` pass on OTP 27.3.4.12, 28.3.1 and
29.1.3, and in AtomVM interpreter, AArch64 precompiled and on-demand JIT modes.
A separate control confirms `maps:filter/2` still raises `case_clause`.
An OTP-27-compiled local `sets` module and regression also pass in the interpreter.
Fresh interpreter and precompiled differential runs match **140/140** and each
16-case filtered/reordered replay passes, including independent result/raw-trace
checks. On-demand JIT reruns match all **38/38 filter recipes** and the **16/16
mixed replay**, with independent checks passing. The other 102 JIT recipes
passed before this filter-only library correction; they were not rerun afterward.
The maintained set-callback CLI test now passes. The full CLI/runtime
corpora were not rerun. Erlang formatting, Python 3.9 grammar and whitespace
checks pass.

The initial on-demand run's default five-second limit timed out compiling the
existing large helper (over 16 seconds in a completed control). Explicit
`--timeout 120` allows cold compilation in each isolated case; this was not a
new JIT defect.

Evidence: `.amp/in/artifacts/lynx-set-callbacks/` contains saved input/replay,
per-mode runs, independent checks, positive/negative controls, cross-OTP error
controls, inventory, preservation hash and harness logs. Separate fix evidence
under `.amp/in/artifacts/sets-filter-fix/` retains the failing baseline, per-mode
ordinary regressions, fresh differential runs, independent checks and replayable
regression packs. No full runtime corpus, fresh Lean laws, v1 sets,
embedded/32-bit/float32 targets, exhaustive callback combinations or all traversal
orders are claimed.

## Queue callback combinations

Add 153 supported-API recipes (2026-11-26), complementing the seeded queue
histories rather than adding new APIs or proof obligations:

- `filter/2` and `filtermap/2`: 24 cases each, including filter list expansion,
  keep/drop/map returns, empty/singleton controls and all three public construction
  histories (`from_list`, repeated `in`, repeated `in_r`).
- `fold/3`: 15 cases with a noncommutative nested accumulator, exact visits and
  accumulator inputs, exposing reversal or incorrect accumulator threading.
- `any/2` and `all/2`: 24 cases each, checking short circuit, neutral/decisive
  predicates, invalid returns and exception prefixes without requiring arbitrary
  visitation order. Preserve raw ETF traces and independently check exact input
  multiplicity and callback counts, not just the observer's validity boolean.
- `delete_with/2` and `delete_with_r/2`: 21 cases each, distinguishing first/last
  exact integer matches from floats and no-match queues. All families include
  invalid callbacks, wrong arities, throw/error/exit after preceding effects and
  empty queues whose valid callback must not run.

The observer retains the original immutable queue and records callback effects
across explicit GC inside every callback and after the operation. Normalize queue
results to ordered lists, never private representations. The CLI regression
checks **both** OTP and AtomVM outputs against an independent list-based model
that invokes no queue APIs or observer helpers, then repeats a 14-case
filtered/reordered replay. These are finite observable contracts, not Lean proofs.

The first interpreter sweep found six differences: OTP raises
`error:{case_clause,not_boolean}` after the second predicate call, while AtomVM's
`queue:any/2` raised `badarg` and `queue:all/2` returned `false`, in all three
construction histories. Both delegate to `lists`. A separate minimal library
correction gives `lists:any/2` and `lists:all/2` explicit true/false cases. The
standalone Erlang regression checks three invalid return values, retained effects
and short circuit for both APIs; it fails unchanged AtomVM and passes OTP 29.1.3
and the fixed interpreter, AArch64 precompiled and on-demand JIT runtimes. Full
`test_lists` and `test_queue` also pass in each of those four environments.

The new observer lives in `queue_callbacks.erl`, compiled/packaged alongside the
unchanged `differential.erl`. Appending it to the large existing helper exposed an
AArch64 compiler defect: `jump_to_continuation/2` attempted an out-of-range ADR to
the module base (`-1048888`, outside the instruction's approximately 1 MiB reach).
Keeping the coherent observer separate avoids triggering that limit; it does not
fix the compiler defect or turn a compilation failure into an expected result.

Validation against OTP 29.1.3:

- All 153 cases and the 14-case filtered/reordered replay match in interpreter,
  AArch64 precompiled and on-demand JIT modes, with zero failures or unverified
  references. Each of the six runs also passes the independent list model and
  raw-trace checker: 96 unordered traces per full run, ten per replay.
- Independent negative controls reject identical wrong OTP/AtomVM reports for
  reversed filter output, reordered callback effects and duplicate unordered
  visits. The last control leaves the observer's returned validity boolean true;
  the independent exact-multiplicity check rejects the raw visits.
- Full CLI harness: 43 passed, three optional skips (46 discovered, 1,036.854s),
  including the new callback/model/replay regression and existing shrinking,
  failure-stage and baseline checks. Erlang format, Python 3.9 grammar and
  whitespace checks pass. Both helper sources match their saved copies in all
  six runs.

Evidence: `.amp/in/artifacts/lynx-queue-callbacks/` contains `final-input.json`,
`final-replay.json`, `final-inventory.json`, all six `final-*` runs, independent
and negative-control logs, the standalone library regression's before/after logs,
compiler-limit reproducer and the harness logs. All 6,929 previous runtime
recipes and metadata remain unchanged: total 7,082; attribution 377/478 sites;
fixture calls 2,351 across 203 exports. No expected failures were added. No full
runtime corpus, fresh Lean laws, embedded/32-bit/float32 or exhaustive callback
combination coverage is claimed. Keep the library correction and standalone
Erlang regression separate from the Lynx coverage and notes.

## Seeded queue sequences

Extend the generated corpus with an eleventh operation family (2026-11-26),
without adding an API, proof obligation or runtime/library fix. Four deterministic
boundary histories start with 0/1/7/32 items; seeded histories start with up to 33
items. Each contains at most 69 operations. The first 36 boundary cases and all
saved replay inputs retain their existing meaning; queue boundaries occupy the
next four positions. Seeded queues use one of the former arithmetic slots every
20 cases, retaining seeded arithmetic exploration in the other slot.

`queue_sequence/2` extends the existing sequence observer. It records each result
or exception and the queue's length, emptiness, ordered contents, both end peeks
and saved queue contents. Mutation results are normalized to lists, not opaque
representations. Each split returns both ordered partitions and keeps the left
partition as the current queue. Joins prepend/append an explicit second queue.
A saved queue initially holds the input value, so deleting a save during shrinking
does not invalidate restore. Failed operations leave both current and saved
values unchanged. Explicit GC checks values retained across later calls.

Histories mix insertion/removal at both ends, drain beyond empty and rebuild,
reverse, split at valid/invalid indices, join asymmetric inputs on both sides,
query membership and delete the first/last exact occurrence. Duplicates, `1` versus
`1.0`, compound exact-distinct values and partial bitstrings distinguish wrong-end,
loose-equality and retention mistakes. Empty `get`/`drop` errors and rejected
splits are followed by further operations to check exception recovery. The
7-item front-drain and 32-item rear-drain controls deliberately empty one internal
half while several elements remain, rather than keeping both halves balanced
by alternating removals.

The public CLI regression compares full traces from **both** VMs with an independent
ordered-list model using no queue APIs or differential helpers, then repeats a
filtered/reordered replay. The existing forced-failure shrink/replay regression
now includes queues, preserving setup, literal operations, seed and original index.
The model is a finite test oracle, not a new Lynx proof.

Validation against OTP 29.1.3:

- All 148 current histories (4,498 operations; seeds 20261126, 20261127 and 42)
  match in interpreter, AArch64 precompiled and on-demand JIT modes. Each VM's
  complete traces also match the list model. The four boundary histories were
  rerun in all modes after strengthening the initial alternating drains to
  one-sided drains; all 144 seeded histories were unchanged. Original sweeps
  and reruns are retained separately, with exact input correspondence checked.
- Nine-case filtered/reordered replay passes in every mode and against the model.
  A normal `--seed 42 --generated 40` run passes all 40 cases across eleven
  families. The first 36 mixed cases and 200 float-only cases are unchanged for
  all three seeds, and all nine helper copies match the final source hash.
- Full CLI harness: 42 passed, three optional skips (45 discovered, 480.486s).
  After strengthening the boundaries, the queue/model/replay and seeded CLI
  reproducibility regressions were rerun and both passed. Forced-failure queue
  shrinking/replay passed in the full harness. Python 3.9 grammar, Erlang format
  and whitespace checks pass.
- Independent negative controls reject identical wrong traces from both VMs
  for swapped ends, lost saved contents and wrong split-error reasons.

Evidence: `.amp/in/artifacts/lynx-queue-sequences/`, including `final-input.json`,
`final-inventory.json`, per-mode runs/replays/boundary reruns, independent logs,
`harness.log`, `final-targeted.log` and `final-validation.json`. Runtime recipes
remain 6,929; fixture calls remain 2,351 across 203 exports, and attribution remains
377/478 sites. No expected failures were added. No full runtime corpus, fresh
Lean laws, embedded/32-bit/float32 or exhaustive operation-history validation
is claimed.

## Unfenced self-exit completion

Add 18 asserted recipes (2026-11-26), extending the previously fenced self-exit
observations without adding an API or runtime change:

- Six legacy self-`exit/2` controls, six unfenced `exit_signal/2` samples and
  six receive-fenced `exit_signal/2` controls combine normal/structured/kill
  reasons with both trap settings. Fifteen recipes have exact outcomes; only
  three fatal unfenced async recipes allow multiple combinations.
- Each monitored worker calls a callback and reports its returned value, then
  completes without an explicit receive on the unfenced path. The exact report
  includes the API's `true` result and a tagged continuation payload. Reporting
  establishes a function return, not worker survival or the internal state of
  the self-signal. The observer waits for DOWN before checking all reports and
  inactive-monitor cleanup, rejecting missing/duplicate/extra reports, wrong
  return values, unexpected DOWN reasons and timeouts.
- Fatal unfenced signals permit death before the report, death after the report,
  or normal completion with the exact report. Normal completion without a report
  is never accepted there. Legacy self-exit while untrapped, including reason
  `normal`, and legacy self-`kill` cannot report. Receive-fenced fatal async calls
  must likewise terminate before the callback returns. Nonfatal cases must
  report exactly once and terminate normally. No catching of exits is involved.
- Both OTP 29.1.3 and AtomVM actually returned and completed normally after
  unfenced self-`kill` in the isolated sweep. The paired receive-fenced control
  dies with `killed` and no report. Normal completion can win over reception;
  `kill` is untrappable when received, not a synchronous self-termination promise
  for `exit_signal/2`. OTP's `send_exit_signal_bif` preserves synchronous self
  suicide only for legacy `exit/2`; its exiting-signal handler discards pending
  exits without replacing an already selected termination reason.
- Recipes require the allowed predicate to be true on each VM, not agreement on
  a scheduling winner. `LYNX_COMPLETION_TRACE` preserves actual ETF observations
  in OTP `proofs.log` and individual AtomVM logs. The CLI regression checks both
  sets against an independent handwritten table, six-case filtered/reordered
  replay and the OTP gate. The twelve async recipes require OTP 29; legacy
  controls introduce no newer API requirement.
- BEAM inspection distinguishes the direct legacy callback, the unfenced async
  callback with no receive operations, and the fenced async callback with a real
  scan/wait. The worker wrapper makes a non-tail callback call before sending the
  report and returning; it has no receive operations. Parent-side receives are
  outside the critical path. These are public behavior samples inspired by
  `exit_signal_self_is_handled_later`, not a proof that every permitted schedule
  or a particular queued-signal state was reached.

Validation against OTP 29.1.3: all 18 cases pass in interpreter, AArch64
precompiled and on-demand JIT modes, zero failures/unverified. Six-case
filtered/reordered replay passes in each mode. Independent tables check raw
traces from both VMs, and the CLI regression checks the OTP gate. Repeating all
18 samples 64 times gives 1,152 observations per VM per mode (6,912 total), all
independently valid. Every unfenced repeat in these runs chose normal completion
with its exact report; no other scheduling winner is required or claimed as
observed. The fatal receive-fenced controls do exercise termination before the
report. The full CLI harness passes 41 tests with three optional skips (44
discovered, 436.619 seconds). Saved BEAM checks, source/helper hashes, Erlang
formatting, Python 3.9 grammar, whitespace and baseline-preservation checks pass.
No runtime fix or expected-failure addition was needed.

All 6,911 earlier recipes and metadata remain unchanged: total 6,929; attribution
377/478 sites in 28 files; fixture counts unchanged at 2,351 calls / 203 exports /
13 modules. Evidence is under `.amp/in/artifacts/lynx-self-exit-completion/`.
No full runtime corpus, fresh Lean laws, embedded/32-bit/float32 or all-schedule
coverage is claimed. Runtime fixes and standalone Erlang regressions, if needed,
remain separate from this coverage batch.

## Lowered-receive continuations

Add 32 recipes (2026-11-26): 16 manually lowered-Core cases and 16 source
controls, inspired by upstream `core_to_leanj_test.exs`'s distinct timeout-only
message binders and compiler-dropped unused receive bodies. These are runtime
observations, not executions of the translator tests or new Lean attribution.

- Two consecutive timeout-only loops use an opaque positive timeout. Effects
  distinguish the first wait, second wait and outer continuation, while a
  captured value must survive both. Failures in either timeout action reach
  the protected computation's handler; a failure in its `of` continuation
  escapes that handler. Handler failures propagate, cleanup runs exactly once,
  and a cleanup failure overrides an earlier continuation failure.
- Dropped-body matches end directly at Core `remove_message`, without a
  fabricated value-producing clause body. Source controls discard distinct
  pure clause results. Exact patterns reject `{take, 7.0}`; opposite queued
  integer-match orders distinguish mailbox order from clause priority. The
  first match is removed exactly once, leaving the decoy, second match and
  sentinels in their original order. No-match cases time out, and timeout-action
  errors are distinguished from errors in the continuation after the timeout.
- A rescan control first selectively scans the entire prepared mailbox for an
  absent message and times out. Its next receive must start scanning afresh
  and consume a previously skipped match, preserving the rest of the mailbox.
- Existing `core_order/1` records outcomes/effects; a small isolated observer
  prepares and drains the mailbox even after caught probe exceptions. The CLI
  regression checks each VM against handwritten expected tuples, not merely
  against the other VM, and replays six filtered/reordered cases with reindexed
  Core companions. Numeric types, mailbox order, exception reasons and exact
  effect counts remain observable.
- Emitted BEAM is inspected for representative Core and source controls:
  two `wait_timeout` sites for the successful timeout sequence, a scan/removal
  branch for the dropped-body case, and two scans for the rescan control.
  The raw-Core and source controls need not produce different final opcodes;
  they independently exercise the two compiler input routes. No new optimized
  mailbox-skip implementation or signal-return-before-reception claim is made.

Validation against OTP 29.1.3: all 32 cases pass in interpreter, AArch64
precompiled and on-demand JIT modes, zero failures/unverified. Six-case
filtered/reordered replay also passes in each mode. Independent tables check all
114 saved AtomVM results and all 114 OTP expectations across those six runs.
Representative BEAM confirms the expected wait/scan/removal sites for both
compiler input routes. The full CLI harness passes 40 tests with three optional
skips (43 discovered, 424.238 seconds). Erlang formatting, Python 3.9 grammar,
whitespace and baseline preservation checks pass. No runtime fix or
expected-failure addition was needed.

All 6,879 earlier recipes and metadata are preserved: total 6,911; attribution
remains 376/478 sites in 28 files; fixture counts stay 2,351 calls / 203 exports /
13 modules. Local evidence is under
`.amp/in/artifacts/lynx-receive-continuations/`. No full runtime corpus, fresh
Lean laws, embedded/32-bit/float32 or all-schedule coverage is claimed. Runtime
fixes and standalone Erlang regressions, if needed, remain separate.

## Liveness/dead-target queries with pending notifications

Add 46 asserted recipes (2026-11-26), without new runtime APIs or changes:

- Six monitor/liveness samples use normal and structured reasons with a held
  live child, an already-dead child, and an unfenced exiting child. Install
  the tested monitor immediately before `is_process_alive/1`, without a
  receive or peer acknowledgement between them. Require the real DOWN,
  `demonitor(Ref, [info]) =:= false` afterwards, and both retained sentinels.
  Live controls require `{true, Reason}`; dead controls require
  `{false, noproc}`. Racing permits all four boolean/reason combinations.
- OTP 29.1.3 actually produced all four pairs in a separate 10,000-sample
  survey: 9,967 `{true, normal}`, 30 `{true, noproc}`, one `{false, normal}`,
  two `{false, noproc}`. This disproved an initial too-strong interpretation
  of the outgoing-signal ordering guarantee. A liveness snapshot is not an
  acknowledgement of monitor establishment. No particular race outcome or
  frequency is required, nor is a specific internal queue state claimed.
- Forty query samples use `is_process_alive/1` and
  `process_info/2` (`registered_name`). For dead-target cases, a coordinator
  awaits the target's DOWN before starting the worker. The worker sends itself
  async normal/structured/kill signals with both trap settings. It reports
  the query result before any subsequent receive. A final same-sender marker
  forces reception, preserving the sentinel and any converted EXIT message.
- For fatal signals, querying before sending must produce exactly one query
  report; receiving the signal before querying must produce none. Only the
  unfenced variant allows either. Survivors must report the exact query result
  and mailbox, not merely avoid crashing. Four held-live controls ensure that
  constant false/undefined query answers cannot pass. Child timeout cleanup and
  target-monitor reaping are explicit. Async injection requires OTP 29.
- This isolates the dead-query fast path from target-death timing: the pending
  signal is self-sent, and the queried dead PID is a separate process. It is a
  public-behavior projection inspired by the upstream killer/query example,
  not a literal translation of its program or a return-before-reception proof.
  The final receive deliberately precludes a survivor escaping a fatal signal.
- Both VMs must return true via the existing `assert_true` contract. Single-line
  `LYNX_QUERY_TRACE` ETF observations are saved in OTP `proofs.log` and each
  AtomVM probe log. Independent handwritten tables check both sets of traces,
  exact controls, six-case filtered/reordered replay and the OTP gate. BEAM
  inspection confirms real query calls and marker receive loops, with no receive
  between the unfenced signal send and query, or tested monitor and liveness call.
- All 46 cases pass against OTP 29.1.3 in interpreter, AArch64 precompiled
  and on-demand JIT modes, zero failures/unverified; independently checked all
  saved traces and final helper hashes. Repeating all probes 64 times produces
  2,944 observations per VM per mode, all independently valid (17,664 total).
  Actual trace frequencies are retained, without requiring all allowed outcomes.
  Three unfenced liveness queries reported before fatal reception on AtomVM but
  not OTP in the isolated interpreter sweep; both satisfy the same contract.
- Full CLI harness: 39 passed, three optional skips (42 discovered). Erlang
  formatting, Python 3.9 grammar, whitespace and baseline checks pass. Temporary
  stale-monitor-cleanup and extra-message injections were rejected; mutated
  files were removed. No runtime fix or expected-failure entry was needed.
- Preserve all 6,833 earlier recipes and metadata: total 6,879; attribution
  376/478 sites in 28 files; fixture counts unchanged. The new attribution
  covers finite projections of `alive_may_leave_monitor_parked` and
  `process_info_of_dead_does_not_flush`, not universal theorem premises.

Local evidence: `.amp/in/artifacts/lynx-query-notifications/` contains saved
inputs, OTP survey/source evidence, per-mode observations, repeated-run traces,
independent CLI/harness logs, BEAM inspection and inventory. No full runtime
corpus, fresh Lean laws, embedded/32-bit/float32 or all-schedule coverage is
claimed. Runtime corrections and standalone Erlang regressions, if needed,
remain separate from this coverage batch.

## Exit reception across trap-flag changes

Add 54 recipes (2026-11-26): self `exit/2`, self `exit_signal/2` and async
self-alias exits with deactivation; three timings, both flag-change directions,
and normal, structured (`{shutdown, <<5:3>>}`) and kill reasons. There are 36
exact controls and 18 unfenced samples. Async cases require an OTP 29 oracle.
No new runtime API or runtime correction is part of this batch.

- `received` sends a later same-sender marker and receives that marker while
  leaving the real converted EXIT and an earlier sentinel unconsumed. Changing
  the flag or removing an alias must not retract/reinterpret that message.
- `before` changes the flag before sending; alias controls deactivate before
  sending, so even kill must be dropped. `pending` changes the flag without a
  reception fence, then deactivates aliases: reception may precede the flag
  change, occur after it, or lose to alias deactivation. This label does not
  assert that a signal was actually still pending on either VM.
- A final same-sender marker forces reception before survivors report. Saved
  BEAM contains the marker `loop_rec`/`wait` paths; these are not optimized-away
  timeout-only receives. Monitor DOWN/report observations include the prior
  flag value, alias-removal result and exact retained mailbox. The outer worker
  kills/reaps a timed-out child; runner timeouts remain an additional bound.
- The allowed sets distinguish fatal exits, ignored normal signals, trapped
  messages and inactive aliases. OTP's legacy suicide flag is selected at send
  time, not retroactively at reception. Legacy normal sent while untrapped
  terminates before return; normal sent while trapping may later be ignored.
  The initial contract incorrectly applied the legacy quirk at reception;
  OTP 29.1.3 `bif.c:send_exit_signal_bif` establishes the corrected rule. The
  first failed comparison is an observer-development error, not a VM defect.
- Each recipe uses the existing `assert_true` contract: agreement on a false
  predicate or exception cannot pass. Raw `LYNX_EXIT_TRACE` ETF byte lists are
  saved in OTP `proofs.log` and each AtomVM probe log, including successful
  cases. Failure observations are not normalized into success. A handwritten
  independent table checks both VMs' raw traces, exact singleton controls,
  six-case filtered/reordered replay and the OTP-version gate. Temporary
  extra-message injection was rejected by both an inactive-alias and a
  received-EXIT control; the mutated source/beam were removed.
- OTP and AtomVM actually chose different legal outcomes for two legacy
  true-to-false unfenced cases: normal could be ignored versus queued as EXIT;
  a structured reason could terminate versus remain a queued EXIT. This is
  evidence that exact trace equality would reject legal timing differences,
  not evidence of a runtime bug or exhaustive scheduling coverage.
- All 54 final recipes satisfy the asserted contract against OTP 29.1.3 in
  interpreter, AArch64 precompiled and on-demand JIT modes, with zero failures
  or unverified cases. Independently checked all three modes' raw traces;
  final helper hashes match the source. Repeated all 54 probes 32 times in
  each mode (1,728 worker observations per VM per mode), all allowed. Actual
  trace frequencies are retained; this does not claim every allowed outcome.
  Focused compilation of just the observer and its dependencies also passes
  all 18 legacy cases on OTP 27.3.4.12. The full helper already uses the newer
  `is_integer/3` guard and cannot be compiled wholesale by OTP 27; no whole
  OTP-27 harness run is claimed.
- Full CLI harness: 38 passed, three optional skips (41 discovered). The first
  suite run exposed a stale total-count assertion (6,779); it was updated to
  the verified 6,833 before the successful full rerun. Erlang formatting,
  Python 3.9 grammar, baseline preservation and whitespace checks pass.
- All 6,779 preceding recipes/metadata are preserved: total 6,833, attribution
  374/478 sites in 28 files; fixture counts unchanged. Only the legacy pending
  self-signal site gains attribution. Async cases end with a receive, so they
  are attributed to the existing reception-fence site, **not** to
  `exit_signal_self_is_handled_later`'s return-before-reception observation.
  Returning before any receive remains outside this batch. Alias behavior is
  an OTP extension, not modeled by Lynx.

Local evidence: `.amp/in/artifacts/lynx-exit-reception/` contains original/final
inputs, per-mode sweeps and raw traces, repeated-run observations, independent
CLI/harness logs, source evidence for the legacy send-time rule, BEAM receive
inspection and regenerated attribution. No all-outcome, full-corpus, fresh
Lean-law, embedded/32-bit or float32 validation is claimed.

## Structural BIF execution paths

Add 70 recipes (2026-11-26) through the existing `execution_paths/3` observer:
`length/1` (8), `hd/1` (6), `tl/1` (6), `tuple_size/1` (5), `element/2` (15),
`byte_size/1` (15) and `bit_size/1` (15). Keep the previous arithmetic,
comparison and map-get observations unchanged; no new runtime API is needed.

- Compare exact body and dynamic-call results/exceptions against guard
  success/rejection. Valid results that do not equal the guard target must
  reject without being confused with invalid inputs. Improper cons cells are
  valid for `hd`/`tl` but invalid for `length`; tuple controls include empty and
  non-tuples, negative/zero/last/past-end indices, float/atom/bignum indices,
  and a runtime-built 33-element tuple.
- Each recipe keeps opaque inputs and the first observations live across calls
  and explicit `garbage_collect/0`, then observes all three paths again.
  Runtime-built lists/tuples include heap terms and live process/reference/fun
  values where only scalar sizes escape. Bitstring controls cover 0/1/7/8/9/
  23/24/25 bits, 31/257-byte binaries, 65-byte aligned and unaligned slices,
  and an unaligned 515-bit slice. Byte size rounds partial bytes up, not down.
- OTP 29.1.3 BEAM inspection confirms body `bif` instructions for `hd`, `tl`,
  `element` and `tuple_size`; `gc_bif` for `length`, `byte_size` and `bit_size`.
  Body failure targets are zero (raise); guards use the same BIF families with
  nonzero rejection targets. Dynamic calls use `erlang:apply/3`. These are
  distinct dispatch/failure contexts, not seven newly exercised dedicated
  structural opcodes. Representative drivers retain inputs and first results
  in stack slots across explicit GC; calls are not constant-folded away.
- Extend the existing CLI regression with independent expected-value tables,
  arithmetic byte-size expectations and an eight-case filtered/reordered replay
  mixing structural and original arithmetic cases. No assembly-shape
  assertions are kept as brittle permanent tests.
- All 70 recipes match OTP 29.1.3 in interpreter, AArch64 precompiled and
  on-demand JIT modes, with zero failures/unverified; no runtime correction or
  expected-failure addition. Full CLI harness: 37 passed, three optional skips
  (40 discovered). The affected independent-oracle/replay test also passes
  after the final constructor adjustment. Erlang formatting, Python 3.9
  grammar and whitespace checks pass.
- Assembly exposed a constant-folded initial improper-tail constructor. Its
  final opaque `lists:reverse/2` construction is present in saved BEAM, and
  was rechecked separately in all three modes. `final-input.json` is the final
  70-recipe selection; `allocated-tail-*` records that one-case recheck.
- All 6,709 earlier recipes and metadata are unchanged: total 6,779,
  attribution still 373/478 sites in 28 files; fixture counts unchanged.
  This batch does not rerun the full runtime corpus, fresh Lean laws or embedded
  targets, and makes no claim of guaranteed heap movement or all GC schedules.

Local evidence: `.amp/in/artifacts/lynx-structural-paths/` contains saved input,
per-mode manifests/results, toolchain, independent CLI and harness logs,
`beam-paths.txt`, `beam-gc-roots.txt` and regenerated inventory.

## Custom map-iterator ordering and consumers

Add 240 observations (2026-11-26), extending the supported-behavior audit rather
than adding APIs or Lean proofs. Runtime correction and standalone `test_maps`
regressions are kept outside the Lynx harness for independent cherry-picking.

- 204 consumer observations use `next`, `fold`, `map`, `filter`, `filtermap` and
  `foreach`, two deterministic total orders, sizes 0/1/31/32/33, and fresh,
  partially consumed, exhausted and explicit-`none` iterators. Retain exact
  ordered key/value visits and operation results; `filtermap` exercises keep,
  drop and transform branches. Do not assert sorting-comparator call order or
  count, unspecified key ties, or private iterator layouts.
- Ten snapshot cases consume retained iterators after updating/removing map
  bindings, comparing both original and changed maps in the requested order.
- Twelve invalid-order cases cover a bad atom and functions of arity 0/1/3
  over empty/singleton/multi-entry maps. Five non-map cases cover valid and
  invalid orders. Nine comparator-exception cases cover throw/error/exit,
  including empty/singleton controls where comparison must not occur.
- The independent CLI oracle derives expected visits and results with Elixir
  list operations and map construction, not the Erlang observation helper.
  Filtered, reordered replay checks eight cases with reindexed module names.
- The new Erlang regression passes on OTP 29.1.3 and fails on unchanged AtomVM.
  Fix `maps:iterator/2` to reject invalid arguments with `badarg`, retaining
  `maps:iterator/1`'s `{badmap, Map}` behavior. The full `test_maps:test/0` passes
  after the correction in interpreter, AArch64 precompiled and on-demand JIT
  modes, and on OTP 26.0.2, 27.3.4.12 and 29.1.3.
- Full CLI harness: 37 passed, three optional skips (40 discovered). This
  includes the independent iterator oracle and reordered/reindexed replay.
  Erlang formatting, Python 3.9 grammar and whitespace checks pass.
- All 240 new recipes match OTP 29.1.3 in interpreter, AArch64 precompiled and
  on-demand JIT modes, without expected-failure additions. All 6,469 preceding
  recipes and metadata remain unchanged: total 6,709; attribution remains
  373/478 sites in 28 files. Fixture recipes and API counts are unchanged. This
  is not a rerun of the entire runtime corpus or fresh Lean laws.

Local evidence: `.amp/in/artifacts/lynx-iterators/`, including before/after
regression logs, saved input, per-mode manifests/results and harness logs.

## Asynchronous exit signals

Add 56 OTP-29-gated observations (2026-11-26), distinct from legacy helpers that
call `exit/2`. Runtime support and standalone `test_exit2` regressions are kept
outside the Lynx harness so they can be cherry-picked independently.

- Self, PID, active alias, reply alias, inactive alias and plain-reference targets
  cover normal, compound partial-bitstring and kill reasons with both trap
  settings. Dead and invalid destinations provide controls. Before/EXIT/after
  observations check exact sender order; reply-before-exit and exit-before-reply
  distinguish alias deactivation order. An exit does not consume a reply alias.
- Unlike `exit(self(), normal)`, an untrapping self-target ignores normal and
  returns true. Fatal self-signals run in monitored workers and remain
  uncatchable. The standalone regression also checks AtomVM echo-port normal,
  kill and dead-target behavior; the port branch is not an OTP comparison.
- The receiver validates aliases and normalizes explicit exit signals while
  splitting its mailbox. Trapped exits become ordered messages; fatal exits
  remain signals so earlier requests can run before termination. No sender
  reads another process's alias table. Legacy `exit/2` is unchanged.
- Reference execution checks `minimum_otp` before starting workers, preventing
  nested isolated `undef` from becoming a false match on older OTP. The public
  CLI test independently derives all expected observations, exercises filtered
  and reindexed replay, and forces the gate with an unavailable OTP version.
- All 6,413 earlier recipes and metadata remain unchanged. Total: 6,469;
  attribution: 373/478 sites in 28 files. Alias cases have no Lean attribution:
  Lynx explicitly does not model alias exits. These are finite observations,
  not universal scheduler or theorem coverage.
- Against OTP 29.1.3 / ERTS 17.1.2, interpreter, AArch64 precompiled and
  on-demand JIT each match 56/56, with zero failures or unverified results.
  JIT uses `--timeout 60 --jobs 2`. The standalone Erlang regression passes on
  OTP and all three AtomVM modes, including a reference-counted binary reason.
  The full interpreter Erlang core corpus passes, as do rebuilt mailbox, heap,
  struct and ENIF tests. Full CLI harness: 36 passed, 3 optional skips (39
  discovered, 323.367 seconds). Erlang formatting, changed C-function formatting,
  Python 3.9 grammar and whitespace checks pass.
- Local processes, ports and aliases are supported. Distribution and the
  priority options of `exit_signal/3` are outside scope. The full Lynx runtime
  corpus, embedded targets and fresh Lean laws were not rerun for this batch.
  Evidence: `.amp/in/artifacts/exit-signal/` and
  `.amp/in/artifacts/lynx-exit-signal/`.

## Fan-out termination and ending-process requests

Add 18 observations (2026-11-26), inspired by `link_exits_in_any_order` and
`queued_requests_answered_one_at_a_time` in upstream `Modules/Signals.lean`
(commit `d526582fef4373b64474b5794050c64ba76d5ff3`). Each scenario uses 1, 4 or
16 peers; no production runtime changes or expected-failure entries are added.

- Fan-out peers install links and monitors before termination. The target sends
  distinct tagged payloads to the root and peers, then exits normally, with a
  compound shutdown reason, or through untrappable self-kill. Every recipient
  must observe its message, EXIT and DOWN in that order with exact payloads and
  reasons. Collect these events in one receive rather than selective receives
  that could hide reordering. The root waits for one peer's report before
  polling its own notifications; either early presence or absence is accepted.
  There is no comparison of cross-recipient arrival order.
- Concurrent requesters call link, monitor, process_info and is_process_alive.
  The live control holds the target until all requesters report their queries;
  the dead control waits for target DOWN before releasing them. These require
  exact live/normal and dead/noproc answers. The racing variant has no such
  completion fence and accepts live-to-dead transitions, never resurrection,
  and normal/normal, normal/noproc or noproc/noproc link/monitor reason pairs.
- Established monitors' normal DOWNs must follow link EXITs. A late monitor's
  immediate noproc reply is not the same termination notification: its order
  relative to a link EXIT is intentionally unconstrained in the racing case.
  An initial overly strict probe rejected DOWN(noproc) before EXIT(normal) in
  one interpreter repetition; this is retained as test-design evidence, not
  diagnosed as a runtime bug or converted into an expected failure.
- Require eventual completion and dead-target queries from every peer, preserve
  unrelated sentinels, reject extra notifications, consume worker monitors and
  normalize PIDs/references. No sleeps or internal short deadlines are used;
  the outer runner bounds hangs. Accepted race variants become `allowed` only
  after checking observations; unexpected values retain diagnostic payloads.
  The public CLI test independently checks exact result shapes and per-peer
  payloads, including a filtered/reindexed saved replay.
- OTP 29.1.3 / ERTS 17.1.2 comparison: ten repetitions of all 18 cases match
  180/180 in interpreter and AArch64 precompiled modes; on-demand JIT matches
  18/18 with `--timeout 60 --jobs 2`. All have zero failures and unverified
  results. The independent-oracle/reindexed-replay test passes. Full harness:
  35 passed, 3 optional environment-gated skips (38 discovered, 306.526 seconds).
  Erlang formatting, Python 3.9 grammar and whitespace checks pass.
- All 6,395 earlier recipes and metadata are unchanged. Total recipes: 6,413;
  attribution grows from 369 to 371/478 sites in 28 files. These are finite
  public-behavior samples, not universal theorem instances or proof that every
  allowed race outcome occurred. Evidence: `.amp/in/artifacts/lynx-fanout/`.
  The full runtime corpus, embedded targets and fresh Lean laws are not rerun.

## Nested fallthrough continuations

Add 32 observations (2026-11-26): 16 annotated-Core probes and 16 ordinary-source
controls, inspired by upstream's `preserves compiler fallthrough labels and
their jump sites` and `preserves nested compiler labels with immediate jumps`
tests (upstream commit `4e19bfc408c7fb9f8b962f400c16e8a8be0e49e3`). Reuse the
existing Core compiler/replay pipeline and ordered-effect observer; no runtime
helpers or theorem attributions are added.

- Two nested nonrecursive Core `letrec` labels carry `letrec_goto` annotations.
  Several failed alternatives share the inner continuation, which records one
  effect and transfers to the shared outer fallback. An outer pattern miss
  bypasses the inner continuation. Successful branches and fallbacks converge
  at an effectful join, returning the original capture, an earlier saved value
  and the branch result, all kept live across calls.
- Distinguish guard false from a failing `element/2` guard, later-clause success
  after a guard error, nested/outer pattern misses, and unguarded success.
  Exceptions in each selected body, the inner/outer continuations, input/saved
  evaluation or final join must propagate with the exact reason and trace;
  they are never interpreted as another failed pattern/guard.
- The public CLI test independently specifies all branch payloads and ordered
  once-only effects, including suppressed bodies/continuations after failure.
  It also replays a filtered/reindexed mixture of saved Core/source cases.
- Saved assembly confirms live guard failure targets, shared Core fallback
  blocks, branch jumps to the common join and values retained in stack slots
  across calls. Ordinary source duplicates the outer fallback call sites in
  the representative inspected case; it still reaches one common result join.
  Compare behavior, not identical compiler layouts or preserved label names.
  This validates compiled BEAM execution, not Lynx's direct Core translator.
- OTP 29.1.3 / ERTS 17.1.2 comparison: interpreter, AArch64 precompiled and
  on-demand JIT each match 32/32, with zero failures or unverified results.
  On-demand JIT uses `--timeout 60 --jobs 2` for support-module compilation.
  Full harness: 34 passed, 3 optional environment-gated skips (37 discovered,
  294.003 seconds). Python 3.9 grammar and whitespace checks pass.
- All 6,363 earlier recipes and metadata are unchanged. Total recipes: 6,395;
  attribution stays 369/478 sites in 28 Lean files. No runtime fix or
  expected-failure entry is added. Evidence is under
  `.amp/in/artifacts/lynx-fallthrough/`: input, toolchain, per-mode results/build
  hashes, inventory, representative assembly and harness log. Full runtime
  corpus and embedded execution are outside this batch.

## Escaping recursive closures

Add 24 observations (2026-11-26), inspired by upstream's `escaping local
functions preserve captures, generated arguments, and self identity` compiler
test and `passes captures explicitly through direct local recursion`. Unlike
existing capture-free countdown/self-reference tests and nonrecursive identity
probes, combine all three behaviors in escaped named recursive closures.

- Two factories return tail-recursive and non-tail-recursive closures, each with
  two runtime-supplied captures wrapped in dynamically constructed tagged
  tuples. Profiles cover empty input, exact integer/float capture preservation,
  and nested maps/lists/partial-bitstrings with three asymmetric input elements.
- Four lifetime paths: local without explicit GC, explicit GC, actual mailbox
  transfer to a monitored worker followed by GC, and return from a creator
  process whose normal `DOWN` is consumed before invocation. GC runs before the
  first invocation and again with the first result and returned self reference
  live. The returned reference is invoked with different input/accumulator
  arguments; identity is compared only within the executing VM.
- Independent public CLI oracles check exact captured data, both accumulated
  results, two self-equality booleans and ordered descent/base/unwind effects.
  Tail accumulation and non-tail reconstruction have deliberately different
  result orders. A filtered/reindexed saved-input replay covers both factories
  and creator-exit/local execution. No raw functions, PIDs or generated function
  names escape into cross-runtime expectations.
- Assembly inspection confirms real two-capture `make_fun3`, indirect
  `call_fun`, both GC calls, tail `call_last` and non-tail recursive `call`
  followed by effectful unwinding. The source-level `apply/2` second invocation
  optimizes to `call_fun` too; this is not distinct apply-opcode coverage or
  a test of Lynx's translator/proofs.
- Interpreter (`build`), AArch64 precompiled (`.amp/in/build-native`) and
  on-demand JIT (`.amp/in/build-on-demand`) each match 24/24 against OTP 29.1.3
  / ERTS 17.1.2, with zero failures/unverified. JIT uses `--timeout 60 --jobs 2`
  for support-module compilation. Full harness: 33 passed, 3 optional
  environment-gated skips (36 discovered), 293.678 seconds. Erlang formatting,
  Python 3.9 grammar and `git diff --check` pass.
- All 6,339 earlier recipes and metadata are unchanged. Total recipes: 6,363;
  attribution stays 369/478 sites in 28 Lean files. No runtime fixes or
  expected-failure entries are added. Evidence is under
  `.amp/in/artifacts/lynx-recursive-closures/`: saved input, toolchain,
  per-mode results/build hashes, inventory, assembly and harness log.
  Full runtime corpus and embedded execution are outside this batch.

## Core try-result binding and exception boundaries

Add 56 observations (2026-11-26), inspired by upstream's `translates zero and
multiple Core try values` compiler test: 36 raw-Core probes and 20 source
controls. Reuse the normal Core companion compilation/replay pipeline and the
existing ordered-effect observer. No runtime helpers, fixes, expected-failure
entries or Lean theorem attributions are added.

- Bind genuine `<>` and `<A,B>` results in Core `try`, distinguishing these from
  source `try ok` and `try {A,B}`, each of which produces one result. Opaque
  effectful calls supply asymmetric atom/integer values; success swaps them.
- Transfer zero/two results out of a `try` through `let`, multi-value `case`
  and nested `try`, on both successful and handled-exception paths. Normalize
  `case` as `let <X,Y> = try ... in case <X,Y> ...`: a direct multi-result
  `try` scrutinee hits OTP 29.1.3's `sys_core_fold:case_opt/3` assumption that
  a non-values scrutinee returns one value. Keep normal compiler optimizations
  enabled. Pair patterns and an opaque guard keep representative matches live.
- Exercise `error`, `throw` and `exit` in protected computations, success bodies
  and handlers. Only protected exceptions enter that `try`'s handler; body and
  handler exceptions escape. Nested probes distinguish inner handling from
  propagation to the outer handler using separate payloads and effect tags.
- The public CLI test independently specifies every value, class/reason and
  once-only ordered trace, and replays a reindexed mixture of saved zero/pair
  Core and source probes. These validate compiled BEAM on AtomVM, not Lynx's
  direct raw-Core translator or model proofs. Saved assembly confirms live
  `try`/`try_end`/`try_case`, preserved pair values and nested handler scopes.
- Interpreter (`build`), AArch64 precompiled (`.amp/in/build-native`) and
  on-demand JIT (`.amp/in/build-on-demand`) each match 56/56 against OTP 29.1.3
  / ERTS 17.1.2, with zero failures/unverified. JIT uses `--timeout 60 --jobs 2`
  to allow support-module compilation. Full harness: 32 passed, 3 optional
  environment-gated skips (35 discovered), 270.206 seconds. Python 3.9 grammar
  and `git diff --check` pass.
- All 6,283 earlier recipes and metadata are unchanged. Total recipes: 6,339;
  attribution stays 369/478 sites in 28 Lean files. Evidence is under
  `.amp/in/artifacts/lynx-core-try/`: input, toolchain, per-mode results/build
  hashes, representative assembly, inventory and harness log. Full runtime
  corpus and embedded execution are outside this batch.

## Binary-construction ordering and exceptions

Add 56 observations (2026-11-26): 28 scenarios each in Erlang source and raw
Core, inspired by Lynx's `binaries nested in arguments are constructed before
the call` compiler test. Reuse the existing Core compiler/replay pipeline and
`core_order/1`/`core_step/3` observer; no runtime changes or new theorem claims.

- Compare exact constructed bitstrings, exception classes/reasons and ordered
  effects. Distinguish segment value/size errors from constructor rejection,
  and invalid early segments from later explicit errors. Include asymmetric
  3+5-bit construction, zero-width integer type checking, short binary inputs,
  invalid UTF scalars, aligned/unaligned appends and retained prefix aliases.
- Nest construction inside calls, tuples and cons heads. In the source control,
  a bad head constructor raises before the tail runs. In raw Core its segment
  effects precede the tail, but final construction follows it: a tail exception
  wins, or `badarg` occurs after the tail effect. Do not impose either trace on
  both paths. Saved assembly confirms that `bs_create_bin` placement and the
  append form; this is not a claim about every legacy/private-append opcode.
- The public CLI test has handwritten expected values and traces for all 56
  observations, including path-specific exception precedence. Differential
  agreement alone is insufficient; every freshly generated OTP oracle is also
  checked against these independent expectations.
- Interpreter (`build`), AArch64 precompiled (`.amp/in/build-native`) and
  on-demand JIT (`.amp/in/build-on-demand`) each match 56/56 against OTP 29.1.3
  / ERTS 17.1.2, with zero failures/unverified. The JIT replay uses
  `--timeout 60 --jobs 2` to allow support-module compilation. No runtime fix
  or expected-failure manifest change was needed. Full harness: 31 passed,
  3 optional environment-gated skips (34 discovered). Python 3.9 grammar and
  `git diff --check` pass.
- All 6,227 earlier recipes and metadata are unchanged. Total recipes: 6,283;
  attribution stays 369/478 sites in 28 Lean files. Evidence is under
  `.amp/in/artifacts/lynx-binary-order/`: saved input, toolchain, per-mode
  references/results/build hashes and representative source/Core assembly.
  Full runtime corpus and embedded execution were not rerun for this batch.

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
