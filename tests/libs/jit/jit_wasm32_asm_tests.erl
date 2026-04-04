%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(jit_wasm32_asm_tests).

-include_lib("eunit/include/eunit.hrl").

%% Cross-validate Bin against a WAT fragment using wat2wasm when available,
%% then assert Value equals Bin.
%% The WAT fragment is placed inside (module (memory 1) (func WATSTR )) and
%% compiled with wat2wasm. The trailing implicit end (0x0b) is stripped so Bin
%% contains only the instruction bytes, matching what jit_wasm32_asm produces.
-define(_assertAsmEqual(Bin, WatStr, Value),
    ?_assertEqual(jit_tests_common:asm(wasm32, Bin, WatStr), Value)
).

%%=============================================================================
%% LEB128 encoding tests
%%=============================================================================

encode_uleb128_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm32_asm:encode_uleb128(0)),
        ?_assertEqual(<<1>>, jit_wasm32_asm:encode_uleb128(1)),
        ?_assertEqual(<<127>>, jit_wasm32_asm:encode_uleb128(127)),
        ?_assertEqual(<<128, 1>>, jit_wasm32_asm:encode_uleb128(128)),
        ?_assertEqual(<<98>>, jit_wasm32_asm:encode_uleb128(98)),
        ?_assertEqual(<<255, 1>>, jit_wasm32_asm:encode_uleb128(255)),
        ?_assertEqual(<<128, 2>>, jit_wasm32_asm:encode_uleb128(256)),
        ?_assertEqual(<<232, 7>>, jit_wasm32_asm:encode_uleb128(1000)),
        ?_assertEqual(<<128, 128, 4>>, jit_wasm32_asm:encode_uleb128(65536))
    ].

encode_sleb128_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm32_asm:encode_sleb128(0)),
        ?_assertEqual(<<1>>, jit_wasm32_asm:encode_sleb128(1)),
        ?_assertEqual(<<63>>, jit_wasm32_asm:encode_sleb128(63)),
        ?_assertEqual(<<192, 0>>, jit_wasm32_asm:encode_sleb128(64)),
        ?_assertEqual(<<127>>, jit_wasm32_asm:encode_sleb128(-1)),
        ?_assertEqual(<<65>>, jit_wasm32_asm:encode_sleb128(-63)),
        ?_assertEqual(<<64>>, jit_wasm32_asm:encode_sleb128(-64)),
        ?_assertEqual(<<191, 127>>, jit_wasm32_asm:encode_sleb128(-65)),
        ?_assertEqual(<<128, 127>>, jit_wasm32_asm:encode_sleb128(-128))
    ].

%%=============================================================================
%% Value types
%%=============================================================================

type_test_() ->
    [
        ?_assertEqual(<<16#7F>>, jit_wasm32_asm:type_i32()),
        ?_assertEqual(<<16#7E>>, jit_wasm32_asm:type_i64()),
        ?_assertEqual(<<16#7D>>, jit_wasm32_asm:type_f32()),
        ?_assertEqual(<<16#7C>>, jit_wasm32_asm:type_f64()),
        ?_assertEqual(<<16#70>>, jit_wasm32_asm:type_funcref()),
        ?_assertEqual(<<16#6F>>, jit_wasm32_asm:type_externref())
    ].

blocktype_test_() ->
    [
        ?_assertEqual(<<16#40>>, jit_wasm32_asm:blocktype_void()),
        ?_assertEqual(<<16#7F>>, jit_wasm32_asm:blocktype_i32())
    ].

%%=============================================================================
%% Control flow instructions
%%=============================================================================

control_flow_test_() ->
    [
        %% Self-contained in a void function — cross-validate with wat2wasm.
        ?_assertAsmEqual(<<16#00>>, "    (unreachable)", jit_wasm32_asm:unreachable()),
        ?_assertAsmEqual(<<16#01>>, "    (nop)", jit_wasm32_asm:nop()),
        ?_assertAsmEqual(<<16#0F>>, "    (return)", jit_wasm32_asm:return()),
        %% Structured-control opcodes and end/else are encoding constants — no WAT needed.
        ?_assertEqual(<<16#02, 16#40>>, jit_wasm32_asm:block(jit_wasm32_asm:blocktype_void())),
        ?_assertEqual(<<16#02, 16#7F>>, jit_wasm32_asm:block(jit_wasm32_asm:blocktype_i32())),
        ?_assertEqual(<<16#03, 16#40>>, jit_wasm32_asm:loop(jit_wasm32_asm:blocktype_void())),
        ?_assertEqual(<<16#04, 16#40>>, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
        ?_assertEqual(<<16#05>>, jit_wasm32_asm:else_()),
        ?_assertEqual(<<16#0B>>, jit_wasm32_asm:end_())
    ].

br_test_() ->
    [
        %% br 0 exits the current function block — valid in a void function.
        ?_assertAsmEqual(<<16#0C, 0>>, "    (br 0)", jit_wasm32_asm:br(0)),
        %% br with depth > 0 needs nested blocks; validate encoding only.
        ?_assertEqual(<<16#0C, 1>>, jit_wasm32_asm:br(1)),
        ?_assertEqual(<<16#0C, 5>>, jit_wasm32_asm:br(5))
    ].

br_if_test_() ->
    [
        ?_assertEqual(<<16#0D, 0>>, jit_wasm32_asm:br_if(0)),
        ?_assertEqual(<<16#0D, 1>>, jit_wasm32_asm:br_if(1)),
        ?_assertEqual(<<16#0D, 3>>, jit_wasm32_asm:br_if(3))
    ].

br_table_test_() ->
    [
        %% br_table with 2 labels [0, 1] and default 2
        ?_assertEqual(
            <<16#0E, 2, 0, 1, 2>>,
            jit_wasm32_asm:br_table([0, 1], 2)
        ),
        %% br_table with 0 labels, default 0
        ?_assertEqual(
            <<16#0E, 0, 0>>,
            jit_wasm32_asm:br_table([], 0)
        )
    ].

call_test_() ->
    [
        ?_assertEqual(<<16#10, 0>>, jit_wasm32_asm:call(0)),
        ?_assertEqual(<<16#10, 1>>, jit_wasm32_asm:call(1)),
        ?_assertEqual(<<16#10, 128, 1>>, jit_wasm32_asm:call(128))
    ].

call_indirect_test_() ->
    [
        ?_assertEqual(<<16#11, 0, 0>>, jit_wasm32_asm:call_indirect(0, 0)),
        ?_assertEqual(<<16#11, 1, 0>>, jit_wasm32_asm:call_indirect(1, 0))
    ].

%%=============================================================================
%% Variable instructions
%%=============================================================================

variable_test_() ->
    Asm = jit_wasm32_asm,
    [
        %% local.get: self-contained (returns the param) — validate with wat2wasm.
        ?_assertAsmEqual(
            <<16#20, 0>>,
            "    (param i32) (result i32)\n    (local.get 0)",
            Asm:local_get(0)
        ),
        ?_assertAsmEqual(
            <<16#20, 3>>,
            "    (param i32 i32 i32 i32) (result i32)\n    (local.get 3)",
            Asm:local_get(3)
        ),
        %% local.set: needs a value on the stack; test as (i32.const 0)(local.set N) sequence.
        ?_assertAsmEqual(
            <<16#41, 0, 16#21, 0>>,
            "    (param i32)\n    (i32.const 0)\n    (local.set 0)",
            <<(Asm:i32_const(0))/binary, (Asm:local_set(0))/binary>>
        ),
        ?_assertAsmEqual(
            <<16#41, 0, 16#21, 5>>,
            "    (param i32 i32 i32 i32 i32 i32)\n    (i32.const 0)\n    (local.set 5)",
            <<(Asm:i32_const(0))/binary, (Asm:local_set(5))/binary>>
        ),
        %% local.tee: consumes and re-pushes; test as (local.tee N (i32.const 0)) sequence.
        ?_assertAsmEqual(
            <<16#41, 0, 16#22, 0>>,
            "    (param i32) (result i32)\n    (local.tee 0 (i32.const 0))",
            <<(Asm:i32_const(0))/binary, (Asm:local_tee(0))/binary>>
        ),
        ?_assertAsmEqual(
            <<16#41, 0, 16#22, 2>>,
            "    (param i32 i32 i32) (result i32)\n    (local.tee 2 (i32.const 0))",
            <<(Asm:i32_const(0))/binary, (Asm:local_tee(2))/binary>>
        ),
        %% global.get/set need global declarations in the module — validate encoding only.
        ?_assertEqual(<<16#23, 0>>, Asm:global_get(0)),
        ?_assertEqual(<<16#24, 1>>, Asm:global_set(1))
    ].

%%=============================================================================
%% Memory instructions
%%=============================================================================

memory_test_() ->
    %% Load/store instructions need an address; we use (param i32)(local.get 0) as the
    %% address arg, so Bin includes the local.get 0 prefix and Value is the combined sequence.
    %% Stores also need a value arg; we use (i32.const 0) for that.
    %% Default WAT alignment matches the align field stored in the binary:
    %%   i32.load / i32.store  → log2(4) = 2
    %%   i32.load16 / i32.store16 → log2(2) = 1
    %%   i32.load8  / i32.store8  → log2(1) = 0
    Asm = jit_wasm32_asm,
    [
        %% i32.load align=2 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#28, 2, 0>>,
            "    (param i32) (result i32)\n    (i32.load (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load(2, 0))/binary>>
        ),
        %% i32.load align=2 offset=40
        ?_assertAsmEqual(
            <<16#20, 0, 16#28, 2, 40>>,
            "    (param i32) (result i32)\n    (i32.load offset=40 (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load(2, 40))/binary>>
        ),
        %% i32.store align=2 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#41, 0, 16#36, 2, 0>>,
            "    (param i32)\n    (i32.store (local.get 0) (i32.const 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_const(0))/binary, (Asm:i32_store(2, 0))/binary>>
        ),
        %% i32.store align=2 offset=112
        ?_assertAsmEqual(
            <<16#20, 0, 16#41, 0, 16#36, 2, 112>>,
            "    (param i32)\n    (i32.store offset=112 (local.get 0) (i32.const 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_const(0))/binary, (Asm:i32_store(2, 112))/binary>>
        ),
        %% i32.load8_s align=0 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#2C, 0, 0>>,
            "    (param i32) (result i32)\n    (i32.load8_s (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load8_s(0, 0))/binary>>
        ),
        %% i32.load8_u align=0 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#2D, 0, 0>>,
            "    (param i32) (result i32)\n    (i32.load8_u (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load8_u(0, 0))/binary>>
        ),
        %% i32.load16_s align=1 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#2E, 1, 0>>,
            "    (param i32) (result i32)\n    (i32.load16_s (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load16_s(1, 0))/binary>>
        ),
        %% i32.load16_u align=1 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#2F, 1, 0>>,
            "    (param i32) (result i32)\n    (i32.load16_u (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load16_u(1, 0))/binary>>
        ),
        %% i32.store8 align=0 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#41, 0, 16#3A, 0, 0>>,
            "    (param i32)\n    (i32.store8 (local.get 0) (i32.const 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_const(0))/binary, (Asm:i32_store8(0, 0))/binary>>
        ),
        %% i32.store16 align=1 offset=0
        ?_assertAsmEqual(
            <<16#20, 0, 16#41, 0, 16#3B, 1, 0>>,
            "    (param i32)\n    (i32.store16 (local.get 0) (i32.const 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_const(0))/binary, (Asm:i32_store16(1, 0))/binary>>
        )
    ].

memory_large_offset_test_() ->
    Asm = jit_wasm32_asm,
    [
        %% offset=256 requires a two-byte ULEB128 (<<128, 2>>); cross-validate with wat2wasm.
        ?_assertAsmEqual(
            <<16#20, 0, 16#28, 2, 128, 2>>,
            "    (param i32) (result i32)\n    (i32.load offset=256 (local.get 0))",
            <<(Asm:local_get(0))/binary, (Asm:i32_load(2, 256))/binary>>
        )
    ].

%%=============================================================================
%% Numeric instructions - constants
%%=============================================================================

const_test_() ->
    %% i32.const/i64.const use signed LEB128 (SLEB128) encoding; cross-validate with
    %% wat2wasm to confirm our encode_sleb128 matches the spec for these values.
    Asm = jit_wasm32_asm,
    [
        ?_assertAsmEqual(<<16#41, 0>>, "    (result i32)\n    (i32.const 0)", Asm:i32_const(0)),
        ?_assertAsmEqual(<<16#41, 1>>, "    (result i32)\n    (i32.const 1)", Asm:i32_const(1)),
        ?_assertAsmEqual(<<16#41, 127>>, "    (result i32)\n    (i32.const -1)", Asm:i32_const(-1)),
        %% 64 requires two SLEB128 bytes (<<0xC0, 0x00>>)
        ?_assertAsmEqual(
            <<16#41, 192, 0>>, "    (result i32)\n    (i32.const 64)", Asm:i32_const(64)
        ),
        %% -65 requires two SLEB128 bytes (<<0xBF, 0x7F>>)
        ?_assertAsmEqual(
            <<16#41, 191, 127>>, "    (result i32)\n    (i32.const -65)", Asm:i32_const(-65)
        ),
        ?_assertAsmEqual(<<16#42, 0>>, "    (result i64)\n    (i64.const 0)", Asm:i64_const(0)),
        ?_assertAsmEqual(<<16#42, 1>>, "    (result i64)\n    (i64.const 1)", Asm:i64_const(1)),
        ?_assertAsmEqual(<<16#42, 127>>, "    (result i64)\n    (i64.const -1)", Asm:i64_const(-1))
    ].

%%=============================================================================
%% Numeric instructions - i32 comparison
%%=============================================================================

i32_comparison_test_() ->
    [
        ?_assertEqual(<<16#45>>, jit_wasm32_asm:i32_eqz()),
        ?_assertEqual(<<16#46>>, jit_wasm32_asm:i32_eq()),
        ?_assertEqual(<<16#47>>, jit_wasm32_asm:i32_ne()),
        ?_assertEqual(<<16#48>>, jit_wasm32_asm:i32_lt_s()),
        ?_assertEqual(<<16#49>>, jit_wasm32_asm:i32_lt_u()),
        ?_assertEqual(<<16#4A>>, jit_wasm32_asm:i32_gt_s()),
        ?_assertEqual(<<16#4B>>, jit_wasm32_asm:i32_gt_u()),
        ?_assertEqual(<<16#4C>>, jit_wasm32_asm:i32_le_s()),
        ?_assertEqual(<<16#4D>>, jit_wasm32_asm:i32_le_u()),
        ?_assertEqual(<<16#4E>>, jit_wasm32_asm:i32_ge_s()),
        ?_assertEqual(<<16#4F>>, jit_wasm32_asm:i32_ge_u())
    ].

%%=============================================================================
%% Numeric instructions - i32 arithmetic and bitwise
%%=============================================================================

i32_arithmetic_test_() ->
    [
        ?_assertEqual(<<16#67>>, jit_wasm32_asm:i32_clz()),
        ?_assertEqual(<<16#68>>, jit_wasm32_asm:i32_ctz()),
        ?_assertEqual(<<16#6A>>, jit_wasm32_asm:i32_add()),
        ?_assertEqual(<<16#6B>>, jit_wasm32_asm:i32_sub()),
        ?_assertEqual(<<16#6C>>, jit_wasm32_asm:i32_mul()),
        ?_assertEqual(<<16#6D>>, jit_wasm32_asm:i32_div_s()),
        ?_assertEqual(<<16#6E>>, jit_wasm32_asm:i32_div_u()),
        ?_assertEqual(<<16#6F>>, jit_wasm32_asm:i32_rem_s()),
        ?_assertEqual(<<16#70>>, jit_wasm32_asm:i32_rem_u()),
        ?_assertEqual(<<16#71>>, jit_wasm32_asm:i32_and()),
        ?_assertEqual(<<16#72>>, jit_wasm32_asm:i32_or()),
        ?_assertEqual(<<16#73>>, jit_wasm32_asm:i32_xor()),
        ?_assertEqual(<<16#74>>, jit_wasm32_asm:i32_shl()),
        ?_assertEqual(<<16#75>>, jit_wasm32_asm:i32_shr_s()),
        ?_assertEqual(<<16#76>>, jit_wasm32_asm:i32_shr_u())
    ].

%%=============================================================================
%% Module structure encoding
%%=============================================================================

wasm_magic_version_test_() ->
    [
        ?_assertEqual(<<16#00, 16#61, 16#73, 16#6D>>, jit_wasm32_asm:wasm_magic()),
        ?_assertEqual(<<16#01, 16#00, 16#00, 16#00>>, jit_wasm32_asm:wasm_version())
    ].

encode_name_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm32_asm:encode_name("")),
        ?_assertEqual(<<2, $f, $0>>, jit_wasm32_asm:encode_name("f0")),
        ?_assertEqual(<<3, $e, $n, $v>>, jit_wasm32_asm:encode_name("env")),
        ?_assertEqual(<<6, $m, $e, $m, $o, $r, $y>>, jit_wasm32_asm:encode_name("memory"))
    ].

encode_vector_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm32_asm:encode_vector([])),
        ?_assertEqual(<<1, 42>>, jit_wasm32_asm:encode_vector([<<42>>])),
        ?_assertEqual(<<2, 1, 2>>, jit_wasm32_asm:encode_vector([<<1>>, <<2>>]))
    ].

encode_func_type_test_() ->
    [
        %% () -> ()
        ?_assertEqual(<<16#60, 0, 0>>, jit_wasm32_asm:encode_func_type([], [])),
        %% (i32, i32, i32) -> i32
        ?_assertEqual(
            <<16#60, 3, 16#7F, 16#7F, 16#7F, 1, 16#7F>>,
            jit_wasm32_asm:encode_func_type(
                [jit_wasm32_asm:type_i32(), jit_wasm32_asm:type_i32(), jit_wasm32_asm:type_i32()],
                [jit_wasm32_asm:type_i32()]
            )
        )
    ].

encode_section_test_() ->
    [
        %% Section with ID 1 and content <<1, 2, 3>>
        ?_assertEqual(<<1, 3, 1, 2, 3>>, jit_wasm32_asm:encode_section(1, <<1, 2, 3>>)),
        %% Empty section
        ?_assertEqual(<<7, 0>>, jit_wasm32_asm:encode_section(7, <<>>))
    ].

encode_func_body_no_locals_test() ->
    %% Body with no locals and a simple return
    %% locals_count=0, local_get(0), end
    Expr = <<(jit_wasm32_asm:local_get(0))/binary, (jit_wasm32_asm:end_())/binary>>,
    Body = jit_wasm32_asm:encode_func_body([], Expr),
    %% Size prefix + 0 locals + local_get(0) + end
    ?assertEqual(<<4, 0, 16#20, 0, 16#0B>>, Body).

encode_func_body_with_locals_test() ->
    %% Body with 8 i32 locals
    Expr = <<(jit_wasm32_asm:local_get(0))/binary, (jit_wasm32_asm:end_())/binary>>,
    Body = jit_wasm32_asm:encode_func_body([{8, jit_wasm32_asm:type_i32()}], Expr),
    %% Size prefix + 1 local_decl + (8 i32) + local_get(0) + end
    ?assertEqual(<<6, 1, 8, 16#7F, 16#20, 0, 16#0B>>, Body).

%%=============================================================================
%% Complete module encoding test
%%=============================================================================

%% Test that we can produce a valid WASM module header
minimal_module_test() ->
    Asm = jit_wasm32_asm,
    %% Build a minimal module: one function that returns its first argument
    Type0 = Asm:encode_func_type(
        [Asm:type_i32(), Asm:type_i32(), Asm:type_i32()],
        [Asm:type_i32()]
    ),
    TypeSection = Asm:encode_type_section([Type0]),
    FunctionSection = Asm:encode_function_section([0]),
    Expr = <<(Asm:local_get(0))/binary, (Asm:end_())/binary>>,
    Body = Asm:encode_func_body([], Expr),
    CodeSection = Asm:encode_code_section([Body]),
    Module = <<
        (Asm:wasm_magic())/binary,
        (Asm:wasm_version())/binary,
        TypeSection/binary,
        FunctionSection/binary,
        CodeSection/binary
    >>,
    %% Verify it starts with the correct magic and version
    ?assertMatch(<<16#00, 16#61, 16#73, 16#6D, 16#01, 16#00, 16#00, 16#00, _/binary>>, Module),
    %% Verify type section (section ID 1)
    <<_Magic:8/binary, 1, _/binary>> = Module,
    ok.
