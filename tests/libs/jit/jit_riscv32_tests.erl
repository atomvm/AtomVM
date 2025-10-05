%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_riscv32_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_riscv32).

% disassembly obtained with:
% arm-elf-objdump -b binary -D dump.bin -M arm

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	00062f83          	lw	t6,0(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	000f80e7          	jalr	t6\n"
            "  1c:	00050f93          	mv	t6,a0\n"
            "  20:	00012083          	lw	ra,0(sp)\n"
            "  24:	00412503          	lw	a0,4(sp)\n"
            "  28:	00812583          	lw	a1,8(sp)\n"
            "  2c:	00c12603          	lw	a2,12(sp)\n"
            "  30:	01010113          	addi	sp,sp,16"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	00462f83          	lw	t6,4(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	000f80e7          	jalr	t6\n"
            "  1c:	00050f93          	mv	t6,a0\n"
            "  20:	00012083          	lw	ra,0(sp)\n"
            "  24:	00412503          	lw	a0,4(sp)\n"
            "  28:	00812583          	lw	a1,8(sp)\n"
            "  2c:	00c12603          	lw	a2,12(sp)\n"
            "  30:	01010113          	addi	sp,sp,16"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	00862f83          	lw	t6,8(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	02a00593          	li	a1,42\n"
            "  1c:	02b00613          	li	a2,43\n"
            "  20:	02c00693          	li	a3,44\n"
            "  24:	000f80e7          	jalr	t6\n"
            "  28:	00050f93          	mv	t6,a0\n"
            "  2c:	00012083          	lw	ra,0(sp)\n"
            "  30:	00412503          	lw	a0,4(sp)\n"
            "  34:	00812583          	lw	a1,8(sp)\n"
            "  38:	00c12603          	lw	a2,12(sp)\n"
            "  3c:	01010113          	addi	sp,sp,16"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	01462f83          	lw	t6,20(a2)\n"
            "   4:	01000613          	li	a2,16\n"
            "   8:	02000693          	li	a3,32\n"
            "   c:	00200713          	li	a4,2\n"
            "  10:	000f8067          	jr	t6"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_6_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get bin_ptr from x_reg 0 (similar to get_list_test pattern)
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:and_(State1, RegA, ?TERM_PRIMARY_CLEAR_MASK),
    % Get another register for the last parameter to test {free, Reg} handling
    {State3, OtherReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    % Call PRIM_BITSTRING_EXTRACT_INTEGER with 6 arguments
    {State4, _ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, RegA}, 64, 8, {free, OtherReg}
    ]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	01852f83          	lw	t6,24(a0)\n"
            "   4:	00300f13          	li	t5,3\n"
            "   8:	ffff4f13          	not	t5,t5\n"
            "   c:	01efffb3          	and	t6,t6,t5\n"
            "  10:	01c52f03          	lw	t5,28(a0)\n"
            "  14:	0b800e93          	li	t4,184\n"
            "  18:	00ce8eb3          	add	t4,t4,a2\n"
            "  1c:	000eae83          	lw	t4,0(t4)\n"
            "  20:	ff010113          	addi	sp,sp,-16\n"
            "  24:	00112023          	sw	ra,0(sp)\n"
            "  28:	00a12223          	sw	a0,4(sp)\n"
            "  2c:	00b12423          	sw	a1,8(sp)\n"
            "  30:	00c12623          	sw	a2,12(sp)\n"
            "  34:	000f8613          	mv	a2,t6\n"
            "  38:	04000693          	li	a3,64\n"
            "  3c:	00800713          	li	a4,8\n"
            "  40:	000f0793          	mv	a5,t5\n"
            "  44:	000e80e7          	jalr	t4\n"
            "  48:	00050e93          	mv	t4,a0\n"
            "  4c:	00012083          	lw	ra,0(sp)\n"
            "  50:	00412503          	lw	a0,4(sp)\n"
            "  54:	00812583          	lw	a1,8(sp)\n"
            "  58:	00c12603          	lw	a2,12(sp)\n"
            "  5c:	01010113          	addi	sp,sp,16"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_extended_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:call_primitive(State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State2, RegB} = ?BACKEND:call_primitive(State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]),
    {State3, RegC} = ?BACKEND:call_primitive(State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State4, ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg, {ptr, RegC}]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	04862f83          	lw	t6,72(a2)\n"
        "   4:	ff010113          	addi	sp,sp,-16\n"
        "   8:	00112023          	sw	ra,0(sp)\n"
        "   c:	00a12223          	sw	a0,4(sp)\n"
        "  10:	00b12423          	sw	a1,8(sp)\n"
        "  14:	00c12623          	sw	a2,12(sp)\n"
        "  18:	01300593          	li	a1,19\n"
        "  1c:	000f80e7          	jalr	t6\n"
        "  20:	00050f93          	mv	t6,a0\n"
        "  24:	00012083          	lw	ra,0(sp)\n"
        "  28:	00412503          	lw	a0,4(sp)\n"
        "  2c:	00812583          	lw	a1,8(sp)\n"
        "  30:	00c12603          	lw	a2,12(sp)\n"
        "  34:	01010113          	addi	sp,sp,16\n"
        "  38:	04862f03          	lw	t5,72(a2)\n"
        "  3c:	fe010113          	addi	sp,sp,-32\n"
        "  40:	00112023          	sw	ra,0(sp)\n"
        "  44:	00a12223          	sw	a0,4(sp)\n"
        "  48:	00b12423          	sw	a1,8(sp)\n"
        "  4c:	00c12623          	sw	a2,12(sp)\n"
        "  50:	01f12823          	sw	t6,16(sp)\n"
        "  54:	01400593          	li	a1,20\n"
        "  58:	000f00e7          	jalr	t5\n"
        "  5c:	00050f13          	mv	t5,a0\n"
        "  60:	00012083          	lw	ra,0(sp)\n"
        "  64:	00412503          	lw	a0,4(sp)\n"
        "  68:	00812583          	lw	a1,8(sp)\n"
        "  6c:	00c12603          	lw	a2,12(sp)\n"
        "  70:	01012f83          	lw	t6,16(sp)\n"
        "  74:	02010113          	addi	sp,sp,32\n"
        "  78:	04862e83          	lw	t4,72(a2)\n"
        "  7c:	fe010113          	addi	sp,sp,-32\n"
        "  80:	00112023          	sw	ra,0(sp)\n"
        "  84:	00a12223          	sw	a0,4(sp)\n"
        "  88:	00b12423          	sw	a1,8(sp)\n"
        "  8c:	00c12623          	sw	a2,12(sp)\n"
        "  90:	01e12823          	sw	t5,16(sp)\n"
        "  94:	01f12a23          	sw	t6,20(sp)\n"
        "  98:	01300593          	li	a1,19\n"
        "  9c:	000e80e7          	jalr	t4\n"
        "  a0:	00050e93          	mv	t4,a0\n"
        "  a4:	00012083          	lw	ra,0(sp)\n"
        "  a8:	00412503          	lw	a0,4(sp)\n"
        "  ac:	00812583          	lw	a1,8(sp)\n"
        "  b0:	00c12603          	lw	a2,12(sp)\n"
        "  b4:	01012f03          	lw	t5,16(sp)\n"
        "  b8:	01412f83          	lw	t6,20(sp)\n"
        "  bc:	02010113          	addi	sp,sp,32\n"
        "  c0:	03462e03          	lw	t3,52(a2)\n"
        "  c4:	fe010113          	addi	sp,sp,-32\n"
        "  c8:	00112023          	sw	ra,0(sp)\n"
        "  cc:	00a12223          	sw	a0,4(sp)\n"
        "  d0:	00b12423          	sw	a1,8(sp)\n"
        "  d4:	00c12623          	sw	a2,12(sp)\n"
        "  d8:	01d12823          	sw	t4,16(sp)\n"
        "  dc:	000fa583          	lw	a1,0(t6)\n"
        "  e0:	000f2603          	lw	a2,0(t5)\n"
        "  e4:	000e00e7          	jalr	t3\n"
        "  e8:	00050e13          	mv	t3,a0\n"
        "  ec:	00012083          	lw	ra,0(sp)\n"
        "  f0:	00412503          	lw	a0,4(sp)\n"
        "  f4:	00812583          	lw	a1,8(sp)\n"
        "  f8:	00c12603          	lw	a2,12(sp)\n"
        "  fc:	01012e83          	lw	t4,16(sp)\n"
        " 100:	02010113          	addi	sp,sp,32\n"
        " 104:	01cea023          	sw	t3,0(t4)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_few_free_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, 2),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, 3),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, 4),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, 5),
    {State6, ResultReg} = ?BACKEND:call_primitive(State5, ?PRIM_BITSTRING_INSERT_INTEGER, [
        t5, t6, {free, t3}, t4, {free, t2}
    ]),
    State7 = ?BACKEND:free_native_registers(State6, [ResultReg, t5, t6, t4]),
    ?BACKEND:assert_all_native_free(State7),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:	00100f93          	li	t6,1\n"
        "   4:	00200f13          	li	t5,2\n"
        "   8:	00300e93          	li	t4,3\n"
        "   c:	00400e13          	li	t3,4\n"
        "  10:	00500393          	li	t2,5\n"
        "  14:	0e400313          	li	t1,228\n"
        "  18:	00c30333          	add	t1,t1,a2\n"
        "  1c:	00032303          	lw	t1,0(t1)\n"
        "  20:	fe010113          	addi	sp,sp,-32\n"
        "  24:	00112023          	sw	ra,0(sp)\n"
        "  28:	00a12223          	sw	a0,4(sp)\n"
        "  2c:	00b12423          	sw	a1,8(sp)\n"
        "  30:	00c12623          	sw	a2,12(sp)\n"
        "  34:	01d12823          	sw	t4,16(sp)\n"
        "  38:	01e12a23          	sw	t5,20(sp)\n"
        "  3c:	01f12c23          	sw	t6,24(sp)\n"
        "  40:	000f0513          	mv	a0,t5\n"
        "  44:	000f8593          	mv	a1,t6\n"
        "  48:	000e0613          	mv	a2,t3\n"
        "  4c:	000e8693          	mv	a3,t4\n"
        "  50:	00038713          	mv	a4,t2\n"
        "  54:	000300e7          	jalr	t1\n"
        "  58:	fea12e23          	sw	a0,-4(sp)\n"
        "  5c:	00012083          	lw	ra,0(sp)\n"
        "  60:	00412503          	lw	a0,4(sp)\n"
        "  64:	00812583          	lw	a1,8(sp)\n"
        "  68:	00c12603          	lw	a2,12(sp)\n"
        "  6c:	01012e83          	lw	t4,16(sp)\n"
        "  70:	01412f03          	lw	t5,20(sp)\n"
        "  74:	01812f83          	lw	t6,24(sp)\n"
        "  78:	02010113          	addi	sp,sp,32"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0085af83          	lw	t6,8(a1)\n"
        "   4:	ffff8f93          	addi	t6,t6,-1\n"
        "   8:	01f5a423          	sw	t6,8(a1)\n"
        "   c:	000f9a63          	bnez	t6,0x20\n"
        "  10:	00000f97          	auipc	t6,0x0\n"
        "  14:	010f8f93          	addi	t6,t6,16\n"
        "  18:	00862f83          	lw	t6,8(a2)\n"
        "  1c:	000f8067          	jr	t6\n"
        "  20:	01062f83          	lw	t6,16(a2)\n"
        "  24:	02400613          	li	a2,36\n"
        "  28:	00200693          	li	a3,2\n"
        "  2c:	00200713          	li	a4,2\n"
        "  30:	fff00793          	li	a5,-1\n"
        "  34:	000f8067          	jr	t6"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, RegA}
    ]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	04c62f03          	lw	t5,76(a2)\n"
        "   8:	00800613          	li	a2,8\n"
        "   c:	2cb00693          	li	a3,715\n"
        "  10:	000f8713          	mv	a4,t6\n"
        "  14:	000f0067          	jr	t5"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0085af83          	lw	t6,8(a1)\n"
        "   4:	ffff8f93          	addi	t6,t6,-1\n"
        "   8:	01f5a423          	sw	t6,8(a1)\n"
        "   c:	000f9a63          	bnez	t6,0x20\n"
        "  10:	00000f97          	auipc	t6,0x0\n"
        "  14:	010f8f93          	addi	t6,t6,16\n"
        "  18:	00862f83          	lw	t6,8(a2)\n"
        "  1c:	000f8067          	jr	t6\n"
        "  20:	01062f83          	lw	t6,16(a2)\n"
        "  24:	02400613          	li	a2,36\n"
        "  28:	00200693          	li	a3,2\n"
        "  2c:	00200713          	li	a4,2\n"
        "  30:	00a00793          	li	a5,10\n"
        "  34:	000f8067          	jr	t6"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	00062f83          	lw	t6,0(a2)\n"
            "   4:	02a00613          	li	a2,42\n"
            "   8:	000f8067          	jr	t6"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

return_if_not_equal_to_ctx_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	05462f83          	lw	t6,84(a2)\n"
                            "   4:	ff010113          	addi	sp,sp,-16\n"
                            "   8:	00112023          	sw	ra,0(sp)\n"
                            "   c:	00a12223          	sw	a0,4(sp)\n"
                            "  10:	00b12423          	sw	a1,8(sp)\n"
                            "  14:	00c12623          	sw	a2,12(sp)\n"
                            "  18:	000f80e7          	jalr	t6\n"
                            "  1c:	00050f93          	mv	t6,a0\n"
                            "  20:	00012083          	lw	ra,0(sp)\n"
                            "  24:	00412503          	lw	a0,4(sp)\n"
                            "  28:	00812583          	lw	a1,8(sp)\n"
                            "  2c:	00c12603          	lw	a2,12(sp)\n"
                            "  30:	01010113          	addi	sp,sp,16\n"
                            "  34:	00af8463          	beq	t6,a0,0x3c\n"
                            "  38:	000f8513          	mv	a0,t6\n"
                            "  3c:	00008067          	ret"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(t5, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	05462f83          	lw	t6,84(a2)\n"
                            "   4:	ff010113          	addi	sp,sp,-16\n"
                            "   8:	00112023          	sw	ra,0(sp)\n"
                            "   c:	00a12223          	sw	a0,4(sp)\n"
                            "  10:	00b12423          	sw	a1,8(sp)\n"
                            "  14:	00c12623          	sw	a2,12(sp)\n"
                            "  18:	000f80e7          	jalr	t6\n"
                            "  1c:	00050f93          	mv	t6,a0\n"
                            "  20:	00012083          	lw	ra,0(sp)\n"
                            "  24:	00412503          	lw	a0,4(sp)\n"
                            "  28:	00812583          	lw	a1,8(sp)\n"
                            "  2c:	00c12603          	lw	a2,12(sp)\n"
                            "  30:	01010113          	addi	sp,sp,16\n"
                            "  34:	000f8f13          	mv	t5,t6\n"
                            "  38:	00af0463          	beq	t5,a0,0x40\n"
                            "  3c:	000f0513          	mv	a0,t5\n"
                            "  40:	00008067          	ret"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	01452f03          	lw	t5,20(a0)\n"
            "   4:	000f2f83          	lw	t6,0(t5)\n"
            "   8:	05f52e23          	sw	t6,92(a0)"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	01452f83          	lw	t6,20(a0)\n"
            "   4:	01cf8f93          	addi	t6,t6,28\n"
            "   8:	01f52a23          	sw	t6,20(a0)"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

if_block_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State2, RegA, RegB}
        end,
        fun({State0, RegA, RegB}) ->
            [
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	000fd463	bgez	t6,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	01efd463	bge	t6,t5,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	02a00e93	li	t4,42\n"
                        "   c:	01dfd463	bge	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 1024},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	40000e93	li	t4,1024\n"
                        "   c:	01dfd463	bge	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2\n"
                        "  14:	0ec0006f	j	0x100"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	000f9463	bnez	t6,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	000f9463	bnez	t6,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', -1},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	fff00e93	li	t4,-1\n"
                        "   c:	01df9463	bne	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	000f9463	bnez	t6,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	000f9463	bnez	t6,0x10\n"
                        "   c:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	03b00e93	li	t4,59\n"
                        "   c:	01df8463	beq	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	03b00e93	li	t4,59\n"
                        "   c:	01df8463	beq	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83	lw	t6,24(a0)\n"
                        "   4:	01c52f03	lw	t5,28(a0)\n"
                        "   8:	02a00e93	li	t4,42\n"
                        "   c:	01df8463	beq	t6,t4,0x14\n"
                        "  10:	002f0f13	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    % Test large immediate (1995) that requires temporary register
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', 1995},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 1)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	7cb00e93          	li	t4,1995\n"
                        "   c:	01df8463          	beq	t6,t4,0x14\n"
                        "  10:	001f0f13          	addi	t5,t5,1\n"
                        "  14:	0ec0006f          	j	0x100"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df8463          	beq	t6,t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df9463          	bne	t6,t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df9463          	bne	t6,t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df9463          	bne	t6,t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df9463          	bne	t6,t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	01ff9e93          	slli	t4,t6,0x1f\n"
                        "   c:	000ec463          	bltz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	01ff9e93          	slli	t4,t6,0x1f\n"
                        "   c:	000ec463          	bltz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	01ff9e93          	slli	t4,t6,0x1f\n"
                        "   c:	000ed463          	bgez	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	01ff9e93          	slli	t4,t6,0x1f\n"
                        "   c:	000ed463          	bgez	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	007ffe93          	andi	t4,t6,7\n"
                        "   c:	000e8463          	beqz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#5, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	005ffe93          	andi	t4,t6,5\n"
                        "   c:	000e8463          	beqz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	007ffe93          	andi	t4,t6,7\n"
                        "   c:	000e8463          	beqz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	ffffce93          	not	t4,t6\n"
                        "   c:	01ce9e93          	slli	t4,t4,0x1c\n"
                        "  10:	000e8463          	beqz	t4,0x18\n"
                        "  14:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	ffffcf93          	not	t6,t6\n"
                        "   c:	01cf9f93          	slli	t6,t6,0x1c\n"
                        "  10:	000f8463          	beqz	t6,0x18\n"
                        "  14:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	000f8e93          	mv	t4,t6\n"
                        "   c:	03f00e13          	li	t3,63\n"
                        "  10:	01cefeb3          	and	t4,t4,t3\n"
                        "  14:	00800e13          	li	t3,8\n"
                        "  18:	01ce8463          	beq	t4,t3,0x20\n"
                        "  1c:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	01efd463          	bge	t6,t5,0x10\n"
                        "   c:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {
                            {free, RegA},
                            '&',
                            ?TERM_BOXED_TAG_MASK,
                            '!=',
                            ?TERM_BOXED_POSITIVE_INTEGER
                        },
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	03f00e93          	li	t4,63\n"
                        "   c:	01dfffb3          	and	t6,t6,t4\n"
                        "  10:	00800e93          	li	t4,8\n"
                        "  14:	01df8463          	beq	t6,t4,0x1c\n"
                        "  18:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                %% Test {RegA, '&', 16#3, '!=', 0} using ANDI instruction
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#3, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01c52f03          	lw	t5,28(a0)\n"
                        "   8:	003ffe93          	andi	t4,t6,3\n"
                        "   c:	000e8463          	beqz	t4,0x14\n"
                        "  10:	002f0f13          	addi	t5,t5,2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end)
            ]
        end}.

if_else_block_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_else_block(
        State2,
        {Reg1, '==', ?TERM_NIL},
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 2)
        end,
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 4)
        end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	01852f83	lw	t6,24(a0)\n"
            "   4:	01c52f03	lw	t5,28(a0)\n"
            "   8:	03b00e93	li	t4,59\n"
            "   c:	01df9663	bne	t6,t4,0x18\n"
            "  10:	002f0f13	addi	t5,t5,2\n"
            "  14:	0080006f	j	0x1c\n"
            "  18:	004f0f13	addi	t5,t5,4"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 3),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	01852f83          	lw	t6,24(a0)\n"
                    "   4:	003fdf93          	srli	t6,t6,0x3"
                >>,
            ?assertEqual(dump_to_bin(Dump), Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 3),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	01852f83          	lw	t6,24(a0)\n"
                    "   4:	003fdf13          	srli	t5,t6,0x3"
                >>,
            ?assertEqual(dump_to_bin(Dump), Stream)
        end)
    ].

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	01852f83          	lw	t6,24(a0)\n"
            "   4:	003f9f93          	slli	t6,t6,0x3"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	00000697          	auipc	a3,0x0\n"
            "   4:	05468067          	jr	84(a3)\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	01068067          	jr	16(a3)\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	03c68067          	jr	60(a3)\n"
            "  18:	0085af83          	lw	t6,8(a1)\n"
            "  1c:	ffff8f93          	addi	t6,t6,-1\n"
            "  20:	01f5a423          	sw	t6,8(a1)\n"
            "  24:	000f8a63          	beqz	t6,0x38\n"
            "  28:	0240006f          	j	0x4c\n"
            "  2c:	00000013          	nop\n"
            "  30:	00000013          	nop\n"
            "  34:	00000013          	nop\n"
            "  38:	00000f97          	auipc	t6,0x0\n"
            "  3c:	014f8f93          	addi	t6,t6,20\n"
            "  40:	01f5a223          	sw	t6,4(a1)\n"
            "  44:	00862f83          	lw	t6,8(a2)\n"
            "  48:	000f8067          	jr	t6\n"
            "  4c:	00062f83          	lw	t6,0(a2)\n"
            "  50:	000f8067          	jr	t6\n"
            "  54:	00462f83          	lw	t6,4(a2)\n"
            "  58:	000f8067          	jr	t6"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test with large gap (256+ bytes) to force mov_immediate path
call_only_or_schedule_next_and_label_relocation_large_gap_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    % Add large padding by emitting many move_to_native_register operations
    % This creates a large gap between the jump table and the actual function bodies
    % Each operation emits ~2 bytes, so 128 operations = ~256 bytes
    StatePadded = lists:foldl(
        fun(_, S) ->
            ?BACKEND:move_to_native_register(S, {x_reg, 2}, a3)
        end,
        State1,
        lists:seq(1, 128)
    ),
    State2 = ?BACKEND:add_label(StatePadded, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    % Extract the final section starting at 0x218 (after jump table 24 bytes + 128 loads 512 bytes)
    % RISC-V: Jump table is 3×8=24 bytes, loads are 4 bytes each
    Dump = <<
        " 218:	0085af83          	lw	t6,8(a1)\n"
        " 21c:	ffff8f93          	addi	t6,t6,-1\n"
        " 220:	01f5a423          	sw	t6,8(a1)\n"
        " 224:	000f8a63          	beqz	t6,0x238\n"
        " 228:	0240006f          	j	0x24c\n"
        " 22c:	00000013          	nop\n"
        " 230:	00000013          	nop\n"
        " 234:	00000013          	nop\n"
        " 238:	00000f97          	auipc	t6,0x0\n"
        " 23c:	014f8f93          	addi	t6,t6,20\n"
        " 240:	01f5a223          	sw	t6,4(a1)\n"
        " 244:	00862f83          	lw	t6,8(a2)\n"
        " 248:	000f8067          	jr	t6\n"
        " 24c:	00062f83          	lw	t6,0(a2)\n"
        " 250:	000f8067          	jr	t6\n"
        " 254:	00462f83          	lw	t6,4(a2)\n"
        " 258:	000f8067          	jr	t6"
    >>,
    {_, RelevantBinary} = split_binary(Stream, 16#218),
    ?assertEqual(dump_to_bin(Dump), RelevantBinary).

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, 998238357]),
    {State3, ResultReg} = ?BACKEND:call_func_ptr(State2, {free, FuncPtr}, [
        ctx, 0, 1, {free, {x_reg, 0}}, {free, ArgReg}
    ]),
    State4 = ?BACKEND:if_block(State3, {ResultReg, '==', 0}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {x_reg, 0}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	02062f83          	lw	t6,32(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	00058513          	mv	a0,a1\n"
            "  1c:	00200593          	li	a1,2\n"
            "  20:	000f80e7          	jalr	t6\n"
            "  24:	00050f93          	mv	t6,a0\n"
            "  28:	00012083          	lw	ra,0(sp)\n"
            "  2c:	00412503          	lw	a0,4(sp)\n"
            "  30:	00812583          	lw	a1,8(sp)\n"
            "  34:	00c12603          	lw	a2,12(sp)\n"
            "  38:	01010113          	addi	sp,sp,16\n"
            "  3c:	03c62f03          	lw	t5,60(a2)\n"
            "  40:	fe010113          	addi	sp,sp,-32\n"
            "  44:	00112023          	sw	ra,0(sp)\n"
            "  48:	00a12223          	sw	a0,4(sp)\n"
            "  4c:	00b12423          	sw	a1,8(sp)\n"
            "  50:	00c12623          	sw	a2,12(sp)\n"
            "  54:	01f12823          	sw	t6,16(sp)\n"
            "  58:	3b7ff5b7          	lui	a1,0x3b7ff\n"
            "  5c:	89558593          	addi	a1,a1,-1899 # 0x3b7fe895\n"
            "  60:	000f00e7          	jalr	t5\n"
            "  64:	00050f13          	mv	t5,a0\n"
            "  68:	00012083          	lw	ra,0(sp)\n"
            "  6c:	00412503          	lw	a0,4(sp)\n"
            "  70:	00812583          	lw	a1,8(sp)\n"
            "  74:	00c12603          	lw	a2,12(sp)\n"
            "  78:	01012f83          	lw	t6,16(sp)\n"
            "  7c:	02010113          	addi	sp,sp,32\n"
            "  80:	ff010113          	addi	sp,sp,-16\n"
            "  84:	00112023          	sw	ra,0(sp)\n"
            "  88:	00a12223          	sw	a0,4(sp)\n"
            "  8c:	00b12423          	sw	a1,8(sp)\n"
            "  90:	00c12623          	sw	a2,12(sp)\n"
            "  94:	00000593          	li	a1,0\n"
            "  98:	00100613          	li	a2,1\n"
            "  9c:	01852683          	lw	a3,24(a0)\n"
            "  a0:	000f0713          	mv	a4,t5\n"
            "  a4:	000f80e7          	jalr	t6\n"
            "  a8:	00050f93          	mv	t6,a0\n"
            "  ac:	00012083          	lw	ra,0(sp)\n"
            "  b0:	00412503          	lw	a0,4(sp)\n"
            "  b4:	00812583          	lw	a1,8(sp)\n"
            "  b8:	00c12603          	lw	a2,12(sp)\n"
            "  bc:	01010113          	addi	sp,sp,16\n"
            "  c0:	000f9863          	bnez	t6,0xd0\n"
            "  c4:	01862f83          	lw	t6,24(a2)\n"
            "  c8:	0c800613          	li	a2,200\n"
            "  cc:	000f8067          	jr	t6\n"
            "  d0:	01f52c23          	sw	t6,24(a0)"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

get_list_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:and_(State1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    State3 = ?BACKEND:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = ?BACKEND:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	00300f13          	li	t5,3\n"
        "   8:	ffff4f13          	not	t5,t5\n"
        "   c:	01efffb3          	and	t6,t6,t5\n"
        "  10:	004fae83          	lw	t4,4(t6)\n"
        "  14:	01452f03          	lw	t5,20(a0)\n"
        "  18:	01df2223          	sw	t4,4(t5)\n"
        "  1c:	000fae83          	lw	t4,0(t6)\n"
        "  20:	01452f03          	lw	t5,20(a0)\n"
        "  24:	01df2023          	sw	t4,0(t5)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, Arg1),
    State2 = ?BACKEND:if_block(
        State1, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(MSt0) ->
            MSt1 = ?BACKEND:if_block(
                MSt0, {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            ),
            MSt2 = ?BACKEND:and_(MSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
            MSt3 = ?BACKEND:move_array_element(MSt2, Reg, 0, Reg),
            ?BACKEND:if_block(
                MSt3,
                {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            )
        end
    ),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    State4 = ?BACKEND:add_label(State3, Label, 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	ffffcf13          	not	t5,t6\n"
        "   8:	01cf1f13          	slli	t5,t5,0x1c\n"
        "   c:	040f0c63          	beqz	t5,0x64\n"
        "  10:	000f8f13          	mv	t5,t6\n"
        "  14:	00300e93          	li	t4,3\n"
        "  18:	01df7f33          	and	t5,t5,t4\n"
        "  1c:	00200e93          	li	t4,2\n"
        "  20:	01df0a63          	beq	t5,t4,0x34\n"
        "  24:	0dc0006f          	j	0x100\n"
        "  28:	00000013          	nop\n"
        "  2c:	00000013          	nop\n"
        "  30:	00000013          	nop\n"
        "  34:	00300f13          	li	t5,3\n"
        "  38:	ffff4f13          	not	t5,t5\n"
        "  3c:	01efffb3          	and	t6,t6,t5\n"
        "  40:	000faf83          	lw	t6,0(t6)\n"
        "  44:	03f00f13          	li	t5,63\n"
        "  48:	01efffb3          	and	t6,t6,t5\n"
        "  4c:	00800f13          	li	t5,8\n"
        "  50:	01ef8a63          	beq	t6,t5,0x64\n"
        "  54:	0ac0006f          	j	0x100\n"
        "  58:	00000013          	nop\n"
        "  5c:	00000013          	nop\n"
        "  60:	00000013          	nop"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

%% Keep the unoptimized version to test the and case.
is_number_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, Arg1),
    State2 = ?BACKEND:if_block(
        State1, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(BSt0) ->
            BSt1 = cond_jump_to_label(
                {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, ?BACKEND, BSt0
            ),
            BSt2 = ?BACKEND:and_(BSt1, Reg, ?TERM_PRIMARY_CLEAR_MASK),
            BSt3 = ?BACKEND:move_array_element(BSt2, Reg, 0, Reg),
            cond_jump_to_label(
                {'and', [
                    {Reg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                    {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FLOAT}
                ]},
                Label,
                ?BACKEND,
                BSt3
            )
        end
    ),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    State4 = ?BACKEND:add_label(State3, Label, 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	ffffcf13          	not	t5,t6\n"
        "   8:	01cf1f13          	slli	t5,t5,0x1c\n"
        "   c:	060f0663          	beqz	t5,0x78\n"
        "  10:	000f8f13          	mv	t5,t6\n"
        "  14:	00300e93          	li	t4,3\n"
        "  18:	01df7f33          	and	t5,t5,t4\n"
        "  1c:	00200e93          	li	t4,2\n"
        "  20:	01df0a63          	beq	t5,t4,0x34\n"
        "  24:	0dc0006f          	j	0x100\n"
        "  28:	00000013          	nop\n"
        "  2c:	00000013          	nop\n"
        "  30:	00000013          	nop\n"
        "  34:	00300f13          	li	t5,3\n"
        "  38:	ffff4f13          	not	t5,t5\n"
        "  3c:	01efffb3          	and	t6,t6,t5\n"
        "  40:	000faf83          	lw	t6,0(t6)\n"
        "  44:	000f8f13          	mv	t5,t6\n"
        "  48:	03f00e93          	li	t4,63\n"
        "  4c:	01df7f33          	and	t5,t5,t4\n"
        "  50:	00800e93          	li	t4,8\n"
        "  54:	03df0263          	beq	t5,t4,0x78\n"
        "  58:	03f00f13          	li	t5,63\n"
        "  5c:	01efffb3          	and	t6,t6,t5\n"
        "  60:	01800f13          	li	t5,24\n"
        "  64:	01ef8a63          	beq	t6,t5,0x78\n"
        "  68:	0980006f          	j	0x100\n"
        "  6c:	00000013          	nop\n"
        "  70:	00000013          	nop\n"
        "  74:	00000013          	nop"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    State4 = ?BACKEND:add_label(State3, Label, 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83 	lw	t6,24(a0)\n"
        "   4:	04b00f13 	li	t5,75\n"
        "   8:	01ef8e63 	beq	t6,t5,0x24\n"
        "   c:	00b00f13 	li	t5,11\n"
        "  10:	01ef8a63 	beq	t6,t5,0x24\n"
        "  14:	0ec0006f 	j	0x100\n"
        "  18:	00000013 	nop\n"
        "  1c:	00000013 	nop\n"
        "  20:	00000013 	nop"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    State4 = ?BACKEND:add_label(State3, Label, 16#1000),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83 	lw	t6,24(a0)\n"
        "   4:	04b00f13 	li	t5,75\n"
        "   8:	01ef8e63 	beq	t6,t5,0x24\n"
        "   c:	00b00f13 	li	t5,11\n"
        "  10:	01ef8a63 	beq	t6,t5,0x24\n"
        "  14:	7ed0006f 	j	0x1000\n"
        "  18:	00000013 	nop\n"
        "  1c:	00000013 	nop\n"
        "  20:	00000013 	nop"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_far_known_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    State1 = ?BACKEND:add_label(State0, Label, 16#1000),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(State2, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	04b00f13          	li	t5,75\n"
        "   8:	01ef8e63          	beq	t6,t5,0x24\n"
        "   c:	00b00f13          	li	t5,11\n"
        "  10:	01ef8a63          	beq	t6,t5,0x24\n"
        "  14:	00000f17          	auipc	t5,0x0\n"
        "  18:	008f2f03          	lw	t5,8(t5)\n"
        "  1c:	000f0067          	jr	t5\n"
        "  20:	00001000          	.word	0x00001000"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test OP_WAIT_TIMEOUT pattern that uses set_continuation_to_offset and continuation_entry_point
wait_timeout_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    Label = 42,
    {State1, OffsetRef0} = ?BACKEND:set_continuation_to_offset(State0),
    {State2, TimeoutReg} = ?BACKEND:move_to_native_register(State1, 5000),
    State3 = ?BACKEND:call_primitive_last(State2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, TimeoutReg}, Label
    ]),
    State4 = ?BACKEND:add_label(State3, OffsetRef0),
    State5 = ?BACKEND:continuation_entry_point(State4),
    {State6, ResultReg0} = ?BACKEND:call_primitive(State5, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    State7 = ?BACKEND:return_if_not_equal_to_ctx(State6, {free, ResultReg0}),
    % ?WAITING_TIMEOUT_EXPIRED
    {State8, ResultReg1} = ?BACKEND:call_primitive(State7, ?PRIM_CONTEXT_GET_FLAGS, [ctx, 2]),
    State9 = ?BACKEND:if_block(State8, {{free, ResultReg1}, '==', 0}, fun(BlockSt) ->
        ?BACKEND:call_primitive_last(BlockSt, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
            ctx, jit_state, Label
        ])
    end),
    State10 = ?BACKEND:update_branches(State9),

    Stream = ?BACKEND:stream(State10),
    Dump = <<
        "   0:	00000f97          	auipc	t6,0x0\n"
        "   4:	024f8f93          	addi	t6,t6,36\n"
        "   8:	01f5a223          	sw	t6,4(a1)\n"
        "   c:	00001fb7          	lui	t6,0x1\n"
        "  10:	388f8f93          	addi	t6,t6,904\n"
        "  14:	07862f03          	lw	t5,120(a2)\n"
        "  18:	000f8613          	mv	a2,t6\n"
        "  1c:	02a00693          	li	a3,42\n"
        "  20:	000f0067          	jr	t5\n"
        "  24:	05462f83          	lw	t6,84(a2)\n"
        "  28:	ff010113          	addi	sp,sp,-16\n"
        "  2c:	00112023          	sw	ra,0(sp)\n"
        "  30:	00a12223          	sw	a0,4(sp)\n"
        "  34:	00b12423          	sw	a1,8(sp)\n"
        "  38:	00c12623          	sw	a2,12(sp)\n"
        "  3c:	000f80e7          	jalr	t6\n"
        "  40:	00050f93          	mv	t6,a0\n"
        "  44:	00012083          	lw	ra,0(sp)\n"
        "  48:	00412503          	lw	a0,4(sp)\n"
        "  4c:	00812583          	lw	a1,8(sp)\n"
        "  50:	00c12603          	lw	a2,12(sp)\n"
        "  54:	01010113          	addi	sp,sp,16\n"
        "  58:	00af8463          	beq	t6,a0,0x60\n"
        "  5c:	000f8513          	mv	a0,t6\n"
        "  60:	00008067          	ret\n"
        "  64:	08400f93          	li	t6,132\n"
        "  68:	00cf8fb3          	add	t6,t6,a2\n"
        "  6c:	000faf83          	lw	t6,0(t6)\n"
        "  70:	ff010113          	addi	sp,sp,-16\n"
        "  74:	00112023          	sw	ra,0(sp)\n"
        "  78:	00a12223          	sw	a0,4(sp)\n"
        "  7c:	00b12423          	sw	a1,8(sp)\n"
        "  80:	00c12623          	sw	a2,12(sp)\n"
        "  84:	00200593          	li	a1,2\n"
        "  88:	000f80e7          	jalr	t6\n"
        "  8c:	00050f93          	mv	t6,a0\n"
        "  90:	00012083          	lw	ra,0(sp)\n"
        "  94:	00412503          	lw	a0,4(sp)\n"
        "  98:	00812583          	lw	a1,8(sp)\n"
        "  9c:	00c12603          	lw	a2,12(sp)\n"
        "  a0:	01010113          	addi	sp,sp,16\n"
        "  a4:	000f9863          	bnez	t6,0xb4\n"
        "  a8:	07c62f83          	lw	t6,124(a2)\n"
        "  ac:	02a00613          	li	a2,42\n"
        "  b0:	000f8067          	jr	t6"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test OP_WAIT pattern that uses set_continuation_to_label
wait_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:set_continuation_to_label(State2, Label),
    State4 = ?BACKEND:call_primitive_last(State3, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),

    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	00000697          	auipc	a3,0x0\n"
        "   4:	00068067          	jr	a3\n"
        "   8:	00000697          	auipc	a3,0x0\n"
        "   c:	00068067          	jr	a3\n"
        "  10:	00000697          	auipc	a3,0x0\n"
        "  14:	00068067          	jr	a3\n"
        "  18:	00000697          	auipc	a3,0x0\n"
        "  1c:	00068067          	jr	a3\n"
        "  20:	00000697          	auipc	a3,0x0\n"
        "  24:	00068067          	jr	a3\n"
        "  28:	00000697          	auipc	a3,0x0\n"
        "  2c:	00068067          	jr	a3\n"
        "  30:	00000f97          	auipc	t6,0x0\n"
        "  34:	004f8f93          	addi	t6,t6,4\n"
        "  38:	01f5a223          	sw	t6,4(a1)\n"
        "  3c:	07462f83          	lw	t6,116(a2)\n"
        "  40:	000f8067          	jr	t6"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test return_labels_and_lines/2 function
return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    % Test return_labels_and_lines with some sample labels and lines
    State1 = ?BACKEND:add_label(State0, 2, 32),
    State2 = ?BACKEND:add_label(State1, 1, 16),

    % {Line, Offset} pairs
    SortedLines = [{10, 16}, {20, 32}],

    State3 = ?BACKEND:return_labels_and_lines(State2, SortedLines),
    Stream = ?BACKEND:stream(State3),

    % Should have generated auipc + addi + ret + labels table + lines table
    % auipc = 4 bytes, addi = 4 bytes, ret = 4 bytes, labels table = 6*2 = 12 bytes, lines table = 6*2 = 12 bytes
    % Total minimum: 36 bytes
    ?assert(byte_size(Stream) >= 36),

    % Expected: auipc a0, 0 + addi a0, a0, 12 + ret + labels table + lines table
    % The data tables start at offset 0xC (12), so we load PC + 12 into a0
    Dump = <<
        "   0:	00000517          	auipc	a0,0x0\n"
        "   4:	00c50513          	addi	a0,a0,12\n"
        "   8:	00008067          	ret\n"
        "   c:	0200                	addi	s0,sp,256\n"
        "   e:	0100                	addi	s0,sp,128\n"
        "  10:	0000                	unimp\n"
        "  12:	1000                	addi	s0,sp,32\n"
        "  14:	0200                	addi	s0,sp,256\n"
        "  16:	0000                	unimp\n"
        "  18:	2000                	fld	fs0,0(s0)\n"
        "  1a:	0200                	addi	s0,sp,256\n"
        "  1c:	0a00                	addi	s0,sp,272\n"
        "  1e:	0000                	unimp\n"
        "  20:	1000                	addi	s0,sp,32\n"
        "  22:	1400                	addi	s0,sp,544\n"
        "  24:	0000                	unimp\n"
        "  26:	2000                	fld	fs0,0(s0)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test call_primitive with {free, {x_reg, X}}
gc_bif2_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_GET_IMPORTED_BIF, [jit_state, 42]),
    {State2, _ResultReg} = ?BACKEND:call_func_ptr(State1, {free, FuncPtr}, [
        ctx, 0, 3, {y_reg, 0}, {free, {x_reg, 0}}
    ]),

    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	02062f83 	lw	t6,32(a2)\n"
        "   4:	ff010113 	addi	sp,sp,-16\n"
        "   8:	00112023 	sw	ra,0(sp)\n"
        "   c:	00a12223 	sw	a0,4(sp)\n"
        "  10:	00b12423 	sw	a1,8(sp)\n"
        "  14:	00c12623 	sw	a2,12(sp)\n"
        "  18:	00058513 	mv	a0,a1\n"
        "  1c:	02a00593 	li	a1,42\n"
        "  20:	000f80e7 	jalr	t6\n"
        "  24:	00050f93 	mv	t6,a0\n"
        "  28:	00012083 	lw	ra,0(sp)\n"
        "  2c:	00412503 	lw	a0,4(sp)\n"
        "  30:	00812583 	lw	a1,8(sp)\n"
        "  34:	00c12603 	lw	a2,12(sp)\n"
        "  38:	01010113 	addi	sp,sp,16\n"
        "  3c:	ff010113 	addi	sp,sp,-16\n"
        "  40:	00112023 	sw	ra,0(sp)\n"
        "  44:	00a12223 	sw	a0,4(sp)\n"
        "  48:	00b12423 	sw	a1,8(sp)\n"
        "  4c:	00c12623 	sw	a2,12(sp)\n"
        "  50:	00000593 	li	a1,0\n"
        "  54:	00300613 	li	a2,3\n"
        "  58:	01452f03 	lw	t5,20(a0)\n"
        "  5c:	000f2683 	lw	a3,0(t5)\n"
        "  60:	01852703 	lw	a4,24(a0)\n"
        "  64:	000f80e7 	jalr	t6\n"
        "  68:	00050f93 	mv	t6,a0\n"
        "  6c:	00012083 	lw	ra,0(sp)\n"
        "  70:	00412503 	lw	a0,4(sp)\n"
        "  74:	00812583 	lw	a1,8(sp)\n"
        "  78:	00c12603 	lw	a2,12(sp)\n"
        "  7c:	01010113 	addi	sp,sp,16"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test case where parameter value is in a1
memory_ensure_free_with_roots_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, {free, a1}, 4, 1
    ]),

    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0b000f93          	li	t6,176\n"
        "   4:	00cf8fb3          	add	t6,t6,a2\n"
        "   8:	000faf83          	lw	t6,0(t6)\n"
        "   c:	ff010113          	addi	sp,sp,-16\n"
        "  10:	00112023          	sw	ra,0(sp)\n"
        "  14:	00a12223          	sw	a0,4(sp)\n"
        "  18:	00b12423          	sw	a1,8(sp)\n"
        "  1c:	00c12623          	sw	a2,12(sp)\n"
        "  20:	00058f13          	mv	t5,a1\n"
        "  24:	000f0613          	mv	a2,t5\n"
        "  28:	00400693          	li	a3,4\n"
        "  2c:	00100713          	li	a4,1\n"
        "  30:	000f80e7          	jalr	t6\n"
        "  34:	00050f93          	mv	t6,a0\n"
        "  38:	00012083          	lw	ra,0(sp)\n"
        "  3c:	00412503          	lw	a0,4(sp)\n"
        "  40:	00812583          	lw	a1,8(sp)\n"
        "  44:	00c12603          	lw	a2,12(sp)\n"
        "  48:	01010113          	addi	sp,sp,16"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0085af83 	lw	t6,8(a1)\n"
        "   4:	ffff8f93 	addi	t6,t6,-1\n"
        "   8:	01f5a423 	sw	t6,8(a1)\n"
        "   c:	000f9a63 	bnez	t6,0x20\n"
        "  10:	00000f97 	auipc	t6,0x0\n"
        "  14:	010f8f93 	addi	t6,t6,16\n"
        "  18:	00862f83 	lw	t6,8(a2)\n"
        "  1c:	000f8067 	jr	t6\n"
        "  20:	0005af03 	lw	t5,0(a1)\n"
        "  24:	000f2f03 	lw	t5,0(t5)\n"
        "  28:	018f1f13 	slli	t5,t5,0x18\n"
        "  2c:	13000f93 	li	t6,304\n"
        "  30:	01ff6f33 	or	t5,t5,t6\n"
        "  34:	05e52e23 	sw	t5,92(a0)\n"
        "  38:	01062f83 	lw	t6,16(a2)\n"
        "  3c:	00200613 	li	a2,2\n"
        "  40:	00500693 	li	a3,5\n"
        "  44:	fff00713 	li	a4,-1\n"
        "  48:	000f8067 	jr	t6"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_fun_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    FuncReg = {x_reg, 0},
    ArgsCount = 0,
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, FuncReg),
    {State3, RegCopy} = ?BACKEND:copy_to_native_register(State2, Reg),
    State4 = ?BACKEND:if_block(
        State3, {RegCopy, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    State5 = ?BACKEND:and_(State4, RegCopy, ?TERM_PRIMARY_CLEAR_MASK),
    State6 = ?BACKEND:move_array_element(State5, RegCopy, 0, RegCopy),
    State7 = ?BACKEND:if_block(
        State6, {RegCopy, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FUN}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    State8 = ?BACKEND:free_native_registers(State7, [RegCopy]),
    State9 = ?BACKEND:call_primitive_with_cp(State8, ?PRIM_CALL_FUN, [
        ctx, jit_state, Reg, ArgsCount
    ]),
    ?BACKEND:assert_all_native_free(State9),
    Stream = ?BACKEND:stream(State9),
    Dump = <<
        "   0:	0085af83 	lw	t6,8(a1)\n"
        "   4:	ffff8f93 	addi	t6,t6,-1\n"
        "   8:	01f5a423 	sw	t6,8(a1)\n"
        "   c:	000f9a63 	bnez	t6,0x20\n"
        "  10:	00000f97 	auipc	t6,0x0\n"
        "  14:	010f8f93 	addi	t6,t6,16\n"
        "  18:	00862f83 	lw	t6,8(a2)\n"
        "  1c:	000f8067 	jr	t6\n"
        "  20:	01852f83 	lw	t6,24(a0)\n"
        "  24:	000f8f13 	mv	t5,t6\n"
        "  28:	000f0e93 	mv	t4,t5\n"
        "  2c:	00300e13 	li	t3,3\n"
        "  30:	01cefeb3 	and	t4,t4,t3\n"
        "  34:	00200e13 	li	t3,2\n"
        "  38:	01ce8c63 	beq	t4,t3,0x50\n"
        "  3c:	04c62f83 	lw	t6,76(a2)\n"
        "  40:	04000613 	li	a2,64\n"
        "  44:	18b00693 	li	a3,395\n"
        "  48:	000f0713 	mv	a4,t5\n"
        "  4c:	000f8067 	jr	t6\n"
        "  50:	00300e93 	li	t4,3\n"
        "  54:	fffece93 	not	t4,t4\n"
        "  58:	01df7f33 	and	t5,t5,t4\n"
        "  5c:	000f2f03 	lw	t5,0(t5)\n"
        "  60:	000f0e93 	mv	t4,t5\n"
        "  64:	03f00e13 	li	t3,63\n"
        "  68:	01cefeb3 	and	t4,t4,t3\n"
        "  6c:	01400e13 	li	t3,20\n"
        "  70:	01ce8c63 	beq	t4,t3,0x88\n"
        "  74:	04c62f83 	lw	t6,76(a2)\n"
        "  78:	07800613 	li	a2,120\n"
        "  7c:	18b00693 	li	a3,395\n"
        "  80:	000f0713 	mv	a4,t5\n"
        "  84:	000f8067 	jr	t6\n"
        "  88:	0005ae83 	lw	t4,0(a1)\n"
        "  8c:	000eae83 	lw	t4,0(t4)\n"
        "  90:	018e9e93 	slli	t4,t4,0x18\n"
        "  94:	2e000f13 	li	t5,736\n"
        "  98:	01eeeeb3 	or	t4,t4,t5\n"
        "  9c:	05d52e23 	sw	t4,92(a0)\n"
        "  a0:	08000f13 	li	t5,128\n"
        "  a4:	00cf0f33 	add	t5,t5,a2\n"
        "  a8:	000f2f03 	lw	t5,0(t5)\n"
        "  ac:	000f8613 	mv	a2,t6\n"
        "  b0:	00000693 	li	a3,0\n"
        "  b4:	000f0067 	jr	t5"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertEqual(dump_to_bin(Dump), Stream).

move_to_vm_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, 0}, <<
                        "   0:	00000f93          	li	t6,0\n"
                        "   4:	01f52c23          	sw	t6,24(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	00000f93          	li	t6,0\n"
                        "   4:	05f52c23          	sw	t6,88(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, t5}, <<
                        "   0:	00000f93          	li	t6,0\n"
                        "   4:	01ff2023          	sw	t6,0(t5)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	00000f13          	li	t5,0\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	01efa423          	sw	t5,8(t6)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	00000f13          	li	t5,0\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	05efa823          	sw	t5,80(t6)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	02a00f93          	li	t6,42\n"
                        "   4:	01f52c23          	sw	t6,24(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	02a00f93          	li	t6,42\n"
                        "   4:	05f52c23          	sw	t6,88(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	01efa423          	sw	t5,8(t6)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	05efa823          	sw	t5,80(t6)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, a3}, <<
                        "   0:	06300f93          	li	t6,99\n"
                        "   4:	01f6a023          	sw	t6,0(a3)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	01c52f83          	lw	t6,28(a0)\n"
                        "   4:	03f52023          	sw	t6,32(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, a1}, <<
                        "   0:	01c52f83          	lw	t6,28(a0)\n"
                        "   4:	01f5a023          	sw	t6,0(a1)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, t3}, {x_reg, 3}, <<
                        "   0:	000e2f83          	lw	t6,0(t3)\n"
                        "   4:	03f52223          	sw	t6,36(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01452f03          	lw	t5,20(a0)\n"
                        "   8:	01ff2223          	sw	t6,4(t5)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	01452f03          	lw	t5,20(a0)\n"
                        "   4:	000f2f83          	lw	t6,0(t5)\n"
                        "   8:	03f52223          	sw	t6,36(a0)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	01452f03          	lw	t5,20(a0)\n"
                        "   4:	004f2f83          	lw	t6,4(t5)\n"
                        "   8:	03f52223          	sw	t6,36(a0)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, t4, {x_reg, 0}, <<
                        "   0:	01d52c23          	sw	t4,24(a0)\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, t5, {x_reg, extra}, <<
                        "   0:	05e52c23          	sw	t5,88(a0)\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, t3, {ptr, a3}, <<
                        "   0:	01c6a023          	sw	t3,0(a3)\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, a1, {y_reg, 0}, <<
                        "   0:	01452f83          	lw	t6,20(a0)\n"
                        "   4:	00bfa023          	sw	a1,0(t6)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                %% Test: Large immediate to x_reg (uses lui + addi in RISC-V)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, 0}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f93          	addi	t6,t6,1656 # 0x12345678\n"
                        "   8:	01f52c23          	sw	t6,24(a0)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, extra}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f93          	addi	t6,t6,1656 # 0x12345678\n"
                        "   8:	05f52c23          	sw	t6,88(a0)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 2}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f93          	addi	t6,t6,1656 # 0x12345678\n"
                        "   8:	01452f03          	lw	t5,20(a0)\n"
                        "   c:	01ff2423          	sw	t6,8(t5)\n"
                        "  10:	0f00006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 20}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f93          	addi	t6,t6,1656 # 0x12345678\n"
                        "   8:	01452f03          	lw	t5,20(a0)\n"
                        "   c:	05ff2823          	sw	t6,80(t5)\n"
                        "  10:	0f00006f          	j	0x100"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {ptr, a3}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f93          	addi	t6,t6,1656 # 0x12345678\n"
                        "   8:	01f6a023          	sw	t6,0(a3)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	05452f83          	lw	t6,84(a0)\n"
                        "   4:	01452f03          	lw	t5,20(a0)\n"
                        "   8:	07ff2e23          	sw	t6,124(t5)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	01452f03          	lw	t5,20(a0)\n"
                        "   4:	07cf2f83          	lw	t6,124(t5)\n"
                        "   8:	05f52a23          	sw	t6,84(a0)\n"
                        "   c:	0f40006f          	j	0x100"
                    >>)
                end),
                %% Test: Large y_reg index (32) that exceeds str immediate offset limit
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 32}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	08000e93          	li	t4,128\n"
                        "   c:	01fe8eb3          	add	t4,t4,t6\n"
                        "  10:	01eea023          	sw	t5,0(t4)\n"
                        "  14:	0ec0006f          	j	0x100"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	fff00f93          	li	t6,-1\n"
                        "   4:	01f52c23          	sw	t6,24(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -100, {x_reg, 0}, <<
                        "   0:	f9c00f93          	li	t6,-100\n"
                        "   4:	01f52c23          	sw	t6,24(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -1000, {x_reg, 0}, <<
                        "   0:	c1800f93          	li	t6,-1000\n"
                        "   4:	01f52c23          	sw	t6,24(a0)\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end)
            ]
        end}.

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(dump_to_bin(Dump), Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 2, {x_reg, 0}, <<
                        "   0:	0086af83          	lw	t6,8(a3)\n"
                        "   4:	01f52c23          	sw	t6,24(a0)"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, a3, 3, {ptr, t4}, <<
                        "   0:	00c6af83          	lw	t6,12(a3)\n"
                        "   4:	01fea023          	sw	t6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, {y_reg, 2}, <<
                        "   0:	0046af03          	lw	t5,4(a3)\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	01efa423          	sw	t5,8(t6)"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (t4)
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, t4, <<
                        "   0:	0046ae83          	lw	t4,4(a3)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {y_reg, 31}, <<
                        "   0:	01c6af03          	lw	t5,28(a3)\n"
                        "   4:	01452f83          	lw	t6,20(a0)\n"
                        "   8:	07efae23          	sw	t5,124(t6)"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {x_reg, 15}, <<
                        "   0:	01c6af83          	lw	t6,28(a3)\n"
                        "   4:	05f52a23          	sw	t6,84(a0)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {x_reg, 2}, <<
                        "   0:	0106af83          	lw	t6,16(a3)\n"
                        "   4:	002f9f93          	slli	t6,t6,0x2\n"
                        "   8:	01f68fb3          	add	t6,a3,t6\n"
                        "   c:	000faf83          	lw	t6,0(t6)\n"
                        "  10:	03f52023          	sw	t6,32(a0)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {ptr, t4}, <<
                        "   0:	0106af83          	lw	t6,16(a3)\n"
                        "   4:	002f9f93          	slli	t6,t6,0x2\n"
                        "   8:	01f68fb3          	add	t6,a3,t6\n"
                        "   c:	000faf83          	lw	t6,0(t6)\n"
                        "  10:	01fea023          	sw	t6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {y_reg, 31}, <<
                        "   0:	0106af83          	lw	t6,16(a3)\n"
                        "   4:	002f9f93          	slli	t6,t6,0x2\n"
                        "   8:	01f68fb3          	add	t6,a3,t6\n"
                        "   c:	000faf83          	lw	t6,0(t6)\n"
                        "  10:	01452f03          	lw	t5,20(a0)\n"
                        "  14:	07ff2e23          	sw	t6,124(t5)"
                    >>)
                end),
                %% move_array_element with integer index and x_reg destination
                ?_test(begin
                    {State1, BaseReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    move_array_element_test0(State1, BaseReg, 2, {x_reg, 5}, <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	008faf03          	lw	t5,8(t6)\n"
                        "   8:	03e52623          	sw	t5,44(a0)"
                    >>)
                end)
            ]
        end}.

get_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% get_array_element: reg[x] to new native reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, t3, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	010e2f83          	lw	t6,16(t3)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual(t6, Reg)
                end)
            ]
        end}.

move_to_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_array_element/4: x_reg to reg[x]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01f6a423          	sw	t6,8(a3)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	000e0f13          	mv	t5,t3\n"
                        "   8:	002f1f13          	slli	t5,t5,0x2\n"
                        "   c:	01e68f33          	add	t5,a3,t5\n"
                        "  10:	01ff2023          	sw	t6,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, t6}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	000faf83          	lw	t6,0(t6)\n"
                        "   4:	000e0f13          	mv	t5,t3\n"
                        "   8:	002f1f13          	slli	t5,t5,0x2\n"
                        "   c:	01e68f33          	add	t5,a3,t5\n"
                        "  10:	01ff2023          	sw	t6,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01452f03          	lw	t5,20(a0)\n"
                        "   4:	008f2f83          	lw	t6,8(t5)\n"
                        "   8:	000e0f13          	mv	t5,t3\n"
                        "   c:	002f1f13          	slli	t5,t5,0x2\n"
                        "  10:	01e68f33          	add	t5,a3,t5\n"
                        "  14:	01ff2023          	sw	t6,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	01f6a423          	sw	t6,8(a3)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [a3, t3]),
                    State2 = setelement(7, State1, [a3, t3]),
                    [a3, t3] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	001e0f13          	addi	t5,t3,1\n"
                        "   8:	002f1f13          	slli	t5,t5,0x2\n"
                        "   c:	01e68f33          	add	t5,a3,t5\n"
                        "  10:	01ff2023          	sw	t6,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [a3, t3]),
                    State2 = setelement(7, State1, [a3, t3]),
                    [a3, t3] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	02a00f93          	li	t6,42\n"
                        "   4:	001e0f13          	addi	t5,t3,1\n"
                        "   8:	002f1f13          	slli	t5,t5,0x2\n"
                        "   c:	01e68f33          	add	t5,a3,t5\n"
                        "  10:	01ff2023          	sw	t6,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

move_to_native_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_native_register/2: imm
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, 42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	02a00f93          	li	t6,42"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: negative value
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	fd600f93          	li	t6,-42"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: -255 (boundary case)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -255),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	f0100f93          	li	t6,-255"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: -256 (boundary case, fits in immediate for RISC-V)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -256),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	f0000f93          	li	t6,-256\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, t5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t5, Reg),
                    Dump = <<
                        "   0:	000f2f03          	lw	t5,0(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	02c52f83          	lw	t6,44(a0)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	01452f03          	lw	t5,20(a0)\n"
                        "   4:	00cf2f83          	lw	t6,12(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, t5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	02a00f13          	li	t5,42"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, t6, t4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	000f8e93          	mv	t4,t6"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, t6}, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	000fae03          	lw	t3,0(t6)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, a3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	02052683          	lw	a3,32(a0)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, a1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	01452f83          	lw	t6,20(a0)\n"
                        "   4:	008fa583          	lw	a1,8(t6)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% Test: ptr with offset to fp_reg (term_to_float)
                ?_test(begin
                    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    State2 = ?BACKEND:move_to_vm_register(
                        State1, {free, {ptr, RegA, 1}}, {fp_reg, 3}
                    ),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	01852f83          	lw	t6,24(a0)\n"
                        "   4:	06052f03          	lw	t5,96(a0)\n"
                        "   8:	004fae83          	lw	t4,4(t6)\n"
                        "   c:	01df2c23          	sw	t4,24(t5)\n"
                        "  10:	008fae83          	lw	t4,8(t6)\n"
                        "  14:	01df2e23          	sw	t4,28(t5)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

add_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:add(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertEqual(dump_to_bin(Dump), Stream).

add_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    add_test0(State0, a2, 2, <<
                        "   0:	00260613          	addi	a2,a2,2\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, 256, <<
                        "   0:	10000f93          	li	t6,256\n"
                        "   4:	01f60633          	add	a2,a2,t6\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, a3, <<
                        "   0:	00d60633          	add	a2,a2,a3\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end)
            ]
        end}.

sub_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:sub(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertEqual(dump_to_bin(Dump), Stream).

sub_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    sub_test0(State0, a2, 2, <<
                        "   0:	ffe60613          	addi	a2,a2,-2\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, 256, <<
                        "   0:	10000f93          	li	t6,256\n"
                        "   4:	41f60633          	sub	a2,a2,t6\n"
                        "   8:	0f80006f          	j	0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, a3, <<
                        "   0:	40d60633          	sub	a2,a2,a3\n"
                        "   4:	0fc0006f          	j	0x100"
                    >>)
                end)
            ]
        end}.

mul_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:mul(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(dump_to_bin(Dump), Stream).

mul_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    mul_test0(State0, a2, 2, <<
                        "   0:	00161613          	slli	a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 3, <<
                        "   0:	00161f93          	slli	t6,a2,0x1\n"
                        "   4:	00cf8633          	add	a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 4, <<
                        "   0:	00261613          	slli	a2,a2,0x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 5, <<
                        "   0:	00261f93          	slli	t6,a2,0x2\n"
                        "   4:	00cf8633          	add	a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 6, <<
                        "   0:	00161f93          	slli	t6,a2,0x1\n"
                        "   4:	00cf8633          	add	a2,t6,a2\n"
                        "   8:	00161613          	slli	a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 7, <<
                        "   0:	00361f93          	slli	t6,a2,0x3\n"
                        "   4:	40cf8633          	sub	a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 8, <<
                        "   0:	00361613          	slli	a2,a2,0x3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 9, <<
                        "   0:	00361f93          	slli	t6,a2,0x3\n"
                        "   4:	00cf8633          	add	a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 10, <<
                        "   0:	00261f93          	slli	t6,a2,0x2\n"
                        "   4:	00cf8633          	add	a2,t6,a2\n"
                        "   8:	00161613          	slli	a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 11, <<
                        "   0:	00b00f93          	li	t6,11\n"
                        "   4:	03f60633          	mul	a2,a2,t6"
                    >>)
                end)
            ]
        end}.

%% Test set_args1 with y_reg pattern
set_args1_y_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    % Call primitive with y_reg argument to trigger {y_reg, X} pattern in set_args1
    % This mirrors: {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}])
    % but with {y_reg, 5} instead of {free, Src}
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_BITSTRING_GET_UTF8, [
        {y_reg, 5}
    ]),

    Stream = ?BACKEND:stream(State1),
    % Expected disassembly for loading from y_reg and calling primitive
    Dump = <<
        "   0:	04300f93          	li	t6,67\n"
        "   4:	002f9f93          	slli	t6,t6,0x2\n"
        "   8:	00cf8fb3          	add	t6,t6,a2\n"
        "   c:	000faf83          	lw	t6,0(t6)\n"
        "  10:	ff010113          	addi	sp,sp,-16\n"
        "  14:	00112023          	sw	ra,0(sp)\n"
        "  18:	00a12223          	sw	a0,4(sp)\n"
        "  1c:	00b12423          	sw	a1,8(sp)\n"
        "  20:	00c12623          	sw	a2,12(sp)\n"
        "  24:	01452f03          	lw	t5,20(a0)\n"
        "  28:	014f2503          	lw	a0,20(t5)\n"
        "  2c:	000f80e7          	jalr	t6\n"
        "  30:	00050f93          	mv	t6,a0\n"
        "  34:	00012083          	lw	ra,0(sp)\n"
        "  38:	00412503          	lw	a0,4(sp)\n"
        "  3c:	00812583          	lw	a1,8(sp)\n"
        "  40:	00c12603          	lw	a2,12(sp)\n"
        "  44:	01010113          	addi	sp,sp,16"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test large Y register read (Y=123, offset=492, exceeds immediate limit)
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move from a large Y register (123 * 4 = 492 bytes, exceeds immediate limit)
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp register for large offset
    Dump = <<
        "   0:	01452f03          	lw	t5,20(a0)\n"
        "   4:	1ec00f93          	li	t6,492\n"
        "   8:	01ef8fb3          	add	t6,t6,t5\n"
        "   c:	000faf83          	lw	t6,0(t6)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(t6, Reg).

%% Test large Y register write with immediate value
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move immediate to a large Y register (123 * 4 = 492 bytes)
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp registers for large offset
    Dump = <<
        "   0:	02a00f13          	li	t5,42\n"
        "   4:	01452f83          	lw	t6,20(a0)\n"
        "   8:	1ec00e93          	li	t4,492\n"
        "   c:	01fe8eb3          	add	t4,t4,t6\n"
        "  10:	01eea023          	sw	t5,0(t4)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test large Y register read with limited registers (uses IP_REG fallback)
large_y_reg_read_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate most available registers to simulate near-exhaustion (leave 1 for the y_reg helper)
    {State1, _} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Leave one register available so the y_reg helper can work, but it will need IP_REG fallback
    {StateFinal, ResultReg} = ?BACKEND:move_to_native_register(State5, {y_reg, 35}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t0+t1 fallback sequence when temps are exhausted
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	01c52f03          	lw	t5,28(a0)\n"
        "   8:	02052e83          	lw	t4,32(a0)\n"
        "   c:	02452e03          	lw	t3,36(a0)\n"
        "  10:	02852383          	lw	t2,40(a0)\n"
        "  14:	01452283          	lw	t0,20(a0)\n"
        "  18:	08c00313          	li	t1,140\n"
        "  1c:	00530333          	add	t1,t1,t0\n"
        "  20:	00032303          	lw	t1,0(t1)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(t1, ResultReg).

%% Test large Y register write with register exhaustion (uses t1/t0 fallback)
large_y_reg_write_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get a source register first
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    % Allocate most remaining registers to simulate exhaustion
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Try to write to large Y register when only one temp register is available
    StateFinal = ?BACKEND:move_to_vm_register(State5, SrcReg, {y_reg, 50}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t1/t0 fallback sequence
    Dump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	01c52f03          	lw	t5,28(a0)\n"
        "   8:	02052e83          	lw	t4,32(a0)\n"
        "   c:	02452e03          	lw	t3,36(a0)\n"
        "  10:	02852383          	lw	t2,40(a0)\n"
        "  14:	01452303          	lw	t1,20(a0)\n"
        "  18:	0c800293          	li	t0,200\n"
        "  1c:	006282b3          	add	t0,t0,t1\n"
        "  20:	01f2a023          	sw	t6,0(t0)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test boundary case: Y=31 (124 bytes, exactly at limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 31}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses direct addressing since 31 * 4 = 124 < 2048
    Dump = <<
        "   0:	01452f03          	lw	t5,20(a0)\n"
        "   4:	07cf2f83          	lw	t6,124(t5)"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(t6, Reg).

%% Test debugger function
debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00100073          	ebreak"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

and_register_exhaustion_negative_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test negative immediate (-4) which should use NOT+AND with t0 as temp
    StateResult = ?BACKEND:and_(StateNoRegs, t6, -4),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	01c52f03          	lw	t5,28(a0)\n"
        "   8:	02052e83          	lw	t4,32(a0)\n"
        "   c:	02452e03          	lw	t3,36(a0)\n"
        "  10:	02852383          	lw	t2,40(a0)\n"
        "  14:	02c52303          	lw	t1,44(a0)\n"
        "  18:	00300293          	li	t0,3\n"
        "  1c:	fff2c293          	not	t0,t0\n"
        "  20:	005fffb3          	and	t6,t6,t0"
    >>,
    ?assertEqual(dump_to_bin(ExpectedDump), Stream).

and_register_exhaustion_positive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test positive immediate (0x3F) which should use AND with t0 as temp
    StateResult = ?BACKEND:and_(StateNoRegs, t6, 16#3F),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	01852f83          	lw	t6,24(a0)\n"
        "   4:	01c52f03          	lw	t5,28(a0)\n"
        "   8:	02052e83          	lw	t4,32(a0)\n"
        "   c:	02452e03          	lw	t3,36(a0)\n"
        "  10:	02852383          	lw	t2,40(a0)\n"
        "  14:	02c52303          	lw	t1,44(a0)\n"
        "  18:	03f00293          	li	t0,63\n"
        "  1c:	005fffb3          	and	t6,t6,t0"
    >>,
    ?assertEqual(dump_to_bin(ExpectedDump), Stream).

jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    % RISC-V: Each jump table entry is 8 bytes (AUIPC + JALR)
    ?assertEqual((512 + 1) * 8, byte_size(Stream)).

alloc_boxed_integer_fragment_small_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 42}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	03c62f83          	lw	t6,60(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	02a00613          	li	a2,42\n"
            "  1c:	00000693          	li	a3,0\n"
            "  20:	000f80e7          	jalr	t6\n"
            "  24:	00050f93          	mv	t6,a0\n"
            "  28:	00012083          	lw	ra,0(sp)\n"
            "  2c:	00412503          	lw	a0,4(sp)\n"
            "  30:	00812583          	lw	a1,8(sp)\n"
            "  34:	00c12603          	lw	a2,12(sp)\n"
            "  38:	01010113          	addi	sp,sp,16"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

alloc_boxed_integer_fragment_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    % Add a call primitive last to emit literal pool
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADMATCH_ATOM, {free, ResultReg}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	03c62f83          	lw	t6,60(a2)\n"
            "   4:	ff010113          	addi	sp,sp,-16\n"
            "   8:	00112023          	sw	ra,0(sp)\n"
            "   c:	00a12223          	sw	a0,4(sp)\n"
            "  10:	00b12423          	sw	a1,8(sp)\n"
            "  14:	00c12623          	sw	a2,12(sp)\n"
            "  18:	9abce637          	lui	a2,0x9abce\n"
            "  1c:	ef060613          	addi	a2,a2,-272\n"
            "  20:	123456b7          	lui	a3,0x12345\n"
            "  24:	67868693          	addi	a3,a3,1656\n"
            "  28:	000f80e7          	jalr	t6\n"
            "  2c:	00050f93          	mv	t6,a0\n"
            "  30:	00012083          	lw	ra,0(sp)\n"
            "  34:	00412503          	lw	a0,4(sp)\n"
            "  38:	00812583          	lw	a1,8(sp)\n"
            "  3c:	00c12603          	lw	a2,12(sp)\n"
            "  40:	01010113          	addi	sp,sp,16\n"
            "  44:	04c62f03          	lw	t5,76(a2)\n"
            "  48:	04800613          	li	a2,72\n"
            "  4c:	28b00693          	li	a3,651\n"
            "  50:	000f8713          	mv	a4,t6\n"
            "  54:	000f0067          	jr	t5"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test for stack alignment issue in call_func_ptr
%% RISC-V maintains 16-byte stack alignment (RISC-V calling convention)
call_func_ptr_stack_alignment_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _ResultReg} = ?BACKEND:call_func_ptr(State4, {free, t3}, [42]),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:	01852f83          	lw	t6,24(a0)\n"
            "   4:	01c52f03          	lw	t5,28(a0)\n"
            "   8:	02052e83          	lw	t4,32(a0)\n"
            "   c:	02452e03          	lw	t3,36(a0)\n"
            "  10:	fe010113          	addi	sp,sp,-32\n"
            "  14:	00112023          	sw	ra,0(sp)\n"
            "  18:	00a12223          	sw	a0,4(sp)\n"
            "  1c:	00b12423          	sw	a1,8(sp)\n"
            "  20:	00c12623          	sw	a2,12(sp)\n"
            "  24:	01d12823          	sw	t4,16(sp)\n"
            "  28:	01e12a23          	sw	t5,20(sp)\n"
            "  2c:	01f12c23          	sw	t6,24(sp)\n"
            "  30:	02a00513          	li	a0,42\n"
            "  34:	000e00e7          	jalr	t3\n"
            "  38:	fea12e23          	sw	a0,-4(sp)\n"
            "  3c:	00012083          	lw	ra,0(sp)\n"
            "  40:	00412503          	lw	a0,4(sp)\n"
            "  44:	00812583          	lw	a1,8(sp)\n"
            "  48:	00c12603          	lw	a2,12(sp)\n"
            "  4c:	01012e83          	lw	t4,16(sp)\n"
            "  50:	01412f03          	lw	t5,20(sp)\n"
            "  54:	01812f83          	lw	t6,24(sp)\n"
            "  58:	02010113          	addi	sp,sp,32"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test for register exhaustion issue in call_func_ptr with 5+ arguments
%% When all registers are used and we call a function with 5+ args,
%% set_args needs temporary registers but none are available
call_func_ptr_register_exhaustion_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

            % Allocate all available registers to simulate register pressure
            {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
            {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
            {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
            {State6, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
            State6
        end,
        fun(State6) ->
            [
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 3, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	01852f83          	lw	t6,24(a0)\n"
                            "   4:	01c52f03          	lw	t5,28(a0)\n"
                            "   8:	02052e83          	lw	t4,32(a0)\n"
                            "   c:	02452e03          	lw	t3,36(a0)\n"
                            "  10:	02852383          	lw	t2,40(a0)\n"
                            "  14:	02c52303          	lw	t1,44(a0)\n"
                            "  18:	fe010113          	addi	sp,sp,-32\n"
                            "  1c:	00112023          	sw	ra,0(sp)\n"
                            "  20:	00a12223          	sw	a0,4(sp)\n"
                            "  24:	00b12423          	sw	a1,8(sp)\n"
                            "  28:	00c12623          	sw	a2,12(sp)\n"
                            "  2c:	00612823          	sw	t1,16(sp)\n"
                            "  30:	01c12a23          	sw	t3,20(sp)\n"
                            "  34:	01d12c23          	sw	t4,24(sp)\n"
                            "  38:	01f12e23          	sw	t6,28(sp)\n"
                            "  3c:	00038613          	mv	a2,t2\n"
                            "  40:	00300693          	li	a3,3\n"
                            "  44:	00100713          	li	a4,1\n"
                            "  48:	000f00e7          	jalr	t5\n"
                            "  4c:	fea12e23          	sw	a0,-4(sp)\n"
                            "  50:	00012083          	lw	ra,0(sp)\n"
                            "  54:	00412503          	lw	a0,4(sp)\n"
                            "  58:	00812583          	lw	a1,8(sp)\n"
                            "  5c:	00c12603          	lw	a2,12(sp)\n"
                            "  60:	01012303          	lw	t1,16(sp)\n"
                            "  64:	01412e03          	lw	t3,20(sp)\n"
                            "  68:	01812e83          	lw	t4,24(sp)\n"
                            "  6c:	01c12f83          	lw	t6,28(sp)\n"
                            "  70:	02010113          	addi	sp,sp,32"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 1, t1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	01852f83          	lw	t6,24(a0)\n"
                            "   4:	01c52f03          	lw	t5,28(a0)\n"
                            "   8:	02052e83          	lw	t4,32(a0)\n"
                            "   c:	02452e03          	lw	t3,36(a0)\n"
                            "  10:	02852383          	lw	t2,40(a0)\n"
                            "  14:	02c52303          	lw	t1,44(a0)\n"
                            "  18:	fe010113          	addi	sp,sp,-32\n"
                            "  1c:	00112023          	sw	ra,0(sp)\n"
                            "  20:	00a12223          	sw	a0,4(sp)\n"
                            "  24:	00b12423          	sw	a1,8(sp)\n"
                            "  28:	00c12623          	sw	a2,12(sp)\n"
                            "  2c:	00612823          	sw	t1,16(sp)\n"
                            "  30:	01c12a23          	sw	t3,20(sp)\n"
                            "  34:	01d12c23          	sw	t4,24(sp)\n"
                            "  38:	01f12e23          	sw	t6,28(sp)\n"
                            "  3c:	00038613          	mv	a2,t2\n"
                            "  40:	00100693          	li	a3,1\n"
                            "  44:	00030713          	mv	a4,t1\n"
                            "  48:	000f00e7          	jalr	t5\n"
                            "  4c:	fea12e23          	sw	a0,-4(sp)\n"
                            "  50:	00012083          	lw	ra,0(sp)\n"
                            "  54:	00412503          	lw	a0,4(sp)\n"
                            "  58:	00812583          	lw	a1,8(sp)\n"
                            "  5c:	00c12603          	lw	a2,12(sp)\n"
                            "  60:	01012303          	lw	t1,16(sp)\n"
                            "  64:	01412e03          	lw	t3,20(sp)\n"
                            "  68:	01812e83          	lw	t4,24(sp)\n"
                            "  6c:	01c12f83          	lw	t6,28(sp)\n"
                            "  70:	02010113          	addi	sp,sp,32"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, t1, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	01852f83          	lw	t6,24(a0)\n"
                            "   4:	01c52f03          	lw	t5,28(a0)\n"
                            "   8:	02052e83          	lw	t4,32(a0)\n"
                            "   c:	02452e03          	lw	t3,36(a0)\n"
                            "  10:	02852383          	lw	t2,40(a0)\n"
                            "  14:	02c52303          	lw	t1,44(a0)\n"
                            "  18:	fe010113          	addi	sp,sp,-32\n"
                            "  1c:	00112023          	sw	ra,0(sp)\n"
                            "  20:	00a12223          	sw	a0,4(sp)\n"
                            "  24:	00b12423          	sw	a1,8(sp)\n"
                            "  28:	00c12623          	sw	a2,12(sp)\n"
                            "  2c:	00612823          	sw	t1,16(sp)\n"
                            "  30:	01c12a23          	sw	t3,20(sp)\n"
                            "  34:	01d12c23          	sw	t4,24(sp)\n"
                            "  38:	01f12e23          	sw	t6,28(sp)\n"
                            "  3c:	00038613          	mv	a2,t2\n"
                            "  40:	00030693          	mv	a3,t1\n"
                            "  44:	00100713          	li	a4,1\n"
                            "  48:	000f00e7          	jalr	t5\n"
                            "  4c:	fea12e23          	sw	a0,-4(sp)\n"
                            "  50:	00012083          	lw	ra,0(sp)\n"
                            "  54:	00412503          	lw	a0,4(sp)\n"
                            "  58:	00812583          	lw	a1,8(sp)\n"
                            "  5c:	00c12603          	lw	a2,12(sp)\n"
                            "  60:	01012303          	lw	t1,16(sp)\n"
                            "  64:	01412e03          	lw	t3,20(sp)\n"
                            "  68:	01812e83          	lw	t4,24(sp)\n"
                            "  6c:	01c12f83          	lw	t6,28(sp)\n"
                            "  70:	02010113          	addi	sp,sp,32"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual(t5, ResultReg)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, a1},
                        [t5, a3]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	01852f83          	lw	t6,24(a0)\n"
                            "   4:	01c52f03          	lw	t5,28(a0)\n"
                            "   8:	02052e83          	lw	t4,32(a0)\n"
                            "   c:	02452e03          	lw	t3,36(a0)\n"
                            "  10:	02852383          	lw	t2,40(a0)\n"
                            "  14:	02c52303          	lw	t1,44(a0)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	00112023          	sw	ra,0(sp)\n"
                            "  20:	00a12223          	sw	a0,4(sp)\n"
                            "  24:	00b12423          	sw	a1,8(sp)\n"
                            "  28:	00c12623          	sw	a2,12(sp)\n"
                            "  2c:	00612823          	sw	t1,16(sp)\n"
                            "  30:	00712a23          	sw	t2,20(sp)\n"
                            "  34:	01c12c23          	sw	t3,24(sp)\n"
                            "  38:	01d12e23          	sw	t4,28(sp)\n"
                            "  3c:	03e12023          	sw	t5,32(sp)\n"
                            "  40:	03f12223          	sw	t6,36(sp)\n"
                            "  44:	00058313          	mv	t1,a1\n"
                            "  48:	000f0513          	mv	a0,t5\n"
                            "  4c:	00068593          	mv	a1,a3\n"
                            "  50:	000300e7          	jalr	t1\n"
                            "  54:	00a12423          	sw	a0,8(sp)\n"
                            "  58:	00012083          	lw	ra,0(sp)\n"
                            "  5c:	00412503          	lw	a0,4(sp)\n"
                            "  60:	00812583          	lw	a1,8(sp)\n"
                            "  64:	00c12603          	lw	a2,12(sp)\n"
                            "  68:	01012303          	lw	t1,16(sp)\n"
                            "  6c:	01412383          	lw	t2,20(sp)\n"
                            "  70:	01812e03          	lw	t3,24(sp)\n"
                            "  74:	01c12e83          	lw	t4,28(sp)\n"
                            "  78:	02012f03          	lw	t5,32(sp)\n"
                            "  7c:	02412f83          	lw	t6,36(sp)\n"
                            "  80:	03010113          	addi	sp,sp,48"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {primitive, 2},
                        [{free, t5}, a3]
                    ),
                    ?assertEqual(ResultReg, t5),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	01852f83          	lw	t6,24(a0)\n"
                            "   4:	01c52f03          	lw	t5,28(a0)\n"
                            "   8:	02052e83          	lw	t4,32(a0)\n"
                            "   c:	02452e03          	lw	t3,36(a0)\n"
                            "  10:	02852383          	lw	t2,40(a0)\n"
                            "  14:	02c52303          	lw	t1,44(a0)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	00112023          	sw	ra,0(sp)\n"
                            "  20:	00a12223          	sw	a0,4(sp)\n"
                            "  24:	00b12423          	sw	a1,8(sp)\n"
                            "  28:	00c12623          	sw	a2,12(sp)\n"
                            "  2c:	00612823          	sw	t1,16(sp)\n"
                            "  30:	00712a23          	sw	t2,20(sp)\n"
                            "  34:	01c12c23          	sw	t3,24(sp)\n"
                            "  38:	01d12e23          	sw	t4,28(sp)\n"
                            "  3c:	03f12023          	sw	t6,32(sp)\n"
                            "  40:	00862303          	lw	t1,8(a2)\n"
                            "  44:	000f0513          	mv	a0,t5\n"
                            "  48:	00068593          	mv	a1,a3\n"
                            "  4c:	000300e7          	jalr	t1\n"
                            "  50:	00050f13          	mv	t5,a0\n"
                            "  54:	00012083          	lw	ra,0(sp)\n"
                            "  58:	00412503          	lw	a0,4(sp)\n"
                            "  5c:	00812583          	lw	a1,8(sp)\n"
                            "  60:	00c12603          	lw	a2,12(sp)\n"
                            "  64:	01012303          	lw	t1,16(sp)\n"
                            "  68:	01412383          	lw	t2,20(sp)\n"
                            "  6c:	01812e03          	lw	t3,24(sp)\n"
                            "  70:	01c12e83          	lw	t4,28(sp)\n"
                            "  74:	02012f83          	lw	t6,32(sp)\n"
                            "  78:	03010113          	addi	sp,sp,48"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

%% Test jump_to_continuation optimization for intra-module returns
jump_to_continuation_test_() ->
    [
        ?_test(begin
            % Test 1: jump_to_continuation at offset 0
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            State1 = ?BACKEND:jump_to_continuation(State0, {free, a0}),
            Stream = ?BACKEND:stream(State1),
            % Expected: riscv32 PIC sequence - simpler than ARM, no prolog/epilog needed
            Dump =
                <<
                    "   0:	00000f97          	auipc	t6,0x0\n"
                    "   4:	00af8fb3          	add	t6,t6,a0\n"
                    "   8:	000f8067          	jr	t6"
                >>,
            ?assertEqual(dump_to_bin(Dump), Stream)
        end),
        ?_test(begin
            % Test 2: jump_to_continuation after jump table (non-zero relative address)
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            % Generate a jump table for 3 labels (4 entries * 8 bytes = 32 bytes)
            State1 = ?BACKEND:jump_table(State0, 3),
            State2 = ?BACKEND:jump_to_continuation(State1, {free, a0}),
            Stream = ?BACKEND:stream(State2),
            % Expected: jump table (32 bytes) + jump_to_continuation
            % NetOffset = 0 - 32 = -32 (0xFFFFFFE0)
            Dump =
                <<
                    "   0:	00000697          	auipc	a3,0x0\n"
                    "   4:	00068067          	jr	a3\n"
                    "   8:	00000697          	auipc	a3,0x0\n"
                    "   c:	00068067          	jr	a3\n"
                    "  10:	00000697          	auipc	a3,0x0\n"
                    "  14:	00068067          	jr	a3\n"
                    "  18:	00000697          	auipc	a3,0x0\n"
                    "  1c:	00068067          	jr	a3\n"
                    "  20:	00000f97          	auipc	t6,0x0\n"
                    "  24:	fe0f8f93          	addi	t6,t6,-32\n"
                    "  28:	00af8fb3          	add	t6,t6,a0\n"
                    "  2c:	000f8067          	jr	t6"
                >>,
            ?assertEqual(dump_to_bin(Dump), Stream)
        end)
    ].

%% Mimic part of add.beam
add_beam_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:move_to_vm_register(State2, 16#9f, {x_reg, 1}),
    State4 = ?BACKEND:move_to_vm_register(State3, 16#8f, {x_reg, 0}),
    State5 = ?BACKEND:call_only_or_schedule_next(State4, 2),
    State6 = ?BACKEND:add_label(State5, 2),
    {State7, ResultReg} = ?BACKEND:call_primitive(State6, ?PRIM_ALLOCATE, [
        ctx, jit_state, 1, 0, 1
    ]),
    State8 = ?BACKEND:if_block(State7, {'(bool)', {free, ResultReg}, '==', false}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State9 = ?BACKEND:move_to_vm_register(State8, ?TERM_NIL, {y_reg, 0}),
    State10 = ?BACKEND:call_or_schedule_next(State9, 3),
    State11 = ?BACKEND:add_label(State10, 3),
    State12 = ?BACKEND:call_primitive_last(State11, ?PRIM_RETURN, [
        ctx, jit_state
    ]),
    % OP_INT_CALL_END
    State13 = ?BACKEND:add_label(State12, 0),
    State14 = ?BACKEND:call_primitive_last(State13, 1, [ctx, jit_state]),
    State15 = ?BACKEND:update_branches(State14),
    Stream = ?BACKEND:stream(State15),
    riscv32_helper:disassemble(Stream),
    Dump =
        <<
            % jump table (new 8-byte format)
            "   0:	00000697          	auipc	a3,0x0\n"
            "   4:	11868067          	jr	280(a3)\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	01868067          	jr	24(a3)\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	05468067          	jr	84(a3)\n"
            "  18:	00000697          	auipc	a3,0x0\n"
            "  1c:	0f868067          	jr	248(a3)\n"
            % label 1
            % {move,{integer,9},{x,1}}.
            "  20:	09f00f93          	li	t6,159\n"
            "  24:	01f52e23          	sw	t6,28(a0)\n"
            % {move,{integer,8},{x,0}}
            "  28:	08f00f93          	li	t6,143\n"
            "  2c:	01f52c23          	sw	t6,24(a0)\n"
            % {call_only,2,{f,2}}.
            "  30:	0085af83          	lw	t6,8(a1)\n"
            "  34:	ffff8f93          	addi	t6,t6,-1\n"
            "  38:	01f5a423          	sw	t6,8(a1)\n"
            "  3c:	000f8a63          	beqz	t6,0x50\n"
            "  40:	0240006f          	j	0x64\n"
            "  44:	00000013          	nop\n"
            "  48:	00000013          	nop\n"
            "  4c:	00000013          	nop\n"
            "  50:	00000f97          	auipc	t6,0x0\n"
            "  54:	014f8f93          	addi	t6,t6,20\n"
            "  58:	01f5a223          	sw	t6,4(a1)\n"
            "  5c:	00862f83          	lw	t6,8(a2)\n"
            "  60:	000f8067          	jr	t6\n"
            % label 2
            % {allocate,1,1}.
            "  64:	01462f83          	lw	t6,20(a2)\n"
            "  68:	ff010113          	addi	sp,sp,-16\n"
            "  6c:	00112023          	sw	ra,0(sp)\n"
            "  70:	00a12223          	sw	a0,4(sp)\n"
            "  74:	00b12423          	sw	a1,8(sp)\n"
            "  78:	00c12623          	sw	a2,12(sp)\n"
            "  7c:	00100613          	li	a2,1\n"
            "  80:	00000693          	li	a3,0\n"
            "  84:	00100713          	li	a4,1\n"
            "  88:	000f80e7          	jalr	t6\n"
            "  8c:	00050f93          	mv	t6,a0\n"
            "  90:	00012083          	lw	ra,0(sp)\n"
            "  94:	00412503          	lw	a0,4(sp)\n"
            "  98:	00812583          	lw	a1,8(sp)\n"
            "  9c:	00c12603          	lw	a2,12(sp)\n"
            "  a0:	01010113          	addi	sp,sp,16\n"
            "  a4:	01ff9f13          	slli	t5,t6,0x1f\n"
            "  a8:	000f4863          	bltz	t5,0xb8\n"
            "  ac:	01862f83          	lw	t6,24(a2)\n"
            "  b0:	0b000613          	li	a2,176\n"
            "  b4:	000f8067          	jr	t6\n"
            % {init_yregs,{list,[{y,0}]}}.
            %% move_to_vm_register(State8, ?TERM_NIL, {y_reg, 0}),
            "  b8:	03b00f13          	li	t5,59\n"
            "  bc:	01452f83          	lw	t6,20(a0)\n"
            "  c0:	01efa023          	sw	t5,0(t6)\n"
            % {call,1,{f,3}}
            %% call_or_schedule_next(State9, 3),
            "  c4:	0005af03          	lw	t5,0(a1)\n"
            "  c8:	000f2f03          	lw	t5,0(t5)\n"
            "  cc:	018f1f13          	slli	t5,t5,0x18\n"
            "  d0:	44000f93          	li	t6,1088\n"
            "  d4:	01ff6f33          	or	t5,t5,t6\n"
            "  d8:	05e52e23          	sw	t5,92(a0)\n"
            "  dc:	0085af83          	lw	t6,8(a1)\n"
            "  e0:	ffff8f93          	addi	t6,t6,-1\n"
            "  e4:	01f5a423          	sw	t6,8(a1)\n"
            "  e8:	000f8a63          	beqz	t6,0xfc\n"
            "  ec:	0240006f          	j	0x110\n"
            "  f0:	00000013          	nop\n"
            "  f4:	00000013          	nop\n"
            "  f8:	00000013          	nop\n"
            "  fc:	00000f97          	auipc	t6,0x0\n"
            " 100:	014f8f93          	addi	t6,t6,20\n"
            " 104:	01f5a223          	sw	t6,4(a1)\n"
            " 108:	00862f83          	lw	t6,8(a2)\n"
            " 10c:	000f8067          	jr	t6\n"
            %% (continuation)
            % label 3
            " 110:	00462f83          	lw	t6,4(a2)\n"
            " 114:	000f8067          	jr	t6\n"
            % label 0
            " 118:	00462f83          	lw	t6,4(a2)\n"
            " 11c:	000f8067          	jr	t6\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

dump_to_bin(Dump) ->
    dump_to_bin0(Dump, addr, []).

-define(IS_HEX_DIGIT(C),
    ((C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F))
).

dump_to_bin0(<<N, $:, Tail/binary>>, addr, Acc) when ?IS_HEX_DIGIT(N) ->
    dump_to_bin0(Tail, hex, Acc);
dump_to_bin0(<<N, Tail/binary>>, addr, Acc) when ?IS_HEX_DIGIT(N) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\s, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$	, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\s, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, hex, Acc);
dump_to_bin0(<<$	, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, hex, Acc);
%% Handle RISC-V 32-bit instructions (8 consecutive hex digits)
dump_to_bin0(<<H1, H2, H3, H4, H5, H6, H7, H8, Sp, Rest/binary>>, hex, Acc) when
    (Sp =:= $	 orelse Sp =:= $\s) andalso
        ?IS_HEX_DIGIT(H1) andalso
        ?IS_HEX_DIGIT(H2) andalso
        ?IS_HEX_DIGIT(H3) andalso
        ?IS_HEX_DIGIT(H4) andalso
        ?IS_HEX_DIGIT(H5) andalso
        ?IS_HEX_DIGIT(H6) andalso
        ?IS_HEX_DIGIT(H7) andalso
        ?IS_HEX_DIGIT(H8)
->
    %% RISC-V instructions are 32-bit little-endian
    Instr = list_to_integer([H1, H2, H3, H4, H5, H6, H7, H8], 16),
    dump_to_bin0(Rest, instr, [<<Instr:32/little>> | Acc]);
%% Handle 32-bits undefined instruction (ARM format with space: "1234 5678")
dump_to_bin0(<<H1, H2, H3, H4, $\s, H5, H6, H7, H8, Sp, Rest/binary>>, hex, Acc) when
    (Sp =:= $	 orelse Sp =:= $\s) andalso
        ?IS_HEX_DIGIT(H1) andalso
        ?IS_HEX_DIGIT(H2) andalso
        ?IS_HEX_DIGIT(H3) andalso
        ?IS_HEX_DIGIT(H4) andalso
        ?IS_HEX_DIGIT(H5) andalso
        ?IS_HEX_DIGIT(H6) andalso
        ?IS_HEX_DIGIT(H7) andalso
        ?IS_HEX_DIGIT(H8)
->
    InstrA = list_to_integer([H1, H2, H3, H4], 16),
    InstrB = list_to_integer([H5, H6, H7, H8], 16),
    dump_to_bin0(Rest, instr, [<<InstrB:16/little>>, <<InstrA:16/little>> | Acc]);
%% Handle 16-bit ARM32 Thumb instructions (4 hex digits)
dump_to_bin0(<<H1, H2, H3, H4, Sp, Rest/binary>>, hex, Acc) when
    (Sp =:= $	 orelse Sp =:= $\s) andalso
        ?IS_HEX_DIGIT(H1) andalso
        ?IS_HEX_DIGIT(H2) andalso
        ?IS_HEX_DIGIT(H3) andalso
        ?IS_HEX_DIGIT(H4)
->
    %% Parse 4 hex digits (ARM32 Thumb 16-bit instruction)
    Instr = list_to_integer([H1, H2, H3, H4], 16),
    dump_to_bin0(Rest, instr, [<<Instr:16/little>> | Acc]);
dump_to_bin0(<<$\n, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc)).
