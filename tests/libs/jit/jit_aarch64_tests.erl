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

-module(jit_aarch64_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_aarch64).

% disassembly obtained with:
% aarch64-elf-objdump -b binary -D dump.bin -M aarch64

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "0:	f9400050 	ldr	x16, [x2]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	aa0003e7 	mov	x7, x0\n"
            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "0:	f9400450 	ldr	x16, [x2, #8]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	aa0003e7 	mov	x7, x0\n"
            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400850 	ldr	x16, [x2, #16]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d2800541 	mov	x1, #0x2a                  	// #42\n"
            "  10:	d2800562 	mov	x2, #0x2b                  	// #43\n"
            "  14:	d2800583 	mov	x3, #0x2c                  	// #44\n"
            "  18:	d63f0200 	blr	x16\n"
            "  1c:	aa0003e7 	mov	x7, x0\n"
            "  20:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  24:	a8c103fe 	ldp	x30, x0, [sp], #16"
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
    Dump =
        <<
            "\n"
            "   0:	f9404850 	ldr	x16, [x2, #144]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d2800261 	mov	x1, #0x13                  	// #19\n"
            "  10:	d63f0200 	blr	x16\n"
            "  14:	aa0003e7 	mov	x7, x0\n"
            "  18:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  1c:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  20:	f9404850 	ldr	x16, [x2, #144]\n"
            "  24:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  28:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "  2c:	f81f0fe7 	str	x7, [sp, #-16]!\n"
            "  30:	d2800281 	mov	x1, #0x14                  	// #20\n"
            "  34:	d63f0200 	blr	x16\n"
            "  38:	aa0003e8 	mov	x8, x0\n"
            "  3c:	f84107e7 	ldr	x7, [sp], #16\n"
            "  40:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  44:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  48:	f9404850 	ldr	x16, [x2, #144]\n"
            "  4c:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  50:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "  54:	a9bf1fe8 	stp	x8, x7, [sp, #-16]!\n"
            "  58:	d2800261 	mov	x1, #0x13                  	// #19\n"
            "  5c:	d63f0200 	blr	x16\n"
            "  60:	aa0003e9 	mov	x9, x0\n"
            "  64:	a8c11fe8 	ldp	x8, x7, [sp], #16\n"
            "  68:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  6c:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  70:	f9403450 	ldr	x16, [x2, #104]\n"
            "  74:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  78:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "  7c:	f81f0fe9 	str	x9, [sp, #-16]!\n"
            "  80:	f94000e1 	ldr	x1, [x7]\n"
            "  84:	f9400102 	ldr	x2, [x8]\n"
            "  88:	d63f0200 	blr	x16\n"
            "  8c:	aa0003e7 	mov	x7, x0\n"
            "  90:	f84107e9 	ldr	x9, [sp], #16\n"
            "  94:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  98:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  9c:	f9000127 	str	x7, [x9]\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9400827 	ldr	x7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	f9000827 	str	x7, [x1, #16]\n"
        "   c:	540000a1 	b.ne	0x20  // b.any\n"
        "  10:	10000087 	adr	x7, 0x20\n"
        "  14:	f9000427 	str	x7, [x1, #8]\n"
        "  18:	f9400847 	ldr	x7, [x2, #16]\n"
        "  1c:	d61f00e0 	br	x7\n"
        "  20:	f9401047 	ldr	x7, [x2, #32]\n"
        "  24:	92800002 	mov	x2, #0xffffffffffffffff    	// #-1\n"
        "  28:	d61f00e0 	br	x7"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9400827 	ldr	x7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	f9000827 	str	x7, [x1, #16]\n"
        "   c:	540000a1 	b.ne	0x20  // b.any\n"
        "  10:	10000087 	adr	x7, 0x20\n"
        "  14:	f9000427 	str	x7, [x1, #8]\n"
        "  18:	f9400847 	ldr	x7, [x2, #16]\n"
        "  1c:	d61f00e0 	br	x7\n"
        "  20:	f9401047 	ldr	x7, [x2, #32]\n"
        "  24:	d2800042 	mov	x2, #0x2                   	// #2\n"
        "  28:	d2800043 	mov	x3, #0x2                   	// #2\n"
        "  2c:	d2800144 	mov	x4, #0xa                   	// #10\n"
        "  30:	d61f00e0 	br	x7"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400047 	ldr	x7, [x2]\n"
            "   4:	d2800542 	mov	x2, #0x2a                  	// #42\n"
            "   8:	d61f00e0 	br	x7"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9401407 	ldr	x7, [x0, #40]\n"
            "   4:	f94000e7 	ldr	x7, [x7]\n"
            "   8:	f9005c07 	str	x7, [x0, #184]"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9401407 	ldr	x7, [x0, #40]\n"
            "   4:	9100e0e7 	add	x7, x7, #0x38\n"
            "   8:	f9001407 	str	x7, [x0, #40]"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    Offset1 = ?BACKEND:offset(State1),
    State2 = ?BACKEND:call_only_or_schedule_next(State1, 2),
    Offset2 = ?BACKEND:offset(State2),
    State3 = ?BACKEND:call_primitive_last(State2, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    Offset0 = ?BACKEND:offset(State3),
    State4 = ?BACKEND:call_primitive_last(State3, 1, [ctx, jit_state]),
    State5 = ?BACKEND:update_branches(State4, [{0, Offset0}, {1, Offset1}, {2, Offset2}]),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:	1400000d 	b	0x34\n"
            "   4:	14000002 	b	0xc\n"
            "   8:	14000009 	b	0x2c\n"
            "   c:	f9400827 	ldr	x7, [x1, #16]\n"
            "  10:	f10004e7 	subs	x7, x7, #0x1\n"
            "  14:	f9000827 	str	x7, [x1, #16]\n"
            "  18:	540000a1 	b.ne	0x2c  // b.any\n"
            "  1c:	10000087 	adr	x7, 0x2c\n"
            "  20:	f9000427 	str	x7, [x1, #8]\n"
            "  24:	f9400847 	ldr	x7, [x2, #16]\n"
            "  28:	d61f00e0 	br	x7\n"
            "  2c:	f9400047 	ldr	x7, [x2]\n"
            "  30:	d61f00e0 	br	x7\n"
            "  34:	f9400447 	ldr	x7, [x2, #8]\n"
            "  38:	d61f00e0 	br	x7"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, 9208452466117618637]),
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
            "   0:	f9402050 	ldr	x16, [x2, #64]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	aa0103e0 	mov	x0, x1\n"
            "  10:	d2800041 	mov	x1, #0x2                   	// #2\n"
            "  14:	d63f0200 	blr	x16\n"
            "  18:	aa0003e7 	mov	x7, x0\n"
            "  1c:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  20:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  24:	f9403c50 	ldr	x16, [x2, #120]\n"
            "  28:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  2c:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "  30:	f81f0fe7 	str	x7, [sp, #-16]!\n"
            "  34:	d29579a1 	mov	x1, #0xabcd                	// #43981\n"
            "  38:	f2b7c041 	movk	x1, #0xbe02, lsl #16\n"
            "  3c:	f2dfd741 	movk	x1, #0xfeba, lsl #32\n"
            "  40:	f2eff941 	movk	x1, #0x7fca, lsl #48\n"
            "  44:	d63f0200 	blr	x16\n"
            "  48:	aa0003e8 	mov	x8, x0\n"
            "  4c:	f84107e7 	ldr	x7, [sp], #16\n"
            "  50:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  54:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  58:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  5c:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "  60:	d2800001 	mov	x1, #0x0                   	// #0\n"
            "  64:	d2800022 	mov	x2, #0x1                   	// #1\n"
            "  68:	f9401803 	ldr	x3, [x0, #48]\n"
            "  6c:	aa0803e4 	mov	x4, x8\n"
            "  70:	d63f00e0 	blr	x7\n"
            "  74:	aa0003e7 	mov	x7, x0\n"
            "  78:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  7c:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  80:	ea0700ff 	tst	x7, x7\n"
            "  84:	54000081 	b.ne	0x94  // b.any\n"
            "  88:	f9401847 	ldr	x7, [x2, #48]\n"
            "  8c:	d2801182 	mov	x2, #0x8c                  	// #140\n"
            "  90:	d61f00e0 	br	x7\n"
            "  94:	f9001807 	str	x7, [x0, #48]"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

get_list_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:and_(State1, Reg, -4),
    State3 = ?BACKEND:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = ?BACKEND:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
        "   4:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "   8:	f9401408 	ldr	x8, [x0, #40]\n"
        "   c:	f94004e9 	ldr	x9, [x7, #8]\n"
        "  10:	f9000509 	str	x9, [x8, #8]\n"
        "  14:	f9401408 	ldr	x8, [x0, #40]\n"
        "  18:	f94000e9 	ldr	x9, [x7]\n"
        "  1c:	f9000109 	str	x9, [x8]"
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
    Offset = ?BACKEND:offset(State3),
    Labels = [{Label, Offset + 16#100}],
    State4 = ?BACKEND:update_branches(State3, Labels),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
        "   4:	aa0703e8 	mov	x8, x7\n"
        "   8:	92400d08 	and	x8, x8, #0xf\n"
        "   c:	f1003d1f 	cmp	x8, #0xf\n"
        "  10:	54000180 	b.eq	0x40  // b.none\n"
        "  14:	aa0703e8 	mov	x8, x7\n"
        "  18:	92400508 	and	x8, x8, #0x3\n"
        "  1c:	f100091f 	cmp	x8, #0x2\n"
        "  20:	54000040 	b.eq	0x28  // b.none\n"
        "  24:	14000047 	b	0x140\n"
        "  28:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "  2c:	f94000e7 	ldr	x7, [x7]\n"
        "  30:	924014e7 	and	x7, x7, #0x3f\n"
        "  34:	f10020ff 	cmp	x7, #0x8\n"
        "  38:	54000040 	b.eq	0x40  // b.none\n"
        "  3c:	14000041 	b	0x140"
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
    Offset = ?BACKEND:offset(State3),
    Labels = [{Label, Offset + 16#100}],
    ?BACKEND:assert_all_native_free(State3),
    State4 = ?BACKEND:update_branches(State3, Labels),
    Stream = ?BACKEND:stream(State4),
%   ok = file:write_file("dump.bin", Stream),
    Dump = <<
        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
        "   4:	f1012cff 	cmp	x7, #0x4b\n"
        "   8:	54000080 	b.eq	0x18  // b.none\n"
        "   c:	f1002cff 	cmp	x7, #0xb\n"
        "  10:	54000040 	b.eq	0x18  // b.none\n"
        "  14:	14000041 	b	0x118"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_test() ->
    %% TODO: Implement AArch64 version
    ok.

call_fun_test() ->
    %% TODO: Implement AArch64 version
    ok.

move_to_vm_register_test_() ->
    %% TODO: Implement AArch64 version
    [].

move_array_element_test_() ->
    %% TODO: Implement AArch64 version
    [].

get_array_element_test_() ->
    %% TODO: Implement AArch64 version
    [].

move_to_array_element_test_() ->
    %% TODO: Implement AArch64 version
    [].

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
dump_to_bin0(<<$\t, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\s, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, hex, Acc);
dump_to_bin0(<<$\t, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, hex, Acc);
dump_to_bin0(<<H1, H2, H3, H4, H5, H6, H7, H8, Sp, Rest/binary>>, hex, Acc) when
    (Sp =:= $\t orelse Sp =:= $\s) andalso
        ?IS_HEX_DIGIT(H1) andalso
        ?IS_HEX_DIGIT(H2) andalso
        ?IS_HEX_DIGIT(H3) andalso
        ?IS_HEX_DIGIT(H4) andalso
        ?IS_HEX_DIGIT(H5) andalso
        ?IS_HEX_DIGIT(H6) andalso
        ?IS_HEX_DIGIT(H7) andalso
        ?IS_HEX_DIGIT(H8)
->
    %% Parse 8 hex digits (AArch64 32-bit instruction)
    Instr = list_to_integer([H1, H2, H3, H4, H5, H6, H7, H8], 16),
    dump_to_bin0(Rest, instr, [<<Instr:32/little>> | Acc]);
dump_to_bin0(<<$\n, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc)).
