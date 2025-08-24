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

-module(jit_armv6m_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_armv6m).

% disassembly obtained with:
% arm-elf-objdump -b binary -D dump.bin -M arm

call_primitive_0_test_DISABLED() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400050 	ldr	x16, [x2]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	aa0003e7 	mov	x7, x0\n"
            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_1_test_DISABLED() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400450 	ldr	x16, [x2, #8]\n"
            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	aa0003e7 	mov	x7, x0\n"
            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_2_args_test_DISABLED() ->
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

call_primitive_extended_regs_test_DISABLED() ->
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

call_ext_only_test_DISABLED() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401027 	ldr	w7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001027 	str	w7, [x1, #16]\n"
        "   c:	540000a1 	b.ne	0x20  // b.any\n"
        "  10:	10000087 	adr	x7, 0x20\n"
        "  14:	f9000427 	str	x7, [x1, #8]\n"
        "  18:	f9400847 	ldr	x7, [x2, #16]\n"
        "  1c:	d61f00e0 	br	x7\n"
        "  20:	f9401047 	ldr	x7, [x2, #32]\n"
        "  24:	d2800042 	mov	x2, #0x2                   	// #2\n"
        "  28:	d2800043 	mov	x3, #0x2                   	// #2\n"
        "  2c:	92800004 	mov	x4, #0xffffffffffffffff    	// #-1\n"
        "  30:	d61f00e0 	br	x7"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_last_test_DISABLED() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401027 	ldr	w7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001027 	str	w7, [x1, #16]\n"
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

call_primitive_last_test_DISABLED() ->
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

return_if_not_equal_to_ctx_test_DISABLED_() ->
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
                    ?assertEqual(r7, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	f9405450 	ldr	x16, [x2, #168]\n"
                            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
                            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
                            "   c:	d63f0200 	blr	x16\n"
                            "  10:	aa0003e7 	mov	x7, x0\n"
                            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
                            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
                            "  1c:	eb0000ff 	cmp	x7, x0\n"
                            "  20:	54000060 	b.eq	0x2c  // b.none\n"
                            "  24:	aa0703e0 	mov	x0, x7\n"
                            "  28:	d65f03c0 	ret"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(r7, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(r8, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	f9405450 	ldr	x16, [x2, #168]\n"
                            "   4:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
                            "   8:	a9bf0be1 	stp	x1, x2, [sp, #-16]!\n"
                            "   c:	d63f0200 	blr	x16\n"
                            "  10:	aa0003e7 	mov	x7, x0\n"
                            "  14:	a8c10be1 	ldp	x1, x2, [sp], #16\n"
                            "  18:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
                            "  1c:	aa0703e8 	mov	x8, x7\n"
                            "  20:	eb00011f 	cmp	x8, x0\n"
                            "  24:	54000060 	b.eq	0x30  // b.none\n"
                            "  28:	aa0803e0 	mov	x0, x8\n"
                            "  2c:	d65f03c0 	ret"
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
            "   0:	6947      	ldr	r7, [r0, #20]\n"
            "   2:	683f      	ldr	r7, [r7, #0]\n"
            "   4:	65c7      	str	r7, [r0, #92]	; 0x5c"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6947      	ldr	r7, [r0, #20]\n"
            "   2:	371c      	adds	r7, #28\n"
            "   4:	6147      	str	r7, [r0, #20]"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f00      	cmp	r7, #0\n"
                        "   6:	d500      	bpl.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	42b7      	cmp	r7, r6\n"
                        "   6:	da00      	bge.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f00      	cmp	r7, #0\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f00      	cmp	r7, #0\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f00      	cmp	r7, #0\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f00      	cmp	r7, #0\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f3b      	cmp	r7, #59	; 0x3b\n"
                        "   6:	d000      	beq.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f3b      	cmp	r7, #59	; 0x3b\n"
                        "   6:	d000      	beq.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f2a      	cmp	r7, #42	; 0x2a\n"
                        "   6:	d000      	beq.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f2a      	cmp	r7, #42	; 0x2a\n"
                        "   6:	d000      	beq.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f3b      	cmp	r7, #59	; 0x3b\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f3b      	cmp	r7, #59	; 0x3b\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f2a      	cmp	r7, #42	; 0x2a\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f2a      	cmp	r7, #42	; 0x2a\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	07fd      	lsls	r5, r7, #31\n"
                        "   6:	d400      	bmi.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	07fd      	lsls	r5, r7, #31\n"
                        "   6:	d400      	bmi.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	07fd      	lsls	r5, r7, #31\n"
                        "   6:	d500      	bpl.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	07fd      	lsls	r5, r7, #31\n"
                        "   6:	d500      	bpl.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	077d      	lsls	r5, r7, #29\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2505      	movs	r5, #5\n"
                        "   6:	422f      	tst	r7, r5\n"
                        "   8:	d000      	beq.n	0xc\n"
                        "   a:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	077d      	lsls	r5, r7, #29\n"
                        "   6:	d100      	bne.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	250f      	movs	r5, #15\n"
                        "   6:	402d      	ands	r5, r5\n"
                        "   8:	2d0f      	cmp	r5, #15\n"
                        "   a:	d000      	beq.n	0xe\n"
                        "   c:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	250f      	movs	r5, #15\n"
                        "   6:	402f      	ands	r7, r5\n"
                        "   8:	2f0f      	cmp	r7, #15\n"
                        "   a:	d000      	beq.n	0xe\n"
                        "   c:	3602      	adds	r6, #2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end)
            ]
        end}.

%% Test coverage for bitwise AND optimization paths
bitwise_and_optimization_test_() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 6}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 7}),
    [
        %% Test optimized case: 16#3 (low bits mask, 2 bits) - lsls r5, r7, #30
        ?_test(begin
            State3 = ?BACKEND:if_block(
                State2,
                {RegA, '&', 16#3, '!=', 0},
                fun(BSt0) ->
                    ?BACKEND:add(BSt0, RegB, 2)
                end
            ),
            Stream = ?BACKEND:stream(State3),
            Dump = <<
                "   0:	6b07      	ldr	r7, [r0, #48]	; 0x30\n"
                "   2:	6b46      	ldr	r6, [r0, #52]	; 0x34\n"
                "   4:	07bd      	lsls	r5, r7, #30\n"
                "   6:	d100      	bne.n	0xa\n"
                "   8:	3602      	adds	r6, #2"
            >>,
            ?assertEqual(dump_to_bin(Dump), Stream),
            ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State3))
        end),
        %% Test optimized case: 16#F (low bits mask, 4 bits) - lsls r5, r7, #28
        ?_test(begin
            State3 = ?BACKEND:if_block(
                State2,
                {RegA, '&', 16#F, '!=', 0},
                fun(BSt0) ->
                    ?BACKEND:add(BSt0, RegB, 2)
                end
            ),
            Stream = ?BACKEND:stream(State3),
            Dump = <<
                "   0:	6b07      	ldr	r7, [r0, #48]	; 0x30\n"
                "   2:	6b46      	ldr	r6, [r0, #52]	; 0x34\n"
                "   4:	073d      	lsls	r5, r7, #28\n"
                "   6:	d100      	bne.n	0xa\n"
                "   8:	3602      	adds	r6, #2"
            >>,
            ?assertEqual(dump_to_bin(Dump), Stream),
            ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State3))
        end),
        %% Test optimized case: 16#3F (low bits mask, 6 bits) - lsls r5, r7, #26
        ?_test(begin
            State3 = ?BACKEND:if_block(
                State2,
                {RegA, '&', 16#3F, '!=', 0},
                fun(BSt0) ->
                    ?BACKEND:add(BSt0, RegB, 2)
                end
            ),
            Stream = ?BACKEND:stream(State3),
            Dump = <<
                "   0:	6b07      	ldr	r7, [r0, #48]	; 0x30\n"
                "   2:	6b46      	ldr	r6, [r0, #52]	; 0x34\n"
                "   4:	06bd      	lsls	r5, r7, #26\n"
                "   6:	d100      	bne.n	0xa\n"
                "   8:	3602      	adds	r6, #2"
            >>,
            ?assertEqual(dump_to_bin(Dump), Stream),
            ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State3))
        end),
        %% Test non-optimized case: 5 (neither single bit nor low bits mask) - mov+tst
        ?_test(begin
            State3 = ?BACKEND:if_block(
                State2,
                {RegA, '&', 5, '!=', 0},
                fun(BSt0) ->
                    ?BACKEND:add(BSt0, RegB, 2)
                end
            ),
            Stream = ?BACKEND:stream(State3),
            Dump = <<
                "   0:	6b07      	ldr	r7, [r0, #48]	; 0x30\n"
                "   2:	6b46      	ldr	r6, [r0, #52]	; 0x34\n"
                "   4:	2505      	movs	r5, #5\n"
                "   6:	422f      	tst	r7, r5\n"
                "   8:	d000      	beq.n	0xc\n"
                "   a:	3602      	adds	r6, #2"
            >>,
            ?assertEqual(dump_to_bin(Dump), Stream),
            ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State3))
        end)
    ].

if_else_block_test_DISABLED() ->
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
            "   0:	f9401807 	ldr	x7, [x0, #48]\n"
            "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
            "   8:	f100ecff 	cmp	x7, #0x3b\n"
            "   c:	54000061 	b.ne	0x18  // b.any\n"
            "  10:	91000908 	add	x8, x8, #0x2\n"
            "  14:	14000002 	b	0x1c\n"
            "  18:	91001108 	add	x8, x8, #0x4"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_right(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	6987      	ldr	r7, [r0, #24]\n"
            "   2:	08ff      	lsrs	r7, r7, #3"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	6987      	ldr	r7, [r0, #24]\n"
            "   2:	00ff      	lsls	r7, r7, #3"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_only_or_schedule_next_and_label_relocation_test_DISABLED() ->
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
            "   c:	b9401027 	ldr	w7, [x1, #16]\n"
            "  10:	f10004e7 	subs	x7, x7, #0x1\n"
            "  14:	b9001027 	str	w7, [x1, #16]\n"
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

call_bif_with_large_literal_integer_test_DISABLED() ->
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
            "  80:	b5000087 	cbnz	x7, 0x90\n"
            "  84:	f9401847 	ldr	x7, [x2, #48]\n"
            "  88:	d2801102 	mov	x2, #0x88                  	// #136\n"
            "  8c:	d61f00e0 	br	x7\n"
            "  90:	f9001807 	str	x7, [x0, #48]"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

get_list_test_DISABLED() ->
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

is_integer_test_DISABLED() ->
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
        "   4:	92400ce8 	and	x8, x7, #0xf\n"
        "   8:	f1003d1f 	cmp	x8, #0xf\n"
        "   c:	54000160 	b.eq	0x38  // b.none\n"
        "  10:	924004e8 	and	x8, x7, #0x3\n"
        "  14:	f100091f 	cmp	x8, #0x2\n"
        "  18:	54000040 	b.eq	0x20  // b.none\n"
        "  1c:	14000047 	b	0x138\n"
        "  20:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "  24:	f94000e7 	ldr	x7, [x7]\n"
        "  28:	924014e7 	and	x7, x7, #0x3f\n"
        "  2c:	f10020ff 	cmp	x7, #0x8\n"
        "  30:	54000040 	b.eq	0x38  // b.none\n"
        "  34:	14000041 	b	0x138"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

is_number_test_DISABLED() ->
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
    Offset = ?BACKEND:offset(State3),
    Labels = [{Label, Offset + 16#100}],
    State4 = ?BACKEND:update_branches(State3, Labels),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
        "   4:	92400ce8 	and	x8, x7, #0xf\n"
        "   8:	f1003d1f 	cmp	x8, #0xf\n"
        "   c:	540001c0 	b.eq	0x44  // b.none\n"
        "  10:	924004e8 	and	x8, x7, #0x3\n"
        "  14:	f100091f 	cmp	x8, #0x2\n"
        "  18:	54000040 	b.eq	0x20  // b.none\n"
        "  1c:	1400004a 	b	0x144\n"
        "  20:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "  24:	f94000e7 	ldr	x7, [x7]\n"
        "  28:	924014e8 	and	x8, x7, #0x3f\n"
        "  2c:	f100211f 	cmp	x8, #0x8\n"
        "  30:	540000a0 	b.eq	0x44  // b.none\n"
        "  34:	924014e7 	and	x7, x7, #0x3f\n"
        "  38:	f10060ff 	cmp	x7, #0x18\n"
        "  3c:	54000040 	b.eq	0x44  // b.none\n"
        "  40:	14000041 	b	0x144"
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
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	2f4b      	cmp	r7, #75	; 0x4b\n"
        "   4:	d002      	beq.n	0xc\n"
        "   6:	2f0b      	cmp	r7, #11\n"
        "   8:	d000      	beq.n	0xc\n"
        "   a:	e07f      	b.n	0x10c"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_test_DISABLED() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401027 	ldr	w7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001027 	str	w7, [x1, #16]\n"
        "   c:	540000a1 	b.ne	0x20  // b.any\n"
        "  10:	10000087 	adr	x7, 0x20\n"
        "  14:	f9000427 	str	x7, [x1, #8]\n"
        "  18:	f9400847 	ldr	x7, [x2, #16]\n"
        "  1c:	d61f00e0 	br	x7\n"
        "  20:	f9400027 	ldr	x7, [x1]\n"
        "  24:	b94000e7 	ldr	w7, [x7]\n"
        "  28:	d3689ce7 	lsl	x7, x7, #24\n"
        "  2c:	d2802610 	mov	x16, #0x130                 	// #304\n"
        "  30:	aa1000e7 	orr	x7, x7, x16\n"
        "  34:	f9005c07 	str	x7, [x0, #184]\n"
        "  38:	f9401047 	ldr	x7, [x2, #32]\n"
        "  3c:	d2800042 	mov	x2, #0x2                   	// #2\n"
        "  40:	d28000a3 	mov	x3, #0x5                   	// #5\n"
        "  44:	92800004 	mov	x4, #0xffffffffffffffff    	// #-1\n"
        "  48:	d61f00e0 	br	x7"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_fun_test_DISABLED() ->
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
        "   0:	b9401027 	ldr	w7, [x1, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001027 	str	w7, [x1, #16]\n"
        "   c:	540000a1 	b.ne	0x20  // b.any\n"
        "  10:	10000087 	adr	x7, 0x20\n"
        "  14:	f9000427 	str	x7, [x1, #8]\n"
        "  18:	f9400847 	ldr	x7, [x2, #16]\n"
        "  1c:	d61f00e0 	br	x7\n"
        "  20:	f9401807 	ldr	x7, [x0, #48]\n"
        "  24:	aa0703e8 	mov	x8, x7\n"
        "  28:	92400509 	and	x9, x8, #0x3\n"
        "  2c:	f100093f 	cmp	x9, #0x2\n"
        "  30:	540000c0 	b.eq	0x48  // b.none\n"
        "  34:	f9404c47 	ldr	x7, [x2, #152]\n"
        "  38:	d2800702 	mov	x2, #0x38                  	// #56\n"
        "  3c:	d2804163 	mov	x3, #0x20b                 	// #523\n"
        "  40:	aa0803e4 	mov	x4, x8\n"
        "  44:	d61f00e0 	br	x7\n"
        "  48:	927ef508 	and	x8, x8, #0xfffffffffffffffc\n"
        "  4c:	f9400108 	ldr	x8, [x8]\n"
        "  50:	92401509 	and	x9, x8, #0x3f\n"
        "  54:	f100513f 	cmp	x9, #0x14\n"
        "  58:	540000c0 	b.eq	0x70  // b.none\n"
        "  5c:	f9404c47 	ldr	x7, [x2, #152]\n"
        "  60:	d2800c02 	mov	x2, #0x60                  	// #96\n"
        "  64:	d2804163 	mov	x3, #0x20b                 	// #523\n"
        "  68:	aa0803e4 	mov	x4, x8\n"
        "  6c:	d61f00e0 	br	x7\n"
        "  70:	f9400028 	ldr	x8, [x1]\n"
        "  74:	b9400108 	ldr	w8, [x8]\n"
        "  78:	d3689d08 	lsl	x8, x8, #24\n"
        "  7c:	d2804c10 	mov	x16, #0x260                 	// #608\n"
        "  80:	aa100108 	orr	x8, x8, x16\n"
        "  84:	f9005c08 	str	x8, [x0, #184]\n"
        "  88:	f9408048 	ldr	x8, [x2, #256]\n"
        "  8c:	aa0703e2 	mov	x2, x7\n"
        "  90:	d2800003 	mov	x3, #0x0                   	// #0\n"
        "  94:	d61f0100 	br	x8"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    Stream = ?BACKEND:stream(State1),
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
                        "   0:	2700      	movs	r7, #0\n"
                        "   2:	6187      	str	r7, [r0, #24]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	2700      	movs	r7, #0\n"
                        "   2:	6587      	str	r7, [r0, #88]	; 0x58"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, r6}, <<
                        "   0:	2700      	movs	r7, #0\n"
                        "   2:	6037      	str	r7, [r6, #0]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	2600      	movs	r6, #0\n"
                        "   4:	60be      	str	r6, [r7, #8]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	2600      	movs	r6, #0\n"
                        "   4:	653e      	str	r6, [r7, #80]	; 0x50"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	272a      	movs	r7, #42	; 0x2a\n"
                        "   2:	6187      	str	r7, [r0, #24]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	272a      	movs	r7, #42	; 0x2a\n"
                        "   2:	6587      	str	r7, [r0, #88]	; 0x58"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	262a      	movs	r6, #42	; 0x2a\n"
                        "   4:	60be      	str	r6, [r7, #8]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	262a      	movs	r6, #42	; 0x2a\n"
                        "   4:	653e      	str	r6, [r7, #80]	; 0x50"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, r3}, <<
                        "   0:	2763      	movs	r7, #99	; 0x63\n"
                        "   2:	601f      	str	r7, [r3, #0]"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	69c7      	ldr	r7, [r0, #28]\n"
                        "   2:	6207      	str	r7, [r0, #32]"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, r1}, <<
                        "   0:	69c7      	ldr	r7, [r0, #28]\n"
                        "   2:	600f      	str	r7, [r1, #0]"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, r4}, {x_reg, 3}, <<
                        "   0:	6827      	ldr	r7, [r4, #0]\n"
                        "   2:	6247      	str	r7, [r0, #36]	; 0x24"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	6946      	ldr	r6, [r0, #20]\n"
                        "   4:	6077      	str	r7, [r6, #4]"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	683f      	ldr	r7, [r7, #0]\n"
                        "   4:	6247      	str	r7, [r0, #36]	; 0x24"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	687f      	ldr	r7, [r7, #4]\n"
                        "   4:	6247      	str	r7, [r0, #36]	; 0x24"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r5, {x_reg, 0}, <<
                        "   0:	6185      	str	r5, [r0, #24]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, r6, {x_reg, extra}, <<
                        "   0:	6586      	str	r6, [r0, #88]	; 0x58"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, r4, {ptr, r3}, <<
                        "   0:	601c      	str	r4, [r3, #0]"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r1, {y_reg, 0}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	6039      	str	r1, [r7, #0]"
                    >>)
                end),
                %% Test: Large immediate to x_reg (32-bit literal pool, aligned case)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, 0}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   6:	1234      	asrs	r4, r6, #8\n"
                        "   8:	6187      	str	r7, [r0, #24]"
                    >>)
                end),
                %% Test: Large immediate to x_reg (32-bit literal pool, unaligned case)
                ?_test(begin
                    %% First do a 2-byte instruction to create unaligned start
                    State1 = ?BACKEND:move_to_vm_register(State0, r1, {ptr, r3}),
                    %% Then do large immediate which should handle unaligned case
                    State2 = ?BACKEND:move_to_vm_register(State1, 16#12345678, {x_reg, 0}),
                    Stream = ?BACKEND:stream(State2),
                    Expected = dump_to_bin(<<
                        "   0:	6019      	str	r1, [r3, #0]\n"
                        "   2:	4f01      	ldr	r7, [pc, #4]	; (0x8)\n"
                        "   4:	e002      	b.n	0xc\n"
                        "   6:	0000      	movs	r0, r0\n"
                        "   8:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   a:	1234      	asrs	r4, r6, #8\n"
                        "   c:	6187      	str	r7, [r0, #24]"
                    >>),
                    ?assertEqual(Expected, Stream)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, extra}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   6:	1234      	asrs	r4, r6, #8\n"
                        "   8:	6587      	str	r7, [r0, #88]	; 0x58"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 2}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   6:	1234      	asrs	r4, r6, #8\n"
                        "   8:	6946      	ldr	r6, [r0, #20]\n"
                        "   a:	60b7      	str	r7, [r6, #8]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 20}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   6:	1234      	asrs	r4, r6, #8\n"
                        "   8:	6946      	ldr	r6, [r0, #20]\n"
                        "   a:	6537      	str	r7, [r6, #80]	; 0x50"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {ptr, r3}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	5678      	ldrsb	r0, [r7, r1]\n"
                        "   6:	1234      	asrs	r4, r6, #8\n"
                        "   8:	601f      	str	r7, [r3, #0]"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	6d47      	ldr	r7, [r0, #84]	; 0x54\n"
                        "   2:	6946      	ldr	r6, [r0, #20]\n"
                        "   4:	67f7      	str	r7, [r6, #124]	; 0x7c"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	6fff      	ldr	r7, [r7, #124]	; 0x7c\n"
                        "   4:	6547      	str	r7, [r0, #84]	; 0x54"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	ffff ffff 			; <UNDEFINED> instruction: 0xffffffff\n"
                        "   8:	6187      	str	r7, [r0, #24]"
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
                    move_array_element_test0(State0, r3, 2, {x_reg, 0}, <<
                        "   0:	689f      	ldr	r7, [r3, #8]\n"
                        "   2:	6187      	str	r7, [r0, #24]"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r3, 3, {ptr, r5}, <<
                        "   0:	68df      	ldr	r7, [r3, #12]\n"
                        "   2:	602f      	str	r7, [r5, #0]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, {y_reg, 2}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	685e      	ldr	r6, [r3, #4]\n"
                        "   4:	60be      	str	r6, [r7, #8]"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (r5)
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, r5, <<
                        "   0:	685d      	ldr	r5, [r3, #4]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 7, {y_reg, 31}, <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	69de      	ldr	r6, [r3, #28]\n"
                        "   4:	67fe      	str	r6, [r7, #124]	; 0x7c"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 7, {x_reg, 15}, <<
                        "   0:	69df      	ldr	r7, [r3, #28]\n"
                        "   2:	6547      	str	r7, [r0, #84]	; 0x54"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {x_reg, 2}, <<
                        "   0:	691f      	ldr	r7, [r3, #16]\n"
                        "   2:	00bf      	lsls	r7, r7, #2\n"
                        "   4:	59df      	ldr	r7, [r3, r7]\n"
                        "   6:	6207      	str	r7, [r0, #32]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {ptr, r5}, <<
                        "   0:	691f      	ldr	r7, [r3, #16]\n"
                        "   2:	00bf      	lsls	r7, r7, #2\n"
                        "   4:	59df      	ldr	r7, [r3, r7]\n"
                        "   6:	602f      	str	r7, [r5, #0]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {y_reg, 31}, <<
                        "   0:	691f      	ldr	r7, [r3, #16]\n"
                        "   2:	00bf      	lsls	r7, r7, #2\n"
                        "   4:	6946      	ldr	r6, [r0, #20]\n"
                        "   6:	59df      	ldr	r7, [r3, r7]\n"
                        "   8:	67f7      	str	r7, [r6, #124]	; 0x7c"
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
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r4, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6927      	ldr	r7, [r4, #16]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual(r7, Reg)
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
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	609f      	str	r7, [r3, #8]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	1c26      	adds	r6, r4, #0\n"
                        "   4:	00b6      	lsls	r6, r6, #2\n"
                        "   6:	519f      	str	r7, [r3, r6]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, r7}, r3, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	683f      	ldr	r7, [r7, #0]\n"
                        "   2:	1c26      	adds	r6, r4, #0\n"
                        "   4:	00b6      	lsls	r6, r6, #2\n"
                        "   6:	519f      	str	r7, [r3, r6]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, r3, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	68bf      	ldr	r7, [r7, #8]\n"
                        "   4:	1c26      	adds	r6, r4, #0\n"
                        "   6:	00b6      	lsls	r6, r6, #2\n"
                        "   8:	519f      	str	r7, [r3, r6]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	609f      	str	r7, [r3, #8]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [r3, r4]),
                    State2 = setelement(7, State1, [r3, r4]),
                    [r3, r4] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, r3, r4, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	1c66      	adds	r6, r4, #1\n"
                        "   4:	00b6      	lsls	r6, r6, #2\n"
                        "   6:	519f      	str	r7, [r3, r6]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [r3, r4]),
                    State2 = setelement(7, State1, [r3, r4]),
                    [r3, r4] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, r3, r4, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	272a      	movs	r7, #42	; 0x2a\n"
                        "   2:	1c66      	adds	r6, r4, #1\n"
                        "   4:	00b6      	lsls	r6, r6, #2\n"
                        "   6:	519f      	str	r7, [r3, r6]"
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
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	272a      	movs	r7, #42	; 0x2a"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, r6}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	6836      	ldr	r6, [r6, #0]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	6a47      	ldr	r7, [r0, #36]	; 0x24"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	68ff      	ldr	r7, [r7, #12]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {fp_reg, N} - DISABLED for ARMv6-M (no FPU)
                %% ?_test(begin
                %%     {State1, Reg} = ?BACKEND:move_to_native_register(State0, {fp_reg, 3}),
                %%     Stream = ?BACKEND:stream(State1),
                %%     ?assertEqual(v0, Reg),
                %%     Dump = <<
                %%         "   0:	f9406007 	ldr	x7, [x0, #192]\n"
                %%         "   4:	fd400ce0 	ldr	d0, [x7, #24]"
                %%     >>,
                %%     ?assertEqual(dump_to_bin(Dump), Stream)
                %% end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, r6),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	262a      	movs	r6, #42	; 0x2a"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, r7, r5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	1c3d      	adds	r5, r7, #0"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, r7}, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	683c      	ldr	r4, [r7, #0]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6a03      	ldr	r3, [r0, #32]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6941      	ldr	r1, [r0, #20]\n"
                        "   2:	6889      	ldr	r1, [r1, #8]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
                %% move_to_native_register/3: {fp_reg, N} - DISABLED for ARMv6-M (no FPU)
                %% ?_test(begin
                %%     State1 = ?BACKEND:move_to_native_register(State0, {fp_reg, 3}, v0),
                %%     Stream = ?BACKEND:stream(State1),
                %%     Dump = <<
                %%         "   0:	f9406007 	ldr	x7, [x0, #192]\n"
                %%         "   4:	fd400ce0 	ldr	d0, [x7, #24]"
                %%     >>,
                %%     ?assertEqual(dump_to_bin(Dump), Stream)
                %% end)
            ]
        end}.

mul_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:mul(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(dump_to_bin(Dump), Stream).

mul_test_DISABLED_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    mul_test0(State0, r2, 2, <<
                        "0:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 3, <<
                        "   0:	d37ff847 	lsl	x7, x2, #1\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 4, <<
                        "0:	d37ef442 	lsl	x2, x2, #2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 5, <<
                        "   0:	d37ef447 	lsl	x7, x2, #2\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 6, <<
                        "   0:	d37ff847 	lsl	x7, x2, #1\n"
                        "   4:	8b0200e2 	add	x2, x7, x2\n"
                        "   8:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 7, <<
                        "   0:	d37df047 	lsl	x7, x2, #3\n"
                        "   4:	cb0200e2 	sub	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 8, <<
                        "0:	d37df042 	lsl	x2, x2, #3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 9, <<
                        "   0:	d37df047 	lsl	x7, x2, #3\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 10, <<
                        "   0:	d37ef447 	lsl	x7, x2, #2\n"
                        "   4:	8b0200e2 	add	x2, x7, x2\n"
                        "   8:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 11, <<
                        "   0:	d2800167 	mov	x7, #0xb                   	// #11\n"
                        "   4:	9b077c42 	mul	x2, x2, x7"
                    >>)
                end)
            ]
        end}.

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
%% Handle 32-bits undefined instruction
dump_to_bin0(<<H1, H2, H3, H4, $\s, H5, H6, H7, H8, Sp, Rest/binary>>, hex, Acc) when
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
    InstrA = list_to_integer([H1, H2, H3, H4], 16),
    InstrB = list_to_integer([H5, H6, H7, H8], 16),
    dump_to_bin0(Rest, instr, [<<InstrA:16/little>>, <<InstrB:16/little>> | Acc]);
%% Handle 16-bit ARM32 Thumb instructions (4 hex digits)
dump_to_bin0(<<H1, H2, H3, H4, Sp, Rest/binary>>, hex, Acc) when
    (Sp =:= $\t orelse Sp =:= $\s) andalso
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
