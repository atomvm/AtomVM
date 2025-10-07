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
            "   0:	f9400050 	ldr	x16, [x2]\n"
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
            "   0:	f9400450 	ldr	x16, [x2, #8]\n"
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

call_ext_last_test() ->
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	b6f80047 	tbz	x7, #63, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	eb0800ff 	cmp	x7, x8\n"
                        "   c:	5400004a 	b.ge	0x14  // b.tcont\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	b5000047 	cbnz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	b5000047 	cbnz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	35000047 	cbnz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	35000047 	cbnz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000041 	b.ne	0x14  // b.any\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000041 	b.ne	0x14  // b.any\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000041 	b.ne	0x14  // b.any\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000041 	b.ne	0x14  // b.any\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	37000047 	tbnz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	37000047 	tbnz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	36000047 	tbz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	36000047 	tbz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f24008ff 	tst	x7, #0x7\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	d28000a9 	mov	x9, #0x5                   	// #5\n"
                        "   c:	ea0900ff 	tst	x7, x9\n"
                        "  10:	54000040 	b.eq	0x18  // b.none\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	f24008ff 	tst	x7, #0x7\n"
                        "   c:	54000040 	b.eq	0x14  // b.none\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	92400ce9 	and	x9, x7, #0xf\n"
                        "   c:	f1003d3f 	cmp	x9, #0xf\n"
                        "  10:	54000040 	b.eq	0x18  // b.none\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
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
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401c08 	ldr	x8, [x0, #56]\n"
                        "   8:	92400ce7 	and	x7, x7, #0xf\n"
                        "   c:	f1003cff 	cmp	x7, #0xf\n"
                        "  10:	54000040 	b.eq	0x18  // b.none\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
            "   0:	f9401807 	ldr	x7, [x0, #48]\n"
            "   4:	d343fce7 	lsr	x7, x7, #3"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	f9401807 	ldr	x7, [x0, #48]\n"
            "   4:	d37df0e7 	lsl	x7, x7, #3"
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

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, {avm_int64_t, 9208452466117618637}]),
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
    State4 = ?BACKEND:add_label(State3, Label, Offset + 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
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
    Offset = ?BACKEND:offset(State3),
    State4 = ?BACKEND:add_label(State3, Label, Offset + 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
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
    ?BACKEND:assert_all_native_free(State3),
    Offset = ?BACKEND:offset(State3),
    State4 = ?BACKEND:add_label(State3, Label, Offset + 16#100),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Offset = ?BACKEND:offset(State3),
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
        "  3c:	d2803163 	mov	x3, #0x18b                 	// #395\n"
        "  40:	aa0803e4 	mov	x4, x8\n"
        "  44:	d61f00e0 	br	x7\n"
        "  48:	927ef508 	and	x8, x8, #0xfffffffffffffffc\n"
        "  4c:	f9400108 	ldr	x8, [x8]\n"
        "  50:	92401509 	and	x9, x8, #0x3f\n"
        "  54:	f100513f 	cmp	x9, #0x14\n"
        "  58:	540000c0 	b.eq	0x70  // b.none\n"
        "  5c:	f9404c47 	ldr	x7, [x2, #152]\n"
        "  60:	d2800c02 	mov	x2, #0x60                  	// #96\n"
        "  64:	d2803163 	mov	x3, #0x18b                 	// #395\n"
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
                        "   0:	f900181f 	str	xzr, [x0, #48]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	f900581f 	str	xzr, [x0, #176]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, r10}, <<
                        "   0:	f900015f 	str	xzr, [x10]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f90008ff 	str	xzr, [x7, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f90050ff 	str	xzr, [x7, #160]"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	f9001807 	str	x7, [x0, #48]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	f9005807 	str	x7, [x0, #176]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   8:	f9000907 	str	x7, [x8, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   8:	f9005107 	str	x7, [x8, #160]"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, r10}, <<
                        "   0:	d2800c67 	mov	x7, #0x63                  	// #99\n"
                        "   4:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	f9401c07 	ldr	x7, [x0, #56]\n"
                        "   4:	f9002007 	str	x7, [x0, #64]"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, r8}, <<
                        "   0:	f9401c07 	ldr	x7, [x0, #56]\n"
                        "   4:	f9000107 	str	x7, [x8]"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, r9}, {x_reg, 3}, <<
                        "   0:	f9400127 	ldr	x7, [x9]\n"
                        "   4:	f9002407 	str	x7, [x0, #72]"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   8:	f9000507 	str	x7, [x8, #8]"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f94000e7 	ldr	x7, [x7]\n"
                        "   8:	f9002407 	str	x7, [x0, #72]"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f94004e7 	ldr	x7, [x7, #8]\n"
                        "   8:	f9002407 	str	x7, [x0, #72]"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {x_reg, 0}, <<
                        "   0:	f900180a 	str	x10, [x0, #48]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {x_reg, extra}, <<
                        "   0:	f900580a 	str	x10, [x0, #176]"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, r9, {ptr, r10}, <<
                        "   0:	f9000149 	str	x9, [x10]"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {y_reg, 0}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f90000ea 	str	x10, [x7]"
                    >>)
                end),
                %% Test: Large immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, 0}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9001807 	str	x7, [x0, #48]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, extra}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9005807 	str	x7, [x0, #176]\n"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 2}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9401408 	ldr	x8, [x0, #40]\n"
                        "  14:	f9000907 	str	x7, [x8, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 20}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9401408 	ldr	x8, [x0, #40]\n"
                        "  14:	f9005107 	str	x7, [x8, #160]"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {ptr, r10}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	f9405407 	ldr	x7, [x0, #168]\n"
                        "   4:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   8:	f9007d07 	str	x7, [x8, #248]"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f9407ce7 	ldr	x7, [x7, #248]\n"
                        "   8:	f9005407 	str	x7, [x0, #168]"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	92800007 	mov	x7, #0xffffffffffffffff    	// #-1\n"
                        "   4:	f9001807 	str	x7, [x0, #48]"
                    >>)
                end),
                %% Test: ptr with offset to fp_reg (term_to_float)
                ?_test(begin
                    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    State2 = ?BACKEND:move_to_vm_register(
                        State1, {free, {ptr, RegA, 1}}, {fp_reg, 3}
                    ),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f94004e7 	ldr	x7, [x7, #8]\n"
                        "   8:	f9406008 	ldr	x8, [x0, #192]\n"
                        "   c:	f9000d07 	str	x7, [x8, #24]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
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
                    move_array_element_test0(State0, r8, 2, {x_reg, 0}, <<
                        "   0:	f9400907 	ldr	x7, [x8, #16]\n"
                        "   4:	f9001807 	str	x7, [x0, #48]"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r8, 3, {ptr, r10}, <<
                        "   0:	f9400d07 	ldr	x7, [x8, #24]\n"
                        "   4:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, {y_reg, 2}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f9400508 	ldr	x8, [x8, #8]\n"
                        "   8:	f90008e8 	str	x8, [x7, #16]"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (r10)
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, r10, <<
                        "   0:	f940050a 	ldr	x10, [x8, #8]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {y_reg, 31}, <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f9401d08 	ldr	x8, [x8, #56]\n"
                        "   8:	f9007ce8 	str	x8, [x7, #248]"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {x_reg, 15}, <<
                        "   0:	f9401d07 	ldr	x7, [x8, #56]\n"
                        "   4:	f9005407 	str	x7, [x0, #168]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {x_reg, 2}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   8:	f9002007 	str	x7, [x0, #64]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {ptr, r10}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   8:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {y_reg, 31}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   8:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   c:	f9007d07 	str	x7, [x8, #248]"
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
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]"
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
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9000907 	str	x7, [x8, #16]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "  0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, r7}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f94000e7 	ldr	x7, [x7]\n"
                        "   4:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f94008e7 	ldr	x7, [x7, #16]\n"
                        "   8:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	f9000d07 	str	x7, [x8, #24]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [r8, r9]),
                    State2 = setelement(7, State1, [r8, r9]),
                    [r8, r9] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, r8, r9, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	f9401807 	ldr	x7, [x0, #48]\n"
                        "   4:	9100052a 	add	x10, x9, #0x1\n"
                        "   8:	f82a7907 	str	x7, [x8, x10, lsl #3]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    State1 = setelement(6, State0, ?BACKEND:available_regs(State0) -- [r8, r9]),
                    State2 = setelement(7, State1, [r8, r9]),
                    [r8, r9] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, r8, r9, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	9100052a 	add	x10, x9, #0x1\n"
                        "   8:	f82a7907 	str	x7, [x8, x10, lsl #3]"
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
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, r6}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	f94000c6 	ldr	x6, [x6]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	f9402407 	ldr	x7, [x0, #72]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	f9401407 	ldr	x7, [x0, #40]\n"
                        "   4:	f9400ce7 	ldr	x7, [x7, #24]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	d2800548 	mov	x8, #0x2a                  	// #42"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, r7, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	aa0703e8 	mov	x8, x7"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, r7}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f94000e8 	ldr	x8, [x7]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402008 	ldr	x8, [x0, #64]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401408 	ldr	x8, [x0, #40]\n"
                        "   4:	f9400908 	ldr	x8, [x8, #16]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
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
