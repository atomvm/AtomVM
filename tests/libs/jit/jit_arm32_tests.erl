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

-module(jit_arm32_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_arm32).

% disassembly obtained with:
%  arm-elf-objdump -D -b binary -marm -z

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual([], ?BACKEND:used_regs(State)),
    Available = ?BACKEND:available_regs(State),
    ?assertEqual(10, length(Available)),
    ?assert(lists:member(r1, Available)),
    ?assert(lists:member(r3, Available)),
    ?assert(lists:member(r4, Available)),
    ?assert(lists:member(r5, Available)),
    ?assert(lists:member(r6, Available)),
    ?assert(lists:member(r7, Available)),
    ?assert(lists:member(r8, Available)),
    ?assert(lists:member(r9, Available)),
    ?assert(lists:member(r10, Available)),
    ?assert(lists:member(r11, Available)).

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e592b000 	ldr	fp, [r2]\n"
            "   4:	e92d0005 	push	{r0, r2}\n"
            "   8:	e59d1008 	ldr	r1, [sp, #8]\n"
            "   c:	e12fff3b 	blx	fp\n"
            "  10:	e1a0b000 	mov	fp, r0\n"
            "  14:	e8bd0005 	pop	{r0, r2}"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e592b004 	ldr	fp, [r2, #4]\n"
            "   4:	e92d0005 	push	{r0, r2}\n"
            "   8:	e59d1008 	ldr	r1, [sp, #8]\n"
            "   c:	e12fff3b 	blx	fp\n"
            "  10:	e1a0b000 	mov	fp, r0\n"
            "  14:	e8bd0005 	pop	{r0, r2}"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e592b014 	ldr	fp, [r2, #20]\n"
            "   4:	e24dd008 	sub	sp, sp, #8\n"
            "   8:	e3a0a002 	mov	sl, #2\n"
            "   c:	e58da000 	str	sl, [sp]\n"
            "  10:	e59d1008 	ldr	r1, [sp, #8]\n"
            "  14:	e3a02010 	mov	r2, #16\n"
            "  18:	e3a03020 	mov	r3, #32\n"
            "  1c:	e12fff3b 	blx	fp\n"
            "  20:	e28dd008 	add	sp, sp, #8\n"
            "  24:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_6_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, ?TERM_PRIMARY_CLEAR_MASK),
    {State3, OtherReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    {State4, _ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, RegA}, 64, 8, {free, OtherReg}
    ]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e3a0a003 	mov	sl, #3\n"
            "   8:	e1cbb00a 	bic	fp, fp, sl\n"
            "   c:	e590a01c 	ldr	sl, [r0, #28]\n"
            "  10:	e59290b8 	ldr	r9, [r2, #184]	@ 0xb8\n"
            "  14:	e92d0005 	push	{r0, r2}\n"
            "  18:	e24dd008 	sub	sp, sp, #8\n"
            "  1c:	e58da004 	str	sl, [sp, #4]\n"
            "  20:	e3a0a008 	mov	sl, #8\n"
            "  24:	e58da000 	str	sl, [sp]\n"
            "  28:	e59d1010 	ldr	r1, [sp, #16]\n"
            "  2c:	e1a0200b 	mov	r2, fp\n"
            "  30:	e3a03040 	mov	r3, #64	@ 0x40\n"
            "  34:	e12fff39 	blx	r9\n"
            "  38:	e1a09000 	mov	r9, r0\n"
            "  3c:	e28dd008 	add	sp, sp, #8\n"
            "  40:	e8bd0005 	pop	{r0, r2}"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_to_vm_register_x_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e580b01c 	str	fp, [r0, #28]"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_to_vm_register_y_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {y_reg, 0}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e590a014 	ldr	sl, [r0, #20]\n"
            "   8:	e58ab000 	str	fp, [sl]"
        >>,
    ?assertStream(arm32, Dump, Stream).

jump_table_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    Stream = ?BACKEND:stream(State1),
    % 4 entries (0..3) * 8 bytes each = 32 bytes
    ?assertEqual(32, byte_size(Stream)).

add_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    ?assertEqual(32, ?BACKEND:offset(State3)).

and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:and_(State1, {free, Reg}, 16#FC),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e3a0a0fc 	mov	sl, #252	@ 0xfc\n"
            "   8:	e00bb00a 	and	fp, fp, sl"
        >>,
    ?assertStream(arm32, Dump, Stream).

or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:or_(State1, Reg, 16#0F),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e3a0a00f 	mov	sl, #15\n"
            "   8:	e18bb00a 	orr	fp, fp, sl"
        >>,
    ?assertStream(arm32, Dump, Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 2),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e1a0b10b 	lsl	fp, fp, #2"
        >>,
    ?assertStream(arm32, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 4),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                    "   4:	e1a0b22b 	lsr	fp, fp, #4"
                >>,
            ?assertStream(arm32, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 4),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                    "   4:	e1a0a22b 	lsr	sl, fp, #4"
                >>,
            ?assertStream(arm32, Dump, Stream)
        end)
    ].

add_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:add(State1, Reg, 42),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e28bb02a 	add	fp, fp, #42	@ 0x2a"
        >>,
    ?assertStream(arm32, Dump, Stream).

sub_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:sub(State1, Reg, 42),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e24bb02a 	sub	fp, fp, #42	@ 0x2a"
        >>,
    ?assertStream(arm32, Dump, Stream).

decrement_reductions_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "   8:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  14:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "  18:	e59da000 	ldr	sl, [sp]\n"
            "  1c:	e59ab008 	ldr	fp, [sl, #8]\n"
            "  20:	e25bb001 	subs	fp, fp, #1\n"
            "  24:	e58ab008 	str	fp, [sl, #8]\n"
            "  28:	1a000007 	bne	0x4c\n"
            "  2c:	e28fb014 	add	fp, pc, #20\n"
            "  30:	e58ab004 	str	fp, [sl, #4]\n"
            "  34:	e592b008 	ldr	fp, [r2, #8]\n"
            "  38:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  3c:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  40:	e1a0e007 	mov	lr, r7\n"
            "  44:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  48:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}"
        >>,
    ?assertStream(arm32, Dump, Stream).

debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    % BKPT is a single 4-byte ARM instruction
    ?assertEqual(4, byte_size(Stream)).

flush_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:flush(State0),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(0, byte_size(Stream)).

xor_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:xor_(State1, Reg, 16#FF),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e3a0a0ff 	mov	sl, #255	@ 0xff\n"
            "   8:	e02bb00a 	eor	fp, fp, sl"
        >>,
    ?assertStream(arm32, Dump, Stream).

mul_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:mul(State1, Reg, 4),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e1a0b10b 	lsl	fp, fp, #2"
        >>,
    ?assertStream(arm32, Dump, Stream).

return_if_not_equal_to_ctx_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [ctx, jit_state]
                    ),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	e592b054 	ldr	fp, [r2, #84]	@ 0x54\n"
                            "   4:	e92d0005 	push	{r0, r2}\n"
                            "   8:	e59d1008 	ldr	r1, [sp, #8]\n"
                            "   c:	e12fff3b 	blx	fp\n"
                            "  10:	e1a0b000 	mov	fp, r0\n"
                            "  14:	e8bd0005 	pop	{r0, r2}\n"
                            "  18:	e15b0000 	cmp	fp, r0\n"
                            "  1c:	0a000001 	beq	0x28\n"
                            "  20:	e1a0000b 	mov	r0, fp\n"
                            "  24:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [ctx, jit_state]
                    ),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	e592b054 	ldr	fp, [r2, #84]	@ 0x54\n"
                            "   4:	e92d0005 	push	{r0, r2}\n"
                            "   8:	e59d1008 	ldr	r1, [sp, #8]\n"
                            "   c:	e12fff3b 	blx	fp\n"
                            "  10:	e1a0b000 	mov	fp, r0\n"
                            "  14:	e8bd0005 	pop	{r0, r2}\n"
                            "  18:	e1a0a00b 	mov	sl, fp\n"
                            "  1c:	e15a0000 	cmp	sl, r0\n"
                            "  20:	0a000001 	beq	0x2c\n"
                            "  24:	e1a0000a 	mov	r0, sl\n"
                            "  28:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e590a014 	ldr	sl, [r0, #20]\n"
            "   4:	e59ab000 	ldr	fp, [sl]\n"
            "   8:	e580b05c 	str	fp, [r0, #92]	@ 0x5c"
        >>,
    ?assertStream(arm32, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e590b014 	ldr	fp, [r0, #20]\n"
            "   4:	e28bb01c 	add	fp, fp, #28\n"
            "   8:	e580b014 	str	fp, [r0, #20]"
        >>,
    ?assertStream(arm32, Dump, Stream).

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
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b0000 	cmp	fp, #0\n"
                            "   c:	5a000000 	bpl	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', RegB},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e15b000a 	cmp	fp, sl\n"
                            "   c:	aa000000 	bge	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 42},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b002a 	cmp	fp, #42	@ 0x2a\n"
                            "   c:	aa000000 	bge	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 1024},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e3a09b01 	mov	r9, #1024	@ 0x400\n"
                            "   c:	e15b0009 	cmp	fp, r9\n"
                            "  10:	aa000000 	bge	0x18\n"
                            "  14:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b0000 	cmp	fp, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b0000 	cmp	fp, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b0000 	cmp	fp, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', ?TERM_NIL},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b003b 	cmp	fp, #59	@ 0x3b\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '==', false},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e31b0001 	tst	fp, #1\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '!=', false},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e31b0001 	tst	fp, #1\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#F, '!=', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e31b000f 	tst	fp, #15\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'and', [{RegA, '<', 0}, {RegB, '==', 0}]},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
                            "   8:	e35b0000 	cmp	fp, #0\n"
                            "   c:	5a000002 	bpl	0x1c\n"
                            "  10:	e35a0000 	cmp	sl, #0\n"
                            "  14:	1a000000 	bne	0x1c\n"
                            "  18:	e28aa002 	add	sl, sl, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
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
        fun(BSt0) -> ?BACKEND:add(BSt0, Reg2, 2) end,
        fun(BSt0) -> ?BACKEND:add(BSt0, Reg2, 4) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
            "   8:	e35b003b 	cmp	fp, #59	@ 0x3b\n"
            "   c:	1a000001 	bne	0x18\n"
            "  10:	e28aa002 	add	sl, sl, #2\n"
            "  14:	ea000000 	b	0x1c\n"
            "  18:	e28aa004 	add	sl, sl, #4"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e59da000 	ldr	sl, [sp]\n"
            "   4:	e59ab008 	ldr	fp, [sl, #8]\n"
            "   8:	e25bb001 	subs	fp, fp, #1\n"
            "   c:	e58ab008 	str	fp, [sl, #8]\n"
            "  10:	1a000007 	bne	0x34\n"
            "  14:	e28fb014 	add	fp, pc, #20\n"
            "  18:	e58ab004 	str	fp, [sl, #4]\n"
            "  1c:	e592b008 	ldr	fp, [r2, #8]\n"
            "  20:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  24:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  28:	e1a0e007 	mov	lr, r7\n"
            "  2c:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  30:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  34:	e592b010 	ldr	fp, [r2, #16]\n"
            "  38:	e24dd008 	sub	sp, sp, #8\n"
            "  3c:	e3e0a000 	mvn	sl, #0\n"
            "  40:	e58da004 	str	sl, [sp, #4]\n"
            "  44:	e3a0a002 	mov	sl, #2\n"
            "  48:	e58da000 	str	sl, [sp]\n"
            "  4c:	e59d1008 	ldr	r1, [sp, #8]\n"
            "  50:	e3a02038 	mov	r2, #56	@ 0x38\n"
            "  54:	e3a03002 	mov	r3, #2\n"
            "  58:	e12fff3b 	blx	fp\n"
            "  5c:	e28dd008 	add	sp, sp, #8\n"
            "  60:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   4:	ea000017 	b	0x68\n"
            "   8:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  14:	ea00000e 	b	0x54\n"
            "  18:	e59da000 	ldr	sl, [sp]\n"
            "  1c:	e59ab008 	ldr	fp, [sl, #8]\n"
            "  20:	e25bb001 	subs	fp, fp, #1\n"
            "  24:	e58ab008 	str	fp, [sl, #8]\n"
            "  28:	1a000009 	bne	0x54\n"
            "  2c:	e1a0b00f 	mov	fp, pc\n"
            "  30:	e3e0a023 	mvn	sl, #35	@ 0x23\n"
            "  34:	e08aa00b 	add	sl, sl, fp\n"
            "  38:	e59db000 	ldr	fp, [sp]\n"
            "  3c:	e58ba004 	str	sl, [fp, #4]\n"
            "  40:	e592b008 	ldr	fp, [r2, #8]\n"
            "  44:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  48:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  4c:	e1a0e007 	mov	lr, r7\n"
            "  50:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  54:	e592b000 	ldr	fp, [r2]\n"
            "  58:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  5c:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  60:	e1a0e007 	mov	lr, r7\n"
            "  64:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  68:	e592b004 	ldr	fp, [r2, #4]\n"
            "  6c:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  70:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  74:	e1a0e007 	mov	lr, r7\n"
            "  78:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:add_label(State4, 0),
    State6 = ?BACKEND:call_primitive_last(State5, 1, [ctx, jit_state]),
    State7 = ?BACKEND:update_branches(State6),
    Stream = ?BACKEND:stream(State7),
    Dump =
        <<
            "   0:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   4:	ea000012 	b	0x54\n"
            "   8:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  14:	eaffffff 	b	0x18\n"
            "  18:	e59da000 	ldr	sl, [sp]\n"
            "  1c:	e59ab008 	ldr	fp, [sl, #8]\n"
            "  20:	e25bb001 	subs	fp, fp, #1\n"
            "  24:	e58ab008 	str	fp, [sl, #8]\n"
            "  28:	1afffffa 	bne	0x18\n"
            "  2c:	e1a0b00f 	mov	fp, pc\n"
            "  30:	e3e0a023 	mvn	sl, #35	@ 0x23\n"
            "  34:	e08aa00b 	add	sl, sl, fp\n"
            "  38:	e59db000 	ldr	fp, [sp]\n"
            "  3c:	e58ba004 	str	sl, [fp, #4]\n"
            "  40:	e592b008 	ldr	fp, [r2, #8]\n"
            "  44:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  48:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  4c:	e1a0e007 	mov	lr, r7\n"
            "  50:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  54:	e592b004 	ldr	fp, [r2, #4]\n"
            "  58:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  5c:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  60:	e1a0e007 	mov	lr, r7\n"
            "  64:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

jump_to_continuation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_to_continuation(State0, {free, r1}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e1a0b00f 	mov	fp, pc\n"
            "   4:	e081100b 	add	r1, r1, fp\n"
            "   8:	e3e0b007 	mvn	fp, #7\n"
            "   c:	e081100b 	add	r1, r1, fp\n"
            "  10:	e59db024 	ldr	fp, [sp, #36]	@ 0x24\n"
            "  14:	e58d1024 	str	r1, [sp, #36]	@ 0x24\n"
            "  18:	e1a0e00b 	mov	lr, fp\n"
            "  1c:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 2, 32),
    State3 = ?BACKEND:add_label(State2, 1, 16),
    SortedLines = [{10, 16}, {20, 32}],
    State4 = ?BACKEND:return_labels_and_lines(State3, SortedLines),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "   8:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   c:	eaffffff 	b	0x10\n"
            "  10:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  14:	ea000001 	b	0x20\n"
            "  18:	e28f0000 	add	r0, pc, #0\n"
            "  1c:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  20:	01000200 	mrseq	r0, R8_usr\n"
            "  24:	10000000 	andne	r0, r0, r0\n"
            "  28:	00000200 	andeq	r0, r0, r0, lsl #4\n"
            "  2c:	02002000 	andeq	r2, r0, #0\n"
            "  30:	00000a00 	andeq	r0, r0, r0, lsl #20\n"
            "  34:	14001000 	strne	r1, [r0], #-0\n"
            "  38:	20000000 	andcs	r0, r0, r0"
        >>,
    ?assertStream(arm32, Dump, Stream).

set_bs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:set_bs(State1, Reg),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e590b018 	ldr	fp, [r0, #24]\n"
            "   4:	e580b064 	str	fp, [r0, #100]	@ 0x64\n"
            "   8:	e3a0a000 	mov	sl, #0\n"
            "   c:	e580a068 	str	sl, [r0, #104]	@ 0x68"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_or_schedule_next_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 3),
    State7 = ?BACKEND:call_primitive_last(State6, ?PRIM_RETURN, [ctx, jit_state]),
    State8 = ?BACKEND:add_label(State7, 0),
    State9 = ?BACKEND:call_primitive_last(State8, 1, [ctx, jit_state]),
    State10 = ?BACKEND:update_branches(State9),
    Stream = ?BACKEND:stream(State10),
    Dump =
        <<
            "   0:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   4:	ea000026 	b	0xa4\n"
            "   8:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "   c:	ea000003 	b	0x20\n"
            "  10:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  14:	ea000018 	b	0x7c\n"
            "  18:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  1c:	ea00001b 	b	0x90\n"
            "  20:	e59da000 	ldr	sl, [sp]\n"
            "  24:	e59ab000 	ldr	fp, [sl]\n"
            "  28:	e59bb000 	ldr	fp, [fp]\n"
            "  2c:	e1a0bc0b 	lsl	fp, fp, #24\n"
            "  30:	e3a0ae1e 	mov	sl, #480	@ 0x1e0\n"
            "  34:	e18bb00a 	orr	fp, fp, sl\n"
            "  38:	e580b05c 	str	fp, [r0, #92]	@ 0x5c\n"
            "  3c:	e59da000 	ldr	sl, [sp]\n"
            "  40:	e59ab008 	ldr	fp, [sl, #8]\n"
            "  44:	e25bb001 	subs	fp, fp, #1\n"
            "  48:	e58ab008 	str	fp, [sl, #8]\n"
            "  4c:	1a00000a 	bne	0x7c\n"
            "  50:	e1a0b00f 	mov	fp, pc\n"
            "  54:	e3e0a047 	mvn	sl, #71	@ 0x47\n"
            "  58:	e08aa00b 	add	sl, sl, fp\n"
            "  5c:	e59db000 	ldr	fp, [sp]\n"
            "  60:	e58ba004 	str	sl, [fp, #4]\n"
            "  64:	e592b008 	ldr	fp, [r2, #8]\n"
            "  68:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  6c:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  70:	e1a0e007 	mov	lr, r7\n"
            "  74:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  78:	e92d4ff2 	push	{r1, r4, r5, r6, r7, r8, r9, sl, fp, lr}\n"
            "  7c:	e592b000 	ldr	fp, [r2]\n"
            "  80:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  84:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  88:	e1a0e007 	mov	lr, r7\n"
            "  8c:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  90:	e592b004 	ldr	fp, [r2, #4]\n"
            "  94:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  98:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  9c:	e1a0e007 	mov	lr, r7\n"
            "  a0:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}\n"
            "  a4:	e592b004 	ldr	fp, [r2, #4]\n"
            "  a8:	e59d7024 	ldr	r7, [sp, #36]	@ 0x24\n"
            "  ac:	e58db024 	str	fp, [sp, #36]	@ 0x24\n"
            "  b0:	e1a0e007 	mov	lr, r7\n"
            "  b4:	e8bd8ff2 	pop	{r1, r4, r5, r6, r7, r8, r9, sl, fp, pc}"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(arm32, Dump, Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[2] to x_reg 0
                ?_test(begin
                    move_array_element_test0(State0, r3, 2, {x_reg, 0}, <<
                        "   0:	e593b008 	ldr	fp, [r3, #8]\n"
                        "   4:	e580b018 	str	fp, [r0, #24]"
                    >>)
                end),
                %% move_array_element: reg[3] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r3, 3, {ptr, r5}, <<
                        "   0:	e593b00c 	ldr	fp, [r3, #12]\n"
                        "   4:	e585b000 	str	fp, [r5]"
                    >>)
                end),
                %% move_array_element: reg[1] to y_reg 2
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, {y_reg, 2}, <<
                        "   0:	e593a004 	ldr	sl, [r3, #4]\n"
                        "   4:	e590b014 	ldr	fp, [r0, #20]\n"
                        "   8:	e58ba008 	str	sl, [fp, #8]"
                    >>)
                end),
                %% move_array_element: reg[1] to native reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, r5, <<
                        "   0:	e5935004 	ldr	r5, [r3, #4]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {x_reg, 2}, <<
                        "   0:	e593b010 	ldr	fp, [r3, #16]\n"
                        "   4:	e1a0b10b 	lsl	fp, fp, #2\n"
                        "   8:	e793b00b 	ldr	fp, [r3, fp]\n"
                        "   c:	e580b020 	str	fp, [r0, #32]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to ptr
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {ptr, r5}, <<
                        "   0:	e593b010 	ldr	fp, [r3, #16]\n"
                        "   4:	e1a0b10b 	lsl	fp, fp, #2\n"
                        "   8:	e793b00b 	ldr	fp, [r3, fp]\n"
                        "   c:	e585b000 	str	fp, [r5]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {y_reg, 2}, <<
                        "   0:	e593b010 	ldr	fp, [r3, #16]\n"
                        "   4:	e1a0b10b 	lsl	fp, fp, #2\n"
                        "   8:	e793b00b 	ldr	fp, [r3, fp]\n"
                        "   c:	e590a014 	ldr	sl, [r0, #20]\n"
                        "  10:	e58ab008 	str	fp, [sl, #8]"
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
                %% get_array_element: reg[4] to new native reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r4, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e594b010 	ldr	fp, [r4, #16]"
                    >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r11, Reg)
                end),
                %% get_array_element: {free, reg}[4]
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, {free, r4}, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5944010 	ldr	r4, [r4, #16]"
                    >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r4, Reg)
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
                %% move_to_array_element/4: x_reg to reg[2]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                        "   4:	e583b008 	str	fp, [r3, #8]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                        "   4:	e1a0a004 	mov	sl, r4\n"
                        "   8:	e1a0a10a 	lsl	sl, sl, #2\n"
                        "   c:	e783b00a 	str	fp, [r3, sl]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset] (2+1=3, 3*4=12)
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
                        "   4:	e583b00c 	str	fp, [r3, #12]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
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
                    ?assertEqual(r11, Reg),
                    Dump = <<
                        "   0:	e3a0b02a 	mov	fp, #42	@ 0x2a"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: negative value via MVN
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -1),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r11, Reg),
                    Dump = <<
                        "   0:	e3e0b000 	mvn	fp, #0"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, r6}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e5966000 	ldr	r6, [r6]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {x_reg, 3}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r11, Reg),
                    Dump = <<
                        "   0:	e590b024 	ldr	fp, [r0, #36]	@ 0x24"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {y_reg, 3}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r11, Reg),
                    Dump = <<
                        "   0:	e590a014 	ldr	sl, [r0, #20]\n"
                        "   4:	e59ab00c 	ldr	fp, [sl, #12]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, r6),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e3a0602a 	mov	r6, #42	@ 0x2a"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, r7, r5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e1a05007 	mov	r5, r7"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, r7}, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5974000 	ldr	r4, [r7]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {x_reg, 2} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5903020 	ldr	r3, [r0, #32]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {y_reg, 2} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e590b014 	ldr	fp, [r0, #20]\n"
                        "   4:	e59b1008 	ldr	r1, [fp, #8]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end)
            ]
        end}.

%% Test large Y register read (Y=1024, offset=4096, exceeds 4095-byte limit)
%% This tests the fix that changed BaseOffset from 4092 to 4080
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 1024}),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(r11, Reg),
    Dump = <<
        "   0:	e590a014 	ldr	sl, [r0, #20]\n"
        "   4:	e28aaeff 	add	sl, sl, #4080	@ 0xff0\n"
        "   8:	e59ab010 	ldr	fp, [sl, #16]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test large Y register write
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, SrcReg, {y_reg, 1024}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e590a014 	ldr	sl, [r0, #20]\n"
        "   8:	e28aaeff 	add	sl, sl, #4080	@ 0xff0\n"
        "   c:	e58ab010 	str	fp, [sl, #16]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test boundary case: Y=1023 (4092 bytes, within 4095 limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 1023}),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(r11, Reg),
    Dump = <<
        "   0:	e590a014 	ldr	sl, [r0, #20]\n"
        "   4:	e59abffc 	ldr	fp, [sl, #4092]	@ 0xffc"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test y_reg load when only one register is available (last register, AvailT=0)
y_reg_load_last_available_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r10} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r9} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r8} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r7} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, r6} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    {State7, r5} = ?BACKEND:move_to_native_register(State6, {x_reg, 6}),
    {State8, r4} = ?BACKEND:move_to_native_register(State7, {x_reg, 7}),
    {State9, r3} = ?BACKEND:move_to_native_register(State8, {x_reg, 8}),
    %% r1 is the last available register
    {State10, r1} = ?BACKEND:move_to_native_register(State9, {y_reg, 0}),
    Stream = ?BACKEND:stream(State10),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
        "   8:	e5909020 	ldr	r9, [r0, #32]\n"
        "   c:	e5908024 	ldr	r8, [r0, #36]	@ 0x24\n"
        "  10:	e5907028 	ldr	r7, [r0, #40]	@ 0x28\n"
        "  14:	e590602c 	ldr	r6, [r0, #44]	@ 0x2c\n"
        "  18:	e5905030 	ldr	r5, [r0, #48]	@ 0x30\n"
        "  1c:	e5904034 	ldr	r4, [r0, #52]	@ 0x34\n"
        "  20:	e5903038 	ldr	r3, [r0, #56]	@ 0x38\n"
        "  24:	e5901014 	ldr	r1, [r0, #20]\n"
        "  28:	e5911000 	ldr	r1, [r1]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Cache invalidation: after free, a reload of the same register should be elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [r11]),
    {State3, r11} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% and_ with negative immediate should invalidate temp register cache
and_negative_imm_invalidates_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r10} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [r10]),
    {State4, r11} = ?BACKEND:and_(State3, {free, r11}, -4),
    {State5, r10} = ?BACKEND:move_to_native_register(State4, {x_reg, 1}),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
        "   8:	e3a0a003 	mov	sl, #3\n"
        "   c:	e1cbb00a 	bic	fp, fp, sl\n"
        "  10:	e590a01c 	ldr	sl, [r0, #28]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% and_ with positive immediate should invalidate temp register cache
and_positive_imm_invalidates_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r10} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [r10]),
    {State4, r11} = ?BACKEND:and_(State3, {free, r11}, 16#3F),
    {State5, r10} = ?BACKEND:move_to_native_register(State4, {x_reg, 1}),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
        "   8:	e3a0a03f 	mov	sl, #63	@ 0x3f\n"
        "   c:	e00bb00a 	and	fp, fp, sl\n"
        "  10:	e590a01c 	ldr	sl, [r0, #28]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% jump_to_label should invalidate all register caching
jump_to_label_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [r11]),
    State3 = ?BACKEND:jump_to_label(State2, 42),
    {State4, r11} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
        "   8:	e590b018 	ldr	fp, [r0, #24]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% move_array_element to x_reg should invalidate vm_loc cache
move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, r10} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    S3 = ?BACKEND:move_array_element(State2, r10, 0, {x_reg, 5}),
    {S4, _} = ?BACKEND:move_to_native_register(S3, {x_reg, 5}),
    Stream = ?BACKEND:stream(S4),
    Dump = <<
        "   0:	e590b02c 	ldr	fp, [r0, #44]	@ 0x2c\n"
        "   4:	e590a018 	ldr	sl, [r0, #24]\n"
        "   8:	e59a9000 	ldr	r9, [sl]\n"
        "   c:	e580902c 	str	r9, [r0, #44]	@ 0x2c\n"
        "  10:	e590902c 	ldr	r9, [r0, #44]	@ 0x2c"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% ldr_y_reg should invalidate its hidden temp register's cache
ldr_y_reg_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r11} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r10} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r9} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [r10, r9]),
    {State5, r10} = ?BACKEND:move_to_native_register(State4, {y_reg, 0}),
    %% r9 was hidden temp - must be invalidated, causing a reload
    {State6, r9} = ?BACKEND:move_to_native_register(State5, {x_reg, 2}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e590a01c 	ldr	sl, [r0, #28]\n"
        "   8:	e5909020 	ldr	r9, [r0, #32]\n"
        "   c:	e5909014 	ldr	r9, [r0, #20]\n"
        "  10:	e599a000 	ldr	sl, [r9]\n"
        "  14:	e5909020 	ldr	r9, [r0, #32]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test shift_right_arith
shift_right_arith_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg} = ?BACKEND:shift_right_arith(State1, {free, Reg}, 4),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e590b018 	ldr	fp, [r0, #24]\n"
        "   4:	e1a0b24b 	asr	fp, fp, #4"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test large jump table
jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual((512 + 1) * 8, byte_size(Stream)).
