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

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6817      	ldr	r7, [r2, #0]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	9902      	ldr	r1, [sp, #8]\n"
            "   6:	47b8      	blx	r7\n"
            "   8:	4607      	mov	r7, r0\n"
            "   a:	bc05      	pop	{r0, r2}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6857      	ldr	r7, [r2, #4]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	9902      	ldr	r1, [sp, #8]\n"
            "   6:	47b8      	blx	r7\n"
            "   8:	4607      	mov	r7, r0\n"
            "   a:	bc05      	pop	{r0, r2}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6897      	ldr	r7, [r2, #8]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	212a      	movs	r1, #42	; 0x2a\n"
            "   6:	222b      	movs	r2, #43	; 0x2b\n"
            "   8:	232c      	movs	r3, #44	; 0x2c\n"
            "   a:	47b8      	blx	r7\n"
            "   c:	4607      	mov	r7, r0\n"
            "   e:	bc05      	pop	{r0, r2}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6957      	ldr	r7, [r2, #20]\n"
            "   2:	b082      	sub	sp, #8\n"
            "   4:	2602      	movs	r6, #2\n"
            "   6:	9600      	str	r6, [sp, #0]\n"
            "   8:	9902      	ldr	r1, [sp, #8]\n"
            "   a:	2210      	movs	r2, #16\n"
            "   c:	2320      	movs	r3, #32\n"
            "   e:	47b8      	blx	r7\n"
            "  10:	b002      	add	sp, #8\n"
            "  12:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
            "   0:	6987      	ldr	r7, [r0, #24]\n"
            "   2:	2603      	movs	r6, #3\n"
            "   4:	43b7      	bics	r7, r6\n"
            "   6:	69c6      	ldr	r6, [r0, #28]\n"
            "   8:	25b8      	movs	r5, #184	; 0xb8\n"
            "   a:	5955      	ldr	r5, [r2, r5]\n"
            "   c:	b405      	push	{r0, r2}\n"
            "   e:	b082      	sub	sp, #8\n"
            "  10:	9601      	str	r6, [sp, #4]\n"
            "  12:	2608      	movs	r6, #8\n"
            "  14:	9600      	str	r6, [sp, #0]\n"
            "  16:	9904      	ldr	r1, [sp, #16]\n"
            "  18:	463a      	mov	r2, r7\n"
            "  1a:	2340      	movs	r3, #64	; 0x40\n"
            "  1c:	47a8      	blx	r5\n"
            "  1e:	4605      	mov	r5, r0\n"
            "  20:	b002      	add	sp, #8\n"
            "  22:	bc05      	pop	{r0, r2}"
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
        "   0:	6c97      	ldr	r7, [r2, #72]	; 0x48\n"
        "   2:	b405      	push	{r0, r2}\n"
        "   4:	2113      	movs	r1, #19\n"
        "   6:	47b8      	blx	r7\n"
        "   8:	4607      	mov	r7, r0\n"
        "   a:	bc05      	pop	{r0, r2}\n"
        "   c:	6c96      	ldr	r6, [r2, #72]	; 0x48\n"
        "   e:	b4c5      	push	{r0, r2, r6, r7}\n"
        "  10:	2114      	movs	r1, #20\n"
        "  12:	47b0      	blx	r6\n"
        "  14:	4605      	mov	r5, r0\n"
        "  16:	bcc5      	pop	{r0, r2, r6, r7}\n"
        "  18:	6c96      	ldr	r6, [r2, #72]	; 0x48\n"
        "  1a:	b4a5      	push	{r0, r2, r5, r7}\n"
        "  1c:	2113      	movs	r1, #19\n"
        "  1e:	47b0      	blx	r6\n"
        "  20:	4606      	mov	r6, r0\n"
        "  22:	bca5      	pop	{r0, r2, r5, r7}\n"
        "  24:	6b54      	ldr	r4, [r2, #52]	; 0x34\n"
        "  26:	b455      	push	{r0, r2, r4, r6}\n"
        "  28:	6839      	ldr	r1, [r7, #0]\n"
        "  2a:	682a      	ldr	r2, [r5, #0]\n"
        "  2c:	47a0      	blx	r4\n"
        "  2e:	4607      	mov	r7, r0\n"
        "  30:	bc55      	pop	{r0, r2, r4, r6}\n"
        "  32:	6037      	str	r7, [r6, #0]"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_few_free_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r7} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, r6} = ?BACKEND:move_to_native_register(State1, 2),
    {State3, r5} = ?BACKEND:move_to_native_register(State2, 3),
    {State4, r4} = ?BACKEND:move_to_native_register(State3, 4),
    {State5, r3} = ?BACKEND:move_to_native_register(State4, 5),
    {State6, ResultReg} = ?BACKEND:call_primitive(State5, ?PRIM_BITSTRING_INSERT_INTEGER, [
        r6, r7, {free, r4}, r5, {free, r3}
    ]),
    State7 = ?BACKEND:free_native_registers(State6, [ResultReg, r6, r7, r5]),
    ?BACKEND:assert_all_native_free(State7),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:	2701      	movs	r7, #1\n"
        "   2:	2602      	movs	r6, #2\n"
        "   4:	2503      	movs	r5, #3\n"
        "   6:	2404      	movs	r4, #4\n"
        "   8:	2305      	movs	r3, #5\n"
        "   a:	21e4      	movs	r1, #228	@ 0xe4\n"
        "   c:	5851      	ldr	r1, [r2, r1]\n"
        "   e:	b4e7      	push	{r0, r1, r2, r5, r6, r7}\n"
        "  10:	b082      	sub	sp, #8\n"
        "  12:	9300      	str	r3, [sp, #0]\n"
        "  14:	4633      	mov	r3, r6\n"
        "  16:	460e      	mov	r6, r1\n"
        "  18:	4618      	mov	r0, r3\n"
        "  1a:	4639      	mov	r1, r7\n"
        "  1c:	4622      	mov	r2, r4\n"
        "  1e:	462b      	mov	r3, r5\n"
        "  20:	47b0      	blx	r6\n"
        "  22:	4604      	mov	r4, r0\n"
        "  24:	b002      	add	sp, #8\n"
        "  26:	bce7      	pop	{r0, r1, r2, r5, r6, r7}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	9e00      	ldr	r6, [sp, #0]\n"
        "   2:	68b7      	ldr	r7, [r6, #8]\n"
        "   4:	3f01      	subs	r7, #1\n"
        "   6:	60b7      	str	r7, [r6, #8]\n"
        "   8:	d109      	bne.n	0x1e\n"
        "   a:	a704      	add	r7, pc, #16	; (adr r7, 0x1c)\n"
        "   c:	3701      	adds	r7, #1\n"
        "   e:	6077      	str	r7, [r6, #4]\n"
        "  10:	6897      	ldr	r7, [r2, #8]\n"
        "  12:	9e05      	ldr	r6, [sp, #20]\n"
        "  14:	9705      	str	r7, [sp, #20]\n"
        "  16:	46b6      	mov	lr, r6\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1e:	6917      	ldr	r7, [r2, #16]\n"
        "  20:	b082      	sub	sp, #8\n"
        "  22:	2601      	movs	r6, #1\n"
        "  24:	4276      	negs	r6, r6\n"
        "  26:	9601      	str	r6, [sp, #4]\n"
        "  28:	2602      	movs	r6, #2\n"
        "  2a:	9600      	str	r6, [sp, #0]\n"
        "  2c:	9902      	ldr	r1, [sp, #8]\n"
        "  2e:	2220      	movs	r2, #32\n"
        "  30:	2302      	movs	r3, #2\n"
        "  32:	47b8      	blx	r7\n"
        "  34:	b002      	add	sp, #8\n"
        "  36:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_unaligned_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    %% First do a 2-byte instruction to create unaligned start
    State1 = ?BACKEND:move_to_vm_register(State0, r1, {ptr, r3}),
    State2 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State1),
    State3 = ?BACKEND:call_primitive_last(State2, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        % State1 = ?BACKEND:move_to_vm_register(State0, r1, {ptr, r3}),
        "   0:	6019      	str	r1, [r3, #0]\n"
        % State2 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State1),
        "   2:	9e00      	ldr	r6, [sp, #0]\n"
        "   4:	68b7      	ldr	r7, [r6, #8]\n"
        "   6:	3f01      	subs	r7, #1\n"
        "   8:	60b7      	str	r7, [r6, #8]\n"
        "   a:	d108      	bne.n	0x1e\n"
        "   c:	a703      	add	r7, pc, #12	; (adr r7, 0x1c)\n"
        "   e:	3701      	adds	r7, #1\n"
        "  10:	6077      	str	r7, [r6, #4]\n"
        "  12:	6897      	ldr	r7, [r2, #8]\n"
        "  14:	9e05      	ldr	r6, [sp, #20]\n"
        "  16:	9705      	str	r7, [sp, #20]\n"
        "  18:	46b6      	mov	lr, r6\n"
        "  1a:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        % State3 = ?BACKEND:call_primitive_last(State2, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
        "  1e:	6917      	ldr	r7, [r2, #16]\n"
        "  20:	b082      	sub	sp, #8\n"
        "  22:	2601      	movs	r6, #1\n"
        "  24:	4276      	negs	r6, r6\n"
        "  26:	9601      	str	r6, [sp, #4]\n"
        "  28:	2602      	movs	r6, #2\n"
        "  2a:	9600      	str	r6, [sp, #0]\n"
        "  2c:	9902      	ldr	r1, [sp, #8]\n"
        "  2e:	2220      	movs	r2, #32\n"
        "  30:	2302      	movs	r3, #2\n"
        "  32:	47b8      	blx	r7\n"
        "  34:	b002      	add	sp, #8\n"
        "  36:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
        % {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        % State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [...
        "   2:	6cd6      	ldr	r6, [r2, #76]	; 0x4c\n"
        "   4:	b082      	sub	sp, #8\n"
        "   6:	9700      	str	r7, [sp, #0]\n"
        "   8:	9902      	ldr	r1, [sp, #8]\n"
        "   a:	2204      	movs	r2, #4\n"
        "   c:	4b00      	ldr	r3, [pc, #0]	; (0x10)\n"
        "   e:	e001      	b.n	0x14\n"
        "  10:	02cb      	lsrs	r3, r1, #16\n"
        "  12:	0000      	movs	r0, r0\n"
        "  14:	47b0      	blx	r6\n"
        "  16:	b002      	add	sp, #8\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        % State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
        "   0:	9e00      	ldr	r6, [sp, #0]\n"
        "   2:	68b7      	ldr	r7, [r6, #8]\n"
        "   4:	3f01      	subs	r7, #1\n"
        "   6:	60b7      	str	r7, [r6, #8]\n"
        "   8:	d109      	bne.n	0x1e\n"
        "   a:	a704      	add	r7, pc, #16	; (adr r7, 0x1c)\n"
        "   c:	3701      	adds	r7, #1\n"
        "   e:	6077      	str	r7, [r6, #4]\n"
        "  10:	6897      	ldr	r7, [r2, #8]\n"
        "  12:	9e05      	ldr	r6, [sp, #20]\n"
        "  14:	9705      	str	r7, [sp, #20]\n"
        "  16:	46b6      	mov	lr, r6\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        % State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
        "1e:	6917      	ldr	r7, [r2, #16]\n"
        "  20:	b082      	sub	sp, #8\n"
        "  22:	260a      	movs	r6, #10\n"
        "  24:	9601      	str	r6, [sp, #4]\n"
        "  26:	2602      	movs	r6, #2\n"
        "  28:	9600      	str	r6, [sp, #0]\n"
        "  2a:	9902      	ldr	r1, [sp, #8]\n"
        "  2c:	2220      	movs	r2, #32\n"
        "  2e:	2302      	movs	r3, #2\n"
        "  30:	47b8      	blx	r7\n"
        "  32:	b002      	add	sp, #8\n"
        "  34:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6817      	ldr	r7, [r2, #0]\n"
            "   2:	222a      	movs	r2, #42	; 0x2a\n"
            "   4:	9e05      	ldr	r6, [sp, #20]\n"
            "   6:	9705      	str	r7, [sp, #20]\n"
            "   8:	46b6      	mov	lr, r6\n"
            "   a:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
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
                            "   0:	6d57      	ldr	r7, [r2, #84]	; 0x54\n"
                            "   2:	b405      	push	{r0, r2}\n"
                            "   4:	9902      	ldr	r1, [sp, #8]\n"
                            "   6:	47b8      	blx	r7\n"
                            "   8:	4607      	mov	r7, r0\n"
                            "   a:	bc05      	pop	{r0, r2}\n"
                            "   c:	4287      	cmp	r7, r0\n"
                            "   e:	d001      	beq.n	0x14\n"
                            "  10:	4638      	mov	r0, r7\n"
                            "  12:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
                    ?assertEqual(r6, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	6d57      	ldr	r7, [r2, #84]	; 0x54\n"
                            "   2:	b405      	push	{r0, r2}\n"
                            "   4:	9902      	ldr	r1, [sp, #8]\n"
                            "   6:	47b8      	blx	r7\n"
                            "   8:	4607      	mov	r7, r0\n"
                            "   a:	bc05      	pop	{r0, r2}\n"
                            "   c:	463e      	mov	r6, r7\n"
                            "   e:	4286      	cmp	r6, r0\n"
                            "  10:	d001      	beq.n	0x16\n"
                            "  12:	4630      	mov	r0, r6\n"
                            "  14:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
            "   0:	6946      	ldr	r6, [r0, #20]\n"
            "   2:	6837      	ldr	r7, [r6, #0]\n"
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
                        {RegA, '<', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2f2a      	cmp	r7, #42	; 0x2a\n"
                        "   6:	da00      	bge.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	4d00      	ldr	r5, [pc, #0]	; (0x8)\n"
                        "   6:	da04      	bge.n	0x12\n"
                        "   8:	0400      	lsls	r0, r0, #16\n"
                        "   a:	0000      	movs	r0, r0\n"
                        "   c:	42af      	cmp	r7, r5\n"
                        "   e:	dafe      	bge.n	0xe\n"
                        "  10:	3602      	adds	r6, #2"
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
                        {RegA, '==', -1},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	2501      	movs	r5, #1\n"
                        "   6:	426d      	negs	r5, r5\n"
                        "   8:	42af      	cmp	r7, r5\n"
                        "   a:	d100      	bne.n	0xe\n"
                        "   c:	3602      	adds	r6, #2"
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
                    % Test large immediate (1995) that requires temporary register
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', 1995},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 1)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	4d00      	ldr	r5, [pc, #0]	; (0x8)\n"
                        "   6:	e001      	b.n	0xc\n"
                        "   8:	07cb      	lsls	r3, r1, #31\n"
                        "   a:	0000      	movs	r0, r0\n"
                        "   c:	42af      	cmp	r7, r5\n"
                        "   e:	d000      	beq.n	0x12\n"
                        "  10:	3601      	adds	r6, #1"
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
                        "   6:	d000      	beq.n	0xa\n"
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
                        "   6:	d000      	beq.n	0xa\n"
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
                        "   4:	43fd      	mvns	r5, r7\n"
                        "   6:	072d      	lsls	r5, r5, #28\n"
                        "   8:	d000      	beq.n	0xc\n"
                        "   a:	3602      	adds	r6, #2"
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
                        "   4:	43ff      	mvns	r7, r7\n"
                        "   6:	073f      	lsls	r7, r7, #28\n"
                        "   8:	d000      	beq.n	0xc\n"
                        "   a:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	463d      	mov	r5, r7\n"
                        "   6:	243f      	movs	r4, #63	; 0x3f\n"
                        "   8:	4025      	ands	r5, r4\n"
                        "   a:	2d08      	cmp	r5, #8\n"
                        "   c:	d000      	beq.n	0x10\n"
                        "   e:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	42b7      	cmp	r7, r6\n"
                        "   6:	da00      	bge.n	0xa\n"
                        "   8:	3602      	adds	r6, #2"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	69c6      	ldr	r6, [r0, #28]\n"
                        "   4:	253f      	movs	r5, #63	; 0x3f\n"
                        "   6:	402f      	ands	r7, r5\n"
                        "   8:	2f08      	cmp	r7, #8\n"
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
                "   6:	d000      	beq.n	0xa\n"
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
                "   6:	d000      	beq.n	0xa\n"
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
                "   6:	d000      	beq.n	0xa\n"
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
            "   0:	6987      	ldr	r7, [r0, #24]\n"
            "   2:	69c6      	ldr	r6, [r0, #28]\n"
            "   4:	2f3b      	cmp	r7, #59	; 0x3b\n"
            "   6:	d101      	bne.n	0xc\n"
            "   8:	3602      	adds	r6, #2\n"
            "   a:	e000      	b.n	0xe\n"
            "   c:	3604      	adds	r6, #4"
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
            "   0:	4b01      	ldr	r3, [pc, #4]	; (0x8)\n"
            "   2:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   4:	449f      	add	pc, r3\n"
            "   6:	46c0      	nop			; (mov r8, r8)\n"
            "   8:	0054      	lsls	r4, r2, #1\n"
            "   a:	0000      	movs	r0, r0\n"
            "   c:	4b01      	ldr	r3, [pc, #4]	; (0x14)\n"
            "   e:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  10:	449f      	add	pc, r3\n"
            "  12:	46c0      	nop			; (mov r8, r8)\n"
            "  14:	0010      	movs	r0, r2\n"
            "  16:	0000      	movs	r0, r0\n"
            "  18:	4b01      	ldr	r3, [pc, #4]	; (0x20)\n"
            "  1a:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  1c:	449f      	add	pc, r3\n"
            "  1e:	46c0      	nop			; (mov r8, r8)\n"
            "  20:	0030      	movs	r0, r6\n"
            "  22:	0000      	movs	r0, r0\n"
            "  24:	9e00      	ldr	r6, [sp, #0]\n"
            "  26:	68b7      	ldr	r7, [r6, #8]\n"
            "  28:	3f01      	subs	r7, #1\n"
            "  2a:	60b7      	str	r7, [r6, #8]\n"
            "  2c:	d004      	beq.n	0x38\n"
            "  2e:	e00f      	b.n	0x50\n"
            "  30:	46c0      	nop			; (mov r8, r8)\n"
            "  32:	46c0      	nop			; (mov r8, r8)\n"
            "  34:	46c0      	nop			; (mov r8, r8)\n"
            "  36:	46c0      	nop			; (mov r8, r8)\n"
            "  38:	a700      	add	r7, pc, #0	; (adr r7, 0x3c)\n"
            "  3a:	2623      	movs	r6, #35	; 0x23\n"
            "  3c:	4276      	negs	r6, r6\n"
            "  3e:	19f6      	adds	r6, r6, r7\n"
            "  40:	9f00      	ldr	r7, [sp, #0]\n"
            "  42:	607e      	str	r6, [r7, #4]\n"
            "  44:	6897      	ldr	r7, [r2, #8]\n"
            "  46:	9e05      	ldr	r6, [sp, #20]\n"
            "  48:	9705      	str	r7, [sp, #20]\n"
            "  4a:	46b6      	mov	lr, r6\n"
            "  4c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  4e:	46c0      	nop			; (mov r8, r8)\n"
            "  50:	6817      	ldr	r7, [r2, #0]\n"
            "  52:	9e05      	ldr	r6, [sp, #20]\n"
            "  54:	9705      	str	r7, [sp, #20]\n"
            "  56:	46b6      	mov	lr, r6\n"
            "  58:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  5a:	46c0      	nop			; (mov r8, r8)\n"
            "  5c:	6857      	ldr	r7, [r2, #4]\n"
            "  5e:	9e05      	ldr	r6, [sp, #20]\n"
            "  60:	9705      	str	r7, [sp, #20]\n"
            "  62:	46b6      	mov	lr, r6\n"
            "  64:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test with different alignment (unaligned start)
call_only_or_schedule_next_and_label_relocation_unaligned_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    %% First do a 2-byte instruction to create unaligned start
    State1 = ?BACKEND:move_to_vm_register(State0, r1, {ptr, r3}),
    State2 = ?BACKEND:jump_table(State1, 2),
    State3 = ?BACKEND:add_label(State2, 1),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:add_label(State4, 2),
    State6 = ?BACKEND:call_primitive_last(State5, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State7 = ?BACKEND:add_label(State6, 0),
    State8 = ?BACKEND:call_primitive_last(State7, 1, [ctx, jit_state]),
    State9 = ?BACKEND:update_branches(State8),
    Stream = ?BACKEND:stream(State9),
    Dump =
        <<
            "   0:	6019      	str	r1, [r3, #0]\n"
            "   2:	4b01      	ldr	r3, [pc, #4]	; (0x8)\n"
            "   4:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   6:	449f      	add	pc, r3\n"
            "   8:	46c0      	nop			; (mov r8, r8)\n"
            "   a:	0056      	lsls	r6, r2, #1\n"
            "   c:	0000      	movs	r0, r0\n"
            "   e:	4b01      	ldr	r3, [pc, #4]	; (0x14)\n"
            "  10:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  12:	449f      	add	pc, r3\n"
            "  14:	46c0      	nop			; (mov r8, r8)\n"
            "  16:	0012      	movs	r2, r2\n"
            "  18:	0000      	movs	r0, r0\n"
            "  1a:	4b01      	ldr	r3, [pc, #4]	; (0x20)\n"
            "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  1e:	449f      	add	pc, r3\n"
            "  20:	46c0      	nop			; (mov r8, r8)\n"
            "  22:	0032      	movs	r2, r6\n"
            "  24:	0000      	movs	r0, r0\n"
            "  26:	46c0      	nop			; (mov r8, r8)\n"
            "  28:	9e00      	ldr	r6, [sp, #0]\n"
            "  2a:	68b7      	ldr	r7, [r6, #8]\n"
            "  2c:	3f01      	subs	r7, #1\n"
            "  2e:	60b7      	str	r7, [r6, #8]\n"
            "  30:	d004      	beq.n	0x3c\n"
            "  32:	e00f      	b.n	0x54\n"
            "  34:	46c0      	nop			; (mov r8, r8)\n"
            "  36:	46c0      	nop			; (mov r8, r8)\n"
            "  38:	46c0      	nop			; (mov r8, r8)\n"
            "  3a:	46c0      	nop			; (mov r8, r8)\n"
            "  3c:	a700      	add	r7, pc, #0	; (adr r7, 0x40)\n"
            "  3e:	2627      	movs	r6, #39	; 0x27\n"
            "  40:	4276      	negs	r6, r6\n"
            "  42:	19f6      	adds	r6, r6, r7\n"
            "  44:	9f00      	ldr	r7, [sp, #0]\n"
            "  46:	607e      	str	r6, [r7, #4]\n"
            "  48:	6897      	ldr	r7, [r2, #8]\n"
            "  4a:	9e05      	ldr	r6, [sp, #20]\n"
            "  4c:	9705      	str	r7, [sp, #20]\n"
            "  4e:	46b6      	mov	lr, r6\n"
            "  50:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  52:	46c0      	nop			; (mov r8, r8)\n"
            "  54:	6817      	ldr	r7, [r2, #0]\n"
            "  56:	9e05      	ldr	r6, [sp, #20]\n"
            "  58:	9705      	str	r7, [sp, #20]\n"
            "  5a:	46b6      	mov	lr, r6\n"
            "  5c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  5e:	46c0      	nop			; (mov r8, r8)\n"
            "  60:	6857      	ldr	r7, [r2, #4]\n"
            "  62:	9e05      	ldr	r6, [sp, #20]\n"
            "  64:	9705      	str	r7, [sp, #20]\n"
            "  66:	46b6      	mov	lr, r6\n"
            "  68:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
            ?BACKEND:move_to_native_register(S, {x_reg, 2}, r3)
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
    % Extract the final section starting at 0x124 to verify the literal pool pattern
    Dump = <<
        " 124:	9e00      	ldr	r6, [sp, #0]\n"
        " 126:	68b7      	ldr	r7, [r6, #8]\n"
        " 128:	3f01      	subs	r7, #1\n"
        " 12a:	60b7      	str	r7, [r6, #8]\n"
        " 12c:	d004      	beq.n	0x138\n"
        " 12e:	e011      	b.n	0x154\n"
        " 130:	46c0      	nop			; (mov r8, r8)\n"
        " 132:	46c0      	nop			; (mov r8, r8)\n"
        " 134:	46c0      	nop			; (mov r8, r8)\n"
        " 136:	46c0      	nop			; (mov r8, r8)\n"
        " 138:	a700      	add	r7, pc, #0	; (adr r7, 0x13c)\n"
        " 13a:	4e01      	ldr	r6, [pc, #4]	; (0x140)\n"
        " 13c:	e002      	b.n	0x144\n"
        " 13e:	0000      	movs	r0, r0\n"
        " 140:	fedd ffff 	stcl2	15, cr13, [sp, #-1020]	; 0xfffffc04\n"
        " 144:	19f6      	adds	r6, r6, r7\n"
        " 146:	9f00      	ldr	r7, [sp, #0]\n"
        " 148:	607e      	str	r6, [r7, #4]\n"
        " 14a:	6897      	ldr	r7, [r2, #8]\n"
        " 14c:	9e05      	ldr	r6, [sp, #20]\n"
        " 14e:	9705      	str	r7, [sp, #20]\n"
        " 150:	46b6      	mov	lr, r6\n"
        " 152:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        " 154:	6817      	ldr	r7, [r2, #0]\n"
        " 156:	9e05      	ldr	r6, [sp, #20]\n"
        " 158:	9705      	str	r7, [sp, #20]\n"
        " 15a:	46b6      	mov	lr, r6\n"
        " 15c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        " 15e:	46c0      	nop			; (mov r8, r8)\n"
        " 160:	6857      	ldr	r7, [r2, #4]\n"
        " 162:	9e05      	ldr	r6, [sp, #20]\n"
        " 164:	9705      	str	r7, [sp, #20]\n"
        " 166:	46b6      	mov	lr, r6\n"
        " 168:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
    >>,
    {_, RelevantBinary} = split_binary(Stream, 16#124),
    ?assertEqual(dump_to_bin(Dump), RelevantBinary).

%% Test with large gap (256+ bytes) and different alignment to force literal pool path
call_only_or_schedule_next_and_label_relocation_large_gap_unaligned_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    % Add large padding by emitting many move_to_native_register operations
    % This creates a large gap between the jump table and the rest of the code
    % Use 127 operations (instead of 128) to create different alignment
    StatePadded = lists:foldl(
        fun(_, S) ->
            ?BACKEND:move_to_native_register(S, {x_reg, 2}, r3)
        end,
        State1,
        lists:seq(1, 127)
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
    % Extract the final section starting at 0x122 to verify the literal pool pattern with different alignment
    Dump = <<
        " 122:	46c0      	nop			; (mov r8, r8)\n"
        " 124:	9e00      	ldr	r6, [sp, #0]\n"
        " 126:	68b7      	ldr	r7, [r6, #8]\n"
        " 128:	3f01      	subs	r7, #1\n"
        " 12a:	60b7      	str	r7, [r6, #8]\n"
        " 12c:	d004      	beq.n	0x138\n"
        " 12e:	e011      	b.n	0x154\n"
        " 130:	46c0      	nop			; (mov r8, r8)\n"
        " 132:	46c0      	nop			; (mov r8, r8)\n"
        " 134:	46c0      	nop			; (mov r8, r8)\n"
        " 136:	46c0      	nop			; (mov r8, r8)\n"
        " 138:	a700      	add	r7, pc, #0	; (adr r7, 0x13c)\n"
        " 13a:	4e01      	ldr	r6, [pc, #4]	; (0x140)\n"
        " 13c:	e002      	b.n	0x144\n"
        " 13e:	0000      	movs	r0, r0\n"
        " 140:	fedd ffff 	stcl2	15, cr13, [sp, #-1020]	; 0xfffffc04\n"
        " 144:	19f6      	adds	r6, r6, r7\n"
        " 146:	9f00      	ldr	r7, [sp, #0]\n"
        " 148:	607e      	str	r6, [r7, #4]\n"
        " 14a:	6897      	ldr	r7, [r2, #8]\n"
        " 14c:	9e05      	ldr	r6, [sp, #20]\n"
        " 14e:	9705      	str	r7, [sp, #20]\n"
        " 150:	46b6      	mov	lr, r6\n"
        " 152:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        " 154:	6817      	ldr	r7, [r2, #0]\n"
        " 156:	9e05      	ldr	r6, [sp, #20]\n"
        " 158:	9705      	str	r7, [sp, #20]\n"
        " 15a:	46b6      	mov	lr, r6\n"
        " 15c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        " 15e:	46c0      	nop			; (mov r8, r8)\n"
        " 160:	6857      	ldr	r7, [r2, #4]\n"
        " 162:	9e05      	ldr	r6, [sp, #20]\n"
        " 164:	9705      	str	r7, [sp, #20]\n"
        " 166:	46b6      	mov	lr, r6\n"
        " 168:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
    >>,
    {_, RelevantBinary} = split_binary(Stream, 16#122),
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
            "   0:	6a17      	ldr	r7, [r2, #32]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	9802      	ldr	r0, [sp, #8]\n"
            "   6:	2102      	movs	r1, #2\n"
            "   8:	47b8      	blx	r7\n"
            "   a:	4607      	mov	r7, r0\n"
            "   c:	bc05      	pop	{r0, r2}\n"
            "   e:	6bd6      	ldr	r6, [r2, #60]	; 0x3c\n"
            "  10:	b4c5      	push	{r0, r2, r6, r7}\n"
            "  12:	4901      	ldr	r1, [pc, #4]	; (0x18)\n"
            "  14:	e002      	b.n	0x1c\n"
            "  16:	0000      	movs	r0, r0\n"
            "  18:	e895 3b7f 	ldmia.w	r5, {r0, r1, r2, r3, r4, r5, r6, r8, r9, fp, ip, sp}\n"
            "  1c:	47b0      	blx	r6\n"
            "  1e:	4605      	mov	r5, r0\n"
            "  20:	bcc5      	pop	{r0, r2, r6, r7}\n"
            "  22:	b405      	push	{r0, r2}\n"
            "  24:	b082      	sub	sp, #8\n"
            "  26:	9500      	str	r5, [sp, #0]\n"
            "  28:	2100      	movs	r1, #0\n"
            "  2a:	2201      	movs	r2, #1\n"
            "  2c:	6983      	ldr	r3, [r0, #24]\n"
            "  2e:	47b8      	blx	r7\n"
            "  30:	4607      	mov	r7, r0\n"
            "  32:	b002      	add	sp, #8\n"
            "  34:	bc05      	pop	{r0, r2}\n"
            "  36:	2f00      	cmp	r7, #0\n"
            "  38:	d105      	bne.n	0x46\n"
            "  3a:	6997      	ldr	r7, [r2, #24]\n"
            "  3c:	223c      	movs	r2, #60	; 0x3c\n"
            "  3e:	9e05      	ldr	r6, [sp, #20]\n"
            "  40:	9705      	str	r7, [sp, #20]\n"
            "  42:	46b6      	mov	lr, r6\n"
            "  44:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  46:	6187      	str	r7, [r0, #24]"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	2603      	movs	r6, #3\n"
        "   4:	43b7      	bics	r7, r6\n"
        "   6:	687d      	ldr	r5, [r7, #4]\n"
        "   8:	6946      	ldr	r6, [r0, #20]\n"
        "   a:	6075      	str	r5, [r6, #4]\n"
        "   c:	683d      	ldr	r5, [r7, #0]\n"
        "   e:	6946      	ldr	r6, [r0, #20]\n"
        "  10:	6035      	str	r5, [r6, #0]"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	43fe      	mvns	r6, r7\n"
        "   4:	0736      	lsls	r6, r6, #28\n"
        "   6:	d015      	beq.n	0x34\n"
        "   8:	463e      	mov	r6, r7\n"
        "   a:	2503      	movs	r5, #3\n"
        "   c:	402e      	ands	r6, r5\n"
        "   e:	2e02      	cmp	r6, #2\n"
        "  10:	d004      	beq.n	0x1c\n"
        "  12:	e075      	b.n	0x100\n"
        "  14:	46c0      	nop			; (mov r8, r8)\n"
        "  16:	46c0      	nop			; (mov r8, r8)\n"
        "  18:	46c0      	nop			; (mov r8, r8)\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	2603      	movs	r6, #3\n"
        "  1e:	43b7      	bics	r7, r6\n"
        "  20:	683f      	ldr	r7, [r7, #0]\n"
        "  22:	263f      	movs	r6, #63	; 0x3f\n"
        "  24:	4037      	ands	r7, r6\n"
        "  26:	2f08      	cmp	r7, #8\n"
        "  28:	d004      	beq.n	0x34\n"
        "  2a:	e069      	b.n	0x100\n"
        "  2c:	46c0      	nop			; (mov r8, r8)\n"
        "  2e:	46c0      	nop			; (mov r8, r8)\n"
        "  30:	46c0      	nop			; (mov r8, r8)\n"
        "  32:	46c0      	nop			; (mov r8, r8)"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	43fe      	mvns	r6, r7\n"
        "   4:	0736      	lsls	r6, r6, #28\n"
        "   6:	d01b      	beq.n	0x40\n"
        "   8:	463e      	mov	r6, r7\n"
        "   a:	2503      	movs	r5, #3\n"
        "   c:	402e      	ands	r6, r5\n"
        "   e:	2e02      	cmp	r6, #2\n"
        "  10:	d004      	beq.n	0x1c\n"
        "  12:	e075      	b.n	0x100\n"
        "  14:	46c0      	nop			; (mov r8, r8)\n"
        "  16:	46c0      	nop			; (mov r8, r8)\n"
        "  18:	46c0      	nop			; (mov r8, r8)\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	2603      	movs	r6, #3\n"
        "  1e:	43b7      	bics	r7, r6\n"
        "  20:	683f      	ldr	r7, [r7, #0]\n"
        "  22:	463e      	mov	r6, r7\n"
        "  24:	253f      	movs	r5, #63	; 0x3f\n"
        "  26:	402e      	ands	r6, r5\n"
        "  28:	2e08      	cmp	r6, #8\n"
        "  2a:	d009      	beq.n	0x40\n"
        "  2c:	263f      	movs	r6, #63	; 0x3f\n"
        "  2e:	4037      	ands	r7, r6\n"
        "  30:	2f18      	cmp	r7, #24\n"
        "  32:	d005      	beq.n	0x40\n"
        "  34:	e064      	b.n	0x100\n"
        "  36:	46c0      	nop			; (mov r8, r8)\n"
        "  38:	46c0      	nop			; (mov r8, r8)\n"
        "  3a:	46c0      	nop			; (mov r8, r8)\n"
        "  3c:	46c0      	nop			; (mov r8, r8)\n"
        "  3e:	46c0      	nop			; (mov r8, r8)"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	2f4b      	cmp	r7, #75	; 0x4b\n"
        "   4:	d006      	beq.n	0x14\n"
        "   6:	2f0b      	cmp	r7, #11\n"
        "   8:	d004      	beq.n	0x14\n"
        "   a:	e079      	b.n	0x100\n"
        "   c:	46c0      	nop			; (mov r8, r8)\n"
        "   e:	46c0      	nop			; (mov r8, r8)\n"
        "  10:	46c0      	nop			; (mov r8, r8)\n"
        "  12:	46c0      	nop			; (mov r8, r8)"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	2f4b      	cmp	r7, #75	; 0x4b\n"
        "   4:	d006      	beq.n	0x14\n"
        "   6:	2f0b      	cmp	r7, #11\n"
        "   8:	d004      	beq.n	0x14\n"
        "   a:	4e01      	ldr	r6, [pc, #4]	; (0x10)\n"
        "   c:	447e      	add	r6, pc\n"
        "   e:	4730      	bx	r6\n"
        "  10:	0ff1      	lsrs	r0, r6, #31\n"
        "  12:	0000      	movs	r0, r0"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_far_unaligned_test() ->
    % Create a new state with a 2-byte instruction already in the stream
    % to simulate starting at an odd offset (offset 2 instead of 0)
    PaddingInstruction = jit_armv6m_asm:bx(lr),
    TempState = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    TempStream = jit_stream_binary:append(?BACKEND:stream(TempState), PaddingInstruction),
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, TempStream),

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
        "   0:	4770      	bx	lr\n"
        "   2:	6987      	ldr	r7, [r0, #24]\n"
        "   4:	2f4b      	cmp	r7, #75	@ 0x4b\n"
        "   6:	d007      	beq.n	0x18\n"
        "   8:	2f0b      	cmp	r7, #11\n"
        "   a:	d005      	beq.n	0x18\n"
        "   c:	4e01      	ldr	r6, [pc, #4]	@ (0x14)\n"
        "   e:	447e      	add	r6, pc\n"
        "  10:	4730      	bx	r6\n"
        "  12:	46c0      	nop			@ (mov r8, r8)\n"
        "  14:	0fef      	lsrs	r7, r5, #31\n"
        "  16:	0000      	movs	r0, r0"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	2f4b      	cmp	r7, #75	; 0x4b\n"
        "   4:	d006      	beq.n	0x14\n"
        "   6:	2f0b      	cmp	r7, #11\n"
        "   8:	d004      	beq.n	0x14\n"
        "   a:	4e01      	ldr	r6, [pc, #4]	; (0x10)\n"
        "   c:	447e      	add	r6, pc\n"
        "   e:	4730      	bx	r6\n"
        "  10:	0ff1      	lsrs	r1, r6, #31\n"
        "  12:	0000      	movs	r0, r0"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_far_known_unaligned_test() ->
    % Create a new state with a 2-byte instruction already in the stream
    % to simulate starting at an odd offset (offset 2 instead of 0)
    PaddingInstruction = jit_armv6m_asm:bx(lr),
    TempState = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    TempStream = jit_stream_binary:append(?BACKEND:stream(TempState), PaddingInstruction),
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, TempStream),

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
        "   0:	4770      	bx	lr\n"
        "   2:	6987      	ldr	r7, [r0, #24]\n"
        "   4:	2f4b      	cmp	r7, #75	; 0x4b\n"
        "   6:	d007      	beq.n	0x18\n"
        "   8:	2f0b      	cmp	r7, #11\n"
        "   a:	d005      	beq.n	0x18\n"
        "   c:	4e01      	ldr	r6, [pc, #4]	; (0x14)\n"
        "   e:	447e      	add	r6, pc\n"
        "  10:	4730      	bx	r6\n"
        "  12:	46c0      	nop			; (mov r8, r8)\n"
        "  14:	0fef      	lsrs	r7, r5, #31\n"
        "  16:	0000      	movs	r0, r0"
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
        "   0:	a707      	add	r7, pc, #28	; (adr r7, 0x22)\n"
        "   2:	3701      	adds	r7, #1\n"
        "   4:	9e00      	ldr	r6, [sp, #0]\n"
        "   6:	6077      	str	r7, [r6, #4]\n"
        "   8:	4f00      	ldr	r7, [pc, #0]	; (0xc)\n"
        "   a:	e001      	b.n	0x10\n"
        "   c:	1388      	asrs	r0, r1, #14\n"
        "   e:	0000      	movs	r0, r0\n"
        "  10:	6f96      	ldr	r6, [r2, #120]	; 0x78\n"
        "  14:	463a      	mov	r2, r7\n"
        "  16:	232a      	movs	r3, #42	; 0x2a\n"
        "  18:	9f05      	ldr	r7, [sp, #20]\n"
        "  1a:	9605      	str	r6, [sp, #20]\n"
        "  1c:	46be      	mov	lr, r7\n"
        "  1e:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  20:	46c0      	nop			; (mov r8, r8)\n"
        "  22:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  24:	6d57      	ldr	r7, [r2, #84]	; 0x54\n"
        "  26:	b405      	push	{r0, r2}\n"
        "  28:	9902      	ldr	r1, [sp, #8]\n"
        "  2a:	47b8      	blx	r7\n"
        "  2c:	4607      	mov	r7, r0\n"
        "  2e:	bc05      	pop	{r0, r2}\n"
        "  30:	4287      	cmp	r7, r0\n"
        "  32:	d001      	beq.n	0x38\n"
        "  34:	4638      	mov	r0, r7\n"
        "  36:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  38:	2784      	movs	r7, #132	; 0x84\n"
        "  3a:	59d7      	ldr	r7, [r2, r7]\n"
        "  3c:	b405      	push	{r0, r2}\n"
        "  3e:	2102      	movs	r1, #2\n"
        "  40:	47b8      	blx	r7\n"
        "  42:	4607      	mov	r7, r0\n"
        "  44:	bc05      	pop	{r0, r2}\n"
        "  46:	2f00      	cmp	r7, #0\n"
        "  48:	d105      	bne.n	0x56\n"
        "  4a:	6fd7      	ldr	r7, [r2, #124]	; 0x7c\n"
        "  4c:	222a      	movs	r2, #42	; 0x2a\n"
        "  4e:	9e05      	ldr	r6, [sp, #20]\n"
        "  50:	9705      	str	r7, [sp, #20]\n"
        "  52:	46b6      	mov	lr, r6\n"
        "  54:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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
        "   0:	4b01      	ldr	r3, [pc, #4]	; (0x8)\n"
        "   2:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "   4:	449f      	add	pc, r3\n"
        "   6:	46c0      	nop			; (mov r8, r8)\n"
        "   8:	0000      	movs	r0, r0\n"
        "   a:	0000      	movs	r0, r0\n"
        "   c:	4b01      	ldr	r3, [pc, #4]	; (0x14)\n"
        "   e:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  10:	449f      	add	pc, r3\n"
        "  12:	46c0      	nop			; (mov r8, r8)\n"
        "  14:	0000      	movs	r0, r0\n"
        "  16:	0000      	movs	r0, r0\n"
        "  18:	4b01      	ldr	r3, [pc, #4]	; (0x20)\n"
        "  1a:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1c:	449f      	add	pc, r3\n"
        "  1e:	46c0      	nop			; (mov r8, r8)\n"
        "  20:	0000      	movs	r0, r0\n"
        "  22:	0000      	movs	r0, r0\n"
        "  24:	4b01      	ldr	r3, [pc, #4]	; (0x2c)\n"
        "  26:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  28:	449f      	add	pc, r3\n"
        "  2a:	46c0      	nop			; (mov r8, r8)\n"
        "  2c:	0000      	movs	r0, r0\n"
        "  2e:	0000      	movs	r0, r0\n"
        "  30:	4b01      	ldr	r3, [pc, #4]	; (0x38)\n"
        "  32:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  34:	449f      	add	pc, r3\n"
        "  36:	46c0      	nop			; (mov r8, r8)\n"
        "  38:	0000      	movs	r0, r0\n"
        "  3a:	0000      	movs	r0, r0\n"
        "  3c:	4b01      	ldr	r3, [pc, #4]	; (0x44)\n"
        "  3e:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  40:	449f      	add	pc, r3\n"
        "  42:	46c0      	nop			; (mov r8, r8)\n"
        "  44:	0000      	movs	r0, r0\n"
        "  46:	0000      	movs	r0, r0\n"
        "  48:	a700      	add	r7, pc, #0	; (adr r7, 0x4c)\n"
        "  4a:	2633      	movs	r6, #51	; 0x33\n"
        "  4c:	4276      	negs	r6, r6\n"
        "  4e:	19f6      	adds	r6, r6, r7\n"
        "  50:	9f00      	ldr	r7, [sp, #0]\n"
        "  52:	607e      	str	r6, [r7, #4]\n"
        "  54:	6f57      	ldr	r7, [r2, #116]	; 0x74\n"
        "  56:	9e05      	ldr	r6, [sp, #20]\n"
        "  58:	9705      	str	r7, [sp, #20]\n"
        "  5a:	46b6      	mov	lr, r6\n"
        "  5c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
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

    % Should have generated adr + pop {r1,r4,r5,r6,r7,pc} + labels table + lines table
    % adr = 4 bytes, pop = 2 bytes, labels table = 6*2 = 12 bytes, lines table = 6*2 = 12 bytes
    % Total minimum: 30 bytes
    ?assert(byte_size(Stream) >= 30),

    % Expected: adr r0, <offset> + pop {r1,r4,r5,r6,r7,pc} + labels table + lines table
    % The data tables start at offset 4, so adr should be adr r0, 4 not adr r0, 8
    Dump = <<
        "   0:	a000      	add	r0, pc, #0	; (adr r0, 0x4)\n"
        "   2:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "   4:	0200      	lsls	r0, r0, #8\n"
        "   6:	0100      	lsls	r0, r0, #4\n"
        "   8:	0000      	movs	r0, r0\n"
        "   a:	1000      	asrs	r0, r0, #32\n"
        "   c:	0200      	lsls	r0, r0, #8\n"
        "   e:	0000      	movs	r0, r0\n"
        "  10:	2000      	movs	r0, #0\n"
        "  12:	0200      	lsls	r0, r0, #8\n"
        "  14:	0a00      	lsrs	r0, r0, #8\n"
        "  16:	0000      	movs	r0, r0\n"
        "  18:	1000      	asrs	r0, r0, #32\n"
        "  1a:	1400      	asrs	r0, r0, #16\n"
        "  1c:	0000      	movs	r0, r0\n"
        "  1e:	2000      	movs	r0, #0"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test return_labels_and_lines/2 with unaligned offset
return_labels_and_lines_unaligned_test() ->
    % Create a new state with a 2-byte instruction already in the stream
    % to simulate starting at an odd offset (offset 2 instead of 0)
    PaddingInstruction = jit_armv6m_asm:bx(lr),
    TempState = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    TempStream = jit_stream_binary:append(?BACKEND:stream(TempState), PaddingInstruction),
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, TempStream),

    % Test return_labels_and_lines with some sample labels and lines
    State1 = ?BACKEND:add_label(State0, 2, 32),
    State2 = ?BACKEND:add_label(State1, 1, 16),

    % {Line, Offset} pairs
    SortedLines = [{10, 16}, {20, 32}],

    State3 = ?BACKEND:return_labels_and_lines(State2, SortedLines),
    Stream = ?BACKEND:stream(State3),

    Dump = <<
        "   0:	4770      	bx	lr\n"
        "2:	a001      	add	r0, pc, #4	; (adr r0, 0x8)\n"
        "4:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "6:	0000      	movs	r0, r0\n"
        "8:	0200      	lsls	r0, r0, #8\n"
        "a:	0100      	lsls	r0, r0, #4\n"
        "c:	0000      	movs	r0, r0\n"
        "e:	1000      	asrs	r0, r0, #32\n"
        "10:	0200      	lsls	r0, r0, #8\n"
        "12:	0000      	movs	r0, r0\n"
        "14:	2000      	movs	r0, #0\n"
        "16:	0200      	lsls	r0, r0, #8\n"
        "18:	0a00      	lsrs	r0, r0, #8\n"
        "1a:	0000      	movs	r0, r0\n"
        "1c:	1000      	asrs	r0, r0, #32\n"
        "1e:	1400      	asrs	r0, r0, #16\n"
        "20:	0000      	movs	r0, r0\n"
        "22:	2000      	movs	r0, #0"
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
        "   0:	6a17      	ldr	r7, [r2, #32]\n"
        "   2:	b405      	push	{r0, r2}\n"
        "   4:	9802      	ldr	r0, [sp, #8]\n"
        "   6:	212a      	movs	r1, #42	; 0x2a\n"
        "   8:	47b8      	blx	r7\n"
        "   a:	4607      	mov	r7, r0\n"
        "   c:	bc05      	pop	{r0, r2}\n"
        "   e:	b405      	push	{r0, r2}\n"
        "  10:	b082      	sub	sp, #8\n"
        "  12:	6986      	ldr	r6, [r0, #24]\n"
        "  14:	9600      	str	r6, [sp, #0]\n"
        "  16:	2100      	movs	r1, #0\n"
        "  18:	2203      	movs	r2, #3\n"
        "  1a:	6946      	ldr	r6, [r0, #20]\n"
        "  1c:	6833      	ldr	r3, [r6, #0]\n"
        "  1e:	47b8      	blx	r7\n"
        "  20:	4607      	mov	r7, r0\n"
        "  22:	b002      	add	sp, #8\n"
        "  24:	bc05      	pop	{r0, r2}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test case where parameter value is in r1
memory_ensure_free_with_roots_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, {free, r1}, 4, 1
    ]),

    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	27b0      	movs	r7, #176	; 0xb0\n"
        "   2:	59d7      	ldr	r7, [r2, r7]\n"
        "   4:	b405      	push	{r0, r2}\n"
        "   6:	b082      	sub	sp, #8\n"
        "   8:	2601      	movs	r6, #1\n"
        "   a:	9600      	str	r6, [sp, #0]\n"
        "   c:	460e      	mov	r6, r1\n"
        "   e:	9904      	ldr	r1, [sp, #16]\n"
        "  10:	4632      	mov	r2, r6\n"
        "  12:	2304      	movs	r3, #4\n"
        "  14:	47b8      	blx	r7\n"
        "  16:	4607      	mov	r7, r0\n"
        "  18:	b002      	add	sp, #8\n"
        "  1a:	bc05      	pop	{r0, r2}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	9e00      	ldr	r6, [sp, #0]\n"
        "   2:	68b7      	ldr	r7, [r6, #8]\n"
        "   4:	3f01      	subs	r7, #1\n"
        "   6:	60b7      	str	r7, [r6, #8]\n"
        "   8:	d109      	bne.n	0x1e\n"
        "   a:	a704      	add	r7, pc, #16	; (adr r7, 0x1c)\n"
        "   c:	3701      	adds	r7, #1\n"
        "   e:	6077      	str	r7, [r6, #4]\n"
        "  10:	6897      	ldr	r7, [r2, #8]\n"
        "  12:	9e05      	ldr	r6, [sp, #20]\n"
        "  14:	9705      	str	r7, [sp, #20]\n"
        "  16:	46b6      	mov	lr, r6\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1e:	9e00      	ldr	r6, [sp, #0]\n"
        "  20:	6837      	ldr	r7, [r6, #0]\n"
        "  22:	683f      	ldr	r7, [r7, #0]\n"
        "  24:	063f      	lsls	r7, r7, #24\n"
        "  26:	4e07      	ldr	r6, [pc, #28]	; (0x44)\n"
        "  28:	4337      	orrs	r7, r6\n"
        "  2a:	65c7      	str	r7, [r0, #92]	; 0x5c\n"
        "  2c:	6917      	ldr	r7, [r2, #16]\n"
        "  2e:	b082      	sub	sp, #8\n"
        "  30:	2601      	movs	r6, #1\n"
        "  32:	4276      	negs	r6, r6\n"
        "  34:	9600      	str	r6, [sp, #0]\n"
        "  36:	9902      	ldr	r1, [sp, #8]\n"
        "  38:	2202      	movs	r2, #2\n"
        "  3a:	2305      	movs	r3, #5\n"
        "  3c:	47b8      	blx	r7\n"
        "  3e:	b002      	add	sp, #8\n"
        "  40:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  42:	0000      	movs	r0, r0\n"
        "  44:	0120      	lsls	r0, r4, #4\n"
        "  46:	0000      	movs	r0, r0\n"
        "  48:	b5f2      	push	{r1, r4, r5, r6, r7, lr}"
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
        "   0:	9e00      	ldr	r6, [sp, #0]\n"
        "   2:	68b7      	ldr	r7, [r6, #8]\n"
        "   4:	3f01      	subs	r7, #1\n"
        "   6:	60b7      	str	r7, [r6, #8]\n"
        "   8:	d109      	bne.n	0x1e\n"
        "   a:	a704      	add	r7, pc, #16	; (adr r7, 0x1c)\n"
        "   c:	3701      	adds	r7, #1\n"
        "   e:	6077      	str	r7, [r6, #4]\n"
        "  10:	6897      	ldr	r7, [r2, #8]\n"
        "  12:	9e05      	ldr	r6, [sp, #20]\n"
        "  14:	9705      	str	r7, [sp, #20]\n"
        "  16:	46b6      	mov	lr, r6\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1e:	6987      	ldr	r7, [r0, #24]\n"
        "  20:	463e      	mov	r6, r7\n"
        "  22:	4635      	mov	r5, r6\n"
        "  24:	2403      	movs	r4, #3\n"
        "  26:	4025      	ands	r5, r4\n"
        "  28:	2d02      	cmp	r5, #2\n"
        "  2a:	d00c      	beq.n	0x46\n"
        "  2c:	6cd7      	ldr	r7, [r2, #76]	; 0x4c\n"
        "  2e:	b082      	sub	sp, #8\n"
        "  30:	9600      	str	r6, [sp, #0]\n"
        "  32:	9902      	ldr	r1, [sp, #8]\n"
        "  34:	222e      	movs	r2, #46	; 0x2e\n"
        "  36:	4b01      	ldr	r3, [pc, #4]	; (0x3c)\n"
        "  38:	e002      	b.n	0x40\n"
        "  3a:	0000      	movs	r0, r0\n"
        "  3c:	018b      	lsls	r3, r1, #6\n"
        "  3e:	0000      	movs	r0, r0\n"
        "  40:	47b8      	blx	r7\n"
        "  42:	b002      	add	sp, #8\n"
        "  44:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  46:	2503      	movs	r5, #3\n"
        "  48:	43ae      	bics	r6, r5\n"
        "  4a:	6836      	ldr	r6, [r6, #0]\n"
        "  4c:	4635      	mov	r5, r6\n"
        "  4e:	243f      	movs	r4, #63	; 0x3f\n"
        "  50:	4025      	ands	r5, r4\n"
        "  52:	2d14      	cmp	r5, #20\n"
        "  54:	d00b      	beq.n	0x6e\n"
        "  56:	6cd7      	ldr	r7, [r2, #76]	; 0x4c\n"
        "  58:	b082      	sub	sp, #8\n"
        "  5a:	9600      	str	r6, [sp, #0]\n"
        "  5c:	9902      	ldr	r1, [sp, #8]\n"
        "  5e:	2258      	movs	r2, #88	; 0x58\n"
        "  60:	4b00      	ldr	r3, [pc, #0]	; (0x64)\n"
        "  62:	e001      	b.n	0x68\n"
        "  64:	018b      	lsls	r3, r1, #6\n"
        "  66:	0000      	movs	r0, r0\n"
        "  68:	47b8      	blx	r7\n"
        "  6a:	b002      	add	sp, #8\n"
        "  6c:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  6e:	9d00      	ldr	r5, [sp, #0]\n"
        "  70:	682e      	ldr	r6, [r5, #0]\n"
        "  72:	6836      	ldr	r6, [r6, #0]\n"
        "  74:	0636      	lsls	r6, r6, #24\n"
        "  76:	4d05      	ldr	r5, [pc, #20]	; (0x8c)\n"
        "  78:	432e      	orrs	r6, r5\n"
        "  7a:	65c6      	str	r6, [r0, #92]	; 0x5c\n"
        "  7c:	2680      	movs	r6, #128	; 0x80\n"
        "  7e:	5996      	ldr	r6, [r2, r6]\n"
        "  80:	463a      	mov	r2, r7\n"
        "  82:	2300      	movs	r3, #0\n"
        "  84:	9f05      	ldr	r7, [sp, #20]\n"
        "  86:	9605      	str	r6, [sp, #20]\n"
        "  88:	46be      	mov	lr, r7\n"
        "  8a:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  8c:	0240      	lsls	r0, r0, #9\n"
        "  8e:	0000      	movs	r0, r0\n"
        "  90:	b5f2      	push	{r1, r4, r5, r6, r7, lr}"
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
                        "   0:	2600      	movs	r6, #0\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
                        "   4:	60be      	str	r6, [r7, #8]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	2600      	movs	r6, #0\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
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
                        "   0:	262a      	movs	r6, #42	; 0x2a\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
                        "   4:	60be      	str	r6, [r7, #8]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	262a      	movs	r6, #42	; 0x2a\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
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
                        "   0:	6946      	ldr	r6, [r0, #20]\n"
                        "   2:	6837      	ldr	r7, [r6, #0]\n"
                        "   4:	6247      	str	r7, [r0, #36]	; 0x24"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	6946      	ldr	r6, [r0, #20]\n"
                        "   2:	6877      	ldr	r7, [r6, #4]\n"
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
                        "   0:	6946      	ldr	r6, [r0, #20]\n"
                        "   2:	6ff7      	ldr	r7, [r6, #124]	; 0x7c\n"
                        "   4:	6547      	str	r7, [r0, #84]	; 0x54"
                    >>)
                end),
                %% Test: Large y_reg index (32) that exceeds str immediate offset limit
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 32}, <<
                        "   0:	262a      	movs	r6, #42	; 0x2a\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
                        "   4:	2580      	movs	r5, #128	; 0x80\n"
                        "   6:	443d      	add	r5, r7\n"
                        "   8:	602e      	str	r6, [r5, #0]"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	2701      	movs	r7, #1\n"
                        "   2:	427f      	negs	r7, r7\n"
                        "   4:	6187      	str	r7, [r0, #24]"
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
                        "   0:	685e      	ldr	r6, [r3, #4]\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
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
                        "   0:	69de      	ldr	r6, [r3, #28]\n"
                        "   2:	6947      	ldr	r7, [r0, #20]\n"
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
                        "   4:	59df      	ldr	r7, [r3, r7]\n"
                        "   6:	6946      	ldr	r6, [r0, #20]\n"
                        "   8:	67f7      	str	r7, [r6, #124]	; 0x7c"
                    >>)
                end),
                %% move_array_element with integer index and x_reg destination
                ?_test(begin
                    {State1, BaseReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    move_array_element_test0(State1, BaseReg, 2, {x_reg, 5}, <<
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	68be      	ldr	r6, [r7, #8]\n"
                        "   4:	62c6      	str	r6, [r0, #44]	; 0x2c"
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
                        "   2:	4626      	mov	r6, r4\n"
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
                        "   2:	4626      	mov	r6, r4\n"
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
                        "   0:	6946      	ldr	r6, [r0, #20]\n"
                        "   2:	68b7      	ldr	r7, [r6, #8]\n"
                        "   4:	4626      	mov	r6, r4\n"
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
                %% move_to_native_register/2: negative value
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	272a      	movs	r7, #42	; 0x2a\n"
                        "   2:	427f      	negs	r7, r7"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: -255 (boundary case)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -255),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	27ff      	movs	r7, #255	; 0xff\n"
                        "   2:	427f      	negs	r7, r7"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_native_register/2: -256 (boundary case, should use literal pool)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -256),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	ff00 ffff 	vmaxnm.f32	<illegal reg q7.5>, q8, <illegal reg q15.5>"
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
                        "   0:	6946      	ldr	r6, [r0, #20]\n"
                        "   2:	68f7      	ldr	r7, [r6, #12]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
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
                        "   0:	463d      	mov	r5, r7"
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
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	68b9      	ldr	r1, [r7, #8]"
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
                        "   0:	6987      	ldr	r7, [r0, #24]\n"
                        "   2:	6e06      	ldr	r6, [r0, #96]	; 0x60\n"
                        "   4:	687d      	ldr	r5, [r7, #4]\n"
                        "   6:	61b5      	str	r5, [r6, #24]\n"
                        "   8:	68bd      	ldr	r5, [r7, #8]\n"
                        "   a:	61f5      	str	r5, [r6, #28]"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

add_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:add(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(dump_to_bin(Dump), Stream).

add_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    add_test0(State0, r2, 2, <<
                        "   0:	3202      	adds	r2, #2"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, r2, 256, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	0100      	lsls	r0, r0, #4\n"
                        "   6:	0000      	movs	r0, r0\n"
                        "   8:	19d2      	adds	r2, r2, r7"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, r2, r3, <<
                        "   0:	18d2      	adds	r2, r2, r3"
                    >>)
                end)
            ]
        end}.

sub_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:sub(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(dump_to_bin(Dump), Stream).

sub_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    sub_test0(State0, r2, 2, <<
                        "   0:	3a02      	subs	r2, #2"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, r2, 256, <<
                        "   0:	4f00      	ldr	r7, [pc, #0]	; (0x4)\n"
                        "   2:	e001      	b.n	0x8\n"
                        "   4:	0100      	lsls	r0, r0, #4\n"
                        "   6:	0000      	movs	r0, r0\n"
                        "   8:	1bd2      	subs	r2, r2, r7"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, r2, r3, <<
                        "   0:	1ad2      	subs	r2, r2, r3"
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
                    mul_test0(State0, r2, 2, <<
                        "   0:	0052      	lsls	r2, r2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 3, <<
                        "   0:	0057      	lsls	r7, r2, #1\n"
                        "   2:	18ba      	adds	r2, r7, r2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 4, <<
                        "   0:	0092      	lsls	r2, r2, #2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 5, <<
                        "   0:	0097      	lsls	r7, r2, #2\n"
                        "   2:	18ba      	adds	r2, r7, r2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 6, <<
                        "   0:	0057      	lsls	r7, r2, #1\n"
                        "   2:	18ba      	adds	r2, r7, r2\n"
                        "   4:	0052      	lsls	r2, r2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 7, <<
                        "   0:	00d7      	lsls	r7, r2, #3\n"
                        "   2:	1aba      	subs	r2, r7, r2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 8, <<
                        "   0:	00d2      	lsls	r2, r2, #3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 9, <<
                        "   0:	00d7      	lsls	r7, r2, #3\n"
                        "   2:	18ba      	adds	r2, r7, r2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 10, <<
                        "   0:	0097      	lsls	r7, r2, #2\n"
                        "   2:	18ba      	adds	r2, r7, r2\n"
                        "   4:	0052      	lsls	r2, r2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 11, <<
                        "   0:	270b      	movs	r7, #11\n"
                        "   2:	437a      	muls	r2, r7"
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
        "   0:	2743      	movs	r7, #67	; 0x43\n"
        "   2:	00bf      	lsls	r7, r7, #2\n"
        "   4:	59d7      	ldr	r7, [r2, r7]\n"
        "   6:	b405      	push	{r0, r2}\n"
        "   8:	6946      	ldr	r6, [r0, #20]\n"
        "   a:	6970      	ldr	r0, [r6, #20]\n"
        "   c:	47b8      	blx	r7\n"
        "   e:	4607      	mov	r7, r0\n"
        "  10:	bc05      	pop	{r0, r2}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test large Y register read (Y=32, offset=128, exceeds 124-byte limit)
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move from a large Y register (32 * 4 = 128 bytes, exceeds 124-byte immediate limit)
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 32}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp register since offset 128 > 124
    Dump = <<
        "   0:	6946      	ldr	r6, [r0, #20]\n"
        "   2:	2780      	movs	r7, #128	; 0x80\n"
        "   4:	4437      	add	r7, r6\n"
        "   6:	683f      	ldr	r7, [r7, #0]"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(r7, Reg).

%% Test large Y register write with available temp registers
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get a native register first
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    % Move to a large Y register (40 * 4 = 160 bytes)
    State2 = ?BACKEND:move_to_vm_register(State1, SrcReg, {y_reg, 40}),
    Stream = ?BACKEND:stream(State2),
    % Expected: uses helper with two temp registers since we have registers available
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	6946      	ldr	r6, [r0, #20]\n"
        "   4:	25a0      	movs	r5, #160	; 0xa0\n"
        "   6:	4435      	add	r5, r6\n"
        "   8:	602f      	str	r7, [r5, #0]"
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
    % Expected: uses IP_REG (r12) fallback sequence
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	69c6      	ldr	r6, [r0, #28]\n"
        "   4:	6a05      	ldr	r5, [r0, #32]\n"
        "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
        "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
        "   a:	6941      	ldr	r1, [r0, #20]\n"
        "   c:	468c      	mov	ip, r1\n"
        "   e:	218c      	movs	r1, #140	; 0x8c\n"
        "  10:	4461      	add	r1, ip\n"
        "  12:	6809      	ldr	r1, [r1, #0]"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(r1, ResultReg).

%% Test large Y register write with register exhaustion (uses IP_REG fallback)
large_y_reg_write_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get a source register first
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    % Allocate most remaining registers to simulate exhaustion
    {State2, r6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r5} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r4} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r3} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Try to write to large Y register when only one temp register is available
    StateFinal = ?BACKEND:move_to_vm_register(State5, SrcReg, {y_reg, 50}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses IP_REG (r12) fallback sequence
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	69c6      	ldr	r6, [r0, #28]\n"
        "   4:	6a05      	ldr	r5, [r0, #32]\n"
        "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
        "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
        "   a:	6941      	ldr	r1, [r0, #20]\n"
        "   c:	468c      	mov	ip, r1\n"
        "   e:	21c8      	movs	r1, #200	; 0xc8\n"
        "  10:	4461      	add	r1, ip\n"
        "  12:	600f      	str	r7, [r1, #0]"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test boundary case: Y=31 (124 bytes, exactly at limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 31}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses direct addressing since 31 * 4 = 124 <= 124
    Dump = <<
        "   0:	6946      	ldr	r6, [r0, #20]\n"
        "   2:	6ff7      	ldr	r7, [r6, #124]	; 0x7c"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream),
    ?assertEqual(r7, Reg).

%% Test debugger function
debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	be00      	bkpt	0x0000"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

and_register_exhaustion_negative_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r5} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r4} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r3} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, r1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test negative immediate (-4) which should use BICS with r0 as temp
    StateResult = ?BACKEND:and_(StateNoRegs, r7, -4),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	69c6      	ldr	r6, [r0, #28]\n"
        "   4:	6a05      	ldr	r5, [r0, #32]\n"
        "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
        "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
        "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
        "   c:	4684      	mov	ip, r0\n"
        "   e:	2003      	movs	r0, #3\n"
        "  10:	4387      	bics	r7, r0\n"
        "  12:	4660      	mov	r0, ip"
    >>,
    ?assertEqual(dump_to_bin(ExpectedDump), Stream).

and_register_exhaustion_positive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r5} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r4} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r3} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, r1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test positive immediate (0x3F) which should use ANDS with r0 as temp
    StateResult = ?BACKEND:and_(StateNoRegs, r7, 16#3F),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	69c6      	ldr	r6, [r0, #28]\n"
        "   4:	6a05      	ldr	r5, [r0, #32]\n"
        "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
        "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
        "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
        "   c:	4684      	mov	ip, r0\n"
        "   e:	203f      	movs	r0, #63	; 0x3f\n"
        "  10:	4007      	ands	r7, r0\n"
        "  12:	4660      	mov	r0, ip"
    >>,
    ?assertEqual(dump_to_bin(ExpectedDump), Stream).

jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual((512 + 1) * 12, byte_size(Stream)).

alloc_boxed_integer_fragment_small_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 42}
    ]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6bd7      	ldr	r7, [r2, #60]	; 0x3c\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	222a      	movs	r2, #42	; 0x2a\n"
            "   6:	2300      	movs	r3, #0\n"
            "   8:	47b8      	blx	r7\n"
            "   a:	4607      	mov	r7, r0\n"
            "   c:	bc05      	pop	{r0, r2}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

alloc_boxed_integer_fragment_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6bd7      	ldr	r7, [r2, #60]	; 0x3c\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	4a00      	ldr	r2, [pc, #0]	; (0x8)\n"
            "   6:	e001      	b.n	0xc\n"
            "   8:	def0      	udf	#240	; 0xf0\n"
            "   a:	9abc      	ldr	r2, [sp, #752]	; 0x2f0\n"
            "   c:	4b00      	ldr	r3, [pc, #0]	; (0x10)\n"
            "   e:	e001      	b.n	0x14\n"
            "  10:	5678      	ldrsb	r0, [r7, r1]\n"
            "  12:	1234      	asrs	r4, r6, #8\n"
            "  14:	47b8      	blx	r7\n"
            "  16:	4607      	mov	r7, r0\n"
            "  18:	bc05      	pop	{r0, r2}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

%% Test for stack alignment issue in call_func_ptr
%% When we have an odd number of saved registers, the stack becomes misaligned
%% before the function call, violating ARM AAPCS which requires 8-byte alignment
call_func_ptr_stack_alignment_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r5} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r4} = ?BACKEND:call_func_ptr(State3, {free, r3}, [42]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	6987      	ldr	r7, [r0, #24]\n"
            "   2:	69c6      	ldr	r6, [r0, #28]\n"
            "   4:	6a05      	ldr	r5, [r0, #32]\n"
            "   6:	b4ed      	push	{r0, r2, r3, r5, r6, r7}\n"
            "   8:	202a      	movs	r0, #42	; 0x2a\n"
            "   a:	4798      	blx	r3\n"
            "   c:	4604      	mov	r4, r0\n"
            "   e:	bced      	pop	{r0, r2, r3, r5, r6, r7}"
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
            {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, r6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State3, r5} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
            {State4, r4} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
            {State5, r3} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
            {State6, r1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
            State6
        end,
        fun(State6) ->
            [
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, r6},
                        [ctx, jit_state, {free, r3}, 3, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	6987      	ldr	r7, [r0, #24]\n"
                            "   2:	69c6      	ldr	r6, [r0, #28]\n"
                            "   4:	6a05      	ldr	r5, [r0, #32]\n"
                            "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
                            "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
                            "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
                            "   c:	b4b7      	push	{r0, r1, r2, r4, r5, r7}\n"
                            "   e:	b082      	sub	sp, #8\n"
                            "  10:	2101      	movs	r1, #1\n"
                            "  12:	9100      	str	r1, [sp, #0]\n"
                            "  14:	9908      	ldr	r1, [sp, #32]\n"
                            "  16:	461a      	mov	r2, r3\n"
                            "  18:	2303      	movs	r3, #3\n"
                            "  1a:	47b0      	blx	r6\n"
                            "  1c:	4606      	mov	r6, r0\n"
                            "  1e:	b002      	add	sp, #8\n"
                            "  20:	bcb7      	pop	{r0, r1, r2, r4, r5, r7}"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, r6},
                        [ctx, jit_state, {free, r3}, 1, r1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	6987      	ldr	r7, [r0, #24]\n"
                            "   2:	69c6      	ldr	r6, [r0, #28]\n"
                            "   4:	6a05      	ldr	r5, [r0, #32]\n"
                            "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
                            "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
                            "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
                            "   c:	b4b7      	push	{r0, r1, r2, r4, r5, r7}\n"
                            "   e:	b082      	sub	sp, #8\n"
                            "  10:	9100      	str	r1, [sp, #0]\n"
                            "  12:	9908      	ldr	r1, [sp, #32]\n"
                            "  14:	461a      	mov	r2, r3\n"
                            "  16:	2301      	movs	r3, #1\n"
                            "  18:	47b0      	blx	r6\n"
                            "  1a:	4606      	mov	r6, r0\n"
                            "  1c:	b002      	add	sp, #8\n"
                            "  1e:	bcb7      	pop	{r0, r1, r2, r4, r5, r7}"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, r6},
                        [ctx, jit_state, {free, r3}, r1, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	6987      	ldr	r7, [r0, #24]\n"
                            "   2:	69c6      	ldr	r6, [r0, #28]\n"
                            "   4:	6a05      	ldr	r5, [r0, #32]\n"
                            "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
                            "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
                            "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
                            "   c:	b4b7      	push	{r0, r1, r2, r4, r5, r7}\n"
                            "   e:	b082      	sub	sp, #8\n"
                            "  10:	2401      	movs	r4, #1\n"
                            "  12:	9400      	str	r4, [sp, #0]\n"
                            "  14:	460f      	mov	r7, r1\n"
                            "  16:	9908      	ldr	r1, [sp, #32]\n"
                            "  18:	461a      	mov	r2, r3\n"
                            "  1a:	463b      	mov	r3, r7\n"
                            "  1c:	47b0      	blx	r6\n"
                            "  1e:	4606      	mov	r6, r0\n"
                            "  20:	b002      	add	sp, #8\n"
                            "  22:	bcb7      	pop	{r0, r1, r2, r4, r5, r7}"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual(r6, ResultReg)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, r1},
                        [r6, r3]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	6987      	ldr	r7, [r0, #24]\n"
                            "   2:	69c6      	ldr	r6, [r0, #28]\n"
                            "   4:	6a05      	ldr	r5, [r0, #32]\n"
                            "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
                            "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
                            "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
                            "   c:	b4ff      	push	{r0, r1, r2, r3, r4, r5, r6, r7}\n"
                            "   e:	460c      	mov	r4, r1\n"
                            "  10:	4630      	mov	r0, r6\n"
                            "  12:	4619      	mov	r1, r3\n"
                            "  14:	47a0      	blx	r4\n"
                            "  16:	9001      	str	r0, [sp, #4]\n"
                            "  18:	bcff      	pop	{r0, r1, r2, r3, r4, r5, r6, r7}"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {primitive, 2},
                        [{free, r6}, r3]
                    ),
                    ?assertEqual(ResultReg, r6),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	6987      	ldr	r7, [r0, #24]\n"
                            "   2:	69c6      	ldr	r6, [r0, #28]\n"
                            "   4:	6a05      	ldr	r5, [r0, #32]\n"
                            "   6:	6a44      	ldr	r4, [r0, #36]	; 0x24\n"
                            "   8:	6a83      	ldr	r3, [r0, #40]	; 0x28\n"
                            "   a:	6ac1      	ldr	r1, [r0, #44]	; 0x2c\n"
                            "   c:	b4ff      	push	{r0, r1, r2, r3, r4, r5, r6, r7}\n"
                            "   e:	6894      	ldr	r4, [r2, #8]\n"
                            "  10:	4630      	mov	r0, r6\n"
                            "  12:	4619      	mov	r1, r3\n"
                            "  14:	47a0      	blx	r4\n"
                            "  16:	9006      	str	r0, [sp, #24]\n"
                            "  18:	bcff      	pop	{r0, r1, r2, r3, r4, r5, r6, r7}"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

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
    Dump =
        <<
            % jump table
            "   0:	4b01      	ldr	r3, [pc, #4]	; (0x8)\n"
            "   2:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   4:	449f      	add	pc, r3\n"
            "   6:	46c0      	nop			; (mov r8, r8)\n"
            "   8:	00d8      	lsls	r0, r3, #3\n"
            "   a:	0000      	movs	r0, r0\n"
            "   c:	4b01      	ldr	r3, [pc, #4]	; (0x14)\n"
            "   e:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  10:	449f      	add	pc, r3\n"
            "  12:	46c0      	nop			; (mov r8, r8)\n"
            "  14:	001c      	movs	r4, r3\n"
            "  16:	0000      	movs	r0, r0\n"
            "  18:	4b01      	ldr	r3, [pc, #4]	; (0x20)\n"
            "  1a:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  1c:	449f      	add	pc, r3\n"
            "  1e:	46c0      	nop			; (mov r8, r8)\n"
            "  20:	0044      	lsls	r4, r0, #1\n"
            "  22:	0000      	movs	r0, r0\n"
            "  24:	4b01      	ldr	r3, [pc, #4]	; (0x2c)\n"
            "  26:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  28:	449f      	add	pc, r3\n"
            "  2a:	46c0      	nop			; (mov r8, r8)\n"
            "  2c:	00a8      	lsls	r0, r5, #2\n"
            "  2e:	0000      	movs	r0, r0\n"
            % label 1
            % {move,{integer,9},{x,1}}.
            "  30:	279f      	movs	r7, #159	; 0x9f\n"
            "  32:	61c7      	str	r7, [r0, #28]\n"
            % {move,{integer,8},{x,0}}
            "  34:	278f      	movs	r7, #143	; 0x8f\n"
            "  36:	6187      	str	r7, [r0, #24]\n"
            % {call_only,2,{f,2}}.
            "  38:	9e00      	ldr	r6, [sp, #0]\n"
            "  3a:	68b7      	ldr	r7, [r6, #8]\n"
            "  3c:	3f01      	subs	r7, #1\n"
            "  3e:	60b7      	str	r7, [r6, #8]\n"
            "  40:	d004      	beq.n	0x4c\n"
            "  42:	e00f      	b.n	0x64\n"
            "  44:	46c0      	nop			; (mov r8, r8)\n"
            "  46:	46c0      	nop			; (mov r8, r8)\n"
            "  48:	46c0      	nop			; (mov r8, r8)\n"
            "  4a:	46c0      	nop			; (mov r8, r8)\n"
            "  4c:	a700      	add	r7, pc, #0	; (adr r7, 0x50)\n"
            "  4e:	2637      	movs	r6, #55	; 0x37\n"
            "  50:	4276      	negs	r6, r6\n"
            "  52:	19f6      	adds	r6, r6, r7\n"
            "  54:	9f00      	ldr	r7, [sp, #0]\n"
            "  56:	607e      	str	r6, [r7, #4]\n"
            "  58:	6897      	ldr	r7, [r2, #8]\n"
            "  5a:	9e05      	ldr	r6, [sp, #20]\n"
            "  5c:	9705      	str	r7, [sp, #20]\n"
            "  5e:	46b6      	mov	lr, r6\n"
            "  60:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  62:	46c0      	nop			; (mov r8, r8)\n"
            % label 2
            % {allocate,1,1}.
            "  64:	6957      	ldr	r7, [r2, #20]\n"
            "  66:	b405      	push	{r0, r2}\n"
            "  68:	b082      	sub	sp, #8\n"
            "  6a:	2601      	movs	r6, #1\n"
            "  6c:	9600      	str	r6, [sp, #0]\n"
            "  6e:	9904      	ldr	r1, [sp, #16]\n"
            "  70:	2201      	movs	r2, #1\n"
            "  72:	2300      	movs	r3, #0\n"
            "  74:	47b8      	blx	r7\n"
            "  76:	4607      	mov	r7, r0\n"
            "  78:	b002      	add	sp, #8\n"
            "  7a:	bc05      	pop	{r0, r2}\n"
            "  7c:	07fe      	lsls	r6, r7, #31\n"
            "  7e:	d405      	bmi.n	0x8c\n"
            "  80:	6997      	ldr	r7, [r2, #24]\n"
            "  82:	2282      	movs	r2, #130	; 0x82\n"
            "  84:	9e05      	ldr	r6, [sp, #20]\n"
            "  86:	9705      	str	r7, [sp, #20]\n"
            "  88:	46b6      	mov	lr, r6\n"
            "  8a:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            % {init_yregs,{list,[{y,0}]}}.
            %% move_to_vm_register(State8, ?TERM_NIL, {y_reg, 0}),
            "  8c:	263b      	movs	r6, #59	; 0x3b\n"
            "  8e:	6947      	ldr	r7, [r0, #20]\n"
            "  90:	603e      	str	r6, [r7, #0]\n"
            % {call,1,{f,3}}
            %% call_or_schedule_next(State9, 3),
            "  92:	9e00      	ldr	r6, [sp, #0]\n"
            "  94:	6837      	ldr	r7, [r6, #0]\n"
            "  96:	683f      	ldr	r7, [r7, #0]\n"
            "  98:	063f      	lsls	r7, r7, #24\n"
            "  9a:	4e0c      	ldr	r6, [pc, #48]	; (0xcc)\n"
            "  9c:	4337      	orrs	r7, r6\n"
            "  9e:	65c7      	str	r7, [r0, #92]	; 0x5c\n"
            "  a0:	9e00      	ldr	r6, [sp, #0]\n"
            "  a2:	68b7      	ldr	r7, [r6, #8]\n"
            "  a4:	3f01      	subs	r7, #1\n"
            "  a6:	60b7      	str	r7, [r6, #8]\n"
            "  a8:	d004      	beq.n	0xb4\n"
            "  aa:	e013      	b.n	0xd4\n"
            "  ac:	46c0      	nop			; (mov r8, r8)\n"
            "  ae:	46c0      	nop			; (mov r8, r8)\n"
            "  b0:	46c0      	nop			; (mov r8, r8)\n"
            "  b2:	46c0      	nop			; (mov r8, r8)\n"
            "  b4:	a700      	add	r7, pc, #0	; (adr r7, 0xb8)\n"
            "  b6:	2693      	movs	r6, #147	; 0x93\n"
            "  b8:	4276      	negs	r6, r6\n"
            "  ba:	19f6      	adds	r6, r6, r7\n"
            "  bc:	9f00      	ldr	r7, [sp, #0]\n"
            "  be:	607e      	str	r6, [r7, #4]\n"
            "  c0:	6897      	ldr	r7, [r2, #8]\n"
            "  c2:	9e05      	ldr	r6, [sp, #20]\n"
            "  c4:	9705      	str	r7, [sp, #20]\n"
            "  c6:	46b6      	mov	lr, r6\n"
            "  c8:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  ca:	0000      	movs	r0, r0\n"
            "  cc:	0340      	lsls	r0, r0, #13\n"
            "  ce:	0000      	movs	r0, r0\n"
            %% (continuation)
            "  d0:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "  d2:	46c0      	nop			; (mov r8, r8)\n"
            % label 3
            "  d4:	6857      	ldr	r7, [r2, #4]\n"
            "  d6:	9e05      	ldr	r6, [sp, #20]\n"
            "  d8:	9705      	str	r7, [sp, #20]\n"
            "  da:	46b6      	mov	lr, r6\n"
            "  dc:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  de:	46c0      	nop			; (mov r8, r8)\n"
            % label 0
            "  e0:	6857      	ldr	r7, [r2, #4]\n"
            "  e2:	9e05      	ldr	r6, [sp, #20]\n"
            "  e4:	9705      	str	r7, [sp, #20]\n"
            "  e6:	46b6      	mov	lr, r6\n"
            "  e8:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
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
    dump_to_bin0(Rest, instr, [<<InstrB:16/little>>, <<InstrA:16/little>> | Acc]);
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
