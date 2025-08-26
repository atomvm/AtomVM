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
            "   4:	9900      	ldr	r1, [sp, #0]\n"
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
            "   4:	9900      	ldr	r1, [sp, #0]\n"
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
            "   2:	2210      	movs	r2, #16\n"
            "   4:	2320      	movs	r3, #32\n"
            "   6:	2502      	movs	r5, #2\n"
            "   8:	4639      	mov	r1, r7\n"
            "   a:	9f05      	ldr	r7, [sp, #20]\n"
            "   c:	46be      	mov	lr, r7\n"
            "   e:	9f04      	ldr	r7, [sp, #16]\n"
            "  10:	9504      	str	r5, [sp, #16]\n"
            "  12:	9105      	str	r1, [sp, #20]\n"
            "  14:	9e03      	ldr	r6, [sp, #12]\n"
            "  16:	bd32      	pop	{r1, r4, r5, pc}"
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
            "  16:	9900      	ldr	r1, [sp, #0]\n"
            "  18:	463a      	mov	r2, r7\n"
            "  1a:	2340      	movs	r3, #64	; 0x40\n"
            "  1c:	47a8      	blx	r5\n"
            "  1e:	4605      	mov	r5, r0\n"
            "  20:	bc05      	pop	{r0, r2}"
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
        "   e:	b485      	push	{r0, r2, r7}\n"
        "  10:	2114      	movs	r1, #20\n"
        "  12:	47b0      	blx	r6\n"
        "  14:	4606      	mov	r6, r0\n"
        "  16:	bc85      	pop	{r0, r2, r7}\n"
        "  18:	6c95      	ldr	r5, [r2, #72]	; 0x48\n"
        "  1a:	b4c5      	push	{r0, r2, r6, r7}\n"
        "  1c:	2113      	movs	r1, #19\n"
        "  1e:	47a8      	blx	r5\n"
        "  20:	4605      	mov	r5, r0\n"
        "  22:	bcc5      	pop	{r0, r2, r6, r7}\n"
        "  24:	6b54      	ldr	r4, [r2, #52]	; 0x34\n"
        "  26:	b425      	push	{r0, r2, r5}\n"
        "  28:	6839      	ldr	r1, [r7, #0]\n"
        "  2a:	6832      	ldr	r2, [r6, #0]\n"
        "  2c:	47a0      	blx	r4\n"
        "  2e:	4604      	mov	r4, r0\n"
        "  30:	bc25      	pop	{r0, r2, r5}\n"
        "  32:	602c      	str	r4, [r5, #0]"
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
        "   8:	d107      	bne.n	0x1a\n"
        "   a:	a703      	add	r7, pc, #12	; (adr r7, 0x18)\n"
        "   c:	6077      	str	r7, [r6, #4]\n"
        "   e:	6897      	ldr	r7, [r2, #8]\n"
        "  10:	9e05      	ldr	r6, [sp, #20]\n"
        "  12:	9705      	str	r7, [sp, #20]\n"
        "  14:	46b6      	mov	lr, r6\n"
        "  16:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  18:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1a:	6917      	ldr	r7, [r2, #16]\n"
        "  1c:	221c      	movs	r2, #28\n"
        "  1e:	2302      	movs	r3, #2\n"
        "  20:	2502      	movs	r5, #2\n"
        "  22:	4639      	mov	r1, r7\n"
        "  24:	9f05      	ldr	r7, [sp, #20]\n"
        "  26:	46be      	mov	lr, r7\n"
        "  28:	9f04      	ldr	r7, [sp, #16]\n"
        "  2a:	9504      	str	r5, [sp, #16]\n"
        "  2c:	9105      	str	r1, [sp, #20]\n"
        "  2e:	9603      	str	r6, [sp, #12]\n"
        "  30:	bd72      	pop	{r1, r4, r5, r6, pc}"
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
        "   0:	6019      	str	r1, [r3, #0]\n"
        "   2:	9e00      	ldr	r6, [sp, #0]\n"
        "   4:	68b7      	ldr	r7, [r6, #8]\n"
        "   6:	3f01      	subs	r7, #1\n"
        "   8:	60b7      	str	r7, [r6, #8]\n"
        "   a:	d108      	bne.n	0x1e\n"
        "   c:	a703      	add	r7, pc, #12	; (adr r7, 0x1c)\n"
        "   e:	6077      	str	r7, [r6, #4]\n"
        "  10:	6897      	ldr	r7, [r2, #8]\n"
        "  12:	9e05      	ldr	r6, [sp, #20]\n"
        "  14:	9705      	str	r7, [sp, #20]\n"
        "  16:	46b6      	mov	lr, r6\n"
        "  18:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  1a:	46c0      	nop			; (mov r8, r8)\n"
        "  1c:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1e:	6917      	ldr	r7, [r2, #16]\n"
        "  20:	2220      	movs	r2, #32\n"
        "  22:	2302      	movs	r3, #2\n"
        "  24:	2502      	movs	r5, #2\n"
        "  26:	4639      	mov	r1, r7\n"
        "  28:	9f05      	ldr	r7, [sp, #20]\n"
        "  2a:	46be      	mov	lr, r7\n"
        "  2c:	9f04      	ldr	r7, [sp, #16]\n"
        "  2e:	9504      	str	r5, [sp, #16]\n"
        "  30:	9105      	str	r1, [sp, #20]\n"
        "  32:	9603      	str	r6, [sp, #12]\n"
        "  34:	bd72      	pop	{r1, r4, r5, r6, pc}"
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
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	6cd6      	ldr	r6, [r2, #76]	; 0x4c\n"
        "   4:	2204      	movs	r2, #4\n"
        "   6:	4b01      	ldr	r3, [pc, #4]	; (0xc)\n"
        "   8:	e002      	b.n	0x10\n"
        "   a:	0000      	movs	r0, r0\n"
        "   c:	080b      	lsrs	r3, r1, #32\n"
        "   e:	0000      	movs	r0, r0\n"
        "  10:	463d      	mov	r5, r7\n"
        "  12:	4631      	mov	r1, r6\n"
        "  14:	9f05      	ldr	r7, [sp, #20]\n"
        "  16:	46be      	mov	lr, r7\n"
        "  18:	9f04      	ldr	r7, [sp, #16]\n"
        "  1a:	9504      	str	r5, [sp, #16]\n"
        "  1c:	9105      	str	r1, [sp, #20]\n"
        "  1e:	9e03      	ldr	r6, [sp, #12]\n"
        "  20:	bd32      	pop	{r1, r4, r5, pc}"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	9e00      	ldr	r6, [sp, #0]\n"
        "   2:	68b7      	ldr	r7, [r6, #8]\n"
        "   4:	3f01      	subs	r7, #1\n"
        "   6:	60b7      	str	r7, [r6, #8]\n"
        "   8:	d107      	bne.n	0x1a\n"
        "   a:	a703      	add	r7, pc, #12	; (adr r7, 0x18)\n"
        "   c:	6077      	str	r7, [r6, #4]\n"
        "   e:	6897      	ldr	r7, [r2, #8]\n"
        "  10:	9e05      	ldr	r6, [sp, #20]\n"
        "  12:	9705      	str	r7, [sp, #20]\n"
        "  14:	46b6      	mov	lr, r6\n"
        "  16:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  18:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1a:	6917      	ldr	r7, [r2, #16]\n"
        "  1c:	221c      	movs	r2, #28\n"
        "  1e:	2302      	movs	r3, #2\n"
        "  20:	2502      	movs	r5, #2\n"
        "  22:	4639      	mov	r1, r7\n"
        "  24:	9f05      	ldr	r7, [sp, #20]\n"
        "  26:	46be      	mov	lr, r7\n"
        "  28:	9f04      	ldr	r7, [sp, #16]\n"
        "  2a:	9504      	str	r5, [sp, #16]\n"
        "  2c:	9105      	str	r1, [sp, #20]\n"
        "  2e:	9603      	str	r6, [sp, #12]\n"
        "  30:	bd72      	pop	{r1, r4, r5, r6, pc}"
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
                            "   4:	9900      	ldr	r1, [sp, #0]\n"
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
                            "   4:	9900      	ldr	r1, [sp, #0]\n"
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
            "   0:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   2:	e018      	b.n	0x36\n"
            "   4:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   6:	e001      	b.n	0xc\n"
            "   8:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
            "   a:	e00f      	b.n	0x2c\n"
            "   c:	9e00      	ldr	r6, [sp, #0]\n"
            "   e:	68b7      	ldr	r7, [r6, #8]\n"
            "  10:	3f01      	subs	r7, #1\n"
            "  12:	60b7      	str	r7, [r6, #8]\n"
            "  14:	d10a      	bne.n	0x2c\n"
            "  16:	9e00      	ldr	r6, [sp, #0]\n"
            "  18:	a701      	add	r7, pc, #4	; (adr r7, 0x20)\n"
            "  1a:	6077      	str	r7, [r6, #4]\n"
            "  1c:	e001      	b.n	0x22\n"
            "  1e:	0000      	movs	r0, r0\n"
            "  20:	e7f2      	b.n	0x8\n"
            "  22:	6897      	ldr	r7, [r2, #8]\n"
            "  24:	9e05      	ldr	r6, [sp, #20]\n"
            "  26:	9705      	str	r7, [sp, #20]\n"
            "  28:	46b6      	mov	lr, r6\n"
            "  2a:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  2c:	6817      	ldr	r7, [r2, #0]\n"
            "  2e:	9e05      	ldr	r6, [sp, #20]\n"
            "  30:	9705      	str	r7, [sp, #20]\n"
            "  32:	46b6      	mov	lr, r6\n"
            "  34:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  36:	6857      	ldr	r7, [r2, #4]\n"
            "  38:	9e05      	ldr	r6, [sp, #20]\n"
            "  3a:	9705      	str	r7, [sp, #20]\n"
            "  3c:	46b6      	mov	lr, r6\n"
            "  3e:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

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
            "   4:	9800      	ldr	r0, [sp, #0]\n"
            "   6:	2102      	movs	r1, #2\n"
            "   8:	47b8      	blx	r7\n"
            "   a:	4607      	mov	r7, r0\n"
            "   c:	bc05      	pop	{r0, r2}\n"
            "   e:	6bd6      	ldr	r6, [r2, #60]	; 0x3c\n"
            "  10:	b485      	push	{r0, r2, r7}\n"
            "  12:	4901      	ldr	r1, [pc, #4]	; (0x18)\n"
            "  14:	e002      	b.n	0x1c\n"
            "  16:	0000      	movs	r0, r0\n"
            "  18:	e895 3b7f 	ldmia.w	r5, {r0, r1, r2, r3, r4, r5, r6, r8, r9, fp, ip, sp}\n"
            "  1c:	47b0      	blx	r6\n"
            "  1e:	4606      	mov	r6, r0\n"
            "  20:	bc85      	pop	{r0, r2, r7}\n"
            "  22:	b405      	push	{r0, r2}\n"
            "  24:	b082      	sub	sp, #8\n"
            "  26:	9600      	str	r6, [sp, #0]\n"
            "  28:	2100      	movs	r1, #0\n"
            "  2a:	2201      	movs	r2, #1\n"
            "  2c:	6983      	ldr	r3, [r0, #24]\n"
            "  2e:	47b8      	blx	r7\n"
            "  30:	4607      	mov	r7, r0\n"
            "  32:	bc05      	pop	{r0, r2}\n"
            "  34:	2f00      	cmp	r7, #0\n"
            "  36:	d105      	bne.n	0x44\n"
            "  38:	6997      	ldr	r7, [r2, #24]\n"
            "  3a:	223a      	movs	r2, #58	; 0x3a\n"
            "  3c:	9e05      	ldr	r6, [sp, #20]\n"
            "  3e:	9705      	str	r7, [sp, #20]\n"
            "  40:	46b6      	mov	lr, r6\n"
            "  42:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
            "  44:	6187      	str	r7, [r0, #24]"
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
        "   6:	6946      	ldr	r6, [r0, #20]\n"
        "   8:	687d      	ldr	r5, [r7, #4]\n"
        "   a:	6075      	str	r5, [r6, #4]\n"
        "   c:	6946      	ldr	r6, [r0, #20]\n"
        "   e:	683d      	ldr	r5, [r7, #0]\n"
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
    Offset = ?BACKEND:offset(State3),
    Labels = [{Label, Offset + 16#100}],
    State4 = ?BACKEND:update_branches(State3, Labels),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	43fe      	mvns	r6, r7\n"
        "   4:	0736      	lsls	r6, r6, #28\n"
        "   6:	d00d      	beq.n	0x24\n"
        "   8:	463e      	mov	r6, r7\n"
        "   a:	2503      	movs	r5, #3\n"
        "   c:	402e      	ands	r6, r5\n"
        "   e:	2e02      	cmp	r6, #2\n"
        "  10:	d000      	beq.n	0x14\n"
        "  12:	e087      	b.n	0x124\n"
        "  14:	2603      	movs	r6, #3\n"
        "  16:	43b7      	bics	r7, r6\n"
        "  18:	683f      	ldr	r7, [r7, #0]\n"
        "  1a:	263f      	movs	r6, #63	; 0x3f\n"
        "  1c:	4037      	ands	r7, r6\n"
        "  1e:	2f08      	cmp	r7, #8\n"
        "  20:	d000      	beq.n	0x24\n"
        "  22:	e07f      	b.n	0x124"
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
    Offset = ?BACKEND:offset(State3),
    Labels = [{Label, Offset + 16#100}],
    State4 = ?BACKEND:update_branches(State3, Labels),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	6987      	ldr	r7, [r0, #24]\n"
        "   2:	43fe      	mvns	r6, r7\n"
        "   4:	0736      	lsls	r6, r6, #28\n"
        "   6:	d012      	beq.n	0x2e\n"
        "   8:	463e      	mov	r6, r7\n"
        "   a:	2503      	movs	r5, #3\n"
        "   c:	402e      	ands	r6, r5\n"
        "   e:	2e02      	cmp	r6, #2\n"
        "  10:	d000      	beq.n	0x14\n"
        "  12:	e08c      	b.n	0x12e\n"
        "  14:	2603      	movs	r6, #3\n"
        "  16:	43b7      	bics	r7, r6\n"
        "  18:	683f      	ldr	r7, [r7, #0]\n"
        "  1a:	463e      	mov	r6, r7\n"
        "  1c:	253f      	movs	r5, #63	; 0x3f\n"
        "  1e:	402e      	ands	r6, r5\n"
        "  20:	2e08      	cmp	r6, #8\n"
        "  22:	d004      	beq.n	0x2e\n"
        "  24:	263f      	movs	r6, #63	; 0x3f\n"
        "  26:	4037      	ands	r7, r6\n"
        "  28:	2f18      	cmp	r7, #24\n"
        "  2a:	d000      	beq.n	0x2e\n"
        "  2c:	e07f      	b.n	0x12e"
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
        "   8:	d107      	bne.n	0x1a\n"
        "   a:	a703      	add	r7, pc, #12	; (adr r7, 0x18)\n"
        "   c:	6077      	str	r7, [r6, #4]\n"
        "   e:	6897      	ldr	r7, [r2, #8]\n"
        "  10:	9e05      	ldr	r6, [sp, #20]\n"
        "  12:	9705      	str	r7, [sp, #20]\n"
        "  14:	46b6      	mov	lr, r6\n"
        "  16:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  18:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1a:	9e00      	ldr	r6, [sp, #0]\n"
        "  1c:	6837      	ldr	r7, [r6, #0]\n"
        "  1e:	683f      	ldr	r7, [r7, #0]\n"
        "  20:	063f      	lsls	r7, r7, #24\n"
        "  22:	4d07      	ldr	r5, [pc, #28]	; (0x40)\n"
        "  24:	432f      	orrs	r7, r5\n"
        "  26:	65c7      	str	r7, [r0, #92]	; 0x5c\n"
        "  28:	6917      	ldr	r7, [r2, #16]\n"
        "  2a:	2202      	movs	r2, #2\n"
        "  2c:	2305      	movs	r3, #5\n"
        "  2e:	2500      	movs	r5, #0\n"
        "  30:	426d      	negs	r5, r5\n"
        "  32:	4639      	mov	r1, r7\n"
        "  34:	9f05      	ldr	r7, [sp, #20]\n"
        "  36:	46be      	mov	lr, r7\n"
        "  38:	9f04      	ldr	r7, [sp, #16]\n"
        "  3a:	9504      	str	r5, [sp, #16]\n"
        "  3c:	9105      	str	r1, [sp, #20]\n"
        "  3e:	9e03      	ldr	r6, [sp, #12]\n"
        "  40:	bd32      	pop	{r1, r4, r5, pc}\n"
        "  42:	0000      	movs	r0, r0\n"
        "  44:	0108      	lsls	r0, r1, #4\n"
        "  46:	0000      	movs	r0, r0"
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
        "   8:	d107      	bne.n	0x1a\n"
        "   a:	a703      	add	r7, pc, #12	; (adr r7, 0x18)\n"
        "   c:	6077      	str	r7, [r6, #4]\n"
        "   e:	6897      	ldr	r7, [r2, #8]\n"
        "  10:	9e05      	ldr	r6, [sp, #20]\n"
        "  12:	9705      	str	r7, [sp, #20]\n"
        "  14:	46b6      	mov	lr, r6\n"
        "  16:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  18:	b5f2      	push	{r1, r4, r5, r6, r7, lr}\n"
        "  1a:	6987      	ldr	r7, [r0, #24]\n"
        "  1c:	463e      	mov	r6, r7\n"
        "  1e:	4635      	mov	r5, r6\n"
        "  20:	2403      	movs	r4, #3\n"
        "  22:	4025      	ands	r5, r4\n"
        "  24:	2d02      	cmp	r5, #2\n"
        "  26:	d00e      	beq.n	0x46\n"
        "  28:	6cd7      	ldr	r7, [r2, #76]	; 0x4c\n"
        "  2a:	222a      	movs	r2, #42	; 0x2a\n"
        "  2c:	4b00      	ldr	r3, [pc, #0]	; (0x30)\n"
        "  2e:	e001      	b.n	0x34\n"
        "  30:	020b      	lsls	r3, r1, #8\n"
        "  32:	0000      	movs	r0, r0\n"
        "  34:	4635      	mov	r5, r6\n"
        "  36:	4639      	mov	r1, r7\n"
        "  38:	9f05      	ldr	r7, [sp, #20]\n"
        "  3a:	46be      	mov	lr, r7\n"
        "  3c:	9f04      	ldr	r7, [sp, #16]\n"
        "  3e:	9504      	str	r5, [sp, #16]\n"
        "  40:	9105      	str	r1, [sp, #20]\n"
        "  42:	9e03      	ldr	r6, [sp, #12]\n"
        "  44:	bd32      	pop	{r1, r4, r5, pc}\n"
        "  46:	2503      	movs	r5, #3\n"
        "  48:	43ae      	bics	r6, r5\n"
        "  4a:	6836      	ldr	r6, [r6, #0]\n"
        "  4c:	4635      	mov	r5, r6\n"
        "  4e:	243f      	movs	r4, #63	; 0x3f\n"
        "  50:	4025      	ands	r5, r4\n"
        "  52:	2d14      	cmp	r5, #20\n"
        "  54:	d00f      	beq.n	0x76\n"
        "  56:	6cd7      	ldr	r7, [r2, #76]	; 0x4c\n"
        "  58:	2258      	movs	r2, #88	; 0x58\n"
        "  5a:	4b01      	ldr	r3, [pc, #4]	; (0x60)\n"
        "  5c:	e002      	b.n	0x64\n"
        "  5e:	0000      	movs	r0, r0\n"
        "  60:	020b      	lsls	r3, r1, #8\n"
        "  62:	0000      	movs	r0, r0\n"
        "  64:	4635      	mov	r5, r6\n"
        "  66:	4639      	mov	r1, r7\n"
        "  68:	9f05      	ldr	r7, [sp, #20]\n"
        "  6a:	46be      	mov	lr, r7\n"
        "  6c:	9f04      	ldr	r7, [sp, #16]\n"
        "  6e:	9504      	str	r5, [sp, #16]\n"
        "  70:	9105      	str	r1, [sp, #20]\n"
        "  72:	9e03      	ldr	r6, [sp, #12]\n"
        "  74:	bd32      	pop	{r1, r4, r5, pc}\n"
        "  76:	9d00      	ldr	r5, [sp, #0]\n"
        "  78:	682e      	ldr	r6, [r5, #0]\n"
        "  7a:	6836      	ldr	r6, [r6, #0]\n"
        "  7c:	0636      	lsls	r6, r6, #24\n"
        "  7e:	4c04      	ldr	r4, [pc, #16]	; (0x90)\n"
        "  80:	4326      	orrs	r6, r4\n"
        "  82:	65c6      	str	r6, [r0, #92]	; 0x5c\n"
        "  84:	2680      	movs	r6, #128	; 0x80\n"
        "  86:	5996      	ldr	r6, [r2, r6]\n"
        "  88:	463a      	mov	r2, r7\n"
        "  8a:	2300      	movs	r3, #0\n"
        "  8c:	9f05      	ldr	r7, [sp, #20]\n"
        "  8e:	9605      	str	r6, [sp, #20]\n"
        "  90:	46be      	mov	lr, r7\n"
        "  92:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  94:	0250      	lsls	r0, r2, #9\n"
        "  96:	0000      	movs	r0, r0"
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
                        "   0:	2700      	movs	r7, #0\n"
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
                        "   0:	6947      	ldr	r7, [r0, #20]\n"
                        "   2:	68bf      	ldr	r7, [r7, #8]\n"
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
                        "   0:	2729      	movs	r7, #41	; 0x29\n"
                        "   2:	427f      	negs	r7, r7"
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
