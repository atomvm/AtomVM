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

-module(jit_xtensa_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_xtensa).

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_state_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Stream = ?BACKEND:stream(State),
    ?assertEqual(0, byte_size(Stream)),
    ?assertEqual(0, ?BACKEND:offset(State)).

jump_table_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	ff          	.byte	0xff\n"
        "   1:	ff          	.byte	0xff\n"
        "   2:	ff          	.byte	0xff\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	00c136        	entry	a1, 96\n"
        "   7:	fffe51        	l32r	a5, 0x0 (0xffffffff)\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	ff          	.byte	0xff\n"
        "  15:	ff          	.byte	0xff\n"
        "  16:	ff          	.byte	0xff\n"
        "  17:	ff          	.byte	0xff\n"
        "  18:	00c136        	entry	a1, 96\n"
        "  1b:	fffe51        	l32r	a5, 0x14 (0xffffffff)\n"
        "  1e:	032382        	l32i	a8, a3, 12\n"
        "  21:	808850        	add	a8, a8, a5\n"
        "  24:	0008a0        	jx	a8\n"
        "  27:	ff          	.byte	0xff\n"
        "  28:	ff          	.byte	0xff\n"
        "  29:	ff          	.byte	0xff\n"
        "  2a:	ff          	.byte	0xff\n"
        "  2b:	ff          	.byte	0xff\n"
        "  2c:	00c136        	entry	a1, 96\n"
        "  2f:	fffe51        	l32r	a5, 0x28 (0xffffffff)\n"
        "  32:	032382        	l32i	a8, a3, 12\n"
        "  35:	808850        	add	a8, a8, a5\n"
        "  38:	0008a0        	jx	a8\n"
        "  3b:	ff          	.byte	0xff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

add_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    State4 = ?BACKEND:add_label(State3, 0),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	003c      	movi.n	a0, 48\n"
        "   2:	360000        	lsi	f0, a0, 216\n"
        "   5:	5100c1        	l32r	a12, 0xfffd4408\n"
        "   8:	fe          	.byte	0xfe\n"
        "   9:	ff          	.byte	0xff\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	003c      	movi.n	a0, 48\n"
        "  16:	360000        	lsi	f0, a0, 216\n"
        "  19:	5100c1        	l32r	a12, 0xfffd441c\n"
        "  1c:	fe          	.byte	0xfe\n"
        "  1d:	ff          	.byte	0xff\n"
        "  1e:	032382        	l32i	a8, a3, 12\n"
        "  21:	808850        	add	a8, a8, a5\n"
        "  24:	0008a0        	jx	a8\n"
        "  27:	ff          	.byte	0xff\n"
        "  28:	003c      	movi.n	a0, 48\n"
        "  2a:	360000        	lsi	f0, a0, 216\n"
        "  2d:	5100c1        	l32r	a12, 0xfffd4430\n"
        "  30:	fe          	.byte	0xfe\n"
        "  31:	ff          	.byte	0xff\n"
        "  32:	032382        	l32i	a8, a3, 12\n"
        "  35:	808850        	add	a8, a8, a5\n"
        "  38:	0008a0        	jx	a8\n"
        "  3b:	ff          	.byte	0xff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

add_label_with_offset_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#20),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	ff          	.byte	0xff\n"
        "   1:	ff          	.byte	0xff\n"
        "   2:	ff          	.byte	0xff\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	00c136        	entry	a1, 96\n"
        "   7:	fffe51        	l32r	a5, 0x0 (0xffffffff)\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	003c      	movi.n	a0, 48\n"
        "  16:	360000        	lsi	f0, a0, 216\n"
        "  19:	5100c1        	l32r	a12, 0xfffd441c\n"
        "  1c:	fe          	.byte	0xfe\n"
        "  1d:	ff          	.byte	0xff\n"
        "  1e:	032382        	l32i	a8, a3, 12\n"
        "  21:	808850        	add	a8, a8, a5\n"
        "  24:	0008a0        	jx	a8\n"
        "  27:	ff          	.byte	0xff\n"
        "  28:	000020        	lsi	f2, a0, 0\n"
        "  2b:	c13600        	mul16u	a3, a6, a0\n"
        "  2e:	fe5100        	f64iter	a5, a1, a0, 3, 1\n"
        "  31:	ff          	.byte	0xff\n"
        "  32:	032382        	l32i	a8, a3, 12\n"
        "  35:	808850        	add	a8, a8, a5\n"
        "  38:	0008a0        	jx	a8\n"
        "  3b:	ff          	.byte	0xff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_native_register_xreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    ?assert(is_atom(Reg)),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_native_register_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, 42),
    Stream = ?BACKEND:stream(State1),
    ?assert(is_atom(Reg)),
    Dump = <<
        "   0:	2aa0f2        	movi	a15, 42"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_native_register_yreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
    ?assert(is_atom(Reg)),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    Dump = <<
        "   0:	0522e2        	l32i	a14, a2, 20\n"
        "   3:	032ef2        	l32i	a15, a14, 12"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_vm_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	2aa0f2        	movi	a15, 42\n"
        "   3:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_vm_register_yreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {y_reg, 2}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	2aa0e2        	movi	a14, 42\n"
        "   3:	0522f2        	l32i	a15, a2, 20\n"
        "   6:	026fe2        	s32i	a14, a15, 8"
    >>,
    ?assertStream(xtensa, Dump, Stream).

and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, 16#3F),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	3fa0e2        	movi	a14, 63\n"
        "   6:	10ffe0        	and	a15, a15, a14"
    >>,
    ?assertStream(xtensa, Dump, Stream).

or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:or_(State1, {free, RegA}, 16#0F),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0fa0e2        	movi	a14, 15\n"
        "   6:	20ffe0        	or	a15, a15, a14"
    >>,
    ?assertStream(xtensa, Dump, Stream).

add_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:add(State1, {free, RegA}, 4),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	04cff2        	addi	a15, a15, 4"
    >>,
    ?assertStream(xtensa, Dump, Stream).

sub_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:sub(State1, {free, RegA}, 4),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	fccff2        	addi	a15, a15, -4"
    >>,
    ?assertStream(xtensa, Dump, Stream).

debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

call_primitive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    ?assert(is_atom(ResultReg)),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	02ad      	mov.n	a10, a2\n"
        "   5:	03bd      	mov.n	a11, a3\n"
        "   7:	000fe0        	callx8	a15\n"
        "   a:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	02ad      	mov.n	a10, a2\n"
        "   5:	03bd      	mov.n	a11, a3\n"
        "   7:	000fe0        	callx8	a15\n"
        "   a:	0a2d      	mov.n	a2, a10\n"
        "   c:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_right(State1, {free, RegA}, 2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	41f2f0        	srli	a15, a15, 2"
    >>,
    ?assertStream(xtensa, Dump, Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_left(State1, {free, RegA}, 2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	11ffe0        	slli	a15, a15, 2"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Test that multiple move_to_native_register calls allocate different registers
register_allocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, Reg3} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    ?assertNotEqual(Reg1, Reg2),
    ?assertNotEqual(Reg2, Reg3),
    ?assertNotEqual(Reg1, Reg3),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32"
    >>,
    ?assertStream(xtensa, Dump, Stream).

update_branches_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_primitive_last(State2, 0, [ctx, jit_state]),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 1, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 0, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump = <<
        "   0:	005a      	add.n	a0, a0, a5\n"
        "   2:	360000        	lsi	f0, a0, 216\n"
        "   5:	5100c1        	l32r	a12, 0xfffd4408\n"
        "   8:	fe          	.byte	0xfe\n"
        "   9:	ff          	.byte	0xff\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	003c      	movi.n	a0, 48\n"
        "  16:	360000        	lsi	f0, a0, 216\n"
        "  19:	5100c1        	l32r	a12, 0xfffd441c\n"
        "  1c:	fe          	.byte	0xfe\n"
        "  1d:	ff          	.byte	0xff\n"
        "  1e:	032382        	l32i	a8, a3, 12\n"
        "  21:	808850        	add	a8, a8, a5\n"
        "  24:	0008a0        	jx	a8\n"
        "  27:	ff          	.byte	0xff\n"
        "  28:	004b      	addi.n	a0, a0, 4\n"
        "  2a:	360000        	lsi	f0, a0, 216\n"
        "  2d:	5100c1        	l32r	a12, 0xfffd4430\n"
        "  30:	fe          	.byte	0xfe\n"
        "  31:	ff          	.byte	0xff\n"
        "  32:	032382        	l32i	a8, a3, 12\n"
        "  35:	808850        	add	a8, a8, a5\n"
        "  38:	0008a0        	jx	a8\n"
        "  3b:	ff          	.byte	0xff\n"
        "  3c:	0024f2        	l32i	a15, a4, 0\n"
        "  3f:	02ad      	mov.n	a10, a2\n"
        "  41:	03bd      	mov.n	a11, a3\n"
        "  43:	000fe0        	callx8	a15\n"
        "  46:	0a2d      	mov.n	a2, a10\n"
        "  48:	000090        	retw\n"
        "  4b:	0124f2        	l32i	a15, a4, 4\n"
        "  4e:	02ad      	mov.n	a10, a2\n"
        "  50:	03bd      	mov.n	a11, a3\n"
        "  52:	000fe0        	callx8	a15\n"
        "  55:	0a2d      	mov.n	a2, a10\n"
        "  57:	000090        	retw\n"
        "  5a:	0024f2        	l32i	a15, a4, 0\n"
        "  5d:	02ad      	mov.n	a10, a2\n"
        "  5f:	03bd      	mov.n	a11, a3\n"
        "  61:	000fe0        	callx8	a15\n"
        "  64:	0a2d      	mov.n	a2, a10\n"
        "  66:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0522e2        	l32i	a14, a2, 20\n"
        "   3:	002ef2        	l32i	a15, a14, 0\n"
        "   6:	1762f2        	s32i	a15, a2, 92"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% mov_immediate tests (via move_to_native_register with integer values)
%%-----------------------------------------------------------------------------

mov_immediate_small_pos_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 42, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	2aa032        	movi	a3, 42"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_small_neg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, -1, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	ffaf32        	movi	a3, -1"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 0, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00a032        	movi	a3, 0"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_max_movi_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 2047, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	ffa732        	movi	a3, 0x7ff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_min_movi_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, -2048, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00a832        	movi	a3, 0xfffff800"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_medium_pos_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 4096, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00a032        	movi	a3, 0\n"
        "   3:	10d332        	addmi	a3, a3, 0x1000"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_medium_neg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, -4096, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00a032        	movi	a3, 0\n"
        "   3:	f0d332        	addmi	a3, a3, 0xfffff000"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 16#12345678, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	000106        	j	0x8\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	5678      	    l32i.n	a7, a6, 20\n"
        "   6:	311234        	lsi	f3, a2, 196\n"
        "   9:	ff          	.byte	0xff\n"
        "   a:	ff          	.byte	0xff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_large_neg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, -100000, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	e7a932        	movi	a3, 0xfffff9e7\n"
        "   3:	1133a0        	slli	a3, a3, 6\n"
        "   6:	a0c332        	addi	a3, a3, -96"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_shifted_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 16#10000, a3),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	01a032        	movi	a3, 1\n"
        "   3:	113300        	slli	a3, a3, 16"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% mov_immediate/2 (fixed-size binary, no L32R) tests
%% Regression coverage for values with 0xFF in intermediate bytes, which used
%% to be misencoded by mov_immediate_large. Exercises the
%% movi + slli-8 + addi byte-chain path that set_cp / rewrite_cp_offset rely on.
%%-----------------------------------------------------------------------------

mov_immediate_bin_small_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 42),
    Dump = <<
        "   0:	2aa032        	movi	a3, 42"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_movi_addmi_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 4096),
    Dump = <<
        "   0:	00a032        	movi	a3, 0\n"
        "   3:	10d332        	addmi	a3, a3, 0x1000"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_ff_middle_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 65444),
    Dump = <<
        "   0:	01a032        	movi	a3, 1\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	00c332        	addi	a3, a3, 0\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	a4c332        	addi	a3, a3, -92"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_00ffffff_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#00FFFFFF),
    Dump = <<
        "   0:	01a032        	movi	a3, 1\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	00c332        	addi	a3, a3, 0\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	00c332        	addi	a3, a3, 0\n"
        "   f:	113380        	slli	a3, a3, 8\n"
        "  12:	ffc332        	addi	a3, a3, -1"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_ffffffff_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#FFFFFFFF),
    Dump = <<
        "   0:	00a032        	movi	a3, 0\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	ffc332        	addi	a3, a3, -1"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_00ff00ff_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#00FF00FF),
    Dump = <<
        "   0:	01a032        	movi	a3, 1\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	ffc332        	addi	a3, a3, -1\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	01c332        	addi	a3, a3, 1\n"
        "   f:	113380        	slli	a3, a3, 8\n"
        "  12:	ffc332        	addi	a3, a3, -1"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_deadbeef_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#DEADBEEF),
    Dump = <<
        "   0:	dfaf32        	movi	a3, -33\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	aec332        	addi	a3, a3, -82\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	bfc332        	addi	a3, a3, -65\n"
        "   f:	113380        	slli	a3, a3, 8\n"
        "  12:	efc332        	addi	a3, a3, -17"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_ffee1234_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#FFEE1234),
    Dump = <<
        "   0:	eeaf32        	movi	a3, -18\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	12c332        	addi	a3, a3, 18\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	34c332        	addi	a3, a3, 52"
    >>,
    ?assertStream(xtensa, Dump, Stream).

mov_immediate_bin_12345678_test() ->
    Stream = jit_xtensa:mov_immediate(a3, 16#12345678),
    Dump = <<
        "   0:	12a032        	movi	a3, 18\n"
        "   3:	113380        	slli	a3, a3, 8\n"
        "   6:	34c332        	addi	a3, a3, 52\n"
        "   9:	113380        	slli	a3, a3, 8\n"
        "   c:	56c332        	addi	a3, a3, 86\n"
        "   f:	113380        	slli	a3, a3, 8\n"
        "  12:	78c332        	addi	a3, a3, 120"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% flush/1, used_regs/1, available_regs/1, free_native_registers/2,
%% assert_all_native_free/1
%%-----------------------------------------------------------------------------

flush_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    State2 = ?BACKEND:flush(State1),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

used_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    UsedRegs = ?BACKEND:used_regs(State2),
    ?assertEqual(lists:sort([Reg1, Reg2]), lists:sort(UsedRegs)).

available_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Available0 = ?BACKEND:available_regs(State0),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    Available1 = ?BACKEND:available_regs(State1),
    ?assert(length(Available0) > length(Available1)),
    ?assertNot(lists:member(Reg1, Available1)).

free_native_registers_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg1, Reg2]),
    ?BACKEND:assert_all_native_free(State3).

assert_all_native_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ok = ?BACKEND:assert_all_native_free(State0).

%%-----------------------------------------------------------------------------
%% return_if_not_equal_to_ctx/2
%%-----------------------------------------------------------------------------

return_if_not_equal_to_ctx_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, Reg}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	041f27        	beq	a15, a2, 0xb\n"
        "   6:	0f2d      	mov.n	a2, a15\n"
        "   8:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% jump_to_offset/2
%%-----------------------------------------------------------------------------

jump_to_offset_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 0),
    State2 = ?BACKEND:jump_to_offset(State1, 0),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	ff          	.byte	0xff\n"
        "   1:	ff          	.byte	0xff\n"
        "   2:	ff          	.byte	0xff\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	00c136        	entry	a1, 96\n"
        "   7:	fffe51        	l32r	a5, 0x0 (0xffffffff)\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	fffa06        	j	0x0"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% jump_to_label/2 (forward reference resolved via add_label/3 + update_branches)
%%-----------------------------------------------------------------------------

jump_to_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 0),
    Ref = make_ref(),
    State2 = ?BACKEND:jump_to_label(State1, Ref),
    TargetOffset = ?BACKEND:offset(State2),
    State3 = ?BACKEND:add_label(State2, Ref, TargetOffset),
    State4 = ?BACKEND:update_branches(State3),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	ff          	.byte	0xff\n"
        "   1:	ff          	.byte	0xff\n"
        "   2:	ff          	.byte	0xff\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	00c136        	entry	a1, 96\n"
        "   7:	fffe51        	l32r	a5, 0x0 (0xffffffff)\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	000506        	j	0x2c\n"
        "  17:	0020f0        	nop\n"
        "  1a:	0020f0        	nop\n"
        "  1d:	0020f0        	nop\n"
        "  20:	0020f0        	nop\n"
        "  23:	0020f0        	nop\n"
        "  26:	0020f0        	nop\n"
        "  29:	0020f0        	nop"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% cond_jump_to_label/3
%%-----------------------------------------------------------------------------

cond_jump_to_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 0),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    Ref = make_ref(),
    State3 = ?BACKEND:cond_jump_to_label(State2, {{free, Reg}, '<', 0}, Ref),
    TargetOffset = ?BACKEND:offset(State3),
    State4 = ?BACKEND:add_label(State3, Ref, TargetOffset),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	ff          	.byte	0xff\n"
        "   1:	ff          	.byte	0xff\n"
        "   2:	ff          	.byte	0xff\n"
        "   3:	ff          	.byte	0xff\n"
        "   4:	00c136        	entry	a1, 96\n"
        "   7:	fffe51        	l32r	a5, 0x0 (0xffffffff)\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	0622f2        	l32i	a15, a2, 24\n"
        "  17:	002f96        	bltz	a15, 0x1d\n"
        "  1a:	0005c6        	j	0x35\n"
        "  1d:	000506        	j	0x35\n"
        "  20:	0020f0        	nop\n"
        "  23:	0020f0        	nop\n"
        "  26:	0020f0        	nop\n"
        "  29:	0020f0        	nop\n"
        "  2c:	0020f0        	nop\n"
        "  2f:	0020f0        	nop\n"
        "  32:	0020f0        	nop"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% jump_to_continuation/2
%%-----------------------------------------------------------------------------

jump_to_continuation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:jump_to_continuation(State1, {free, Reg}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0323e2        	l32i	a14, a3, 12\n"
        "   6:	80eef0        	add	a14, a14, a15\n"
        "   9:	03cee2        	addi	a14, a14, 3\n"
        "   c:	000ea0        	jx	a14"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block/3
%%-----------------------------------------------------------------------------

if_block_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1,
        {Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_else_block/4
%%-----------------------------------------------------------------------------

if_else_block_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_else_block(
        State1,
        {Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end,
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000146        	j	0xf\n"
        "   9:	0041f0        	break	1, 15\n"
        "   c:	000086        	j	0x12\n"
        "   f:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% get_regs_tracking/1
%%-----------------------------------------------------------------------------

get_regs_tracking_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Regs = ?BACKEND:get_regs_tracking(State0),
    ?assertNotEqual(undefined, Regs).

%%-----------------------------------------------------------------------------
%% if_block/3 first clause: {'and', CondList}
%%-----------------------------------------------------------------------------

if_block_and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {'and', [{Reg1, '==', 0}, {Reg2, '==', 0}]}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	002f16        	beqz	a15, 0xc\n"
        "   9:	000206        	j	0x15\n"
        "   c:	002e16        	beqz	a14, 0x12\n"
        "   f:	000086        	j	0x15\n"
        "  12:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond '<' variants
%%-----------------------------------------------------------------------------

%% {Reg, '<', 0} with bare register (not {free, Reg})
if_block_cond_lt_0_bare_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '<', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f96        	bltz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '<', IS_B4CONST} -> blti
if_block_cond_lt_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '<', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021fa6        	blti	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '<', Val} where 0 < Val <= 255 and not B4CONST -> movi+blt
if_block_cond_lt_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '<', 100}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	64a0e2        	movi	a14, 100\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '<', Val} where Val > 255 -> mov_immediate+blt
if_block_cond_lt_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '<', 1000}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Val, '<', Reg} where 0 <= Val <= 255 -> movi+blt(Temp, Reg)
if_block_cond_uint8_lt_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {42, '<', Reg}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	022ef7        	blt	a14, a15, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Val, '<', Reg} where Val > 255 -> mov_immediate+blt(Temp, Reg)
if_block_cond_large_lt_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {1000, '<', Reg}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	022ef7        	blt	a14, a15, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg1, '<', Reg2} both native registers -> blt(Reg1, Reg2)
if_block_cond_reg_lt_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(State2, {Reg1, '<', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond '==' variants
%%-----------------------------------------------------------------------------

%% {{free, Reg}, '==', 0} -> covers {free, Reg0} -> Reg0 path in beqz clause
if_block_cond_free_eq_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '==', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg1, '==', Reg2} bare registers -> beq(Reg1, Reg2)
if_block_cond_eq_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(State2, {Reg1, '==', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg1}, '==', Reg2} -> covers {free, Reg0} -> Reg0 path in beq clause
if_block_cond_free_eq_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {{free, Reg1}, '==', Reg2}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {'(int)', Reg, '==', 0} -> delegates to beqz
if_block_cond_int_eq_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(int)', Reg, '==', 0}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {'(int)', Reg, '==', Val} -> delegates to '==' clause
if_block_cond_int_eq_val_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(int)', Reg, '==', 42}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '==', IS_B4CONST} -> beqi
if_block_cond_eq_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '==', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021f26        	beqi	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '==', Val} where 0 < Val <= 255, not B4CONST -> movi+beq
if_block_cond_eq_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '==', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg1}, '==', {free, Reg2}} -> beq(RegA, RegB), frees both
if_block_cond_free_eq_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2,
        {{free, Reg1}, '==', {free, Reg2}},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '==', Val} where Val > 255 -> mov_immediate+beq
if_block_cond_eq_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '==', 1000}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond '!=' variants
%%-----------------------------------------------------------------------------

%% {Reg, '!=', 0} bare register -> bnez
if_block_cond_ne_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f56        	bnez	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg}, '!=', 0} -> covers {free, Reg0} -> Reg0 path in bnez clause
if_block_cond_free_ne_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f56        	bnez	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '!=', IS_B4CONST} -> bnei
if_block_cond_ne_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021f66        	bnei	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '!=', Val} where 0 < Val <= 255, not B4CONST -> movi+bne
if_block_cond_ne_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg1, '!=', Reg2} both native registers -> bne(Reg1, Reg2)
if_block_cond_ne_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(State2, {Reg1, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {'(int)', Reg, '!=', Val} -> delegates to '!=' clause
if_block_cond_int_ne_val_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(int)', Reg, '!=', 42}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '!=', Val} where Val > 255 -> mov_immediate+bne
if_block_cond_ne_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {Reg, '!=', 1000}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond '(bool)' variants
%%-----------------------------------------------------------------------------

%% {'(bool)', Reg, '==', false} -> beqz
if_block_cond_bool_eq_false_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(bool)', Reg, '==', false}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {'(bool)', Reg, '!=', false} -> bnez
if_block_cond_bool_ne_false_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(bool)', Reg, '!=', false}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f56        	bnez	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond '&' variants
%%-----------------------------------------------------------------------------

%% {Reg, '&', Mask, '!=', 0} -> mov_immediate(Mask)+and+bnez
if_block_cond_and_ne_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {Reg, '&', 15, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0fa0e2        	movi	a14, 15\n"
        "   6:	10efe0        	and	a14, a15, a14\n"
        "   9:	002e56        	bnez	a14, 0xf\n"
        "   c:	000086        	j	0x12\n"
        "   f:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '&', 16#F, '!=', 16#F} bare IS_GPR -> movi(-1)+xor+slli+bnez
if_block_cond_and_nibble_ne_f_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {Reg, '&', 16#F, '!=', 16#F}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	ffafe2        	movi	a14, -1\n"
        "   6:	30eef0        	xor	a14, a14, a15\n"
        "   9:	01ee40        	slli	a14, a14, 28\n"
        "   c:	002e56        	bnez	a14, 0x12\n"
        "   f:	000086        	j	0x15\n"
        "  12:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg}, '&', 16#F, '!=', 16#F} -> neg+addi+slli+bnez
if_block_cond_free_and_nibble_ne_f_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {{free, Reg}, '&', 16#F, '!=', 16#F}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	60f0f0        	neg	a15, a15\n"
        "   6:	ffcff2        	addi	a15, a15, -1\n"
        "   9:	01ff40        	slli	a15, a15, 28\n"
        "   c:	002f56        	bnez	a15, 0x12\n"
        "   f:	000086        	j	0x15\n"
        "  12:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '&', Mask, '!=', RegB} general clause, IS_GPR(Val) sub-case
if_block_cond_and_ne_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {Reg1, '&', 16#FF, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0fdd      	mov.n	a13, a15\n"
        "   8:	ffa0c2        	movi	a12, 255\n"
        "   b:	10ddc0        	and	a13, a13, a12\n"
        "   e:	029de7        	bne	a13, a14, 0x14\n"
        "  11:	000086        	j	0x17\n"
        "  14:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {Reg, '&', Mask, '!=', Imm} general clause, integer Val sub-case
if_block_cond_and_ne_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {Reg, '&', 16#FF, '!=', 42}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0fed      	mov.n	a14, a15\n"
        "   5:	ffa0d2        	movi	a13, 255\n"
        "   8:	10eed0        	and	a14, a14, a13\n"
        "   b:	2aa0d2        	movi	a13, 42\n"
        "   e:	029ed7        	bne	a14, a13, 0x14\n"
        "  11:	000086        	j	0x17\n"
        "  14:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg}, '&', Mask, '!=', RegB} general clause with free reg, IS_GPR(Val) sub-case
if_block_cond_free_and_ne_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {{free, Reg1}, '&', 16#FF, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	ffa0d2        	movi	a13, 255\n"
        "   9:	10ffd0        	and	a15, a15, a13\n"
        "   c:	029fe7        	bne	a15, a14, 0x12\n"
        "   f:	000086        	j	0x15\n"
        "  12:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% {{free, Reg}, '&', Mask, '!=', Imm} general clause with free reg, integer Val sub-case
if_block_cond_free_and_ne_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {{free, Reg}, '&', 16#FF, '!=', 42}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	ffa0e2        	movi	a14, 255\n"
        "   6:	10ffe0        	and	a15, a15, a14\n"
        "   9:	2aa0e2        	movi	a14, 42\n"
        "   c:	029fe7        	bne	a15, a14, 0x12\n"
        "   f:	000086        	j	0x15\n"
        "  12:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% patch_branch / branch_to_offset_code / branch_to_label_code
%%-----------------------------------------------------------------------------

%% patch_branch near {far_branch}: forward reference resolved to near J + NOPs
patch_branch_near_forward_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Ref = make_ref(),
    State1 = ?BACKEND:jump_to_label(State0, Ref),
    TargetOffset = ?BACKEND:offset(State1),
    State2 = ?BACKEND:add_label(State1, Ref, TargetOffset),
    State3 = ?BACKEND:update_branches(State2),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	000506        	j	0x18\n"
        "   3:	0020f0        	nop\n"
        "   6:	0020f0        	nop\n"
        "   9:	0020f0        	nop\n"
        "   c:	0020f0        	nop\n"
        "   f:	0020f0        	nop\n"
        "  12:	0020f0        	nop\n"
        "  15:	0020f0        	nop"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% patch_branch far {far_branch}: forward reference resolved to far indirect jump
patch_branch_far_forward_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Ref = make_ref(),
    State1 = ?BACKEND:jump_to_label(State0, Ref),
    %% Define label at a far offset without emitting that much data
    State2 = ?BACKEND:add_label(State1, Ref, 200000),
    State3 = ?BACKEND:update_branches(State2),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	032382        	l32i	a8, a3, 12\n"
        "   3:	03a0f2        	movi	a15, 3\n"
        "   6:	11ff80        	slli	a15, a15, 8\n"
        "   9:	0dcff2        	addi	a15, a15, 13\n"
        "   c:	11ff80        	slli	a15, a15, 8\n"
        "   f:	40cff2        	addi	a15, a15, 64\n"
        "  12:	8088f0        	add	a8, a8, a15\n"
        "  15:	0008a0        	jx	a8"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% branch_to_label_code first clause: jump to already-known label (backward jump)
branch_to_backward_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Ref = make_ref(),
    %% Define label at current offset (0) before emitting the jump
    State1 = ?BACKEND:add_label(State0, Ref, 0),
    State2 = ?BACKEND:jump_to_label(State1, Ref),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	ffff06        	j	0x0"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% branch_to_offset_code far branch: jump_to_offset with target > 131071 bytes away
jump_to_offset_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_to_offset(State0, 200000),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	032382        	l32i	a8, a3, 12\n"
        "   3:	03a0f2        	movi	a15, 3\n"
        "   6:	11ff80        	slli	a15, a15, 8\n"
        "   9:	0dcff2        	addi	a15, a15, 13\n"
        "   c:	11ff80        	slli	a15, a15, 8\n"
        "   f:	40cff2        	addi	a15, a15, 64\n"
        "  12:	8088f0        	add	a8, a8, a15\n"
        "  15:	0008a0        	jx	a8"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% return_if_not_equal_to_ctx with Reg = a2 (I2 = <<>>, no MOV needed)
%%-----------------------------------------------------------------------------

return_if_not_equal_to_ctx_a2_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:return_if_not_equal_to_ctx(State0, {free, a2}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	021227        	beq	a2, a2, 0x6\n"
        "   3:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive second clause: when available_regs = 0
%%-----------------------------------------------------------------------------

call_primitive_no_avail_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, _} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    {State7, _} = ?BACKEND:move_to_native_register(State6, {x_reg, 6}),
    {State8, _} = ?BACKEND:move_to_native_register(State7, {x_reg, 7}),
    {State9, _} = ?BACKEND:move_to_native_register(State8, {x_reg, 8}),
    {State10, _} = ?BACKEND:move_to_native_register(State9, {x_reg, 9}),
    {State11, _} = ?BACKEND:call_primitive(State10, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State11),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	0922c2        	l32i	a12, a2, 36\n"
        "   c:	0a22b2        	l32i	a11, a2, 40\n"
        "   f:	0b22a2        	l32i	a10, a2, 44\n"
        "  12:	0c2292        	l32i	a9, a2, 48\n"
        "  15:	0d2272        	l32i	a7, a2, 52\n"
        "  18:	0e2262        	l32i	a6, a2, 56\n"
        "  1b:	0f2252        	l32i	a5, a2, 60\n"
        "  1e:	0c61f2        	s32i	a15, a1, 48\n"
        "  21:	0d61e2        	s32i	a14, a1, 52\n"
        "  24:	0e61d2        	s32i	a13, a1, 56\n"
        "  27:	0f61c2        	s32i	a12, a1, 60\n"
        "  2a:	1061b2        	s32i	a11, a1, 64\n"
        "  2d:	1161a2        	s32i	a10, a1, 68\n"
        "  30:	126192        	s32i	a9, a1, 72\n"
        "  33:	0024f2        	l32i	a15, a4, 0\n"
        "  36:	02ad      	mov.n	a10, a2\n"
        "  38:	03bd      	mov.n	a11, a3\n"
        "  3a:	000fe0        	callx8	a15\n"
        "  3d:	0a8d      	mov.n	a8, a10\n"
        "  3f:	0c21f2        	l32i	a15, a1, 48\n"
        "  42:	0d21e2        	l32i	a14, a1, 52\n"
        "  45:	0e21d2        	l32i	a13, a1, 56\n"
        "  48:	0f21c2        	l32i	a12, a1, 60\n"
        "  4b:	1021b2        	l32i	a11, a1, 64\n"
        "  4e:	1121a2        	l32i	a10, a1, 68\n"
        "  51:	122192        	l32i	a9, a1, 72"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive_last with offset arg
%%-----------------------------------------------------------------------------

call_primitive_last_with_offset_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, offset]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	02ad      	mov.n	a10, a2\n"
        "   5:	03bd      	mov.n	a11, a3\n"
        "   7:	03a0c2        	movi	a12, 3\n"
        "   a:	000fe0        	callx8	a15\n"
        "   d:	0a2d      	mov.n	a2, a10\n"
        "   f:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% patch_branches_for_label clause 3: non-matching branch is skipped
%%-----------------------------------------------------------------------------

patch_branches_for_label_skip_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    %% Forward jump to label 2 (pending in branches)
    State2 = ?BACKEND:jump_to_label(State1, 2),
    %% Define label 1: patch_branches_for_label iterates [{2,...}], label 2 != 1 -> clause 3
    State3 = ?BACKEND:add_label(State2, 1),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:add_label(State4, 0),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	000054        	lsi	f5, a0, 0\n"
        "   3:	c13600        	mul16u	a3, a6, a0\n"
        "   6:	fe5100        	f64iter	a5, a1, a0, 3, 1\n"
        "   9:	ff          	.byte	0xff\n"
        "   a:	032382        	l32i	a8, a3, 12\n"
        "   d:	808850        	add	a8, a8, a5\n"
        "  10:	0008a0        	jx	a8\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	000054        	lsi	f5, a0, 0\n"
        "  17:	c13600        	mul16u	a3, a6, a0\n"
        "  1a:	fe5100        	f64iter	a5, a1, a0, 3, 1\n"
        "  1d:	ff          	.byte	0xff\n"
        "  1e:	032382        	l32i	a8, a3, 12\n"
        "  21:	808850        	add	a8, a8, a5\n"
        "  24:	0008a0        	jx	a8\n"
        "  27:	ff          	.byte	0xff\n"
        "  28:	000054        	lsi	f5, a0, 0\n"
        "  2b:	c13600        	mul16u	a3, a6, a0\n"
        "  2e:	fe5100        	f64iter	a5, a1, a0, 3, 1\n"
        "  31:	ff          	.byte	0xff\n"
        "  32:	032382        	l32i	a8, a3, 12\n"
        "  35:	808850        	add	a8, a8, a5\n"
        "  38:	0008a0        	jx	a8\n"
        "  3b:	ff          	.byte	0xff\n"
        "  3c:	000506        	j	0x54\n"
        "  3f:	0020f0        	nop\n"
        "  42:	0020f0        	nop\n"
        "  45:	0020f0        	nop\n"
        "  48:	0020f0        	nop\n"
        "  4b:	0020f0        	nop\n"
        "  4e:	0020f0        	nop\n"
        "  51:	0020f0        	nop"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond with {free, Reg} for '<' variants
%%-----------------------------------------------------------------------------

if_block_cond_free_lt_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '<', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021fa6        	blti	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_lt_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '<', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_lt_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '<', 1000}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_uint8_lt_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {42, '<', {free, Reg}}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	022ef7        	blt	a14, a15, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_large_lt_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {1000, '<', {free, Reg}}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	022ef7        	blt	a14, a15, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_reg_lt_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {{free, Reg1}, '<', Reg2}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	022fe7        	blt	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond with {free, Reg} for '!=' variants
%%-----------------------------------------------------------------------------

if_block_cond_free_ne_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '!=', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021f66        	bnei	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_ne_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '!=', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_ne_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2, {{free, Reg1}, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_ne_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {{free, Reg}, '!=', 1000}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	029fe7        	bne	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond with {free, Reg} for '==' variants
%%-----------------------------------------------------------------------------

if_block_cond_free_eq_b4const_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '==', 1}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	021f26        	beqi	a15, 1, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_eq_uint8_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {{free, Reg}, '==', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	2aa0e2        	movi	a14, 42\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_eq_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {{free, Reg}, '==', 1000}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	e8a3e2        	movi	a14, 0x3e8\n"
        "   6:	021fe7        	beq	a15, a14, 0xc\n"
        "   9:	000086        	j	0xf\n"
        "   c:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond with {free, Reg} for '(bool)' variants
%%-----------------------------------------------------------------------------

if_block_cond_free_bool_eq_false_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(bool)', {free, Reg}, '==', false}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f16        	beqz	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

if_block_cond_free_bool_ne_false_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {'(bool)', {free, Reg}, '!=', false}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	002f56        	bnez	a15, 0x9\n"
        "   6:	000086        	j	0xc\n"
        "   9:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% if_block_cond with {free, Reg} for '&' != 0 variant
%%-----------------------------------------------------------------------------

if_block_cond_free_and_ne_zero_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(
        State1, {{free, Reg}, '&', 15, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end
    ),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0fa0e2        	movi	a14, 15\n"
        "   6:	10efe0        	and	a14, a15, a14\n"
        "   9:	002e56        	bnez	a14, 0xf\n"
        "   c:	000086        	j	0x12\n"
        "   f:	0041f0        	break	1, 15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% shift_right: {free, Reg} with Shift > 15 (movi+ssr+srl in-place)
%%-----------------------------------------------------------------------------

shift_right_free_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_right(State1, {free, RegA}, 16),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	10a0e2        	movi	a14, 16\n"
        "   6:	400e00        	ssr	a14\n"
        "   9:	91f0f0        	srl	a15, a15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% shift_right: plain Reg with Shift =< 15 (srli to new result reg)
%%-----------------------------------------------------------------------------

shift_right_new_reg_small_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, ResultReg} = ?BACKEND:shift_right(State1, RegA, 3),
    ?assertNotEqual(RegA, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	41e3f0        	srli	a14, a15, 3"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% shift_right: plain Reg with Shift > 15 (movi+ssr+srl to new result reg)
%%-----------------------------------------------------------------------------

shift_right_new_reg_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, ResultReg} = ?BACKEND:shift_right(State1, RegA, 16),
    ?assertNotEqual(RegA, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	10a0e2        	movi	a14, 16\n"
        "   6:	400e00        	ssr	a14\n"
        "   9:	91e0f0        	srl	a14, a15"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% shift_right_arith: {free, Reg} (srai in-place)
%%-----------------------------------------------------------------------------

shift_right_arith_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_right_arith(State1, {free, RegA}, 3),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	21f3f0        	srai	a15, a15, 3"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% shift_right_arith: plain Reg (srai to new result reg)
%%-----------------------------------------------------------------------------

shift_right_arith_new_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, ResultReg} = ?BACKEND:shift_right_arith(State1, RegA, 3),
    ?assertNotEqual(RegA, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	21e3f0        	srai	a14, a15, 3"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% div_reg: quos instruction
%%-----------------------------------------------------------------------------

div_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, RegA} = ?BACKEND:div_reg(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	d2ffe0        	quos	a15, a15, a14"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% rem_reg: rems instruction
%%-----------------------------------------------------------------------------

rem_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, RegA} = ?BACKEND:rem_reg(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	f2ffe0        	rems	a15, a15, a14"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: native register -> {x_reg, extra}
%%-----------------------------------------------------------------------------

move_to_vm_register_native_to_xreg_extra_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {x_reg, extra}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	1662f2        	s32i	a15, a2, 88"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: native register -> {ptr, Reg}
%%-----------------------------------------------------------------------------

move_to_vm_register_native_to_ptr_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {ptr, a5}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0065f2        	s32i	a15, a5, 0"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: native register -> {y_reg, Y}
%%-----------------------------------------------------------------------------

move_to_vm_register_native_to_yreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {y_reg, 3}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0522e2        	l32i	a14, a2, 20\n"
        "   6:	036ef2        	s32i	a15, a14, 12"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: large integer (> 255) -> {x_reg, 0}
%%-----------------------------------------------------------------------------

move_to_vm_register_large_int_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 256, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	00a1f2        	movi	a15, 0x100\n"
        "   3:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: {x_reg, extra} source -> {x_reg, 0}
%%-----------------------------------------------------------------------------

move_to_vm_register_xreg_extra_src_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, extra}, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	1622f2        	l32i	a15, a2, 88\n"
        "   3:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: {x_reg, X} source -> {x_reg, 1}
%%-----------------------------------------------------------------------------

move_to_vm_register_xreg_src_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, 0}, {x_reg, 1}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0762f2        	s32i	a15, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: {ptr, Reg} source -> {x_reg, 0}
%%-----------------------------------------------------------------------------

move_to_vm_register_ptr_src_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {ptr, a5}, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0025f2        	l32i	a15, a5, 0\n"
        "   3:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register_emit: {y_reg, Y} source -> {x_reg, 1}
%%-----------------------------------------------------------------------------

move_to_vm_register_yreg_src_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {y_reg, 0}, {x_reg, 1}),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0522e2        	l32i	a14, a2, 20\n"
        "   3:	002ef2        	l32i	a15, a14, 0\n"
        "   6:	0762f2        	s32i	a15, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive with {avm_int64_t, Value}: int64 at a10+a11 (even pair, first)
%%-----------------------------------------------------------------------------

call_primitive_int64_at_a10_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:call_primitive(State0, 0, [{avm_int64_t, 16#123456789ABCDEF0}]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	000146        	j	0xc\n"
        "   6:	f0ff00        	subx8	a15, a15, a0\n"
        "   9:	de          	.byte	0xde\n"
        "   a:	9abc      	beqz.n	a10, 0x47\n"
        "   c:	ffffa1        	l32r	a10, 0x8 (0x9abcdef0)\n"
        "   f:	000146        	j	0x18\n"
        "  12:	78ff00        	lsi	f0, a15, 0x1e0\n"
        "  15:	123456        	bnez	a4, 0x13c\n"
        "  18:	ffffb1        	l32r	a11, 0x14 (0x12345678)\n"
        "  1b:	000fe0        	callx8	a15\n"
        "  1e:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive with {avm_int64_t, Value}: ctx at a10, int64 skips a11, uses a12+a13
%%-----------------------------------------------------------------------------

call_primitive_int64_skip_a11_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:call_primitive(State0, 0, [ctx, {avm_int64_t, 16#123456789ABCDEF0}]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	02ad      	mov.n	a10, a2\n"
        "   5:	0000c6        	j	0xc\n"
        "   8:	bcdef0        	lsi	f15, a14, 0x2f0\n"
        "   b:	c19a      	add.n	a12, a1, a9\n"
        "   d:	ff          	.byte	0xff\n"
        "   e:	ff          	.byte	0xff\n"
        "   f:	000146        	j	0x18\n"
        "  12:	78ff00        	lsi	f0, a15, 0x1e0\n"
        "  15:	123456        	bnez	a4, 0x13c\n"
        "  18:	ffffd1        	l32r	a13, 0x14 (0x12345678)\n"
        "  1b:	000fe0        	callx8	a15\n"
        "  1e:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive with {avm_int64_t, Value}: ctx+jit_state at a10+a11, int64 at a12+a13
%%-----------------------------------------------------------------------------

call_primitive_int64_at_a12_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:call_primitive(State0, 0, [
        ctx, jit_state, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	02ad      	mov.n	a10, a2\n"
        "   5:	03bd      	mov.n	a11, a3\n"
        "   7:	000146        	j	0x10\n"
        "   a:	f0ff00        	subx8	a15, a15, a0\n"
        "   d:	de          	.byte	0xde\n"
        "   e:	9abc      	beqz.n	a10, 0x4b\n"
        "  10:	ffffc1        	l32r	a12, 0xc (0x9abcdef0)\n"
        "  13:	000146        	j	0x1c\n"
        "  16:	78ff00        	lsi	f0, a15, 0x1e0\n"
        "  19:	123456        	bnez	a4, 0x140\n"
        "  1c:	ffffd1        	l32r	a13, 0x18 (0x12345678)\n"
        "  1f:	000fe0        	callx8	a15\n"
        "  22:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive with {avm_int64_t, Value}: ctx+jit_state+offset, int64 skips a13, uses a14+a15
%%-----------------------------------------------------------------------------

call_primitive_int64_skip_a13_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:call_primitive(State0, 0, [
        ctx, jit_state, offset, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	0fdd      	mov.n	a13, a15\n"
        "   5:	02ad      	mov.n	a10, a2\n"
        "   7:	03bd      	mov.n	a11, a3\n"
        "   9:	03a0c2        	movi	a12, 3\n"
        "   c:	000106        	j	0x14\n"
        "   f:	ff          	.byte	0xff\n"
        "  10:	bcdef0        	lsi	f15, a14, 0x2f0\n"
        "  13:	e19a      	add.n	a14, a1, a9\n"
        "  15:	ff          	.byte	0xff\n"
        "  16:	ff          	.byte	0xff\n"
        "  17:	000146        	j	0x20\n"
        "  1a:	78ff00        	lsi	f0, a15, 0x1e0\n"
        "  1d:	123456        	bnez	a4, 0x144\n"
        "  20:	fffff1        	l32r	a15, 0x1c (0x12345678)\n"
        "  23:	000de0        	callx8	a13\n"
        "  26:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% call_primitive with {avm_int64_t, Value}: ctx+jit_state+offset+{x_reg,0}, int64 at a14+a15
%%-----------------------------------------------------------------------------

call_primitive_int64_at_a14_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _} = ?BACKEND:call_primitive(State0, 0, [
        ctx, jit_state, offset, {x_reg, 0}, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	0024f2        	l32i	a15, a4, 0\n"
        "   3:	0f9d      	mov.n	a9, a15\n"
        "   5:	02ad      	mov.n	a10, a2\n"
        "   7:	03bd      	mov.n	a11, a3\n"
        "   9:	03a0c2        	movi	a12, 3\n"
        "   c:	0622d2        	l32i	a13, a2, 24\n"
        "   f:	000146        	j	0x18\n"
        "  12:	f0ff00        	subx8	a15, a15, a0\n"
        "  15:	de          	.byte	0xde\n"
        "  16:	9abc      	beqz.n	a10, 0x53\n"
        "  18:	ffffe1        	l32r	a14, 0x14 (0x9abcdef0)\n"
        "  1b:	000146        	j	0x24\n"
        "  1e:	78ff00        	lsi	f0, a15, 0x1e0\n"
        "  21:	123456        	bnez	a4, 0x148\n"
        "  24:	fffff1        	l32r	a15, 0x20 (0x12345678)\n"
        "  27:	0009e0        	callx8	a9\n"
        "  2a:	0a7d      	mov.n	a7, a10"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%%-----------------------------------------------------------------------------
%% Register cache tests
%%-----------------------------------------------------------------------------

%% Verify move_array_element for {x_reg, X} destination updates the cache for
%% the Temp register so a subsequent move_to_native_register({x_reg, X}) returns
%% Temp directly without emitting a second load.
move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    S3 = ?BACKEND:move_array_element(State2, a14, 0, {x_reg, 5}),
    {S4, _} = ?BACKEND:move_to_native_register(S3, {x_reg, 5}),
    Stream = ?BACKEND:stream(S4),
    Dump = <<
        "   0:	0b22f2        	l32i	a15, a2, 44\n"
        "   3:	0622e2        	l32i	a14, a2, 24\n"
        "   6:	002ed2        	l32i	a13, a14, 0\n"
        "   9:	0b62d2        	s32i	a13, a2, 44"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify move_to_native_register/3 for {x_reg, N} sets the cache so that a
%% subsequent move_to_native_register/2 for the same value returns the register
%% from cache without emitting a second load.
fixed_dst_x_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, a13),
    {State2, a13} = ?BACKEND:move_to_native_register(State1, {x_reg, 2}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<"   0:	0822d2        	l32i	a13, a2, 32">>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify move_to_native_register/3 for {y_reg, Y} sets the cache so that a
%% subsequent move_to_native_register/2 for the same value returns the register
%% from cache without emitting a second load.
fixed_dst_y_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, a13),
    {State2, a13} = ?BACKEND:move_to_native_register(State1, {y_reg, 2}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0522f2        	l32i	a15, a2, 20\n"
        "   3:	022fd2        	l32i	a13, a15, 8"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify that an if_block whose body is a terminal (call_primitive_last,
%% which uses jit_regs:unreachable) preserves the pre-block cache at the merge
%% point, so move_to_native_register after the block hits the cache.
call_primitive_last_if_block_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(
        State2,
        {a15, '==', 0},
        fun(S) -> ?BACKEND:call_primitive_last(S, 0, [ctx, jit_state]) end
    ),
    {State4, a14} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	01a0f2        	movi	a15, 1\n"
        "   3:	0622e2        	l32i	a14, a2, 24\n"
        "   6:	002f16        	beqz	a15, 0xc\n"
        "   9:	000386        	j	0x1b\n"
        "   c:	0024f2        	l32i	a15, a4, 0\n"
        "   f:	02ad      	mov.n	a10, a2\n"
        "  11:	03bd      	mov.n	a11, a3\n"
        "  13:	000fe0        	callx8	a15\n"
        "  16:	0a2d      	mov.n	a2, a10\n"
        "  18:	000090        	retw"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify that an if_block whose body is a terminal (jump_to_label,
%% which uses jit_regs:unreachable) preserves the pre-block cache at the merge
%% point, so move_to_native_register after the block hits the cache.
jump_to_label_if_block_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(
        State2,
        {a15, '==', 0},
        fun(S) -> ?BACKEND:jump_to_label(S, 42) end
    ),
    {State4, a14} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	01a0f2        	movi	a15, 1\n"
        "   3:	0622e2        	l32i	a14, a2, 24\n"
        "   6:	002f16        	beqz	a15, 0xc\n"
        "   9:	0005c6        	j	0x24\n"
        "   c:	ff          	.byte	0xff\n"
        "   d:	ff          	.byte	0xff\n"
        "   e:	ff          	.byte	0xff\n"
        "   f:	ff          	.byte	0xff\n"
        "  10:	ff          	.byte	0xff\n"
        "  11:	ff          	.byte	0xff\n"
        "  12:	ff          	.byte	0xff\n"
        "  13:	ff          	.byte	0xff\n"
        "  14:	ff          	.byte	0xff\n"
        "  15:	ff          	.byte	0xff\n"
        "  16:	ff          	.byte	0xff\n"
        "  17:	ff          	.byte	0xff\n"
        "  18:	ff          	.byte	0xff\n"
        "  19:	ff          	.byte	0xff\n"
        "  1a:	ff          	.byte	0xff\n"
        "  1b:	ff          	.byte	0xff\n"
        "  1c:	ff          	.byte	0xff\n"
        "  1d:	ff          	.byte	0xff\n"
        "  1e:	ff          	.byte	0xff\n"
        "  1f:	ff          	.byte	0xff\n"
        "  20:	ff          	.byte	0xff\n"
        "  21:	ff          	.byte	0xff\n"
        "  22:	ff          	.byte	0xff\n"
        "  23:	ff          	.byte	0xff"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify ldr_y_reg invalidates its hidden temp register's cache entry.
%% ldr_y_reg uses first_avail(AvailT) as a scratch register to load the
%% Y_REGS pointer, but without the fix this temp was not invalidated.
ldr_y_reg_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, a13} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [a14, a13]),
    %% y_reg load: Reg=a14 (first avail), hidden temp=a13 (first avail of remaining)
    {State5, a14} = ?BACKEND:move_to_native_register(State4, {y_reg, 0}),
    %% a13 was hidden temp — if not invalidated, cache still says a13={x_reg,2}
    {State6, a13} = ?BACKEND:move_to_native_register(State5, {x_reg, 2}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	0522d2        	l32i	a13, a2, 20\n"
        "   c:	002de2        	l32i	a14, a13, 0\n"
        "   f:	0822d2        	l32i	a13, a2, 32"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify decrement_reductions_and_maybe_schedule_next invalidates the cache
%% so that a subsequent move_to_native_register emits a fresh load.
%% schedule_next clobbers caller-saved registers at the continuation point,
%% so the pre-block cache must not be reused there.
decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State1),
    %% After schedule_next, cache must be invalidated: a fresh load is emitted.
    {State3, a14} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0223e2        	l32i	a14, a3, 8\n"
        "   6:	ffcee2        	addi	a14, a14, -1\n"
        "   9:	0263e2        	s32i	a14, a3, 8\n"
        "   c:	029e56        	bnez	a14, 0x39\n"
        "   f:	0323e2        	l32i	a14, a3, 12\n"
        "  12:	36a082        	movi	a8, 54\n"
        "  15:	0020f0        	nop\n"
        "  18:	0020f0        	nop\n"
        "  1b:	0020f0        	nop\n"
        "  1e:	0020f0        	nop\n"
        "  21:	80ee80        	add	a14, a14, a8\n"
        "  24:	0163e2        	s32i	a14, a3, 4\n"
        "  27:	0224f2        	l32i	a15, a4, 8\n"
        "  2a:	02ad      	mov.n	a10, a2\n"
        "  2c:	03bd      	mov.n	a11, a3\n"
        "  2e:	000fe0        	callx8	a15\n"
        "  31:	0a2d      	mov.n	a2, a10\n"
        "  33:	000090        	retw\n"
        "  36:	00c136        	entry	a1, 96\n"
        "  39:	0622e2        	l32i	a14, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify move_to_vm_register_emit for {x_reg, N} leaves Temp in cache so a
%% subsequent move_to_native_register for the same x_reg reuses Temp without load.
cached_move_to_vm_x_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, 1}, {x_reg, 0}),
    %% a15 (Temp) is in cache as {x_reg, 1}: no extra load emitted.
    {State2, a15} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0722f2        	l32i	a15, a2, 28\n"
        "   3:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Verify move_to_vm_register_emit for {y_reg, Y} leaves Temp in cache so a
%% subsequent move_to_native_register for the same y_reg reuses Temp without load.
cached_move_to_vm_y_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {y_reg, 0}, {x_reg, 0}),
    %% a15 (Temp) is in cache as {y_reg, 0}: no extra load emitted.
    {State2, a15} = ?BACKEND:move_to_native_register(State1, {y_reg, 0}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0522e2        	l32i	a14, a2, 20\n"
        "   3:	002ef2        	l32i	a15, a14, 0\n"
        "   6:	0662f2        	s32i	a15, a2, 24"
    >>,
    ?assertStream(xtensa, Dump, Stream).

%% Regression tests for the hidden-temp cache-invalidation bug in the binary
%% arithmetic and bitwise ops (add/sub/or_/xor_). When the immediate operand
%% does not fit the instruction's inline form, these ops pick first_avail/1 as
%% a scratch register (the hidden Temp), materialize the immediate there with
%% mov_immediate, then combine it with Reg. mov_immediate invalidates Temp's
%% cache entry, but the ops used to keep the *pre*-mov_immediate cache (Regs0),
%% which resurrected Temp's stale value. A later move_to_native_register for
%% that value then wrongly reused the clobbered register instead of reloading.
%%
%% Each test caches {x_reg, 1} in a14, frees it (so it stays cached but becomes
%% available), then runs the op on a15: a14 is picked as the hidden Temp and
%% clobbered with the immediate. Re-requesting {x_reg, 1} must emit a fresh
%% l32i a14, a2, 28 rather than reuse the now-clobbered a14.

add_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, a13} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [a14, a13]),
    %% Hidden temp = a14 (first avail); add materializes 1000 through a movi temp.
    State5 = ?BACKEND:add(State4, a15, 1000),
    %% a14 was the hidden temp: it must be reloaded, not reused from cache.
    {State6, a14} = ?BACKEND:move_to_native_register(State5, {x_reg, 1}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	e8a3e2        	movi	a14, 0x3e8\n"
        "   c:	80ffe0        	add	a15, a15, a14\n"
        "   f:	0722e2        	l32i	a14, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).

sub_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, a13} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [a14, a13]),
    %% Hidden temp = a14 (first avail); sub materializes 1000 through a movi temp.
    State5 = ?BACKEND:sub(State4, a15, 1000),
    %% a14 was the hidden temp: it must be reloaded, not reused from cache.
    {State6, a14} = ?BACKEND:move_to_native_register(State5, {x_reg, 1}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	e8a3e2        	movi	a14, 0x3e8\n"
        "   c:	c0ffe0        	sub	a15, a15, a14\n"
        "   f:	0722e2        	l32i	a14, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).

or_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, a13} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [a14, a13]),
    %% Hidden temp = a14 (first avail); xtensa lacks ori, so 1000 uses a movi temp.
    State5 = ?BACKEND:or_(State4, a15, 1000),
    %% a14 was the hidden temp: it must be reloaded, not reused from cache.
    {State6, a14} = ?BACKEND:move_to_native_register(State5, {x_reg, 1}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	e8a3e2        	movi	a14, 0x3e8\n"
        "   c:	20ffe0        	or	a15, a15, a14\n"
        "   f:	0722e2        	l32i	a14, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).

xor_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, a15} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, a14} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, a13} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [a14, a13]),
    %% Hidden temp = a14 (first avail); xtensa lacks xori, so 1000 uses a movi temp.
    State5 = ?BACKEND:xor_(State4, a15, 1000),
    %% a14 was the hidden temp: it must be reloaded, not reused from cache.
    {State6, a14} = ?BACKEND:move_to_native_register(State5, {x_reg, 1}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	0622f2        	l32i	a15, a2, 24\n"
        "   3:	0722e2        	l32i	a14, a2, 28\n"
        "   6:	0822d2        	l32i	a13, a2, 32\n"
        "   9:	e8a3e2        	movi	a14, 0x3e8\n"
        "   c:	30ffe0        	xor	a15, a15, a14\n"
        "   f:	0722e2        	l32i	a14, a2, 28"
    >>,
    ?assertStream(xtensa, Dump, Stream).
