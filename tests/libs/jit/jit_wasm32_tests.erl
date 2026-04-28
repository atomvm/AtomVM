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

-module(jit_wasm32_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_wasm32).
-define(JUMP_TABLE_ENTRY_SIZE, 4).

%%=============================================================================
%% Basic backend properties
%%=============================================================================

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual(<<>>, ?BACKEND:stream(State)),
    ?assertEqual([], ?BACKEND:used_regs(State)),
    %% 8 scratch locals (indices 3-10)
    ?assertEqual(8, length(?BACKEND:available_regs(State))).

%%=============================================================================
%% Register allocation
%%=============================================================================

alloc_and_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    %% Allocate a register
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    ?assertEqual(local3, Reg1),
    ?assertEqual([Reg1], ?BACKEND:used_regs(State1)),
    %% Free it
    State2 = ?BACKEND:free_native_registers(State1, [Reg1]),
    ?assertEqual([], ?BACKEND:used_regs(State2)),
    ?assertEqual(8, length(?BACKEND:available_regs(State2))),
    ok.

assert_all_native_free_test() ->
    State = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual(ok, ?BACKEND:assert_all_native_free(State)).

%%=============================================================================
%% Jump table
%%=============================================================================

jump_table_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    Stream = ?BACKEND:stream(State1),
    %% Header: num_entries=4 (labels_count+1), wasm_offset=0, lines_offset=0
    <<NumEntries:32/little, WasmOffset:32/little, LinesOffset:32/little, _JumpTable/binary>> =
        Stream,
    ?assertEqual(4, NumEntries),
    ?assertEqual(0, WasmOffset),
    ?assertEqual(0, LinesOffset),
    %% Jump table area should be 4 * 4 = 16 bytes of zeros
    JumpTableSize = 4 * ?JUMP_TABLE_ENTRY_SIZE,
    ?assertEqual(12 + JumpTableSize, byte_size(Stream)).

%%=============================================================================
%% Label management
%%=============================================================================

add_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    %% Add label 0
    State2 = ?BACKEND:add_label(State1, 0),
    Offset0 = ?BACKEND:offset(State2),
    %% For wasm32, offset = label * JUMP_TABLE_ENTRY_SIZE
    ?assertEqual(0, Offset0),
    %% Add label 1
    State3 = ?BACKEND:add_label(State2, 1),
    Offset1 = ?BACKEND:offset(State3),
    ?assertEqual(?JUMP_TABLE_ENTRY_SIZE, Offset1),
    ok.

%%=============================================================================
%% Emit and instruction generation
%%=============================================================================

debugger_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:debugger(State2),
    %% Just verify it doesn't crash - the instruction goes to current_body,
    %% not the stream
    ?assertEqual(<<>>, ?BACKEND:stream(State0)),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	00                        	unreachable\n"
        " 0000e3:	20 00                     	local.get 0\n"
        " 0000e5:	0f                        	return\n"
        " 0000e6:	0b                        	end\n"
        " 0000e9:	08 7f                     	local[3..10] type=i32\n"
        " 0000eb:	20 00                     	local.get 0\n"
        " 0000ed:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Arithmetic operations
%%=============================================================================

shift_right_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right(State3, {free, Reg}, 2),
    %% Result should reuse the freed register
    ?assertEqual(Reg, ResultReg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 02                     	i32.const 2\n"
        " 0000ed:	76                        	i32.shr_u\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

shift_right_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% Pass local without {free, _} wrapper
    {State4, ResultReg} = ?BACKEND:shift_right(State3, Reg, 4),
    ?assert(ResultReg =/= Reg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 04                     	i32.const 4\n"
        " 0000ed:	76                        	i32.shr_u\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:shift_left(State3, Reg, 3),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 03                     	i32.const 3\n"
        " 0000ed:	74                        	i32.shl\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

div_reg_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:div_reg(State4, {free, Reg1}, Reg2),
    ?assertEqual(Reg1, ResultReg),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	6d                        	i32.div_s\n"
        " 0000f5:	21 03                     	local.set 3\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

div_reg_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:div_reg(State4, Reg1, Reg2),
    ?assert(ResultReg =/= Reg1),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	6d                        	i32.div_s\n"
        " 0000f5:	21 05                     	local.set 5\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

rem_reg_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:rem_reg(State4, {free, Reg1}, Reg2),
    ?assertEqual(Reg1, ResultReg),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	6f                        	i32.rem_s\n"
        " 0000f5:	21 03                     	local.set 3\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

rem_reg_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:rem_reg(State4, Reg1, Reg2),
    ?assert(ResultReg =/= Reg1),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	6f                        	i32.rem_s\n"
        " 0000f5:	21 05                     	local.set 5\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Memory access
%%=============================================================================

move_to_vm_register_x_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, Reg, {x_reg, 1}),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	36 02 1c                  	i32.store 2 28\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

move_to_vm_register_y_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, Reg, {y_reg, 0}),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 14                  	i32.load 2 20\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 04                     	local.get 4\n"
        " 0000f2:	20 03                     	local.get 3\n"
        " 0000f4:	36 02 00                  	i32.store 2 0\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

move_to_native_register_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    ?assertEqual(local3, Reg),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	0f                        	return\n"
        " 0000ec:	0b                        	end\n"
        " 0000ef:	08 7f                     	local[3..10] type=i32\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

copy_to_native_register_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:copy_to_native_register(State2, {x_reg, 0}),
    ?assertEqual(local3, Reg),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	0f                        	return\n"
        " 0000ec:	0b                        	end\n"
        " 0000ef:	08 7f                     	local[3..10] type=i32\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:increment_sp(State2, 4),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	20 00                     	local.get 0\n"
        " 0000e6:	28 02 14                  	i32.load 2 20\n"
        " 0000e9:	41 10                     	i32.const 16\n"
        " 0000eb:	6a                        	i32.add\n"
        " 0000ec:	36 02 14                  	i32.store 2 20\n"
        " 0000ef:	20 00                     	local.get 0\n"
        " 0000f1:	0f                        	return\n"
        " 0000f2:	0b                        	end\n"
        " 0000f5:	08 7f                     	local[3..10] type=i32\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

set_bs_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:set_bs(State3, Reg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	36 02 64                  	i32.store 2 100\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	41 00                     	i32.const 0\n"
        " 0000f4:	36 02 68                  	i32.store 2 104\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

get_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:get_array_element(State3, {ptr, Reg}, 0),
    ?assertEqual(local4, ResultReg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	28 02 00                  	i32.load 2 0\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

move_to_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_array_element(State3, 42, {ptr, Reg}, 0),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 2a                     	i32.const 42\n"
        " 0000ed:	36 02 00                  	i32.store 2 0\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

move_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:move_array_element(State4, {ptr, Reg1}, 0, {ptr, Reg2}),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 04                     	local.get 4\n"
        " 0000f2:	20 03                     	local.get 3\n"
        " 0000f4:	28 02 00                  	i32.load 2 0\n"
        " 0000f7:	36 02 00                  	i32.store 2 0\n"
        " 0000fa:	20 00                     	local.get 0\n"
        " 0000fc:	0f                        	return\n"
        " 0000fd:	0b                        	end\n"
        " 000100:	08 7f                     	local[3..10] type=i32\n"
        " 000102:	20 00                     	local.get 0\n"
        " 000104:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Continuation and module index
%%=============================================================================

set_continuation_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:set_continuation_to_label(State2, 1),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000ec:	08 7f                     	local[3..10] type=i32\n"
        " 0000ee:	20 01                     	local.get 1\n"
        " 0000f0:	41 02                     	i32.const 2\n"
        " 0000f2:	36 02 04                  	i32.store 2 4\n"
        " 0000f5:	20 00                     	local.get 0\n"
        " 0000f7:	0f                        	return\n"
        " 0000f8:	0b                        	end\n"
        " 0000fb:	08 7f                     	local[3..10] type=i32\n"
        " 0000fd:	20 00                     	local.get 0\n"
        " 0000ff:	0b                        	end\n"
        " 000102:	08 7f                     	local[3..10] type=i32\n"
        " 000104:	20 00                     	local.get 0\n"
        " 000106:	0b                        	end\n"
        " 000109:	08 7f                     	local[3..10] type=i32\n"
        " 00010b:	20 00                     	local.get 0\n"
        " 00010d:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

set_continuation_to_offset_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, {wasm_cont, ContLabel}} = ?BACKEND:set_continuation_to_offset(State2),
    ?assert(is_integer(ContLabel)),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000f2:	08 7f                     	local[3..10] type=i32\n"
        " 0000f4:	20 01                     	local.get 1\n"
        " 0000f6:	41 05                     	i32.const 5\n"
        " 0000f8:	36 02 04                  	i32.store 2 4\n"
        " 0000fb:	20 00                     	local.get 0\n"
        " 0000fd:	0f                        	return\n"
        " 0000fe:	0b                        	end\n"
        " 000101:	08 7f                     	local[3..10] type=i32\n"
        " 000103:	20 00                     	local.get 0\n"
        " 000105:	0b                        	end\n"
        " 000108:	08 7f                     	local[3..10] type=i32\n"
        " 00010a:	20 00                     	local.get 0\n"
        " 00010c:	0b                        	end\n"
        " 00010f:	08 7f                     	local[3..10] type=i32\n"
        " 000111:	20 00                     	local.get 0\n"
        " 000113:	0b                        	end\n"
        " 000116:	08 7f                     	local[3..10] type=i32\n"
        " 000118:	20 00                     	local.get 0\n"
        " 00011a:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

continuation_entry_point_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:continuation_entry_point(State0),
    %% Should be a no-op
    ?assertEqual(?BACKEND:stream(State0), ?BACKEND:stream(State1)),
    ok.

get_module_index_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:get_module_index(State2),
    ?assertEqual(local3, Reg),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 01                     	local.get 1\n"
        " 0000e4:	28 02 00                  	i32.load 2 0\n"
        " 0000e7:	28 02 00                  	i32.load 2 0\n"
        " 0000ea:	21 03                     	local.set 3\n"
        " 0000ec:	20 00                     	local.get 0\n"
        " 0000ee:	0f                        	return\n"
        " 0000ef:	0b                        	end\n"
        " 0000f2:	08 7f                     	local[3..10] type=i32\n"
        " 0000f4:	20 00                     	local.get 0\n"
        " 0000f6:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Control flow
%%=============================================================================

jump_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:jump_to_label(State2, 1),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000ec:	08 7f                     	local[3..10] type=i32\n"
        " 0000ee:	20 01                     	local.get 1\n"
        " 0000f0:	41 02                     	i32.const 2\n"
        " 0000f2:	36 02 04                  	i32.store 2 4\n"
        " 0000f5:	20 00                     	local.get 0\n"
        " 0000f7:	0f                        	return\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end\n"
        " 00010c:	08 7f                     	local[3..10] type=i32\n"
        " 00010e:	20 00                     	local.get 0\n"
        " 000110:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

cond_jump_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:cond_jump_to_label(State3, {{free, Reg}, '==', {free, Reg}}, 1),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000ec:	08 7f                     	local[3..10] type=i32\n"
        " 0000ee:	20 00                     	local.get 0\n"
        " 0000f0:	28 02 18                  	i32.load 2 24\n"
        " 0000f3:	21 03                     	local.set 3\n"
        " 0000f5:	20 03                     	local.get 3\n"
        " 0000f7:	20 03                     	local.get 3\n"
        " 0000f9:	46                        	i32.eq\n"
        " 0000fa:	04 40                     	if\n"
        " 0000fc:	20 01                     	  local.get 1\n"
        " 0000fe:	41 02                     	  i32.const 2\n"
        " 000100:	36 02 04                  	  i32.store 2 4\n"
        " 000103:	20 00                     	  local.get 0\n"
        " 000105:	0f                        	  return\n"
        " 000106:	0b                        	end\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0f                        	return\n"
        " 00010a:	0b                        	end\n"
        " 00010d:	08 7f                     	local[3..10] type=i32\n"
        " 00010f:	20 00                     	local.get 0\n"
        " 000111:	0b                        	end\n"
        " 000114:	08 7f                     	local[3..10] type=i32\n"
        " 000116:	20 00                     	local.get 0\n"
        " 000118:	0b                        	end\n"
        " 00011b:	08 7f                     	local[3..10] type=i32\n"
        " 00011d:	20 00                     	local.get 0\n"
        " 00011f:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

return_if_not_equal_to_ctx_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:return_if_not_equal_to_ctx(State3, {free, Reg}),
    %% Register should be freed
    ?assertEqual([], ?BACKEND:used_regs(State4)),
    ok.

if_block_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {{free, Reg}, '==', {free, Reg}}, fun(S) ->
        ?BACKEND:debugger(S)
    end),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	46                        	i32.eq\n"
        " 0000ee:	04 40                     	if\n"
        " 0000f0:	00                        	  unreachable\n"
        " 0000f1:	0b                        	end\n"
        " 0000f2:	20 00                     	local.get 0\n"
        " 0000f4:	0f                        	return\n"
        " 0000f5:	0b                        	end\n"
        " 0000f8:	08 7f                     	local[3..10] type=i32\n"
        " 0000fa:	20 00                     	local.get 0\n"
        " 0000fc:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

if_else_block_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_else_block(
        State3,
        {Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end,
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 00                     	i32.const 0\n"
        " 0000ed:	46                        	i32.eq\n"
        " 0000ee:	04 40                     	if\n"
        " 0000f0:	00                        	  unreachable\n"
        " 0000f1:	05                        	else\n"
        " 0000f2:	00                        	  unreachable\n"
        " 0000f3:	0b                        	end\n"
        " 0000f4:	20 00                     	local.get 0\n"
        " 0000f6:	0f                        	return\n"
        " 0000f7:	0b                        	end\n"
        " 0000fa:	08 7f                     	local[3..10] type=i32\n"
        " 0000fc:	20 00                     	local.get 0\n"
        " 0000fe:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Condition generation
%%=============================================================================

condition_lt_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '<', 10}, fun(S) -> ?BACKEND:debugger(S) end),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 0a                     	i32.const 10\n"
        " 0000f3:	48                        	i32.lt_s\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_eq_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '==', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 2a                     	i32.const 42\n"
        " 0000f3:	46                        	i32.eq\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 00                     	i32.const 0\n"
        " 0000f3:	47                        	i32.ne\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_and_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'and', [{Reg, '==', 0}, {Reg, '!=', 1}]},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 00                     	i32.const 0\n"
        " 0000f3:	46                        	i32.eq\n"
        " 0000f4:	20 03                     	local.get 3\n"
        " 0000f6:	41 01                     	i32.const 1\n"
        " 0000f8:	47                        	i32.ne\n"
        " 0000f9:	71                        	i32.and\n"
        " 0000fa:	04 40                     	if\n"
        " 0000fc:	00                        	  unreachable\n"
        " 0000fd:	0b                        	end\n"
        " 0000fe:	20 00                     	local.get 0\n"
        " 000100:	0f                        	return\n"
        " 000101:	0b                        	end\n"
        " 000104:	08 7f                     	local[3..10] type=i32\n"
        " 000106:	20 00                     	local.get 0\n"
        " 000108:	0b                        	end\n"
        " 00010b:	08 7f                     	local[3..10] type=i32\n"
        " 00010d:	20 00                     	local.get 0\n"
        " 00010f:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_mask_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {Reg, '&', 16#F, '!=', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 0f                     	i32.const 15\n"
        " 0000f3:	71                        	i32.and\n"
        " 0000f4:	41 00                     	i32.const 0\n"
        " 0000f6:	47                        	i32.ne\n"
        " 0000f7:	04 40                     	if\n"
        " 0000f9:	00                        	  unreachable\n"
        " 0000fa:	0b                        	end\n"
        " 0000fb:	20 00                     	local.get 0\n"
        " 0000fd:	0f                        	return\n"
        " 0000fe:	0b                        	end\n"
        " 000101:	08 7f                     	local[3..10] type=i32\n"
        " 000103:	20 00                     	local.get 0\n"
        " 000105:	0b                        	end\n"
        " 000108:	08 7f                     	local[3..10] type=i32\n"
        " 00010a:	20 00                     	local.get 0\n"
        " 00010c:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_bool_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(bool)', Reg, '==', false},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 01                     	i32.const 1\n"
        " 0000f3:	71                        	i32.and\n"
        " 0000f4:	45                        	i32.eqz\n"
        " 0000f5:	04 40                     	if\n"
        " 0000f7:	00                        	  unreachable\n"
        " 0000f8:	0b                        	end\n"
        " 0000f9:	20 00                     	local.get 0\n"
        " 0000fb:	0f                        	return\n"
        " 0000fc:	0b                        	end\n"
        " 0000ff:	08 7f                     	local[3..10] type=i32\n"
        " 000101:	20 00                     	local.get 0\n"
        " 000103:	0b                        	end\n"
        " 000106:	08 7f                     	local[3..10] type=i32\n"
        " 000108:	20 00                     	local.get 0\n"
        " 00010a:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_bool_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(bool)', Reg, '!=', false},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 01                     	i32.const 1\n"
        " 0000f3:	71                        	i32.and\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_int_cast_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(int)', Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 00                     	i32.const 0\n"
        " 0000f3:	46                        	i32.eq\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Primitive calls
%%=============================================================================

call_primitive_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, ResultLocal} = ?BACKEND:call_primitive(State2, 0, [ctx, jit_state]),
    ?assertEqual(local3, ResultLocal),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	20 01                     	local.get 1\n"
        " 0000e6:	20 02                     	local.get 2\n"
        " 0000e8:	28 02 00                  	i32.load 2 0\n"
        " 0000eb:	11 01 00                  	call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_primitive_last(State2, 0, [ctx, jit_state]),
    %% All regs should be free after a tail call
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    ok.

unreachable_test_state() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    ?BACKEND:add_label(State1, 0).

setup_cached_x_reg0(State0) ->
    {State1, CondReg} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, CachedReg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    {?BACKEND:free_native_registers(State2, [CachedReg]), CondReg}.

setup_cached_x_reg0_with_offset(State0) ->
    {State1, OffsetReg} = ?BACKEND:move_to_native_register(State0, 16#100),
    {State2, CondReg} = ?BACKEND:move_to_native_register(State1, 1),
    {State3, CachedReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {?BACKEND:free_native_registers(State3, [CachedReg]), CondReg, OffsetReg, CachedReg}.

terminal_if_preserves_cached_x_reg0(State0, TerminalFun) ->
    {State1, CondReg} = setup_cached_x_reg0(State0),
    State2 = ?BACKEND:if_block(State1, {{free, CondReg}, '==', 0}, TerminalFun),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State3.

call_primitive_last_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, 0, [ctx, jit_state])
    end),
    Stream = ?BACKEND:stream(?BACKEND:return_labels_and_lines(State0, [])),
    Dump = <<
        " 0000b3:	08 7f                     	local[3..10] type=i32\n"
        " 0000b5:	41 01                     	i32.const 1\n"
        " 0000b7:	21 03                     	local.set 3\n"
        " 0000b9:	20 00                     	local.get 0\n"
        " 0000bb:	28 02 18                  	i32.load 2 24\n"
        " 0000be:	21 04                     	local.set 4\n"
        " 0000c0:	20 03                     	local.get 3\n"
        " 0000c2:	41 00                     	i32.const 0\n"
        " 0000c4:	46                        	i32.eq\n"
        " 0000c5:	04 40                     	if\n"
        " 0000c7:	20 00                     	  local.get 0\n"
        " 0000c9:	20 01                     	  local.get 1\n"
        " 0000cb:	20 02                     	  local.get 2\n"
        " 0000cd:	28 02 00                  	  i32.load 2 0\n"
        " 0000d0:	11 01 00                  	  call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 0000d3:	0f                        	  return\n"
        " 0000d4:	0b                        	end\n"
        " 0000d5:	20 00                     	local.get 0\n"
        " 0000d7:	28 02 18                  	i32.load 2 24\n"
        " 0000da:	21 03                     	local.set 3\n"
        " 0000dc:	20 00                     	local.get 0\n"
        " 0000de:	0f                        	return\n"
        " 0000df:	0b                        	end\n"
        " 0000e2:	08 7f                     	local[3..10] type=i32\n"
        " 0000e4:	20 00                     	local.get 0\n"
        " 0000e6:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

jump_to_label_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_label(BSt0, 42)
    end),
    Stream = ?BACKEND:stream(?BACKEND:return_labels_and_lines(State0, [])),
    Dump = <<
        " 0000b3:	08 7f                     	local[3..10] type=i32\n"
        " 0000b5:	41 01                     	i32.const 1\n"
        " 0000b7:	21 03                     	local.set 3\n"
        " 0000b9:	20 00                     	local.get 0\n"
        " 0000bb:	28 02 18                  	i32.load 2 24\n"
        " 0000be:	21 04                     	local.set 4\n"
        " 0000c0:	20 03                     	local.get 3\n"
        " 0000c2:	41 00                     	i32.const 0\n"
        " 0000c4:	46                        	i32.eq\n"
        " 0000c5:	04 40                     	if\n"
        " 0000c7:	20 01                     	  local.get 1\n"
        " 0000c9:	41 2b                     	  i32.const 43\n"
        " 0000cb:	36 02 04                  	  i32.store 2 4\n"
        " 0000ce:	20 00                     	  local.get 0\n"
        " 0000d0:	0f                        	  return\n"
        " 0000d1:	0b                        	end\n"
        " 0000d2:	20 00                     	local.get 0\n"
        " 0000d4:	28 02 18                  	i32.load 2 24\n"
        " 0000d7:	21 03                     	local.set 3\n"
        " 0000d9:	20 00                     	local.get 0\n"
        " 0000db:	0f                        	return\n"
        " 0000dc:	0b                        	end\n"
        " 0000df:	08 7f                     	local[3..10] type=i32\n"
        " 0000e1:	20 00                     	local.get 0\n"
        " 0000e3:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

jump_to_offset_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_offset(BSt0, 16#100)
    end),
    Stream = ?BACKEND:stream(?BACKEND:return_labels_and_lines(State0, [])),
    Dump = <<
        " 0000b3:	08 7f                     	local[3..10] type=i32\n"
        " 0000b5:	41 01                     	i32.const 1\n"
        " 0000b7:	21 03                     	local.set 3\n"
        " 0000b9:	20 00                     	local.get 0\n"
        " 0000bb:	28 02 18                  	i32.load 2 24\n"
        " 0000be:	21 04                     	local.set 4\n"
        " 0000c0:	20 03                     	local.get 3\n"
        " 0000c2:	41 00                     	i32.const 0\n"
        " 0000c4:	46                        	i32.eq\n"
        " 0000c5:	04 40                     	if\n"
        " 0000c7:	20 01                     	  local.get 1\n"
        " 0000c9:	41 3e                     	  i32.const 62\n"
        " 0000cb:	36 02 04                  	  i32.store 2 4\n"
        " 0000ce:	20 00                     	  local.get 0\n"
        " 0000d0:	0f                        	  return\n"
        " 0000d1:	0b                        	end\n"
        " 0000d2:	20 00                     	local.get 0\n"
        " 0000d4:	28 02 18                  	i32.load 2 24\n"
        " 0000d7:	21 03                     	local.set 3\n"
        " 0000d9:	20 00                     	local.get 0\n"
        " 0000db:	0f                        	return\n"
        " 0000dc:	0b                        	end\n"
        " 0000df:	08 7f                     	local[3..10] type=i32\n"
        " 0000e1:	20 00                     	local.get 0\n"
        " 0000e3:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

jump_to_continuation_if_block_preserves_cache_test() ->
    State0 = unreachable_test_state(),
    {State1, CondReg, OffsetReg, _CachedReg} = setup_cached_x_reg0_with_offset(State0),
    State2 = ?BACKEND:if_block(State1, {{free, CondReg}, '==', 0}, fun(BSt0) ->
        ?BACKEND:jump_to_continuation(BSt0, {free, OffsetReg})
    end),
    {State3, _Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(?BACKEND:return_labels_and_lines(State3, [])),
    Dump = <<
        " 0000b3:	08 7f                     	local[3..10] type=i32\n"
        " 0000b5:	41 80 02                  	i32.const 256\n"
        " 0000b8:	21 03                     	local.set 3\n"
        " 0000ba:	41 01                     	i32.const 1\n"
        " 0000bc:	21 04                     	local.set 4\n"
        " 0000be:	20 00                     	local.get 0\n"
        " 0000c0:	28 02 18                  	i32.load 2 24\n"
        " 0000c3:	21 05                     	local.set 5\n"
        " 0000c5:	20 04                     	local.get 4\n"
        " 0000c7:	41 00                     	i32.const 0\n"
        " 0000c9:	46                        	i32.eq\n"
        " 0000ca:	04 40                     	if\n"
        " 0000cc:	20 01                     	  local.get 1\n"
        " 0000ce:	20 03                     	  local.get 3\n"
        " 0000d0:	41 04                     	  i32.const 4\n"
        " 0000d2:	6e                        	  i32.div_u\n"
        " 0000d3:	41 01                     	  i32.const 1\n"
        " 0000d5:	6a                        	  i32.add\n"
        " 0000d6:	36 02 04                  	  i32.store 2 4\n"
        " 0000d9:	20 00                     	  local.get 0\n"
        " 0000db:	0f                        	  return\n"
        " 0000dc:	0b                        	end\n"
        " 0000dd:	20 00                     	local.get 0\n"
        " 0000df:	28 02 18                  	i32.load 2 24\n"
        " 0000e2:	21 04                     	local.set 4\n"
        " 0000e4:	20 00                     	local.get 0\n"
        " 0000e6:	0f                        	return\n"
        " 0000e7:	0b                        	end\n"
        " 0000ea:	08 7f                     	local[3..10] type=i32\n"
        " 0000ec:	20 00                     	local.get 0\n"
        " 0000ee:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Scheduling
%%=============================================================================

decrement_reductions_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 01                     	local.get 1\n"
        " 0000ea:	28 02 08                  	i32.load 2 8\n"
        " 0000ed:	41 01                     	i32.const 1\n"
        " 0000ef:	6b                        	i32.sub\n"
        " 0000f0:	21 03                     	local.set 3\n"
        " 0000f2:	20 01                     	local.get 1\n"
        " 0000f4:	20 03                     	local.get 3\n"
        " 0000f6:	36 02 08                  	i32.store 2 8\n"
        " 0000f9:	20 03                     	local.get 3\n"
        " 0000fb:	45                        	i32.eqz\n"
        " 0000fc:	04 40                     	if\n"
        " 0000fe:	20 01                     	  local.get 1\n"
        " 000100:	41 03                     	  i32.const 3\n"
        " 000102:	36 02 04                  	  i32.store 2 4\n"
        " 000105:	20 00                     	  local.get 0\n"
        " 000107:	20 01                     	  local.get 1\n"
        " 000109:	20 02                     	  local.get 2\n"
        " 00010b:	28 02 08                  	  i32.load 2 8\n"
        " 00010e:	11 01 00                  	  call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 000111:	0f                        	  return\n"
        " 000112:	0b                        	end\n"
        " 000113:	20 01                     	local.get 1\n"
        " 000115:	41 03                     	i32.const 3\n"
        " 000117:	36 02 04                  	i32.store 2 4\n"
        " 00011a:	20 00                     	local.get 0\n"
        " 00011c:	0f                        	return\n"
        " 00011d:	0b                        	end\n"
        " 000120:	08 7f                     	local[3..10] type=i32\n"
        " 000122:	20 00                     	local.get 0\n"
        " 000124:	0b                        	end\n"
        " 000127:	08 7f                     	local[3..10] type=i32\n"
        " 000129:	20 00                     	local.get 0\n"
        " 00012b:	0f                        	return\n"
        " 00012c:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    State5 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State4),
    {State6, Reg} = ?BACKEND:move_to_native_register(State5, {x_reg, 0}),
    State7 = ?BACKEND:return_labels_and_lines(State6, []),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        " 0000b9:	08 7f                     	local[3..10] type=i32\n"
        " 0000bb:	20 00                     	local.get 0\n"
        " 0000bd:	28 02 18                  	i32.load 2 24\n"
        " 0000c0:	21 03                     	local.set 3\n"
        " 0000c2:	20 01                     	local.get 1\n"
        " 0000c4:	28 02 08                  	i32.load 2 8\n"
        " 0000c7:	41 01                     	i32.const 1\n"
        " 0000c9:	6b                        	i32.sub\n"
        " 0000ca:	21 03                     	local.set 3\n"
        " 0000cc:	20 01                     	local.get 1\n"
        " 0000ce:	20 03                     	local.get 3\n"
        " 0000d0:	36 02 08                  	i32.store 2 8\n"
        " 0000d3:	20 03                     	local.get 3\n"
        " 0000d5:	45                        	i32.eqz\n"
        " 0000d6:	04 40                     	if\n"
        " 0000d8:	20 01                     	  local.get 1\n"
        " 0000da:	41 03                     	  i32.const 3\n"
        " 0000dc:	36 02 04                  	  i32.store 2 4\n"
        " 0000df:	20 00                     	  local.get 0\n"
        " 0000e1:	20 01                     	  local.get 1\n"
        " 0000e3:	20 02                     	  local.get 2\n"
        " 0000e5:	28 02 08                  	  i32.load 2 8\n"
        " 0000e8:	11 01 00                  	  call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 0000eb:	0f                        	  return\n"
        " 0000ec:	0b                        	end\n"
        " 0000ed:	20 01                     	local.get 1\n"
        " 0000ef:	41 03                     	i32.const 3\n"
        " 0000f1:	36 02 04                  	i32.store 2 4\n"
        " 0000f4:	20 00                     	local.get 0\n"
        " 0000f6:	0f                        	return\n"
        " 0000f7:	0b                        	end\n"
        " 0000fa:	08 7f                     	local[3..10] type=i32\n"
        " 0000fc:	20 00                     	local.get 0\n"
        " 0000fe:	0b                        	end\n"
        " 000101:	08 7f                     	local[3..10] type=i32\n"
        " 000103:	20 00                     	local.get 0\n"
        " 000105:	28 02 18                  	i32.load 2 24\n"
        " 000108:	21 03                     	local.set 3\n"
        " 00010a:	20 00                     	local.get 0\n"
        " 00010c:	0f                        	return\n"
        " 00010d:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

call_or_schedule_next_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_or_schedule_next(State2, 1),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000f2:	08 7f                     	local[3..10] type=i32\n"
        " 0000f4:	20 01                     	local.get 1\n"
        " 0000f6:	28 02 00                  	i32.load 2 0\n"
        " 0000f9:	28 02 00                  	i32.load 2 0\n"
        " 0000fc:	21 03                     	local.set 3\n"
        " 0000fe:	20 03                     	local.get 3\n"
        " 000100:	41 18                     	i32.const 24\n"
        " 000102:	74                        	i32.shl\n"
        " 000103:	21 03                     	local.set 3\n"
        " 000105:	41 c0 00                  	i32.const 64\n"
        " 000108:	21 04                     	local.set 4\n"
        " 00010a:	20 03                     	local.get 3\n"
        " 00010c:	20 04                     	local.get 4\n"
        " 00010e:	72                        	i32.or\n"
        " 00010f:	21 03                     	local.set 3\n"
        " 000111:	20 00                     	local.get 0\n"
        " 000113:	20 03                     	local.get 3\n"
        " 000115:	36 02 5c                  	i32.store 2 92\n"
        " 000118:	20 01                     	local.get 1\n"
        " 00011a:	28 02 08                  	i32.load 2 8\n"
        " 00011d:	41 01                     	i32.const 1\n"
        " 00011f:	6b                        	i32.sub\n"
        " 000120:	21 03                     	local.set 3\n"
        " 000122:	20 01                     	local.get 1\n"
        " 000124:	20 03                     	local.get 3\n"
        " 000126:	36 02 08                  	i32.store 2 8\n"
        " 000129:	20 03                     	local.get 3\n"
        " 00012b:	45                        	i32.eqz\n"
        " 00012c:	04 40                     	if\n"
        " 00012e:	20 01                     	  local.get 1\n"
        " 000130:	41 02                     	  i32.const 2\n"
        " 000132:	36 02 04                  	  i32.store 2 4\n"
        " 000135:	20 00                     	  local.get 0\n"
        " 000137:	20 01                     	  local.get 1\n"
        " 000139:	20 02                     	  local.get 2\n"
        " 00013b:	28 02 08                  	  i32.load 2 8\n"
        " 00013e:	11 01 00                  	  call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 000141:	0f                        	  return\n"
        " 000142:	0b                        	end\n"
        " 000143:	20 01                     	local.get 1\n"
        " 000145:	41 02                     	i32.const 2\n"
        " 000147:	36 02 04                  	i32.store 2 4\n"
        " 00014a:	20 00                     	local.get 0\n"
        " 00014c:	0f                        	return\n"
        " 00014d:	20 00                     	local.get 0\n"
        " 00014f:	0f                        	return\n"
        " 000150:	0b                        	end\n"
        " 000153:	08 7f                     	local[3..10] type=i32\n"
        " 000155:	20 00                     	local.get 0\n"
        " 000157:	0b                        	end\n"
        " 00015a:	08 7f                     	local[3..10] type=i32\n"
        " 00015c:	20 00                     	local.get 0\n"
        " 00015e:	0b                        	end\n"
        " 000161:	08 7f                     	local[3..10] type=i32\n"
        " 000163:	20 00                     	local.get 0\n"
        " 000165:	0b                        	end\n"
        " 000168:	08 7f                     	local[3..10] type=i32\n"
        " 00016a:	20 00                     	local.get 0\n"
        " 00016c:	0f                        	return\n"
        " 00016d:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

call_only_or_schedule_next_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 1),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000ec:	08 7f                     	local[3..10] type=i32\n"
        " 0000ee:	20 01                     	local.get 1\n"
        " 0000f0:	28 02 08                  	i32.load 2 8\n"
        " 0000f3:	41 01                     	i32.const 1\n"
        " 0000f5:	6b                        	i32.sub\n"
        " 0000f6:	21 03                     	local.set 3\n"
        " 0000f8:	20 01                     	local.get 1\n"
        " 0000fa:	20 03                     	local.get 3\n"
        " 0000fc:	36 02 08                  	i32.store 2 8\n"
        " 0000ff:	20 03                     	local.get 3\n"
        " 000101:	45                        	i32.eqz\n"
        " 000102:	04 40                     	if\n"
        " 000104:	20 01                     	  local.get 1\n"
        " 000106:	41 02                     	  i32.const 2\n"
        " 000108:	36 02 04                  	  i32.store 2 4\n"
        " 00010b:	20 00                     	  local.get 0\n"
        " 00010d:	20 01                     	  local.get 1\n"
        " 00010f:	20 02                     	  local.get 2\n"
        " 000111:	28 02 08                  	  i32.load 2 8\n"
        " 000114:	11 01 00                  	  call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 000117:	0f                        	  return\n"
        " 000118:	0b                        	end\n"
        " 000119:	20 01                     	local.get 1\n"
        " 00011b:	41 02                     	i32.const 2\n"
        " 00011d:	36 02 04                  	i32.store 2 4\n"
        " 000120:	20 00                     	local.get 0\n"
        " 000122:	0f                        	return\n"
        " 000123:	20 00                     	local.get 0\n"
        " 000125:	0f                        	return\n"
        " 000126:	0b                        	end\n"
        " 000129:	08 7f                     	local[3..10] type=i32\n"
        " 00012b:	20 00                     	local.get 0\n"
        " 00012d:	0b                        	end\n"
        " 000130:	08 7f                     	local[3..10] type=i32\n"
        " 000132:	20 00                     	local.get 0\n"
        " 000134:	0b                        	end\n"
        " 000137:	08 7f                     	local[3..10] type=i32\n"
        " 000139:	20 00                     	local.get 0\n"
        " 00013b:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% CP operations
%%=============================================================================

move_to_cp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:move_to_cp(State2, {y_reg, 0}),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 14                  	i32.load 2 20\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	28 02 00                  	i32.load 2 0\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	20 03                     	local.get 3\n"
        " 0000f4:	36 02 5c                  	i32.store 2 92\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Complete WASM module generation
%%=============================================================================

return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    %% Add some instructions to label 0
    State3 = ?BACKEND:debugger(State2),
    State4 = ?BACKEND:add_label(State3, 1),
    %% Add some instructions to label 1
    State5 = ?BACKEND:debugger(State4),
    %% Finalize
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    %% Verify the stream has a valid WASM module embedded
    <<NumEntries:32/little, WasmOffset:32/little, _Rest/binary>> = Stream,
    ?assertEqual(3, NumEntries),
    ?assert(WasmOffset > 0),
    ?assert(WasmOffset < byte_size(Stream)),
    %% Extract the WASM module
    WasmModule = binary:part(Stream, WasmOffset, byte_size(Stream) - WasmOffset),
    %% Verify WASM magic number and version
    <<16#00, 16#61, 16#73, 16#6D, 16#01, 16#00, 16#00, 16#00, _Sections/binary>> = WasmModule,
    ok.

return_labels_and_lines_empty_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 0),
    State2 = ?BACKEND:return_labels_and_lines(State1, []),
    Stream = ?BACKEND:stream(State2),
    <<NumEntries:32/little, WasmOffset:32/little, _Rest/binary>> = Stream,
    ?assertEqual(1, NumEntries),
    ?assert(WasmOffset > 0),
    %% Extract WASM module and verify magic
    WasmModule = binary:part(Stream, WasmOffset, byte_size(Stream) - WasmOffset),
    <<16#00, 16#61, 16#73, 16#6D, 16#01, 16#00, 16#00, 16#00, _/binary>> = WasmModule,
    ok.

%%=============================================================================
%% Function pointer calls (BIFs)
%%=============================================================================

call_func_ptr_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, FuncPtr} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:call_func_ptr(State3, {free, FuncPtr}, [ctx]),
    ?assertEqual(local4, ResultReg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 0000f0:	21 04                     	local.set 4\n"
        " 0000f2:	20 00                     	local.get 0\n"
        " 0000f4:	0f                        	return\n"
        " 0000f5:	0b                        	end\n"
        " 0000f8:	08 7f                     	local[3..10] type=i32\n"
        " 0000fa:	20 00                     	local.get 0\n"
        " 0000fc:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Flush
%%=============================================================================

flush_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:flush(State0),
    ?assertEqual(?BACKEND:stream(State0), ?BACKEND:stream(State1)),
    ok.

%%=============================================================================
%% Update branches
%%=============================================================================

update_branches_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:update_branches(State0),
    ?assertEqual(<<>>, ?BACKEND:stream(State1)).

%%=============================================================================
%% Shift right arithmetic
%%=============================================================================

shift_right_arith_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right_arith(State3, {free, Reg}, 2),
    ?assertEqual(Reg, ResultReg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 02                     	i32.const 2\n"
        " 0000ed:	75                        	i32.shr_s\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

shift_right_arith_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right_arith(State3, Reg, 4),
    ?assert(ResultReg =/= Reg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 04                     	i32.const 4\n"
        " 0000ed:	75                        	i32.shr_s\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Bitwise operations
%%=============================================================================

and_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:and_(State3, Reg, 16#FF),
    ?assert(ResultReg =/= Reg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 ff 01                  	i32.const 255\n"
        " 0000ee:	71                        	i32.and\n"
        " 0000ef:	21 04                     	local.set 4\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	0f                        	return\n"
        " 0000f4:	0b                        	end\n"
        " 0000f7:	08 7f                     	local[3..10] type=i32\n"
        " 0000f9:	20 00                     	local.get 0\n"
        " 0000fb:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

or_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:or_(State4, Reg1, Reg2),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	72                        	i32.or\n"
        " 0000f5:	21 03                     	local.set 3\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	0b                        	end\n"
        " 0000fd:	08 7f                     	local[3..10] type=i32\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

xor_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:xor_(State3, Reg, 16#FF),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 ff 01                  	i32.const 255\n"
        " 0000ee:	73                        	i32.xor\n"
        " 0000ef:	21 03                     	local.set 3\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	0f                        	return\n"
        " 0000f4:	0b                        	end\n"
        " 0000f7:	08 7f                     	local[3..10] type=i32\n"
        " 0000f9:	20 00                     	local.get 0\n"
        " 0000fb:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Arithmetic
%%=============================================================================

add_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:add(State3, Reg, 42),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 2a                     	i32.const 42\n"
        " 0000ed:	6a                        	i32.add\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

sub_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:sub(State3, Reg, 10),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 0a                     	i32.const 10\n"
        " 0000ed:	6b                        	i32.sub\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

mul_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:mul(State3, Reg, 3),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 03                     	local.get 3\n"
        " 0000eb:	41 03                     	i32.const 3\n"
        " 0000ed:	6c                        	i32.mul\n"
        " 0000ee:	21 03                     	local.set 3\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%%=============================================================================
%% Additional coverage: edge cases and more argument types
%%=============================================================================

%% Test free_native_register with non-scratch local (should be no-op)
free_native_register_noop_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    %% Free a non-scratch value (should not crash)
    State1 = ?BACKEND:free_native_registers(State0, [{x_reg, 0}]),
    ?assertEqual(8, length(?BACKEND:available_regs(State1))),
    %% Free a ptr
    {State2, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State3 = ?BACKEND:free_native_registers(State2, [{ptr, Reg}]),
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    ok.

%% Test move_to_native_register with explicit local target
move_to_native_register_explicit_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_native_register(State3, {y_reg, 0}, Reg),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 14                  	i32.load 2 20\n"
        " 0000ee:	28 02 00                  	i32.load 2 0\n"
        " 0000f1:	21 03                     	local.set 3\n"
        " 0000f3:	20 00                     	local.get 0\n"
        " 0000f5:	0f                        	return\n"
        " 0000f6:	0b                        	end\n"
        " 0000f9:	08 7f                     	local[3..10] type=i32\n"
        " 0000fb:	20 00                     	local.get 0\n"
        " 0000fd:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%% Test move_to_array_element with IndexLocal variant
move_to_array_element_index_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Base} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, IdxLocal} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:move_to_array_element(State4, 42, {ptr, Base}, IdxLocal, 4),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	28 02 1c                  	i32.load 2 28\n"
        " 0000ee:	21 04                     	local.set 4\n"
        " 0000f0:	20 03                     	local.get 3\n"
        " 0000f2:	20 04                     	local.get 4\n"
        " 0000f4:	41 02                     	i32.const 2\n"
        " 0000f6:	74                        	i32.shl\n"
        " 0000f7:	6a                        	i32.add\n"
        " 0000f8:	41 2a                     	i32.const 42\n"
        " 0000fa:	36 02 10                  	i32.store 2 16\n"
        " 0000fd:	20 00                     	local.get 0\n"
        " 0000ff:	0f                        	return\n"
        " 000100:	0b                        	end\n"
        " 000103:	08 7f                     	local[3..10] type=i32\n"
        " 000105:	20 00                     	local.get 0\n"
        " 000107:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%% Test jump_to_offset
jump_to_offset_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    %% Jump to a known label offset
    Offset1 = ?BACKEND:offset(State2),
    State3 = ?BACKEND:jump_to_offset(State2, Offset1),
    _ = State3,
    %% Jump to unknown offset (fallback path)
    State4 = ?BACKEND:jump_to_offset(State2, 99999),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000ec:	08 7f                     	local[3..10] type=i32\n"
        " 0000ee:	20 01                     	local.get 1\n"
        " 0000f0:	41 a5 c3 01               	i32.const 24997\n"
        " 0000f4:	36 02 04                  	i32.store 2 4\n"
        " 0000f7:	20 00                     	local.get 0\n"
        " 0000f9:	0f                        	return\n"
        " 0000fa:	20 00                     	local.get 0\n"
        " 0000fc:	0f                        	return\n"
        " 0000fd:	0b                        	end\n"
        " 000100:	08 7f                     	local[3..10] type=i32\n"
        " 000102:	20 00                     	local.get 0\n"
        " 000104:	0b                        	end\n"
        " 000107:	08 7f                     	local[3..10] type=i32\n"
        " 000109:	20 00                     	local.get 0\n"
        " 00010b:	0b                        	end\n"
        " 00010e:	08 7f                     	local[3..10] type=i32\n"
        " 000110:	20 00                     	local.get 0\n"
        " 000112:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%% Test jump_to_continuation
jump_to_continuation_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:jump_to_continuation(State3, {free, Reg}),
    ?assertEqual([], ?BACKEND:used_regs(State4)),
    ok.

%% Test reference label
add_label_reference_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    Ref = make_ref(),
    State3 = ?BACKEND:add_label(State2, Ref),
    _Offset = ?BACKEND:offset(State3),
    ok.

call_primitive_with_cp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_primitive_with_cp(State2, 0, [ctx, jit_state]),
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 01                     	local.get 1\n"
        " 0000ea:	28 02 00                  	i32.load 2 0\n"
        " 0000ed:	28 02 00                  	i32.load 2 0\n"
        " 0000f0:	21 03                     	local.set 3\n"
        " 0000f2:	20 03                     	local.get 3\n"
        " 0000f4:	41 18                     	i32.const 24\n"
        " 0000f6:	74                        	i32.shl\n"
        " 0000f7:	21 03                     	local.set 3\n"
        " 0000f9:	41 20                     	i32.const 32\n"
        " 0000fb:	21 04                     	local.set 4\n"
        " 0000fd:	20 03                     	local.get 3\n"
        " 0000ff:	20 04                     	local.get 4\n"
        " 000101:	72                        	i32.or\n"
        " 000102:	21 03                     	local.set 3\n"
        " 000104:	20 00                     	local.get 0\n"
        " 000106:	20 03                     	local.get 3\n"
        " 000108:	36 02 5c                  	i32.store 2 92\n"
        " 00010b:	20 00                     	local.get 0\n"
        " 00010d:	20 01                     	local.get 1\n"
        " 00010f:	20 02                     	local.get 2\n"
        " 000111:	28 02 00                  	i32.load 2 0\n"
        " 000114:	11 01 00                  	call_indirect 0 <env.__indirect_function_table> (type 1)\n"
        " 000117:	0f                        	return\n"
        " 000118:	20 00                     	local.get 0\n"
        " 00011a:	0f                        	return\n"
        " 00011b:	0b                        	end\n"
        " 00011e:	08 7f                     	local[3..10] type=i32\n"
        " 000120:	20 00                     	local.get 0\n"
        " 000122:	0b                        	end\n"
        " 000125:	08 7f                     	local[3..10] type=i32\n"
        " 000127:	20 00                     	local.get 0\n"
        " 000129:	0f                        	return\n"
        " 00012a:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%% Test update_branches with empty branches list
update_branches_empty_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:update_branches(State0),
    ?assertEqual(<<>>, ?BACKEND:stream(State1)).

move_to_vm_register_ptr_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, {ptr, Reg}, {x_reg, 2}),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	28 02 00                  	i32.load 2 0\n"
        " 0000f0:	36 02 20                  	i32.store 2 32\n"
        " 0000f3:	20 00                     	local.get 0\n"
        " 0000f5:	0f                        	return\n"
        " 0000f6:	0b                        	end\n"
        " 0000f9:	08 7f                     	local[3..10] type=i32\n"
        " 0000fb:	20 00                     	local.get 0\n"
        " 0000fd:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

move_to_vm_register_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, {free, Reg}, {x_reg, 2}),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	28 02 18                  	i32.load 2 24\n"
        " 0000e7:	21 03                     	local.set 3\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	20 03                     	local.get 3\n"
        " 0000ed:	36 02 20                  	i32.store 2 32\n"
        " 0000f0:	20 00                     	local.get 0\n"
        " 0000f2:	0f                        	return\n"
        " 0000f3:	0b                        	end\n"
        " 0000f6:	08 7f                     	local[3..10] type=i32\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

%% Test condition with local < other_local
condition_lt_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:if_block(State4, {{free, Reg1}, '<', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 00                     	local.get 0\n"
        " 0000f1:	28 02 1c                  	i32.load 2 28\n"
        " 0000f4:	21 04                     	local.set 4\n"
        " 0000f6:	20 03                     	local.get 3\n"
        " 0000f8:	20 04                     	local.get 4\n"
        " 0000fa:	48                        	i32.lt_s\n"
        " 0000fb:	04 40                     	if\n"
        " 0000fd:	00                        	  unreachable\n"
        " 0000fe:	0b                        	end\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0f                        	return\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end\n"
        " 00010c:	08 7f                     	local[3..10] type=i32\n"
        " 00010e:	20 00                     	local.get 0\n"
        " 000110:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_int_lt_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {0, '<', {free, Reg}}, fun(S) -> ?BACKEND:debugger(S) end),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	41 00                     	i32.const 0\n"
        " 0000f1:	20 03                     	local.get 3\n"
        " 0000f3:	48                        	i32.lt_s\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_ne_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:if_block(State4, {Reg1, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 00                     	local.get 0\n"
        " 0000f1:	28 02 1c                  	i32.load 2 28\n"
        " 0000f4:	21 04                     	local.set 4\n"
        " 0000f6:	20 03                     	local.get 3\n"
        " 0000f8:	20 04                     	local.get 4\n"
        " 0000fa:	47                        	i32.ne\n"
        " 0000fb:	04 40                     	if\n"
        " 0000fd:	00                        	  unreachable\n"
        " 0000fe:	0b                        	end\n"
        " 0000ff:	20 00                     	local.get 0\n"
        " 000101:	0f                        	return\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end\n"
        " 00010c:	08 7f                     	local[3..10] type=i32\n"
        " 00010e:	20 00                     	local.get 0\n"
        " 000110:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

condition_int_cast_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(int)', Reg, '!=', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    State5 = ?BACKEND:return_labels_and_lines(State4, []),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        " 0000e6:	08 7f                     	local[3..10] type=i32\n"
        " 0000e8:	20 00                     	local.get 0\n"
        " 0000ea:	28 02 18                  	i32.load 2 24\n"
        " 0000ed:	21 03                     	local.set 3\n"
        " 0000ef:	20 03                     	local.get 3\n"
        " 0000f1:	41 00                     	i32.const 0\n"
        " 0000f3:	47                        	i32.ne\n"
        " 0000f4:	04 40                     	if\n"
        " 0000f6:	00                        	  unreachable\n"
        " 0000f7:	0b                        	end\n"
        " 0000f8:	20 00                     	local.get 0\n"
        " 0000fa:	0f                        	return\n"
        " 0000fb:	0b                        	end\n"
        " 0000fe:	08 7f                     	local[3..10] type=i32\n"
        " 000100:	20 00                     	local.get 0\n"
        " 000102:	0b                        	end\n"
        " 000105:	08 7f                     	local[3..10] type=i32\n"
        " 000107:	20 00                     	local.get 0\n"
        " 000109:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

call_func_ptr_primitive_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, ResultReg} = ?BACKEND:call_func_ptr(State2, {primitive, 5}, [ctx]),
    ?assertEqual(local3, ResultReg),
    State4 = ?BACKEND:return_labels_and_lines(State3, []),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        " 0000e0:	08 7f                     	local[3..10] type=i32\n"
        " 0000e2:	20 00                     	local.get 0\n"
        " 0000e4:	20 02                     	local.get 2\n"
        " 0000e6:	28 02 14                  	i32.load 2 20\n"
        " 0000e9:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 0000ec:	21 03                     	local.set 3\n"
        " 0000ee:	20 00                     	local.get 0\n"
        " 0000f0:	0f                        	return\n"
        " 0000f1:	0b                        	end\n"
        " 0000f4:	08 7f                     	local[3..10] type=i32\n"
        " 0000f6:	20 00                     	local.get 0\n"
        " 0000f8:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

encode_table_section_test() ->
    Table = <<(jit_wasm32_asm:type_funcref())/binary, 16#00, 0>>,
    Result = jit_wasm32_asm:encode_table_section([Table]),
    ?assertMatch(<<4, _, _/binary>>, Result),
    ok.

alloc_local_overflow_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(65536)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    {State3, R1} = ?BACKEND:call_primitive(State2, 0, [ctx]),
    {State4, R2} = ?BACKEND:call_primitive(State3, 0, [ctx]),
    {State5, R3} = ?BACKEND:call_primitive(State4, 0, [ctx]),
    {State6, R4} = ?BACKEND:call_primitive(State5, 0, [ctx]),
    {State7, R5} = ?BACKEND:call_primitive(State6, 0, [ctx]),
    {State8, R6} = ?BACKEND:call_primitive(State7, 0, [ctx]),
    {State9, R7} = ?BACKEND:call_primitive(State8, 0, [ctx]),
    {State10, R8} = ?BACKEND:call_primitive(State9, 0, [ctx]),
    ?assertEqual(8, length(?BACKEND:used_regs(State10))),
    ?assertEqual([], ?BACKEND:available_regs(State10)),
    {State11, R9} = ?BACKEND:call_primitive(State10, 0, [ctx]),
    ?assertEqual(local11, R9),
    {State12, R10} = ?BACKEND:call_primitive(State11, 0, [ctx]),
    ?assertEqual(local12, R10),
    ?assertEqual(10, length(?BACKEND:used_regs(State12))),
    State13 = ?BACKEND:free_native_registers(
        State12,
        [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
    ),
    ok = ?BACKEND:assert_all_native_free(State13),
    State14 = ?BACKEND:add_label(State13, 2),
    State15 = ?BACKEND:return_labels_and_lines(State14, []),
    Stream = ?BACKEND:stream(State15),
    Dump = <<
        " 0000e7:	0a 7f                     	local[3..12] type=i32\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	0b                        	end\n"
        " 0000ef:	0a 7f                     	local[3..12] type=i32\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	20 02                     	local.get 2\n"
        " 0000f5:	28 02 00                  	i32.load 2 0\n"
        " 0000f8:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 0000fb:	21 03                     	local.set 3\n"
        " 0000fd:	20 00                     	local.get 0\n"
        " 0000ff:	20 02                     	local.get 2\n"
        " 000101:	28 02 00                  	i32.load 2 0\n"
        " 000104:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000107:	21 04                     	local.set 4\n"
        " 000109:	20 00                     	local.get 0\n"
        " 00010b:	20 02                     	local.get 2\n"
        " 00010d:	28 02 00                  	i32.load 2 0\n"
        " 000110:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000113:	21 05                     	local.set 5\n"
        " 000115:	20 00                     	local.get 0\n"
        " 000117:	20 02                     	local.get 2\n"
        " 000119:	28 02 00                  	i32.load 2 0\n"
        " 00011c:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00011f:	21 06                     	local.set 6\n"
        " 000121:	20 00                     	local.get 0\n"
        " 000123:	20 02                     	local.get 2\n"
        " 000125:	28 02 00                  	i32.load 2 0\n"
        " 000128:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00012b:	21 07                     	local.set 7\n"
        " 00012d:	20 00                     	local.get 0\n"
        " 00012f:	20 02                     	local.get 2\n"
        " 000131:	28 02 00                  	i32.load 2 0\n"
        " 000134:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000137:	21 08                     	local.set 8\n"
        " 000139:	20 00                     	local.get 0\n"
        " 00013b:	20 02                     	local.get 2\n"
        " 00013d:	28 02 00                  	i32.load 2 0\n"
        " 000140:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000143:	21 09                     	local.set 9\n"
        " 000145:	20 00                     	local.get 0\n"
        " 000147:	20 02                     	local.get 2\n"
        " 000149:	28 02 00                  	i32.load 2 0\n"
        " 00014c:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00014f:	21 0a                     	local.set 10\n"
        " 000151:	20 00                     	local.get 0\n"
        " 000153:	20 02                     	local.get 2\n"
        " 000155:	28 02 00                  	i32.load 2 0\n"
        " 000158:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00015b:	21 0b                     	local.set 11\n"
        " 00015d:	20 00                     	local.get 0\n"
        " 00015f:	20 02                     	local.get 2\n"
        " 000161:	28 02 00                  	i32.load 2 0\n"
        " 000164:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000167:	21 0c                     	local.set 12\n"
        " 000169:	20 01                     	local.get 1\n"
        " 00016b:	41 03                     	i32.const 3\n"
        " 00016d:	36 02 04                  	i32.store 2 4\n"
        " 000170:	20 00                     	local.get 0\n"
        " 000172:	0f                        	return\n"
        " 000173:	0b                        	end\n"
        " 000176:	0a 7f                     	local[3..12] type=i32\n"
        " 000178:	20 00                     	local.get 0\n"
        " 00017a:	0f                        	return\n"
        " 00017b:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).

alloc_local_overflow_reuse_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(65536)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    {State3, R1} = ?BACKEND:call_primitive(State2, 0, [ctx]),
    {State4, _} = ?BACKEND:call_primitive(State3, 0, [ctx]),
    {State5, _} = ?BACKEND:call_primitive(State4, 0, [ctx]),
    {State6, _} = ?BACKEND:call_primitive(State5, 0, [ctx]),
    {State7, _} = ?BACKEND:call_primitive(State6, 0, [ctx]),
    {State8, _} = ?BACKEND:call_primitive(State7, 0, [ctx]),
    {State9, _} = ?BACKEND:call_primitive(State8, 0, [ctx]),
    {State10, _} = ?BACKEND:call_primitive(State9, 0, [ctx]),
    {State11, R9} = ?BACKEND:call_primitive(State10, 0, [ctx]),
    ?assertEqual(local11, R9),
    State12 = ?BACKEND:free_native_registers(State11, [R1, R9]),
    {State13, Reused1} = ?BACKEND:call_primitive(State12, 0, [ctx]),
    {State14, Reused2} = ?BACKEND:call_primitive(State13, 0, [ctx]),
    ?assert(lists:member(Reused1, [local3, local11])),
    ?assert(lists:member(Reused2, [local3, local11])),
    State15 = ?BACKEND:return_labels_and_lines(State14, []),
    Stream = ?BACKEND:stream(State15),
    Dump = <<
        " 0000e7:	09 7f                     	local[3..11] type=i32\n"
        " 0000e9:	20 00                     	local.get 0\n"
        " 0000eb:	0b                        	end\n"
        " 0000ef:	09 7f                     	local[3..11] type=i32\n"
        " 0000f1:	20 00                     	local.get 0\n"
        " 0000f3:	20 02                     	local.get 2\n"
        " 0000f5:	28 02 00                  	i32.load 2 0\n"
        " 0000f8:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 0000fb:	21 03                     	local.set 3\n"
        " 0000fd:	20 00                     	local.get 0\n"
        " 0000ff:	20 02                     	local.get 2\n"
        " 000101:	28 02 00                  	i32.load 2 0\n"
        " 000104:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000107:	21 04                     	local.set 4\n"
        " 000109:	20 00                     	local.get 0\n"
        " 00010b:	20 02                     	local.get 2\n"
        " 00010d:	28 02 00                  	i32.load 2 0\n"
        " 000110:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000113:	21 05                     	local.set 5\n"
        " 000115:	20 00                     	local.get 0\n"
        " 000117:	20 02                     	local.get 2\n"
        " 000119:	28 02 00                  	i32.load 2 0\n"
        " 00011c:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00011f:	21 06                     	local.set 6\n"
        " 000121:	20 00                     	local.get 0\n"
        " 000123:	20 02                     	local.get 2\n"
        " 000125:	28 02 00                  	i32.load 2 0\n"
        " 000128:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00012b:	21 07                     	local.set 7\n"
        " 00012d:	20 00                     	local.get 0\n"
        " 00012f:	20 02                     	local.get 2\n"
        " 000131:	28 02 00                  	i32.load 2 0\n"
        " 000134:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000137:	21 08                     	local.set 8\n"
        " 000139:	20 00                     	local.get 0\n"
        " 00013b:	20 02                     	local.get 2\n"
        " 00013d:	28 02 00                  	i32.load 2 0\n"
        " 000140:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000143:	21 09                     	local.set 9\n"
        " 000145:	20 00                     	local.get 0\n"
        " 000147:	20 02                     	local.get 2\n"
        " 000149:	28 02 00                  	i32.load 2 0\n"
        " 00014c:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00014f:	21 0a                     	local.set 10\n"
        " 000151:	20 00                     	local.get 0\n"
        " 000153:	20 02                     	local.get 2\n"
        " 000155:	28 02 00                  	i32.load 2 0\n"
        " 000158:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 00015b:	21 0b                     	local.set 11\n"
        " 00015d:	20 00                     	local.get 0\n"
        " 00015f:	20 02                     	local.get 2\n"
        " 000161:	28 02 00                  	i32.load 2 0\n"
        " 000164:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000167:	21 03                     	local.set 3\n"
        " 000169:	20 00                     	local.get 0\n"
        " 00016b:	20 02                     	local.get 2\n"
        " 00016d:	28 02 00                  	i32.load 2 0\n"
        " 000170:	11 00 00                  	call_indirect 0 <env.__indirect_function_table> (type 0)\n"
        " 000173:	21 0b                     	local.set 11\n"
        " 000175:	20 00                     	local.get 0\n"
        " 000177:	0f                        	return\n"
        " 000178:	0b                        	end\n"
        " 00017b:	09 7f                     	local[3..11] type=i32\n"
        " 00017d:	20 00                     	local.get 0\n"
        " 00017f:	0b                        	end"
    >>,
    ?assertStream(wasm32, Dump, Stream).
