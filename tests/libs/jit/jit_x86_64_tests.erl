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

-module(jit_x86_64_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% TODO: fix compilation of tests.
-define(JIT_VARIANT_PIC, 1).

-define(TERM_PRIMARY_MASK, 16#3).
-define(TERM_PRIMARY_BOXED, 16#2).
% ~3
-define(TERM_PRIMARY_CLEAR_MASK, -4).

-define(TERM_IMMED_TAG_MASK, 16#F).
-define(TERM_INTEGER_TAG, 16#F).

-define(TERM_BOXED_TAG_MASK, 16#3F).
-define(TERM_BOXED_POSITIVE_INTEGER, 16#8).

-define(FALSE_ATOM_INDEX, 0).
-define(TRUE_ATOM_INDEX, 1).

-define(FALSE_ATOM, ((?FALSE_ATOM_INDEX bsl 6) bor 16#B)).
-define(TRUE_ATOM, ((?TRUE_ATOM_INDEX bsl 6) bor 16#B)).

-define(PRIM_PUT_LIST, 13).
-define(PRIM_EXTENDED_REGISTER_PTR, 18).

% disassembly obtained with:
% x86_64-elf-objdump -b binary -D dump.bin -M x86-64 -mi386

call_primitive_0_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, rax} = jit_x86_64:call_primitive(State0, 0, [ctx, jit_state]),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "0:   48 8b 02                mov    (%rdx),%rax\n"
            "3:   57                      push   %rdi\n"
            "4:   56                      push   %rsi\n"
            "5:   52                      push   %rdx\n"
            "6:   ff d0                   callq  *%rax\n"
            "8:   5a                      pop    %rdx\n"
            "9:   5e                      pop    %rsi\n"
            "a:   5f                      pop    %rdi\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_1_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, rax} = jit_x86_64:call_primitive(State0, 1, [ctx, jit_state]),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "0:   48 8b 42 08             mov    0x8(%rdx),%rax\n"
            "4:   57                      push   %rdi\n"
            "5:   56                      push   %rsi\n"
            "6:   52                      push   %rdx\n"
            "7:   ff d0                   callq  *%rax\n"
            "9:   5a                      pop    %rdx\n"
            "a:   5e                      pop    %rsi\n"
            "b:   5f                      pop    %rdi\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_2_args_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, rax} = jit_x86_64:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "   0:  48 8b 42 10             mov    0x10(%rdx),%rax\n"
            "   4:  57                      push   %rdi\n"
            "   5:  56                      push   %rsi\n"
            "   6:  52                      push   %rdx\n"
            "   7:  48 c7 c6 2a 00 00 00    mov    $0x2a,%rsi\n"
            "   e:  48 c7 c2 2b 00 00 00    mov    $0x2b,%rdx\n"
            "  15:  48 c7 c1 2c 00 00 00    mov    $0x2c,%rcx\n"
            "  1c:  ff d0                   callq  *%rax\n"
            "  1e:  5a                      pop    %rdx\n"
            "  1f:  5e                      pop    %rsi\n"
            "  20:  5f                      pop    %rdi\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_extended_regs_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, RegA} = jit_x86_64:call_primitive(
        State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State2, RegB} = jit_x86_64:call_primitive(
        State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]
    ),
    {State3, RegC} = jit_x86_64:call_primitive(
        State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State4, ResultReg} = jit_x86_64:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = jit_x86_64:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = jit_x86_64:free_native_register(State5, ResultReg),
    State7 = jit_x86_64:free_native_register(State6, {ptr, RegC}),
    jit_x86_64:assert_all_native_free(State7),
    Stream = jit_x86_64:stream(State7),
    file:write_file("dump.bin", Stream),
    Dump =
        <<
            "   0:	48 8b 82 90 00 00 00 	mov    0x90(%rdx),%rax\n"
            "   7:	57                   	push   %rdi\n"
            "   8:	56                   	push   %rsi\n"
            "   9:	52                   	push   %rdx\n"
            "   a:	48 c7 c6 13 00 00 00 	mov    $0x13,%rsi\n"
            "  11:	ff d0                	callq  *%rax\n"
            "  13:	5a                   	pop    %rdx\n"
            "  14:	5e                   	pop    %rsi\n"
            "  15:	5f                   	pop    %rdi\n"
            "  16:	4c 8b 9a 90 00 00 00 	mov    0x90(%rdx),%r11\n"
            "  1d:	57                   	push   %rdi\n"
            "  1e:	56                   	push   %rsi\n"
            "  1f:	52                   	push   %rdx\n"
            "  20:	50                   	push   %rax\n"
            "  21:	48 c7 c6 14 00 00 00 	mov    $0x14,%rsi\n"
            "  28:	41 ff d3             	callq  *%r11\n"
            "  2b:	49 89 c2             	mov    %rax,%r10\n"
            "  2e:	58                   	pop    %rax\n"
            "  2f:	5a                   	pop    %rdx\n"
            "  30:	5e                   	pop    %rsi\n"
            "  31:	5f                   	pop    %rdi\n"
            "  32:	4c 8b 9a 90 00 00 00 	mov    0x90(%rdx),%r11\n"
            "  39:	57                   	push   %rdi\n"
            "  3a:	56                   	push   %rsi\n"
            "  3b:	52                   	push   %rdx\n"
            "  3c:	41 52                	push   %r10\n"
            "  3e:	50                   	push   %rax\n"
            "  3f:	48 c7 c6 13 00 00 00 	mov    $0x13,%rsi\n"
            "  46:	41 ff d3             	callq  *%r11\n"
            "  49:	49 89 c1             	mov    %rax,%r9\n"
            "  4c:	58                   	pop    %rax\n"
            "  4d:	41 5a                	pop    %r10\n"
            "  4f:	5a                   	pop    %rdx\n"
            "  50:	5e                   	pop    %rsi\n"
            "  51:	5f                   	pop    %rdi\n"
            "  52:	4c 8b 5a 68          	mov    0x68(%rdx),%r11\n"
            "  56:	57                   	push   %rdi\n"
            "  57:	56                   	push   %rsi\n"
            "  58:	52                   	push   %rdx\n"
            "  59:	41 51                	push   %r9\n"
            "  5b:	48 8b 30             	mov    (%rax),%rsi\n"
            "  5e:	49 8b 12             	mov    (%r10),%rdx\n"
            "  61:	41 ff d3             	callq  *%r11\n"
            "  64:	41 59                	pop    %r9\n"
            "  66:	5a                   	pop    %rdx\n"
            "  67:	5e                   	pop    %rsi\n"
            "  68:	5f                   	pop    %rdi\n"
            "  69:	49 89 01             	mov    %rax,(%r9)\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_or_schedule_next_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:call_ext_onlylast_or_schedule_next(State0, 2, 2, -1),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "\n"
            "   0:	ff 4e 10             	decl   0x10(%rsi)\n"
            "   3:	75 11                	jne    0x16\n"
            "   5:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x16\n"
            "   c:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
            "  10:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
            "  14:	ff e0                	jmpq   *%rax\n"
            "  16:	48 8b 42 20          	mov    0x20(%rdx),%rax\n"
            "  1a:	48 c7 c2 02 00 00 00 	mov    $0x2,%rdx\n"
            "  21:	48 c7 c1 02 00 00 00 	mov    $0x2,%rcx\n"
            "  28:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8\n"
            "  2f:	ff e0                	jmpq   *%rax\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_or_schedule_next_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:call_ext_or_schedule_next(State0, 2, 2),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "\n"
            "   0:	48 8b 06             	mov    (%rsi),%rax\n"
            "   3:	8b 00                	mov    (%rax),%eax\n"
            "   5:	48 c1 e0 18          	shl    $0x18,%rax\n"
            "   9:	48 0d 1c 01 00 00    	or     $0x11c,%rax\n"
            "   f:	48 89 87 b8 00 00 00 	mov    %rax,0xb8(%rdi)\n"
            "  16:	ff 4e 10             	decl   0x10(%rsi)\n"
            "  19:	75 11                	jne    0x2c\n"
            "  1b:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x2c\n"
            "  22:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
            "  26:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
            "  2a:	ff e0                	jmpq   *%rax\n"
            "  2c:	48 8b 42 20          	mov    0x20(%rdx),%rax\n"
            "  30:	48 c7 c2 02 00 00 00 	mov    $0x2,%rdx\n"
            "  37:	48 c7 c1 02 00 00 00 	mov    $0x2,%rcx\n"
            "  3e:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8\n"
            "  45:	ff e0                	jmpq   *%rax\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "   0:  48 8b 02                mov    (%rdx),%rax\n"
            "   3:  48 c7 c2 2a 00 00 00    mov    $0x2a,%rdx\n"
            "   a:  ff e0                   jmpq   *%rax\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

move_to_cp_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:move_to_cp(State0, {y_reg, 0}),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
            "   4:	48 8b 00             	mov    (%rax),%rax\n"
            "   7:	48 89 87 b8 00 00 00 	mov    %rax,0xb8(%rdi)\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

increment_sp_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:increment_sp(State0, 7),
    Stream = jit_x86_64:stream(State1),
    Dump =
        <<
            "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
            "   4:	48 83 c0 38          	add    $0x38,%rax\n"
            "   8:	48 89 47 28          	mov    %rax,0x28(%rdi)\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:jump_table(State0, 3),
    Offset1 = jit_x86_64:offset(State1),
    State2 = jit_x86_64:call_only_or_schedule_next(State1, 2),
    Offset2 = jit_x86_64:offset(State2),
    State3 = jit_x86_64:call_primitive_last(State2, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    Offset0 = jit_x86_64:offset(State3),
    State4 = jit_x86_64:call_primitive_last(State3, 1, [ctx, jit_state]),
    State5 = jit_x86_64:update_branches(State4, [{0, Offset0}, {1, Offset1}, {2, Offset2}]),
    Stream = jit_x86_64:stream(State5),
    Dump =
        <<
            "   0:	e9 2a 00 00 00       	jmpq   0x2f\n"
            "   5:	e9 05 00 00 00       	jmpq   0xf\n"
            "   a:	e9 1b 00 00 00       	jmpq   0x2a\n"
            "   f:	ff 4e 10             	decl   0x10(%rsi)\n"
            "  12:	74 05                	je     0x19\n"
            "  14:	e9 11 00 00 00       	jmpq   0x2a\n"
            "  19:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x2a\n"
            "  20:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
            "  24:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
            "  28:	ff e0                	jmpq   *%rax\n"
            "  2a:	48 8b 02             	mov    (%rdx),%rax\n"
            "  2d:	ff e0                	jmpq   *%rax\n"
            "  2f:	48 8b 42 08          	mov    0x8(%rdx),%rax\n"
            "  33:	ff e0                	jmpq   *%rax\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_bif_with_large_literal_integer_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, FuncPtr} = jit_x86_64:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = jit_x86_64:call_primitive(State1, 15, [ctx, 9208452466117618637]),
    {State3, ResultReg} = jit_x86_64:call_func_ptr(State2, {free, FuncPtr}, [
        ctx, 0, 1, {free, {x_reg, 0}}, {free, ArgReg}
    ]),
    State4 = jit_x86_64:handle_error_if_zero(State3, ResultReg),
    State5 = jit_x86_64:move_to_vm_register(State4, ResultReg, {x_reg, 0}),
    State6 = jit_x86_64:free_native_register(State5, ResultReg),
    jit_x86_64:assert_all_native_free(State6),
    Stream = jit_x86_64:stream(State6),
    Dump =
        <<
            "   0:	48 8b 42 40          	mov    0x40(%rdx),%rax\n"
            "   4:	57                   	push   %rdi\n"
            "   5:	56                   	push   %rsi\n"
            "   6:	52                   	push   %rdx\n"
            "   7:	48 89 f7             	mov    %rsi,%rdi\n"
            "   a:	48 c7 c6 02 00 00 00 	mov    $0x2,%rsi\n"
            "  11:	ff d0                	callq  *%rax\n"
            "  13:	5a                   	pop    %rdx\n"
            "  14:	5e                   	pop    %rsi\n"
            "  15:	5f                   	pop    %rdi\n"
            "  16:	4c 8b 5a 78          	mov    0x78(%rdx),%r11\n"
            "  1a:	57                   	push   %rdi\n"
            "  1b:	56                   	push   %rsi\n"
            "  1c:	52                   	push   %rdx\n"
            "  1d:	50                   	push   %rax\n"
            "  1e:	48 be cd ab 02 be ba 	movabs $0x7fcafebabe02abcd,%rsi\n"
            "  25:	fe ca 7f \n"
            "  28:	41 ff d3             	callq  *%r11\n"
            "  2b:	49 89 c2             	mov    %rax,%r10\n"
            "  2e:	58                   	pop    %rax\n"
            "  2f:	5a                   	pop    %rdx\n"
            "  30:	5e                   	pop    %rsi\n"
            "  31:	5f                   	pop    %rdi\n"
            "  32:	57                   	push   %rdi\n"
            "  33:	56                   	push   %rsi\n"
            "  34:	52                   	push   %rdx\n"
            "  35:	48 c7 c6 00 00 00 00 	mov    $0x0,%rsi\n"
            "  3c:	48 c7 c2 01 00 00 00 	mov    $0x1,%rdx\n"
            "  43:	48 8b 4f 30          	mov    0x30(%rdi),%rcx\n"
            "  47:	4d 89 d0             	mov    %r10,%r8\n"
            "  4a:	ff d0                	callq  *%rax\n"
            "  4c:	5a                   	pop    %rdx\n"
            "  4d:	5e                   	pop    %rsi\n"
            "  4e:	5f                   	pop    %rdi\n"
            "  4f:	48 85 c0             	test   %rax,%rax\n"
            "  52:	75 06                	jne    0x5a\n"
            "  54:	48 8b 42 30          	mov    0x30(%rdx),%rax\n"
            "  58:	ff e0                	jmpq   *%rax\n"
            "  5a:	48 89 47 30          	mov    %rax,0x30(%rdi)\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

get_list_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    {State1, Reg} = jit_x86_64:move_to_native_register(State0, {x_reg, 0}),
    State2 = jit_x86_64:and_(State1, Reg, -4),
    State3 = jit_x86_64:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = jit_x86_64:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = jit_x86_64:free_native_register(State4, Reg),
    jit_x86_64:assert_all_native_free(State5),
    Stream = jit_x86_64:stream(State5),
    Dump = <<
        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "   4:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "   8:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
        "   c:	4c 8b 50 08          	mov    0x8(%rax),%r10\n"
        "  10:	4d 89 53 08          	mov    %r10,0x8(%r11)\n"
        "  14:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
        "  18:	4c 8b 10             	mov    (%rax),%r10\n"
        "  1b:	4d 89 13             	mov    %r10,(%r11)\n"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_integer_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    Label = 1,
    {State1, Reg} = jit_x86_64:move_to_native_register(State0, {x_reg, 0}),
    {State2, OffsetRef} = jit_x86_64:jump_to_offset_if_and_equal(
        State1, Reg, ?TERM_IMMED_TAG_MASK, ?TERM_INTEGER_TAG
    ),
    State3 = jit_x86_64:jump_to_label_if_and_not_equal(
        State2, Reg, ?TERM_PRIMARY_MASK, ?TERM_PRIMARY_BOXED, Label
    ),
    State4 = jit_x86_64:and_(State3, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    State5 = jit_x86_64:move_array_element(State4, Reg, 0, Reg),
    State6 = jit_x86_64:jump_to_label_if_and_not_equal(
        State5, {free, Reg}, ?TERM_BOXED_TAG_MASK, ?TERM_BOXED_POSITIVE_INTEGER, Label
    ),
    Offset = jit_x86_64:offset(State6),
    Labels = [{Label, Offset + 16#100}, {OffsetRef, Offset}],
    jit_x86_64:assert_all_native_free(State6),
    State7 = jit_x86_64:update_branches(State6, Labels),
    Stream = jit_x86_64:stream(State7),
    Dump = <<
        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "   4:	49 89 c3             	mov    %rax,%r11\n"
        "   7:	49 83 e3 0f          	and    $0xf,%r11\n"
        "   b:	49 83 fb 0f          	cmp    $0xf,%r11\n"
        "   f:	74 27                	je     0x38\n"
        "  11:	49 89 c3             	mov    %rax,%r11\n"
        "  14:	49 83 e3 03          	and    $0x3,%r11\n"
        "  18:	41 83 fb 02          	cmp    $0x2,%r11d\n"
        "  1c:	74 05                	je     0x23\n"
        "  1e:	e9 15 01 00 00       	jmpq   0x138\n"
        "  23:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "  27:	48 8b 00             	mov    (%rax),%rax\n"
        "  2a:	48 83 e0 3f          	and    $0x3f,%rax\n"
        "  2e:	83 f8 08             	cmp    $0x8,%eax\n"
        "  31:	74 05                	je     0x38\n"
        "  33:	e9 00 01 00 00       	jmpq   0x138\n"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

is_boolean_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    Label = 1,
    {State1, Reg} = jit_x86_64:move_to_native_register(State0, {x_reg, 0}),
    {State2, OffsetRef} = jit_x86_64:jump_to_offset_if_equal(State1, Reg, ?TRUE_ATOM),
    State3 = jit_x86_64:jump_to_label_if_not_equal(State2, Reg, ?FALSE_ATOM, Label),
    State4 = jit_x86_64:free_native_register(State3, Reg),
    Offset = jit_x86_64:offset(State3),
    Labels = [{Label, Offset + 16#100}, {OffsetRef, Offset}],
    jit_x86_64:assert_all_native_free(State4),
    State5 = jit_x86_64:update_branches(State4, Labels),
    Stream = jit_x86_64:stream(State5),
    Dump = <<
        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "   4:	48 83 f8 4b          	cmp    $0x4b,%rax\n"
        "   8:	74 0b                	je     0x15\n"
        "   a:	48 83 f8 0b          	cmp    $0xb,%rax\n"
        "   e:	74 05                	je     0x15\n"
        "  10:	e9 00 01 00 00       	jmpq   0x115\n"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_test() ->
    State0 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new()),
    State1 = jit_x86_64:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = jit_x86_64:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    jit_x86_64:assert_all_native_free(State2),
    Stream = jit_x86_64:stream(State2),
    Dump = <<
"
   0:	ff 4e 10             	decl   0x10(%rsi)
   3:	75 11                	jne    0x16
   5:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x16
   c:	48 89 46 08          	mov    %rax,0x8(%rsi)
  10:	48 8b 42 10          	mov    0x10(%rdx),%rax
  14:	ff e0                	jmpq   *%rax
  16:	48 8b 06             	mov    (%rsi),%rax
  19:	8b 00                	mov    (%rax),%eax
  1b:	48 c1 e0 18          	shl    $0x18,%rax
  1f:	48 0d 1c 01 00 00    	or     $0x11c,%rax
  25:	48 89 87 b8 00 00 00 	mov    %rax,0xb8(%rdi)
  2c:	48 8b 42 20          	mov    0x20(%rdx),%rax
  30:	48 c7 c2 02 00 00 00 	mov    $0x2,%rdx
  37:	48 c7 c1 05 00 00 00 	mov    $0x5,%rcx
  3e:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8
  45:	ff e0                	jmpq   *%rax
"
    >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

dump_to_bin(Dump) ->
    dump_to_bin0(Dump, addr, []).

dump_to_bin0(<<N, Tail/binary>>, addr, Acc) when
    (N >= $0 andalso N =< $9) orelse (N >= $a andalso N =< $f)
->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\ , Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$\t, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<$:, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, pre_hex, Acc);
dump_to_bin0(<<$\ , Tail/binary>>, pre_hex, Acc) ->
    dump_to_bin0(Tail, pre_hex, Acc);
dump_to_bin0(<<$\t, Tail/binary>>, pre_hex, Acc) ->
    dump_to_bin0(Tail, pre_hex, Acc);
dump_to_bin0(<<_Other, _Tail/binary>> = Bin, pre_hex, Acc) ->
    dump_to_bin0(Bin, hex, Acc);
dump_to_bin0(<<D1, D2, $\ , Tail/binary>>, hex, Acc) when
    ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f)) andalso
        ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f))
->
    dump_to_bin0(Tail, hex, [list_to_integer([D1, D2], 16) | Acc]);
dump_to_bin0(<<$\n, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc)).
