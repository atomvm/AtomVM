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

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_x86_64).

% disassembly obtained with:
% x86_64-elf-objdump -b binary -D dump.bin -M x86-64 -mi386

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
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
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
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
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    Stream = ?BACKEND:stream(State1),
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
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:call_primitive(
        State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State2, RegB} = ?BACKEND:call_primitive(
        State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]
    ),
    {State3, RegC} = ?BACKEND:call_primitive(
        State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State4, ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg, {ptr, RegC}]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
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
            "  21:	52                   	push   %rdx\n"
            "  22:	48 c7 c6 14 00 00 00 	mov    $0x14,%rsi\n"
            "  29:	41 ff d3             	callq  *%r11\n"
            "  2c:	5a                   	pop    %rdx\n"
            "  2d:	49 89 c3             	mov    %rax,%r11\n"
            "  30:	58                   	pop    %rax\n"
            "  31:	5a                   	pop    %rdx\n"
            "  32:	5e                   	pop    %rsi\n"
            "  33:	5f                   	pop    %rdi\n"
            "  34:	4c 8b 92 90 00 00 00 	mov    0x90(%rdx),%r10\n"
            "  3b:	57                   	push   %rdi\n"
            "  3c:	56                   	push   %rsi\n"
            "  3d:	52                   	push   %rdx\n"
            "  3e:	41 53                	push   %r11\n"
            "  40:	50                   	push   %rax\n"
            "  41:	48 c7 c6 13 00 00 00 	mov    $0x13,%rsi\n"
            "  48:	41 ff d2             	callq  *%r10\n"
            "  4b:	49 89 c2             	mov    %rax,%r10\n"
            "  4e:	58                   	pop    %rax\n"
            "  4f:	41 5b                	pop    %r11\n"
            "  51:	5a                   	pop    %rdx\n"
            "  52:	5e                   	pop    %rsi\n"
            "  53:	5f                   	pop    %rdi\n"
            "  54:	4c 8b 4a 68          	mov    0x68(%rdx),%r9\n"
            "  58:	57                   	push   %rdi\n"
            "  59:	56                   	push   %rsi\n"
            "  5a:	52                   	push   %rdx\n"
            "  5b:	41 52                	push   %r10\n"
            "  5d:	52                   	push   %rdx\n"
            "  5e:	48 8b 30             	mov    (%rax),%rsi\n"
            "  61:	49 8b 13             	mov    (%r11),%rdx\n"
            "  64:	41 ff d1             	callq  *%r9\n"
            "  67:	5a                   	pop    %rdx\n"
            "  68:	41 5a                	pop    %r10\n"
            "  6a:	5a                   	pop    %rdx\n"
            "  6b:	5e                   	pop    %rsi\n"
            "  6c:	5f                   	pop    %rdi\n"
            "  6d:	49 89 02             	mov    %rax,(%r10)"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_few_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r11} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r10} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r9} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r8} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, rcx} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),

    CreatedBin = rax,
    Offset = r11,
    SrcReg = r8,
    SizeValue = r9,
    FlagsValue = rcx,

    {State7, r8} = ?BACKEND:call_primitive(State6, ?PRIM_BITSTRING_INSERT_INTEGER, [
        CreatedBin, Offset, {free, SrcReg}, SizeValue, {free, FlagsValue}
    ]),
    Stream = ?BACKEND:stream(State7),
    Dump =
        <<
            "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
            "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
            "   8:	4c 8b 57 40          	mov    0x40(%rdi),%r10\n"
            "   c:	4c 8b 4f 48          	mov    0x48(%rdi),%r9\n"
            "  10:	4c 8b 47 50          	mov    0x50(%rdi),%r8\n"
            "  14:	48 8b 4f 58          	mov    0x58(%rdi),%rcx\n"
            "  18:	57                   	push   %rdi\n"
            "  19:	56                   	push   %rsi\n"
            "  1a:	52                   	push   %rdx\n"
            "  1b:	41 51                	push   %r9\n"
            "  1d:	41 52                	push   %r10\n"
            "  1f:	41 53                	push   %r11\n"
            "  21:	50                   	push   %rax\n"
            "  22:	48 8b 92 c8 01 00 00 	mov    0x1c8(%rdx),%rdx\n"
            "  29:	52                   	push   %rdx\n"
            "  2a:	48 89 c7             	mov    %rax,%rdi\n"
            "  2d:	4c 89 de             	mov    %r11,%rsi\n"
            "  30:	4c 89 c2             	mov    %r8,%rdx\n"
            "  33:	4c 87 c9             	xchg   %r9,%rcx\n"
            "  36:	4d 89 c8             	mov    %r9,%r8\n"
            "  39:	58                   	pop    %rax\n"
            "  3a:	ff d0                	callq  *%rax\n"
            "  3c:	49 89 c0             	mov    %rax,%r8\n"
            "  3f:	58                   	pop    %rax\n"
            "  40:	41 5b                	pop    %r11\n"
            "  42:	41 5a                	pop    %r10\n"
            "  44:	41 59                	pop    %r9\n"
            "  46:	5a                   	pop    %rdx\n"
            "  47:	5e                   	pop    %rsi\n"
            "  48:	5f                   	pop    %rdi"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
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

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	ff 4e 10             	decl   0x10(%rsi)\n"
            "   3:	75 11                	jne    0x16\n"
            "   5:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x16\n"
            "   c:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
            "  10:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
            "  14:	ff e0                	jmpq   *%rax\n"
            "  16:	48 8b 42 20          	mov    0x20(%rdx),%rax\n"
            "  1a:	48 c7 c2 02 00 00 00 	mov    $0x2,%rdx\n"
            "  21:	48 c7 c1 02 00 00 00 	mov    $0x2,%rcx\n"
            "  28:	49 c7 c0 0a 00 00 00 	mov    $0xa,%r8\n"
            "  2f:	ff e0                	jmpq   *%rax\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:  48 8b 02                mov    (%rdx),%rax\n"
            "   3:  48 c7 c2 2a 00 00 00    mov    $0x2a,%rdx\n"
            "   a:  ff e0                   jmpq   *%rax\n"
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
                    ?assertEqual(rax, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	48 8b 82 a8 00 00 00 	mov    0xa8(%rdx),%rax\n"
                            "   7:	57                   	push   %rdi\n"
                            "   8:	56                   	push   %rsi\n"
                            "   9:	52                   	push   %rdx\n"
                            "   a:	ff d0                	callq  *%rax\n"
                            "   c:	5a                   	pop    %rdx\n"
                            "   d:	5e                   	pop    %rsi\n"
                            "   e:	5f                   	pop    %rdi\n"
                            "   f:	48 39 f8             	cmp    %rdi,%rax\n"
                            "  12:	74 01                	je     0x15\n"
                            "  14:	c3                   	retq"
                        >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(rax, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(r11, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	48 8b 82 a8 00 00 00 	mov    0xa8(%rdx),%rax\n"
                            "   7:	57                   	push   %rdi\n"
                            "   8:	56                   	push   %rsi\n"
                            "   9:	52                   	push   %rdx\n"
                            "   a:	ff d0                	callq  *%rax\n"
                            "   c:	5a                   	pop    %rdx\n"
                            "   d:	5e                   	pop    %rsi\n"
                            "   e:	5f                   	pop    %rdi\n"
                            "   f:	49 89 c3             	mov    %rax,%r11\n"
                            "  12:	49 39 fb             	cmp    %rdi,%r11\n"
                            "  15:	74 04                	je     0x1b\n"
                            "  17:	4c 89 d8             	mov    %r11,%rax\n"
                            "  1a:	c3                   	retq"
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
            "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
            "   4:	48 8b 00             	mov    (%rax),%rax\n"
            "   7:	48 89 87 b8 00 00 00 	mov    %rax,0xb8(%rdi)\n"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
            "   4:	48 83 c0 38          	add    $0x38,%rax\n"
            "   8:	48 89 47 28          	mov    %rax,0x28(%rdi)\n"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	7d 04                	jge    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	4c 39 d8             	cmp    %r11,%rax\n"
                        "   b:	7d 04                	jge    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	85 c0                	test   %eax,%eax\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	85 c0                	test   %eax,%eax\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	74 04                	je     0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	74 04                	je     0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	74 04                	je     0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	74 04                	je     0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	75 04                	jne    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	75 04                	jne    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	a8 07                	test   $0x7,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	a8 07                	test   $0x7,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	49 89 c2             	mov    %rax,%r10\n"
                        "   b:	41 80 e2 0f          	and    $0xf,%r10b\n"
                        "   f:	41 80 fa 0f          	cmp    $0xf,%r10b\n"
                        "  13:	74 04                	je     0x19\n"
                        "  15:	49 83 c3 02          	add    $0x2,%r11"
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
                        "   8:	24 0f                	and    $0xf,%al\n"
                        "   a:	80 f8 0f             	cmp    $0xf,%al\n"
                        "   d:	74 04                	je     0x13\n"
                        "   f:	49 83 c3 02          	add    $0x2,%r11"
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
            "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
            "   4:	4c 8b 5f 38          	mov    0x38(%rdi),%r11\n"
            "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
            "   c:	75 06                	jne    0x14\n"
            "   e:	49 83 c3 02          	add    $0x2,%r11\n"
            "  12:	eb 04                	jmp    0x18\n"
            "  14:	49 83 c3 04          	add    $0x4,%r11"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_right(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
            "   4:	48 c1 e8 03          	shr    $0x3,%rax"
        >>,
    ?assertEqual(dump_to_bin(Dump), Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
            "   4:	48 c1 e0 03          	shl    $0x3,%rax"
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
            "  1e:	52                   	push   %rdx\n"
            "  1f:	48 be cd ab 02 be ba 	movabs $0x7fcafebabe02abcd,%rsi\n"
            "  26:	fe ca 7f \n"
            "  29:	41 ff d3             	callq  *%r11\n"
            "  2c:	5a                   	pop    %rdx\n"
            "  2d:	49 89 c3             	mov    %rax,%r11\n"
            "  30:	58                   	pop    %rax\n"
            "  31:	5a                   	pop    %rdx\n"
            "  32:	5e                   	pop    %rsi\n"
            "  33:	5f                   	pop    %rdi\n"
            "  34:	57                   	push   %rdi\n"
            "  35:	56                   	push   %rsi\n"
            "  36:	52                   	push   %rdx\n"
            "  37:	48 c7 c6 00 00 00 00 	mov    $0x0,%rsi\n"
            "  3e:	48 c7 c2 01 00 00 00 	mov    $0x1,%rdx\n"
            "  45:	48 8b 4f 30          	mov    0x30(%rdi),%rcx\n"
            "  49:	4d 89 d8             	mov    %r11,%r8\n"
            "  4c:	ff d0                	callq  *%rax\n"
            "  4e:	5a                   	pop    %rdx\n"
            "  4f:	5e                   	pop    %rsi\n"
            "  50:	5f                   	pop    %rdi\n"
            "  51:	48 85 c0             	test   %rax,%rax\n"
            "  54:	75 0d                	jne    0x63\n"
            "  56:	48 8b 42 30          	mov    0x30(%rdx),%rax\n"
            "  5a:	48 c7 c2 5a 00 00 00 	mov    $0x5a,%rdx\n"
            "  61:	ff e0                	jmpq   *%rax\n"
            "  63:	48 89 47 30          	mov    %rax,0x30(%rdi)"
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
        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "   4:	49 89 c3             	mov    %rax,%r11\n"
        "   7:	41 80 e3 0f          	and    $0xf,%r11b\n"
        "   b:	41 80 fb 0f          	cmp    $0xf,%r11b\n"
        "   f:	74 25                	je     0x36\n"
        "  11:	49 89 c3             	mov    %rax,%r11\n"
        "  14:	41 80 e3 03          	and    $0x3,%r11b\n"
        "  18:	41 80 fb 02          	cmp    $0x2,%r11b\n"
        "  1c:	74 05                	je     0x23\n"
        "  1e:	e9 13 01 00 00       	jmpq   0x136\n"
        "  23:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "  27:	48 8b 00             	mov    (%rax),%rax\n"
        "  2a:	24 3f                	and    $0x3f,%al\n"
        "  2c:	80 f8 08             	cmp    $0x8,%al\n"
        "  2f:	74 05                	je     0x36\n"
        "  31:	e9 00 01 00 00       	jmpq   0x136"
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
        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "   4:	49 89 c3             	mov    %rax,%r11\n"
        "   7:	41 80 e3 0f          	and    $0xf,%r11b\n"
        "   b:	41 80 fb 0f          	cmp    $0xf,%r11b\n"
        "   f:	74 32                	je     0x43\n"
        "  11:	49 89 c3             	mov    %rax,%r11\n"
        "  14:	41 80 e3 03          	and    $0x3,%r11b\n"
        "  18:	41 80 fb 02          	cmp    $0x2,%r11b\n"
        "  1c:	74 05                	je     0x23\n"
        "  1e:	e9 20 01 00 00       	jmpq   0x143\n"
        "  23:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "  27:	48 8b 00             	mov    (%rax),%rax\n"
        "  2a:	49 89 c3             	mov    %rax,%r11\n"
        "  2d:	41 80 e3 3f          	and    $0x3f,%r11b\n"
        "  31:	41 80 fb 08          	cmp    $0x8,%r11b\n"
        "  35:	74 0c                	je     0x43\n"
        "  37:	24 3f                	and    $0x3f,%al\n"
        "  39:	80 f8 18             	cmp    $0x18,%al\n"
        "  3c:	74 05                	je     0x43\n"
        "  3e:	e9 00 01 00 00       	jmpq   0x143"
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
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "\n"
        "   0:	ff 4e 10             	decl   0x10(%rsi)\n"
        "   3:	75 11                	jne    0x16\n"
        "   5:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x16\n"
        "   c:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
        "  10:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
        "  14:	ff e0                	jmpq   *%rax\n"
        "  16:	48 8b 06             	mov    (%rsi),%rax\n"
        "  19:	8b 00                	mov    (%rax),%eax\n"
        "  1b:	48 c1 e0 18          	shl    $0x18,%rax\n"
        "  1f:	48 0d 1c 01 00 00    	or     $0x11c,%rax\n"
        "  25:	48 89 87 b8 00 00 00 	mov    %rax,0xb8(%rdi)\n"
        "  2c:	48 8b 42 20          	mov    0x20(%rdx),%rax\n"
        "  30:	48 c7 c2 02 00 00 00 	mov    $0x2,%rdx\n"
        "  37:	48 c7 c1 05 00 00 00 	mov    $0x5,%rcx\n"
        "  3e:	49 c7 c0 ff ff ff ff 	mov    $0xffffffffffffffff,%r8\n"
        "  45:	ff e0                	jmpq   *%rax\n"
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
        "   0:	ff 4e 10             	decl   0x10(%rsi)\n"
        "   3:	75 11                	jne    0x16\n"
        "   5:	48 8d 05 0a 00 00 00 	lea    0xa(%rip),%rax        # 0x16\n"
        "   c:	48 89 46 08          	mov    %rax,0x8(%rsi)\n"
        "  10:	48 8b 42 10          	mov    0x10(%rdx),%rax\n"
        "  14:	ff e0                	jmpq   *%rax\n"
        "  16:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
        "  1a:	49 89 c3             	mov    %rax,%r11\n"
        "  1d:	4d 89 da             	mov    %r11,%r10\n"
        "  20:	41 80 e2 03          	and    $0x3,%r10b\n"
        "  24:	41 80 fa 02          	cmp    $0x2,%r10b\n"
        "  28:	74 1a                	je     0x44\n"
        "  2a:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax\n"
        "  31:	48 c7 c2 31 00 00 00 	mov    $0x31,%rdx\n"
        "  38:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx\n"
        "  3f:	4d 89 d8             	mov    %r11,%r8\n"
        "  42:	ff e0                	jmpq   *%rax\n"
        "  44:	49 83 e3 fc          	and    $0xfffffffffffffffc,%r11\n"
        "  48:	4d 8b 1b             	mov    (%r11),%r11\n"
        "  4b:	4d 89 da             	mov    %r11,%r10\n"
        "  4e:	41 80 e2 3f          	and    $0x3f,%r10b\n"
        "  52:	41 80 fa 14          	cmp    $0x14,%r10b\n"
        "  56:	74 1a                	je     0x72\n"
        "  58:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax\n"
        "  5f:	48 c7 c2 5f 00 00 00 	mov    $0x5f,%rdx\n"
        "  66:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx\n"
        "  6d:	4d 89 d8             	mov    %r11,%r8\n"
        "  70:	ff e0                	jmpq   *%rax\n"
        "  72:	4c 8b 1e             	mov    (%rsi),%r11\n"
        "  75:	45 8b 1b             	mov    (%r11),%r11d\n"
        "  78:	49 c1 e3 18          	shl    $0x18,%r11\n"
        "  7c:	49 81 cb 78 02 00 00 	or     $0x278,%r11\n"
        "  83:	4c 89 9f b8 00 00 00 	mov    %r11,0xb8(%rdi)\n"
        "  8a:	4c 8b 9a 00 01 00 00 	mov    0x100(%rdx),%r11\n"
        "  91:	48 89 c2             	mov    %rax,%rdx\n"
        "  94:	48 c7 c1 00 00 00 00 	mov    $0x0,%rcx\n"
        "  9b:	41 ff e3             	jmpq   *%r11"
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
                        "   0:	48 83 67 30 00       	andq   $0x0,0x30(%rdi)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	48 83 a7 b0 00 00 00 	andq   $0x0,0xb0(%rdi)\n"
                        "   7:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, r10}, <<
                        "   0:	49 83 62 00 00       	andq   $0x0,0x0(%r10)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 83 60 10 00       	andq   $0x0,0x10(%rax)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 83 a0 a0 00 00 00 	andq   $0x0,0xa0(%rax)\n"
                        "   b:	00 "
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	48 c7 47 30 2a 00 00 	movq   $0x2a,0x30(%rdi)\n"
                        "   7:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	48 c7 87 b0 00 00 00 	movq   $0x2a,0xb0(%rdi)\n"
                        "   7:	2a 00 00 00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 c7 40 10 2a 00 00 	movq   $0x2a,0x10(%rax)\n"
                        "   b:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 c7 80 a0 00 00 00 	movq   $0x2a,0xa0(%rax)\n"
                        "   b:	2a 00 00 00 "
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, r10}, <<
                        "   0:	49 c7 42 00 63 00 00 	movq   $0x63,0x0(%r10)\n"
                        "   7:	00 "
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	48 8b 47 38          	mov    0x38(%rdi),%rax\n"
                        "   4:	48 89 47 40          	mov    %rax,0x40(%rdi)"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, r8}, <<
                        "   0:	48 8b 47 38          	mov    0x38(%rdi),%rax\n"
                        "   4:	49 89 00             	mov    %rax,(%r8)"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, r9}, {x_reg, 3}, <<
                        "   0:	49 8b 01             	mov    (%r9),%rax\n"
                        "   3:	48 89 47 48          	mov    %rax,0x48(%rdi)"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
                        "   8:	49 89 43 08          	mov    %rax,0x8(%r11)"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 8b 00             	mov    (%rax),%rax\n"
                        "   7:	48 89 47 48          	mov    %rax,0x48(%rdi)"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 8b 40 08          	mov    0x8(%rax),%rax\n"
                        "   8:	48 89 47 48          	mov    %rax,0x48(%rdi)"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {x_reg, 0}, <<
                        "   0:	48 89 47 30          	mov    %rax,0x30(%rdi)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {x_reg, extra}, <<
                        "   0:	48 89 87 b0 00 00 00 	mov    %rax,0xb0(%rdi)"
                    >>)
                end),
                %% Test: Atom register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {ptr, r10}, <<
                        "   0:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {y_reg, 0}, <<
                        "   0:\t48 8b 47 28           mov    0x28(%rdi),%rax\n"
                        "   4:\t48 89 00              mov    %rax,(%rax)"
                    >>)
                end),
                %% Test: Large immediate to x_reg (movabsq)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, 0}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	48 89 47 30          	mov    %rax,0x30(%rdi)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, extra}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	48 89 87 b0 00 00 00 	mov    %rax,0xb0(%rdi)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 2}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	49 bb f0 de bc 9a 78 	movabs $0x123456789abcdef0,%r11\n"
                        "   b:	56 34 12 \n"
                        "   e:	4c 89 58 10          	mov    %r11,0x10(%rax)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 20}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	49 bb f0 de bc 9a 78 	movabs $0x123456789abcdef0,%r11\n"
                        "   b:	56 34 12 \n"
                        "   e:	4c 89 98 a0 00 00 00 	mov    %r11,0xa0(%rax)"
                    >>)
                end),
                %% Test: Large immediate to ptr (movabsq)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {ptr, r10}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	48 8b 87 a8 00 00 00 	mov    0xa8(%rdi),%rax\n"
                        "   7:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
                        "   b:	49 89 83 f8 00 00 00 	mov    %rax,0xf8(%r11)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, extra}, {y_reg, 31}, <<
                        "   0:	48 8b 87 b0 00 00 00 	mov    0xb0(%rdi),%rax\n"
                        "   7:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
                        "   b:	49 89 83 f8 00 00 00 	mov    %rax,0xf8(%r11)"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	48 8b 80 f8 00 00 00 	mov    0xf8(%rax),%rax\n"
                        "   b:	48 89 87 a8 00 00 00 	mov    %rax,0xa8(%rdi)"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	48 c7 47 30 ff ff ff 	movq   $0xffffffffffffffff,0x30(%rdi)\n"
                        "   7:	ff "
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	48 8b 40 08          	mov    0x8(%rax),%rax\n"
                        "   8:	4c 8b 9f c0 00 00 00 	mov    0xc0(%rdi),%r11\n"
                        "   f:	49 89 43 18          	mov    %rax,0x18(%r11)"
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
                        "   0:	49 8b 40 10          	mov    0x10(%r8),%rax\n"
                        "   4:	48 89 47 30          	mov    %rax,0x30(%rdi)"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r8, 3, {ptr, r10}, <<
                        "   0:	49 8b 40 18          	mov    0x18(%r8),%rax\n"
                        "   4:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, {y_reg, 2}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	4d 8b 58 08          	mov    0x8(%r8),%r11\n"
                        "   8:	4c 89 58 10          	mov    %r11,0x10(%rax)"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (r10)
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, r10, <<
                        "   0:	4d 8b 50 08          	mov    0x8(%r8),%r10"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg (high index)
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {y_reg, 31}, <<
                        "   0:	48 8b 47 28          	mov    0x28(%rdi),%rax\n"
                        "   4:	4d 8b 58 38          	mov    0x38(%r8),%r11\n"
                        "   8:	4c 89 98 f8 00 00 00 	mov    %r11,0xf8(%rax)"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg (high index)
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {x_reg, 15}, <<
                        "   0:	49 8b 40 38          	mov    0x38(%r8),%rax\n"
                        "   4:	48 89 87 a8 00 00 00 	mov    %rax,0xa8(%rdi)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {x_reg, 2}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   8:	4c 01 c0             	add    %r8,%rax\n"
                        "   b:	48 8b 00             	mov    (%rax),%rax\n"
                        "   e:	48 89 47 40          	mov    %rax,0x40(%rdi)\n"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {ptr, r10}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   8:	4c 01 c0             	add    %r8,%rax\n"
                        "   b:	48 8b 00             	mov    (%rax),%rax\n"
                        "   e:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {y_reg, 31}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	4c 8b 5f 28          	mov    0x28(%rdi),%r11\n"
                        "   8:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   c:	4c 01 c0             	add    %r8,%rax\n"
                        "   f:	48 8b 00             	mov    (%rax),%rax\n"
                        "  12:	49 89 83 f8 00 00 00 	mov    %rax,0xf8(%r11)"
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
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream),
                    ?assertEqual(rax, Reg)
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
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	49 89 40 10          	mov    %rax,0x10(%r8)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	48 8b 47 30          	mov    0x30(%rdi),%rax\n"
                        "   4:	49 89 40 18          	mov    %rax,0x18(%r8)"
                    >>,
                    ?assertEqual(dump_to_bin(Dump), Stream)
                end)
            ]
        end}.

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
