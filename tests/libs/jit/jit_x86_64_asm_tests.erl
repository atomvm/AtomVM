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

-module(jit_x86_64_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(x86_64, Bin, Str), Value)
).

movq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#8B, 16#42, 16#30>>,
            "movq 0x30(%rdx), %rax",
            jit_x86_64_asm:movq({16#30, rdx}, rax)
        ),
        ?_assertAsmEqual(<<16#48, 16#89, 16#C0>>, "movq %rax, %rax", jit_x86_64_asm:movq(rax, rax)),
        ?_assertAsmEqual(<<16#49, 16#89, 16#C0>>, "movq %rax, %r8", jit_x86_64_asm:movq(rax, r8)),
        ?_assertAsmEqual(<<16#49, 16#89, 16#C1>>, "movq %rax, %r9", jit_x86_64_asm:movq(rax, r9)),
        ?_assertAsmEqual(<<16#49, 16#89, 16#C2>>, "movq %rax, %r10", jit_x86_64_asm:movq(rax, r10)),
        ?_assertAsmEqual(<<16#49, 16#89, 16#C3>>, "movq %rax, %r11", jit_x86_64_asm:movq(rax, r11)),
        ?_assertAsmEqual(<<16#48, 16#89, 16#C1>>, "movq %rax, %rcx", jit_x86_64_asm:movq(rax, rcx)),
        ?_assertAsmEqual(<<16#48, 16#89, 16#C2>>, "movq %rax, %rdx", jit_x86_64_asm:movq(rax, rdx)),
        ?_assertAsmEqual(<<16#48, 16#89, 16#C6>>, "movq %rax, %rsi", jit_x86_64_asm:movq(rax, rsi)),
        ?_assertAsmEqual(<<16#48, 16#89, 16#C7>>, "movq %rax, %rdi", jit_x86_64_asm:movq(rax, rdi)),

        % movq({0, SrcReg}, DestReg)
        ?_assertAsmEqual(
            <<16#48, 16#8B, 16#02>>, "movq (%rdx), %rax", jit_x86_64_asm:movq({0, rdx}, rax)
        ),
        ?_assertAsmEqual(
            <<16#4C, 16#8B, 16#00>>, "movq (%rax), %r8", jit_x86_64_asm:movq({0, rax}, r8)
        ),

        % movq({Offset, SrcReg}, DestReg) disp8
        ?_assertAsmEqual(
            <<16#48, 16#8B, 16#42, 16#7F>>,
            "movq 0x7f(%rdx), %rax",
            jit_x86_64_asm:movq({127, rdx}, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#8B, 16#42, 16#80>>,
            "movq -0x80(%rdx), %rax",
            jit_x86_64_asm:movq({-128, rdx}, rax)
        ),

        % movq({Offset, SrcReg}, DestReg) disp32
        ?_assertAsmEqual(
            <<16#48, 16#8B, 16#82, 16#78, 16#56, 16#34, 16#12>>,
            "movq 0x12345678(%rdx), %rax",
            jit_x86_64_asm:movq({305419896, rdx}, rax)
        ),

        % movq(DestReg, {0, SrcReg})
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#02>>, "movq %rax, (%rdx)", jit_x86_64_asm:movq(rax, {0, rdx})
        ),
        ?_assertAsmEqual(
            <<16#4C, 16#89, 16#00>>, "movq %r8, (%rax)", jit_x86_64_asm:movq(r8, {0, rax})
        ),

        % movq(DestReg, {Offset, SrcReg}) disp8
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#42, 16#7F>>,
            "movq %rax, 0x7f(%rdx)",
            jit_x86_64_asm:movq(rax, {127, rdx})
        ),
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#42, 16#80>>,
            "movq %rax, -0x80(%rdx)",
            jit_x86_64_asm:movq(rax, {-128, rdx})
        ),

        % movq(DestReg, {Offset, SrcReg}) disp32
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#82, 16#78, 16#56, 16#34, 16#12>>,
            "movq %rax, 0x12345678(%rdx)",
            jit_x86_64_asm:movq(rax, {305419896, rdx})
        ),

        % movq(SrcReg, DestReg) (register to register)
        ?_assertAsmEqual(<<16#48, 16#89, 16#C1>>, "movq %rax, %rcx", jit_x86_64_asm:movq(rax, rcx)),
        ?_assertAsmEqual(<<16#4C, 16#89, 16#C2>>, "movq %r8, %rdx", jit_x86_64_asm:movq(r8, rdx)),

        % movq(Imm, DestReg)
        ?_assertAsmEqual(
            <<16#48, 16#c7, 16#c0, 16#78, 16#56, 16#34, 16#12>>,
            "movq $0x12345678, %rax",
            jit_x86_64_asm:movq(305419896, rax)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#c7, 16#c0, 16#78, 16#56, 16#34, 16#12>>,
            "movq $0x12345678, %r8",
            jit_x86_64_asm:movq(305419896, r8)
        ),

        % movq(Imm, {Offset, DestReg})
        ?_assertAsmEqual(
            <<16#48, 16#c7, 16#42, 16#10, 16#12345678:32/little>>,
            "movq $0x12345678, 0x10(%rdx)",
            jit_x86_64_asm:movq(16#12345678, {16, rdx})
        ),
        % movq $0x6ef, 16#80(%rdi)
        ?_assertAsmEqual(
            <<16#48, 16#c7, 16#87, 16#80, 0, 0, 0, 16#ef, 16#06, 0, 0>>,
            "movq $0x6ef, 0x80(%rdi)",
            jit_x86_64_asm:movq(16#6EF, {16#80, rdi})
        ),
        ?_assertAsmEqual(
            <<72, 137, 4, 10>>,
            "movq %rax,(%rdx,%rcx,1)",
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 1})
        ),
        ?_assertAsmEqual(
            <<72, 137, 4, 74>>,
            "movq %rax,(%rdx,%rcx,2)",
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 2})
        ),
        ?_assertAsmEqual(
            <<72, 137, 4, 138>>,
            "movq %rax,(%rdx,%rcx,4)",
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 4})
        ),
        ?_assertAsmEqual(
            <<72, 137, 4, 202>>,
            "movq %rax,(%rdx,%rcx,8)",
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 8})
        ),
        ?_assertAsmEqual(
            <<72, 137, 132, 74, 128, 0, 0, 0>>,
            "movq %rax,0x80(%rdx,%rcx,2)",
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 2})
        ),
        ?_assertAsmEqual(
            <<72, 137, 132, 138, 128, 0, 0, 0>>,
            "movq %rax,0x80(%rdx,%rcx,4)",
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 4})
        ),
        ?_assertAsmEqual(
            <<72, 137, 132, 202, 128, 0, 0, 0>>,
            "movq %rax,0x80(%rdx,%rcx,8)",
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 8})
        ),
        ?_assertAsmEqual(
            <<16#48, 16#C7, 16#84, 16#0A, 16#80:32/little, 16#42:32/little>>,
            "movq    $0x42,0x80(%rdx,%rcx,1)",
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 1})
        ),
        ?_assertAsmEqual(
            <<72, 199, 132, 74, 128, 0, 0, 0, 66, 0, 0, 0>>,
            "movq $0x42,0x80(%rdx,%rcx,2)",
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 2})
        ),
        ?_assertAsmEqual(
            <<72, 199, 132, 138, 128, 0, 0, 0, 66, 0, 0, 0>>,
            "movq $0x42,0x80(%rdx,%rcx,4)",
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 4})
        ),
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#44, 16#4A, 16#10>>,
            "movq %rax,0x10(%rdx,%rcx,2)",
            jit_x86_64_asm:movq(rax, {16, rdx, rcx, 2})
        ),
        ?_assertAsmEqual(
            <<16#4F, 16#89, 16#44, 16#91, 16#80>>,
            "movq %r8,-0x80(%r9,%r10,4)",
            jit_x86_64_asm:movq(r8, {-128, r9, r10, 4})
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#89, 16#5C, 16#D0, 16#7F>>,
            "movq %r11,0x7f(%r8,%rdx,8)",
            jit_x86_64_asm:movq(r11, {127, r8, rdx, 8})
        ),
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#8C, 16#3E, 16#78, 16#56, 16#34, 16#12>>,
            "movq %rcx,0x12345678(%rsi,%rdi,1)",
            jit_x86_64_asm:movq(rcx, {305419896, rsi, rdi, 1})
        ),
        ?_assertAsmEqual(
            <<72, 199, 68, 200, 24, 95, 0, 0, 0>>,
            "movq $0x5f,0x18(%rax,%rcx,8)",
            jit_x86_64_asm:movq(95, {16#18, rax, rcx, 8})
        ),
        ?_assertAsmEqual(
            <<16#48, 16#89, 16#44, 16#0A, 16#10>>,
            "movq %rax,0x10(%rdx,%rcx,1)",
            jit_x86_64_asm:movq(rax, {16, rdx, rcx, 1})
        ),
        ?_assertAsmEqual(
            <<72, 199, 68, 74, 32, 42, 0, 0, 0>>,
            "movq $0x2a,0x20(%rdx,%rcx,2)",
            jit_x86_64_asm:movq(42, {32, rdx, rcx, 2})
        ),
        ?_assertAsmEqual(
            <<72, 199, 68, 138, 48, 33, 0, 0, 0>>,
            "movq $0x21,0x30(%rdx,%rcx,4)",
            jit_x86_64_asm:movq(33, {48, rdx, rcx, 4})
        ),
        ?_assertAsmEqual(
            <<72, 199, 68, 10, 64, 55, 0, 0, 0>>,
            "movq $0x37,0x40(%rdx,%rcx,1)",
            jit_x86_64_asm:movq(55, {64, rdx, rcx, 1})
        ),
        ?_assertAsmEqual(
            <<72, 199, 132, 202, 128, 0, 0, 0, 153, 0, 0, 0>>,
            "movq $0x99,0x80(%rdx,%rcx,8)",
            jit_x86_64_asm:movq(16#99, {16#80, rdx, rcx, 8})
        )
    ].

movabsq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#B8, 16#12345678:64/little>>,
            "movabsq $0x12345678,%rax",
            jit_x86_64_asm:movabsq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<73, 184, 120, 86, 52, 18, 0, 0, 0, 0>>,
            "movabsq $0x12345678,%r8",
            jit_x86_64_asm:movabsq(16#12345678, r8)
        ),
        ?_assertAsmEqual(
            <<73, 185, 120, 86, 52, 18, 0, 0, 0, 0>>,
            "movabsq $0x12345678,%r9",
            jit_x86_64_asm:movabsq(16#12345678, r9)
        ),
        ?_assertAsmEqual(
            <<73, 186, 120, 86, 52, 18, 0, 0, 0, 0>>,
            "movabsq $0x12345678,%r10",
            jit_x86_64_asm:movabsq(16#12345678, r10)
        ),
        ?_assertAsmEqual(
            <<73, 187, 120, 86, 52, 18, 0, 0, 0, 0>>,
            "movabsq $0x12345678,%r11",
            jit_x86_64_asm:movabsq(16#12345678, r11)
        )
    ].

movl_test_() ->
    [
        ?_assertAsmEqual(<<16#8b, 16#00>>, "movl (%rax),%eax", jit_x86_64_asm:movl({0, rax}, rax)),
        ?_assertAsmEqual(<<16#8b, 16#01>>, "movl (%rcx),%eax", jit_x86_64_asm:movl({0, rcx}, rax)),
        ?_assertAsmEqual(<<16#8b, 16#09>>, "movl (%rcx),%ecx", jit_x86_64_asm:movl({0, rcx}, rcx)),
        ?_assertAsmEqual(
            <<16#44, 16#8B, 16#00>>, "movl (%rax),%r8d", jit_x86_64_asm:movl({0, rax}, r8)
        ),
        ?_assertAsmEqual(
            <<16#44, 16#8B, 16#01>>, "movl (%rcx),%r8d", jit_x86_64_asm:movl({0, rcx}, r8)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#8B, 16#00>>, "movl (%r8),%eax", jit_x86_64_asm:movl({0, r8}, rax)
        ),
        ?_assertAsmEqual(
            <<16#45, 16#8B, 16#01>>, "movl (%r9),%r8d", jit_x86_64_asm:movl({0, r9}, r8)
        )
    ].

shlq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#C1, 16#E0, 16#02>>, "shl $0x2,%rax", jit_x86_64_asm:shlq(2, rax)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#C1, 16#E3, 16#02>>, "shl $0x2,%r11", jit_x86_64_asm:shlq(2, r11)
        )
    ].

shrq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#C1, 16#E8, 16#02>>, "shr $0x2,%rax", jit_x86_64_asm:shrq(2, rax)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#C1, 16#EB, 16#02>>, "shr $0x2,%r11", jit_x86_64_asm:shrq(2, r11)
        )
    ].

testb_test_() ->
    [
        ?_assertAsmEqual(<<16#A8, 16#01>>, "test $0x1,%al", jit_x86_64_asm:testb(1, rax)),
        ?_assertAsmEqual(<<16#84, 16#C0>>, "test %al,%al", jit_x86_64_asm:testb(rax, rax)),
        ?_assertAsmEqual(<<16#84, 16#C0>>, "test %al,%al", jit_x86_64_asm:testb(rax, rax)),
        ?_assertAsmEqual(<<16#84, 16#C9>>, "test %cl,%cl", jit_x86_64_asm:testb(rcx, rcx)),
        ?_assertAsmEqual(<<16#45, 16#84, 16#C0>>, "test %r8b,%r8b", jit_x86_64_asm:testb(r8, r8)),
        ?_assertAsmEqual(<<16#45, 16#84, 16#C9>>, "test %r9b,%r9b", jit_x86_64_asm:testb(r9, r9))
    ].

testb_imm_reg_test_() ->
    [
        % testb imm8, rax (no REX)
        ?_assertAsmEqual(<<16#A8, 16#01>>, "test $0x1,%al", jit_x86_64_asm:testb(1, rax)),
        % testb imm8, rcx (no REX)
        ?_assertAsmEqual(<<16#F6, 16#C1, 16#7F>>, "test $0x7f,%cl", jit_x86_64_asm:testb(127, rcx)),
        % testb imm8, r8 (REX)
        ?_assertAsmEqual(
            <<16#41, 16#F6, 16#C0, 16#01>>, "test $0x1,%r8b", jit_x86_64_asm:testb(1, r8)
        ),
        % testb imm8, r9 (REX)
        ?_assertAsmEqual(
            <<16#41, 16#F6, 16#C1, 16#7F>>, "test $0x7f,%r9b", jit_x86_64_asm:testb(127, r9)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#F6, 16#C1, 16#80>>, "test $0x80,%r9b", jit_x86_64_asm:testb(128, r9)
        )
    ].

testq_test_() ->
    [
        ?_assertAsmEqual(<<16#48, 16#85, 16#C0>>, "test %rax,%rax", jit_x86_64_asm:testq(rax, rax)),
        ?_assertAsmEqual(<<16#48, 16#85, 16#C9>>, "test %rcx,%rcx", jit_x86_64_asm:testq(rcx, rcx)),
        ?_assertAsmEqual(<<16#48, 16#85, 16#D2>>, "test %rdx,%rdx", jit_x86_64_asm:testq(rdx, rdx)),
        ?_assertAsmEqual(<<16#48, 16#85, 16#F6>>, "test %rsi,%rsi", jit_x86_64_asm:testq(rsi, rsi)),
        ?_assertAsmEqual(<<16#48, 16#85, 16#FF>>, "test %rdi,%rdi", jit_x86_64_asm:testq(rdi, rdi)),
        ?_assertAsmEqual(<<16#4D, 16#85, 16#C0>>, "test %r8,%r8", jit_x86_64_asm:testq(r8, r8)),
        ?_assertAsmEqual(<<16#4D, 16#85, 16#C9>>, "test %r9,%r9", jit_x86_64_asm:testq(r9, r9)),

        % testq imm32, rax (special encoding, always 4 bytes)
        ?_assertAsmEqual(
            <<16#48, 16#A9, 1, 0, 0, 0>>, "test $0x1,%rax", jit_x86_64_asm:testq(1, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#A9, 127, 0, 0, 0>>, "test $0x7f,%rax", jit_x86_64_asm:testq(127, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#A9, 128, 255, 255, 255>>, "test $-128,%rax", jit_x86_64_asm:testq(-128, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#A9, 16#78, 16#56, 16#34, 16#12>>,
            "test $0x12345678,%rax",
            jit_x86_64_asm:testq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#A9, 16#88, 16#A9, 16#CB, 16#ED>>,
            "test $-0x12345678,%rax",
            jit_x86_64_asm:testq(-16#12345678, rax)
        ),
        % testq imm32, reg (all others, always 4 bytes)
        ?_assertAsmEqual(
            <<16#48, 16#F7, 16#C1, 127, 0, 0, 0>>, "test $0x7f,%rcx", jit_x86_64_asm:testq(127, rcx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#F7, 16#C2, 128, 255, 255, 255>>,
            "test $-128,%rdx",
            jit_x86_64_asm:testq(-128, rdx)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#F7, 16#C0, 42, 0, 0, 0>>, "test $0x2a,%r8", jit_x86_64_asm:testq(42, r8)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#F7, 16#C1, 16#78, 16#56, 16#34, 16#12>>,
            "test $0x12345678,%r9",
            jit_x86_64_asm:testq(16#12345678, r9)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#F7, 16#C2, 16#88, 16#A9, 16#CB, 16#ED>>,
            "test $-0x12345678,%rdx",
            jit_x86_64_asm:testq(-16#12345678, rdx)
        )
    ].

testl_test_() ->
    [
        ?_assertAsmEqual(<<16#85, 16#C1>>, "testl %eax, %ecx", jit_x86_64_asm:testl(rax, rcx)),
        ?_assertAsmEqual(
            <<16#41, 16#85, 16#C8>>, "testl %ecx, %r8d", jit_x86_64_asm:testl(rcx, r8)
        ),
        ?_assertAsmEqual(
            <<16#44, 16#85, 16#C1>>, "testl %r8d, %ecx", jit_x86_64_asm:testl(r8, rcx)
        ),
        ?_assertAsmEqual(<<16#45, 16#85, 16#C0>>, "testl %r8d, %r8d", jit_x86_64_asm:testl(r8, r8))
    ].

jnz_test_() ->
    [
        ?_assertAsmEqual(<<16#75, 0>>, "jnz .+2", jit_x86_64_asm:jnz(2))
    ].

jnz_rel8_test_() ->
    [
        ?_assertEqual({1, <<16#75, 0>>}, jit_x86_64_asm:jnz_rel8(2))
    ].

jmp_rel32_test_() ->
    [
        % Test relocation placeholder (should produce -4 for relocation mechanism)
        ?_assertEqual({1, <<16#E9, 252, 255, 255, 255>>}, jit_x86_64_asm:jmp_rel32(1)),
        % Test other cases with automatic PC adjustment
        ?_assertEqual({1, <<16#E9, 0:32/little>>}, jit_x86_64_asm:jmp_rel32(5)),
        ?_assertEqual({1, <<16#E9, 246, 255, 255, 255>>}, jit_x86_64_asm:jmp_rel32(-5))
    ].

andq_test_() ->
    [
        ?_assertAsmEqual(<<72, 131, 224, 66>>, "and $0x42,%rax", jit_x86_64_asm:andq(16#42, rax)),
        % Test andq with offset addressing
        ?_assertAsmEqual(
            <<72, 131, 98, 16, 66>>, "andq $0x42,0x10(%rdx)", jit_x86_64_asm:andq(16#42, {16, rdx})
        ),
        ?_assertAsmEqual(
            <<73, 131, 96, 8, 127>>, "andq $0x7f,0x8(%r8)", jit_x86_64_asm:andq(16#7F, {8, r8})
        ),
        ?_assertAsmEqual(
            <<72, 131, 167, 144, 0, 0, 0, 0>>,
            "andq $0x0,0x90(%rdi)",
            jit_x86_64_asm:andq(0, {16#90, rdi})
        )
    ].

andl_test_() ->
    [
        % andl imm8, r32 (no REX)
        ?_assertAsmEqual(<<16#83, 16#E0, 16#01>>, "andl $0x1, %eax", jit_x86_64_asm:andl(1, rax)),
        ?_assertAsmEqual(
            <<16#83, 16#E1, 16#7F>>, "andl $0x7f, %ecx", jit_x86_64_asm:andl(127, rcx)
        ),
        % andl imm8, r32 (REX)
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#E0, 16#01>>, "andl $0x1, %r8d", jit_x86_64_asm:andl(1, r8)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#E1, 16#7F>>, "andl $0x7f, %r9d", jit_x86_64_asm:andl(127, r9)
        ),
        % andl imm32, r32 (no REX)
        ?_assertAsmEqual(
            <<16#25, 16#FF, 16#FF, 16#FF, 16#00>>,
            "andl $0xffffff, %eax",
            jit_x86_64_asm:andl(16#00FFFFFF, rax)
        ),
        ?_assertAsmEqual(
            <<16#81, 16#E1, 16#34, 16#12, 16#00, 16#00>>,
            "andl $0x1234, %ecx",
            jit_x86_64_asm:andl(16#1234, rcx)
        ),
        % andl imm32, r32 (REX)
        ?_assertAsmEqual(
            <<16#41, 16#81, 16#E0, 16#FF, 16#FF, 16#FF, 16#00>>,
            "andl $0xffffff, %r8d",
            jit_x86_64_asm:andl(16#00FFFFFF, r8)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#81, 16#E1, 16#34, 16#12, 16#00, 16#00>>,
            "andl $0x1234, %r9d",
            jit_x86_64_asm:andl(16#1234, r9)
        )
    ].

cmpq_test_() ->
    [
        ?_assertAsmEqual(<<16#48, 16#39, 16#C1>>, "cmpq %rax, %rcx", jit_x86_64_asm:cmpq(rax, rcx)),
        ?_assertAsmEqual(<<16#49, 16#39, 16#C2>>, "cmpq %rax, %r10", jit_x86_64_asm:cmpq(rax, r10)),
        ?_assertAsmEqual(<<16#49, 16#39, 16#C3>>, "cmpq %rax, %r11", jit_x86_64_asm:cmpq(rax, r11)),
        ?_assertAsmEqual(<<16#48, 16#39, 16#C6>>, "cmpq %rax, %rsi", jit_x86_64_asm:cmpq(rax, rsi)),
        ?_assertAsmEqual(<<16#48, 16#39, 16#CE>>, "cmpq %rcx, %rsi", jit_x86_64_asm:cmpq(rcx, rsi)),
        ?_assertAsmEqual(<<16#48, 16#39, 16#C8>>, "cmpq %rcx, %rax", jit_x86_64_asm:cmpq(rcx, rax)),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#F8, 16#01>>, "cmpq $0x1, %rax", jit_x86_64_asm:cmpq(1, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#FE, 16#02>>, "cmpq $0x2, %rsi", jit_x86_64_asm:cmpq(2, rsi)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#FB, 16#02>>, "cmpq $0x2, %r11", jit_x86_64_asm:cmpq(2, r11)
        ),
        % 32-bit immediates
        ?_assertAsmEqual(
            <<16#48, 16#3D, 16#78, 16#56, 16#34, 16#12>>,
            "cmpq $0x12345678,%rax",
            jit_x86_64_asm:cmpq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#81, 16#FE, 16#78, 16#56, 16#34, 16#12>>,
            "cmpq $0x12345678,%rsi",
            jit_x86_64_asm:cmpq(16#12345678, rsi)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#81, 16#FB, 16#78, 16#56, 16#34, 16#12>>,
            "cmpq $0x12345678,%r11",
            jit_x86_64_asm:cmpq(16#12345678, r11)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#3D, 16#88, 16#A9, 16#CB, 16#ED>>,
            "cmpq $-0x12345678,%rax",
            jit_x86_64_asm:cmpq(-16#12345678, rax)
        )
    ].

cmpb_test_() ->
    [
        % cmpb rax, rax
        ?_assertAsmEqual(<<16#38, 16#C0>>, "cmpb %al, %al", jit_x86_64_asm:cmpb(rax, rax)),
        % cmpb rax, rcx
        ?_assertAsmEqual(<<16#38, 16#C1>>, "cmpb %al, %cl", jit_x86_64_asm:cmpb(rax, rcx)),
        % cmpb rcx, rax
        ?_assertAsmEqual(<<16#38, 16#C8>>, "cmpb %cl, %al", jit_x86_64_asm:cmpb(rcx, rax)),
        % cmpb r8, rax (REX prefix)
        ?_assertAsmEqual(<<16#44, 16#38, 16#C0>>, "cmpb %r8b, %al", jit_x86_64_asm:cmpb(r8, rax)),
        % cmpb rax, r8 (REX prefix)
        ?_assertAsmEqual(<<16#41, 16#38, 16#C0>>, "cmpb %al, %r8b", jit_x86_64_asm:cmpb(rax, r8)),
        % cmpb r8, r9 (REX prefix)
        ?_assertAsmEqual(<<16#45, 16#38, 16#C1>>, "cmpb %r8b, %r9b", jit_x86_64_asm:cmpb(r8, r9)),
        % cmpb Imm, Reg
        ?_assertAsmEqual(
            <<16#80, 16#f9, 16#42>>, "cmpb $0x42, %cl", jit_x86_64_asm:cmpb(16#42, rcx)
        ),
        ?_assertAsmEqual(
            <<16#80, 16#f9, 16#ff>>, "cmpb $0xff, %cl", jit_x86_64_asm:cmpb(16#ff, rcx)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#80, 16#f9, 16#42>>, "cmpb $0x42, %r9b", jit_x86_64_asm:cmpb(16#42, r9)
        )
    ].

addq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C0, 16#01>>, "addq $0x1, %rax", jit_x86_64_asm:addq(1, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C1, 16#01>>, "addq $0x1, %rcx", jit_x86_64_asm:addq(1, rcx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C2, 16#01>>, "addq $0x1, %rdx", jit_x86_64_asm:addq(1, rdx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C6, 16#01>>, "addq $0x1, %rsi", jit_x86_64_asm:addq(1, rsi)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C7, 16#01>>, "addq $0x1, %rdi", jit_x86_64_asm:addq(1, rdi)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C0, 16#01>>, "addq $0x1, %r8", jit_x86_64_asm:addq(1, r8)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C1, 16#01>>, "addq $0x1, %r9", jit_x86_64_asm:addq(1, r9)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C2, 16#01>>, "addq $0x1, %r10", jit_x86_64_asm:addq(1, r10)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C3, 16#01>>, "addq $0x1, %r11", jit_x86_64_asm:addq(1, r11)
        ),
        % Register to register
        ?_assertAsmEqual(<<16#48, 16#01, 16#C1>>, "addq %rax, %rcx", jit_x86_64_asm:addq(rax, rcx)),
        ?_assertAsmEqual(<<16#49, 16#01, 16#C2>>, "addq %rax, %r10", jit_x86_64_asm:addq(rax, r10)),
        ?_assertAsmEqual(<<16#49, 16#01, 16#C3>>, "addq %rax, %r11", jit_x86_64_asm:addq(rax, r11)),
        ?_assertAsmEqual(<<16#48, 16#01, 16#C6>>, "addq %rax, %rsi", jit_x86_64_asm:addq(rax, rsi)),
        ?_assertAsmEqual(<<16#48, 16#01, 16#CE>>, "addq %rcx, %rsi", jit_x86_64_asm:addq(rcx, rsi)),
        ?_assertAsmEqual(<<16#48, 16#01, 16#C8>>, "addq %rcx, %rax", jit_x86_64_asm:addq(rcx, rax)),

        % 32-bit immediates
        ?_assertAsmEqual(
            <<16#48, 16#05, 16#78, 16#56, 16#34, 16#12>>,
            "addq $0x12345678,%rax",
            jit_x86_64_asm:addq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#81, 16#C6, 16#78, 16#56, 16#34, 16#12>>,
            "addq $0x12345678,%rsi",
            jit_x86_64_asm:addq(16#12345678, rsi)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#81, 16#C3, 16#78, 16#56, 16#34, 16#12>>,
            "addq $0x12345678,%r11",
            jit_x86_64_asm:addq(16#12345678, r11)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#05, 16#88, 16#A9, 16#CB, 16#ED>>,
            "addq $-0x12345678,%rax",
            jit_x86_64_asm:addq(-16#12345678, rax)
        )
    ].

imulq_test_() ->
    [
        % imulq imm8, reg
        ?_assertAsmEqual(
            <<16#48, 16#6B, 16#C0, 16#05>>, "imulq $0x5, %rax", jit_x86_64_asm:imulq(5, rax)
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#6B, 16#D2, 16#7F>>, "imulq $0x7f, %r10", jit_x86_64_asm:imulq(127, r10)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#6B, 16#C9, 16#80>>, "imulq $-128, %rcx", jit_x86_64_asm:imulq(-128, rcx)
        ),
        % imulq imm32, reg
        ?_assertAsmEqual(
            <<16#48, 16#69, 16#C0, 16#78, 16#56, 16#34, 16#12>>,
            "imulq $0x12345678,%rax",
            jit_x86_64_asm:imulq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#69, 16#D2, 16#78, 16#56, 16#34, 16#12>>,
            "imulq $0x12345678,%r10",
            jit_x86_64_asm:imulq(16#12345678, r10)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#69, 16#C9, 16#88, 16#A9, 16#CB, 16#ED>>,
            "imulq $-0x12345678,%rcx",
            jit_x86_64_asm:imulq(-16#12345678, rcx)
        ),
        % imulq reg, reg (register to register)
        ?_assertAsmEqual(
            <<16#48, 16#0F, 16#AF, 16#C1>>, "imul %rcx,%rax", jit_x86_64_asm:imulq(rcx, rax)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#0F, 16#AF, 16#C2>>, "imul %r10,%rax", jit_x86_64_asm:imulq(r10, rax)
        ),
        ?_assertAsmEqual(
            <<16#4C, 16#0F, 16#AF, 16#D1>>, "imul %rcx,%r10", jit_x86_64_asm:imulq(rcx, r10)
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#0F, 16#AF, 16#D2>>, "imul %r10,%r10", jit_x86_64_asm:imulq(r10, r10)
        )
    ].

cmpl_test_() ->
    [
        ?_assertAsmEqual(
            <<16#83, 16#F8, 16#42>>, "cmp $0x42,%eax", jit_x86_64_asm:cmpl(16#42, rax)
        ),
        ?_assertAsmEqual(
            <<16#83, 16#F9, 16#42>>, "cmp $0x42,%ecx", jit_x86_64_asm:cmpl(16#42, rcx)
        ),
        ?_assertAsmEqual(
            <<16#83, 16#FA, 16#42>>, "cmp $0x42,%edx", jit_x86_64_asm:cmpl(16#42, rdx)
        ),
        ?_assertAsmEqual(
            <<16#83, 16#FE, 16#42>>, "cmp $0x42,%esi", jit_x86_64_asm:cmpl(16#42, rsi)
        ),
        ?_assertAsmEqual(
            <<16#83, 16#FF, 16#42>>, "cmp $0x42,%edi", jit_x86_64_asm:cmpl(16#42, rdi)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#F8, 16#42>>, "cmp $0x42,%r8d", jit_x86_64_asm:cmpl(16#42, r8)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#F9, 16#42>>, "cmp $0x42,%r9d", jit_x86_64_asm:cmpl(16#42, r9)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#FA, 16#42>>, "cmp $0x42,%r10d", jit_x86_64_asm:cmpl(16#42, r10)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#83, 16#FB, 16#42>>, "cmp $0x42,%r11d", jit_x86_64_asm:cmpl(16#42, r11)
        )
    ].

orq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C8, 16#01>>, "orq $0x1,%rax", jit_x86_64_asm:orq(1, rax)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C8, 16#42>>, "orq $0x42,%r8", jit_x86_64_asm:orq(16#42, r8)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#C9, 16#42>>, "orq $0x42,%r9", jit_x86_64_asm:orq(16#42, r9)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#CA, 16#42>>, "orq $0x42,%r10", jit_x86_64_asm:orq(16#42, r10)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#83, 16#CB, 16#42>>, "orq $0x42,%r11", jit_x86_64_asm:orq(16#42, r11)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#C9, 16#42>>, "orq $0x42,%rcx", jit_x86_64_asm:orq(16#42, rcx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#CA, 16#42>>, "orq $0x42,%rdx", jit_x86_64_asm:orq(16#42, rdx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#CE, 16#42>>, "orq $0x42,%rsi", jit_x86_64_asm:orq(16#42, rsi)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#83, 16#CF, 16#42>>, "orq $0x42,%rdi", jit_x86_64_asm:orq(16#42, rdi)
        ),
        % Test for orq with a 32-bit immediate value
        ?_assertAsmEqual(
            <<16#48, 16#0D, 16#12345678:32/little>>,
            "orq $0x12345678,%rax",
            jit_x86_64_asm:orq(16#12345678, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#81, 16#C9, 16#12345678:32/little>>,
            "orq $0x12345678,%rcx",
            jit_x86_64_asm:orq(16#12345678, rcx)
        ),
        ?_assertAsmEqual(
            <<16#49, 16#81, 16#C8, 16#12345678:32/little>>,
            "orq $0x12345678,%r8",
            jit_x86_64_asm:orq(16#12345678, r8)
        )
    ].

pushq_test_() ->
    [
        ?_assertAsmEqual(<<16#50>>, "push %rax", jit_x86_64_asm:pushq(rax)),
        ?_assertAsmEqual(<<16#41, 16#50>>, "push %r8", jit_x86_64_asm:pushq(r8))
    ].

popq_test_() ->
    [
        ?_assertAsmEqual(<<16#58>>, "pop %rax", jit_x86_64_asm:popq(rax)),
        ?_assertAsmEqual(<<16#41, 16#58>>, "pop %r8", jit_x86_64_asm:popq(r8))
    ].

callq_test_() ->
    [
        ?_assertAsmEqual(<<16#FF, 16#D0>>, "call *%rax", jit_x86_64_asm:callq({rax})),
        ?_assertAsmEqual(<<16#FF, 16#D1>>, "call *%rcx", jit_x86_64_asm:callq({rcx})),
        ?_assertAsmEqual(<<16#FF, 16#D2>>, "call *%rdx", jit_x86_64_asm:callq({rdx})),
        ?_assertAsmEqual(<<16#FF, 16#D6>>, "call *%rsi", jit_x86_64_asm:callq({rsi})),
        ?_assertAsmEqual(<<16#FF, 16#D7>>, "call *%rdi", jit_x86_64_asm:callq({rdi})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#D0>>, "call *%r8", jit_x86_64_asm:callq({r8})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#D1>>, "call *%r9", jit_x86_64_asm:callq({r9})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#D2>>, "call *%r10", jit_x86_64_asm:callq({r10})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#D3>>, "call *%r11", jit_x86_64_asm:callq({r11}))
    ].

jmpq_test_() ->
    [
        ?_assertAsmEqual(<<16#FF, 16#E0>>, "jmp *%rax", jit_x86_64_asm:jmpq({rax})),
        ?_assertAsmEqual(<<16#FF, 16#E1>>, "jmp *%rcx", jit_x86_64_asm:jmpq({rcx})),
        ?_assertAsmEqual(<<16#FF, 16#E2>>, "jmp *%rdx", jit_x86_64_asm:jmpq({rdx})),
        ?_assertAsmEqual(<<16#FF, 16#E6>>, "jmp *%rsi", jit_x86_64_asm:jmpq({rsi})),
        ?_assertAsmEqual(<<16#FF, 16#E7>>, "jmp *%rdi", jit_x86_64_asm:jmpq({rdi})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#E0>>, "jmp *%r8", jit_x86_64_asm:jmpq({r8})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#E1>>, "jmp *%r9", jit_x86_64_asm:jmpq({r9})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#E2>>, "jmp *%r10", jit_x86_64_asm:jmpq({r10})),
        ?_assertAsmEqual(<<16#41, 16#FF, 16#E3>>, "jmp *%r11", jit_x86_64_asm:jmpq({r11}))
    ].

leaq_rel32_test_() ->
    [
        ?_assertEqual(
            {3, <<16#48, 16#8D, 16#0D, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, rcx)
        ),
        ?_assertEqual(
            {3, <<16#4C, 16#8D, 16#05, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, r8)
        ),
        ?_assertEqual(
            {3, <<16#4C, 16#8D, 16#1D, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, r11)
        )
    ].

leaq_test_() ->
    [
        % leaq with disp8, low registers
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#47, 16#10>>,
            "leaq 0x10(%rdi),%rax",
            jit_x86_64_asm:leaq({16, rdi}, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#41, 16#20>>,
            "leaq 0x20(%rcx),%rax",
            jit_x86_64_asm:leaq({32, rcx}, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#71, 16#7F>>,
            "leaq 0x7f(%rcx),%rsi",
            jit_x86_64_asm:leaq({127, rcx}, rsi)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#77, 16#80>>,
            "leaq -0x80(%rdi),%rsi",
            jit_x86_64_asm:leaq({-128, rdi}, rsi)
        ),

        % leaq with disp8, high registers
        ?_assertAsmEqual(
            <<16#4C, 16#8D, 16#47, 16#10>>,
            "leaq 0x10(%rdi),%r8",
            jit_x86_64_asm:leaq({16, rdi}, r8)
        ),
        ?_assertAsmEqual(
            <<16#4C, 16#8D, 16#49, 16#20>>,
            "leaq 0x20(%rcx),%r9",
            jit_x86_64_asm:leaq({32, rcx}, r9)
        ),

        % leaq with disp32, low registers
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#87, 16#78, 16#56, 16#34, 16#12>>,
            "leaq 0x12345678(%rdi),%rax",
            jit_x86_64_asm:leaq({305419896, rdi}, rax)
        ),
        % leaq with disp32, high registers
        ?_assertAsmEqual(
            <<16#4C, 16#8D, 16#87, 16#78, 16#56, 16#34, 16#12>>,
            "leaq 0x12345678(%rdi),%r8",
            jit_x86_64_asm:leaq({305419896, rdi}, r8)
        ),
        % leaq with RIP-relative addressing (low registers)
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#05, 16#00, 16#00, 16#00, 16#00>>,
            "leaq 0x0(%rip),%rax",
            jit_x86_64_asm:leaq({rip, 0}, rax)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#0D, 16#34, 16#12, 16#00, 16#00>>,
            "leaq 0x1234(%rip),%rcx",
            jit_x86_64_asm:leaq({rip, 16#1234}, rcx)
        ),
        % leaq with RIP-relative addressing (high registers)
        ?_assertAsmEqual(
            <<16#4C, 16#8D, 16#05, 16#78, 16#56, 16#34, 16#12>>,
            "leaq 0x12345678(%rip),%r8",
            jit_x86_64_asm:leaq({rip, 16#12345678}, r8)
        ),
        % leaq with RIP-relative addressing (negative offset)
        ?_assertAsmEqual(
            <<16#48, 16#8D, 16#0D, 16#FC, 16#FF, 16#FF, 16#FF>>,
            "leaq -0x4(%rip),%rcx",
            jit_x86_64_asm:leaq({rip, -4}, rcx)
        )
    ].

jz_test_() ->
    [
        ?_assertAsmEqual(<<16#74, 16#fc>>, "jz .-2", jit_x86_64_asm:jz(-2)),
        ?_assertAsmEqual(<<16#74, 16#80>>, "jz .-126", jit_x86_64_asm:jz(-126)),
        ?_assertAsmEqual(<<16#74, 16#7f>>, "jz .+129", jit_x86_64_asm:jz(129))
    ].

jz_rel8_test_() ->
    [
        ?_assertEqual(
            {1, jit_tests_common:asm(x86_64, <<16#74, 16#fc>>, "jz .-2")},
            jit_x86_64_asm:jz_rel8(-2)
        )
    ].

jge_test_() ->
    [
        ?_assertAsmEqual(<<16#7d, 16#f4>>, "jge .-10", jit_x86_64_asm:jge(-10))
    ].

jge_rel8_test_() ->
    [
        ?_assertEqual(
            {1, jit_tests_common:asm(x86_64, <<16#7d, 16#05>>, "jge .+7")},
            jit_x86_64_asm:jge_rel8(7)
        )
    ].

jmp_rel8_test_() ->
    [
        ?_assertEqual(
            {1, jit_tests_common:asm(x86_64, <<16#eb, 16#05>>, "jmp .+7")},
            jit_x86_64_asm:jmp_rel8(7)
        )
    ].

jmp_test_() ->
    [
        % Test short jump (8-bit)
        ?_assertAsmEqual(<<16#eb, 16#05>>, "jmp .+7", jit_x86_64_asm:jmp(7)),
        % Test boundary cases for short vs near jump selection

        % Last short jump (positive)
        ?_assertAsmEqual(<<16#eb, 16#7f>>, "jmp .+129", jit_x86_64_asm:jmp(129)),
        % First near jump (positive)
        ?_assertAsmEqual(
            <<16#e9, 16#7d, 16#00, 16#00, 16#00>>, "jmp .+130", jit_x86_64_asm:jmp(130)
        ),
        % Last short jump (negative)
        ?_assertAsmEqual(<<16#eb, 16#80>>, "jmp .-126", jit_x86_64_asm:jmp(-126)),
        % First near jump (negative)
        ?_assertAsmEqual(
            <<16#e9, 16#7c, 16#ff, 16#ff, 16#ff>>, "jmp .-127", jit_x86_64_asm:jmp(-127)
        )
    ].

andb_test_() ->
    [
        ?_assertAsmEqual(<<16#24, 16#01>>, "andb $0x1, %al", jit_x86_64_asm:andb(1, rax)),
        ?_assertAsmEqual(<<16#24, 16#80>>, "andb $0x80, %al", jit_x86_64_asm:andb(128, rax)),
        ?_assertAsmEqual(<<16#24, 16#ff>>, "andb $0xff, %al", jit_x86_64_asm:andb(-1, rax)),
        ?_assertAsmEqual(<<16#24, 16#ff>>, "andb $0xff, %al", jit_x86_64_asm:andb(255, rax)),
        ?_assertAsmEqual(<<16#80, 16#e1, 16#7f>>, "andb $0x7f, %cl", jit_x86_64_asm:andb(127, rcx)),
        ?_assertAsmEqual(<<16#80, 16#e1, 16#80>>, "andb $0x80, %cl", jit_x86_64_asm:andb(128, rcx)),
        ?_assertAsmEqual(<<16#80, 16#e1, 16#ff>>, "andb $0xff, %cl", jit_x86_64_asm:andb(-1, rcx)),
        ?_assertAsmEqual(<<16#80, 16#e1, 16#ff>>, "andb $0xff, %cl", jit_x86_64_asm:andb(255, rcx)),
        ?_assertAsmEqual(
            <<16#41, 16#80, 16#e0, 16#01>>, "andb $0x1, %r8b", jit_x86_64_asm:andb(1, r8)
        ),
        ?_assertAsmEqual(
            <<16#41, 16#80, 16#e1, 16#7f>>, "andb $0x7f, %r9b", jit_x86_64_asm:andb(127, r9)
        )
    ].

subq_test_() ->
    [
        ?_assertAsmEqual(<<16#48, 16#29, 16#c1>>, "subq %rax, %rcx", jit_x86_64_asm:subq(rax, rcx)),
        ?_assertAsmEqual(<<16#49, 16#29, 16#c2>>, "subq %rax, %r10", jit_x86_64_asm:subq(rax, r10)),
        ?_assertAsmEqual(<<16#4c, 16#29, 16#c1>>, "subq %r8, %rcx", jit_x86_64_asm:subq(r8, rcx))
    ].

decl_test_() ->
    [
        ?_assertAsmEqual(<<16#ff, 16#4e, 16#10>>, "decl 0x10(%rsi)", jit_x86_64_asm:decl({16, rsi}))
    ].

orq_rel32_test_() ->
    [
        ?_assertEqual(
            {2,
                jit_tests_common:asm(
                    x86_64, <<16#48, 16#0d, 16#78, 16#56, 16#34, 16#12>>, "orq $0x12345678, %rax"
                )},
            jit_x86_64_asm:orq_rel32(16#12345678, rax)
        ),
        ?_assertEqual(
            {3,
                jit_tests_common:asm(
                    x86_64,
                    <<16#48, 16#81, 16#c9, 16#78, 16#56, 16#34, 16#12>>,
                    "orq $0x12345678, %rcx"
                )},
            jit_x86_64_asm:orq_rel32(16#12345678, rcx)
        ),
        ?_assertEqual(
            {3,
                jit_tests_common:asm(
                    x86_64,
                    <<16#49, 16#81, 16#c8, 16#78, 16#56, 16#34, 16#12>>,
                    "orq $0x12345678, %r8"
                )},
            jit_x86_64_asm:orq_rel32(16#12345678, r8)
        )
    ].

retq_test_() ->
    [
        ?_assertAsmEqual(<<16#c3>>, "retq", jit_x86_64_asm:retq())
    ].

xchgq_test_() ->
    [
        ?_assertAsmEqual(<<16#90>>, "xchg %rax,%rax", jit_x86_64_asm:xchgq(rax, rax)),
        ?_assertAsmEqual(<<16#48, 16#91>>, "xchg %rax,%rcx", jit_x86_64_asm:xchgq(rax, rcx)),
        ?_assertAsmEqual(<<16#48, 16#92>>, "xchg %rax,%rdx", jit_x86_64_asm:xchgq(rax, rdx)),
        ?_assertAsmEqual(<<16#48, 16#96>>, "xchg %rax,%rsi", jit_x86_64_asm:xchgq(rax, rsi)),
        ?_assertAsmEqual(<<16#48, 16#97>>, "xchg %rax,%rdi", jit_x86_64_asm:xchgq(rax, rdi)),
        ?_assertAsmEqual(<<16#49, 16#90>>, "xchg %rax,%r8", jit_x86_64_asm:xchgq(rax, r8)),
        ?_assertAsmEqual(<<16#49, 16#91>>, "xchg %rax,%r9", jit_x86_64_asm:xchgq(rax, r9)),
        ?_assertAsmEqual(<<16#49, 16#92>>, "xchg %rax,%r10", jit_x86_64_asm:xchgq(rax, r10)),
        ?_assertAsmEqual(<<16#49, 16#93>>, "xchg %rax,%r11", jit_x86_64_asm:xchgq(rax, r11)),

        % xchg reg, rax - commutative, should use same short encoding
        ?_assertAsmEqual(<<16#48, 16#91>>, "xchg %rcx,%rax", jit_x86_64_asm:xchgq(rcx, rax)),
        ?_assertAsmEqual(<<16#48, 16#92>>, "xchg %rdx,%rax", jit_x86_64_asm:xchgq(rdx, rax)),
        ?_assertAsmEqual(<<16#49, 16#91>>, "xchg %r9,%rax", jit_x86_64_asm:xchgq(r9, rax)),

        % xchg reg, reg - general form (REX.W + 0x87 /r)
        ?_assertAsmEqual(
            <<16#48, 16#87, 16#D1>>, "xchg %rdx,%rcx", jit_x86_64_asm:xchgq(rdx, rcx)
        ),
        ?_assertAsmEqual(
            <<16#48, 16#87, 16#F2>>, "xchg %rsi,%rdx", jit_x86_64_asm:xchgq(rsi, rdx)
        ),
        ?_assertAsmEqual(
            <<16#4C, 16#87, 16#C1>>, "xchg %r8,%rcx", jit_x86_64_asm:xchgq(r8, rcx)
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#87, 16#C8>>, "xchg %r9,%r8", jit_x86_64_asm:xchgq(r9, r8)
        ),
        ?_assertAsmEqual(
            <<16#4D, 16#87, 16#D1>>, "xchg %r10,%r9", jit_x86_64_asm:xchgq(r10, r9)
        )
    ].
