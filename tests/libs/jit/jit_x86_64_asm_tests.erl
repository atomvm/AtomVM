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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

movq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#8B, 16#42, 16#30>>, jit_x86_64_asm:movq({16#30, rdx}, rax)),
        ?_assertEqual(<<16#48, 16#89, 16#C0>>, jit_x86_64_asm:movq(rax, rax)),
        ?_assertEqual(<<16#49, 16#89, 16#C0>>, jit_x86_64_asm:movq(rax, r8)),
        ?_assertEqual(<<16#49, 16#89, 16#C1>>, jit_x86_64_asm:movq(rax, r9)),
        ?_assertEqual(<<16#49, 16#89, 16#C2>>, jit_x86_64_asm:movq(rax, r10)),
        ?_assertEqual(<<16#49, 16#89, 16#C3>>, jit_x86_64_asm:movq(rax, r11)),
        ?_assertEqual(<<16#48, 16#89, 16#C1>>, jit_x86_64_asm:movq(rax, rcx)),
        ?_assertEqual(<<16#48, 16#89, 16#C2>>, jit_x86_64_asm:movq(rax, rdx)),
        ?_assertEqual(<<16#48, 16#89, 16#C6>>, jit_x86_64_asm:movq(rax, rsi)),
        ?_assertEqual(<<16#48, 16#89, 16#C7>>, jit_x86_64_asm:movq(rax, rdi)),

        % movq({0, SrcReg}, DestReg)
        ?_assertEqual(<<16#48, 16#8B, 16#02>>, jit_x86_64_asm:movq({0, rdx}, rax)),
        ?_assertEqual(<<16#4C, 16#8B, 16#00>>, jit_x86_64_asm:movq({0, rax}, r8)),

        % movq({Offset, SrcReg}, DestReg) disp8
        ?_assertEqual(<<16#48, 16#8B, 16#42, 16#7F>>, jit_x86_64_asm:movq({127, rdx}, rax)),
        ?_assertEqual(<<16#48, 16#8B, 16#42, 16#80>>, jit_x86_64_asm:movq({-128, rdx}, rax)),

        % movq({Offset, SrcReg}, DestReg) disp32
        ?_assertEqual(
            <<16#48, 16#8B, 16#82, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:movq({305419896, rdx}, rax)
        ),

        % movq(DestReg, {0, SrcReg})
        ?_assertEqual(<<16#48, 16#89, 16#02>>, jit_x86_64_asm:movq(rax, {0, rdx})),
        ?_assertEqual(<<16#4C, 16#89, 16#00>>, jit_x86_64_asm:movq(r8, {0, rax})),

        % movq(DestReg, {Offset, SrcReg}) disp8
        ?_assertEqual(<<16#48, 16#89, 16#42, 16#7F>>, jit_x86_64_asm:movq(rax, {127, rdx})),
        ?_assertEqual(<<16#48, 16#89, 16#42, 16#80>>, jit_x86_64_asm:movq(rax, {-128, rdx})),

        % movq(DestReg, {Offset, SrcReg}) disp32
        ?_assertEqual(
            <<16#48, 16#89, 16#82, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:movq(rax, {305419896, rdx})
        ),

        % movq(SrcReg, DestReg) (register to register)
        ?_assertEqual(<<16#48, 16#89, 16#C1>>, jit_x86_64_asm:movq(rax, rcx)),
        ?_assertEqual(<<16#4C, 16#89, 16#C2>>, jit_x86_64_asm:movq(r8, rdx)),

        % movq(Imm, DestReg)
        ?_assertEqual(
            <<16#48, 16#c7, 16#c0, 16#78, 16#56, 16#34, 16#12>>, jit_x86_64_asm:movq(305419896, rax)
        ),
        ?_assertEqual(
            <<16#49, 16#c7, 16#c0, 16#78, 16#56, 16#34, 16#12>>, jit_x86_64_asm:movq(305419896, r8)
        ),

        % movq(Imm, {Offset, DestReg})
        ?_assertEqual(
            <<16#48, 16#c7, 16#42, 16#10, 16#12345678:32/little>>,
            jit_x86_64_asm:movq(16#12345678, {16, rdx})
        ),
        % movq $0x6ef, 16#80(%rdi)
        ?_assertEqual(
            <<16#48, 16#c7, 16#87, 16#80, 0, 0, 0, 16#ef, 16#06, 0, 0>>,
            jit_x86_64_asm:movq(16#6EF, {16#80, rdi})
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:   48 89 04 0a  mov     %rax,0x0(%rdx,%rcx,1)">>),
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 1})
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:   48 89 04 4a  mov     %rax,0x0(%rdx,%rcx,2)">>),
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 2})
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:   48 89 04 8a  mov     %rax,0x0(%rdx,%rcx,4)">>),
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 4})
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:   48 89 04 ca  mov     %rax,0x0(%rdx,%rcx,8)">>),
            jit_x86_64_asm:movq(rax, {0, rdx, rcx, 8})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 89 84 4a 80 00 00    mov     %rax,0x80(%rdx,%rcx,2)\n"
                    "7:  00"
                >>
            ),
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 2})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 89 84 8a 80 00 00    mov     %rax,0x80(%rdx,%rcx,4)\n"
                    "7:  00"
                >>
            ),
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 4})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 89 84 ca 80 00 00    mov     %rax,0x80(%rdx,%rcx,8)\n"
                    "7:  00"
                >>
            ),
            jit_x86_64_asm:movq(rax, {128, rdx, rcx, 8})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:	 48 c7 84 0a 80 00 00    movq    $0x42,0x80(%rdx,%rcx,1)\n"
                    "7:  00 42 00 00 00"
                >>
            ),
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 1})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 c7 84 4a 80 00 00    movq    $0x42,0x80(%rdx,%rcx,2)\n"
                    "7:  00 42 00 00 00"
                >>
            ),
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 2})
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 c7 84 8a 80 00 00    movq    $0x42,0x80(%rdx,%rcx,4)\n"
                    "7:  00 42 00 00 00"
                >>
            ),
            jit_x86_64_asm:movq(16#42, {128, rdx, rcx, 4})
        )
    ].

movabsq_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  48 b8 78 56 34 12 00 	movabs $0x12345678,%rax\n"
                    "7:  00 00 00"
                >>
            ),
            jit_x86_64_asm:movabsq(16#12345678, rax)
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  49 b8 78 56 34 12 00 	movabs $0x12345678,%r8\n"
                    "7:  00 00 00"
                >>
            ),
            jit_x86_64_asm:movabsq(16#12345678, r8)
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  49 b9 78 56 34 12 00 	movabs $0x12345678,%r9\n"
                    "7:  00 00 00"
                >>
            ),
            jit_x86_64_asm:movabsq(16#12345678, r9)
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  49 ba 78 56 34 12 00 	movabs $0x12345678,%r10\n"
                    "7:  00 00 00"
                >>
            ),
            jit_x86_64_asm:movabsq(16#12345678, r10)
        ),
        ?_assertEqual(
            dump_to_bin(
                <<
                    "0:  49 bb 78 56 34 12 00 	movabs $0x12345678,%r11\n"
                    "7:  00 00 00"
                >>
            ),
            jit_x86_64_asm:movabsq(16#12345678, r11)
        )
    ].

movl_test_() ->
    [
        ?_assertEqual(<<16#8b, 16#00>>, jit_x86_64_asm:movl({0, rax}, rax)),
        ?_assertEqual(<<16#8b, 16#01>>, jit_x86_64_asm:movl({0, rcx}, rax)),
        ?_assertEqual(<<16#8b, 16#09>>, jit_x86_64_asm:movl({0, rcx}, rcx)),
        ?_assertEqual(<<16#44, 16#8B, 16#00>>, jit_x86_64_asm:movl({0, rax}, r8)),
        ?_assertEqual(<<16#44, 16#8B, 16#01>>, jit_x86_64_asm:movl({0, rcx}, r8)),
        ?_assertEqual(<<16#41, 16#8B, 16#00>>, jit_x86_64_asm:movl({0, r8}, rax)),
        ?_assertEqual(<<16#45, 16#8B, 16#01>>, jit_x86_64_asm:movl({0, r9}, r8))
    ].

shlq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#C1, 16#E0, 16#02>>, jit_x86_64_asm:shlq(2, rax)),
        ?_assertEqual(<<16#49, 16#C1, 16#E3, 16#02>>, jit_x86_64_asm:shlq(2, r11))
    ].

shrq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#C1, 16#E8, 16#02>>, jit_x86_64_asm:shrq(2, rax)),
        ?_assertEqual(<<16#49, 16#C1, 16#EB, 16#02>>, jit_x86_64_asm:shrq(2, r11))
    ].

testb_test_() ->
    [
        ?_assertEqual(<<16#A8, 16#01>>, jit_x86_64_asm:testb(1, rax)),
        ?_assertEqual(<<16#84, 16#C0>>, jit_x86_64_asm:testb(rax, rax)),
        ?_assertEqual(<<16#84, 16#C0>>, jit_x86_64_asm:testb(rax, rax)),
        ?_assertEqual(<<16#84, 16#C9>>, jit_x86_64_asm:testb(rcx, rcx)),
        ?_assertEqual(<<16#45, 16#84, 16#C0>>, jit_x86_64_asm:testb(r8, r8)),
        ?_assertEqual(<<16#45, 16#84, 16#C9>>, jit_x86_64_asm:testb(r9, r9))
    ].

testb_imm_reg_test_() ->
    [
        % testb imm8, rax (no REX)
        ?_assertEqual(<<16#A8, 16#01>>, jit_x86_64_asm:testb(1, rax)),
        % testb imm8, rcx (no REX)
        ?_assertEqual(<<16#F6, 16#C1, 16#7F>>, jit_x86_64_asm:testb(127, rcx)),
        % testb imm8, r8 (REX)
        ?_assertEqual(<<16#41, 16#F6, 16#C0, 16#01>>, jit_x86_64_asm:testb(1, r8)),
        % testb imm8, r9 (REX)
        ?_assertEqual(<<16#41, 16#F6, 16#C1, 16#7F>>, jit_x86_64_asm:testb(127, r9)),
        ?_assertEqual(<<16#41, 16#F6, 16#C1, 16#80>>, jit_x86_64_asm:testb(128, r9))
    ].

testq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#85, 16#C0>>, jit_x86_64_asm:testq(rax, rax)),
        ?_assertEqual(<<16#48, 16#85, 16#C9>>, jit_x86_64_asm:testq(rcx, rcx)),
        ?_assertEqual(<<16#48, 16#85, 16#D2>>, jit_x86_64_asm:testq(rdx, rdx)),
        ?_assertEqual(<<16#48, 16#85, 16#F6>>, jit_x86_64_asm:testq(rsi, rsi)),
        ?_assertEqual(<<16#48, 16#85, 16#FF>>, jit_x86_64_asm:testq(rdi, rdi)),
        ?_assertEqual(<<16#4D, 16#85, 16#C0>>, jit_x86_64_asm:testq(r8, r8)),
        ?_assertEqual(<<16#4D, 16#85, 16#C9>>, jit_x86_64_asm:testq(r9, r9)),

        % testq imm32, rax (special encoding, always 4 bytes)
        ?_assertEqual(<<16#48, 16#A9, 1, 0, 0, 0>>, jit_x86_64_asm:testq(1, rax)),
        ?_assertEqual(<<16#48, 16#A9, 127, 0, 0, 0>>, jit_x86_64_asm:testq(127, rax)),
        ?_assertEqual(<<16#48, 16#A9, 128, 255, 255, 255>>, jit_x86_64_asm:testq(-128, rax)),
        ?_assertEqual(
            <<16#48, 16#A9, 16#78, 16#56, 16#34, 16#12>>, jit_x86_64_asm:testq(16#12345678, rax)
        ),
        ?_assertEqual(
            <<16#48, 16#A9, 16#88, 16#A9, 16#CB, 16#ED>>, jit_x86_64_asm:testq(-16#12345678, rax)
        ),
        % testq imm32, reg (all others, always 4 bytes)
        ?_assertEqual(<<16#48, 16#F7, 16#C1, 127, 0, 0, 0>>, jit_x86_64_asm:testq(127, rcx)),
        ?_assertEqual(<<16#48, 16#F7, 16#C2, 128, 255, 255, 255>>, jit_x86_64_asm:testq(-128, rdx)),
        ?_assertEqual(<<16#49, 16#F7, 16#C0, 42, 0, 0, 0>>, jit_x86_64_asm:testq(42, r8)),
        ?_assertEqual(
            <<16#49, 16#F7, 16#C1, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:testq(16#12345678, r9)
        ),
        ?_assertEqual(
            <<16#48, 16#F7, 16#C2, 16#88, 16#A9, 16#CB, 16#ED>>,
            jit_x86_64_asm:testq(-16#12345678, rdx)
        )
    ].

testl_test_() ->
    [
        ?_assertEqual(<<16#85, 16#C1>>, jit_x86_64_asm:testl(rax, rcx)),
        ?_assertEqual(<<16#41, 16#85, 16#C8>>, jit_x86_64_asm:testl(rcx, r8)),
        ?_assertEqual(<<16#44, 16#85, 16#C1>>, jit_x86_64_asm:testl(r8, rcx)),
        ?_assertEqual(<<16#45, 16#85, 16#C0>>, jit_x86_64_asm:testl(r8, r8))
    ].

jnz_test_() ->
    [
        ?_assertEqual(<<16#75, 0>>, jit_x86_64_asm:jnz(0))
    ].

jnz_rel8_test_() ->
    [
        ?_assertEqual({1, <<16#75, -4>>}, jit_x86_64_asm:jnz_rel8(-4))
    ].

jmp_rel32_test_() ->
    [
        ?_assertEqual({1, <<16#E9, 5:32/little>>}, jit_x86_64_asm:jmp_rel32(5))
    ].

andq_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(<<"0:   48 83 e0 42     and     $0x42,%rax">>),
            jit_x86_64_asm:andq(16#42, rax)
        ),
        % Test andq with offset addressing
        ?_assertEqual(
            dump_to_bin(<<"0:   48 83 62 10 42  andq    $0x42,0x10(%rdx)">>),
            jit_x86_64_asm:andq(16#42, {16, rdx})
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:   49 83 60 08 7f  andq    $0x7f,0x8(%r8)">>),
            jit_x86_64_asm:andq(16#7F, {8, r8})
        ),
        ?_assertEqual(
            dump_to_bin(<<
                "   0:	48 83 a7 90 00 00 00 	andq   $0x0,0x90(%rdi)\n"
                "   7:	00"
            >>),
            jit_x86_64_asm:andq(0, {16#90, rdi})
        )
    ].

andl_test_() ->
    [
        % andl imm8, r32 (no REX)
        ?_assertEqual(<<16#83, 16#E0, 16#01>>, jit_x86_64_asm:andl(1, rax)),
        ?_assertEqual(<<16#83, 16#E1, 16#7F>>, jit_x86_64_asm:andl(127, rcx)),
        % andl imm8, r32 (REX)
        ?_assertEqual(<<16#41, 16#83, 16#E0, 16#01>>, jit_x86_64_asm:andl(1, r8)),
        ?_assertEqual(<<16#41, 16#83, 16#E1, 16#7F>>, jit_x86_64_asm:andl(127, r9))
    ].

cmpq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#39, 16#C1>>, jit_x86_64_asm:cmpq(rax, rcx)),
        ?_assertEqual(<<16#49, 16#39, 16#C2>>, jit_x86_64_asm:cmpq(rax, r10)),
        ?_assertEqual(<<16#49, 16#39, 16#C3>>, jit_x86_64_asm:cmpq(rax, r11)),
        ?_assertEqual(<<16#48, 16#39, 16#C6>>, jit_x86_64_asm:cmpq(rax, rsi)),
        ?_assertEqual(<<16#48, 16#39, 16#CE>>, jit_x86_64_asm:cmpq(rcx, rsi)),
        ?_assertEqual(<<16#48, 16#39, 16#C8>>, jit_x86_64_asm:cmpq(rcx, rax)),
        ?_assertEqual(<<16#48, 16#83, 16#F8, 16#01>>, jit_x86_64_asm:cmpq(1, rax)),
        ?_assertEqual(<<16#48, 16#83, 16#FE, 16#02>>, jit_x86_64_asm:cmpq(2, rsi)),
        ?_assertEqual(<<16#49, 16#83, 16#FB, 16#02>>, jit_x86_64_asm:cmpq(2, r11)),
        % 32-bit immediates
        ?_assertEqual(
            <<16#48, 16#81, 16#F8, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:cmpq(16#12345678, rax)
        ),
        ?_assertEqual(
            <<16#48, 16#81, 16#FE, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:cmpq(16#12345678, rsi)
        ),
        ?_assertEqual(
            <<16#49, 16#81, 16#FB, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:cmpq(16#12345678, r11)
        ),
        ?_assertEqual(
            <<16#48, 16#81, 16#F8, 16#88, 16#A9, 16#CB, 16#ED>>,
            jit_x86_64_asm:cmpq(-16#12345678, rax)
        )
    ].

cmpb_test_() ->
    [
        % cmpb rax, rax
        ?_assertEqual(<<16#38, 16#C0>>, jit_x86_64_asm:cmpb(rax, rax)),
        % cmpb rax, rcx
        ?_assertEqual(<<16#38, 16#C1>>, jit_x86_64_asm:cmpb(rax, rcx)),
        % cmpb rcx, rax
        ?_assertEqual(<<16#38, 16#C8>>, jit_x86_64_asm:cmpb(rcx, rax)),
        % cmpb r8, rax (REX prefix)
        ?_assertEqual(<<16#44, 16#38, 16#C0>>, jit_x86_64_asm:cmpb(r8, rax)),
        % cmpb rax, r8 (REX prefix)
        ?_assertEqual(<<16#41, 16#38, 16#C0>>, jit_x86_64_asm:cmpb(rax, r8)),
        % cmpb r8, r9 (REX prefix)
        ?_assertEqual(<<16#45, 16#38, 16#C1>>, jit_x86_64_asm:cmpb(r8, r9)),
        % cmpb Imm, Reg
        ?_assertEqual(
            dump_to_bin(<<"0:  80 f9 42     cmp     $0x42,%cl">>), jit_x86_64_asm:cmpb(16#42, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:  80 f9 ff     cmp     $0xff,%cl">>), jit_x86_64_asm:cmpb(16#ff, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0:  41 80 f9 42  cmp     $0x42,%r9b">>), jit_x86_64_asm:cmpb(16#42, r9)
        )
    ].

addq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#83, 16#C0, 16#01>>, jit_x86_64_asm:addq(1, rax)),
        ?_assertEqual(<<16#48, 16#83, 16#C1, 16#01>>, jit_x86_64_asm:addq(1, rcx)),
        ?_assertEqual(<<16#48, 16#83, 16#C2, 16#01>>, jit_x86_64_asm:addq(1, rdx)),
        ?_assertEqual(<<16#48, 16#83, 16#C6, 16#01>>, jit_x86_64_asm:addq(1, rsi)),
        ?_assertEqual(<<16#48, 16#83, 16#C7, 16#01>>, jit_x86_64_asm:addq(1, rdi)),
        ?_assertEqual(<<16#49, 16#83, 16#C0, 16#01>>, jit_x86_64_asm:addq(1, r8)),
        ?_assertEqual(<<16#49, 16#83, 16#C1, 16#01>>, jit_x86_64_asm:addq(1, r9)),
        ?_assertEqual(<<16#49, 16#83, 16#C2, 16#01>>, jit_x86_64_asm:addq(1, r10)),
        ?_assertEqual(<<16#49, 16#83, 16#C3, 16#01>>, jit_x86_64_asm:addq(1, r11)),
        % Register to register
        ?_assertEqual(<<16#48, 16#01, 16#C1>>, jit_x86_64_asm:addq(rax, rcx)),
        ?_assertEqual(<<16#49, 16#01, 16#C2>>, jit_x86_64_asm:addq(rax, r10)),
        ?_assertEqual(<<16#49, 16#01, 16#C3>>, jit_x86_64_asm:addq(rax, r11)),
        ?_assertEqual(<<16#48, 16#01, 16#C6>>, jit_x86_64_asm:addq(rax, rsi)),
        ?_assertEqual(<<16#48, 16#01, 16#CE>>, jit_x86_64_asm:addq(rcx, rsi)),
        ?_assertEqual(<<16#48, 16#01, 16#C8>>, jit_x86_64_asm:addq(rcx, rax)),

        % 32-bit immediates
        ?_assertEqual(
            <<16#48, 16#81, 16#C0, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:addq(16#12345678, rax)
        ),
        ?_assertEqual(
            <<16#48, 16#81, 16#C6, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:addq(16#12345678, rsi)
        ),
        ?_assertEqual(
            <<16#49, 16#81, 16#C3, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:addq(16#12345678, r11)
        ),
        ?_assertEqual(
            <<16#48, 16#81, 16#C0, 16#88, 16#A9, 16#CB, 16#ED>>,
            jit_x86_64_asm:addq(-16#12345678, rax)
        )
    ].

imulq_test_() ->
    [
        % imulq imm8, reg
        ?_assertEqual(<<16#48, 16#6B, 16#C0, 16#05>>, jit_x86_64_asm:imulq(5, rax)),
        ?_assertEqual(<<16#4D, 16#6B, 16#D2, 16#7F>>, jit_x86_64_asm:imulq(127, r10)),
        ?_assertEqual(<<16#48, 16#6B, 16#C9, 16#80>>, jit_x86_64_asm:imulq(-128, rcx)),
        % imulq imm32, reg
        ?_assertEqual(
            <<16#48, 16#69, 16#C0, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:imulq(16#12345678, rax)
        ),
        ?_assertEqual(
            <<16#4D, 16#69, 16#D2, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:imulq(16#12345678, r10)
        ),
        ?_assertEqual(
            <<16#48, 16#69, 16#C9, 16#88, 16#A9, 16#CB, 16#ED>>,
            jit_x86_64_asm:imulq(-16#12345678, rcx)
        ),
        % imulq reg, reg (register to register)
        ?_assertEqual(<<16#48, 16#0F, 16#AF, 16#C1>>, jit_x86_64_asm:imulq(rcx, rax)),
        ?_assertEqual(<<16#49, 16#0F, 16#AF, 16#C2>>, jit_x86_64_asm:imulq(r10, rax)),
        ?_assertEqual(<<16#4C, 16#0F, 16#AF, 16#D1>>, jit_x86_64_asm:imulq(rcx, r10)),
        ?_assertEqual(<<16#4D, 16#0F, 16#AF, 16#D2>>, jit_x86_64_asm:imulq(r10, r10))
    ].

cmpl_test_() ->
    [
        ?_assertEqual(<<16#83, 16#F8, 16#42>>, jit_x86_64_asm:cmpl(16#42, rax)),
        ?_assertEqual(<<16#83, 16#F9, 16#42>>, jit_x86_64_asm:cmpl(16#42, rcx)),
        ?_assertEqual(<<16#83, 16#FA, 16#42>>, jit_x86_64_asm:cmpl(16#42, rdx)),
        ?_assertEqual(<<16#83, 16#FE, 16#42>>, jit_x86_64_asm:cmpl(16#42, rsi)),
        ?_assertEqual(<<16#83, 16#FF, 16#42>>, jit_x86_64_asm:cmpl(16#42, rdi)),
        ?_assertEqual(<<16#41, 16#83, 16#F8, 16#42>>, jit_x86_64_asm:cmpl(16#42, r8)),
        ?_assertEqual(<<16#41, 16#83, 16#F9, 16#42>>, jit_x86_64_asm:cmpl(16#42, r9)),
        ?_assertEqual(<<16#41, 16#83, 16#FA, 16#42>>, jit_x86_64_asm:cmpl(16#42, r10)),
        ?_assertEqual(<<16#41, 16#83, 16#FB, 16#42>>, jit_x86_64_asm:cmpl(16#42, r11))
    ].

orq_test_() ->
    [
        ?_assertEqual(<<16#48, 16#83, 16#C8, 16#01>>, jit_x86_64_asm:orq(1, rax)),
        ?_assertEqual(<<16#49, 16#83, 16#C8, 16#42>>, jit_x86_64_asm:orq(16#42, r8)),
        ?_assertEqual(<<16#49, 16#83, 16#C9, 16#42>>, jit_x86_64_asm:orq(16#42, r9)),
        ?_assertEqual(<<16#49, 16#83, 16#CA, 16#42>>, jit_x86_64_asm:orq(16#42, r10)),
        ?_assertEqual(<<16#49, 16#83, 16#CB, 16#42>>, jit_x86_64_asm:orq(16#42, r11)),
        ?_assertEqual(<<16#48, 16#83, 16#C9, 16#42>>, jit_x86_64_asm:orq(16#42, rcx)),
        ?_assertEqual(<<16#48, 16#83, 16#CA, 16#42>>, jit_x86_64_asm:orq(16#42, rdx)),
        ?_assertEqual(<<16#48, 16#83, 16#CE, 16#42>>, jit_x86_64_asm:orq(16#42, rsi)),
        ?_assertEqual(<<16#48, 16#83, 16#CF, 16#42>>, jit_x86_64_asm:orq(16#42, rdi)),
        % Test for orq with a 32-bit immediate value
        ?_assertEqual(
            <<16#48, 16#0D, 16#12345678:32/little>>, jit_x86_64_asm:orq(16#12345678, rax)
        ),
        ?_assertEqual(
            <<16#48, 16#81, 16#C9, 16#12345678:32/little>>, jit_x86_64_asm:orq(16#12345678, rcx)
        ),
        ?_assertEqual(
            <<16#49, 16#81, 16#C8, 16#12345678:32/little>>, jit_x86_64_asm:orq(16#12345678, r8)
        )
    ].

pushq_test_() ->
    [
        ?_assertEqual(<<16#50>>, jit_x86_64_asm:pushq(rax)),
        ?_assertEqual(<<16#41, 16#50>>, jit_x86_64_asm:pushq(r8))
    ].

popq_test_() ->
    [
        ?_assertEqual(<<16#58>>, jit_x86_64_asm:popq(rax)),
        ?_assertEqual(<<16#41, 16#58>>, jit_x86_64_asm:popq(r8))
    ].

callq_test_() ->
    [
        ?_assertEqual(<<16#FF, 16#D0>>, jit_x86_64_asm:callq({rax})),
        ?_assertEqual(<<16#FF, 16#D1>>, jit_x86_64_asm:callq({rcx})),
        ?_assertEqual(<<16#FF, 16#D2>>, jit_x86_64_asm:callq({rdx})),
        ?_assertEqual(<<16#FF, 16#D6>>, jit_x86_64_asm:callq({rsi})),
        ?_assertEqual(<<16#FF, 16#D7>>, jit_x86_64_asm:callq({rdi})),
        ?_assertEqual(<<16#41, 16#FF, 16#D0>>, jit_x86_64_asm:callq({r8})),
        ?_assertEqual(<<16#41, 16#FF, 16#D1>>, jit_x86_64_asm:callq({r9})),
        ?_assertEqual(<<16#41, 16#FF, 16#D2>>, jit_x86_64_asm:callq({r10})),
        ?_assertEqual(<<16#41, 16#FF, 16#D3>>, jit_x86_64_asm:callq({r11}))
    ].

jmpq_test_() ->
    [
        ?_assertEqual(<<16#FF, 16#E0>>, jit_x86_64_asm:jmpq({rax})),
        ?_assertEqual(<<16#FF, 16#E1>>, jit_x86_64_asm:jmpq({rcx})),
        ?_assertEqual(<<16#FF, 16#E2>>, jit_x86_64_asm:jmpq({rdx})),
        ?_assertEqual(<<16#FF, 16#E6>>, jit_x86_64_asm:jmpq({rsi})),
        ?_assertEqual(<<16#FF, 16#E7>>, jit_x86_64_asm:jmpq({rdi})),
        ?_assertEqual(<<16#41, 16#FF, 16#E0>>, jit_x86_64_asm:jmpq({r8})),
        ?_assertEqual(<<16#41, 16#FF, 16#E1>>, jit_x86_64_asm:jmpq({r9})),
        ?_assertEqual(<<16#41, 16#FF, 16#E2>>, jit_x86_64_asm:jmpq({r10})),
        ?_assertEqual(<<16#41, 16#FF, 16#E3>>, jit_x86_64_asm:jmpq({r11}))
    ].

leaq_rel32_test_() ->
    [
        ?_assertEqual(
            {3, <<16#48, 16#8D, 16#0D, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, rcx)
        ),
        ?_assertEqual(
            {3, <<16#49, 16#8D, 16#05, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, r8)
        ),
        ?_assertEqual(
            {3, <<16#49, 16#8D, 16#1D, 16#10, 0, 0, 0>>},
            jit_x86_64_asm:leaq_rel32({16, rip}, r11)
        )
    ].

leaq_test_() ->
    [
        % leaq with disp8, low registers
        ?_assertEqual(<<16#48, 16#8D, 16#47, 16#10>>, jit_x86_64_asm:leaq({16, rdi}, rax)),
        ?_assertEqual(<<16#48, 16#8D, 16#41, 16#20>>, jit_x86_64_asm:leaq({32, rcx}, rax)),
        ?_assertEqual(<<16#48, 16#8D, 16#71, 16#7F>>, jit_x86_64_asm:leaq({127, rcx}, rsi)),
        ?_assertEqual(<<16#48, 16#8D, 16#77, 16#80>>, jit_x86_64_asm:leaq({-128, rdi}, rsi)),

        % leaq with disp8, high registers
        ?_assertEqual(<<16#4C, 16#8D, 16#47, 16#10>>, jit_x86_64_asm:leaq({16, rdi}, r8)),
        ?_assertEqual(<<16#4C, 16#8D, 16#49, 16#20>>, jit_x86_64_asm:leaq({32, rcx}, r9)),

        % leaq with disp32, low registers
        ?_assertEqual(
            <<16#48, 16#8D, 16#87, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:leaq({305419896, rdi}, rax)
        ),
        % leaq with disp32, high registers
        ?_assertEqual(
            <<16#4C, 16#8D, 16#87, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:leaq({305419896, rdi}, r8)
        )
    ].

movsd_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(<<"0:	f2 0f 10 00          	movsd  (%rax),%xmm0">>),
            jit_x86_64_asm:movsd({0, rax}, xmm0)
        ),
        ?_assertEqual(
            dump_to_bin(<<"4:	f2 0f 10 09          	movsd  (%rcx),%xmm1">>),
            jit_x86_64_asm:movsd({0, rcx}, xmm1)
        ),
        ?_assertEqual(
            dump_to_bin(<<"8:	f2 0f 10 12          	movsd  (%rdx),%xmm2">>),
            jit_x86_64_asm:movsd({0, rdx}, xmm2)
        ),
        ?_assertEqual(
            dump_to_bin(<<"c:	f2 0f 10 1e          	movsd  (%rsi),%xmm3">>),
            jit_x86_64_asm:movsd({0, rsi}, xmm3)
        ),
        ?_assertEqual(
            dump_to_bin(<<"10:	f2 0f 10 27          	movsd  (%rdi),%xmm4">>),
            jit_x86_64_asm:movsd({0, rdi}, xmm4)
        ),
        ?_assertEqual(
            dump_to_bin(<<"14:	f2 0f 10 28          	movsd  (%rax),%xmm5">>),
            jit_x86_64_asm:movsd({0, rax}, xmm5)
        ),
        ?_assertEqual(
            dump_to_bin(<<"18:	f2 0f 10 31          	movsd  (%rcx),%xmm6">>),
            jit_x86_64_asm:movsd({0, rcx}, xmm6)
        ),
        ?_assertEqual(
            dump_to_bin(<<"1c:	f2 0f 10 3a          	movsd  (%rdx),%xmm7">>),
            jit_x86_64_asm:movsd({0, rdx}, xmm7)
        ),
        ?_assertEqual(
            dump_to_bin(<<"20:	f2 41 0f 10 00       	movsd  (%r8),%xmm0">>),
            jit_x86_64_asm:movsd({0, r8}, xmm0)
        ),
        ?_assertEqual(
            dump_to_bin(<<"4d:	f2 0f 10 40 08       	movsd  0x8(%rax),%xmm0">>),
            jit_x86_64_asm:movsd({8, rax}, xmm0)
        ),
        ?_assertEqual(
            dump_to_bin(<<"52:	f2 0f 10 48 10       	movsd  0x10(%rax),%xmm1">>),
            jit_x86_64_asm:movsd({16, rax}, xmm1)
        ),
        ?_assertEqual(
            dump_to_bin(<<"57:	f2 41 0f 10 40 20    	movsd  0x20(%r8),%xmm0">>),
            jit_x86_64_asm:movsd({32, r8}, xmm0)
        ),
        ?_assertEqual(
            dump_to_bin(<<"5d:	f2 41 0f 10 43 40    	movsd  0x40(%r11),%xmm0">>),
            jit_x86_64_asm:movsd({64, r11}, xmm0)
        )
    ].

movq_sib_test_() ->
    [
        % movq %rax, 16#10(%rdx,%rcx,2)
        ?_assertEqual(
            % REX.W, ModRM, SIB, disp8
            <<16#48, 16#89, 16#44, 16#4A, 16#10>>,
            jit_x86_64_asm:movq(rax, {16, rdx, rcx, 2})
        ),
        % movq %r8, -0x80(%r9,%r10,4)
        ?_assertEqual(
            % REX.WRXB, ModRM, SIB, disp32
            <<16#4F, 16#89, 16#44, 16#91, 16#80>>,
            jit_x86_64_asm:movq(r8, {-128, r9, r10, 4})
        ),
        % movq %r11, 16#7f(%r8,%rdx,8)
        ?_assertEqual(
            % REX.WRXB, ModRM, SIB, disp8
            <<16#4D, 16#89, 16#5C, 16#D0, 16#7F>>,
            jit_x86_64_asm:movq(r11, {127, r8, rdx, 8})
        ),
        % movq %rcx, 16#12345678(%rsi,%rdi,1)
        ?_assertEqual(
            % REX.W, ModRM, SIB, disp32
            <<16#48, 16#89, 16#8C, 16#3E, 16#78, 16#56, 16#34, 16#12>>,
            jit_x86_64_asm:movq(rcx, {305419896, rsi, rdi, 1})
        ),
        % movq $95, 16#18(%rax, %rcx, 8)
        ?_assertEqual(
            % REX.W, ModRM, SIB, disp32
            <<16#48, 16#C7, 16#84, 16#C8, 16#18, 16#00, 16#00, 16#00, 16#5F, 16#00, 16#00, 16#00>>,
            jit_x86_64_asm:movq(95, {16#18, rax, rcx, 8})
        )
    ].

jz_test_() ->
    [
        ?_assertEqual(dump_to_bin(<<"2: 74 fc   je     0 <x>">>), jit_x86_64_asm:jz(-4))
    ].

jz_rel8_test_() ->
    [
        ?_assertEqual({1, dump_to_bin(<<"2: 74 fc   je     0 <x>">>)}, jit_x86_64_asm:jz_rel8(-4))
    ].

jge_test_() ->
    [
        ?_assertEqual(dump_to_bin(<<"8: 7d f6   jge    0 <x>">>), jit_x86_64_asm:jge(-10))
    ].

jge_rel8_test_() ->
    [
        ?_assertEqual({1, dump_to_bin(<<"8: 7d f6   jge    0 <x>">>)}, jit_x86_64_asm:jge_rel8(-10))
    ].

jmp_rel8_test_() ->
    [
        ?_assertEqual({1, dump_to_bin(<<"0: eb 05   jmp    7 <x>">>)}, jit_x86_64_asm:jmp_rel8(5))
    ].

andb_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(<<"0: 24 01        and $0x1,%al">>), jit_x86_64_asm:andb(1, rax)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 24 80        and $0x80,%al">>), jit_x86_64_asm:andb(128, rax)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 24 ff        and $0xff,%al">>), jit_x86_64_asm:andb(-1, rax)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 24 ff        and $0xff,%al">>), jit_x86_64_asm:andb(255, rax)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 80 e1 7f     and $0x7f,%cl">>), jit_x86_64_asm:andb(127, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 80 e1 80     and $0x80,%cl">>), jit_x86_64_asm:andb(128, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 80 e1 ff     and $0xff,%cl">>), jit_x86_64_asm:andb(-1, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 80 e1 ff     and $0xff,%cl">>), jit_x86_64_asm:andb(255, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 41 80 e0 01  and $0x1,%r8b">>), jit_x86_64_asm:andb(1, r8)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 41 80 e1 7f  and $0x7f,%r9b">>), jit_x86_64_asm:andb(127, r9)
        )
    ].

subq_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(<<"0: 48 29 c1     sub  %rax,%rcx">>), jit_x86_64_asm:subq(rax, rcx)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 49 29 c2     sub  %rax,%r10">>), jit_x86_64_asm:subq(rax, r10)
        ),
        ?_assertEqual(
            dump_to_bin(<<"0: 4c 29 c1     sub  %r8,%rcx">>), jit_x86_64_asm:subq(r8, rcx)
        )
    ].

decl_test_() ->
    [
        ?_assertEqual(
            dump_to_bin(<<"0: ff 4e 10     decl 0x10(%rsi)">>), jit_x86_64_asm:decl({16, rsi})
        )
    ].

orq_rel32_test_() ->
    [
        ?_assertEqual(
            {2, dump_to_bin(<<"0:  48 0d 78 56 34 12     or  $0x12345678,%rax">>)},
            jit_x86_64_asm:orq_rel32(16#12345678, rax)
        ),
        ?_assertEqual(
            {3, dump_to_bin(<<"0:  48 81 c9 78 56 34 12  or  $0x12345678,%rcx">>)},
            jit_x86_64_asm:orq_rel32(16#12345678, rcx)
        ),
        ?_assertEqual(
            {3, dump_to_bin(<<"0:  49 81 c8 78 56 34 12  or  $0x12345678,%r8">>)},
            jit_x86_64_asm:orq_rel32(16#12345678, r8)
        )
    ].

retq_test_() ->
    [
        ?_assertEqual(dump_to_bin(<<"0:	c3  retq">>), jit_x86_64_asm:retq())
    ].

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
dump_to_bin0(Bin, pre_hex, Acc) ->
    dump_to_bin0(Bin, hex, Acc);
dump_to_bin0(<<$\ , $\ , Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<$\t, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<$\ , Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, hex, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<D1, D2, Tail/binary>>, hex, Acc) when
    ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f)) andalso
        ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f))
->
    dump_to_bin0(Tail, hex, [list_to_integer([D1, D2], 16) | Acc]);
dump_to_bin0(<<_Other, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<$\n, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
dump_to_bin0(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc)).
