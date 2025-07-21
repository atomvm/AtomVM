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

-module(jit_aarch64_asm_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

add_test_() ->
    [
        ?_assertEqual(<<16#9100e0e7:32/little>>, jit_aarch64_asm:add(r7, r7, 56)),
        ?_assertEqual(<<16#91000000:32/little>>, jit_aarch64_asm:add(r0, r0, 0)),
        ?_assertEqual(<<16#91000421:32/little>>, jit_aarch64_asm:add(r1, r1, 1))
    ].

b_test_() ->
    [
        ?_assertEqual(<<16#14000000:32/little>>, jit_aarch64_asm:b(0)),
        ?_assertEqual(<<16#14000004:32/little>>, jit_aarch64_asm:b(16)),
        ?_assertEqual(<<16#14000001:32/little>>, jit_aarch64_asm:b(4))
    ].

brk_test_() ->
    [
        ?_assertEqual(<<16#D4200000:32/little>>, jit_aarch64_asm:brk(0)),
        ?_assertEqual(<<16#D4201900:32/little>>, jit_aarch64_asm:brk(200))
    ].

blr_test_() ->
    [
        ?_assertEqual(<<16#D63F0000:32/little>>, jit_aarch64_asm:blr(r0)),
        ?_assertEqual(<<16#D63F0020:32/little>>, jit_aarch64_asm:blr(r1)),
        ?_assertEqual(<<16#D63F01A0:32/little>>, jit_aarch64_asm:blr(r13))
    ].

br_test_() ->
    [
        ?_assertEqual(<<16#D61F0000:32/little>>, jit_aarch64_asm:br(r0)),
        ?_assertEqual(<<16#D61F0020:32/little>>, jit_aarch64_asm:br(r1)),
        ?_assertEqual(<<16#D61F01A0:32/little>>, jit_aarch64_asm:br(r13))
    ].

ldr_test_() ->
    [
        ?_assertEqual(<<16#F9400421:32/little>>, jit_aarch64_asm:ldr(r1, {8, r1})),
        ?_assertEqual(<<16#F9403042:32/little>>, jit_aarch64_asm:ldr(r2, {96, r2}))
    ].

mov_test_() ->
    [
        % mov immediate - simple cases
        ?_assertEqual(<<16#D2800000:32/little>>, jit_aarch64_asm:mov(r0, 0)),
        ?_assertEqual(<<16#D2801901:32/little>>, jit_aarch64_asm:mov(r1, 200)),
        ?_assertEqual(<<16#d28000b3:32/little>>, jit_aarch64_asm:mov(r19, 5)),
        ?_assertEqual(<<16#92800094:32/little>>, jit_aarch64_asm:mov(r20, -5)),
        ?_assertEqual(<<16#d2800015:32/little>>, jit_aarch64_asm:mov(r21, 0)),
        ?_assertEqual(<<16#d29ffff0:32/little>>, jit_aarch64_asm:mov(r16, 16#FFFF)),
        ?_assertEqual(<<16#929fffcf:32/little>>, jit_aarch64_asm:mov(r15, -16#FFFF)),

        % mov immediate - complex cases requiring multiple instructions
        ?_assertEqual(<<16#d2a00052:32/little>>, jit_aarch64_asm:mov(r18, 16#20000)),
        ?_assertEqual(<<16#b26fbbf1:32/little>>, jit_aarch64_asm:mov(r17, -131072)),

        % mov immediate - very large value requiring multiple instructions
        ?_assertEqual(
            <<16#D29579A1:32/little, 16#F2B7C041:32/little, 16#F2DFD741:32/little,
                16#F2EFF941:32/little>>,
            jit_aarch64_asm:mov(r1, 9208452466117618637)
        ),

        % mov register
        ?_assertEqual(<<16#AA0103E0:32/little>>, jit_aarch64_asm:mov(r0, r1)),
        ?_assertEqual(<<16#AA0703E1:32/little>>, jit_aarch64_asm:mov(r1, r7))
    ].

orr_test_() ->
    [
        % ORR Rd, XZR, Rm (MOV)
        ?_assertEqual(<<16#AA0103E0:32/little>>, jit_aarch64_asm:orr(r0, xzr, r1)),
        % ORR Rd, Rn, Rm
        ?_assertEqual(<<16#AA010020:32/little>>, jit_aarch64_asm:orr(r0, r1, r1)),
        ?_assertEqual(<<16#AA020041:32/little>>, jit_aarch64_asm:orr(r1, r2, r2))
    ].

str_test_() ->
    [
        ?_assertEqual(<<16#F9000421:32/little>>, jit_aarch64_asm:str(r1, {8, r1})),
        ?_assertEqual(<<16#F9003042:32/little>>, jit_aarch64_asm:str(r2, {96, r2})),
        % str with xzr (zero register) - stores zero to memory
        ?_assertEqual(<<16#F900001F:32/little>>, jit_aarch64_asm:str(xzr, {0, r0})),
        ?_assertEqual(<<16#F900043F:32/little>>, jit_aarch64_asm:str(xzr, {8, r1})),
        ?_assertEqual(<<16#F900085F:32/little>>, jit_aarch64_asm:str(xzr, {16, r2}))
    ].

str_x_test_() ->
    [
        % Store-update (writeback) with SP
        ?_assertEqual(
            <<16#F81F0FE7:32/little>>,
            jit_aarch64_asm:str_x(r7, {sp, -16}, '!')
        ),
        % Store-update (writeback) with SP, positive offset
        ?_assertEqual(
            <<16#F8010FE7:32/little>>,
            jit_aarch64_asm:str_x(r7, {sp, 16}, '!')
        ),
        % Store-update (writeback) with SP, zero offset
        ?_assertEqual(
            <<16#F80007E7:32/little>>,
            jit_aarch64_asm:str_x(r7, {sp}, 0)
        )
    ].

cmp_test_() ->
    [
        % cmp reg, reg
        ?_assertEqual(<<16#EB01001F:32/little>>, jit_aarch64_asm:cmp(r0, r1)),
        % cmp reg, imm
        ?_assertEqual(<<16#F100001F:32/little>>, jit_aarch64_asm:cmp(r0, 0)),
        ?_assertEqual(<<16#F103001F:32/little>>, jit_aarch64_asm:cmp(r0, 192))
    ].

cmp32_test_() ->
    [
        % cmp32 reg, reg
        ?_assertEqual(<<16#6B01001F:32/little>>, jit_aarch64_asm:cmp32(r0, r1)),
        % cmp32 reg, imm
        ?_assertEqual(<<16#7100001F:32/little>>, jit_aarch64_asm:cmp32(r0, 0)),
        ?_assertEqual(<<16#7103001F:32/little>>, jit_aarch64_asm:cmp32(r0, 192))
    ].

and_test_() ->
    [
        % AND reg, reg, reg
        ?_assertEqual(<<16#8A010020:32/little>>, jit_aarch64_asm:and_(r0, r1, r1)),
        % AND reg, reg, imm
        ?_assertEqual(<<16#927A0420:32/little>>, jit_aarch64_asm:and_(r0, r1, 192)),
        ?_assertEqual(<<16#927ff8e7:32/little>>, jit_aarch64_asm:and_(r7, r7, -2)),
        ?_assertEqual(<<16#9200cc41:32/little>>, jit_aarch64_asm:and_(r1, r2, 16#f0f0f0f0f0f0f0f)),
        ?_assertEqual(<<16#92603c62:32/little>>, jit_aarch64_asm:and_(r2, r3, 16#ffff00000000)),
        ?_assertEqual(<<16#92785c83:32/little>>, jit_aarch64_asm:and_(r3, r4, 16#ffffff00))
    ].

lsl_test_() ->
    [
        ?_assertEqual(<<16#D3607C00:32/little>>, jit_aarch64_asm:lsl(r0, r0, 32))
    ].

lsr_test_() ->
    [
        ?_assertEqual(<<16#D340FC00:32/little>>, jit_aarch64_asm:lsr(r0, r0, 0)),
        ?_assertEqual(<<16#D340FC01:32/little>>, jit_aarch64_asm:lsr(r1, r0, 0)),
        ?_assertEqual(<<16#D360FC00:32/little>>, jit_aarch64_asm:lsr(r0, r0, 32))
    ].

ret_test_() ->
    [
        ?_assertEqual(<<16#D65F03C0:32/little>>, jit_aarch64_asm:ret())
    ].

tst_test_() ->
    [
        % TST reg, reg
        ?_assertEqual(<<16#EA01001F:32/little>>, jit_aarch64_asm:tst(r0, r1)),
        % TST reg, imm (power of 2)
        ?_assertEqual(<<16#F27C001F:32/little>>, jit_aarch64_asm:tst(r0, 16))
    ].

tst32_test_() ->
    [
        % TST32 reg, reg
        ?_assertEqual(<<16#6A01001F:32/little>>, jit_aarch64_asm:tst32(r0, r1)),
        % TST32 reg, imm (power of 2)
        ?_assertEqual(<<16#721C001F:32/little>>, jit_aarch64_asm:tst32(r0, 16))
    ].

bcc_test_() ->
    [
        ?_assertEqual(<<16#54000000:32/little>>, jit_aarch64_asm:bcc(eq, 0)),
        ?_assertEqual(<<16#54000001:32/little>>, jit_aarch64_asm:bcc(ne, 0)),
        ?_assertEqual(<<16#54000400:32/little>>, jit_aarch64_asm:bcc(eq, 128))
    ].

stp_x_test_() ->
    [
        ?_assertEqual(
            <<16#a8815113:32/little>>,
            jit_aarch64_asm:stp_x(r19, r20, {r8}, 16)
        ),
        ?_assertEqual(
            <<16#a88153f3:32/little>>,
            jit_aarch64_asm:stp_x(r19, r20, {sp}, 16)
        ),
        % Store-update (writeback) variants
        ?_assertEqual(
            <<16#a9bf27e8:32/little>>,
            jit_aarch64_asm:stp_x(r8, r9, {sp, -16}, '!')
        ),
        ?_assertEqual(
            <<16#a98127e8:32/little>>,
            jit_aarch64_asm:stp_x(r8, r9, {sp, 16}, '!')
        ),
        ?_assertEqual(
            <<16#a98027e8:32/little>>,
            jit_aarch64_asm:stp_x(r8, r9, {sp, 0}, '!')
        )
    ].

ldp_x_test_() ->
    [
        ?_assertEqual(
            <<16#a8c15113:32/little>>,
            jit_aarch64_asm:ldp_x(r19, r20, {r8}, 16)
        ),
        ?_assertEqual(
            <<16#a8c153f3:32/little>>,
            jit_aarch64_asm:ldp_x(r19, r20, {sp}, 16)
        )
    ].

ldr_x_test_() ->
    [
        % Load-update (writeback) with SP, negative offset
        ?_assertEqual(
            <<16#F85F0FE7:32/little>>,
            jit_aarch64_asm:ldr_x(r7, {sp, -16}, '!')
        ),
        % Load-update (writeback) with SP, positive offset
        ?_assertEqual(
            <<16#F8410FE7:32/little>>,
            jit_aarch64_asm:ldr_x(r7, {sp, 16}, '!')
        ),
        % Load-update (writeback) with SP, zero offset
        ?_assertEqual(
            <<16#F84007E7:32/little>>,
            jit_aarch64_asm:ldr_x(r7, {sp}, 0)
        )
    ].

subs_test_() ->
    [
        % SUBS with immediate
        ?_assertEqual(<<16#F1000021:32/little>>, jit_aarch64_asm:subs(r1, r1, 0)),
        ?_assertEqual(<<16#F1000421:32/little>>, jit_aarch64_asm:subs(r1, r1, 1)),
        % SUBS with register
        ?_assertEqual(<<16#eb000021:32/little>>, jit_aarch64_asm:subs(r1, r1, r0)),
        ?_assertEqual(<<16#eb0a0021:32/little>>, jit_aarch64_asm:subs(r1, r1, r10))
    ].

adr_test_() ->
    [
        %% ADR x0, #0
        ?_assertEqual(<<16#10000000:32/little>>, jit_aarch64_asm:adr(r0, 0)),
        %% ADR x1, #4
        ?_assertEqual(<<16#10000021:32/little>>, jit_aarch64_asm:adr(r1, 4)),
        %% ADR x2, #-4
        ?_assertEqual(<<16#10ffffe2:32/little>>, jit_aarch64_asm:adr(r2, -4)),
        %% ADR x3, #1048572 (max positive)
        ?_assertEqual(<<16#107fffe3:32/little>>, jit_aarch64_asm:adr(r3, 1048572)),
        %% ADR x4, #-1048576 (max negative)
        ?_assertEqual(<<16#10800004:32/little>>, jit_aarch64_asm:adr(r4, -1048576)),
        %% ADR with offset not a multiple of 4 is valid
        ?_assertEqual(<<16#70000000:32/little>>, jit_aarch64_asm:adr(r0, 3))
    ].
