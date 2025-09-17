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

-module(jit_armv6m_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(arm, Bin, Str), Value)
).

adds_test_() ->
    [
        ?_assertAsmEqual(<<16#3038:16/little>>, "adds r0, #56", jit_armv6m_asm:adds(r0, 56)),
        ?_assertAsmEqual(
            <<16#3038:16/little>>, "adds r0, r0, #56", jit_armv6m_asm:adds(r0, r0, 56)
        ),
        ?_assertAsmEqual(<<16#3000:16/little>>, "adds r0, #0", jit_armv6m_asm:adds(r0, 0)),
        ?_assertAsmEqual(<<16#3101:16/little>>, "adds r1, #1", jit_armv6m_asm:adds(r1, 1)),
        ?_assertAsmEqual(<<16#1C42:16/little>>, "adds r2, r0, #1", jit_armv6m_asm:adds(r2, r0, 1)),
        ?_assertAsmEqual(<<16#18c9:16/little>>, "adds r1, r1, r3", jit_armv6m_asm:adds(r1, r1, r3)),
        ?_assertAsmEqual(<<16#1850:16/little>>, "adds r0, r2, r1", jit_armv6m_asm:adds(r0, r2, r1))
    ].

add_test_() ->
    [
        %% ARMv6-M Thumb ADD instructions (register, high registers supported)
        %% ADD Rd, Rm - adds register value to register (supports PC)
        ?_assertAsmEqual(<<16#449f:16/little>>, "add pc, r3", jit_armv6m_asm:add(pc, r3)),
        ?_assertAsmEqual(<<16#4440:16/little>>, "add r0, r8", jit_armv6m_asm:add(r0, r8)),
        ?_assertAsmEqual(<<16#4488:16/little>>, "add r8, r1", jit_armv6m_asm:add(r8, r1)),
        ?_assertAsmEqual(<<16#44c9:16/little>>, "add r9, r9", jit_armv6m_asm:add(r9, r9)),
        ?_assertAsmEqual(<<16#4419:16/little>>, "add r1, r3", jit_armv6m_asm:add(r1, r3))
    ].

subs_test_() ->
    [
        ?_assertAsmEqual(<<16#3f38:16/little>>, "subs r7, #56", jit_armv6m_asm:subs(r7, 56)),
        ?_assertAsmEqual(
            <<16#3f38:16/little>>, "subs r7, r7, #56", jit_armv6m_asm:subs(r7, r7, 56)
        ),
        ?_assertAsmEqual(<<16#3800:16/little>>, "subs r0, #0", jit_armv6m_asm:subs(r0, 0)),
        ?_assertAsmEqual(<<16#1e42:16/little>>, "subs r2, r0, #1", jit_armv6m_asm:subs(r2, r0, 1)),
        ?_assertAsmEqual(<<16#1ad1:16/little>>, "subs r1, r2, r3", jit_armv6m_asm:subs(r1, r2, r3))
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(<<16#B082:16/little>>, "sub sp, #8", jit_armv6m_asm:sub(sp, 8)),
        ?_assertAsmEqual(<<16#B082:16/little>>, "sub sp, sp, #8", jit_armv6m_asm:sub(sp, sp, 8)),
        ?_assertAsmEqual(<<16#B080:16/little>>, "sub sp, #0", jit_armv6m_asm:sub(sp, 0)),
        ?_assertAsmEqual(<<16#B084:16/little>>, "sub sp, #16", jit_armv6m_asm:sub(sp, 16)),
        ?_assertAsmEqual(<<16#B0FF:16/little>>, "sub sp, #508", jit_armv6m_asm:sub(sp, 508))
    ].

muls_test_() ->
    [
        ?_assertAsmEqual(<<16#4359:16/little>>, "muls r1, r3", jit_armv6m_asm:muls(r1, r3)),
        ?_assertAsmEqual(<<16#4348:16/little>>, "muls r0, r1", jit_armv6m_asm:muls(r0, r1))
    ].

b_test_() ->
    [
        %% Thumb B (unconditional) encoding tests - ARMv6-M 16-bit only
        ?_assertAsmEqual(<<16#E7FE:16/little>>, "b .+0", jit_armv6m_asm:b(0)),
        ?_assertAsmEqual(<<16#E006:16/little>>, "b .+16", jit_armv6m_asm:b(16)),
        ?_assertAsmEqual(<<16#E7DE:16/little>>, "b .-64", jit_armv6m_asm:b(-64)),
        ?_assertAsmEqual(<<16#E000:16/little>>, "b .+4", jit_armv6m_asm:b(4)),
        ?_assertAsmEqual(<<16#E3FF:16/little>>, "b .+2050", jit_armv6m_asm:b(2050)),
        ?_assertAsmEqual(<<16#E400:16/little>>, "b .-2044", jit_armv6m_asm:b(-2044)),
        %% Test error cases for offsets too large for ARMv6-M
        ?_assertError({unencodable_offset, 2052}, jit_armv6m_asm:b(2052)),
        ?_assertError({unencodable_offset, -2046}, jit_armv6m_asm:b(-2046))
    ].

blx_test_() ->
    [
        %% Thumb BLX (register) encoding tests
        ?_assertAsmEqual(<<16#4780:16/little>>, "blx r0", jit_armv6m_asm:blx(r0)),
        ?_assertAsmEqual(<<16#4788:16/little>>, "blx r1", jit_armv6m_asm:blx(r1)),
        ?_assertAsmEqual(<<16#47E8:16/little>>, "blx r13", jit_armv6m_asm:blx(r13))
    ].

bx_test_() ->
    [
        %% Thumb BX (branch exchange) encoding tests
        ?_assertAsmEqual(<<16#4700:16/little>>, "bx r0", jit_armv6m_asm:bx(r0)),
        ?_assertAsmEqual(<<16#4708:16/little>>, "bx r1", jit_armv6m_asm:bx(r1)),
        ?_assertAsmEqual(<<16#4768:16/little>>, "bx r13", jit_armv6m_asm:bx(r13))
    ].

ldr_test_() ->
    [
        %% ARMv6-M Thumb LDR immediate offset (0-124, multiple of 4)
        ?_assertAsmEqual(
            <<16#6889:16/little>>, "ldr r1, [r1, #8]", jit_armv6m_asm:ldr(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#6982:16/little>>, "ldr r2, [r0, #24]", jit_armv6m_asm:ldr(r2, {r0, 24})
        ),
        %% SP-relative load (0-1020, multiple of 4)
        ?_assertAsmEqual(
            <<16#9f00:16/little>>, "ldr r7, [sp, #0]", jit_armv6m_asm:ldr(r7, {sp, 0})
        ),
        ?_assertAsmEqual(
            <<16#9801:16/little>>, "ldr r0, [sp, #4]", jit_armv6m_asm:ldr(r0, {sp, 4})
        ),
        %% PC-relative load (0-1020, multiple of 4)
        ?_assertAsmEqual(
            <<16#4a18:16/little>>, "ldr r2, [pc, #96]", jit_armv6m_asm:ldr(r2, {pc, 96})
        ),
        %% Register offset
        ?_assertAsmEqual(
            <<16#58d1:16/little>>, "ldr r1, [r2, r3]", jit_armv6m_asm:ldr(r1, {r2, r3})
        )
    ].

movs_test_() ->
    [
        %% ARMv6-M Thumb MOVS instructions (sets flags)
        %% MOVS immediate (8-bit only, 0-255)
        ?_assertAsmEqual(<<16#2000:16/little>>, "movs r0, #0", jit_armv6m_asm:movs(r0, 0)),
        ?_assertAsmEqual(<<16#2101:16/little>>, "movs r1, #1", jit_armv6m_asm:movs(r1, 1)),
        ?_assertAsmEqual(<<16#22ff:16/little>>, "movs r2, #255", jit_armv6m_asm:movs(r2, 255)),
        %% MOVS register - low registers only (r0-r7)
        ?_assertAsmEqual(<<16#0008:16/little>>, "movs r0, r1", jit_armv6m_asm:movs(r0, r1)),
        ?_assertAsmEqual(<<16#001a:16/little>>, "movs r2, r3", jit_armv6m_asm:movs(r2, r3))
    ].

mov_test_() ->
    [
        %% ARMv6-M Thumb MOV instructions (no flags, for high registers)
        %% MOV register - requires at least one high register (r8-r15)
        ?_assertAsmEqual(<<16#4680:16/little>>, "mov r8, r0", jit_armv6m_asm:mov(r8, r0)),
        ?_assertAsmEqual(<<16#4640:16/little>>, "mov r0, r8", jit_armv6m_asm:mov(r0, r8)),
        ?_assertAsmEqual(<<16#46c8:16/little>>, "mov r8, r9", jit_armv6m_asm:mov(r8, r9)),
        ?_assertAsmEqual(<<16#46c0:16/little>>, "mov r8, r8", jit_armv6m_asm:mov(r8, r8)),
        ?_assertAsmEqual(<<16#4619:16/little>>, "mov r1, r3", jit_armv6m_asm:mov(r1, r3)),
        ?_assertAsmEqual(<<16#46c0:16/little>>, "nop", jit_armv6m_asm:nop())
    ].

str_test_() ->
    [
        %% ARMv6-M Thumb STR immediate offset (0-124, multiple of 4)
        ?_assertAsmEqual(
            <<16#6089:16/little>>, "str r1, [r1, #8]", jit_armv6m_asm:str(r1, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#6182:16/little>>, "str r2, [r0, #24]", jit_armv6m_asm:str(r2, {r0, 24})
        ),
        %% SP-relative store (0-1020, multiple of 4)
        ?_assertAsmEqual(
            <<16#9700:16/little>>, "str r7, [sp, #0]", jit_armv6m_asm:str(r7, {sp, 0})
        ),
        ?_assertAsmEqual(
            <<16#9001:16/little>>, "str r0, [sp, #4]", jit_armv6m_asm:str(r0, {sp, 4})
        ),
        %% Register offset
        ?_assertAsmEqual(
            <<16#50d1:16/little>>, "str r1, [r2, r3]", jit_armv6m_asm:str(r1, {r2, r3})
        )
    ].

cmp_test_() ->
    [
        %% ARMv6-M Thumb CMP register (low registers only)
        ?_assertAsmEqual(<<16#4288:16/little>>, "cmp r0, r1", jit_armv6m_asm:cmp(r0, r1)),
        ?_assertAsmEqual(<<16#42bb:16/little>>, "cmp r3, r7", jit_armv6m_asm:cmp(r3, r7)),
        %% ARMv6-M Thumb CMP immediate (8-bit, 0-255, low registers only)
        ?_assertAsmEqual(<<16#2800:16/little>>, "cmp r0, #0", jit_armv6m_asm:cmp(r0, 0)),
        ?_assertAsmEqual(<<16#2805:16/little>>, "cmp r0, #5", jit_armv6m_asm:cmp(r0, 5)),
        ?_assertAsmEqual(<<16#2fff:16/little>>, "cmp r7, #255", jit_armv6m_asm:cmp(r7, 255))
    ].

ands_test_() ->
    [
        %% ARMv6-M Thumb ANDS register (2-operand: Rd = Rd AND Rm)
        ?_assertAsmEqual(<<16#4008:16/little>>, "ands r0, r1", jit_armv6m_asm:ands(r0, r1)),
        ?_assertAsmEqual(<<16#4011:16/little>>, "ands r1, r2", jit_armv6m_asm:ands(r1, r2)),
        ?_assertAsmEqual(<<16#401a:16/little>>, "ands r2, r3", jit_armv6m_asm:ands(r2, r3))
    ].

orrs_test_() ->
    [
        %% ARMv6-M Thumb ORRS register (2-operand: Rd = Rd OR Rm, sets flags)
        ?_assertAsmEqual(<<16#4308:16/little>>, "orrs r0, r1", jit_armv6m_asm:orrs(r0, r1)),
        ?_assertAsmEqual(<<16#4311:16/little>>, "orrs r1, r2", jit_armv6m_asm:orrs(r1, r2)),
        ?_assertAsmEqual(<<16#431a:16/little>>, "orrs r2, r3", jit_armv6m_asm:orrs(r2, r3))
    ].

bics_test_() ->
    [
        ?_assertAsmEqual(<<16#4391:16/little>>, "bics r1, r2", jit_armv6m_asm:bics(r1, r2)),
        ?_assertAsmEqual(<<16#43a3:16/little>>, "bics r3, r4", jit_armv6m_asm:bics(r3, r4))
    ].

negs_test_() ->
    [
        ?_assertAsmEqual(<<16#4251:16/little>>, "negs r1, r2", jit_armv6m_asm:negs(r1, r2)),
        ?_assertAsmEqual(<<16#4263:16/little>>, "negs r3, r4", jit_armv6m_asm:negs(r3, r4))
    ].

rsbs_test_() ->
    [
        ?_assertAsmEqual(<<16#4251:16/little>>, "rsbs r1, r2, 0", jit_armv6m_asm:rsbs(r1, r2, 0)),
        ?_assertAsmEqual(<<16#4263:16/little>>, "rsbs r3, r4, 0", jit_armv6m_asm:rsbs(r3, r4, 0))
    ].

lsls_test_() ->
    [
        %% ARMv6-M Thumb LSLS immediate shift (1-31)
        ?_assertAsmEqual(<<16#0148:16/little>>, "lsls r0, r1, #5", jit_armv6m_asm:lsls(r0, r1, 5)),
        ?_assertAsmEqual(<<16#0212:16/little>>, "lsls r2, r2, #8", jit_armv6m_asm:lsls(r2, r2, 8)),
        %% LSLS register shift
        ?_assertAsmEqual(<<16#409a:16/little>>, "lsls r2, r3", jit_armv6m_asm:lsls(r2, r3))
    ].

lsrs_test_() ->
    [
        %% ARMv6-M Thumb LSRS immediate shift (1-32)
        ?_assertAsmEqual(<<16#0948:16/little>>, "lsrs r0, r1, #5", jit_armv6m_asm:lsrs(r0, r1, 5)),
        ?_assertAsmEqual(<<16#0a12:16/little>>, "lsrs r2, r2, #8", jit_armv6m_asm:lsrs(r2, r2, 8)),
        %% LSRS register shift
        ?_assertAsmEqual(<<16#40da:16/little>>, "lsrs r2, r3", jit_armv6m_asm:lsrs(r2, r3))
    ].

tst_test_() ->
    [
        %% ARMv6-M Thumb TST instructions (register only, low registers)
        %% TST Rn, Rm - test bits (performs Rn & Rm, updates flags)
        ?_assertAsmEqual(<<16#4208:16/little>>, "tst r0, r1", jit_armv6m_asm:tst(r0, r1)),
        ?_assertAsmEqual(<<16#421a:16/little>>, "tst r2, r3", jit_armv6m_asm:tst(r2, r3)),
        ?_assertAsmEqual(<<16#4239:16/little>>, "tst r1, r7", jit_armv6m_asm:tst(r1, r7))
    ].

bcc_test_() ->
    [
        %% Thumb conditional branch encoding tests - ARMv6-M 16-bit only
        ?_assertAsmEqual(<<16#D0FE:16/little>>, "beq .+0", jit_armv6m_asm:bcc(eq, 0)),
        ?_assertAsmEqual(<<16#D1FE:16/little>>, "bne .+0", jit_armv6m_asm:bcc(ne, 0)),
        ?_assertAsmEqual(<<16#D1DE:16/little>>, "bne .-64", jit_armv6m_asm:bcc(ne, -64)),
        ?_assertAsmEqual(<<16#D03E:16/little>>, "beq .+128", jit_armv6m_asm:bcc(eq, 128)),
        ?_assertAsmEqual(<<16#D23E:16/little>>, "bcs .+128", jit_armv6m_asm:bcc(cs, 128)),
        ?_assertAsmEqual(<<16#D33E:16/little>>, "bcc .+128", jit_armv6m_asm:bcc(cc, 128)),
        ?_assertAsmEqual(<<16#D43E:16/little>>, "bmi .+128", jit_armv6m_asm:bcc(mi, 128)),
        ?_assertAsmEqual(<<16#D53E:16/little>>, "bpl .+128", jit_armv6m_asm:bcc(pl, 128)),
        ?_assertAsmEqual(<<16#D63E:16/little>>, "bvs .+128", jit_armv6m_asm:bcc(vs, 128)),
        ?_assertAsmEqual(<<16#D83E:16/little>>, "bhi .+128", jit_armv6m_asm:bcc(hi, 128)),
        ?_assertAsmEqual(<<16#D93E:16/little>>, "bls .+128", jit_armv6m_asm:bcc(ls, 128)),
        ?_assertAsmEqual(<<16#DA3E:16/little>>, "bge .+128", jit_armv6m_asm:bcc(ge, 128)),
        ?_assertAsmEqual(<<16#DB3E:16/little>>, "blt .+128", jit_armv6m_asm:bcc(lt, 128)),
        ?_assertAsmEqual(<<16#DC3E:16/little>>, "bgt .+128", jit_armv6m_asm:bcc(gt, 128)),
        ?_assertAsmEqual(<<16#DD3E:16/little>>, "ble .+128", jit_armv6m_asm:bcc(le, 128)),
        ?_assertAsmEqual(<<16#E03E:16/little>>, "bal .+128", jit_armv6m_asm:bcc(al, 128)),
        ?_assertAsmEqual(<<16#D07F:16/little>>, "beq .+258", jit_armv6m_asm:bcc(eq, 258)),
        ?_assertAsmEqual(<<16#D180:16/little>>, "bne .-252", jit_armv6m_asm:bcc(ne, -252)),
        %% Test error cases for offsets too large for ARMv6-M
        ?_assertError({unencodable_offset, 260}, jit_armv6m_asm:bcc(eq, 260)),
        ?_assertError({unencodable_offset, -254}, jit_armv6m_asm:bcc(ne, -254))
    ].

adr_test_() ->
    [
        %% ARMv6-M Thumb ADR (PC-relative address) - implemented as ADD Rd, PC, #imm
        %% adr(Rd, N) means "Rd = current_PC + N" where PC is instruction address
        %% Range: 4-1024, must be multiple of 4
        ?_assertAsmEqual(<<16#a000:16/little>>, "adr r0, .+4", jit_armv6m_asm:adr(r0, 4)),
        ?_assertAsmEqual(<<16#a101:16/little>>, "adr r1, .+8", jit_armv6m_asm:adr(r1, 8)),
        ?_assertAsmEqual(<<16#a202:16/little>>, "adr r2, .+12", jit_armv6m_asm:adr(r2, 12)),
        ?_assertAsmEqual(<<16#a708:16/little>>, "adr r7, .+36", jit_armv6m_asm:adr(r7, 36)),
        %% Test maximum offset value (1024 bytes)
        ?_assertAsmEqual(<<16#a0ff:16/little>>, "adr r0, .+1024", jit_armv6m_asm:adr(r0, 1024))
    ].

push_test_() ->
    [
        %% ARMv6-M Thumb PUSH instruction (low registers + optional LR)
        %% Single register push
        ?_assertAsmEqual(<<16#b401:16/little>>, "push {r0}", jit_armv6m_asm:push([r0])),
        %% Multiple register push
        ?_assertAsmEqual(
            <<16#b407:16/little>>, "push {r0, r1, r2}", jit_armv6m_asm:push([r0, r1, r2])
        ),
        %% Push with LR
        ?_assertAsmEqual(<<16#b500:16/little>>, "push {lr}", jit_armv6m_asm:push([lr])),
        %% Push registers + LR
        ?_assertAsmEqual(
            <<16#b507:16/little>>, "push {r0, r1, r2, lr}", jit_armv6m_asm:push([r0, r1, r2, lr])
        )
    ].

pop_test_() ->
    [
        %% ARMv6-M Thumb POP instruction (low registers + optional PC)
        %% Single register pop
        ?_assertAsmEqual(<<16#bc01:16/little>>, "pop {r0}", jit_armv6m_asm:pop([r0])),
        %% Multiple register pop
        ?_assertAsmEqual(
            <<16#bc07:16/little>>, "pop {r0, r1, r2}", jit_armv6m_asm:pop([r0, r1, r2])
        ),
        %% Pop with PC
        ?_assertAsmEqual(<<16#bd00:16/little>>, "pop {pc}", jit_armv6m_asm:pop([pc])),
        %% Pop registers + PC
        ?_assertAsmEqual(
            <<16#bd07:16/little>>, "pop {r0, r1, r2, pc}", jit_armv6m_asm:pop([r0, r1, r2, pc])
        )
    ].

bkpt_test_() ->
    [
        %% BKPT #0
        ?_assertAsmEqual(<<16#be00:16/little>>, "bkpt #0", jit_armv6m_asm:bkpt(0)),
        %% BKPT #1
        ?_assertAsmEqual(<<16#be01:16/little>>, "bkpt #1", jit_armv6m_asm:bkpt(1)),
        %% BKPT #255
        ?_assertAsmEqual(<<16#beff:16/little>>, "bkpt #255", jit_armv6m_asm:bkpt(255))
    ].

mvns_test_() ->
    [
        %% ARMv6-M Thumb MVNS instructions (register only, low registers)
        %% MVNS Rd, Rm - bitwise NOT (performs ~Rm -> Rd, sets flags)
        ?_assertAsmEqual(<<16#43e3:16/little>>, "mvns r3, r4", jit_armv6m_asm:mvns(r3, r4)),
        ?_assertAsmEqual(<<16#43f3:16/little>>, "mvns r3, r6", jit_armv6m_asm:mvns(r3, r6)),
        ?_assertAsmEqual(<<16#43c8:16/little>>, "mvns r0, r1", jit_armv6m_asm:mvns(r0, r1))
    ].
