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

-module(jit_arm32_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(arm32, Bin, Str), Value)
).

add_test_() ->
    [
        %% ADD register
        ?_assertAsmEqual(
            <<16#E0810002:32/little>>, "add r0, r1, r2", jit_arm32_asm:add(al, r0, r1, r2)
        ),
        ?_assertAsmEqual(
            <<16#E0833004:32/little>>, "add r3, r3, r4", jit_arm32_asm:add(al, r3, r4)
        ),
        %% ADD immediate
        ?_assertAsmEqual(
            <<16#E2800038:32/little>>, "add r0, r0, #56", jit_arm32_asm:add(al, r0, 56)
        ),
        ?_assertAsmEqual(<<16#E2811001:32/little>>, "add r1, r1, #1", jit_arm32_asm:add(al, r1, 1)),
        %% ADD with condition
        ?_assertAsmEqual(
            <<16#00810002:32/little>>, "addeq r0, r1, r2", jit_arm32_asm:add(eq, r0, r1, r2)
        ),
        %% ADD with rotated immediate (e.g., 256 = 0x100 = 1 ROR 24 = rot=12, imm8=1)
        ?_assertAsmEqual(
            <<16#E2800C01:32/little>>, "add r0, r0, #256", jit_arm32_asm:add(al, r0, 256)
        )
    ].

sub_test_() ->
    [
        %% SUB register
        ?_assertAsmEqual(
            <<16#E0410002:32/little>>, "sub r0, r1, r2", jit_arm32_asm:sub(al, r0, r1, r2)
        ),
        %% SUB immediate
        ?_assertAsmEqual(
            <<16#E2400038:32/little>>, "sub r0, r0, #56", jit_arm32_asm:sub(al, r0, 56)
        ),
        ?_assertAsmEqual(<<16#E2411001:32/little>>, "sub r1, r1, #1", jit_arm32_asm:sub(al, r1, 1)),
        ?_assertAsmEqual(<<16#E24DD008:32/little>>, "sub sp, sp, #8", jit_arm32_asm:sub(al, sp, 8))
    ].

mul_test_() ->
    [
        ?_assertAsmEqual(
            <<16#E0000291:32/little>>, "mul r0, r1, r2", jit_arm32_asm:mul(al, r0, r1, r2)
        ),
        ?_assertAsmEqual(
            <<16#E0020093:32/little>>, "mul r2, r3, r0", jit_arm32_asm:mul(al, r2, r3, r0)
        )
    ].

and_test_() ->
    [
        %% AND register
        ?_assertAsmEqual(
            <<16#E0010002:32/little>>, "and r0, r1, r2", jit_arm32_asm:and_(al, r0, r1, r2)
        ),
        %% AND immediate
        ?_assertAsmEqual(
            <<16#E20000FF:32/little>>, "and r0, r0, #255", jit_arm32_asm:and_(al, r0, 255)
        )
    ].

orr_test_() ->
    [
        %% ORR register
        ?_assertAsmEqual(
            <<16#E1810002:32/little>>, "orr r0, r1, r2", jit_arm32_asm:orr(al, r0, r1, r2)
        ),
        %% ORR immediate
        ?_assertAsmEqual(
            <<16#E38000FF:32/little>>, "orr r0, r0, #255", jit_arm32_asm:orr(al, r0, 255)
        )
    ].

eor_test_() ->
    [
        %% EOR register
        ?_assertAsmEqual(
            <<16#E0210002:32/little>>, "eor r0, r1, r2", jit_arm32_asm:eor(al, r0, r1, r2)
        ),
        %% EOR immediate
        ?_assertAsmEqual(
            <<16#E22000FF:32/little>>, "eor r0, r0, #255", jit_arm32_asm:eor(al, r0, 255)
        )
    ].

bic_test_() ->
    [
        %% BIC register
        ?_assertAsmEqual(
            <<16#E1C10002:32/little>>, "bic r0, r1, r2", jit_arm32_asm:bic(al, r0, r1, r2)
        ),
        %% BIC immediate
        ?_assertAsmEqual(<<16#E3C00003:32/little>>, "bic r0, r0, #3", jit_arm32_asm:bic(al, r0, 3))
    ].

mvn_test_() ->
    [
        %% MVN register
        ?_assertAsmEqual(<<16#E1E00001:32/little>>, "mvn r0, r1", jit_arm32_asm:mvn(al, r0, r1)),
        %% MVN immediate
        ?_assertAsmEqual(<<16#E3E000FF:32/little>>, "mvn r0, #255", jit_arm32_asm:mvn(al, r0, 255))
    ].

mov_test_() ->
    [
        %% MOV register
        ?_assertAsmEqual(<<16#E1A00001:32/little>>, "mov r0, r1", jit_arm32_asm:mov(al, r0, r1)),
        ?_assertAsmEqual(<<16#E1A01002:32/little>>, "mov r1, r2", jit_arm32_asm:mov(al, r1, r2)),
        %% MOV immediate
        ?_assertAsmEqual(<<16#E3A00000:32/little>>, "mov r0, #0", jit_arm32_asm:mov(al, r0, 0)),
        ?_assertAsmEqual(<<16#E3A010FF:32/little>>, "mov r1, #255", jit_arm32_asm:mov(al, r1, 255)),
        %% MOV with condition
        ?_assertAsmEqual(<<16#01A00001:32/little>>, "moveq r0, r1", jit_arm32_asm:mov(eq, r0, r1)),
        %% Convenience 2-arg MOV
        ?_assertAsmEqual(<<16#E1A00001:32/little>>, "mov r0, r1", jit_arm32_asm:mov(r0, r1))
    ].

cmp_test_() ->
    [
        %% CMP register
        ?_assertAsmEqual(<<16#E1500001:32/little>>, "cmp r0, r1", jit_arm32_asm:cmp(al, r0, r1)),
        ?_assertAsmEqual(<<16#E1570003:32/little>>, "cmp r7, r3", jit_arm32_asm:cmp(al, r7, r3)),
        %% CMP immediate
        ?_assertAsmEqual(<<16#E3500000:32/little>>, "cmp r0, #0", jit_arm32_asm:cmp(al, r0, 0)),
        ?_assertAsmEqual(<<16#E3500005:32/little>>, "cmp r0, #5", jit_arm32_asm:cmp(al, r0, 5)),
        ?_assertAsmEqual(<<16#E35700FF:32/little>>, "cmp r7, #255", jit_arm32_asm:cmp(al, r7, 255))
    ].

tst_test_() ->
    [
        %% TST register
        ?_assertAsmEqual(<<16#E1100001:32/little>>, "tst r0, r1", jit_arm32_asm:tst(al, r0, r1)),
        ?_assertAsmEqual(<<16#E1120003:32/little>>, "tst r2, r3", jit_arm32_asm:tst(al, r2, r3)),
        %% TST immediate
        ?_assertAsmEqual(<<16#E3100001:32/little>>, "tst r0, #1", jit_arm32_asm:tst(al, r0, 1))
    ].

ldr_test_() ->
    [
        %% LDR with positive immediate offset
        ?_assertAsmEqual(
            <<16#E5910008:32/little>>, "ldr r0, [r1, #8]", jit_arm32_asm:ldr(al, r0, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#E5901018:32/little>>, "ldr r1, [r0, #24]", jit_arm32_asm:ldr(al, r1, {r0, 24})
        ),
        %% LDR with zero offset
        ?_assertAsmEqual(
            <<16#E5910000:32/little>>, "ldr r0, [r1]", jit_arm32_asm:ldr(al, r0, {r1, 0})
        ),
        %% LDR with register offset
        ?_assertAsmEqual(
            <<16#E7910002:32/little>>, "ldr r0, [r1, r2]", jit_arm32_asm:ldr(al, r0, {r1, r2})
        ),
        %% SP-relative load
        ?_assertAsmEqual(
            <<16#E59D0004:32/little>>, "ldr r0, [sp, #4]", jit_arm32_asm:ldr(al, r0, {sp, 4})
        ),
        %% LDR with negative offset
        ?_assertAsmEqual(
            <<16#E5110008:32/little>>, "ldr r0, [r1, #-8]", jit_arm32_asm:ldr(al, r0, {r1, -8})
        )
    ].

str_test_() ->
    [
        %% STR with positive immediate offset
        ?_assertAsmEqual(
            <<16#E5810008:32/little>>, "str r0, [r1, #8]", jit_arm32_asm:str(al, r0, {r1, 8})
        ),
        ?_assertAsmEqual(
            <<16#E5801018:32/little>>, "str r1, [r0, #24]", jit_arm32_asm:str(al, r1, {r0, 24})
        ),
        %% STR with zero offset
        ?_assertAsmEqual(
            <<16#E5810000:32/little>>, "str r0, [r1]", jit_arm32_asm:str(al, r0, {r1, 0})
        ),
        %% STR with register offset
        ?_assertAsmEqual(
            <<16#E7810002:32/little>>, "str r0, [r1, r2]", jit_arm32_asm:str(al, r0, {r1, r2})
        ),
        %% SP-relative store
        ?_assertAsmEqual(
            <<16#E58D0004:32/little>>, "str r0, [sp, #4]", jit_arm32_asm:str(al, r0, {sp, 4})
        )
    ].

lsl_test_() ->
    [
        %% LSL by immediate
        ?_assertAsmEqual(
            <<16#E1A00281:32/little>>, "lsl r0, r1, #5", jit_arm32_asm:lsl(al, r0, r1, 5)
        ),
        ?_assertAsmEqual(
            <<16#E1A02402:32/little>>, "lsl r2, r2, #8", jit_arm32_asm:lsl(al, r2, r2, 8)
        ),
        %% LSL by register
        ?_assertAsmEqual(<<16#E1A02312:32/little>>, "lsl r2, r2, r3", jit_arm32_asm:lsl(al, r2, r3))
    ].

lsr_test_() ->
    [
        %% LSR by immediate
        ?_assertAsmEqual(
            <<16#E1A002A1:32/little>>, "lsr r0, r1, #5", jit_arm32_asm:lsr(al, r0, r1, 5)
        ),
        ?_assertAsmEqual(
            <<16#E1A02422:32/little>>, "lsr r2, r2, #8", jit_arm32_asm:lsr(al, r2, r2, 8)
        ),
        %% LSR by register
        ?_assertAsmEqual(<<16#E1A02332:32/little>>, "lsr r2, r2, r3", jit_arm32_asm:lsr(al, r2, r3))
    ].

asr_test_() ->
    [
        %% ASR by immediate
        ?_assertAsmEqual(
            <<16#E1A002C1:32/little>>, "asr r0, r1, #5", jit_arm32_asm:asr(al, r0, r1, 5)
        ),
        ?_assertAsmEqual(
            <<16#E1A02442:32/little>>, "asr r2, r2, #8", jit_arm32_asm:asr(al, r2, r2, 8)
        ),
        ?_assertAsmEqual(
            <<16#E1A00040:32/little>>, "asr r0, r0, #32", jit_arm32_asm:asr(al, r0, r0, 32)
        ),
        %% ASR by register
        ?_assertAsmEqual(<<16#E1A02352:32/little>>, "asr r2, r2, r3", jit_arm32_asm:asr(al, r2, r3))
    ].

b_test_() ->
    [
        %% B (unconditional) - offset 0 means branch to self (PC+8-8=PC)
        ?_assertAsmEqual(<<16#EAFFFFFE:32/little>>, "b .+0", jit_arm32_asm:b(al, 0)),
        %% B forward
        ?_assertAsmEqual(<<16#EA000002:32/little>>, "b .+16", jit_arm32_asm:b(al, 16)),
        %% B backward
        ?_assertAsmEqual(<<16#EAFFFFF8:32/little>>, "b .-24", jit_arm32_asm:b(al, -24)),
        %% B with condition
        ?_assertAsmEqual(<<16#0AFFFFFE:32/little>>, "beq .+0", jit_arm32_asm:b(eq, 0)),
        ?_assertAsmEqual(<<16#1AFFFFFE:32/little>>, "bne .+0", jit_arm32_asm:b(ne, 0)),
        ?_assertAsmEqual(<<16#AAFFFFFE:32/little>>, "bge .+0", jit_arm32_asm:b(ge, 0)),
        ?_assertAsmEqual(<<16#BAFFFFFE:32/little>>, "blt .+0", jit_arm32_asm:b(lt, 0)),
        ?_assertAsmEqual(<<16#CAFFFFFE:32/little>>, "bgt .+0", jit_arm32_asm:b(gt, 0)),
        ?_assertAsmEqual(<<16#DAFFFFFE:32/little>>, "ble .+0", jit_arm32_asm:b(le, 0))
    ].

blx_test_() ->
    [
        %% BLX (branch with link and exchange)
        ?_assertAsmEqual(<<16#E12FFF30:32/little>>, "blx r0", jit_arm32_asm:blx(al, r0)),
        ?_assertAsmEqual(<<16#E12FFF31:32/little>>, "blx r1", jit_arm32_asm:blx(al, r1))
    ].

push_test_() ->
    [
        %% single-register PUSH = STR Rd, [SP, #-4]!
        ?_assertAsmEqual(<<16#E52D0004:32/little>>, "push {r0}", jit_arm32_asm:push([r0])),
        %% gcc as uses STMDB for push {sp}
        ?_assertAsmEqual(<<16#E92D2000:32/little>>, "push {sp}", jit_arm32_asm:push([sp])),
        %% multi-register PUSH = STMDB SP!
        ?_assertAsmEqual(
            <<16#E92D0007:32/little>>, "push {r0, r1, r2}", jit_arm32_asm:push([r0, r1, r2])
        ),
        ?_assertAsmEqual(<<16#E52DE004:32/little>>, "push {lr}", jit_arm32_asm:push([lr])),
        ?_assertAsmEqual(
            <<16#E92D4007:32/little>>, "push {r0, r1, r2, lr}", jit_arm32_asm:push([r0, r1, r2, lr])
        ),
        ?_assertAsmEqual(
            <<16#E92D40F0:32/little>>,
            "push {r4, r5, r6, r7, lr}",
            jit_arm32_asm:push([r4, r5, r6, r7, lr])
        )
    ].

pop_test_() ->
    [
        %% single-register POP = LDR Rd, [SP], #4
        ?_assertAsmEqual(<<16#E49D0004:32/little>>, "pop {r0}", jit_arm32_asm:pop([r0])),
        %% gcc as accepts to encode this, even if behavior is undefined
        ?_assertAsmEqual(<<16#E49DD004:32/little>>, "pop {sp}", jit_arm32_asm:pop([sp])),
        %% multi-register POP = LDMIA SP!
        ?_assertAsmEqual(
            <<16#E8BD0007:32/little>>, "pop {r0, r1, r2}", jit_arm32_asm:pop([r0, r1, r2])
        ),
        ?_assertAsmEqual(<<16#E49DF004:32/little>>, "pop {pc}", jit_arm32_asm:pop([pc])),
        ?_assertAsmEqual(
            <<16#E8BD8007:32/little>>, "pop {r0, r1, r2, pc}", jit_arm32_asm:pop([r0, r1, r2, pc])
        ),
        ?_assertAsmEqual(
            <<16#E8BD80F0:32/little>>,
            "pop {r4, r5, r6, r7, pc}",
            jit_arm32_asm:pop([r4, r5, r6, r7, pc])
        )
    ].

bkpt_test_() ->
    [
        ?_assertAsmEqual(<<16#E1200070:32/little>>, "bkpt #0", jit_arm32_asm:bkpt(0)),
        ?_assertAsmEqual(<<16#E1200071:32/little>>, "bkpt #1", jit_arm32_asm:bkpt(1)),
        ?_assertAsmEqual(<<16#E12FFF7F:32/little>>, "bkpt #65535", jit_arm32_asm:bkpt(65535))
    ].
