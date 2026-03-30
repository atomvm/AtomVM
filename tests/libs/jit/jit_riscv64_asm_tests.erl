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

-module(jit_riscv64_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(riscv64, Bin, Str), Value)
).

%%=============================================================================
%% R-type arithmetic and logical instructions (delegated to riscv32)
%%=============================================================================

add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00628533:32/little>>, "add a0, t0, t1", jit_riscv64_asm:add(a0, t0, t1)
        ),
        %% compressed: add a1, a1, a0
        ?_assertAsmEqual(
            <<16#95aa:16/little>>, "add a1, a1, a0", jit_riscv64_asm:add(a1, a1, a0)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#40628533:32/little>>, "sub a0, t0, t1", jit_riscv64_asm:sub(a0, t0, t1)
        ),
        %% compressed: sub a1, a1, a0
        ?_assertAsmEqual(
            <<16#8d89:16/little>>, "sub a1, a1, a0", jit_riscv64_asm:sub(a1, a1, a0)
        )
    ].

and_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062f533:32/little>>, "and a0, t0, t1", jit_riscv64_asm:and_(a0, t0, t1)
        ),
        %% compressed: and a1, a1, a2
        ?_assertAsmEqual(
            <<16#8df1:16/little>>, "and a1, a1, a2", jit_riscv64_asm:and_(a1, a1, a2)
        )
    ].

or_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062e533:32/little>>, "or a0, t0, t1", jit_riscv64_asm:or_(a0, t0, t1)
        ),
        %% compressed: or a1, a1, a2
        ?_assertAsmEqual(
            <<16#8dd1:16/little>>, "or a1, a1, a2", jit_riscv64_asm:or_(a1, a1, a2)
        ),
        %% or_/2 (2-arg form)
        ?_assertAsmEqual(
            <<16#8dd1:16/little>>, "or a1, a1, a2", jit_riscv64_asm:or_(a1, a2)
        )
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062c533:32/little>>, "xor a0, t0, t1", jit_riscv64_asm:xor_(a0, t0, t1)
        ),
        %% compressed: xor a1, a1, a2
        ?_assertAsmEqual(
            <<16#8db1:16/little>>, "xor a1, a1, a2", jit_riscv64_asm:xor_(a1, a1, a2)
        )
    ].

sll_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00629533:32/little>>, "sll a0, t0, t1", jit_riscv64_asm:sll(a0, t0, t1)
        )
    ].

srl_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062d533:32/little>>, "srl a0, t0, t1", jit_riscv64_asm:srl(a0, t0, t1)
        )
    ].

sra_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4062d533:32/little>>, "sra a0, t0, t1", jit_riscv64_asm:sra(a0, t0, t1)
        )
    ].

slt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062a533:32/little>>, "slt a0, t0, t1", jit_riscv64_asm:slt(a0, t0, t1)
        )
    ].

sltu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062b533:32/little>>, "sltu a0, t0, t1", jit_riscv64_asm:sltu(a0, t0, t1)
        )
    ].

%%=============================================================================
%% I-type immediate instructions (delegated to riscv32)
%%=============================================================================

addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#01428513:32/little>>, "addi a0, t0, 20", jit_riscv64_asm:addi(a0, t0, 20)
        ),
        %% compressed: c.addi
        ?_assertAsmEqual(
            <<16#15fd:16/little>>, "addi a1, a1, -1", jit_riscv64_asm:addi(a1, a1, -1)
        )
    ].

andi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2f513:32/little>>, "andi a0, t0, 255", jit_riscv64_asm:andi(a0, t0, 255)
        ),
        %% compressed: c.andi
        ?_assertAsmEqual(
            <<16#89bd:16/little>>, "andi a1, a1, 15", jit_riscv64_asm:andi(a1, a1, 15)
        )
    ].

ori_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2e513:32/little>>, "ori a0, t0, 255", jit_riscv64_asm:ori(a0, t0, 255)
        )
    ].

xori_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2c513:32/little>>, "xori a0, t0, 255", jit_riscv64_asm:xori(a0, t0, 255)
        )
    ].

slti_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0142a513:32/little>>, "slti a0, t0, 20", jit_riscv64_asm:slti(a0, t0, 20)
        )
    ].

sltiu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0142b513:32/little>>, "sltiu a0, t0, 20", jit_riscv64_asm:sltiu(a0, t0, 20)
        )
    ].

addiw_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0142851B:32/little>>, "addiw a0, t0, 20", jit_riscv64_asm:addiw(a0, t0, 20)
        ),
        ?_assertAsmEqual(
            <<16#0000051B:32/little>>, "addiw a0, zero, 0", jit_riscv64_asm:addiw(a0, zero, 0)
        ),
        ?_assertAsmEqual(
            <<16#FFF0051B:32/little>>, "addiw a0, zero, -1", jit_riscv64_asm:addiw(a0, zero, -1)
        )
    ].

%%=============================================================================
%% RV64-specific shift instructions (6-bit shift amounts)
%%=============================================================================

slli_test_() ->
    [
        %% Compressed: c.slli a0, 8 (Rd==Rs1, Rd!=zero, 1<=Shamt<=63)
        ?_assertAsmEqual(
            <<16#0522:16/little>>, "slli a0, a0, 8", jit_riscv64_asm:slli(a0, a0, 8)
        ),
        %% Compressed with 6-bit shamt (>31, RV64-specific)
        ?_assertAsmEqual(
            <<16#1502:16/little>>, "c.slli a0, 0x20", jit_riscv64_asm:slli(a0, a0, 32)
        ),
        ?_assertAsmEqual(
            <<16#157e:16/little>>, "c.slli a0, 0x3f", jit_riscv64_asm:slli(a0, a0, 63)
        ),
        %% Non-compressed: different Rd and Rs1
        ?_assertAsmEqual(
            <<16#02029513:32/little>>, "slli a0, t0, 0x20", jit_riscv64_asm:slli(a0, t0, 32)
        ),
        %% Non-compressed: shamt=0
        ?_assertAsmEqual(
            <<16#00051513:32/little>>, "slli a0, a0, 0", jit_riscv64_asm:slli(a0, a0, 0)
        )
    ].

srli_test_() ->
    [
        %% Compressed: c.srli a0, 32 (Rd==Rs1, compressed reg, 6-bit shamt)
        ?_assertAsmEqual(
            <<16#9101:16/little>>, "c.srli a0, 0x20", jit_riscv64_asm:srli(a0, a0, 32)
        ),
        %% Non-compressed: different Rd and Rs1
        ?_assertAsmEqual(
            <<16#0202d513:32/little>>, "srli a0, t0, 0x20", jit_riscv64_asm:srli(a0, t0, 32)
        ),
        %% Non-compressed: Rd==Rs1 but not compressed reg (t0)
        ?_assertAsmEqual(
            <<16#0202d293:32/little>>, "srli t0, t0, 0x20", jit_riscv64_asm:srli(t0, t0, 32)
        )
    ].

srai_test_() ->
    [
        %% Compressed: c.srai a0, 32 (Rd==Rs1, compressed reg, 6-bit shamt)
        ?_assertAsmEqual(
            <<16#9501:16/little>>, "c.srai a0, 0x20", jit_riscv64_asm:srai(a0, a0, 32)
        ),
        %% Non-compressed: different Rd and Rs1
        ?_assertAsmEqual(
            <<16#4202d513:32/little>>, "srai a0, t0, 0x20", jit_riscv64_asm:srai(a0, t0, 32)
        ),
        %% Non-compressed: Rd==Rs1 but not compressed reg (t0)
        ?_assertAsmEqual(
            <<16#4202d293:32/little>>, "srai t0, t0, 0x20", jit_riscv64_asm:srai(t0, t0, 32)
        )
    ].

%%=============================================================================
%% Load instructions (delegated to riscv32)
%%=============================================================================

lw_test_() ->
    [
        ?_assertAsmEqual(<<16#4108:16/little>>, "lw a0, 0(a0)", jit_riscv64_asm:lw(a0, a0, 0)),
        ?_assertAsmEqual(<<16#4108:16/little>>, "lw a0, 0(a0)", jit_riscv64_asm:lw(a0, a0)),
        ?_assertAsmEqual(<<16#414c:16/little>>, "lw a1, 4(a0)", jit_riscv64_asm:lw(a1, a0, 4))
    ].

lh_test_() ->
    [
        ?_assertAsmEqual(<<16#00051503:32/little>>, "lh a0, 0(a0)", jit_riscv64_asm:lh(a0, a0, 0)),
        ?_assertAsmEqual(<<16#00051503:32/little>>, "lh a0, 0(a0)", jit_riscv64_asm:lh(a0, a0))
    ].

lhu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00055503:32/little>>, "lhu a0, 0(a0)", jit_riscv64_asm:lhu(a0, a0, 0)
        ),
        ?_assertAsmEqual(<<16#00055503:32/little>>, "lhu a0, 0(a0)", jit_riscv64_asm:lhu(a0, a0))
    ].

lb_test_() ->
    [
        ?_assertAsmEqual(<<16#00050503:32/little>>, "lb a0, 0(a0)", jit_riscv64_asm:lb(a0, a0, 0)),
        ?_assertAsmEqual(<<16#00050503:32/little>>, "lb a0, 0(a0)", jit_riscv64_asm:lb(a0, a0))
    ].

lbu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00054503:32/little>>, "lbu a0, 0(a0)", jit_riscv64_asm:lbu(a0, a0, 0)
        ),
        ?_assertAsmEqual(<<16#00054503:32/little>>, "lbu a0, 0(a0)", jit_riscv64_asm:lbu(a0, a0))
    ].

%%=============================================================================
%% RV64 doubleword load/store instructions (RV64-specific)
%%=============================================================================

ld_test_() ->
    [
        %% Non-compressed: ld with non-compressed regs
        ?_assertAsmEqual(
            <<16#0002b503:32/little>>, "ld a0, 0(t0)", jit_riscv64_asm:ld(a0, t0, 0)
        ),
        ?_assertAsmEqual(
            <<16#0082b503:32/little>>, "ld a0, 8(t0)", jit_riscv64_asm:ld(a0, t0, 8)
        ),
        %% Compressed: c.ld (both regs compressed, 8-aligned, 0..248)
        ?_assertAsmEqual(
            <<16#6108:16/little>>, "ld a0, 0(a0)", jit_riscv64_asm:ld(a0, a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#650c:16/little>>, "ld a1, 8(a0)", jit_riscv64_asm:ld(a1, a0, 8)
        ),
        ?_assertAsmEqual(
            <<16#7d68:16/little>>, "ld a0, 248(a0)", jit_riscv64_asm:ld(a0, a0, 248)
        ),
        %% Compressed: c.ldsp (load from sp)
        ?_assertAsmEqual(
            <<16#6502:16/little>>, "ld a0, 0(sp)", jit_riscv64_asm:ld(a0, sp, 0)
        ),
        ?_assertAsmEqual(
            <<16#6522:16/little>>, "ld a0, 8(sp)", jit_riscv64_asm:ld(a0, sp, 8)
        ),
        ?_assertAsmEqual(
            <<16#757e:16/little>>, "ld a0, 504(sp)", jit_riscv64_asm:ld(a0, sp, 504)
        ),
        %% Negative offset (non-compressed)
        ?_assertAsmEqual(
            <<16#ff82b503:32/little>>, "ld a0, -8(t0)", jit_riscv64_asm:ld(a0, t0, -8)
        ),
        %% 2-arg form (ld Rd, Rs1 => ld Rd, Rs1, 0)
        ?_assertAsmEqual(
            <<16#6108:16/little>>, "ld a0, 0(a0)", jit_riscv64_asm:ld(a0, a0)
        ),
        %% ld with compressed reg from sp (c.ld for a1, a0)
        ?_assertAsmEqual(
            <<16#6188:16/little>>, "ld a0, 0(a1)", jit_riscv64_asm:ld(a0, a1, 0)
        )
    ].

sd_test_() ->
    [
        %% Non-compressed: sd with non-compressed regs
        ?_assertAsmEqual(
            <<16#00a2b023:32/little>>, "sd a0, 0(t0)", jit_riscv64_asm:sd(t0, a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#00a2b423:32/little>>, "sd a0, 8(t0)", jit_riscv64_asm:sd(t0, a0, 8)
        ),
        %% Compressed: c.sd (both regs compressed, 8-aligned, 0..248)
        ?_assertAsmEqual(
            <<16#e108:16/little>>, "sd a0, 0(a0)", jit_riscv64_asm:sd(a0, a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#e50c:16/little>>, "sd a1, 8(a0)", jit_riscv64_asm:sd(a0, a1, 8)
        ),
        ?_assertAsmEqual(
            <<16#fd68:16/little>>, "sd a0, 248(a0)", jit_riscv64_asm:sd(a0, a0, 248)
        ),
        %% Compressed: c.sdsp (store to sp)
        ?_assertAsmEqual(
            <<16#e02a:16/little>>, "sd a0, 0(sp)", jit_riscv64_asm:sd(sp, a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#e42a:16/little>>, "sd a0, 8(sp)", jit_riscv64_asm:sd(sp, a0, 8)
        ),
        ?_assertAsmEqual(
            <<16#ffaa:16/little>>, "sd a0, 504(sp)", jit_riscv64_asm:sd(sp, a0, 504)
        ),
        %% Negative offset (non-compressed)
        ?_assertAsmEqual(
            <<16#fea2bc23:32/little>>, "sd a0, -8(t0)", jit_riscv64_asm:sd(t0, a0, -8)
        ),
        %% 2-arg form (sd Rs2, Rs1 => sd Rs1, Rs2, 0)
        ?_assertAsmEqual(
            <<16#e188:16/little>>, "sd a0, 0(a1)", jit_riscv64_asm:sd(a0, a1)
        )
    ].

%%=============================================================================
%% RV64 load word unsigned (RV64-specific)
%%=============================================================================

lwu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0002e503:32/little>>, "lwu a0, 0(t0)", jit_riscv64_asm:lwu(a0, t0, 0)
        ),
        ?_assertAsmEqual(
            <<16#0082e503:32/little>>, "lwu a0, 8(t0)", jit_riscv64_asm:lwu(a0, t0, 8)
        ),
        %% 2-arg form
        ?_assertAsmEqual(
            <<16#0002e503:32/little>>, "lwu a0, 0(t0)", jit_riscv64_asm:lwu(a0, t0)
        )
    ].

%%=============================================================================
%% Store instructions (delegated to riscv32)
%%=============================================================================

sw_test_() ->
    [
        ?_assertAsmEqual(<<16#c10c:16/little>>, "sw a1, 0(a0)", jit_riscv64_asm:sw(a0, a1, 0)),
        ?_assertAsmEqual(<<16#c10c:16/little>>, "sw a1, 0(a0)", jit_riscv64_asm:sw(a1, a0)),
        ?_assertAsmEqual(<<16#c14c:16/little>>, "sw a1, 4(a0)", jit_riscv64_asm:sw(a0, a1, 4))
    ].

sh_test_() ->
    [
        ?_assertAsmEqual(<<16#00b51023:32/little>>, "sh a1, 0(a0)", jit_riscv64_asm:sh(a0, a1, 0)),
        ?_assertAsmEqual(<<16#00b51023:32/little>>, "sh a1, 0(a0)", jit_riscv64_asm:sh(a1, a0))
    ].

sb_test_() ->
    [
        ?_assertAsmEqual(<<16#00b50023:32/little>>, "sb a1, 0(a0)", jit_riscv64_asm:sb(a0, a1, 0)),
        ?_assertAsmEqual(<<16#00b50023:32/little>>, "sb a1, 0(a0)", jit_riscv64_asm:sb(a1, a0))
    ].

%%=============================================================================
%% Branch instructions (delegated to riscv32)
%%=============================================================================

beq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00628463:32/little>>, "beq t0, t1, .+8", jit_riscv64_asm:beq(t0, t1, 8)
        ),
        %% c.beqz (compressed reg with zero)
        ?_assertAsmEqual(
            <<16#c101:16/little>>, "beq a0, zero, .", jit_riscv64_asm:beq(a0, zero, 0)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00629463:32/little>>, "bne t0, t1, .+8", jit_riscv64_asm:bne(t0, t1, 8)
        ),
        %% c.bnez (compressed reg with zero)
        ?_assertAsmEqual(
            <<16#ea11:16/little>>, "bne a2, zero, .+20", jit_riscv64_asm:bne(a2, zero, 20)
        )
    ].

blt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062c463:32/little>>, "blt t0, t1, .+8", jit_riscv64_asm:blt(t0, t1, 8)
        )
    ].

bge_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062d463:32/little>>, "bge t0, t1, .+8", jit_riscv64_asm:bge(t0, t1, 8)
        )
    ].

bltu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062e463:32/little>>, "bltu t0, t1, .+8", jit_riscv64_asm:bltu(t0, t1, 8)
        )
    ].

bgeu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062f463:32/little>>, "bgeu t0, t1, .+8", jit_riscv64_asm:bgeu(t0, t1, 8)
        )
    ].

%%=============================================================================
%% JAL - RV64-specific (no c_jal, that opcode is c_addiw on RV64)
%%=============================================================================

jal_test_() ->
    [
        %% jal zero uses c.j (compressed)
        ?_assertAsmEqual(
            <<16#a001:16/little>>, "j .", jit_riscv64_asm:jal(zero, 0)
        ),
        ?_assertAsmEqual(
            <<16#a021:16/little>>, "j .+8", jit_riscv64_asm:jal(zero, 8)
        ),
        %% jal ra does NOT use c.jal on RV64, uses full J-type
        ?_assertAsmEqual(
            <<16#008000ef:32/little>>, "jal .+8", jit_riscv64_asm:jal(ra, 8)
        ),
        %% jal to other register
        ?_assertAsmEqual(
            <<16#008005ef:32/little>>, "jal a1, .+8", jit_riscv64_asm:jal(a1, 8)
        )
    ].

%%=============================================================================
%% Jump instructions (delegated/RV64-specific)
%%=============================================================================

jalr_test_() ->
    [
        ?_assertAsmEqual(<<16#9502:16/little>>, "jalr a0", jit_riscv64_asm:jalr(ra, a0, 0)),
        ?_assertAsmEqual(<<16#9502:16/little>>, "jalr a0", jit_riscv64_asm:jalr(ra, a0)),
        ?_assertAsmEqual(
            <<16#004500e7:32/little>>, "jalr 4(a0)", jit_riscv64_asm:jalr(ra, a0, 4)
        )
    ].

%%=============================================================================
%% Upper immediate (delegated to riscv32)
%%=============================================================================

lui_test_() ->
    [
        ?_assertAsmEqual(<<16#6505:16/little>>, "lui a0, 1", jit_riscv64_asm:lui(a0, 1)),
        ?_assertAsmEqual(<<16#65c9:16/little>>, "lui a1, 18", jit_riscv64_asm:lui(a1, 18))
    ].

auipc_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00001517:32/little>>, "auipc a0, 1", jit_riscv64_asm:auipc(a0, 1)
        )
    ].

%%=============================================================================
%% Pseudo-instructions
%%=============================================================================

nop_test_() ->
    [
        %% Our nop emits a 4-byte NOP (addi x0, x0, 0)
        %% Use .option norvc to prevent GNU as from compressing to c.nop
        ?_assertAsmEqual(
            <<16#00000013:32/little>>, ".option norvc\nnop", jit_riscv64_asm:nop()
        )
    ].

li_test_() ->
    [
        %% Small positive immediate (c.li)
        ?_assertAsmEqual(
            <<16#4529:16/little>>, "li a0, 10", jit_riscv64_asm:li(a0, 10)
        ),
        %% Negative immediate (c.li)
        ?_assertAsmEqual(
            <<16#5575:16/little>>, "li a0, -3", jit_riscv64_asm:li(a0, -3)
        ),
        %% 12-bit immediate (addi from zero)
        ?_assertAsmEqual(
            <<16#10000513:32/little>>, "li a0, 256", jit_riscv64_asm:li(a0, 256)
        ),
        %% Larger positive (addi from zero, near limit)
        ?_assertAsmEqual(
            <<16#7ff00513:32/little>>, "li a0, 2047", jit_riscv64_asm:li(a0, 2047)
        ),
        %% Negative 12-bit
        ?_assertAsmEqual(
            <<16#80000513:32/little>>, "li a0, -2048", jit_riscv64_asm:li(a0, -2048)
        ),
        %% Values near top of signed 32-bit range (0x7FFFF800 - 0x7FFFFFFF)
        %% These must go through unsigned 32-bit path (lui+addi+slli+srli)
        %% to avoid sign extension corruption on RV64.
        %% On RV64, GNU as uses addiw for li with these values, but we use
        %% slli+srli to clear upper bits instead. Use explicit instruction
        %% sequences to validate against cross-binutils.
        ?_assertAsmEqual(
            <<16#80000537:32/little, 16#157D:16/little, 16#1502:16/little, 16#9101:16/little>>,
            "lui a0, 0x80000\nc.addi a0, -1\nc.slli a0, 32\nc.srli a0, 32",
            jit_riscv64_asm:li(a0, 16#7FFFFFFF)
        ),
        ?_assertAsmEqual(
            <<16#80000537:32/little, 16#80050513:32/little, 16#1502:16/little, 16#9101:16/little>>,
            "lui a0, 0x80000\naddi a0, a0, -2048\nc.slli a0, 32\nc.srli a0, 32",
            jit_riscv64_asm:li(a0, 16#7FFFF800)
        ),
        %% Value just below the boundary (uses li_32bit path: lui+addiw)
        ?_assertAsmEqual(
            <<16#7FFFF537:32/little, 16#7FF5051B:32/little>>,
            "li a0, 0x7FFFF7FF",
            jit_riscv64_asm:li(a0, 16#7FFFF7FF)
        ),
        %% Exact boundary 0x80000000 (unsigned 32-bit path: lui+slli+srli)
        ?_assertAsmEqual(
            <<16#80000537:32/little, 16#1502:16/little, 16#9101:16/little>>,
            "lui a0, 0x80000\nc.slli a0, 32\nc.srli a0, 32",
            jit_riscv64_asm:li(a0, 16#80000000)
        ),
        %% 64-bit: 0x100000000 (1 << 32) -- li_32bit(1) = addiw a0,zero,1; then slli 32
        ?_assertAsmEqual(
            <<16#0010051B:32/little, 16#1502:16/little>>,
            "addiw a0, zero, 1\nc.slli a0, 32",
            jit_riscv64_asm:li(a0, 16#100000000)
        ),
        %% Negative 64-bit: -1 (0xFFFFFFFFFFFFFFFF)
        ?_assertAsmEqual(
            <<16#557D:16/little>>,
            "li a0, -1",
            jit_riscv64_asm:li(a0, -1)
        )
    ].

mv_test_() ->
    [
        ?_assertAsmEqual(
            <<16#852a:16/little>>, "mv a0, a0", jit_riscv64_asm:mv(a0, a0)
        ),
        ?_assertAsmEqual(
            <<16#85ae:16/little>>, "mv a1, a1", jit_riscv64_asm:mv(a1, a1)
        )
    ].

not_test_() ->
    [
        ?_assertAsmEqual(
            <<16#fff54513:32/little>>, "not a0, a0", jit_riscv64_asm:not_(a0, a0)
        )
    ].

neg_test_() ->
    [
        ?_assertAsmEqual(
            <<16#40a00533:32/little>>, "neg a0, a0", jit_riscv64_asm:neg(a0, a0)
        )
    ].

j_test_() ->
    [
        ?_assertAsmEqual(
            <<16#a021:16/little>>, "j .+8", jit_riscv64_asm:j(8)
        ),
        ?_assertAsmEqual(
            <<16#bff5:16/little>>, "j .-4", jit_riscv64_asm:j(-4)
        )
    ].

jr_test_() ->
    [
        ?_assertAsmEqual(<<16#8502:16/little>>, "jr a0", jit_riscv64_asm:jr(a0)),
        ?_assertAsmEqual(<<16#8282:16/little>>, "jr t0", jit_riscv64_asm:jr(t0))
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8082:16/little>>, "ret", jit_riscv64_asm:ret()
        )
    ].

call_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00000517:32/little, 16#9502:16/little>>,
            "auipc a0, 0\njalr a0",
            jit_riscv64_asm:call(a0, 0)
        )
    ].

%%=============================================================================
%% M Extension (delegated to riscv32)
%%=============================================================================

mul_test_() ->
    [
        ?_assertAsmEqual(
            <<16#02f50533:32/little>>, "mul a0, a0, a5", jit_riscv64_asm:mul(a0, a0, a5)
        )
    ].

div_test_() ->
    [
        ?_assertAsmEqual(
            <<16#02f54533:32/little>>, "div a0, a0, a5", jit_riscv64_asm:div_(a0, a0, a5)
        )
    ].

rem_test_() ->
    [
        ?_assertAsmEqual(
            <<16#02f56533:32/little>>, "rem a0, a0, a5", jit_riscv64_asm:rem_(a0, a0, a5)
        )
    ].

%%=============================================================================
%% C Extension - Register-register (delegated to riscv32)
%%=============================================================================

c_add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9532:16/little>>, "c.add a0, a2", jit_riscv64_asm:c_add(a0, a2)
        )
    ].

c_sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d09:16/little>>, "c.sub a0, a0", jit_riscv64_asm:c_sub(a0, a0)
        )
    ].

c_and_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d6d:16/little>>, "c.and a0, a1", jit_riscv64_asm:c_and(a0, a1)
        )
    ].

c_or_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d4d:16/little>>, "c.or a0, a1", jit_riscv64_asm:c_or(a0, a1)
        )
    ].

c_xor_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d2d:16/little>>, "c.xor a0, a1", jit_riscv64_asm:c_xor(a0, a1)
        )
    ].

c_mv_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8532:16/little>>, "c.mv a0, a2", jit_riscv64_asm:c_mv(a0, a2)
        )
    ].

%%=============================================================================
%% C Extension - Immediate (delegated to riscv32)
%%=============================================================================

c_addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0511:16/little>>, "c.addi a0, 4", jit_riscv64_asm:c_addi(a0, 4)
        )
    ].

c_andi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8929:16/little>>, "c.andi a0, 10", jit_riscv64_asm:c_andi(a0, 10)
        )
    ].

c_li_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4529:16/little>>, "c.li a0, 10", jit_riscv64_asm:c_li(a0, 10)
        )
    ].

c_lui_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6505:16/little>>, "c.lui a0, 1", jit_riscv64_asm:c_lui(a0, 1)
        )
    ].

c_addi16sp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6141:16/little>>, "c.addi16sp sp, 16", jit_riscv64_asm:c_addi16sp(16)
        )
    ].

c_addi4spn_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0048:16/little>>, "c.addi4spn a0, sp, 4", jit_riscv64_asm:c_addi4spn(a0, 4)
        )
    ].

%%=============================================================================
%% C Extension - Shifts (delegated to riscv32, 6-bit shamt)
%%=============================================================================

c_slli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#050e:16/little>>, "c.slli a0, 3", jit_riscv64_asm:c_slli(a0, 3)
        ),
        %% 6-bit shamt (>31, valid on RV64)
        ?_assertAsmEqual(
            <<16#1502:16/little>>, "c.slli a0, 0x20", jit_riscv64_asm:c_slli(a0, 32)
        )
    ].

c_srli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#810d:16/little>>, "c.srli a0, 3", jit_riscv64_asm:c_srli(a0, 3)
        ),
        ?_assertAsmEqual(
            <<16#9101:16/little>>, "c.srli a0, 0x20", jit_riscv64_asm:c_srli(a0, 32)
        )
    ].

c_srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#850d:16/little>>, "c.srai a0, 3", jit_riscv64_asm:c_srai(a0, 3)
        ),
        ?_assertAsmEqual(
            <<16#9501:16/little>>, "c.srai a0, 0x20", jit_riscv64_asm:c_srai(a0, 32)
        )
    ].

%%=============================================================================
%% C Extension - Word loads/stores (delegated to riscv32)
%%=============================================================================

c_lw_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4188:16/little>>, "c.lw a0, 0(a1)", jit_riscv64_asm:c_lw(a0, {a1, 0})
        )
    ].

c_sw_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c188:16/little>>, "c.sw a0, 0(a1)", jit_riscv64_asm:c_sw(a0, {a1, 0})
        )
    ].

c_lwsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4502:16/little>>, "c.lwsp a0, 0(sp)", jit_riscv64_asm:c_lwsp(a0, 0)
        )
    ].

c_swsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c02a:16/little>>, "c.swsp a0, 0(sp)", jit_riscv64_asm:c_swsp(a0, 0)
        )
    ].

%%=============================================================================
%% C Extension - RV64 Doubleword loads/stores (RV64-specific)
%%=============================================================================

c_ld_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6108:16/little>>, "c.ld a0, 0(a0)", jit_riscv64_asm:c_ld(a0, {a0, 0})
        ),
        ?_assertAsmEqual(
            <<16#650c:16/little>>, "c.ld a1, 8(a0)", jit_riscv64_asm:c_ld(a1, {a0, 8})
        ),
        ?_assertAsmEqual(
            <<16#7d68:16/little>>, "c.ld a0, 248(a0)", jit_riscv64_asm:c_ld(a0, {a0, 248})
        )
    ].

c_sd_test_() ->
    [
        ?_assertAsmEqual(
            <<16#e108:16/little>>, "c.sd a0, 0(a0)", jit_riscv64_asm:c_sd(a0, {a0, 0})
        ),
        ?_assertAsmEqual(
            <<16#e50c:16/little>>, "c.sd a1, 8(a0)", jit_riscv64_asm:c_sd(a1, {a0, 8})
        ),
        ?_assertAsmEqual(
            <<16#fd68:16/little>>, "c.sd a0, 248(a0)", jit_riscv64_asm:c_sd(a0, {a0, 248})
        )
    ].

c_ldsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6502:16/little>>, "c.ldsp a0, 0(sp)", jit_riscv64_asm:c_ldsp(a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#6522:16/little>>, "c.ldsp a0, 8(sp)", jit_riscv64_asm:c_ldsp(a0, 8)
        ),
        ?_assertAsmEqual(
            <<16#757e:16/little>>, "c.ldsp a0, 504(sp)", jit_riscv64_asm:c_ldsp(a0, 504)
        )
    ].

c_sdsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#e02a:16/little>>, "c.sdsp a0, 0(sp)", jit_riscv64_asm:c_sdsp(a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#e42a:16/little>>, "c.sdsp a0, 8(sp)", jit_riscv64_asm:c_sdsp(a0, 8)
        ),
        ?_assertAsmEqual(
            <<16#ffaa:16/little>>, "c.sdsp a0, 504(sp)", jit_riscv64_asm:c_sdsp(a0, 504)
        )
    ].

%%=============================================================================
%% C Extension - Branch and Jump (delegated to riscv32)
%%=============================================================================

c_beqz_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c111:16/little>>, "c.beqz a0, .+4", jit_riscv64_asm:c_beqz(a0, 4)
        )
    ].

c_bnez_test_() ->
    [
        ?_assertAsmEqual(
            <<16#e111:16/little>>, "c.bnez a0, .+4", jit_riscv64_asm:c_bnez(a0, 4)
        )
    ].

c_j_test_() ->
    [
        ?_assertAsmEqual(
            <<16#a011:16/little>>, "c.j .+4", jit_riscv64_asm:c_j(4)
        )
    ].

c_jr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8502:16/little>>, "c.jr a0", jit_riscv64_asm:c_jr(a0)
        )
    ].

c_jalr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9502:16/little>>, "c.jalr a0", jit_riscv64_asm:c_jalr(a0)
        )
    ].

%%=============================================================================
%% C Extension - System (delegated to riscv32)
%%=============================================================================

c_ebreak_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9002:16/little>>, "c.ebreak", jit_riscv64_asm:c_ebreak()
        )
    ].

c_nop_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0001:16/little>>, "c.nop", jit_riscv64_asm:c_nop()
        )
    ].
