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

-module(jit_riscv32_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(riscv32, Bin, Str), Value)
).

%%-----------------------------------------------------------------------------
%% R-type arithmetic and logical instruction tests
%%-----------------------------------------------------------------------------

add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00628533:32/little>>, "add a0, t0, t1", jit_riscv32_asm:add(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00a585b3:32/little>>, "add a1, a1, a0", jit_riscv32_asm:add(a1, a1, a0)
        ),
        ?_assertAsmEqual(
            <<16#01e787b3:32/little>>, "add a5, a5, t5", jit_riscv32_asm:add(a5, a5, t5)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#40628533:32/little>>, "sub a0, t0, t1", jit_riscv32_asm:sub(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#40a585b3:32/little>>, "sub a1, a1, a0", jit_riscv32_asm:sub(a1, a1, a0)
        ),
        ?_assertAsmEqual(
            <<16#41e787b3:32/little>>, "sub a5, a5, t5", jit_riscv32_asm:sub(a5, a5, t5)
        )
    ].

and_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062f533:32/little>>, "and a0, t0, t1", jit_riscv32_asm:and_(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5f5b3:32/little>>, "and a1, a1, a2", jit_riscv32_asm:and_(a1, a1, a2)
        )
    ].

or_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062e533:32/little>>, "or a0, t0, t1", jit_riscv32_asm:or_(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5e5b3:32/little>>, "or a1, a1, a2", jit_riscv32_asm:or_(a1, a1, a2)
        )
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062c533:32/little>>, "xor a0, t0, t1", jit_riscv32_asm:xor_(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5c5b3:32/little>>, "xor a1, a1, a2", jit_riscv32_asm:xor_(a1, a1, a2)
        )
    ].

sll_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00629533:32/little>>, "sll a0, t0, t1", jit_riscv32_asm:sll(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c59633:32/little>>, "sll a2, a1, a2", jit_riscv32_asm:sll(a2, a1, a2)
        )
    ].

srl_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062d533:32/little>>, "srl a0, t0, t1", jit_riscv32_asm:srl(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5d633:32/little>>, "srl a2, a1, a2", jit_riscv32_asm:srl(a2, a1, a2)
        )
    ].

sra_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4062d533:32/little>>, "sra a0, t0, t1", jit_riscv32_asm:sra(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#40c5d633:32/little>>, "sra a2, a1, a2", jit_riscv32_asm:sra(a2, a1, a2)
        )
    ].

slt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062a533:32/little>>, "slt a0, t0, t1", jit_riscv32_asm:slt(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5a633:32/little>>, "slt a2, a1, a2", jit_riscv32_asm:slt(a2, a1, a2)
        )
    ].

sltu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062b533:32/little>>, "sltu a0, t0, t1", jit_riscv32_asm:sltu(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#00c5b633:32/little>>, "sltu a2, a1, a2", jit_riscv32_asm:sltu(a2, a1, a2)
        )
    ].

%%-----------------------------------------------------------------------------
%% I-type immediate instruction tests
%%-----------------------------------------------------------------------------

addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#01428513:32/little>>, "addi a0, t0, 20", jit_riscv32_asm:addi(a0, t0, 20)
        ),
        ?_assertAsmEqual(
            <<16#fff58593:32/little>>, "addi a1, a1, -1", jit_riscv32_asm:addi(a1, a1, -1)
        ),
        ?_assertAsmEqual(
            <<16#7ff00513:32/little>>, "addi a0, zero, 2047", jit_riscv32_asm:addi(a0, zero, 2047)
        ),
        ?_assertAsmEqual(
            <<16#80000593:32/little>>, "addi a1, zero, -2048", jit_riscv32_asm:addi(a1, zero, -2048)
        )
    ].

andi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2f513:32/little>>, "andi a0, t0, 255", jit_riscv32_asm:andi(a0, t0, 255)
        ),
        ?_assertAsmEqual(
            <<16#00f5f593:32/little>>, "andi a1, a1, 15", jit_riscv32_asm:andi(a1, a1, 15)
        )
    ].

ori_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2e513:32/little>>, "ori a0, t0, 255", jit_riscv32_asm:ori(a0, t0, 255)
        ),
        ?_assertAsmEqual(
            <<16#00f5e593:32/little>>, "ori a1, a1, 15", jit_riscv32_asm:ori(a1, a1, 15)
        )
    ].

xori_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0ff2c513:32/little>>, "xori a0, t0, 255", jit_riscv32_asm:xori(a0, t0, 255)
        ),
        ?_assertAsmEqual(
            <<16#fff5c593:32/little>>, "xori a1, a1, -1", jit_riscv32_asm:xori(a1, a1, -1)
        )
    ].

slli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00329513:32/little>>, "slli a0, t0, 3", jit_riscv32_asm:slli(a0, t0, 3)
        ),
        ?_assertAsmEqual(
            <<16#01f59593:32/little>>, "slli a1, a1, 31", jit_riscv32_asm:slli(a1, a1, 31)
        ),
        ?_assertAsmEqual(
            <<16#00051513:32/little>>, "slli a0, a0, 0", jit_riscv32_asm:slli(a0, a0, 0)
        )
    ].

srli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0032d513:32/little>>, "srli a0, t0, 3", jit_riscv32_asm:srli(a0, t0, 3)
        ),
        ?_assertAsmEqual(
            <<16#01f5d593:32/little>>, "srli a1, a1, 31", jit_riscv32_asm:srli(a1, a1, 31)
        )
    ].

srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4032d513:32/little>>, "srai a0, t0, 3", jit_riscv32_asm:srai(a0, t0, 3)
        ),
        ?_assertAsmEqual(
            <<16#41f5d593:32/little>>, "srai a1, a1, 31", jit_riscv32_asm:srai(a1, a1, 31)
        )
    ].

slti_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0142a513:32/little>>, "slti a0, t0, 20", jit_riscv32_asm:slti(a0, t0, 20)
        ),
        ?_assertAsmEqual(
            <<16#fff5a593:32/little>>, "slti a1, a1, -1", jit_riscv32_asm:slti(a1, a1, -1)
        )
    ].

sltiu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0142b513:32/little>>, "sltiu a0, t0, 20", jit_riscv32_asm:sltiu(a0, t0, 20)
        ),
        ?_assertAsmEqual(
            <<16#00153513:32/little>>, "sltiu a0, a0, 1", jit_riscv32_asm:sltiu(a0, a0, 1)
        )
    ].

%%-----------------------------------------------------------------------------
%% Load instruction tests
%%-----------------------------------------------------------------------------

lw_test_() ->
    [
        ?_assertAsmEqual(<<16#00052503:32/little>>, "lw a0, 0(a0)", jit_riscv32_asm:lw(a0, a0, 0)),
        ?_assertAsmEqual(<<16#00052503:32/little>>, "lw a0, 0(a0)", jit_riscv32_asm:lw(a0, a0)),
        ?_assertAsmEqual(<<16#00452583:32/little>>, "lw a1, 4(a0)", jit_riscv32_asm:lw(a1, a0, 4)),
        ?_assertAsmEqual(
            <<16#ffc52503:32/little>>, "lw a0, -4(a0)", jit_riscv32_asm:lw(a0, a0, -4)
        ),
        ?_assertAsmEqual(
            <<16#7ff52503:32/little>>, "lw a0, 2047(a0)", jit_riscv32_asm:lw(a0, a0, 2047)
        )
    ].

lh_test_() ->
    [
        ?_assertAsmEqual(<<16#00051503:32/little>>, "lh a0, 0(a0)", jit_riscv32_asm:lh(a0, a0, 0)),
        ?_assertAsmEqual(<<16#00051503:32/little>>, "lh a0, 0(a0)", jit_riscv32_asm:lh(a0, a0)),
        ?_assertAsmEqual(<<16#00251583:32/little>>, "lh a1, 2(a0)", jit_riscv32_asm:lh(a1, a0, 2))
    ].

lhu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00055503:32/little>>, "lhu a0, 0(a0)", jit_riscv32_asm:lhu(a0, a0, 0)
        ),
        ?_assertAsmEqual(<<16#00055503:32/little>>, "lhu a0, 0(a0)", jit_riscv32_asm:lhu(a0, a0)),
        ?_assertAsmEqual(<<16#00255583:32/little>>, "lhu a1, 2(a0)", jit_riscv32_asm:lhu(a1, a0, 2))
    ].

lb_test_() ->
    [
        ?_assertAsmEqual(<<16#00050503:32/little>>, "lb a0, 0(a0)", jit_riscv32_asm:lb(a0, a0, 0)),
        ?_assertAsmEqual(<<16#00050503:32/little>>, "lb a0, 0(a0)", jit_riscv32_asm:lb(a0, a0)),
        ?_assertAsmEqual(<<16#00150583:32/little>>, "lb a1, 1(a0)", jit_riscv32_asm:lb(a1, a0, 1))
    ].

lbu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00054503:32/little>>, "lbu a0, 0(a0)", jit_riscv32_asm:lbu(a0, a0, 0)
        ),
        ?_assertAsmEqual(<<16#00054503:32/little>>, "lbu a0, 0(a0)", jit_riscv32_asm:lbu(a0, a0)),
        ?_assertAsmEqual(<<16#00154583:32/little>>, "lbu a1, 1(a0)", jit_riscv32_asm:lbu(a1, a0, 1))
    ].

%%-----------------------------------------------------------------------------
%% Store instruction tests
%%-----------------------------------------------------------------------------

sw_test_() ->
    [
        ?_assertAsmEqual(<<16#00b52023:32/little>>, "sw a1, 0(a0)", jit_riscv32_asm:sw(a0, a1, 0)),
        ?_assertAsmEqual(<<16#00b52023:32/little>>, "sw a1, 0(a0)", jit_riscv32_asm:sw(a1, a0)),
        ?_assertAsmEqual(<<16#00b52223:32/little>>, "sw a1, 4(a0)", jit_riscv32_asm:sw(a0, a1, 4)),
        ?_assertAsmEqual(<<16#feb52e23:32/little>>, "sw a1, -4(a0)", jit_riscv32_asm:sw(a0, a1, -4))
    ].

sh_test_() ->
    [
        ?_assertAsmEqual(<<16#00b51023:32/little>>, "sh a1, 0(a0)", jit_riscv32_asm:sh(a0, a1, 0)),
        ?_assertAsmEqual(<<16#00b51023:32/little>>, "sh a1, 0(a0)", jit_riscv32_asm:sh(a1, a0)),
        ?_assertAsmEqual(<<16#00b51123:32/little>>, "sh a1, 2(a0)", jit_riscv32_asm:sh(a0, a1, 2))
    ].

sb_test_() ->
    [
        ?_assertAsmEqual(<<16#00b50023:32/little>>, "sb a1, 0(a0)", jit_riscv32_asm:sb(a0, a1, 0)),
        ?_assertAsmEqual(<<16#00b50023:32/little>>, "sb a1, 0(a0)", jit_riscv32_asm:sb(a1, a0)),
        ?_assertAsmEqual(<<16#00b500a3:32/little>>, "sb a1, 1(a0)", jit_riscv32_asm:sb(a0, a1, 1))
    ].

%%-----------------------------------------------------------------------------
%% Branch instruction tests
%%-----------------------------------------------------------------------------

beq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00628463:32/little>>, "beq t0, t1, .+8", jit_riscv32_asm:beq(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb50ee3:32/little>>, "beq a0, a1, .-4", jit_riscv32_asm:beq(a0, a1, -4)
        ),
        ?_assertAsmEqual(
            <<16#00050063:32/little>>, "beq a0, zero, .", jit_riscv32_asm:beq(a0, zero, 0)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00629463:32/little>>, "bne t0, t1, .+8", jit_riscv32_asm:bne(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb51ee3:32/little>>, "bne a0, a1, .-4", jit_riscv32_asm:bne(a0, a1, -4)
        )
    ].

blt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062c463:32/little>>, "blt t0, t1, .+8", jit_riscv32_asm:blt(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb54ee3:32/little>>, "blt a0, a1, .-4", jit_riscv32_asm:blt(a0, a1, -4)
        )
    ].

bge_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062d463:32/little>>, "bge t0, t1, .+8", jit_riscv32_asm:bge(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb55ee3:32/little>>, "bge a0, a1, .-4", jit_riscv32_asm:bge(a0, a1, -4)
        )
    ].

bltu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062e463:32/little>>, "bltu t0, t1, .+8", jit_riscv32_asm:bltu(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb56ee3:32/little>>, "bltu a0, a1, .-4", jit_riscv32_asm:bltu(a0, a1, -4)
        )
    ].

bgeu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062f463:32/little>>, "bgeu t0, t1, .+8", jit_riscv32_asm:bgeu(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb57ee3:32/little>>, "bgeu a0, a1, .-4", jit_riscv32_asm:bgeu(a0, a1, -4)
        )
    ].

%%-----------------------------------------------------------------------------
%% Jump instruction tests
%%-----------------------------------------------------------------------------

jal_test_() ->
    [
        ?_assertAsmEqual(
            <<16#008000ef:32/little>>, "jal .+8", jit_riscv32_asm:jal(ra, 8)
        ),
        ?_assertAsmEqual(
            <<16#ffdff0ef:32/little>>, "jal .-4", jit_riscv32_asm:jal(ra, -4)
        ),
        ?_assertAsmEqual(
            <<16#00000517:32/little, 16#000500e7:32/little>>,
            "auipc a0, 0\njalr a0",
            jit_riscv32_asm:call(a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#00002517:32/little, 16#800500e7:32/little>>,
            "auipc a0, 0x2\njalr -2048(a0)",
            jit_riscv32_asm:call(a0, 16#1800)
        )
    ].

jalr_test_() ->
    [
        ?_assertAsmEqual(<<16#000500e7:32/little>>, "jalr a0", jit_riscv32_asm:jalr(ra, a0, 0)),
        ?_assertAsmEqual(<<16#000500e7:32/little>>, "jalr a0", jit_riscv32_asm:jalr(ra, a0)),
        ?_assertAsmEqual(<<16#004500e7:32/little>>, "jalr 4(a0)", jit_riscv32_asm:jalr(ra, a0, 4))
    ].

%%-----------------------------------------------------------------------------
%% Upper immediate instruction tests
%%-----------------------------------------------------------------------------

lui_test_() ->
    [
        ?_assertAsmEqual(<<16#000125b7:32/little>>, "lui a1, 18", jit_riscv32_asm:lui(a1, 18)),
        ?_assertAsmEqual(<<16#00001537:32/little>>, "lui a0, 1", jit_riscv32_asm:lui(a0, 1)),
        ?_assertAsmEqual(<<16#fffff5b7:32/little>>, "lui a1, 0xfffff", jit_riscv32_asm:lui(a1, -1))
    ].

auipc_test_() ->
    [
        ?_assertAsmEqual(<<16#00012597:32/little>>, "auipc a1, 18", jit_riscv32_asm:auipc(a1, 18)),
        ?_assertAsmEqual(<<16#00001517:32/little>>, "auipc a0, 1", jit_riscv32_asm:auipc(a0, 1))
    ].

%%-----------------------------------------------------------------------------
%% Pseudo-instruction tests
%%-----------------------------------------------------------------------------

nop_test_() ->
    [
        ?_assertAsmEqual(<<16#00000013:32/little>>, "nop", jit_riscv32_asm:nop())
    ].

li_small_test_() ->
    [
        ?_assertAsmEqual(<<16#00a00513:32/little>>, "li a0, 10", jit_riscv32_asm:li(a0, 10)),
        ?_assertAsmEqual(<<16#fff00513:32/little>>, "li a0, -1", jit_riscv32_asm:li(a0, -1)),
        ?_assertAsmEqual(<<16#7ff00513:32/little>>, "li a0, 2047", jit_riscv32_asm:li(a0, 2047))
    ].

li_large_test_() ->
    [
        % 0x12345 = 74565 - requires lui + addi
        ?_assertAsmEqual(
            <<16#00012537:32/little, 16#34550513:32/little>>,
            "lui a0, 0x12\naddi a0, a0, 0x345",
            jit_riscv32_asm:li(a0, 16#12345)
        ),
        % 0x80000000 = -2147483648 (minimum 32-bit signed)
        ?_assertAsmEqual(
            <<16#800005b7:32/little, 16#00058593:32/little>>,
            "lui a1, 0x80000\naddi a1, a1, 0",
            jit_riscv32_asm:li(a1, -16#80000000)
        ),
        % 0x7FFFFFFF = 2147483647 (maximum 32-bit signed)
        ?_assertAsmEqual(
            <<16#80000537:32/little, 16#fff50513:32/little>>,
            "lui a0, 0x80000\naddi a0, a0, -1",
            jit_riscv32_asm:li(a0, 16#7FFFFFFF)
        )
    ].

mv_test_() ->
    [
        ?_assertAsmEqual(<<16#00050513:32/little>>, "mv a0, a0", jit_riscv32_asm:mv(a0, a0)),
        ?_assertAsmEqual(<<16#00058593:32/little>>, "mv a1, a1", jit_riscv32_asm:mv(a1, a1))
    ].

not_test_() ->
    [
        ?_assertAsmEqual(<<16#fff54513:32/little>>, "not a0, a0", jit_riscv32_asm:not_(a0, a0)),
        ?_assertAsmEqual(<<16#fff5c593:32/little>>, "not a1, a1", jit_riscv32_asm:not_(a1, a1))
    ].

neg_test_() ->
    [
        ?_assertAsmEqual(<<16#40a00533:32/little>>, "neg a0, a0", jit_riscv32_asm:neg(a0, a0)),
        ?_assertAsmEqual(<<16#40b005b3:32/little>>, "neg a1, a1", jit_riscv32_asm:neg(a1, a1))
    ].

j_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0080006f:32/little>>, "j .+8", jit_riscv32_asm:j(8)
        ),
        ?_assertAsmEqual(
            <<16#ffdff06f:32/little>>, "j .-4", jit_riscv32_asm:j(-4)
        )
    ].

jr_test_() ->
    [
        ?_assertAsmEqual(<<16#00050067:32/little>>, "jr a0", jit_riscv32_asm:jr(a0)),
        ?_assertAsmEqual(<<16#00028067:32/little>>, "jr t0", jit_riscv32_asm:jr(t0))
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(<<16#00008067:32/little>>, "ret", jit_riscv32_asm:ret())
    ].

%%-----------------------------------------------------------------------------
%% M Extension (Multiply/Divide) instruction tests
%%-----------------------------------------------------------------------------

mul_test_() ->
    [
        ?_assertAsmEqual(
            <<16#02f50533:32/little>>, "mul a0, a0, a5", jit_riscv32_asm:mul(a0, a0, a5)
        ),
        ?_assertAsmEqual(
            <<16#03f60633:32/little>>, "mul a2, a2, t6", jit_riscv32_asm:mul(a2, a2, t6)
        ),
        ?_assertAsmEqual(
            <<16#026585b3:32/little>>, "mul a1, a1, t1", jit_riscv32_asm:mul(a1, a1, t1)
        ),
        ?_assertAsmEqual(
            <<16#02d282b3:32/little>>, "mul t0, t0, a3", jit_riscv32_asm:mul(t0, t0, a3)
        )
    ].

%%-----------------------------------------------------------------------------
%% System instruction tests
%%-----------------------------------------------------------------------------

ebreak_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00100073:32/little>>, "ebreak", jit_riscv32_asm:ebreak()
        )
    ].

bkpt_test_() ->
    [
        % bkpt is an ARM compatibility wrapper that generates ebreak
        % The immediate parameter is ignored
        ?_assertAsmEqual(
            <<16#00100073:32/little>>, "ebreak", jit_riscv32_asm:bkpt(0)
        ),
        ?_assertAsmEqual(
            <<16#00100073:32/little>>, "ebreak", jit_riscv32_asm:bkpt(42)
        ),
        ?_assertAsmEqual(
            <<16#00100073:32/little>>, "ebreak", jit_riscv32_asm:bkpt(255)
        )
    ].
