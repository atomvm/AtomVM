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
            <<16#95aa:16/little>>, "add a1, a1, a0", jit_riscv32_asm:add(a1, a1, a0)
        ),
        ?_assertAsmEqual(
            <<16#97fa:16/little>>, "add a5, a5, t5", jit_riscv32_asm:add(a5, a5, t5)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#40628533:32/little>>, "sub a0, t0, t1", jit_riscv32_asm:sub(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#8d89:16/little>>, "sub a1, a1, a0", jit_riscv32_asm:sub(a1, a1, a0)
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
            <<16#8df1:16/little>>, "and a1, a1, a2", jit_riscv32_asm:and_(a1, a1, a2)
        )
    ].

or_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062e533:32/little>>, "or a0, t0, t1", jit_riscv32_asm:or_(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#8dd1:16/little>>, "or a1, a1, a2", jit_riscv32_asm:or_(a1, a1, a2)
        )
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0062c533:32/little>>, "xor a0, t0, t1", jit_riscv32_asm:xor_(a0, t0, t1)
        ),
        ?_assertAsmEqual(
            <<16#8db1:16/little>>, "xor a1, a1, a2", jit_riscv32_asm:xor_(a1, a1, a2)
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
            <<16#15fd:16/little>>, "addi a1, a1, -1", jit_riscv32_asm:addi(a1, a1, -1)
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
            <<16#89bd:16/little>>, "andi a1, a1, 15", jit_riscv32_asm:andi(a1, a1, 15)
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
            <<16#05fe:16/little>>, "slli a1, a1, 31", jit_riscv32_asm:slli(a1, a1, 31)
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
            <<16#81fd:16/little>>, "srli a1, a1, 31", jit_riscv32_asm:srli(a1, a1, 31)
        )
    ].

srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4032d513:32/little>>, "srai a0, t0, 3", jit_riscv32_asm:srai(a0, t0, 3)
        ),
        ?_assertAsmEqual(
            <<16#85fd:16/little>>, "srai a1, a1, 31", jit_riscv32_asm:srai(a1, a1, 31)
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
        ?_assertAsmEqual(<<16#4108:16/little>>, "lw a0, 0(a0)", jit_riscv32_asm:lw(a0, a0, 0)),
        ?_assertAsmEqual(<<16#4108:16/little>>, "lw a0, 0(a0)", jit_riscv32_asm:lw(a0, a0)),
        ?_assertAsmEqual(<<16#414c:16/little>>, "lw a1, 4(a0)", jit_riscv32_asm:lw(a1, a0, 4)),
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
        ?_assertAsmEqual(<<16#c10c:16/little>>, "sw a1, 0(a0)", jit_riscv32_asm:sw(a0, a1, 0)),
        ?_assertAsmEqual(<<16#c10c:16/little>>, "sw a1, 0(a0)", jit_riscv32_asm:sw(a1, a0)),
        ?_assertAsmEqual(<<16#c14c:16/little>>, "sw a1, 4(a0)", jit_riscv32_asm:sw(a0, a1, 4)),
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
        % Test c.beqz (compressed reg with zero)
        ?_assertAsmEqual(
            <<16#c101:16/little>>, "beq a0, zero, .", jit_riscv32_asm:beq(a0, zero, 0)
        ),
        ?_assertAsmEqual(
            <<16#cf81:16/little>>, "beq a5, zero, .+24", jit_riscv32_asm:beq(a5, zero, 24)
        ),
        % Test beq with non-compressed reg and zero (falls through to encode_b_type)
        ?_assertAsmEqual(
            <<16#00030463:32/little>>, "beq t1, zero, .+8", jit_riscv32_asm:beq(t1, zero, 8)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00629463:32/little>>, "bne t0, t1, .+8", jit_riscv32_asm:bne(t0, t1, 8)
        ),
        ?_assertAsmEqual(
            <<16#feb51ee3:32/little>>, "bne a0, a1, .-4", jit_riscv32_asm:bne(a0, a1, -4)
        ),
        % Test c.bnez (compressed reg with zero)
        ?_assertAsmEqual(
            <<16#ea11:16/little>>, "bne a2, zero, .+20", jit_riscv32_asm:bne(a2, zero, 20)
        ),
        ?_assertAsmEqual(
            <<16#fffd:16/little>>, "bne a5, zero, .-2", jit_riscv32_asm:bne(a5, zero, -2)
        ),
        % Test bne with non-compressed reg and zero (falls through to encode_b_type)
        ?_assertAsmEqual(
            <<16#00031463:32/little>>, "bne t1, zero, .+8", jit_riscv32_asm:bne(t1, zero, 8)
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
            <<16#2021:16/little>>, "jal .+8", jit_riscv32_asm:jal(ra, 8)
        ),
        ?_assertAsmEqual(
            <<16#3ff5:16/little>>, "jal .-4", jit_riscv32_asm:jal(ra, -4)
        ),
        % Test c.j (jal zero, offset)
        ?_assertAsmEqual(
            <<16#a011:16/little>>, "j .+4", jit_riscv32_asm:jal(zero, 4)
        ),
        ?_assertAsmEqual(
            <<16#bffd:16/little>>, "j .-2", jit_riscv32_asm:jal(zero, -2)
        ),
        % Test full J-type encoding (encode_j_type) with larger offsets
        ?_assertAsmEqual(
            <<16#008005ef:32/little>>, "jal a1, .+8", jit_riscv32_asm:jal(a1, 8)
        ),
        ?_assertAsmEqual(
            <<16#ffdffa6f:32/little>>, "jal s4, .-4", jit_riscv32_asm:jal(s4, -4)
        ),
        % Test with maximum positive offset (1048574)
        ?_assertAsmEqual(
            <<16#7ffff56f:32/little>>, "jal a0, .+1048574", jit_riscv32_asm:jal(a0, 1048574)
        ),
        % Test with maximum negative offset (-1048576)
        ?_assertAsmEqual(
            <<16#800005ef:32/little>>, "jal a1, .-1048576", jit_riscv32_asm:jal(a1, -1048576)
        ),
        ?_assertAsmEqual(
            <<16#00000517:32/little, 16#9502:16/little>>,
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
        ?_assertAsmEqual(<<16#9502:16/little>>, "jalr a0", jit_riscv32_asm:jalr(ra, a0, 0)),
        ?_assertAsmEqual(<<16#9502:16/little>>, "jalr a0", jit_riscv32_asm:jalr(ra, a0)),
        ?_assertAsmEqual(<<16#004500e7:32/little>>, "jalr 4(a0)", jit_riscv32_asm:jalr(ra, a0, 4))
    ].

%%-----------------------------------------------------------------------------
%% Upper immediate instruction tests
%%-----------------------------------------------------------------------------

lui_test_() ->
    [
        ?_assertAsmEqual(<<16#65c9:16/little>>, "lui a1, 18", jit_riscv32_asm:lui(a1, 18)),
        ?_assertAsmEqual(<<16#6505:16/little>>, "lui a0, 1", jit_riscv32_asm:lui(a0, 1)),
        ?_assertAsmEqual(<<16#75fd:16/little>>, "lui a1, 0xfffff", jit_riscv32_asm:lui(a1, -1))
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
        % We want a 4-byte NOP for padding, so use .option norvc to force non-compressed
        ?_assertAsmEqual(<<16#00000013:32/little>>, ".option norvc\nnop", jit_riscv32_asm:nop())
    ].

li_test_() ->
    [
        ?_assertAsmEqual(<<16#4529:16/little>>, "li a0, 10", jit_riscv32_asm:li(a0, 10)),
        ?_assertAsmEqual(<<16#557d:16/little>>, "li a0, -1", jit_riscv32_asm:li(a0, -1)),
        ?_assertAsmEqual(<<16#7ff00513:32/little>>, "li a0, 2047", jit_riscv32_asm:li(a0, 2047)),

        % 0x12345 = 74565 - requires lui + addi
        ?_assertAsmEqual(
            <<16#6549:16/little, 16#34550513:32/little>>,
            "li a0, 0x12345",
            jit_riscv32_asm:li(a0, 16#12345)
        ),
        % 0x7FFFFFFF = 2147483647 (maximum 32-bit signed)
        ?_assertAsmEqual(
            <<16#80000537:32/little, 16#157d:16/little>>,
            "li a0, 0x7fffffff",
            jit_riscv32_asm:li(a0, 16#7FFFFFFF)
        ),
        % 0xFFFFFFFF = 4294967295 (maximum 32-bit unsigned, interpreted as -1 signed)
        ?_assertAsmEqual(
            <<16#fff00513:32/little>>,
            "li a0, 0xffffffff",
            jit_riscv32_asm:li(a0, 16#FFFFFFFF)
        ),
        % Test lui-only cases (lower 12 bits are zero)
        % 0x80000000 = 2147483648 (unsigned), -2147483648 (signed)
        ?_assertAsmEqual(
            <<16#800005b7:32/little>>,
            "li a1, 0x80000000",
            jit_riscv32_asm:li(a1, 16#80000000)
        ),
        % Same value as signed negative
        ?_assertAsmEqual(
            <<16#800005b7:32/little>>,
            "li a1, -0x80000000",
            jit_riscv32_asm:li(a1, -16#80000000)
        ),
        % 0x100000 = 1048576 (lower 12 bits zero)
        ?_assertAsmEqual(
            <<16#00100537:32/little>>,
            "li a0, 0x100000",
            jit_riscv32_asm:li(a0, 16#100000)
        ),
        % Test c.lui cases (2 bytes, -32 to 31, lower 12 bits zero)
        % 0x1000 = 4096 (upper = 1)
        ?_assertAsmEqual(
            <<16#6505:16/little>>,
            "li a0, 0x1000",
            jit_riscv32_asm:li(a0, 16#1000)
        ),
        % 0x1f000 = 126976 (upper = 31, max for c.lui)
        ?_assertAsmEqual(
            <<16#657d:16/little>>,
            "li a0, 0x1f000",
            jit_riscv32_asm:li(a0, 16#1f000)
        ),
        % 0xfffffffffffe0000 = -131072 (upper = -32, min for c.lui)
        ?_assertAsmEqual(
            <<16#7501:16/little>>,
            "li a0, -0x20000",
            jit_riscv32_asm:li(a0, -16#20000)
        ),
        % 0x20000 = 131072 (upper = 32, just outside c.lui range, needs full lui)
        ?_assertAsmEqual(
            <<16#00020537:32/little>>,
            "li a0, 0x20000",
            jit_riscv32_asm:li(a0, 16#20000)
        )
    ].

mv_test_() ->
    [
        ?_assertAsmEqual(<<16#852a:16/little>>, "mv a0, a0", jit_riscv32_asm:mv(a0, a0)),
        ?_assertAsmEqual(<<16#85ae:16/little>>, "mv a1, a1", jit_riscv32_asm:mv(a1, a1))
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
            <<16#a021:16/little>>, "j .+8", jit_riscv32_asm:j(8)
        ),
        ?_assertAsmEqual(
            <<16#bff5:16/little>>, "j .-4", jit_riscv32_asm:j(-4)
        )
    ].

jr_test_() ->
    [
        ?_assertAsmEqual(<<16#8502:16/little>>, "jr a0", jit_riscv32_asm:jr(a0)),
        ?_assertAsmEqual(<<16#8282:16/little>>, "jr t0", jit_riscv32_asm:jr(t0))
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(<<16#8082:16/little>>, "ret", jit_riscv32_asm:ret())
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

c_ebreak_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9002:16/little>>, "c.ebreak", jit_riscv32_asm:c_ebreak()
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Arithmetic and Logical instruction tests
%%-----------------------------------------------------------------------------

c_add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9532:16/little>>, "c.add a0, a2", jit_riscv32_asm:c_add(a0, a2)
        ),
        ?_assertAsmEqual(
            <<16#95be:16/little>>, "c.add a1, a5", jit_riscv32_asm:c_add(a1, a5)
        ),
        ?_assertAsmEqual(
            <<16#9522:16/little>>, "c.add a0, s0", jit_riscv32_asm:c_add(a0, s0)
        )
    ].

c_mv_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8532:16/little>>, "c.mv a0, a2", jit_riscv32_asm:c_mv(a0, a2)
        ),
        ?_assertAsmEqual(
            <<16#85be:16/little>>, "c.mv a1, a5", jit_riscv32_asm:c_mv(a1, a5)
        ),
        ?_assertAsmEqual(
            <<16#842a:16/little>>, "c.mv s0, a0", jit_riscv32_asm:c_mv(s0, a0)
        )
    ].

c_sub_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d09:16/little>>, "c.sub a0, a0", jit_riscv32_asm:c_sub(a0, a0)
        ),
        ?_assertAsmEqual(
            <<16#8d8d:16/little>>, "c.sub a1, a1", jit_riscv32_asm:c_sub(a1, a1)
        ),
        ?_assertAsmEqual(
            <<16#8c0d:16/little>>, "c.sub s0, a1", jit_riscv32_asm:c_sub(s0, a1)
        )
    ].

c_and_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d6d:16/little>>, "c.and a0, a1", jit_riscv32_asm:c_and(a0, a1)
        ),
        ?_assertAsmEqual(
            <<16#8fed:16/little>>, "c.and a5, a1", jit_riscv32_asm:c_and(a5, a1)
        ),
        ?_assertAsmEqual(
            <<16#8c6d:16/little>>, "c.and s0, a1", jit_riscv32_asm:c_and(s0, a1)
        )
    ].

c_or_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d4d:16/little>>, "c.or a0, a1", jit_riscv32_asm:c_or(a0, a1)
        ),
        ?_assertAsmEqual(
            <<16#8fcd:16/little>>, "c.or a5, a1", jit_riscv32_asm:c_or(a5, a1)
        ),
        ?_assertAsmEqual(
            <<16#8c4d:16/little>>, "c.or s0, a1", jit_riscv32_asm:c_or(s0, a1)
        )
    ].

c_xor_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8d2d:16/little>>, "c.xor a0, a1", jit_riscv32_asm:c_xor(a0, a1)
        ),
        ?_assertAsmEqual(
            <<16#8fad:16/little>>, "c.xor a5, a1", jit_riscv32_asm:c_xor(a5, a1)
        ),
        ?_assertAsmEqual(
            <<16#8c2d:16/little>>, "c.xor s0, a1", jit_riscv32_asm:c_xor(s0, a1)
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Immediate instruction tests
%%-----------------------------------------------------------------------------

c_addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0511:16/little>>, "c.addi a0, 4", jit_riscv32_asm:c_addi(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#15fd:16/little>>, "c.addi a1, -1", jit_riscv32_asm:c_addi(a1, -1)
        ),
        ?_assertAsmEqual(
            <<16#0541:16/little>>, "c.addi a0, 16", jit_riscv32_asm:c_addi(a0, 16)
        ),
        ?_assertAsmEqual(
            <<16#1561:16/little>>, "c.addi a0, -8", jit_riscv32_asm:c_addi(a0, -8)
        )
    ].

c_andi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8929:16/little>>, "c.andi a0, 10", jit_riscv32_asm:c_andi(a0, 10)
        ),
        ?_assertAsmEqual(
            <<16#99fd:16/little>>, "c.andi a1, -1", jit_riscv32_asm:c_andi(a1, -1)
        ),
        ?_assertAsmEqual(
            <<16#8941:16/little>>, "c.andi a0, 16", jit_riscv32_asm:c_andi(a0, 16)
        )
    ].

c_li_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4529:16/little>>, "c.li a0, 10", jit_riscv32_asm:c_li(a0, 10)
        ),
        ?_assertAsmEqual(
            <<16#55fd:16/little>>, "c.li a1, -1", jit_riscv32_asm:c_li(a1, -1)
        ),
        ?_assertAsmEqual(
            <<16#4505:16/little>>, "c.li a0, 1", jit_riscv32_asm:c_li(a0, 1)
        ),
        ?_assertAsmEqual(
            <<16#5501:16/little>>, "c.li a0, -32", jit_riscv32_asm:c_li(a0, -32)
        )
    ].

c_lui_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6529:16/little>>, "c.lui a0, 10", jit_riscv32_asm:c_lui(a0, 10)
        ),
        ?_assertAsmEqual(
            <<16#75fd:16/little>>, "c.lui a1, 0xfffff", jit_riscv32_asm:c_lui(a1, -1)
        ),
        ?_assertAsmEqual(
            <<16#6505:16/little>>, "c.lui a0, 1", jit_riscv32_asm:c_lui(a0, 1)
        )
    ].

c_addi16sp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#6141:16/little>>, "c.addi16sp sp, 16", jit_riscv32_asm:c_addi16sp(16)
        ),
        ?_assertAsmEqual(
            <<16#7101:16/little>>, "c.addi16sp sp, -512", jit_riscv32_asm:c_addi16sp(-512)
        ),
        ?_assertAsmEqual(
            <<16#6161:16/little>>, "c.addi16sp sp, 80", jit_riscv32_asm:c_addi16sp(80)
        )
    ].

c_addi4spn_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0048:16/little>>, "c.addi4spn a0, sp, 4", jit_riscv32_asm:c_addi4spn(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#1010:16/little>>, "c.addi4spn a2, sp, 32", jit_riscv32_asm:c_addi4spn(a2, 32)
        ),
        ?_assertAsmEqual(
            <<16#1ffc:16/little>>,
            "c.addi4spn a5, sp, 1020",
            jit_riscv32_asm:c_addi4spn(a5, 1020)
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Shift instruction tests
%%-----------------------------------------------------------------------------

c_slli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#050e:16/little>>, "c.slli a0, 3", jit_riscv32_asm:c_slli(a0, 3)
        ),
        ?_assertAsmEqual(
            <<16#05fe:16/little>>, "c.slli a1, 31", jit_riscv32_asm:c_slli(a1, 31)
        ),
        ?_assertAsmEqual(
            <<16#0542:16/little>>, "c.slli a0, 16", jit_riscv32_asm:c_slli(a0, 16)
        )
    ].

c_srli_test_() ->
    [
        ?_assertAsmEqual(
            <<16#810d:16/little>>, "c.srli a0, 3", jit_riscv32_asm:c_srli(a0, 3)
        ),
        ?_assertAsmEqual(
            <<16#81fd:16/little>>, "c.srli a1, 31", jit_riscv32_asm:c_srli(a1, 31)
        ),
        ?_assertAsmEqual(
            <<16#8141:16/little>>, "c.srli a0, 16", jit_riscv32_asm:c_srli(a0, 16)
        )
    ].

c_srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#850d:16/little>>, "c.srai a0, 3", jit_riscv32_asm:c_srai(a0, 3)
        ),
        ?_assertAsmEqual(
            <<16#85fd:16/little>>, "c.srai a1, 31", jit_riscv32_asm:c_srai(a1, 31)
        ),
        ?_assertAsmEqual(
            <<16#8541:16/little>>, "c.srai a0, 16", jit_riscv32_asm:c_srai(a0, 16)
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Load/Store instruction tests
%%-----------------------------------------------------------------------------

c_lw_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4188:16/little>>, "c.lw a0, 0(a1)", jit_riscv32_asm:c_lw(a0, {a1, 0})
        ),
        ?_assertAsmEqual(
            <<16#41d8:16/little>>, "c.lw a4, 4(a1)", jit_riscv32_asm:c_lw(a4, {a1, 4})
        ),
        ?_assertAsmEqual(
            <<16#5ffc:16/little>>, "c.lw a5, 124(a5)", jit_riscv32_asm:c_lw(a5, {a5, 124})
        )
    ].

c_sw_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c188:16/little>>, "c.sw a0, 0(a1)", jit_riscv32_asm:c_sw(a0, {a1, 0})
        ),
        ?_assertAsmEqual(
            <<16#c1d8:16/little>>, "c.sw a4, 4(a1)", jit_riscv32_asm:c_sw(a4, {a1, 4})
        ),
        ?_assertAsmEqual(
            <<16#dffc:16/little>>, "c.sw a5, 124(a5)", jit_riscv32_asm:c_sw(a5, {a5, 124})
        )
    ].

c_lwsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#4502:16/little>>, "c.lwsp a0, 0(sp)", jit_riscv32_asm:c_lwsp(a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#4512:16/little>>, "c.lwsp a0, 4(sp)", jit_riscv32_asm:c_lwsp(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#50fe:16/little>>, "c.lwsp ra, 252(sp)", jit_riscv32_asm:c_lwsp(ra, 252)
        )
    ].

c_swsp_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c02a:16/little>>, "c.swsp a0, 0(sp)", jit_riscv32_asm:c_swsp(a0, 0)
        ),
        ?_assertAsmEqual(
            <<16#c22a:16/little>>, "c.swsp a0, 4(sp)", jit_riscv32_asm:c_swsp(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#dfe6:16/little>>, "c.swsp s9, 252(sp)", jit_riscv32_asm:c_swsp(s9, 252)
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Branch and Jump instruction tests
%%-----------------------------------------------------------------------------

c_beqz_test_() ->
    [
        ?_assertAsmEqual(
            <<16#c111:16/little>>, "c.beqz a0, .+4", jit_riscv32_asm:c_beqz(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#dced:16/little>>, "c.beqz s1, .-6", jit_riscv32_asm:c_beqz(s1, -6)
        ),
        ?_assertAsmEqual(
            <<16#c101:16/little>>, "c.beqz a0, .", jit_riscv32_asm:c_beqz(a0, 0)
        )
    ].

c_bnez_test_() ->
    [
        ?_assertAsmEqual(
            <<16#e111:16/little>>, "c.bnez a0, .+4", jit_riscv32_asm:c_bnez(a0, 4)
        ),
        ?_assertAsmEqual(
            <<16#fced:16/little>>, "c.bnez s1, .-6", jit_riscv32_asm:c_bnez(s1, -6)
        ),
        ?_assertAsmEqual(
            <<16#e101:16/little>>, "c.bnez a0, .", jit_riscv32_asm:c_bnez(a0, 0)
        )
    ].

c_j_test_() ->
    [
        ?_assertAsmEqual(
            <<16#a011:16/little>>, "c.j .+4", jit_riscv32_asm:c_j(4)
        ),
        ?_assertAsmEqual(
            <<16#bfed:16/little>>, "c.j .-6", jit_riscv32_asm:c_j(-6)
        ),
        ?_assertAsmEqual(
            <<16#a001:16/little>>, "c.j .", jit_riscv32_asm:c_j(0)
        )
    ].

c_jal_test_() ->
    [
        ?_assertAsmEqual(
            <<16#2021:16/little>>, "c.jal .+8", jit_riscv32_asm:c_jal(8)
        ),
        ?_assertAsmEqual(
            <<16#3ff5:16/little>>, "c.jal .-4", jit_riscv32_asm:c_jal(-4)
        ),
        ?_assertAsmEqual(
            <<16#2001:16/little>>, "c.jal .", jit_riscv32_asm:c_jal(0)
        )
    ].

c_jr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#8502:16/little>>, "c.jr a0", jit_riscv32_asm:c_jr(a0)
        ),
        ?_assertAsmEqual(
            <<16#8402:16/little>>, "c.jr s0", jit_riscv32_asm:c_jr(s0)
        ),
        ?_assertAsmEqual(
            <<16#8082:16/little>>, "c.jr ra", jit_riscv32_asm:c_jr(ra)
        )
    ].

c_jalr_test_() ->
    [
        ?_assertAsmEqual(
            <<16#9502:16/little>>, "c.jalr a0", jit_riscv32_asm:c_jalr(a0)
        ),
        ?_assertAsmEqual(
            <<16#9402:16/little>>, "c.jalr s0", jit_riscv32_asm:c_jalr(s0)
        )
    ].

%%-----------------------------------------------------------------------------
%% C Extension - Pseudo-instruction tests
%%-----------------------------------------------------------------------------

c_nop_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0001:16/little>>, "c.nop", jit_riscv32_asm:c_nop()
        )
    ].
