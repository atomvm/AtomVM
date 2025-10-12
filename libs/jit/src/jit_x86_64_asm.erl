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

-module(jit_x86_64_asm).

-export([
    movq/2,
    movabsq/2,
    movl/2,
    shlq/2,
    shrq/2,
    testb/2,
    testl/2,
    testq/2,
    jz/1,
    jz_rel8/1,
    jnz/1,
    jnz_rel8/1,
    jge/1,
    jge_rel8/1,
    jmp/1,
    jmp_rel8/1,
    jmp_rel32/1,
    andq/2,
    andl/2,
    andb/2,
    cmpl/2,
    cmpq/2,
    addq/2,
    subq/2,
    imulq/2,
    decl/1,
    orq/2,
    orq_rel32/2,
    leaq/2,
    leaq_rel32/2,
    callq/1,
    pushq/1,
    popq/1,
    jmpq/1,
    retq/0,
    cmpb/2
]).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-type x86_64_register() :: rax | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11.

% Encode a register on 4 bits
% https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
-spec x86_64_x_reg(x86_64_register()) -> {0..1, 0..7}.
x86_64_x_reg(rax) -> {0, 0};
x86_64_x_reg(rcx) -> {0, 1};
x86_64_x_reg(rdx) -> {0, 2};
x86_64_x_reg(rsi) -> {0, 6};
x86_64_x_reg(rdi) -> {0, 7};
x86_64_x_reg(r8) -> {1, 0};
x86_64_x_reg(r9) -> {1, 1};
x86_64_x_reg(r10) -> {1, 2};
x86_64_x_reg(r11) -> {1, 3}.

-define(X86_64_REX(W, R, X, B), <<4:4, W:1, R:1, X:1, B:1>> / binary).

movq({0, SrcReg}, DestReg) when is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, 0:2, MODRM_REG:3, MODRM_RM:3>>;
movq({Offset, SrcReg}, DestReg) when is_atom(DestReg) andalso ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp8
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, 1:2, MODRM_REG:3, MODRM_RM:3, Offset>>;
movq({Offset, SrcReg}, DestReg) when is_atom(DestReg) andalso ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp32
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, 2:2, MODRM_REG:3, MODRM_RM:3, Offset:32/little>>;
movq(DestReg, {0, SrcReg}) when is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, 0:2, MODRM_REG:3, MODRM_RM:3>>;
movq(DestReg, {Offset, SrcReg}) when is_atom(DestReg) andalso ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp8
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, 1:2, MODRM_REG:3, MODRM_RM:3, Offset>>;
movq(DestReg, {Offset, SrcReg}) when is_atom(DestReg) andalso ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp32
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, 2:2, MODRM_REG:3, MODRM_RM:3, Offset:32/little>>;
movq(SrcReg, DestReg) when is_atom(SrcReg) andalso is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, 3:2, MODRM_REG:3, MODRM_RM:3>>;
movq(Imm, DestReg) when is_integer(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>;
movq(Imm, {Offset, DestReg}) when is_integer(Imm) andalso ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, 1:2, 0:3, MODRM_RM:3, Offset, Imm:32/little>>;
movq(Imm, {Offset, DestReg}) when is_integer(Imm) andalso ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, 2:2, 0:3, MODRM_RM:3, Offset:32/little, Imm:32/little>>;
% movq reg, {0, base, index, scale} - SIB with no displacement
movq(RegA, {0, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8)
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB, mod=00 for no displacement
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        0:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3
    >>;
movq(RegA, {Offset, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT8_T(Offset),
    Offset =/= 0
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        1:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset
    >>;
movq(RegA, {Offset, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT32_T(Offset)
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        2:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset:32/little
    >>;
movq(Imm, {Offset, Base, Index, Scale}) when
    is_integer(Imm),
    is_atom(Base),
    is_atom(Index),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT8_T(Offset),
    Offset =/= 0
->
    {REX_B, MODRM_BASE} = x86_64_x_reg(Base),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(Index),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB, mod=01 for disp8
    <<
        ?X86_64_REX(1, 0, REX_X, REX_B),
        16#c7,
        1:2,
        0:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset,
        Imm:32/little
    >>;
movq(Imm, {Offset, Base, Index, Scale}) when
    is_integer(Imm),
    is_atom(Base),
    is_atom(Index),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT32_T(Offset)
->
    {REX_B, MODRM_BASE} = x86_64_x_reg(Base),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(Index),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, 0, REX_X, REX_B),
        16#c7,
        2:2,
        0:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset:32/little,
        Imm:32/little
    >>.

movabsq(Imm, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, (16#B8 + Index), Imm:64/little>>;
        {1, Index} -> <<16#49, (16#B8 + Index), Imm:64/little>>
    end.

movl({0, SrcReg}, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    (case {REX_R, REX_B} of
        {0, 0} -> <<16#8B, 0:2, MODRM_REG:3, MODRM_RM:3>>;
        _ -> <<?X86_64_REX(0, REX_R, 0, REX_B), 16#8B, 0:2, MODRM_REG:3, MODRM_RM:3>>
    end).

shlq(Imm, Reg) when ?IS_UINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#C1, (16#E0 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#C1, (16#E0 + Index), Imm>>
    end.

shrq(Imm, Reg) when ?IS_UINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#C1, (16#E8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#C1, (16#E8 + Index), Imm>>
    end.

testb(Reg, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#84, (16#C0 bor (Index bsl 3) bor Index)>>;
        {1, Index} -> <<16#45, 16#84, (16#C0 bor (Index bsl 3) bor Index)>>
    end;
testb(Imm, rax) when ?IS_UINT8_T(Imm); ?IS_SINT8_T(Imm) ->
    <<16#A8, Imm>>;
testb(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg); ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % TEST r/m8, imm8: 0xF6 /0 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#F6, 3:2, 0:3, MODRM_RM:3, Imm>>.

testq(Reg, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#85, (16#C0 bor (Index bsl 3) bor Index)>>;
        {1, Index} -> <<16#4D, 16#85, (16#C0 bor (Index bsl 3) bor Index)>>
    end;
testq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % TEST rax, imm32: REX.W 0xA9 imm32
    <<16#48, 16#A9, Imm:32/little>>;
testq(Imm, Reg) when is_atom(Reg), Reg =/= rax, ?IS_SINT32_T(Imm) ->
    % TEST r/m64, imm32: REX.W 0xF7 /0 ModRM imm32
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#F7, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>.

testl(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    Prefix =
        case {REX_R, REX_B} of
            {0, 0} -> <<>>;
            _ -> <<?X86_64_REX(0, REX_R, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#85, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>.

jz(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#74, AdjustedOffset>>.

jz_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jz(Offset)}.

jnz(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#75, AdjustedOffset>>.

jnz_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jnz(Offset)}.

jge(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#7D, AdjustedOffset>>.

jge_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jge(Offset)}.

jmp(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#EB, AdjustedOffset>>;
jmp(Offset) when ?IS_SINT32_T(Offset) ->
    % Adjust for 5-byte near jump instruction size
    AdjustedOffset = Offset - 5,
    <<16#E9, AdjustedOffset:32/little>>.

jmp_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    {1, <<16#EB, AdjustedOffset>>}.

jmp_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    % Adjust for 5-byte near jump instruction size
    AdjustedOffset = Offset - 5,
    {1, <<16#E9, AdjustedOffset:32/little>>}.

andq(Imm, DestReg) when ?IS_SINT8_T(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 3:2, 4:3, MODRM_RM:3, Imm>>;
andq(Imm, {Offset, DestReg}) when ?IS_SINT8_T(Imm) andalso ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 1:2, 4:3, MODRM_RM:3, Offset, Imm>>;
andq(Imm, {Offset, DestReg}) when ?IS_SINT8_T(Imm) andalso ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 2:2, 4:3, MODRM_RM:3, Offset:32/little, Imm>>.

andl(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m32, imm8: 0x83 /4 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#83, 3:2, 4:3, MODRM_RM:3, Imm>>;
andl(Imm, rax) when ?IS_UINT32_T(Imm) ->
    % Special short encoding for AND EAX, imm32: 0x25 imm32
    <<16#25, Imm:32/little>>;
andl(Imm, Reg) when ?IS_UINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m32, imm32: 0x81 /4 ModRM imm32 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#81, 3:2, 4:3, MODRM_RM:3, Imm:32/little>>.

andb(Imm, rax) when ?IS_UINT8_T(Imm) orelse ?IS_SINT8_T(Imm) ->
    <<16#24, Imm>>;
andb(Imm, Reg) when ?IS_UINT8_T(Imm) orelse ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m8, imm8: 0x80 /4 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#80, 3:2, 4:3, MODRM_RM:3, Imm>>.

cmpb(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    Prefix =
        case {REX_R, REX_B} of
            {0, 0} -> <<>>;
            _ -> <<?X86_64_REX(0, REX_R, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#38, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>;
cmpb(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % CMP r/m8, imm8: 0x80 /7 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#80, 3:2, 7:3, MODRM_RM:3, Imm>>.

cmpl(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    case REX_B of
        % No REX needed for rax..rdi
        0 -> <<16#83, 3:2, 7:3, MODRM_RM:3, Imm>>;
        % REX.B needed for r8..r11
        1 -> <<16#41, 16#83, 3:2, 7:3, MODRM_RM:3, Imm>>
    end.

cmpq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#39, 3:2, MODRM_REG:3, MODRM_RM:3>>;
cmpq(Imm, Reg) when ?IS_SINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#83, (16#F8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#83, (16#F8 + Index), Imm>>
    end;
cmpq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % Special short encoding for cmp imm32, %rax
    <<16#48, 16#3D, Imm:32/little>>;
cmpq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 7:3, MODRM_RM:3, Imm:32/little>>.

addq(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#83, (16#C0 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#83, (16#C0 + Index), Imm>>
    end;
addq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % Special short encoding for add imm32, %rax
    <<16#48, 16#05, Imm:32/little>>;
addq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>;
addq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#01, 3:2, MODRM_REG:3, MODRM_RM:3>>.

subq(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#29, 3:2, MODRM_REG:3, MODRM_RM:3>>.

imulq(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_H, MODRM} = x86_64_x_reg(Reg),
    REX = 16#48 bor (REX_H bsl 2) bor REX_H,
    <<REX, 16#6B, (16#C0 bor (MODRM bsl 3) bor MODRM), Imm>>;
imulq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_H, MODRM} = x86_64_x_reg(Reg),
    REX = 16#48 bor (REX_H bsl 2) bor REX_H,
    <<REX, 16#69, (16#C0 bor (MODRM bsl 3) bor MODRM), Imm:32/little>>;
imulq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    % DestReg for REX.R and ModRM.reg
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    % SrcReg for REX.B and ModRM.rm
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    REX = 16#48 bor (REX_R bsl 2) bor REX_B,
    <<REX, 16#0F, 16#AF, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>.

decl({Offset, rsi}) when ?IS_SINT8_T(Offset) ->
    <<16#FF, 16#4E, Offset>>.

orq_rel32(Imm, rax) when ?IS_UINT32_T(Imm) ->
    {2, <<?X86_64_REX(1, 0, 0, 0), 16#0D, Imm:32/little>>};
orq_rel32(Imm, Reg) when ?IS_UINT32_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    {3, <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 1:3, MODRM_RM:3, Imm:32/little>>}.

orq(Imm, Reg) when ?IS_SINT8_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 3:2, 1:3, MODRM_RM:3, Imm>>;
orq(Imm, rax) when ?IS_UINT32_T(Imm) ->
    <<?X86_64_REX(1, 0, 0, 0), 16#0D, Imm:32/little>>;
orq(Imm, Reg) when ?IS_UINT32_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 1:3, MODRM_RM:3, Imm:32/little>>.

leaq_rel32({Offset, rip}, Reg) when is_atom(Reg), ?IS_SINT32_T(Offset) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> {3, <<16#48, 16#8D, (16#05 + (Index bsl 3)), Offset:32/little>>};
        {1, Index} -> {3, <<16#4C, 16#8D, (16#05 + (Index bsl 3)), Offset:32/little>>}
    end.

leaq({rip, Offset}, DestReg) when is_atom(DestReg), ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    % RIP-relative addressing: ModRM: mod=00, reg=DestReg, rm=101 (RIP-relative)
    <<?X86_64_REX(1, REX_R, 0, 0), 16#8D, 0:2, MODRM_REG:3, 5:3, Offset:32/little>>;
leaq({Offset, BaseReg}, DestReg) when is_atom(BaseReg), is_atom(DestReg), ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(BaseReg),
    % ModRM: mod=01 (disp8), reg=DestReg, rm=BaseReg
    % SIB: scale=0, index=100 (none), base=BaseReg
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8D, 1:2, MODRM_REG:3, MODRM_RM:3, Offset>>;
leaq({Offset, BaseReg}, DestReg) when is_atom(BaseReg), is_atom(DestReg), ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(BaseReg),
    % ModRM: mod=10 (disp32), reg=DestReg, rm=BaseReg
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8D, 2:2, MODRM_REG:3, MODRM_RM:3, Offset:32/little>>.

callq({Reg}) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#FF, (16#D0 + Index)>>;
        {1, Index} -> <<16#41, 16#FF, (16#D0 + Index)>>
    end.

pushq(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<(16#50 + Index)>>;
        {1, Index} -> <<16#41, (16#50 + Index)>>
    end.

popq(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<(16#58 + Index)>>;
        {1, Index} -> <<16#41, (16#58 + Index)>>
    end.

jmpq({Reg}) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#FF, (16#E0 + Index)>>;
        {1, Index} -> <<16#41, 16#FF, (16#E0 + Index)>>
    end.

retq() ->
    <<16#C3>>.
