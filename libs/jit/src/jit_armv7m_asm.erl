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

%% @doc Thumb-2 (ARMv7-M / ARMv8-M) instruction assembler.
%%
%% This module encodes Thumb-2 32-bit instructions that are not available
%% in the ARMv6-M (Thumb-1 only) instruction set. It is used as a companion
%% to jit_armv6m_asm when the thumb2 variant is enabled.
%%
%% Reference: ARM Architecture Reference Manual ARMv7-M (DDI 0403E).
%% These encodings are also valid for ARMv8-M Mainline.
%% Thumb-2 instructions are encoded as two 16-bit halfwords, stored
%% little-endian. The first halfword contains the high bits.

-module(jit_armv7m_asm).

-export([
    b_w/1,
    movw/2,
    movt/2
]).

-type arm_gpr_register() :: jit_armv6m_asm:arm_gpr_register().

%%-----------------------------------------------------------------------------
%% Thumb-2 32-bit branch (B.W)
%%
%% Encoding T4 (ARMv7-M):
%%   First halfword:  11110 S imm10[9:0]
%%   Second halfword: 10 J1 1 J2 imm11[10:0]
%%
%% Where:
%%   I1 = NOT(J1 XOR S)
%%   I2 = NOT(J2 XOR S)
%%   imm32 = SignExtend(S:I1:I2:imm10:imm11:0, 32)
%%
%% Range: -16777216 to +16777214 (±16 MB), 2-byte aligned
%%
%% The offset is relative to PC (instruction address + 4).
%%-----------------------------------------------------------------------------
-spec b_w(integer()) -> binary().
b_w(Offset) when
    is_integer(Offset),
    Offset >= -16777216,
    Offset =< 16777214,
    (Offset rem 2) =:= 0
->
    %% imm32 = SignExtend(S:I1:I2:imm10:imm11:0, 32)
    %% After dropping the low zero bit we have a 24-bit value: S:I1:I2:imm10:imm11
    Imm24 = (Offset bsr 1) band 16#FFFFFF,
    S = (Imm24 bsr 23) band 1,
    I1 = (Imm24 bsr 22) band 1,
    I2 = (Imm24 bsr 21) band 1,
    Imm10 = (Imm24 bsr 11) band 16#3FF,
    Imm11 = Imm24 band 16#7FF,
    %% J1 = NOT(I1 XOR S), J2 = NOT(I2 XOR S)
    J1 = (1 - (I1 bxor S)) band 1,
    J2 = (1 - (I2 bxor S)) band 1,
    HW1 = (2#11110 bsl 11) bor (S bsl 10) bor Imm10,
    HW2 = (2#10 bsl 14) bor (J1 bsl 13) bor (1 bsl 12) bor (J2 bsl 11) bor Imm11,
    <<HW1:16/little, HW2:16/little>>;
b_w(Offset) ->
    error({unencodable_branch_offset, Offset}).

%%-----------------------------------------------------------------------------
%% Thumb-2 MOVW (Move Wide) - loads 16-bit immediate into lower half of register
%%
%% Encoding T3:
%%   First halfword:  11110 i 10 0 1 0 0 imm4[3:0]
%%   Second halfword: 0 imm3[2:0] Rd[3:0] imm8[7:0]
%%
%% Where: imm16 = imm4:i:imm3:imm8
%%
%% Sets lower 16 bits of Rd to imm16, zeroes upper 16 bits.
%%-----------------------------------------------------------------------------
-spec movw(arm_gpr_register(), non_neg_integer()) -> binary().
movw(Rd, Imm16) when is_integer(Imm16), Imm16 >= 0, Imm16 =< 65535 ->
    encode_mov_imm16(2#100100, Rd, Imm16);
movw(_Rd, Imm) ->
    error({unencodable_immediate, Imm}).

%%-----------------------------------------------------------------------------
%% Thumb-2 MOVT (Move Top) - loads 16-bit immediate into upper half of register
%%
%% Encoding T1:
%%   First halfword:  11110 i 10 1 1 0 0 imm4[3:0]
%%   Second halfword: 0 imm3[2:0] Rd[3:0] imm8[7:0]
%%
%% Where: imm16 = imm4:i:imm3:imm8
%%
%% Sets upper 16 bits of Rd to imm16, preserves lower 16 bits.
%%-----------------------------------------------------------------------------
-spec movt(arm_gpr_register(), non_neg_integer()) -> binary().
movt(Rd, Imm16) when is_integer(Imm16), Imm16 >= 0, Imm16 =< 65535 ->
    encode_mov_imm16(2#101100, Rd, Imm16);
movt(_Rd, Imm) ->
    error({unencodable_immediate, Imm}).

%%-----------------------------------------------------------------------------
%% Internal helpers
%%-----------------------------------------------------------------------------

%% Encode a MOVW or MOVT instruction, differing only in the opcode field.
encode_mov_imm16(Opcode, Rd, Imm16) ->
    RdNum = jit_armv6m_asm:reg_to_num(Rd),
    Imm4 = (Imm16 bsr 12) band 16#F,
    I = (Imm16 bsr 11) band 1,
    Imm3 = (Imm16 bsr 8) band 7,
    Imm8 = Imm16 band 16#FF,
    HW1 = (2#11110 bsl 11) bor (I bsl 10) bor (Opcode bsl 4) bor Imm4,
    HW2 = (Imm3 bsl 12) bor (RdNum bsl 8) bor Imm8,
    <<HW1:16/little, HW2:16/little>>.
