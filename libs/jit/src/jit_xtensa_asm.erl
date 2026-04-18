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

-module(jit_xtensa_asm).

-export([
    add/3,
    sub/3,
    and_/3,
    or_/3,
    xor_/3,
    srl/3,
    slli/3,
    srli/3,
    srai/3,
    ssr/1,
    mull/3,
    quos/3,
    rems/3,
    addi/3,
    addmi/3,
    movi/2,
    mov/2,
    l32i/3,
    s32i/3,
    beq/3,
    bne/3,
    blt/3,
    beqz/2,
    bnez/2,
    bltz/2,
    beqi/3,
    bnei/3,
    blti/3,
    j/1,
    jx/1,
    callx8/1,
    retw/0,
    entry/2,
    l32r/2,
    nop/0,
    break/2,
    neg/2
]).

-ifdef(JIT_DWARF).
-export([reg_to_num/1]).
-endif.

-export_type([
    xtensa_register/0
]).

%% Xtensa Assembler for ESP32 (Xtensa LX6/LX7, windowed ABI)
%% This assembler includes ESP32 specific instructions such as quos and rems.
%%
%% Xtensa Register Set (16 registers with windowed ABI):
%%   a0  - Return address
%%   a1  - Stack pointer
%%   a2  - Function argument 0 / Return value
%%   a3  - Function argument 1
%%   a4  - Function argument 2
%%   a5  - Function argument 3
%%   a6  - Function argument 4
%%   a7  - Function argument 5
%%   a8  - Temporary (caller-saved)
%%   a9  - Temporary (caller-saved)
%%   a10 - Temporary (caller-saved)
%%   a11 - Temporary (caller-saved)
%%   a12 - Callee-saved
%%   a13 - Callee-saved
%%   a14 - Callee-saved
%%   a15 - Callee-saved
%%
%% Instruction Encoding:
%%   24-bit (3 bytes) for standard instructions
%%   16-bit (2 bytes) for narrow/density instructions
%%   Byte ordering: little-endian (byte0 = bits[7:0])
%%
%% Instruction Formats (24-bit):
%%   RRR:  op2[23:20] | op1[19:16] | r[15:12] | s[11:8]  | t[7:4]  | op0[3:0]
%%   RRI8: imm8[23:16]| r[15:12]   | s[11:8]  | t[7:4]   | op0[3:0]
%%   RI16: imm16[23:8]             | t[7:4]   | op0[3:0]
%%   CALL: offset[23:6]           | n[5:4]   | op0[3:0]
%%   BRI8: imm8[23:16]| r[15:12]  | s[11:8]  | t[7:4]   | op0[3:0]
%%   BRI12:imm12[23:12]           | s[11:8]  | t[7:4]   | op0[3:0]
%%
%% See: Xtensa Instruction Set Architecture (ISA) Reference Manual

-type xtensa_register() ::
    a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | a8
    | a9
    | a10
    | a11
    | a12
    | a13
    | a14
    | a15.

-spec reg_to_num(xtensa_register()) -> 0..15.
reg_to_num(a0) -> 0;
reg_to_num(a1) -> 1;
reg_to_num(a2) -> 2;
reg_to_num(a3) -> 3;
reg_to_num(a4) -> 4;
reg_to_num(a5) -> 5;
reg_to_num(a6) -> 6;
reg_to_num(a7) -> 7;
reg_to_num(a8) -> 8;
reg_to_num(a9) -> 9;
reg_to_num(a10) -> 10;
reg_to_num(a11) -> 11;
reg_to_num(a12) -> 12;
reg_to_num(a13) -> 13;
reg_to_num(a14) -> 14;
reg_to_num(a15) -> 15.

%%=============================================================================
%% 24-bit Instruction Format Encoders
%%=============================================================================

%% RRR format: op1[23:20] | op2[19:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rrr(integer(), integer(), integer(), integer(), integer(), integer()) -> binary().
encode_rrr(Op0, T, S, R, Op1, Op2) ->
    Instr = (Op1 bsl 20) bor (Op2 bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% RRI8 format: imm8[23:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rri8(integer(), integer(), integer(), integer(), integer()) -> binary().
encode_rri8(Op0, T, S, R, Imm8) ->
    Instr = ((Imm8 band 16#FF) bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% RI16 format: imm16[23:8] | t[7:4] | op0[3:0]
-spec encode_ri16(integer(), integer(), integer()) -> binary().
encode_ri16(Op0, T, Imm16) ->
    Instr = ((Imm16 band 16#FFFF) bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% CALL format: offset[23:6] | n[5:4] | op0[3:0]
-spec encode_call(integer(), integer(), integer()) -> binary().
encode_call(Op0, N, Offset18) ->
    Instr = ((Offset18 band 16#3FFFF) bsl 6) bor (N bsl 4) bor Op0,
    <<Instr:24/little>>.

%% BRI8 format for conditional branches: imm8[23:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_bri8(integer(), integer(), integer(), integer(), integer()) -> binary().
encode_bri8(Op0, T, S, R, Imm8) ->
    Instr = ((Imm8 band 16#FF) bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% BRI12 format for BEQZ/BNEZ/BGEZ/BLTZ:
%% imm12[23:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_bri12(integer(), integer(), integer(), integer()) -> binary().
encode_bri12(Op0, T, S, Imm12) ->
    Instr = ((Imm12 band 16#FFF) bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%%=============================================================================
%% 16-bit Narrow Instruction Format Encoders
%%=============================================================================

%% RRRN format: r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rrrn(integer(), integer(), integer(), integer()) -> binary().
encode_rrrn(Op0, T, S, R) ->
    Instr = (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:16/little>>.

%%=============================================================================
%% RRR-type Arithmetic Instructions
%%=============================================================================

%% ADD: AR[r] = AR[s] + AR[t]
%% op0=0, op1=8, op2=0
-spec add(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
add(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#8, 0).

%% SUB: AR[r] = AR[s] - AR[t]
%% op0=0, op1=12, op2=0
-spec sub(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
sub(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#C, 0).

%% AND: AR[r] = AR[s] & AR[t]
%% op0=0, op1=1, op2=0
-spec and_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
and_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#1, 0).

%% OR: AR[r] = AR[s] | AR[t]
%% op0=0, op1=2, op2=0
-spec or_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
or_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#2, 0).

%% XOR: AR[r] = AR[s] ^ AR[t]
%% op0=0, op1=3, op2=0
-spec xor_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
xor_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#3, 0).

%% NEG: AR[r] = 0 - AR[t]
%% op0=0, t=At, s=0, r=Ar, bits[23:20]=6, bits[19:16]=0
-spec neg(xtensa_register(), xtensa_register()) -> binary().
neg(Ar, At) ->
    encode_rrr(0, reg_to_num(At), 0, reg_to_num(Ar), 16#6, 0).

%%=============================================================================
%% Shift Instructions
%%=============================================================================

%% SRL: AR[r] = AR[t] >> SAR (logical)
%% Must set SAR with SSR first.
%% op0=0, bits[23:20]=0x9, bits[19:16]=1, s=0
-spec srl(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
srl(Ar, _As, At) ->
    encode_rrr(0, reg_to_num(At), 0, reg_to_num(Ar), 16#9, 16#1).

%% SSR: Set SAR for right shift. SAR = AR[s][4:0]
%% op0=0, op1=4, op2=0, r=0, t=0
-spec ssr(xtensa_register()) -> binary().
ssr(As) ->
    encode_rrr(0, 0, reg_to_num(As), 0, 16#4, 0).

%% SLLI: AR[r] = AR[s] << sa (1..31)
%% op0=0, RRR format with shift amount encoded as (32 - sa).
%% The encoded value split: sa_enc[4] at bits[23:20], op=1 at bits[19:16],
%% r at bits[15:12], s at bits[11:8], sa_enc[3:0] at bits[7:4].
-spec slli(xtensa_register(), xtensa_register(), 1..31) -> binary().
slli(Ar, As, Sa) when Sa >= 1, Sa =< 31 ->
    SaEnc = 32 - Sa,
    Sa4 = (SaEnc bsr 4) band 1,
    Sa30 = SaEnc band 16#F,
    Instr =
        (Sa4 bsl 20) bor (16#1 bsl 16) bor (reg_to_num(Ar) bsl 12) bor
            (reg_to_num(As) bsl 8) bor (Sa30 bsl 4) bor 0,
    <<Instr:24/little>>.

%% SRLI: AR[r] = AR[t] >> sa (logical, immediate)
%% op0=0, t=At, s=sa[3:0], r=Ar, bits[23:20]=4, bits[19:16]=1
-spec srli(xtensa_register(), xtensa_register(), 0..15) -> binary().
srli(Ar, At, Sa) when Sa >= 0, Sa =< 15 ->
    encode_rrr(0, reg_to_num(At), Sa band 16#F, reg_to_num(Ar), 16#4, 1).

%% SRAI: AR[r] = AR[t] >> sa (arithmetic, immediate)
%% op0=0, bits[23:20] = 2 + sa[4], bits[19:16] = 1, r=Ar, s=sa[3:0], t=At
-spec srai(xtensa_register(), xtensa_register(), 0..31) -> binary().
srai(Ar, At, Sa) when Sa >= 0, Sa =< 31 ->
    Sa4 = (Sa bsr 4) band 1,
    Sa30 = Sa band 16#F,
    Instr =
        ((2 + Sa4) bsl 20) bor (16#1 bsl 16) bor (reg_to_num(Ar) bsl 12) bor
            (Sa30 bsl 8) bor (reg_to_num(At) bsl 4) bor 0,
    <<Instr:24/little>>.

%%=============================================================================
%% Multiply/Divide Instructions
%%=============================================================================

%% MULL: AR[r] = AR[s] * AR[t] (low 32 bits)
%% op0=0, op1=8, op2=2
-spec mull(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
mull(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#8, 16#2).

%% QUOS: AR[r] = AR[s] / AR[t] (signed)
%% op0=0, op1=13, op2=2
-spec quos(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
quos(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#D, 16#2).

%% REMS: AR[r] = AR[s] % AR[t] (signed)
%% op0=0, op1=15, op2=2
-spec rems(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
rems(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#F, 16#2).

%%=============================================================================
%% Immediate Arithmetic
%%=============================================================================

%% ADDI: AR[t] = AR[s] + sign_extend(imm8)
%% op0=2, r=0xC
-spec addi(xtensa_register(), xtensa_register(), -128..127) -> binary().
addi(At, As, Imm8) when Imm8 >= -128, Imm8 =< 127 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#C, Imm8).

%% ADDMI: AR[t] = AR[s] + sign_extend(imm8 << 8)
%% op0=2, r=0xD
%% Imm must be a multiple of 256 in range -32768..32512
-spec addmi(xtensa_register(), xtensa_register(), integer()) -> binary().
addmi(At, As, Imm) when Imm rem 256 =:= 0, Imm >= -32768, Imm =< 32512 ->
    Imm8 = (Imm bsr 8) band 16#FF,
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#D, Imm8);
addmi(_At, _As, Imm) ->
    error({addmi_value_out_of_range, Imm}).

%%=============================================================================
%% Move Instructions
%%=============================================================================

%% MOVI: AR[t] = sign_extend_12(imm)
%% op0=2, r=0xA, imm12 split: bits[11:8] in s field, bits[7:0] in imm8 field
-spec movi(xtensa_register(), -2048..2047) -> binary().
movi(At, Imm) when Imm >= -2048, Imm =< 2047 ->
    Imm12 = Imm band 16#FFF,
    Imm8 = Imm12 band 16#FF,
    Imm118 = (Imm12 bsr 8) band 16#F,
    encode_rri8(16#2, reg_to_num(At), Imm118, 16#A, Imm8).

%% MOV.N: AR[t] = AR[s] (narrow, 2 bytes)
-spec mov(xtensa_register(), xtensa_register()) -> binary().
mov(Dst, Src) ->
    encode_rrrn(16#D, reg_to_num(Dst), reg_to_num(Src), 0).

%%=============================================================================
%% Load Instructions
%%=============================================================================

%% L32I: AR[t] = mem[AR[s] + imm8*4]
%% op0=2, r=2
-spec l32i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l32i(At, As, Offset) when Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#2, Offset div 4).

%%=============================================================================
%% Store Instructions
%%=============================================================================

%% S32I: mem[AR[s] + imm8*4] = AR[t]
%% op0=2, r=6
-spec s32i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
s32i(At, As, Offset) when Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#6, Offset div 4).

%%=============================================================================
%% Branch Instructions
%%=============================================================================

%% BEQ: if AR[s] == AR[t] then PC += sign_extend(imm8)
%% op0=7, r=1
-spec beq(xtensa_register(), xtensa_register(), integer()) -> binary().
beq(As, At, Offset) when Offset >= -128, Offset =< 127 ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#1, Offset).

%% BNE: if AR[s] != AR[t] then PC += sign_extend(imm8)
%% op0=7, r=9
-spec bne(xtensa_register(), xtensa_register(), integer()) -> binary().
bne(As, At, Offset) when Offset >= -128, Offset =< 127 ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#9, Offset).

%% BLT: if AR[s] < AR[t] (signed) then PC += sign_extend(imm8)
%% op0=7, r=2
-spec blt(xtensa_register(), xtensa_register(), integer()) -> binary().
blt(As, At, Offset) when Offset >= -128, Offset =< 127 ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#2, Offset).

%% BEQZ: if AR[s] == 0 then PC += sign_extend(imm12)
%% Encoding: imm12[11:0] in bits[23:12], s[11:8], t=0001b[7:4], op0=0110b[3:0]
-spec beqz(xtensa_register(), integer()) -> binary().
beqz(As, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_bri12(16#6, 16#1, reg_to_num(As), Offset band 16#FFF).

%% BNEZ: if AR[s] != 0 then PC += sign_extend(imm12)
%% op0=6, t=5 (BNZ type)
-spec bnez(xtensa_register(), integer()) -> binary().
bnez(As, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_bri12(16#6, 16#5, reg_to_num(As), Offset band 16#FFF).

%% BLTZ: if AR[s] < 0 then PC += sign_extend(imm12)
%% op0=6, t=9
-spec bltz(xtensa_register(), integer()) -> binary().
bltz(As, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_bri12(16#6, 16#9, reg_to_num(As), Offset band 16#FFF).

%% BEQI: if AR[s] == b4const(r) then PC += sign_extend(imm8)
%% op0=6, r encodes constant via b4const table, t=2
-spec beqi(xtensa_register(), integer(), integer()) -> binary().
beqi(As, Imm, Offset) when Offset >= -128, Offset =< 127 ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#2, reg_to_num(As), R, Offset).

%% BNEI: if AR[s] != b4const(r) then PC += sign_extend(imm8)
%% op0=6, t=6 (0x6)
-spec bnei(xtensa_register(), integer(), integer()) -> binary().
bnei(As, Imm, Offset) when Offset >= -128, Offset =< 127 ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#6, reg_to_num(As), R, Offset).

%% BLTI: if AR[s] < b4const(r) then PC += sign_extend(imm8)
%% op0=6, t=0xA
-spec blti(xtensa_register(), integer(), integer()) -> binary().
blti(As, Imm, Offset) when Offset >= -128, Offset =< 127 ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#A, reg_to_num(As), R, Offset).

%%=============================================================================
%% B4CONST encoding table
%%=============================================================================

%% B4CONST table maps r field to constants for BEQI/BNEI/BLTI
b4const_encode(-1) -> 0;
b4const_encode(1) -> 1;
b4const_encode(2) -> 2;
b4const_encode(3) -> 3;
b4const_encode(4) -> 4;
b4const_encode(5) -> 5;
b4const_encode(6) -> 6;
b4const_encode(7) -> 7;
b4const_encode(8) -> 8;
b4const_encode(10) -> 9;
b4const_encode(12) -> 10;
b4const_encode(16) -> 11;
b4const_encode(32) -> 12;
b4const_encode(64) -> 13;
b4const_encode(128) -> 14;
b4const_encode(256) -> 15.

%%=============================================================================
%% Jump Instructions
%%=============================================================================

%% J: PC = PC + sign_extend(offset18) + 4
%% CALL format: op0=6, n=0
%% offset18 is an 18-bit signed value: range -131072..131071
-spec j(integer()) -> binary().
j(Offset) when Offset >= -131072, Offset =< 131071 ->
    encode_call(16#6, 0, Offset band 16#3FFFF);
j(Offset) ->
    error({j_offset_out_of_range, Offset}).

%% JX: PC = AR[s]
%% op0=0, t=0xa, s=register, r=0, op1=0, op2=0
-spec jx(xtensa_register()) -> binary().
jx(As) ->
    encode_rrr(0, 16#A, reg_to_num(As), 0, 0, 0).

%% CALLX8: indirect windowed call through register with 8-register rotation
%% op0=0, t=0xE, s=register, r=0, op1=0, op2=0
-spec callx8(xtensa_register()) -> binary().
callx8(As) ->
    encode_rrr(0, 16#E, reg_to_num(As), 0, 0, 0).

%% RETW: return from windowed subroutine. Rotates window back and PC = a0[29:0]
%% op0=0, t=0x9, s=0, r=0, op1=0, op2=0
-spec retw() -> binary().
retw() ->
    encode_rrr(0, 16#9, 0, 0, 0, 0).

%% ENTRY: allocate stack frame and rotate register window
%% BRI12 format: op0=6, t=3, s=register, imm12=framesize/8
-spec entry(xtensa_register(), non_neg_integer()) -> binary().
entry(As, FrameSize) when (FrameSize rem 8) =:= 0, FrameSize >= 0, FrameSize =< 32760 ->
    Imm12 = FrameSize bsr 3,
    encode_bri12(16#6, 16#3, reg_to_num(As), Imm12 band 16#FFF).

%%=============================================================================
%% L32R and Misc Instructions
%%=============================================================================

%% L32R: AR[t] = mem[((PC + 3) & ~3) + sign_extend(imm16) * 4 - ...]
%% op0=1
-spec l32r(xtensa_register(), integer()) -> binary().
l32r(At, Imm16) ->
    encode_ri16(16#1, reg_to_num(At), Imm16).

%% NOP
%% op0=0, t=15, s=0, r=2, op1=0, op2=0
-spec nop() -> binary().
nop() ->
    encode_rrr(0, 15, 0, 2, 0, 0).

%% BREAK: breakpoint/debug trap
%% op0=0, op1=0, op2=0, r=4
-spec break(integer(), integer()) -> binary().
break(S, T) ->
    encode_rrr(0, T band 16#F, S band 16#F, 16#4, 0, 0).
