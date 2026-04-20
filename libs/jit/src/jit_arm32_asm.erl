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

-module(jit_arm32_asm).

-export([
    add/3,
    add/4,
    sub/3,
    sub/4,
    subs/3,
    subs/4,
    mul/4,
    and_/3,
    and_/4,
    orr/3,
    orr/4,
    eor/3,
    eor/4,
    bic/3,
    bic/4,
    mvn/3,
    mov/3,
    mov/2,
    cmp/3,
    tst/3,
    ldr/3,
    str/3,
    lsl/3,
    lsl/4,
    lsr/3,
    lsr/4,
    asr/3,
    asr/4,
    b/2,
    blx/2,
    push/1,
    pop/1,
    bkpt/1,
    reg_to_num/1,
    encode_imm/1
]).

-export_type([
    cc/0
]).

-type arm_gpr_register() ::
    r0
    | r1
    | r2
    | r3
    | r4
    | r5
    | r6
    | r7
    | r8
    | r9
    | r10
    | r11
    | r12
    | r13
    | r14
    | r15
    | sp
    | lr
    | pc.

-type cc() :: eq | ne | cs | cc | mi | pl | vs | vc | hi | ls | ge | lt | gt | le | al.

%%-----------------------------------------------------------------------------
%% Helper functions
%%-----------------------------------------------------------------------------

reg_to_num(r0) -> 0;
reg_to_num(r1) -> 1;
reg_to_num(r2) -> 2;
reg_to_num(r3) -> 3;
reg_to_num(r4) -> 4;
reg_to_num(r5) -> 5;
reg_to_num(r6) -> 6;
reg_to_num(r7) -> 7;
reg_to_num(r8) -> 8;
reg_to_num(r9) -> 9;
reg_to_num(r10) -> 10;
reg_to_num(r11) -> 11;
reg_to_num(r12) -> 12;
reg_to_num(r13) -> 13;
reg_to_num(r14) -> 14;
reg_to_num(r15) -> 15;
reg_to_num(sp) -> 13;
reg_to_num(lr) -> 14;
reg_to_num(pc) -> 15.

-spec cond_to_num(cc()) -> 0..14.
cond_to_num(eq) -> 0;
cond_to_num(ne) -> 1;
cond_to_num(cs) -> 2;
cond_to_num(cc) -> 3;
cond_to_num(mi) -> 4;
cond_to_num(pl) -> 5;
cond_to_num(vs) -> 6;
cond_to_num(vc) -> 7;
cond_to_num(hi) -> 8;
cond_to_num(ls) -> 9;
cond_to_num(ge) -> 10;
cond_to_num(lt) -> 11;
cond_to_num(gt) -> 12;
cond_to_num(le) -> 13;
cond_to_num(al) -> 14.

%% Encode a 32-bit immediate as an 8-bit value with 4-bit rotation.
%% ARM32 data processing immediates are encoded as: imm8 ROR (rotate * 2)
%% Returns {Rotate, Imm8} or false if not encodable.
-spec encode_imm(non_neg_integer()) -> {non_neg_integer(), non_neg_integer()} | false.
encode_imm(Imm) when Imm >= 0, Imm =< 255 ->
    {0, Imm};
encode_imm(Imm) when is_integer(Imm), Imm >= 0, Imm =< 16#FFFFFFFF ->
    encode_imm(Imm, 1);
encode_imm(_) ->
    false.

encode_imm(_Imm, 16) ->
    false;
encode_imm(Imm, Rotate) ->
    %% ARM encoding: value = imm8 ROR (Rotate * 2)
    %% So imm8 = value ROL (Rotate * 2) = value ROR (32 - Rotate * 2)
    %% We rotate LEFT by Rotate*2 to find the candidate imm8
    Shift = Rotate * 2,
    Candidate = ((Imm bsl Shift) bor (Imm bsr (32 - Shift))) band 16#FFFFFFFF,
    case Candidate =< 255 of
        true -> {Rotate, Candidate};
        false -> encode_imm(Imm, Rotate + 1)
    end.

%%-----------------------------------------------------------------------------
%% Data processing instructions
%% Format: cond[31:28] 00 I[25] opcode[24:21] S[20] Rn[19:16] Rd[15:12] operand2[11:0]
%%-----------------------------------------------------------------------------

%% Helper for data processing register form (operand2 = Rm)
dp_reg(Cond, Opcode, S, Rd, Rn, Rm) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    Instr =
        (CondNum bsl 28) bor (Opcode bsl 21) bor (S bsl 20) bor
            (RnNum bsl 16) bor (RdNum bsl 12) bor RmNum,
    <<Instr:32/little>>.

%% Helper for data processing immediate form (operand2 = rotate:imm8)
dp_imm(Cond, Opcode, S, Rd, Rn, Imm) ->
    case encode_imm(Imm band 16#FFFFFFFF) of
        {Rotate, Imm8} ->
            CondNum = cond_to_num(Cond),
            RdNum = reg_to_num(Rd),
            RnNum = reg_to_num(Rn),
            Instr =
                (CondNum bsl 28) bor (1 bsl 25) bor (Opcode bsl 21) bor (S bsl 20) bor
                    (RnNum bsl 16) bor (RdNum bsl 12) bor (Rotate bsl 8) bor Imm8,
            <<Instr:32/little>>;
        false ->
            error({unencodable_immediate, Imm})
    end.

%% ADD Rd, Rn, Rm (3-register form)
-spec add(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
add(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#0100, 0, Rd, Rn, Rm);
add(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#0100, 0, Rd, Rn, Imm).

%% ADD Rd, Rd, Rm/Imm (2-operand shorthand)
-spec add(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
add(Cond, Rd, RmOrImm) ->
    add(Cond, Rd, Rd, RmOrImm).

%% SUB Rd, Rn, Rm/Imm
-spec sub(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
sub(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#0010, 0, Rd, Rn, Rm);
sub(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#0010, 0, Rd, Rn, Imm).

-spec sub(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
sub(Cond, Rd, RmOrImm) ->
    sub(Cond, Rd, Rd, RmOrImm).

%% SUBS Rd, Rn, Rm/Imm (subtract with flag setting)
-spec subs(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) ->
    binary().
subs(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#0010, 1, Rd, Rn, Rm);
subs(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#0010, 1, Rd, Rn, Imm).

-spec subs(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
subs(Cond, Rd, RmOrImm) ->
    subs(Cond, Rd, Rd, RmOrImm).

%% AND Rd, Rn, Rm/Imm
-spec and_(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) ->
    binary().
and_(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#0000, 0, Rd, Rn, Rm);
and_(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#0000, 0, Rd, Rn, Imm).

-spec and_(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
and_(Cond, Rd, RmOrImm) ->
    and_(Cond, Rd, Rd, RmOrImm).

%% ORR Rd, Rn, Rm/Imm
-spec orr(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
orr(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1100, 0, Rd, Rn, Rm);
orr(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1100, 0, Rd, Rn, Imm).

-spec orr(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
orr(Cond, Rd, RmOrImm) ->
    orr(Cond, Rd, Rd, RmOrImm).

%% EOR Rd, Rn, Rm/Imm
-spec eor(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
eor(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#0001, 0, Rd, Rn, Rm);
eor(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#0001, 0, Rd, Rn, Imm).

-spec eor(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
eor(Cond, Rd, RmOrImm) ->
    eor(Cond, Rd, Rd, RmOrImm).

%% BIC Rd, Rn, Rm/Imm (bit clear = AND NOT)
-spec bic(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
bic(Cond, Rd, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1110, 0, Rd, Rn, Rm);
bic(Cond, Rd, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1110, 0, Rd, Rn, Imm).

-spec bic(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
bic(Cond, Rd, RmOrImm) ->
    bic(Cond, Rd, Rd, RmOrImm).

%% MVN Rd, Rm/Imm (move NOT)
-spec mvn(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
mvn(Cond, Rd, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1111, 0, Rd, r0, Rm);
mvn(Cond, Rd, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1111, 0, Rd, r0, Imm).

%% MOV Rd, Rm/Imm
-spec mov(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
mov(Cond, Rd, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1101, 0, Rd, r0, Rm);
mov(Cond, Rd, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1101, 0, Rd, r0, Imm).

%% MOV Rd, Rm (unconditional, convenience form)
-spec mov(arm_gpr_register(), arm_gpr_register()) -> binary().
mov(Rd, Rm) ->
    mov(al, Rd, Rm).

%% CMP Rn, Rm/Imm (sets flags, no destination register)
-spec cmp(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
cmp(Cond, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1010, 1, r0, Rn, Rm);
cmp(Cond, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1010, 1, r0, Rn, Imm).

%% TST Rn, Rm/Imm (sets flags, no destination register)
-spec tst(cc(), arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
tst(Cond, Rn, Rm) when is_atom(Rm) ->
    dp_reg(Cond, 2#1000, 1, r0, Rn, Rm);
tst(Cond, Rn, Imm) when is_integer(Imm) ->
    dp_imm(Cond, 2#1000, 1, r0, Rn, Imm).

%%-----------------------------------------------------------------------------
%% Multiply instructions
%%-----------------------------------------------------------------------------

%% MUL Rd, Rm, Rs (Rd = Rm * Rs)
%% Encoding: cond[31:28] 0000 000 S Rd[19:16] 0000 Rs[11:8] 1001 Rm[3:0]
-spec mul(cc(), arm_gpr_register(), arm_gpr_register(), arm_gpr_register()) -> binary().
mul(Cond, Rd, Rm, Rs) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    RsNum = reg_to_num(Rs),
    Instr =
        (CondNum bsl 28) bor (RdNum bsl 16) bor (RsNum bsl 8) bor
            (2#1001 bsl 4) bor RmNum,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Shift instructions (data processing with shifted register)
%%-----------------------------------------------------------------------------

%% LSL Rd, Rm, #imm5 (logical shift left by immediate)
%% Encoding: MOV Rd, Rm, LSL #imm5
-spec lsl(cc(), arm_gpr_register(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
lsl(Cond, Rd, Rm, Imm) when is_integer(Imm), Imm >= 0, Imm =< 31 ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% MOV Rd, Rm, LSL #imm5: cond 000 1101 0 0000 Rd imm5 000 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (Imm bsl 7) bor RmNum,
    <<Instr:32/little>>;
%% LSL Rd, Rm, Rs (register shift)
lsl(Cond, Rd, Rm, Rs) when is_atom(Rs) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    RsNum = reg_to_num(Rs),
    %% MOV Rd, Rm, LSL Rs: cond 000 1101 0 0000 Rd Rs 0001 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (RsNum bsl 8) bor (2#0001 bsl 4) bor RmNum,
    <<Instr:32/little>>.

%% LSL Rd, Rd, Rs/Imm (2-operand shorthand)
-spec lsl(cc(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
lsl(Cond, Rd, RsOrImm) ->
    lsl(Cond, Rd, Rd, RsOrImm).

%% LSR Rd, Rm, #imm5 (logical shift right by immediate)
-spec lsr(cc(), arm_gpr_register(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
lsr(Cond, Rd, Rm, Imm) when is_integer(Imm), Imm >= 1, Imm =< 32 ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% imm5=0 means shift by 32
    Imm5 =
        if
            Imm =:= 32 -> 0;
            true -> Imm
        end,
    %% MOV Rd, Rm, LSR #imm5: cond 000 1101 0 0000 Rd imm5 010 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (Imm5 bsl 7) bor (2#010 bsl 4) bor RmNum,
    <<Instr:32/little>>;
%% LSR Rd, Rm, Rs (register shift)
lsr(Cond, Rd, Rm, Rs) when is_atom(Rs) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    RsNum = reg_to_num(Rs),
    %% MOV Rd, Rm, LSR Rs: cond 000 1101 0 0000 Rd Rs 0011 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (RsNum bsl 8) bor (2#0011 bsl 4) bor RmNum,
    <<Instr:32/little>>.

-spec lsr(cc(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
lsr(Cond, Rd, RsOrImm) ->
    lsr(Cond, Rd, Rd, RsOrImm).

%% ASR Rd, Rm, #imm5 (arithmetic shift right by immediate)
-spec asr(cc(), arm_gpr_register(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
asr(Cond, Rd, Rm, Imm) when is_integer(Imm), Imm >= 1, Imm =< 32 ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    Imm5 =
        if
            Imm =:= 32 -> 0;
            true -> Imm
        end,
    %% MOV Rd, Rm, ASR #imm5: cond 000 1101 0 0000 Rd imm5 100 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (Imm5 bsl 7) bor (2#100 bsl 4) bor RmNum,
    <<Instr:32/little>>;
%% ASR Rd, Rm, Rs (register shift)
asr(Cond, Rd, Rm, Rs) when is_atom(Rs) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    RsNum = reg_to_num(Rs),
    %% MOV Rd, Rm, ASR Rs: cond 000 1101 0 0000 Rd Rs 0101 Rm
    Instr =
        (CondNum bsl 28) bor (2#1101 bsl 21) bor (RdNum bsl 12) bor
            (RsNum bsl 8) bor (2#0101 bsl 4) bor RmNum,
    <<Instr:32/little>>.

-spec asr(cc(), arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
asr(Cond, Rd, RsOrImm) ->
    asr(Cond, Rd, Rd, RsOrImm).

%%-----------------------------------------------------------------------------
%% Load/Store instructions
%% Format: cond[31:28] 01 I[25] P[24] U[23] B[22] W[21] L[20] Rn[19:16] Rd[15:12] offset[11:0]
%%-----------------------------------------------------------------------------

%% LDR Rd, [Rn, #offset] or LDR Rd, [Rn, Rm]
-spec ldr(cc(), arm_gpr_register(), {arm_gpr_register(), integer() | arm_gpr_register()}) ->
    binary().
ldr(Cond, Rd, {Rn, Offset}) when is_integer(Offset) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    {U, AbsOffset} =
        if
            Offset >= 0 -> {1, Offset};
            true -> {0, -Offset}
        end,
    true = (AbsOffset =< 4095),
    %% I=0 (immediate), P=1 (pre-indexed), U, B=0 (word), W=0 (no writeback), L=1 (load)
    Instr =
        (CondNum bsl 28) bor (2#01 bsl 26) bor (1 bsl 24) bor (U bsl 23) bor
            (1 bsl 20) bor (RnNum bsl 16) bor (RdNum bsl 12) bor AbsOffset,
    <<Instr:32/little>>;
ldr(Cond, Rd, {Rn, Rm}) when is_atom(Rm) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% I=1 (register), P=1, U=1 (add), B=0, W=0, L=1
    Instr =
        (CondNum bsl 28) bor (2#011 bsl 25) bor (1 bsl 24) bor (1 bsl 23) bor
            (1 bsl 20) bor (RnNum bsl 16) bor (RdNum bsl 12) bor RmNum,
    <<Instr:32/little>>.

%% STR Rd, [Rn, #offset] or STR Rd, [Rn, Rm]
-spec str(cc(), arm_gpr_register(), {arm_gpr_register(), integer() | arm_gpr_register()}) ->
    binary().
str(Cond, Rd, {Rn, Offset}) when is_integer(Offset) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    {U, AbsOffset} =
        if
            Offset >= 0 -> {1, Offset};
            true -> {0, -Offset}
        end,
    true = (AbsOffset =< 4095),
    %% I=0 (immediate), P=1, U, B=0, W=0, L=0 (store)
    Instr =
        (CondNum bsl 28) bor (2#01 bsl 26) bor (1 bsl 24) bor (U bsl 23) bor
            (RnNum bsl 16) bor (RdNum bsl 12) bor AbsOffset,
    <<Instr:32/little>>;
str(Cond, Rd, {Rn, Rm}) when is_atom(Rm) ->
    CondNum = cond_to_num(Cond),
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% I=1 (register), P=1, U=1, B=0, W=0, L=0
    Instr =
        (CondNum bsl 28) bor (2#011 bsl 25) bor (1 bsl 24) bor (1 bsl 23) bor
            (RnNum bsl 16) bor (RdNum bsl 12) bor RmNum,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Branch instructions
%%-----------------------------------------------------------------------------

%% B{cond} offset - branch with condition
%% Encoding: cond[31:28] 1010 imm24[23:0]
%% offset is in bytes relative to PC (current instruction + 8 in ARM mode)
-spec b(cc(), integer()) -> binary().
b(Cond, Offset) when is_integer(Offset) ->
    CondNum = cond_to_num(Cond),
    %% ARM PC is current instruction + 8, so adjust
    AdjustedOffset = Offset - 8,
    %% Offset must be word-aligned (multiple of 4)
    0 = AdjustedOffset rem 4,
    Imm24 = AdjustedOffset div 4,
    %% Check range: 24-bit signed (-2^23 to 2^23-1)
    true = (Imm24 >= -(1 bsl 23)) andalso (Imm24 < (1 bsl 23)),
    Instr = (CondNum bsl 28) bor (2#1010 bsl 24) bor (Imm24 band 16#FFFFFF),
    <<Instr:32/little>>.

%% BLX{cond} Rm - branch with link and exchange
%% Encoding: cond[31:28] 0001 0010 1111 1111 1111 0011 Rm[3:0]
-spec blx(cc(), arm_gpr_register()) -> binary().
blx(Cond, Rm) ->
    CondNum = cond_to_num(Cond),
    RmNum = reg_to_num(Rm),
    Instr = (CondNum bsl 28) bor 16#012FFF30 bor RmNum,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Stack operations (PUSH/POP via STMDB/LDMIA)
%%-----------------------------------------------------------------------------

%% PUSH {reglist} = STMDB SP!, {reglist}
%% Encoding: cond[31:28] 100 1 0 0 1 0 1101 reglist[15:0]
-spec push([arm_gpr_register()]) -> binary().
push([sp]) ->
    %% GNU as uses STMDB for push {sp} (STR SP,[SP,#-4]! has undefined behavior)
    RegMask = 1 bsl 13,
    Instr = (14 bsl 28) bor (2#100100101101 bsl 16) bor RegMask,
    <<Instr:32/little>>;
push([Reg]) ->
    %% STR Rd, [SP, #-4]!: single-register push (matches GNU as encoding)
    RegNum = reg_to_num(Reg),
    Instr = 16#E52D0004 bor (RegNum bsl 12),
    <<Instr:32/little>>;
push(RegList) ->
    RegMask = reglist_to_mask(RegList),
    %% STMDB SP!: cond=AL 1001 0010 1101 reglist
    Instr = (14 bsl 28) bor (2#100100101101 bsl 16) bor RegMask,
    <<Instr:32/little>>.

%% POP {reglist} = LDMIA SP!, {reglist}
%% Encoding: cond[31:28] 100 0 1 0 1 1 1101 reglist[15:0]
-spec pop([arm_gpr_register()]) -> binary().
pop([sp]) ->
    %% GNU as uses LDMIA for pop {sp} (LDR SP,[SP],#4 has undefined behavior)
    RegMask = 1 bsl 13,
    Instr = (14 bsl 28) bor (2#100010111101 bsl 16) bor RegMask,
    <<Instr:32/little>>;
pop([Reg]) ->
    %% LDR Rd, [SP], #4: single-register pop (matches GNU as encoding)
    RegNum = reg_to_num(Reg),
    Instr = 16#E49D0004 bor (RegNum bsl 12),
    <<Instr:32/little>>;
pop(RegList) ->
    RegMask = reglist_to_mask(RegList),
    %% LDMIA SP!: cond=AL 1000 1011 1101 reglist
    Instr = (14 bsl 28) bor (2#100010111101 bsl 16) bor RegMask,
    <<Instr:32/little>>.

reglist_to_mask(RegList) ->
    lists:foldl(
        fun(Reg, Acc) ->
            Acc bor (1 bsl reg_to_num(Reg))
        end,
        0,
        RegList
    ).

%%-----------------------------------------------------------------------------
%% Miscellaneous instructions
%%-----------------------------------------------------------------------------

%% BKPT #imm16 (always unconditional)
%% Encoding: 1110 0001 0010 imm12[19:8] 0111 imm4[3:0]
-spec bkpt(non_neg_integer()) -> binary().
bkpt(Imm) when is_integer(Imm), Imm >= 0, Imm =< 16#FFFF ->
    Imm12 = (Imm bsr 4) band 16#FFF,
    Imm4 = Imm band 16#F,
    Instr = (14 bsl 28) bor (2#00010010 bsl 20) bor (Imm12 bsl 8) bor (2#0111 bsl 4) bor Imm4,
    <<Instr:32/little>>.
