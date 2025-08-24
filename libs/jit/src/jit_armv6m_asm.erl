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

-module(jit_armv6m_asm).

-export([
    adds/2,
    adds/3,
    subs/2,
    subs/3,
    muls/2,
    b/1,
    bcc/2,
    blx/1,
    bx/1,
    cmp/2,
    ands/2,
    orrs/2,
    ldr/2,
    lsls/2,
    lsls/3,
    lsrs/2,
    lsrs/3,
    mov/2,
    movs/2,
    str/2,
    tst/2,
    stp/4,
    ldp/4,
    adr/2,
    push/1,
    pop/1
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

%% Convert register atoms to register numbers for assembly generation
%% for r0 to r30
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
%% Stack pointer (SP) is r13
reg_to_num(sp) -> 13;
%% Link register (LR) is r14
reg_to_num(lr) -> 14;
%% Program counter (PC) is r15
reg_to_num(pc) -> 15.

%% Convert condition atom to condition code number
-spec cond_to_num(atom()) -> 0..15.
% Equal (Z set)
cond_to_num(eq) -> 0;
% Not equal (Z clear)
cond_to_num(ne) -> 1;
% Carry set
cond_to_num(cs) -> 2;
% Carry clear
cond_to_num(cc) -> 3;
% Minus (N set)
cond_to_num(mi) -> 4;
% Plus (N clear)
cond_to_num(pl) -> 5;
% Overflow set
cond_to_num(vs) -> 6;
% Overflow clear
cond_to_num(vc) -> 7;
% Higher (unsigned)
cond_to_num(hi) -> 8;
% Lower or same (unsigned)
cond_to_num(ls) -> 9;
% Greater than or equal (signed)
cond_to_num(ge) -> 10;
% Less than (signed)
cond_to_num(lt) -> 11;
% Greater than (signed)
cond_to_num(gt) -> 12;
% Less than or equal (signed)
cond_to_num(le) -> 13;
% Always
cond_to_num(al) -> 14;
% Never
cond_to_num(nv) -> 15.

%% Emit an ADDS instruction (Thumb encoding)
%% ADDS Rd, #imm - adds immediate value to register and sets flags (2-operand form)
-spec adds(arm_gpr_register(), integer()) -> binary().
adds(Rd, Imm) when is_atom(Rd), is_integer(Imm), Imm >= 0, Imm =< 255 ->
    adds(Rd, Rd, Imm);
adds(Rd, Imm) when is_atom(Rd), is_integer(Imm) ->
    error({unencodable_immediate, Imm}).

%% ADDS Rd, Rn, #imm - adds immediate value to register and sets flags (3-operand form)
-spec adds(arm_gpr_register(), arm_gpr_register(), integer()) -> binary().

adds(Rd, Rd, Imm) when is_atom(Rd), is_integer(Imm), Imm >= 0, Imm =< 255 ->
    %% Thumb ADDS (immediate, 8-bit) encoding: 00110dddiiiiiiii (Rd = Rn)
    RdNum = reg_to_num(Rd),
    <<(16#3000 bor ((RdNum band 7) bsl 8) bor (Imm band 255)):16/little>>;
adds(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 7 ->
    %% Thumb ADDS (immediate, 3-bit) encoding: 0001110iiinnnddd
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    <<(16#1C00 bor ((Imm band 7) bsl 6) bor ((RnNum band 7) bsl 3) bor (RdNum band 7)):16/little>>;
adds(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm) ->
    error({unencodable_immediate, Imm});
adds(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    %% Thumb ADDS (register) encoding: 0001100mmmnnnddd
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    <<
        (16#1800 bor ((RmNum band 7) bsl 6) bor ((RnNum band 7) bsl 3) bor (RdNum band 7)):16/little
    >>.

%% Emit an unconditional branch (B) instruction (Thumb encoding, ARMv6-M)
%% offset is in bytes, relative to the PC+4 (next instruction)
%% ARMv6-M only supports 16-bit Thumb B with ±2KB range
-spec b(integer()) -> binary().
b(Offset) when is_integer(Offset), Offset >= -2044, Offset =< 2050, (Offset rem 2) =:= 0 ->
    %% Thumb B (unconditional) encoding: 11100iiiiiiiiiii
    %% imm11 is (Offset / 2) signed, fits in 11 bits
    %% Adjust offset by -4 to match assembler behavior (PC+4 relative)
    AdjustedOffset = Offset - 4,
    Offset11 = AdjustedOffset div 2,
    <<(16#E000 bor (Offset11 band 16#7FF)):16/little>>;
b(Offset) when is_integer(Offset) ->
    error({unencodable_offset, Offset}).

%% Emit a branch with link register (BLR) instruction (Thumb encoding)
%% Register is the register atom (r0-r15)
-spec blx(arm_gpr_register()) -> binary().
blx(Reg) when is_atom(Reg) ->
    RegNum = reg_to_num(Reg),
    %% Thumb BLX (register) encoding: 010001111mmmm000
    %% This branches to register and sets LR
    <<(16#4780 bor (RegNum bsl 3)):16/little>>.

%% Emit a branch register (BR) instruction (Thumb encoding)
%% Register is the register atom (r0-r15)
-spec bx(arm_gpr_register()) -> binary().
bx(Reg) when is_atom(Reg) ->
    RegNum = reg_to_num(Reg),
    %% Thumb BX (branch exchange) encoding: 010001110mmmm000
    %% This branches to register without setting LR
    <<(16#4700 bor (RegNum bsl 3)):16/little>>.

%% Emit a load register (LDR) instruction
-spec ldr(arm_gpr_register(), {arm_gpr_register(), integer()}) -> binary().
%% LDR Rt, [Rn, #imm5*4] - 16-bit immediate offset (0-124, multiple of 4)
ldr(Rt, {Rn, Imm}) when
    is_atom(Rt),
    is_atom(Rn),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 124,
    (Imm rem 4) =:= 0,
    Rt =/= sp,
    Rt =/= pc,
    Rn =/= sp,
    Rn =/= pc
->
    RtNum = reg_to_num(Rt),
    RnNum = reg_to_num(Rn),
    Imm5 = Imm div 4,
    %% Thumb LDR immediate: 01101iiiiinnnttt
    <<(16#6800 bor (Imm5 bsl 6) bor (RnNum bsl 3) bor RtNum):16/little>>;
%% LDR Rt, [SP, #imm8*4] - SP-relative load (0-1020, multiple of 4)
ldr(Rt, {sp, Imm}) when
    is_atom(Rt),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 1020,
    (Imm rem 4) =:= 0
->
    RtNum = reg_to_num(Rt),
    Imm8 = Imm div 4,
    %% Thumb LDR SP-relative: 10011tttiiiiiiii
    <<(16#9800 bor (RtNum bsl 8) bor Imm8):16/little>>;
%% LDR Rt, [PC, #imm8*4] - PC-relative load (0-1020, multiple of 4)
ldr(Rt, {pc, Imm}) when
    is_atom(Rt),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 1020,
    (Imm rem 4) =:= 0
->
    RtNum = reg_to_num(Rt),
    Imm8 = Imm div 4,
    %% Thumb LDR PC-relative: 01001tttiiiiiiii
    <<(16#4800 bor (RtNum bsl 8) bor Imm8):16/little>>;
%% LDR Rt, [Rn, Rm] - register offset
ldr(Rt, {Rn, Rm}) when
    is_atom(Rt),
    is_atom(Rn),
    is_atom(Rm),
    Rt =/= sp,
    Rt =/= pc,
    Rn =/= sp,
    Rn =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RtNum = reg_to_num(Rt),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% Thumb LDR register: 0101100mmmnnntttt
    <<(16#5800 bor (RmNum bsl 6) bor (RnNum bsl 3) bor RtNum):16/little>>.

%% ARMv6-M Thumb MOVS instruction (sets flags)
-spec movs(arm_gpr_register(), integer() | arm_gpr_register()) -> binary().
%% MOVS immediate - 8-bit immediates only (0-255)
movs(Rd, Imm) when
    is_atom(Rd),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 255
->
    RdNum = reg_to_num(Rd),
    %% Thumb MOVS immediate: 00100dddiiiiiiii
    <<(16#2000 bor (RdNum bsl 8) bor Imm):16/little>>;
%% MOVS register - low registers only (both must be r0-r7)
movs(Rd, Rm) when
    is_atom(Rd), is_atom(Rm)
->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    case RdNum =< 7 andalso RmNum =< 7 of
        true ->
            %% Thumb MOVS register: 0000000000mmmdddd
            <<(16#0000 bor (RmNum bsl 3) bor RdNum):16/little>>;
        false ->
            error({movs_requires_low_registers, {Rd, Rm}})
    end.

%% ARMv6-M Thumb MOV instruction - handle both immediate and register moves
-spec mov(arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
%% MOV immediate (using MOVS for low registers with immediate 0-255)
mov(Rd, Imm) when is_atom(Rd), is_integer(Imm), Imm >= 0, Imm =< 255 ->
    RdNum = reg_to_num(Rd),
    case RdNum =< 7 of
        true ->
            %% Use MOVS for low registers with immediate
            movs(Rd, Imm);
        false ->
            %% For high registers, need to use a different approach
            %% ARMv6-M doesn't support immediate moves to high registers directly
            error({unsupported_immediate_to_high_register, Rd, Imm})
    end;
%% MOV register - handle both high and low register cases
mov(Rd, Rm) when is_atom(Rd), is_atom(Rm) ->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    case RdNum >= 8 orelse RmNum >= 8 of
        true ->
            %% Thumb MOV high register: 01000110DMmmmdddd
            D =
                if
                    RdNum >= 8 -> 1;
                    true -> 0
                end,
            M =
                if
                    RmNum >= 8 -> 1;
                    true -> 0
                end,
            RdLow = RdNum band 7,
            RmLow = RmNum band 7,
            <<(16#4600 bor (D bsl 7) bor (M bsl 6) bor (RmLow bsl 3) bor RdLow):16/little>>;
        false ->
            %% For low registers, use ADDS Rd, Rm, #0 (ARMv6-M standard practice)
            adds(Rd, Rm, 0)
    end.

%% ARMv6-M Thumb STR immediate offset (0-124, multiple of 4)
str(Rt, {Rn, Imm}) when
    is_atom(Rt),
    is_atom(Rn),
    is_integer(Imm),
    Rt =/= sp,
    Rt =/= pc,
    Rn =/= sp,
    Rn =/= pc,
    Imm >= 0,
    Imm =< 124,
    (Imm rem 4) =:= 0
->
    RtNum = reg_to_num(Rt),
    RnNum = reg_to_num(Rn),
    Imm5 = Imm div 4,
    %% Thumb STR immediate: 01100iiiiinnnttt
    <<(16#6000 bor (Imm5 bsl 6) bor (RnNum bsl 3) bor RtNum):16/little>>;
%% SP-relative STR (0-1020, multiple of 4)
str(Rt, {sp, Imm}) when
    is_atom(Rt),
    is_integer(Imm),
    Rt =/= sp,
    Rt =/= pc,
    Imm >= 0,
    Imm =< 1020,
    (Imm rem 4) =:= 0
->
    RtNum = reg_to_num(Rt),
    Imm8 = Imm div 4,
    %% Thumb STR SP relative: 1001ttttiiiiiiiii
    <<(16#9000 bor (RtNum bsl 8) bor Imm8):16/little>>;
%% STR Rt, [Rn, Rm] - register offset
str(Rt, {Rn, Rm}) when
    is_atom(Rt),
    is_atom(Rn),
    is_atom(Rm),
    Rt =/= sp,
    Rt =/= pc,
    Rn =/= sp,
    Rn =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RtNum = reg_to_num(Rt),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% Thumb STR register: 0101000mmmnnntttt
    <<(16#5000 bor (RmNum bsl 6) bor (RnNum bsl 3) bor RtNum):16/little>>.

%% Emit a store pair (STP) instruction for 64-bit registers
%% stp(Rn, Rm, {Base}, Imm) -> binary()
%% stp(Rn, Rm, {Base, Imm}, '!') -> binary() (store-update)
-spec stp(
    arm_gpr_register(),
    arm_gpr_register(),
    {arm_gpr_register()} | {arm_gpr_register(), integer()},
    integer() | '!'
) -> binary().
stp(Rn, Rm, {Base}, Imm) when
    is_atom(Rn),
    is_atom(Rm),
    is_atom(Base),
    is_integer(Imm),
    Imm >= -512,
    Imm =< 504,
    (Imm rem 8) =:= 0
->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    BaseNum = reg_to_num(Base),
    %% STP encoding: 1010100010|imm7|base|rm|rn
    %% 0xa9bf0000 | ((Imm div 8) band 0x7f) << 15 | Base << 5 | Rm << 10 | Rn
    <<
        (16#A8800000 bor ((Imm div 8) bsl 15) bor (BaseNum bsl 5) bor (RmNum bsl 10) bor RnNum):32/little
    >>;
stp(Rn, Rm, {Base, Imm}, '!') when
    is_atom(Rn),
    is_atom(Rm),
    is_atom(Base),
    is_integer(Imm),
    Imm >= -512,
    Imm =< 504,
    (Imm rem 8) =:= 0
->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    BaseNum = reg_to_num(Base),
    <<
        (16#A9800000 bor (((Imm div 8) band 16#7F) bsl 15) bor (BaseNum bsl 5) bor (RmNum bsl 10) bor
            RnNum):32/little
    >>.

%% Emit a load pair (LDP) instruction for 64-bit registers
%% ldp(Rn, Rm, {Base}, Imm) -> binary()
-spec ldp(arm_gpr_register(), arm_gpr_register(), {arm_gpr_register()}, integer()) ->
    binary().
ldp(Rn, Rm, {Base}, Imm) when
    is_atom(Rn),
    is_atom(Rm),
    is_atom(Base),
    is_integer(Imm),
    Imm >= -512,
    Imm =< 504,
    (Imm rem 8) =:= 0
->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    BaseNum = reg_to_num(Base),
    %% LDP encoding: 1010100011|imm7|base|rm|rn
    <<
        (16#A8C00000 bor (((Imm div 8) band 16#7F) bsl 15) bor (BaseNum bsl 5) bor (RmNum bsl 10) bor
            RnNum):32/little
    >>.

%% Emit a conditional branch instruction
-spec bcc(cc(), integer()) -> binary().
%% Special case: 'al' (always) condition uses unconditional branch for efficiency
bcc(al, Offset) when is_integer(Offset) ->
    b(Offset);
bcc(Cond, Offset) when
    is_atom(Cond), is_integer(Offset), Offset >= -252, Offset =< 258, (Offset rem 2) =:= 0
->
    CondNum = cond_to_num(Cond),
    %% Thumb conditional branch encoding (ARMv6-M): 1101cccciiiiiiiii
    %% imm8 is (Offset / 2) signed, fits in 8 bits
    %% ARMv6-M only supports 16-bit Thumb conditional branches with ±256B range
    %% Adjust offset by -4 to match assembler behavior (PC+4 relative)
    AdjustedOffset = Offset - 4,
    Offset8 = AdjustedOffset div 2,
    <<(16#D000 bor (CondNum bsl 8) bor (Offset8 band 16#FF)):16/little>>;
bcc(Cond, Offset) when is_atom(Cond), is_integer(Offset) ->
    error({unencodable_offset, Offset}).

%% ARMv6-M Thumb CMP instruction
-spec cmp(arm_gpr_register(), arm_gpr_register() | integer()) -> binary().
%% CMP register-register form (low registers only)
cmp(Rn, Rm) when
    is_atom(Rn),
    is_atom(Rm),
    Rn =/= sp,
    Rn =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    case RnNum =< 7 andalso RmNum =< 7 of
        true ->
            %% Thumb CMP register: 0100001010mmmnnn
            <<(16#4280 bor (RmNum bsl 3) bor RnNum):16/little>>;
        false ->
            error({cmp_requires_low_registers, {Rn, Rm}})
    end;
%% CMP register-immediate form (8-bit immediate 0-255)
cmp(Rn, Imm) when
    is_atom(Rn),
    is_integer(Imm),
    Rn =/= sp,
    Rn =/= pc,
    Imm >= 0,
    Imm =< 255
->
    RnNum = reg_to_num(Rn),
    case RnNum =< 7 of
        true ->
            %% Thumb CMP immediate: 00101nnniiiiiiiii
            <<(16#2800 bor (RnNum bsl 8) bor Imm):16/little>>;
        false ->
            error({cmp_immediate_requires_low_register, Rn})
    end;
cmp(Rn, Imm) when is_atom(Rn), is_integer(Imm) ->
    error({unencodable_immediate, Imm}).

%% Emit an AND instruction (bitwise AND)
%% ARMv6-M Thumb ANDS instruction (register only - no immediate support)
-spec ands(arm_gpr_register(), arm_gpr_register()) -> binary().
ands(Rd, Rm) when
    is_atom(Rd),
    is_atom(Rm),
    Rd =/= sp,
    Rd =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% Thumb ANDS (2-operand): 0100000000mmmddd
    <<(16#4000 bor (RmNum bsl 3) bor RdNum):16/little>>.

%% ARMv6-M Thumb ORRS instruction (register only - sets flags)
-spec orrs(arm_gpr_register(), arm_gpr_register()) -> binary().
orrs(Rd, Rm) when
    is_atom(Rd),
    is_atom(Rm),
    Rd =/= sp,
    Rd =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% Thumb ORRS (2-operand): 0100001100mmmddd
    <<(16#4300 bor (RmNum bsl 3) bor RdNum):16/little>>.

%% ARMv6-M Thumb logical shift left (LSLS) instructions
-spec lsls(arm_gpr_register(), arm_gpr_register(), integer()) -> binary().
%% LSLS Rd, Rm, #imm5 - immediate shift (1-31)
lsls(Rd, Rm, Imm) when
    is_atom(Rd),
    is_atom(Rm),
    is_integer(Imm),
    Imm >= 1,
    Imm =< 31,
    Rd =/= sp,
    Rd =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% Thumb LSLS immediate: 00000iiiiimmmddd
    <<(16#0000 bor (Imm bsl 6) bor (RmNum bsl 3) bor RdNum):16/little>>.

-spec lsls(arm_gpr_register(), arm_gpr_register()) -> binary().
%% LSLS Rdn, Rm - register shift (Rdn = Rdn << Rm)
lsls(Rdn, Rm) when
    is_atom(Rdn),
    is_atom(Rm),
    Rdn =/= sp,
    Rdn =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdnNum = reg_to_num(Rdn),
    RmNum = reg_to_num(Rm),
    %% Thumb LSLS register: 0100000010mmmddd
    <<(16#4080 bor (RmNum bsl 3) bor RdnNum):16/little>>.

%% ARMv6-M Thumb logical shift right (LSRS) instructions
-spec lsrs(arm_gpr_register(), arm_gpr_register(), integer()) -> binary().
%% LSRS Rd, Rm, #imm5 - immediate shift (1-32)
lsrs(Rd, Rm, Imm) when
    is_atom(Rd),
    is_atom(Rm),
    is_integer(Imm),
    Imm >= 1,
    Imm =< 32,
    Rd =/= sp,
    Rd =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    %% Thumb LSRS immediate: 00001iiiiimmmddd (imm5=0 means shift by 32)
    Imm5 =
        if
            Imm =:= 32 -> 0;
            true -> Imm
        end,
    <<(16#0800 bor (Imm5 bsl 6) bor (RmNum bsl 3) bor RdNum):16/little>>.

-spec lsrs(arm_gpr_register(), arm_gpr_register()) -> binary().
%% LSRS Rdn, Rm - register shift (Rdn = Rdn >> Rm)
lsrs(Rdn, Rm) when
    is_atom(Rdn),
    is_atom(Rm),
    Rdn =/= sp,
    Rdn =/= pc,
    Rm =/= sp,
    Rm =/= pc
->
    RdnNum = reg_to_num(Rdn),
    RmNum = reg_to_num(Rm),
    %% Thumb LSRS register: 0100000011mmmddd
    <<(16#40C0 bor (RmNum bsl 3) bor RdnNum):16/little>>.

%% ARMv6-M Thumb TST instruction (register only)
-spec tst(arm_gpr_register(), arm_gpr_register()) -> binary().
%% TST Rn, Rm - test bits (performs Rn & Rm, updates flags, low registers only)
tst(Rn, Rm) when is_atom(Rn), is_atom(Rm) ->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    case RnNum =< 7 andalso RmNum =< 7 of
        true ->
            %% Thumb TST register: 0100001000mmmnnn
            <<(16#4200 bor (RmNum bsl 3) bor RnNum):16/little>>;
        false ->
            error({tst_requires_low_registers, {Rn, Rm}})
    end.

%% Emit a SUBS instruction (Thumb encoding)
%% SUBS Rd, #imm - subtracts immediate value from register and sets flags (2-operand form)
-spec subs(arm_gpr_register(), integer()) -> binary().
subs(Rd, Imm) when is_atom(Rd), is_integer(Imm), Imm >= 0, Imm =< 255 ->
    subs(Rd, Rd, Imm);
subs(Rd, Imm) when is_atom(Rd), is_integer(Imm) ->
    error({unencodable_immediate, Imm}).

%% SUBS Rd, Rn, #imm - subtracts immediate value from register and sets flags (3-operand form)
-spec subs(arm_gpr_register(), arm_gpr_register(), integer()) -> binary().
subs(Rd, Rd, Imm) when is_atom(Rd), is_integer(Imm), Imm >= 0, Imm =< 255 ->
    %% Thumb SUBS (immediate, 8-bit) encoding: 00111dddiiiiiiii (Rd = Rn)
    RdNum = reg_to_num(Rd),
    <<(16#3800 bor ((RdNum band 7) bsl 8) bor (Imm band 255)):16/little>>;
subs(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 7 ->
    %% Thumb SUBS (immediate, 3-bit) encoding: 0001111iiinnnddd
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    <<(16#1E00 bor ((Imm band 7) bsl 6) bor ((RnNum band 7) bsl 3) bor (RdNum band 7)):16/little>>;
subs(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm) ->
    error({unencodable_immediate, Imm});
subs(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    %% Thumb SUBS (register) encoding: 0001101mmmnnnddd
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    <<
        (16#1A00 bor ((RmNum band 7) bsl 6) bor ((RnNum band 7) bsl 3) bor (RdNum band 7)):16/little
    >>.

%% ARMv6-M Thumb address calculation (ADR) instruction
%% ADR is implemented as ADD Rd, PC, #imm8*4 in Thumb
%% In Thumb, PC = current_instruction_address + 4, so adr(Rd, N) means:
%% Rd = (current_pc + 4) + immediate = current_pc + (N - 4) + 4 = current_pc + N
-spec adr(arm_gpr_register(), integer()) -> binary().
adr(Rd, Offset) when
    is_atom(Rd),
    is_integer(Offset),
    Offset >= 4,
    Offset =< 1024,
    (Offset rem 4) =:= 0
->
    RdNum = reg_to_num(Rd),
    %% PC-relative offset in Thumb is (PC+4) + immediate
    %% So for adr(Rd, N): immediate = N - 4
    Immediate = Offset - 4,
    Imm8 = Immediate div 4,
    %% Thumb ADR (ADD PC-relative): 10100dddiiiiiiii
    <<(16#A000 bor (RdNum bsl 8) bor Imm8):16/little>>.

%% Emit a MULS instruction (Thumb encoding)
%% MULS Rd, Rm - multiply Rd by Rm, store result in Rd (sets flags)
-spec muls(arm_gpr_register(), arm_gpr_register()) -> binary().
muls(Rd, Rm) when is_atom(Rd), is_atom(Rm) ->
    %% Thumb MULS encoding: 0100001101mmmrrr (Rd is both source and destination)
    RdNum = reg_to_num(Rd),
    RmNum = reg_to_num(Rm),
    <<(16#4340 bor (RmNum bsl 3) bor RdNum):16/little>>.

%% ARMv6-M Thumb PUSH instruction
%% PUSH {register_list} - push registers to stack (low registers + optional LR)
-spec push([arm_gpr_register()]) -> binary().
push(RegList) when is_list(RegList) ->
    %% Process register list and build bitmask
    {LowRegMask, LRBit} = process_reglist(RegList, lr),
    %% Thumb PUSH encoding: 1011010Rlllllll where R=LR bit, lllllll=low register mask
    <<(16#B400 bor (LRBit bsl 8) bor LowRegMask):16/little>>.

%% ARMv6-M Thumb POP instruction
%% POP {register_list} - pop registers from stack (low registers + optional PC)
-spec pop([arm_gpr_register()]) -> binary().
pop(RegList) when is_list(RegList) ->
    %% Process register list and build bitmask
    {LowRegMask, PCBit} = process_reglist(RegList, pc),
    %% Thumb POP encoding: 1011110Plllllll where P=PC bit, lllllll=low register mask
    <<(16#BC00 bor (PCBit bsl 8) bor LowRegMask):16/little>>.

%% Generic helper function to process register lists for PUSH/POP
process_reglist(RegList, SpecialReg) ->
    RegBits = lists:foldl(
        fun(Reg, Acc) ->
            Acc + (1 bsl reg_to_num(Reg))
        end,
        0,
        RegList
    ),
    LowRegsBits = RegBits band 2#1111111,
    SpecialRegBit = RegBits band (1 bsl reg_to_num(SpecialReg)),
    if
        RegBits =/= LowRegsBits + SpecialRegBit ->
            error({invalid_register, RegBits - LowRegsBits - SpecialRegBit});
        SpecialRegBit =/= 0 ->
            {LowRegsBits, 1};
        true ->
            {LowRegsBits, 0}
    end.
