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

-module(jit_riscv32_asm).

-export([
    % R-type arithmetic and logical instructions
    add/3,
    sub/3,
    and_/3,
    or_/2,
    or_/3,
    xor_/3,
    sll/3,
    srl/3,
    sra/3,
    slt/3,
    sltu/3,
    % I-type immediate instructions
    addi/3,
    andi/3,
    ori/3,
    xori/3,
    slli/3,
    srli/3,
    srai/3,
    slti/3,
    sltiu/3,
    % Load instructions
    lw/2,
    lw/3,
    lh/2,
    lh/3,
    lhu/2,
    lhu/3,
    lb/2,
    lb/3,
    lbu/2,
    lbu/3,
    % Store instructions
    sw/2,
    sw/3,
    sh/2,
    sh/3,
    sb/2,
    sb/3,
    % Branch instructions
    beq/3,
    bne/3,
    blt/3,
    bge/3,
    bltu/3,
    bgeu/3,
    % Jump instructions
    jal/2,
    jalr/3,
    jalr/2,
    % Upper immediate instructions
    lui/2,
    auipc/2,
    % Pseudo-instructions
    nop/0,
    li/2,
    mv/2,
    not_/2,
    neg/2,
    j/1,
    jr/1,
    ret/0,
    call/2,
    % M extension (multiply/divide)
    mul/3,
    % System instructions
    bkpt/1,
    ebreak/0
]).

-export_type([
    riscv_register/0
]).

%% RISC-V 32-bit (RV32I) Assembler
%%
%% This module provides an assembler for the RISC-V 32-bit instruction set.
%% It generates binary machine code for RISC-V instructions following the
%% RV32I base integer instruction set architecture.
%%
%% RISC-V Register Set (32 registers):
%%   x0  (zero) - Hardwired zero (reads as 0, writes ignored)
%%   x1  (ra)   - Return address
%%   x2  (sp)   - Stack pointer
%%   x3  (gp)   - Global pointer
%%   x4  (tp)   - Thread pointer
%%   x5  (t0)   - Temporary register 0
%%   x6  (t1)   - Temporary register 1
%%   x7  (t2)   - Temporary register 2
%%   x8  (s0/fp)- Saved register 0 / Frame pointer
%%   x9  (s1)   - Saved register 1
%%   x10 (a0)   - Function argument 0 / Return value 0
%%   x11 (a1)   - Function argument 1 / Return value 1
%%   x12 (a2)   - Function argument 2
%%   x13 (a3)   - Function argument 3
%%   x14 (a4)   - Function argument 4
%%   x15 (a5)   - Function argument 5
%%   x16 (a6)   - Function argument 6
%%   x17 (a7)   - Function argument 7
%%   x18 (s2)   - Saved register 2
%%   x19 (s3)   - Saved register 3
%%   x20 (s4)   - Saved register 4
%%   x21 (s5)   - Saved register 5
%%   x22 (s6)   - Saved register 6
%%   x23 (s7)   - Saved register 7
%%   x24 (s8)   - Saved register 8
%%   x25 (s9)   - Saved register 9
%%   x26 (s10)  - Saved register 10
%%   x27 (s11)  - Saved register 11
%%   x28 (t3)   - Temporary register 3
%%   x29 (t4)   - Temporary register 4
%%   x30 (t5)   - Temporary register 5
%%   x31 (t6)   - Temporary register 6
%%
%% RISC-V Calling Convention (ILP32):
%%   - Arguments: a0-a7 (x10-x17)
%%   - Return values: a0-a1 (x10-x11)
%%   - Caller-saved: t0-t6, a0-a7
%%   - Callee-saved: s0-s11, sp, ra
%%   - Stack grows downward
%%   - Stack must be 16-byte aligned at function call boundaries
%%
%% Instruction Encoding:
%%   All RV32I instructions are 32 bits (4 bytes).
%%   Bit ordering is little-endian within each 32-bit word.
%%
%% See: RISC-V Instruction Set Manual, Volume I: User-Level ISA
%% https://riscv.org/technical/specifications/
%% https://github.com/riscv/riscv-isa-manual/

-type riscv_register() ::
    zero
    | ra
    | sp
    | gp
    | tp
    | t0
    | t1
    | t2
    | s0
    | fp
    | s1
    | a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | s2
    | s3
    | s4
    | s5
    | s6
    | s7
    | s8
    | s9
    | s10
    | s11
    | t3
    | t4
    | t5
    | t6.

%%-----------------------------------------------------------------------------
%% Helper functions
%%-----------------------------------------------------------------------------

%% Convert register atoms to register numbers (0-31)
-spec reg_to_num(riscv_register()) -> 0..31.
% ABI names
reg_to_num(zero) -> 0;
reg_to_num(ra) -> 1;
reg_to_num(sp) -> 2;
reg_to_num(gp) -> 3;
reg_to_num(tp) -> 4;
reg_to_num(t0) -> 5;
reg_to_num(t1) -> 6;
reg_to_num(t2) -> 7;
reg_to_num(s0) -> 8;
reg_to_num(fp) -> 8;
reg_to_num(s1) -> 9;
reg_to_num(a0) -> 10;
reg_to_num(a1) -> 11;
reg_to_num(a2) -> 12;
reg_to_num(a3) -> 13;
reg_to_num(a4) -> 14;
reg_to_num(a5) -> 15;
reg_to_num(a6) -> 16;
reg_to_num(a7) -> 17;
reg_to_num(s2) -> 18;
reg_to_num(s3) -> 19;
reg_to_num(s4) -> 20;
reg_to_num(s5) -> 21;
reg_to_num(s6) -> 22;
reg_to_num(s7) -> 23;
reg_to_num(s8) -> 24;
reg_to_num(s9) -> 25;
reg_to_num(s10) -> 26;
reg_to_num(s11) -> 27;
reg_to_num(t3) -> 28;
reg_to_num(t4) -> 29;
reg_to_num(t5) -> 30;
reg_to_num(t6) -> 31.

%%-----------------------------------------------------------------------------
%% R-type instruction encoding
%%-----------------------------------------------------------------------------

%% R-type instruction format:
%% funct7 (7) | rs2 (5) | rs1 (5) | funct3 (3) | rd (5) | opcode (7)
%% Bits:  31-25     24-20     19-15     14-12      11-7      6-0

-spec encode_r_type(
    Opcode :: 0..127,
    Rd :: riscv_register(),
    Funct3 :: 0..7,
    Rs1 :: riscv_register(),
    Rs2 :: riscv_register(),
    Funct7 :: 0..127
) -> binary().
encode_r_type(Opcode, Rd, Funct3, Rs1, Rs2, Funct7) ->
    RdNum = reg_to_num(Rd),
    Rs1Num = reg_to_num(Rs1),
    Rs2Num = reg_to_num(Rs2),
    Instr =
        (Funct7 bsl 25) bor
            (Rs2Num bsl 20) bor
            (Rs1Num bsl 15) bor
            (Funct3 bsl 12) bor
            (RdNum bsl 7) bor
            Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% R-type arithmetic and logical instructions
%%-----------------------------------------------------------------------------

%% ADD - Add
%% rd = rs1 + rs2
-spec add(riscv_register(), riscv_register(), riscv_register()) -> binary().
add(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#00).

%% SUB - Subtract
%% rd = rs1 - rs2
-spec sub(riscv_register(), riscv_register(), riscv_register()) -> binary().
sub(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0100000
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#20).

%% AND - Bitwise AND
%% rd = rs1 & rs2
-spec and_(riscv_register(), riscv_register(), riscv_register()) -> binary().
and_(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 111, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#7, Rs1, Rs2, 16#00).

%% OR - Bitwise OR
%% rd = rs1 | rs2
-spec or_(riscv_register(), riscv_register(), riscv_register()) -> binary().
or_(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 110, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#6, Rs1, Rs2, 16#00).

%% OR - Bitwise OR (in-place)
%% rd = rd | rs
-spec or_(riscv_register(), riscv_register()) -> binary().
or_(Rd, Rs) ->
    or_(Rd, Rd, Rs).

%% XOR - Bitwise XOR
%% rd = rs1 ^ rs2
-spec xor_(riscv_register(), riscv_register(), riscv_register()) -> binary().
xor_(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 100, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#4, Rs1, Rs2, 16#00).

%% SLL - Shift Left Logical
%% rd = rs1 << rs2[4:0]
-spec sll(riscv_register(), riscv_register(), riscv_register()) -> binary().
sll(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 001, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#1, Rs1, Rs2, 16#00).

%% SRL - Shift Right Logical
%% rd = rs1 >> rs2[4:0] (zero-extend)
-spec srl(riscv_register(), riscv_register(), riscv_register()) -> binary().
srl(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 101, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#5, Rs1, Rs2, 16#00).

%% SRA - Shift Right Arithmetic
%% rd = rs1 >> rs2[4:0] (sign-extend)
-spec sra(riscv_register(), riscv_register(), riscv_register()) -> binary().
sra(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 101, Funct7: 0100000
    encode_r_type(16#33, Rd, 16#5, Rs1, Rs2, 16#20).

%% SLT - Set Less Than
%% rd = (rs1 < rs2) ? 1 : 0 (signed)
-spec slt(riscv_register(), riscv_register(), riscv_register()) -> binary().
slt(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 010, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#2, Rs1, Rs2, 16#00).

%% SLTU - Set Less Than Unsigned
%% rd = (rs1 < rs2) ? 1 : 0 (unsigned)
-spec sltu(riscv_register(), riscv_register(), riscv_register()) -> binary().
sltu(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 011, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#3, Rs1, Rs2, 16#00).

%%-----------------------------------------------------------------------------
%% I-type instruction encoding
%%-----------------------------------------------------------------------------

%% I-type instruction format:
%% imm[11:0] (12) | rs1 (5) | funct3 (3) | rd (5) | opcode (7)
%% Bits:  31-20       19-15     14-12      11-7      6-0

-spec encode_i_type(
    Opcode :: 0..127,
    Rd :: riscv_register(),
    Funct3 :: 0..7,
    Rs1 :: riscv_register(),
    Imm :: integer()
) -> binary().
encode_i_type(Opcode, Rd, Funct3, Rs1, Imm) ->
    RdNum = reg_to_num(Rd),
    Rs1Num = reg_to_num(Rs1),
    % Sign-extend and mask to 12 bits
    ImmMasked = Imm band 16#FFF,
    Instr =
        (ImmMasked bsl 20) bor
            (Rs1Num bsl 15) bor
            (Funct3 bsl 12) bor
            (RdNum bsl 7) bor
            Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% I-type immediate arithmetic and logical instructions
%%-----------------------------------------------------------------------------

%% ADDI - Add Immediate
%% rd = rs1 + imm
-spec addi(riscv_register(), riscv_register(), integer()) -> binary().
addi(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 000
    encode_i_type(16#13, Rd, 16#0, Rs1, Imm);
addi(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% ANDI - AND Immediate
%% rd = rs1 & imm
-spec andi(riscv_register(), riscv_register(), integer()) -> binary().
andi(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 111
    encode_i_type(16#13, Rd, 16#7, Rs1, Imm);
andi(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% ORI - OR Immediate
%% rd = rs1 | imm
-spec ori(riscv_register(), riscv_register(), integer()) -> binary().
ori(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 110
    encode_i_type(16#13, Rd, 16#6, Rs1, Imm);
ori(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% XORI - XOR Immediate
%% rd = rs1 ^ imm
-spec xori(riscv_register(), riscv_register(), integer()) -> binary().
xori(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 100
    encode_i_type(16#13, Rd, 16#4, Rs1, Imm);
xori(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% SLTI - Set Less Than Immediate
%% rd = (rs1 < imm) ? 1 : 0 (signed)
-spec slti(riscv_register(), riscv_register(), integer()) -> binary().
slti(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 010
    encode_i_type(16#13, Rd, 16#2, Rs1, Imm);
slti(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% SLTIU - Set Less Than Immediate Unsigned
%% rd = (rs1 < imm) ? 1 : 0 (unsigned)
-spec sltiu(riscv_register(), riscv_register(), integer()) -> binary().
sltiu(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 011
    encode_i_type(16#13, Rd, 16#3, Rs1, Imm);
sltiu(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% I-type immediate shift instructions
%%-----------------------------------------------------------------------------

%% SLLI - Shift Left Logical Immediate
%% rd = rs1 << shamt
-spec slli(riscv_register(), riscv_register(), 0..31) -> binary().
slli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 31 ->
    % Opcode: 0010011 (0x13), Funct3: 001, Imm[11:5] = 0000000
    encode_i_type(16#13, Rd, 16#1, Rs1, Shamt);
slli(_Rd, _Rs1, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 31}).

%% SRLI - Shift Right Logical Immediate
%% rd = rs1 >> shamt (zero-extend)
-spec srli(riscv_register(), riscv_register(), 0..31) -> binary().
srli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 31 ->
    % Opcode: 0010011 (0x13), Funct3: 101, Imm[11:5] = 0000000
    encode_i_type(16#13, Rd, 16#5, Rs1, Shamt);
srli(_Rd, _Rs1, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 31}).

%% SRAI - Shift Right Arithmetic Immediate
%% rd = rs1 >> shamt (sign-extend)
-spec srai(riscv_register(), riscv_register(), 0..31) -> binary().
srai(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 31 ->
    % Opcode: 0010011 (0x13), Funct3: 101, Imm[11:5] = 0100000
    % The encoding uses bit 30 (Imm[10]) to distinguish SRAI from SRLI
    ImmWithBit30 = Shamt bor (1 bsl 10),
    encode_i_type(16#13, Rd, 16#5, Rs1, ImmWithBit30);
srai(_Rd, _Rs1, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 31}).

%%-----------------------------------------------------------------------------
%% Load instructions (I-type)
%%-----------------------------------------------------------------------------

%% LW - Load Word
%% rd = mem[rs1 + offset] (32-bit)
-spec lw({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lw(Rd, {Rs1, Offset}) ->
    lw(Rd, Rs1, Offset);
lw(Rd, Rs1) when is_atom(Rs1) ->
    lw(Rd, Rs1, 0).

-spec lw(riscv_register(), riscv_register(), integer()) -> binary().
lw(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0000011 (0x03), Funct3: 010
    encode_i_type(16#03, Rd, 16#2, Rs1, Offset);
lw(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% LH - Load Halfword (sign-extended)
%% rd = sign_extend(mem[rs1 + offset][15:0])
-spec lh({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lh(Rd, {Rs1, Offset}) ->
    lh(Rd, Rs1, Offset);
lh(Rd, Rs1) when is_atom(Rs1) ->
    lh(Rd, Rs1, 0).

-spec lh(riscv_register(), riscv_register(), integer()) -> binary().
lh(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0000011 (0x03), Funct3: 001
    encode_i_type(16#03, Rd, 16#1, Rs1, Offset);
lh(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% LHU - Load Halfword Unsigned (zero-extended)
%% rd = zero_extend(mem[rs1 + offset][15:0])
-spec lhu({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lhu(Rd, {Rs1, Offset}) ->
    lhu(Rd, Rs1, Offset);
lhu(Rd, Rs1) when is_atom(Rs1) ->
    lhu(Rd, Rs1, 0).

-spec lhu(riscv_register(), riscv_register(), integer()) -> binary().
lhu(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0000011 (0x03), Funct3: 101
    encode_i_type(16#03, Rd, 16#5, Rs1, Offset);
lhu(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% LB - Load Byte (sign-extended)
%% rd = sign_extend(mem[rs1 + offset][7:0])
-spec lb({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lb(Rd, {Rs1, Offset}) ->
    lb(Rd, Rs1, Offset);
lb(Rd, Rs1) when is_atom(Rs1) ->
    lb(Rd, Rs1, 0).

-spec lb(riscv_register(), riscv_register(), integer()) -> binary().
lb(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0000011 (0x03), Funct3: 000
    encode_i_type(16#03, Rd, 16#0, Rs1, Offset);
lb(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% LBU - Load Byte Unsigned (zero-extended)
%% rd = zero_extend(mem[rs1 + offset][7:0])
-spec lbu({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lbu(Rd, {Rs1, Offset}) ->
    lbu(Rd, Rs1, Offset);
lbu(Rd, Rs1) when is_atom(Rs1) ->
    lbu(Rd, Rs1, 0).

-spec lbu(riscv_register(), riscv_register(), integer()) -> binary().
lbu(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0000011 (0x03), Funct3: 100
    encode_i_type(16#03, Rd, 16#4, Rs1, Offset);
lbu(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% S-type instruction encoding (for stores)
%%-----------------------------------------------------------------------------

%% S-type instruction format:
%% imm[11:5] (7) | rs2 (5) | rs1 (5) | funct3 (3) | imm[4:0] (5) | opcode (7)
%% Bits:  31-25      24-20     19-15     14-12      11-7          6-0

-spec encode_s_type(
    Opcode :: 0..127,
    Funct3 :: 0..7,
    Rs1 :: riscv_register(),
    Rs2 :: riscv_register(),
    Imm :: integer()
) -> binary().
encode_s_type(Opcode, Funct3, Rs1, Rs2, Imm) ->
    Rs1Num = reg_to_num(Rs1),
    Rs2Num = reg_to_num(Rs2),
    % Split immediate: imm[11:5] goes to bits 31-25, imm[4:0] goes to bits 11-7
    ImmMasked = Imm band 16#FFF,
    Imm11_5 = (ImmMasked bsr 5) band 16#7F,
    Imm4_0 = ImmMasked band 16#1F,
    Instr =
        (Imm11_5 bsl 25) bor
            (Rs2Num bsl 20) bor
            (Rs1Num bsl 15) bor
            (Funct3 bsl 12) bor
            (Imm4_0 bsl 7) bor
            Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Store instructions (S-type)
%%-----------------------------------------------------------------------------

%% SW - Store Word
%% mem[rs1 + offset] = rs2[31:0]
-spec sw({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
sw(Rs2, {Rs1, Offset}) ->
    sw(Rs1, Rs2, Offset);
sw(Rs2, Rs1) when is_atom(Rs1) ->
    sw(Rs1, Rs2, 0).

-spec sw(riscv_register(), riscv_register(), integer()) -> binary().
sw(Rs1, Rs2, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0100011 (0x23), Funct3: 010
    encode_s_type(16#23, 16#2, Rs1, Rs2, Offset);
sw(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% SH - Store Halfword
%% mem[rs1 + offset][15:0] = rs2[15:0]
-spec sh({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
sh(Rs2, {Rs1, Offset}) ->
    sh(Rs1, Rs2, Offset);
sh(Rs2, Rs1) when is_atom(Rs1) ->
    sh(Rs1, Rs2, 0).

-spec sh(riscv_register(), riscv_register(), integer()) -> binary().
sh(Rs1, Rs2, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0100011 (0x23), Funct3: 001
    encode_s_type(16#23, 16#1, Rs1, Rs2, Offset);
sh(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% SB - Store Byte
%% mem[rs1 + offset][7:0] = rs2[7:0]
-spec sb({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
sb(Rs2, {Rs1, Offset}) ->
    sb(Rs1, Rs2, Offset);
sb(Rs2, Rs1) when is_atom(Rs1) ->
    sb(Rs1, Rs2, 0).

-spec sb(riscv_register(), riscv_register(), integer()) -> binary().
sb(Rs1, Rs2, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 0100011 (0x23), Funct3: 000
    encode_s_type(16#23, 16#0, Rs1, Rs2, Offset);
sb(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% B-type instruction encoding (for branches)
%%-----------------------------------------------------------------------------

%% B-type instruction format:
%% imm[12|10:5] (7) | rs2 (5) | rs1 (5) | funct3 (3) | imm[4:1|11] (5) | opcode (7)
%% Bits:  31-25         24-20     19-15     14-12      11-7              6-0
%%
%% The immediate is split across the instruction and represents a signed offset
%% in multiples of 2 bytes (must be 2-byte aligned).
%% Range: ±4 KiB (±4096 bytes)

-spec encode_b_type(
    Opcode :: 0..127,
    Funct3 :: 0..7,
    Rs1 :: riscv_register(),
    Rs2 :: riscv_register(),
    Offset :: integer()
) -> binary().
encode_b_type(Opcode, Funct3, Rs1, Rs2, Offset) ->
    Rs1Num = reg_to_num(Rs1),
    Rs2Num = reg_to_num(Rs2),
    % Offset must be 2-byte aligned and in range [-4096, 4094]
    % Extract bits: imm[12], imm[10:5], imm[4:1], imm[11]
    OffsetMasked = Offset band 16#1FFF,
    % imm[12] -> bit 31
    Imm12 = (OffsetMasked bsr 12) band 1,
    % imm[10:5] -> bits 30-25
    Imm10_5 = (OffsetMasked bsr 5) band 16#3F,
    % imm[4:1] -> bits 11-8
    Imm4_1 = (OffsetMasked bsr 1) band 16#F,
    % imm[11] -> bit 7
    Imm11 = (OffsetMasked bsr 11) band 1,
    Instr =
        (Imm12 bsl 31) bor
            (Imm10_5 bsl 25) bor
            (Rs2Num bsl 20) bor
            (Rs1Num bsl 15) bor
            (Funct3 bsl 12) bor
            (Imm4_1 bsl 8) bor
            (Imm11 bsl 7) bor
            Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Branch instructions (B-type)
%%-----------------------------------------------------------------------------

%% BEQ - Branch if Equal
%% if (rs1 == rs2) pc += offset
-spec beq(riscv_register(), riscv_register(), integer()) -> binary().
beq(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 000
    encode_b_type(16#63, 16#0, Rs1, Rs2, Offset);
beq(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
beq(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%% BNE - Branch if Not Equal
%% if (rs1 != rs2) pc += offset
-spec bne(riscv_register(), riscv_register(), integer()) -> binary().
bne(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 001
    encode_b_type(16#63, 16#1, Rs1, Rs2, Offset);
bne(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
bne(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%% BLT - Branch if Less Than (signed)
%% if (rs1 < rs2) pc += offset
-spec blt(riscv_register(), riscv_register(), integer()) -> binary().
blt(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 100
    encode_b_type(16#63, 16#4, Rs1, Rs2, Offset);
blt(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
blt(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%% BGE - Branch if Greater or Equal (signed)
%% if (rs1 >= rs2) pc += offset
-spec bge(riscv_register(), riscv_register(), integer()) -> binary().
bge(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 101
    encode_b_type(16#63, 16#5, Rs1, Rs2, Offset);
bge(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
bge(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%% BLTU - Branch if Less Than Unsigned
%% if (rs1 < rs2) pc += offset (unsigned)
-spec bltu(riscv_register(), riscv_register(), integer()) -> binary().
bltu(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 110
    encode_b_type(16#63, 16#6, Rs1, Rs2, Offset);
bltu(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
bltu(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%% BGEU - Branch if Greater or Equal Unsigned
%% if (rs1 >= rs2) pc += offset (unsigned)
-spec bgeu(riscv_register(), riscv_register(), integer()) -> binary().
bgeu(Rs1, Rs2, Offset) when
    Offset >= -4096, Offset =< 4094, (Offset rem 2) =:= 0
->
    % Opcode: 1100011 (0x63), Funct3: 111
    encode_b_type(16#63, 16#7, Rs1, Rs2, Offset);
bgeu(_Rs1, _Rs2, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
bgeu(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -4096, 4094}).

%%-----------------------------------------------------------------------------
%% J-type instruction encoding (for JAL)
%%-----------------------------------------------------------------------------

%% J-type instruction format (JAL):
%% imm[20|10:1|11|19:12] (20) | rd (5) | opcode (7)
%% Bits:  31-12                  11-7     6-0
%%
%% The immediate represents a signed offset in multiples of 2 bytes.
%% Range: ±1 MiB (±1048576 bytes)

-spec encode_j_type(
    Opcode :: 0..127, Rd :: riscv_register(), Offset :: integer()
) -> binary().
encode_j_type(Opcode, Rd, Offset) ->
    RdNum = reg_to_num(Rd),
    % Extract immediate bits: imm[20], imm[10:1], imm[11], imm[19:12]
    OffsetMasked = Offset band 16#1FFFFF,
    % imm[20] -> bit 31
    Imm20 = (OffsetMasked bsr 20) band 1,
    % imm[10:1] -> bits 30-21
    Imm10_1 = (OffsetMasked bsr 1) band 16#3FF,
    % imm[11] -> bit 20
    Imm11 = (OffsetMasked bsr 11) band 1,
    % imm[19:12] -> bits 19-12
    Imm19_12 = (OffsetMasked bsr 12) band 16#FF,
    Instr =
        (Imm20 bsl 31) bor
            (Imm10_1 bsl 21) bor
            (Imm11 bsl 20) bor
            (Imm19_12 bsl 12) bor
            (RdNum bsl 7) bor
            Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% U-type instruction encoding (for LUI, AUIPC)
%%-----------------------------------------------------------------------------

%% U-type instruction format:
%% imm[31:12] (20) | rd (5) | opcode (7)
%% Bits:  31-12        11-7     6-0

-spec encode_u_type(
    Opcode :: 0..127, Rd :: riscv_register(), Imm :: integer()
) -> binary().
encode_u_type(Opcode, Rd, Imm) ->
    RdNum = reg_to_num(Rd),
    % Upper 20 bits of immediate
    ImmUpper = (Imm bsr 12) band 16#FFFFF,
    Instr = (ImmUpper bsl 12) bor (RdNum bsl 7) bor Opcode,
    <<Instr:32/little>>.

%%-----------------------------------------------------------------------------
%% Jump and link instructions
%%-----------------------------------------------------------------------------

%% JAL - Jump and Link
%% rd = pc + 4; pc += offset
-spec jal(riscv_register(), integer()) -> binary().
jal(Rd, Offset) when
    Offset >= -1048576, Offset =< 1048574, (Offset rem 2) =:= 0
->
    % Opcode: 1101111 (0x6F)
    encode_j_type(16#6F, Rd, Offset);
jal(_Rd, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
jal(_Rd, Offset) ->
    error({offset_out_of_range, Offset, -1048576, 1048574}).

%% JALR - Jump and Link Register
%% rd = pc + 4; pc = (rs1 + offset) & ~1
-spec jalr(riscv_register(), riscv_register(), integer()) -> binary().
jalr(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    % Opcode: 1100111 (0x67), Funct3: 000
    encode_i_type(16#67, Rd, 16#0, Rs1, Offset);
jalr(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%% JALR - Jump and Link Register (no offset)
%% rd = pc + 4; pc = rs1 & ~1
-spec jalr(riscv_register(), riscv_register()) -> binary().
jalr(Rd, Rs1) ->
    jalr(Rd, Rs1, 0).

%%-----------------------------------------------------------------------------
%% Upper immediate instructions
%%-----------------------------------------------------------------------------

%% LUI - Load Upper Immediate
%% rd = imm << 12
-spec lui(riscv_register(), integer()) -> binary().
lui(Rd, Imm) when Imm >= -16#80000, Imm =< 16#7FFFF ->
    % Opcode: 0110111 (0x37)
    encode_u_type(16#37, Rd, Imm bsl 12);
lui(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -16#80000, 16#7FFFF}).

%% AUIPC - Add Upper Immediate to PC
%% rd = pc + (imm << 12)
-spec auipc(riscv_register(), integer()) -> binary().
auipc(Rd, Imm) when Imm >= -16#80000, Imm =< 16#7FFFF ->
    % Opcode: 0010111 (0x17)
    encode_u_type(16#17, Rd, Imm bsl 12);
auipc(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -16#80000, 16#7FFFF}).

%%-----------------------------------------------------------------------------
%% Pseudo-instructions
%%-----------------------------------------------------------------------------
%% These are convenience instructions that map to actual RV32I instructions

%% NOP - No Operation
%% Expands to: addi x0, x0, 0
-spec nop() -> binary().
nop() ->
    addi(zero, zero, 0).

%% LI - Load Immediate
%% Load a 32-bit immediate value into a register
%% For small immediates (-2048 to 2047): addi rd, x0, imm
%% For larger immediates: lui + addi sequence
-spec li(riscv_register(), integer()) -> binary().
li(Rd, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Small immediate: addi rd, x0, imm
    addi(Rd, zero, Imm);
li(Rd, Imm) when Imm >= -16#80000000, Imm =< 16#7FFFFFFF ->
    % Large immediate: lui + addi
    % Split into upper 20 bits and lower 12 bits
    % Need to account for sign extension of lower 12 bits
    Lower = Imm band 16#FFF,
    % If lower 12 bits has sign bit set, we need to add 1 to upper
    UpperRaw =
        if
            Lower >= 16#800 ->
                (Imm bsr 12) + 1;
            true ->
                Imm bsr 12
        end,
    % Mask to 20 bits first, then sign extend if needed
    UpperMasked = UpperRaw band 16#FFFFF,
    Upper =
        if
            UpperMasked band 16#80000 =/= 0 ->
                % Bit 19 is set, so this is negative in 20-bit representation
                % Sign extend from 20 bits
                UpperMasked - 16#100000;
            true ->
                % Positive value
                UpperMasked
        end,
    % Sign extend lower 12 bits
    LowerSigned =
        if
            Lower >= 16#800 -> Lower - 16#1000;
            true -> Lower
        end,
    LuiInstr = lui(Rd, Upper),
    AddiInstr = addi(Rd, Rd, LowerSigned),
    <<LuiInstr/binary, AddiInstr/binary>>;
li(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -16#80000000, 16#7FFFFFFF}).

%% MV - Move (copy register)
%% Expands to: addi rd, rs, 0
-spec mv(riscv_register(), riscv_register()) -> binary().
mv(Rd, Rs) ->
    addi(Rd, Rs, 0).

%% NOT - Bitwise NOT
%% Expands to: xori rd, rs, -1
-spec not_(riscv_register(), riscv_register()) -> binary().
not_(Rd, Rs) ->
    xori(Rd, Rs, -1).

%% NEG - Negate (two's complement)
%% Expands to: sub rd, x0, rs
-spec neg(riscv_register(), riscv_register()) -> binary().
neg(Rd, Rs) ->
    sub(Rd, zero, Rs).

%% J - Unconditional Jump
%% Expands to: jal x0, offset
-spec j(integer()) -> binary().
j(Offset) ->
    jal(zero, Offset).

%% JR - Jump Register
%% Expands to: jalr x0, rs, 0
-spec jr(riscv_register()) -> binary().
jr(Rs) ->
    jalr(zero, Rs, 0).

%% RET - Return from subroutine
%% Expands to: jalr x0, ra, 0
-spec ret() -> binary().
ret() ->
    jalr(zero, ra, 0).

%% CALL - Call function (far call using AUIPC + JALR)
%% This is a two-instruction sequence for calling functions beyond JAL range
%% Expands to: auipc ra, offset[31:12]; jalr ra, ra, offset[11:0]
-spec call(riscv_register(), integer()) -> binary().
call(Rd, Offset) when Offset >= -16#80000000, Offset =< 16#7FFFFFFF ->
    % Split offset into upper 20 bits and lower 12 bits
    Lower = Offset band 16#FFF,
    % If lower 12 bits has sign bit set, we need to add 1 to upper
    Upper =
        if
            Lower >= 16#800 ->
                ((Offset bsr 12) + 1) band 16#FFFFF;
            true ->
                (Offset bsr 12) band 16#FFFFF
        end,
    % Sign extend lower 12 bits
    LowerSigned =
        if
            Lower >= 16#800 -> Lower - 16#1000;
            true -> Lower
        end,
    AuipcInstr = auipc(Rd, Upper),
    JalrInstr = jalr(ra, Rd, LowerSigned),
    <<AuipcInstr/binary, JalrInstr/binary>>;
call(_Rd, Offset) ->
    error({offset_out_of_range, Offset, -16#80000000, 16#7FFFFFFF}).

%% EBREAK - Environment Breakpoint
%% Causes a breakpoint exception to be raised.
%% This is the RISC-V equivalent of ARM's BKPT instruction.
%% Encoding: 0x00100073
-spec ebreak() -> binary().
ebreak() ->
    <<16#73, 16#00, 16#10, 16#00>>.

%% BKPT - Breakpoint (for ARM compatibility)
%% In RISC-V, this is implemented as EBREAK.
%% The immediate parameter is ignored for compatibility with ARM.
-spec bkpt(integer()) -> binary().
bkpt(_Imm) ->
    ebreak().

%% MUL - Multiply (RV32M extension)
%% Multiplies rs1 by rs2 and places the lower 32 bits in rd
%% Format: mul rd, rs1, rs2
%% Encoding: R-type with opcode=0x33, funct3=0x0, funct7=0x01
-spec mul(riscv_register(), riscv_register(), riscv_register()) -> binary().
mul(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0000001
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#01).
