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
    % C extension (compressed) - arithmetic/logical
    c_add/2,
    c_sub/2,
    c_and/2,
    c_or/2,
    c_xor/2,
    c_mv/2,
    % C extension - immediate instructions
    c_addi/2,
    c_andi/2,
    c_li/2,
    c_lui/2,
    c_addi16sp/1,
    c_addi4spn/2,
    % C extension - shift instructions
    c_slli/2,
    c_srli/2,
    c_srai/2,
    % C extension - load/store
    c_lw/2,
    c_sw/2,
    c_lwsp/2,
    c_swsp/2,
    % C extension - branches and jumps
    c_beqz/2,
    c_bnez/2,
    c_j/1,
    c_jal/1,
    c_jr/1,
    c_jalr/1,
    % C extension - system instructions
    c_ebreak/0,
    % C extension - pseudo-instructions
    c_nop/0
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
add(Rd, Rs1, Rs2) when Rd =:= Rs1, Rd =/= zero, Rs2 =/= zero ->
    % Use c.add when rd == rs1 and neither register is zero
    c_add(Rd, Rs2);
add(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#00).

%% SUB - Subtract
%% rd = rs1 - rs2
-spec sub(riscv_register(), riscv_register(), riscv_register()) -> binary().
sub(Rd, Rs1, Rs2) when Rd =:= Rs1 ->
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs2) of
        true -> c_sub(Rd, Rs2);
        false -> encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#20)
    end;
sub(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0100000
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#20).

%% AND - Bitwise AND
%% rd = rs1 & rs2
-spec and_(riscv_register(), riscv_register(), riscv_register()) -> binary().
and_(Rd, Rs1, Rs2) when Rd =:= Rs1 ->
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs2) of
        true -> c_and(Rd, Rs2);
        false -> encode_r_type(16#33, Rd, 16#7, Rs1, Rs2, 16#00)
    end;
and_(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 111, Funct7: 0000000
    encode_r_type(16#33, Rd, 16#7, Rs1, Rs2, 16#00).

%% OR - Bitwise OR
%% rd = rs1 | rs2
-spec or_(riscv_register(), riscv_register(), riscv_register()) -> binary().
or_(Rd, Rs1, Rs2) when Rd =:= Rs1 ->
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs2) of
        true -> c_or(Rd, Rs2);
        false -> encode_r_type(16#33, Rd, 16#6, Rs1, Rs2, 16#00)
    end;
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
xor_(Rd, Rs1, Rs2) when Rd =:= Rs1 ->
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs2) of
        true -> c_xor(Rd, Rs2);
        false -> encode_r_type(16#33, Rd, 16#4, Rs1, Rs2, 16#00)
    end;
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
addi(Rd, Rs1, Imm) when Rd =:= Rs1, Rd =/= zero, Imm >= -32, Imm =< 31 ->
    % Use c.addi when rd == rs1, rd != zero, and imm fits in 6 bits (signed)
    c_addi(Rd, Imm);
addi(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Opcode: 0010011 (0x13), Funct3: 000
    encode_i_type(16#13, Rd, 16#0, Rs1, Imm);
addi(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).

%% ANDI - AND Immediate
%% rd = rs1 & imm
-spec andi(riscv_register(), riscv_register(), integer()) -> binary().
andi(Rd, Rs1, Imm) when Rd =:= Rs1, Imm >= -32, Imm =< 31 ->
    case is_compressed_reg(Rd) of
        true -> c_andi(Rd, Imm);
        false -> encode_i_type(16#13, Rd, 16#7, Rs1, Imm)
    end;
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
slli(Rd, Rs1, Shamt) when Rd =:= Rs1, Rd =/= zero, Shamt >= 1, Shamt =< 31 ->
    % Use c.slli when rd == rs1, rd != zero, and shamt != 0 (c.slli with shamt=0 is reserved)
    c_slli(Rd, Shamt);
slli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 31 ->
    % Opcode: 0010011 (0x13), Funct3: 001, Imm[11:5] = 0000000
    encode_i_type(16#13, Rd, 16#1, Rs1, Shamt);
slli(_Rd, _Rs1, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 31}).

%% SRLI - Shift Right Logical Immediate
%% rd = rs1 >> shamt (zero-extend)
-spec srli(riscv_register(), riscv_register(), 0..31) -> binary().
srli(Rd, Rs1, Shamt) when Rd =:= Rs1, Shamt >= 0, Shamt =< 31 ->
    case is_compressed_reg(Rd) of
        true -> c_srli(Rd, Shamt);
        false -> encode_i_type(16#13, Rd, 16#5, Rs1, Shamt)
    end;
srli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 31 ->
    % Opcode: 0010011 (0x13), Funct3: 101, Imm[11:5] = 0000000
    encode_i_type(16#13, Rd, 16#5, Rs1, Shamt);
srli(_Rd, _Rs1, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 31}).

%% SRAI - Shift Right Arithmetic Immediate
%% rd = rs1 >> shamt (sign-extend)
-spec srai(riscv_register(), riscv_register(), 0..31) -> binary().
srai(Rd, Rs1, Shamt) when Rd =:= Rs1, Shamt >= 0, Shamt =< 31 ->
    case is_compressed_reg(Rd) of
        true ->
            c_srai(Rd, Shamt);
        false ->
            ImmWithBit30 = Shamt bor (1 bsl 10),
            encode_i_type(16#13, Rd, 16#5, Rs1, ImmWithBit30)
    end;
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
lw(Rd, sp, Offset) when Rd =/= zero, Offset >= 0, Offset =< 252, Offset rem 4 =:= 0 ->
    % Use c.lwsp for loads from sp with aligned offset in range
    c_lwsp(Rd, Offset);
lw(Rd, Rs1, Offset) when Offset >= 0, Offset =< 124, Offset rem 4 =:= 0 ->
    % Use c.lw when both registers are in compressed set and offset is aligned
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs1) of
        true -> c_lw(Rd, {Rs1, Offset});
        false -> encode_i_type(16#03, Rd, 16#2, Rs1, Offset)
    end;
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
sw(sp, Rs2, Offset) when Offset >= 0, Offset =< 252, Offset rem 4 =:= 0 ->
    % Use c.swsp for stores to sp with aligned offset in range
    c_swsp(Rs2, Offset);
sw(Rs1, Rs2, Offset) when Offset >= 0, Offset =< 124, Offset rem 4 =:= 0 ->
    % Use c.sw when both registers are in compressed set and offset is aligned
    case is_compressed_reg(Rs1) andalso is_compressed_reg(Rs2) of
        true -> c_sw(Rs2, {Rs1, Offset});
        false -> encode_s_type(16#23, 16#2, Rs1, Rs2, Offset)
    end;
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
beq(Rs1, zero, Offset) when Offset >= -256, Offset =< 254, (Offset rem 2) =:= 0 ->
    % Use c.beqz when comparing with zero and offset fits
    case is_compressed_reg(Rs1) of
        true -> c_beqz(Rs1, Offset);
        false -> encode_b_type(16#63, 16#0, Rs1, zero, Offset)
    end;
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
bne(Rs1, zero, Offset) when Offset >= -256, Offset =< 254, (Offset rem 2) =:= 0 ->
    % Use c.bnez when comparing with zero and offset fits
    case is_compressed_reg(Rs1) of
        true -> c_bnez(Rs1, Offset);
        false -> encode_b_type(16#63, 16#1, Rs1, zero, Offset)
    end;
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
jal(zero, Offset) when Offset >= -2048, Offset =< 2046, (Offset rem 2) =:= 0 ->
    % Use c.j when rd is zero (no link) and offset fits
    c_j(Offset);
jal(ra, Offset) when Offset >= -2048, Offset =< 2046, (Offset rem 2) =:= 0 ->
    % Use c.jal when rd is ra and offset fits (RV32C only)
    c_jal(Offset);
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
jalr(zero, Rs1, 0) when Rs1 =/= zero ->
    % Use c.jr for jump to register without link (rd=zero, offset=0)
    c_jr(Rs1);
jalr(ra, Rs1, 0) when Rs1 =/= zero ->
    % Use c.jalr for jump to register with link (rd=ra, offset=0)
    c_jalr(Rs1);
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
lui(Rd, Imm) when Rd =/= zero, Imm >= -32, Imm =< 31, Imm =/= 0 ->
    % Use c.lui when rd != zero and imm fits in 6 bits (signed) and imm != 0
    c_lui(Rd, Imm);
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
li(Rd, Imm) when Rd =/= zero, Imm >= -32, Imm =< 31 ->
    % Use c.li when rd != zero and imm fits in 6 bits (signed)
    c_li(Rd, Imm);
li(Rd, Imm) when Imm >= -2048, Imm =< 2047 ->
    % Small immediate: addi rd, x0, imm
    addi(Rd, zero, Imm);
% Handle unsigned values that represent small signed values (e.g., 0xFFFFFFFF = -1)
li(Rd, Imm) when Imm > 16#7FFFFFFF, Imm - 16#100000000 >= -2048 ->
    % This unsigned value fits in 12-bit signed range when normalized
    addi(Rd, zero, Imm - 16#100000000);
li(Rd, Imm) when Imm >= -16#80000000, Imm =< 16#FFFFFFFF ->
    % Large immediate: lui or lui + addi
    % Split into upper 20 bits and lower 12 bits
    % Need to account for sign extension of lower 12 bits
    % Work with unsigned values to avoid issues with arithmetic right shift
    UnsignedImm =
        if
            Imm < 0 -> Imm + 16#100000000;
            true -> Imm
        end,
    Lower = UnsignedImm band 16#FFF,
    % If lower 12 bits has sign bit set, we need to add 1 to upper
    % because addi will sign-extend the immediate
    UpperRaw =
        if
            Lower >= 16#800 ->
                (UnsignedImm bsr 12) + 1;
            true ->
                UnsignedImm bsr 12
        end,
    % Mask to 20 bits first, then sign extend if needed for lui instruction
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
    % Only emit addi if lower bits are non-zero
    LuiInstr = lui(Rd, Upper),
    if
        Lower =:= 0 ->
            % Just lui is sufficient when lower 12 bits are zero
            LuiInstr;
        true ->
            % Sign extend lower 12 bits
            LowerSigned =
                if
                    Lower >= 16#800 -> Lower - 16#1000;
                    true -> Lower
                end,
            AddiInstr = addi(Rd, Rd, LowerSigned),
            <<LuiInstr/binary, AddiInstr/binary>>
    end;
li(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -16#80000000, 16#FFFFFFFF}).

%% MV - Move (copy register)
%% Expands to: addi rd, rs, 0 or c.mv rd, rs
-spec mv(riscv_register(), riscv_register()) -> binary().
mv(Rd, Rs) when Rd =/= zero, Rs =/= zero ->
    % Use c.mv when both rd and rs are not zero
    c_mv(Rd, Rs);
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

%% MUL - Multiply (RV32M extension)
%% Multiplies rs1 by rs2 and places the lower 32 bits in rd
%% Format: mul rd, rs1, rs2
%% Encoding: R-type with opcode=0x33, funct3=0x0, funct7=0x01
-spec mul(riscv_register(), riscv_register(), riscv_register()) -> binary().
mul(Rd, Rs1, Rs2) ->
    % Opcode: 0110011 (0x33), Funct3: 000, Funct7: 0000001
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#01).

%%-----------------------------------------------------------------------------
%% C Extension (RV32C) - Compressed Instructions
%%-----------------------------------------------------------------------------
%% The C extension adds 16-bit compressed instructions to reduce code size.
%% All compressed instructions are 16 bits (2 bytes) and use a different
%% encoding format from the base 32-bit instructions.
%%
%% Register encoding for compressed instructions:
%% - Some instructions use the full 5-bit register encoding (x0-x31)
%% - Others use 3-bit encoding for registers x8-x15 (s0, s1, a0-a5)
%%   This is called the "compressed register set" or "C register set"
%%
%% Instruction formats:
%% - CR (Register): funct4 | rd/rs1 | rs2 | op
%% - CI (Immediate): funct3 | imm | rd/rs1 | imm | op
%% - CSS (Stack Store): funct3 | imm | rs2 | op
%% - CIW (Wide Immediate): funct3 | imm | rd' | op
%% - CL (Load): funct3 | imm | rs1' | imm | rd' | op
%% - CS (Store): funct3 | imm | rs1' | imm | rs2' | op
%% - CA (Arithmetic): funct6 | rd'/rs1' | funct2 | rs2' | op
%% - CB (Branch): funct3 | offset | rs1' | offset | op
%% - CJ (Jump): funct3 | jump target | op
%%
%% See: RISC-V Instruction Set Manual, Volume I, Chapter 16
%%-----------------------------------------------------------------------------

%% Convert register to compressed register encoding (3 bits for x8-x15)
%% Returns the 3-bit encoding (0-7 maps to x8-x15)
-spec reg_to_c_num(riscv_register()) -> 0..7.
reg_to_c_num(s0) -> 0;
reg_to_c_num(fp) -> 0;
reg_to_c_num(s1) -> 1;
reg_to_c_num(a0) -> 2;
reg_to_c_num(a1) -> 3;
reg_to_c_num(a2) -> 4;
reg_to_c_num(a3) -> 5;
reg_to_c_num(a4) -> 6;
reg_to_c_num(a5) -> 7;
reg_to_c_num(Reg) -> error({register_not_in_compressed_set, Reg, 's0/fp, s1, a0-a5'}).

%% Check if a register is in the compressed register set (s0/fp, s1, a0-a5)
-spec is_compressed_reg(riscv_register()) -> boolean().
is_compressed_reg(s0) -> true;
is_compressed_reg(fp) -> true;
is_compressed_reg(s1) -> true;
is_compressed_reg(a0) -> true;
is_compressed_reg(a1) -> true;
is_compressed_reg(a2) -> true;
is_compressed_reg(a3) -> true;
is_compressed_reg(a4) -> true;
is_compressed_reg(a5) -> true;
is_compressed_reg(_) -> false.

%%-----------------------------------------------------------------------------
%% CR-type instruction encoding (Compressed Register format)
%%-----------------------------------------------------------------------------
%% CR format: funct4 (4) | rd/rs1 (5) | rs2 (5) | op (2)
%% Bits:      15-12         11-7          6-2       1-0

-spec encode_cr_type(
    Opcode :: 0..3,
    Rd :: riscv_register(),
    Rs2 :: riscv_register(),
    Funct4 :: 0..15
) -> binary().
encode_cr_type(Opcode, Rd, Rs2, Funct4) ->
    RdNum = reg_to_num(Rd),
    Rs2Num = reg_to_num(Rs2),
    Instr =
        (Funct4 bsl 12) bor
            (RdNum bsl 7) bor
            (Rs2Num bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CI-type instruction encoding (Compressed Immediate format)
%%-----------------------------------------------------------------------------
%% CI format: funct3 (3) | imm[5] (1) | rd/rs1 (5) | imm[4:0] (5) | op (2)
%% Bits:      15-13        12            11-7          6-2            1-0

-spec encode_ci_type(
    Opcode :: 0..3,
    Rd :: riscv_register(),
    Imm :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_ci_type(Opcode, Rd, Imm, Funct3) ->
    RdNum = reg_to_num(Rd),
    % Extract immediate bits
    ImmMasked = Imm band 16#3F,
    Imm5 = (ImmMasked bsr 5) band 1,
    Imm4_0 = ImmMasked band 16#1F,
    Instr =
        (Funct3 bsl 13) bor
            (Imm5 bsl 12) bor
            (RdNum bsl 7) bor
            (Imm4_0 bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CSS-type instruction encoding (Compressed Stack Store format)
%%-----------------------------------------------------------------------------
%% CSS format: funct3 (3) | imm[5:0] (6) | rs2 (5) | op (2)
%% Bits:       15-13        12-7           6-2       1-0

-spec encode_css_type(
    Opcode :: 0..3,
    Rs2 :: riscv_register(),
    Imm :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_css_type(Opcode, Rs2, Imm, Funct3) ->
    Rs2Num = reg_to_num(Rs2),
    % Extract immediate bits (typically scaled for word access)
    ImmMasked = Imm band 16#3F,
    Instr =
        (Funct3 bsl 13) bor
            (ImmMasked bsl 7) bor
            (Rs2Num bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CIW-type instruction encoding (Compressed Wide Immediate format)
%%-----------------------------------------------------------------------------
%% CIW format: funct3 (3) | imm[7:0] (8) | rd' (3) | op (2)
%% Bits:       15-13        12-5           4-2       1-0

-spec encode_ciw_type(
    Opcode :: 0..3,
    Rd :: riscv_register(),
    Imm :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_ciw_type(Opcode, Rd, Imm, Funct3) ->
    RdNum = reg_to_c_num(Rd),
    ImmMasked = Imm band 16#FF,
    Instr =
        (Funct3 bsl 13) bor
            (ImmMasked bsl 5) bor
            (RdNum bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CL-type instruction encoding (Compressed Load format)
%%-----------------------------------------------------------------------------
%% CL format: funct3 (3) | imm (3) | rs1' (3) | imm (2) | rd' (3) | op (2)
%% Bits:      15-13        12-10     9-7        6-5       4-2       1-0

-spec encode_cl_type(
    Opcode :: 0..3,
    Rd :: riscv_register(),
    Rs1 :: riscv_register(),
    Imm :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_cl_type(Opcode, Rd, Rs1, Imm, Funct3) ->
    RdNum = reg_to_c_num(Rd),
    Rs1Num = reg_to_c_num(Rs1),
    % For LW: imm[5:3] goes to bits 12-10, imm[2] goes to bit 6, imm[6] goes to bit 5
    ImmMasked = Imm band 16#7F,
    Imm5_3 = (ImmMasked bsr 3) band 7,
    Imm2 = (ImmMasked bsr 2) band 1,
    Imm6 = (ImmMasked bsr 6) band 1,
    Instr =
        (Funct3 bsl 13) bor
            (Imm5_3 bsl 10) bor
            (Rs1Num bsl 7) bor
            (Imm2 bsl 6) bor
            (Imm6 bsl 5) bor
            (RdNum bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CS-type instruction encoding (Compressed Store format)
%%-----------------------------------------------------------------------------
%% CS format: funct3 (3) | imm (3) | rs1' (3) | imm (2) | rs2' (3) | op (2)
%% Bits:      15-13        12-10     9-7        6-5       4-2        1-0

-spec encode_cs_type(
    Opcode :: 0..3,
    Rs1 :: riscv_register(),
    Rs2 :: riscv_register(),
    Imm :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_cs_type(Opcode, Rs1, Rs2, Imm, Funct3) ->
    Rs1Num = reg_to_c_num(Rs1),
    Rs2Num = reg_to_c_num(Rs2),
    % For SW: imm[5:3] goes to bits 12-10, imm[2] goes to bit 6, imm[6] goes to bit 5
    ImmMasked = Imm band 16#7F,
    Imm5_3 = (ImmMasked bsr 3) band 7,
    Imm2 = (ImmMasked bsr 2) band 1,
    Imm6 = (ImmMasked bsr 6) band 1,
    Instr =
        (Funct3 bsl 13) bor
            (Imm5_3 bsl 10) bor
            (Rs1Num bsl 7) bor
            (Imm2 bsl 6) bor
            (Imm6 bsl 5) bor
            (Rs2Num bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CA-type instruction encoding (Compressed Arithmetic format)
%%-----------------------------------------------------------------------------
%% CA format: funct6 (6) | rd'/rs1' (3) | funct2 (2) | rs2' (3) | op (2)
%% Bits:      15-10        9-7             6-5          4-2        1-0

-spec encode_ca_type(
    Opcode :: 0..3,
    Rd :: riscv_register(),
    Rs2 :: riscv_register(),
    Funct2 :: 0..3,
    Funct6 :: 0..63
) -> binary().
encode_ca_type(Opcode, Rd, Rs2, Funct2, Funct6) ->
    RdNum = reg_to_c_num(Rd),
    Rs2Num = reg_to_c_num(Rs2),
    Instr =
        (Funct6 bsl 10) bor
            (RdNum bsl 7) bor
            (Funct2 bsl 5) bor
            (Rs2Num bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CB-type instruction encoding (Compressed Branch format)
%%-----------------------------------------------------------------------------
%% CB format: funct3 (3) | offset (8) | rs1' (3) | op (2)
%% Bits:      15-13        12-5         4-2        1-0
%% Offset encoding: offset[8|4:3|7:6|2:1|5] -> bits [12|11:10|6:5|4:3|2]

-spec encode_cb_type(
    Opcode :: 0..3,
    Rs1 :: riscv_register(),
    Offset :: integer(),
    Funct3 :: 0..7
) -> binary().
encode_cb_type(Opcode, Rs1, Offset, Funct3) ->
    Rs1Num = reg_to_c_num(Rs1),
    % Extract offset bits: offset[8|4:3|7:6|2:1|5] -> bits [12|11:10|6:5|4:3|2]
    OffsetMasked = Offset band 16#1FF,
    Offset8 = (OffsetMasked bsr 8) band 1,
    Offset4_3 = (OffsetMasked bsr 3) band 3,
    Offset7_6 = (OffsetMasked bsr 6) band 3,
    Offset2_1 = (OffsetMasked bsr 1) band 3,
    Offset5 = (OffsetMasked bsr 5) band 1,
    Instr =
        (Funct3 bsl 13) bor
            (Offset8 bsl 12) bor
            (Offset4_3 bsl 10) bor
            (Rs1Num bsl 7) bor
            (Offset7_6 bsl 5) bor
            (Offset2_1 bsl 3) bor
            (Offset5 bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% CJ-type instruction encoding (Compressed Jump format)
%%-----------------------------------------------------------------------------
%% CJ format: funct3 (3) | jump target (11) | op (2)
%% Bits:      15-13        12-2              1-0
%% Target encoding: target[11|4|9:8|10|6|7|3:1|5] -> bits [12|11|10:9|8|7|6|5:3|2]

-spec encode_cj_type(Opcode :: 0..3, Offset :: integer(), Funct3 :: 0..7) -> binary().
encode_cj_type(Opcode, Offset, Funct3) ->
    % Extract offset bits: offset[11|4|9:8|10|6|7|3:1|5]
    OffsetMasked = Offset band 16#FFF,
    Offset11 = (OffsetMasked bsr 11) band 1,
    Offset4 = (OffsetMasked bsr 4) band 1,
    Offset9_8 = (OffsetMasked bsr 8) band 3,
    Offset10 = (OffsetMasked bsr 10) band 1,
    Offset6 = (OffsetMasked bsr 6) band 1,
    Offset7 = (OffsetMasked bsr 7) band 1,
    Offset3_1 = (OffsetMasked bsr 1) band 7,
    Offset5 = (OffsetMasked bsr 5) band 1,
    OffsetBits =
        (Offset11 bsl 10) bor
            (Offset4 bsl 9) bor
            (Offset9_8 bsl 7) bor
            (Offset10 bsl 6) bor
            (Offset6 bsl 5) bor
            (Offset7 bsl 4) bor
            (Offset3_1 bsl 1) bor
            Offset5,
    Instr =
        (Funct3 bsl 13) bor
            (OffsetBits bsl 2) bor
            Opcode,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% C Extension - Arithmetic and Logical Instructions
%%-----------------------------------------------------------------------------

%% C.ADD - Compressed Add
%% rd = rd + rs2 (both rd and rs2 are full 5-bit registers)
%% Format: CR-type
%% Encoding: funct4=1001 (0x9), op=10 (0x2)
-spec c_add(riscv_register(), riscv_register()) -> binary().
c_add(Rd, Rs2) ->
    encode_cr_type(16#2, Rd, Rs2, 16#9).

%% C.MV - Compressed Move (copy register)
%% rd = rs2 (both are full 5-bit registers)
%% Format: CR-type
%% Encoding: funct4=1000 (0x8), op=10 (0x2)
-spec c_mv(riscv_register(), riscv_register()) -> binary().
c_mv(Rd, Rs2) ->
    encode_cr_type(16#2, Rd, Rs2, 16#8).

%% C.SUB - Compressed Subtract
%% rd' = rd' - rs2' (both use 3-bit compressed register encoding)
%% Format: CA-type
%% Encoding: funct6=100011 (0x23), funct2=00, op=01 (0x1)
-spec c_sub(riscv_register(), riscv_register()) -> binary().
c_sub(Rd, Rs2) ->
    encode_ca_type(16#1, Rd, Rs2, 16#0, 16#23).

%% C.AND - Compressed Bitwise AND
%% rd' = rd' & rs2'
%% Format: CA-type
%% Encoding: funct6=100011 (0x23), funct2=11, op=01 (0x1)
-spec c_and(riscv_register(), riscv_register()) -> binary().
c_and(Rd, Rs2) ->
    encode_ca_type(16#1, Rd, Rs2, 16#3, 16#23).

%% C.OR - Compressed Bitwise OR
%% rd' = rd' | rs2'
%% Format: CA-type
%% Encoding: funct6=100011 (0x23), funct2=10, op=01 (0x1)
-spec c_or(riscv_register(), riscv_register()) -> binary().
c_or(Rd, Rs2) ->
    encode_ca_type(16#1, Rd, Rs2, 16#2, 16#23).

%% C.XOR - Compressed Bitwise XOR
%% rd' = rd' ^ rs2'
%% Format: CA-type
%% Encoding: funct6=100011 (0x23), funct2=01, op=01 (0x1)
-spec c_xor(riscv_register(), riscv_register()) -> binary().
c_xor(Rd, Rs2) ->
    encode_ca_type(16#1, Rd, Rs2, 16#1, 16#23).

%%-----------------------------------------------------------------------------
%% C Extension - Immediate Instructions
%%-----------------------------------------------------------------------------

%% C.ADDI - Compressed Add Immediate
%% rd = rd + imm (rd is full 5-bit register, imm is 6-bit signed)
%% Format: CI-type
%% Encoding: funct3=000, op=01 (0x1)
-spec c_addi(riscv_register(), integer()) -> binary().
c_addi(Rd, Imm) when Imm >= -32, Imm =< 31, Rd =/= zero ->
    encode_ci_type(16#1, Rd, Imm, 16#0);
c_addi(zero, _Imm) ->
    error({invalid_compressed_instruction, c_addi, 'rd cannot be zero'});
c_addi(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -32, 31}).

%% C.ANDI - Compressed AND Immediate
%% rd' = rd' & imm (rd' uses 3-bit encoding, imm is 6-bit signed)
%% Format: CB-type (with special encoding)
%% Encoding: funct3=100, imm[5]=bit12, funct2=10, imm[4:0]=bits 6:2, op=01
-spec c_andi(riscv_register(), integer()) -> binary().
c_andi(Rd, Imm) when Imm >= -32, Imm =< 31 ->
    RdNum = reg_to_c_num(Rd),
    ImmMasked = Imm band 16#3F,
    Imm5 = (ImmMasked bsr 5) band 1,
    Imm4_0 = ImmMasked band 16#1F,
    Instr =
        (16#4 bsl 13) bor
            (Imm5 bsl 12) bor
            (16#2 bsl 10) bor
            (RdNum bsl 7) bor
            (Imm4_0 bsl 2) bor
            16#1,
    <<Instr:16/little>>;
c_andi(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -32, 31}).

%% C.LI - Compressed Load Immediate
%% rd = imm (rd is full 5-bit register, imm is 6-bit signed)
%% Format: CI-type
%% Encoding: funct3=010, op=01 (0x1)
-spec c_li(riscv_register(), integer()) -> binary().
c_li(Rd, Imm) when Imm >= -32, Imm =< 31 ->
    encode_ci_type(16#1, Rd, Imm, 16#2);
c_li(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -32, 31}).

%% C.LUI - Compressed Load Upper Immediate
%% rd = imm << 12 (rd is full 5-bit register, imm is 6-bit signed non-zero)
%% Format: CI-type
%% Encoding: funct3=011, op=01 (0x1)
-spec c_lui(riscv_register(), integer()) -> binary().
c_lui(Rd, Imm) when Imm >= -32, Imm =< 31, Imm =/= 0, Rd =/= zero, Rd =/= sp ->
    encode_ci_type(16#1, Rd, Imm, 16#3);
c_lui(Rd, _Imm) when Rd =:= zero; Rd =:= sp ->
    error({invalid_compressed_instruction, c_lui, 'rd cannot be zero or sp'});
c_lui(_Rd, 0) ->
    error({invalid_compressed_instruction, c_lui, 'immediate cannot be zero'});
c_lui(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, -32, 31}).

%% C.ADDI16SP - Compressed Add Immediate to SP (scaled by 16)
%% sp = sp + imm (imm is 10-bit signed, must be multiple of 16, non-zero)
%% Format: CI-type (special encoding)
%% Encoding: funct3=011, rd/rs1=sp (x2), op=01
-spec c_addi16sp(integer()) -> binary().
c_addi16sp(Imm) when
    Imm >= -512, Imm =< 496, (Imm rem 16) =:= 0, Imm =/= 0
->
    % Immediate encoding: nzimm[9|4|6|8:7|5] -> bits [12|6|5|4:3|2]
    ImmMasked = Imm band 16#3FF,
    Imm9 = (ImmMasked bsr 9) band 1,
    Imm4 = (ImmMasked bsr 4) band 1,
    Imm6 = (ImmMasked bsr 6) band 1,
    Imm8_7 = (ImmMasked bsr 7) band 3,
    Imm5 = (ImmMasked bsr 5) band 1,
    ImmBits = (Imm9 bsl 5) bor (Imm4 bsl 4) bor (Imm6 bsl 3) bor (Imm8_7 bsl 1) bor Imm5,
    encode_ci_type(16#1, sp, ImmBits, 16#3);
c_addi16sp(0) ->
    error({invalid_compressed_instruction, c_addi16sp, 'immediate cannot be zero'});
c_addi16sp(Imm) when (Imm rem 16) =/= 0 ->
    error({immediate_not_aligned, Imm, 16});
c_addi16sp(Imm) ->
    error({immediate_out_of_range, Imm, -512, 496}).

%% C.ADDI4SPN - Compressed Add Immediate (scaled by 4) to SP, store in rd'
%% rd' = sp + imm (imm is 10-bit unsigned, must be multiple of 4, non-zero)
%% Format: CIW-type
%% Encoding: funct3=000, op=00 (0x0)
-spec c_addi4spn(riscv_register(), integer()) -> binary().
c_addi4spn(Rd, Imm) when
    Imm >= 4, Imm =< 1020, (Imm rem 4) =:= 0
->
    % Immediate encoding: nzuimm[5:4|9:6|2|3] -> bits [12:11|10:7|6|5]
    ImmMasked = Imm band 16#3FF,
    Imm5_4 = (ImmMasked bsr 4) band 3,
    Imm9_6 = (ImmMasked bsr 6) band 15,
    Imm2 = (ImmMasked bsr 2) band 1,
    Imm3 = (ImmMasked bsr 3) band 1,
    ImmBits = (Imm5_4 bsl 6) bor (Imm9_6 bsl 2) bor (Imm2 bsl 1) bor Imm3,
    encode_ciw_type(16#0, Rd, ImmBits, 16#0);
c_addi4spn(_Rd, Imm) when Imm =:= 0 ->
    error({invalid_compressed_instruction, c_addi4spn, 'immediate cannot be zero'});
c_addi4spn(_Rd, Imm) when (Imm rem 4) =/= 0 ->
    error({immediate_not_aligned, Imm, 4});
c_addi4spn(_Rd, Imm) ->
    error({immediate_out_of_range, Imm, 4, 1020}).

%%-----------------------------------------------------------------------------
%% C Extension - Shift Instructions
%%-----------------------------------------------------------------------------

%% C.SLLI - Compressed Shift Left Logical Immediate
%% rd = rd << shamt (rd is full 5-bit register, shamt is 6-bit unsigned)
%% Format: CI-type
%% Encoding: funct3=000, op=10 (0x2)
-spec c_slli(riscv_register(), 0..63) -> binary().
c_slli(Rd, Shamt) when Shamt >= 0, Shamt =< 63, Rd =/= zero ->
    encode_ci_type(16#2, Rd, Shamt, 16#0);
c_slli(zero, _Shamt) ->
    error({invalid_compressed_instruction, c_slli, 'rd cannot be zero'});
c_slli(_Rd, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 63}).

%% C.SRLI - Compressed Shift Right Logical Immediate
%% rd' = rd' >> shamt (rd' uses 3-bit encoding, shamt is 6-bit unsigned)
%% Format: CB-type (with special encoding)
%% Encoding: funct3=100, shamt[5]=bit12, funct2=00, shamt[4:0]=bits 6:2, op=01
-spec c_srli(riscv_register(), 0..63) -> binary().
c_srli(Rd, Shamt) when Shamt >= 0, Shamt =< 63 ->
    RdNum = reg_to_c_num(Rd),
    Shamt5 = (Shamt bsr 5) band 1,
    Shamt4_0 = Shamt band 16#1F,
    Instr =
        (16#4 bsl 13) bor
            (Shamt5 bsl 12) bor
            (16#0 bsl 10) bor
            (RdNum bsl 7) bor
            (Shamt4_0 bsl 2) bor
            16#1,
    <<Instr:16/little>>;
c_srli(_Rd, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 63}).

%% C.SRAI - Compressed Shift Right Arithmetic Immediate
%% rd' = rd' >> shamt (sign-extend, rd' uses 3-bit encoding, shamt is 6-bit unsigned)
%% Format: CB-type (with special encoding)
%% Encoding: funct3=100, shamt[5]=bit12, funct2=01, shamt[4:0]=bits 6:2, op=01
-spec c_srai(riscv_register(), 0..63) -> binary().
c_srai(Rd, Shamt) when Shamt >= 0, Shamt =< 63 ->
    RdNum = reg_to_c_num(Rd),
    Shamt5 = (Shamt bsr 5) band 1,
    Shamt4_0 = Shamt band 16#1F,
    Instr =
        (16#4 bsl 13) bor
            (Shamt5 bsl 12) bor
            (16#1 bsl 10) bor
            (RdNum bsl 7) bor
            (Shamt4_0 bsl 2) bor
            16#1,
    <<Instr:16/little>>;
c_srai(_Rd, Shamt) ->
    error({shift_amount_out_of_range, Shamt, 0, 63}).

%%-----------------------------------------------------------------------------
%% C Extension - Load/Store Instructions
%%-----------------------------------------------------------------------------

%% C.LW - Compressed Load Word
%% rd' = mem[rs1' + offset] (both use 3-bit encoding, offset is 7-bit unsigned, multiple of 4)
%% Format: CL-type
%% Encoding: funct3=010, op=00 (0x0)
-spec c_lw(riscv_register(), {riscv_register(), integer()}) -> binary().
c_lw(Rd, {Rs1, Offset}) when
    Offset >= 0, Offset =< 124, (Offset rem 4) =:= 0
->
    encode_cl_type(16#0, Rd, Rs1, Offset, 16#2);
c_lw(_Rd, {_Rs1, Offset}) when (Offset rem 4) =/= 0 ->
    error({offset_not_aligned, Offset, 4});
c_lw(_Rd, {_Rs1, Offset}) ->
    error({offset_out_of_range, Offset, 0, 124}).

%% C.SW - Compressed Store Word
%% mem[rs1' + offset] = rs2' (both use 3-bit encoding, offset is 7-bit unsigned, multiple of 4)
%% Format: CS-type
%% Encoding: funct3=110, op=00 (0x0)
-spec c_sw(riscv_register(), {riscv_register(), integer()}) -> binary().
c_sw(Rs2, {Rs1, Offset}) when
    Offset >= 0, Offset =< 124, (Offset rem 4) =:= 0
->
    encode_cs_type(16#0, Rs1, Rs2, Offset, 16#6);
c_sw(_Rs2, {_Rs1, Offset}) when (Offset rem 4) =/= 0 ->
    error({offset_not_aligned, Offset, 4});
c_sw(_Rs2, {_Rs1, Offset}) ->
    error({offset_out_of_range, Offset, 0, 124}).

%% C.LWSP - Compressed Load Word from Stack Pointer
%% rd = mem[sp + offset] (rd is full 5-bit register, offset is 8-bit unsigned, multiple of 4)
%% Format: CI-type (special encoding)
%% Encoding: funct3=010, op=10 (0x2)
-spec c_lwsp(riscv_register(), integer()) -> binary().
c_lwsp(Rd, Offset) when
    Offset >= 0, Offset =< 252, (Offset rem 4) =:= 0, Rd =/= zero
->
    % Offset encoding: offset[5|4:2|7:6] -> bits [12|6:4|3:2]
    OffsetMasked = Offset band 16#FF,
    Offset5 = (OffsetMasked bsr 5) band 1,
    Offset4_2 = (OffsetMasked bsr 2) band 7,
    Offset7_6 = (OffsetMasked bsr 6) band 3,
    ImmBits = (Offset5 bsl 5) bor (Offset4_2 bsl 2) bor Offset7_6,
    encode_ci_type(16#2, Rd, ImmBits, 16#2);
c_lwsp(zero, _Offset) ->
    error({invalid_compressed_instruction, c_lwsp, 'rd cannot be zero'});
c_lwsp(_Rd, Offset) when (Offset rem 4) =/= 0 ->
    error({offset_not_aligned, Offset, 4});
c_lwsp(_Rd, Offset) ->
    error({offset_out_of_range, Offset, 0, 252}).

%% C.SWSP - Compressed Store Word to Stack Pointer
%% mem[sp + offset] = rs2 (rs2 is full 5-bit register, offset is 8-bit unsigned, multiple of 4)
%% Format: CSS-type
%% Encoding: funct3=110, op=10 (0x2)
-spec c_swsp(riscv_register(), integer()) -> binary().
c_swsp(Rs2, Offset) when
    Offset >= 0, Offset =< 252, (Offset rem 4) =:= 0
->
    % Offset encoding: offset[5:2|7:6] -> bits [12:9|8:7]
    OffsetMasked = Offset band 16#FF,
    Offset5_2 = (OffsetMasked bsr 2) band 15,
    Offset7_6 = (OffsetMasked bsr 6) band 3,
    ImmBits = (Offset5_2 bsl 2) bor Offset7_6,
    encode_css_type(16#2, Rs2, ImmBits, 16#6);
c_swsp(_Rs2, Offset) when (Offset rem 4) =/= 0 ->
    error({offset_not_aligned, Offset, 4});
c_swsp(_Rs2, Offset) ->
    error({offset_out_of_range, Offset, 0, 252}).

%%-----------------------------------------------------------------------------
%% C Extension - Branch and Jump Instructions
%%-----------------------------------------------------------------------------

%% C.BEQZ - Compressed Branch if Equal to Zero
%% if (rs1' == 0) pc += offset (rs1' uses 3-bit encoding, offset is 9-bit signed, multiple of 2)
%% Format: CB-type
%% Encoding: funct3=110, op=01 (0x1)
-spec c_beqz(riscv_register(), integer()) -> binary().
c_beqz(Rs1, Offset) when
    Offset >= -256, Offset =< 254, (Offset rem 2) =:= 0
->
    encode_cb_type(16#1, Rs1, Offset, 16#6);
c_beqz(_Rs1, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
c_beqz(_Rs1, Offset) ->
    error({offset_out_of_range, Offset, -256, 254}).

%% C.BNEZ - Compressed Branch if Not Equal to Zero
%% if (rs1' != 0) pc += offset (rs1' uses 3-bit encoding, offset is 9-bit signed, multiple of 2)
%% Format: CB-type
%% Encoding: funct3=111, op=01 (0x1)
-spec c_bnez(riscv_register(), integer()) -> binary().
c_bnez(Rs1, Offset) when
    Offset >= -256, Offset =< 254, (Offset rem 2) =:= 0
->
    encode_cb_type(16#1, Rs1, Offset, 16#7);
c_bnez(_Rs1, Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
c_bnez(_Rs1, Offset) ->
    error({offset_out_of_range, Offset, -256, 254}).

%% C.J - Compressed Unconditional Jump
%% pc += offset (offset is 12-bit signed, multiple of 2)
%% Format: CJ-type
%% Encoding: funct3=101, op=01 (0x1)
-spec c_j(integer()) -> binary().
c_j(Offset) when
    Offset >= -2048, Offset =< 2046, (Offset rem 2) =:= 0
->
    encode_cj_type(16#1, Offset, 16#5);
c_j(Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
c_j(Offset) ->
    error({offset_out_of_range, Offset, -2048, 2046}).

%% C.JAL - Compressed Jump and Link (RV32C only, rd is implicitly ra)
%% ra = pc + 2; pc += offset (offset is 12-bit signed, multiple of 2)
%% Format: CJ-type
%% Encoding: funct3=001 (0x1), op=01 (0x1)
-spec c_jal(integer()) -> binary().
c_jal(Offset) when
    Offset >= -2048, Offset =< 2046, (Offset rem 2) =:= 0
->
    encode_cj_type(16#1, Offset, 16#1);
c_jal(Offset) when (Offset rem 2) =/= 0 ->
    error({offset_not_aligned, Offset, 2});
c_jal(Offset) ->
    error({offset_out_of_range, Offset, -2048, 2046}).

%% C.JR - Compressed Jump Register
%% pc = rs1 (rs1 is full 5-bit register, must not be zero)
%% Format: CR-type
%% Encoding: funct4=1000 (0x8), rs2=x0, op=10 (0x2)
-spec c_jr(riscv_register()) -> binary().
c_jr(Rs1) when Rs1 =/= zero ->
    encode_cr_type(16#2, Rs1, zero, 16#8);
c_jr(zero) ->
    error({invalid_compressed_instruction, c_jr, 'rs1 cannot be zero'}).

%% C.JALR - Compressed Jump and Link Register
%% ra = pc + 2; pc = rs1 (rs1 is full 5-bit register, must not be zero)
%% Format: CR-type
%% Encoding: funct4=1001 (0x9), rs2=x0, op=10 (0x2)
-spec c_jalr(riscv_register()) -> binary().
c_jalr(Rs1) when Rs1 =/= zero ->
    encode_cr_type(16#2, Rs1, zero, 16#9);
c_jalr(zero) ->
    error({invalid_compressed_instruction, c_jalr, 'rs1 cannot be zero'}).

%% C.EBREAK - Compressed Environment Breakpoint
%% Causes a breakpoint exception to be raised
%% Format: CR-type
%% Encoding: funct4=1001 (0x9), rs1/rd=x0, rs2=x0, op=10 (0x2)
-spec c_ebreak() -> binary().
c_ebreak() ->
    encode_cr_type(16#2, zero, zero, 16#9).

%%-----------------------------------------------------------------------------
%% C Extension - Pseudo-instructions
%%-----------------------------------------------------------------------------

%% C.NOP - Compressed No Operation
%% Expands to: c.addi x0, 0
%% Format: CI-type
%% Encoding: funct3=000, rd/rs1=x0, imm=0, op=01 (0x1)
-spec c_nop() -> binary().
c_nop() ->
    encode_ci_type(16#1, zero, 0, 16#0).
