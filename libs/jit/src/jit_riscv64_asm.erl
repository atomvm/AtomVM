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

-module(jit_riscv64_asm).

%% RV64IMAC assembler module.
%% Most base RV32I/RV32M/RV32C instructions encode identically on RV64, so
%% we delegate to jit_riscv32_asm where the encoding is unchanged.
%% RV64-specific instructions (ld, sd, lwu, 6-bit shifts, etc.) and
%% instructions whose compressed forms differ (jal without c_jal) are
%% implemented locally.

-export([
    % R-type arithmetic and logical instructions (delegate to riscv32)
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
    addiw/3,
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
    % RV64 doubleword load/store
    ld/2,
    ld/3,
    sd/2,
    sd/3,
    % RV64 load word unsigned
    lwu/2,
    lwu/3,
    % Store instructions
    sw/2,
    sw/3,
    sh/2,
    sh/3,
    sb/2,
    sb/3,
    % Branch instructions (delegate to riscv32)
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
    % Upper immediate (delegate to riscv32)
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
    % M-extension (delegate to riscv32)
    mul/3,
    div_/3,
    rem_/3,
    % C Extension - Register-register (delegate to riscv32)
    c_add/2,
    c_sub/2,
    c_and/2,
    c_or/2,
    c_xor/2,
    c_mv/2,
    % C Extension - Immediate (delegate to riscv32)
    c_addi/2,
    c_andi/2,
    c_li/2,
    c_lui/2,
    c_addi16sp/1,
    c_addi4spn/2,
    % C Extension - Shifts (delegate to riscv32, same encoding supports 6-bit shamt)
    c_slli/2,
    c_srli/2,
    c_srai/2,
    % C Extension - Word loads/stores (delegate to riscv32, still valid on RV64)
    c_lw/2,
    c_sw/2,
    c_lwsp/2,
    c_swsp/2,
    % C Extension - RV64 Doubleword loads/stores
    c_ld/2,
    c_sd/2,
    c_ldsp/2,
    c_sdsp/2,
    % C Extension - Branch and Jump (delegate to riscv32)
    c_beqz/2,
    c_bnez/2,
    c_j/1,
    c_jr/1,
    c_jalr/1,
    % C Extension - System (delegate to riscv32)
    c_ebreak/0,
    c_nop/0
]).

-export_type([riscv_register/0]).

-import(jit_riscv32_asm, [
    reg_to_num/1,
    reg_to_c_num/1,
    is_compressed_reg/1,
    encode_i_type/5,
    encode_s_type/5,
    encode_j_type/3
]).

-type riscv_register() :: jit_riscv32_asm:riscv_register().

%%=============================================================================
%% Delegated instructions - identical encoding on RV32 and RV64
%%=============================================================================

%% R-type arithmetic and logical
add(Rd, Rs1, Rs2) -> jit_riscv32_asm:add(Rd, Rs1, Rs2).
sub(Rd, Rs1, Rs2) -> jit_riscv32_asm:sub(Rd, Rs1, Rs2).
and_(Rd, Rs1, Rs2) -> jit_riscv32_asm:and_(Rd, Rs1, Rs2).
or_(Rs1, Rs2) -> jit_riscv32_asm:or_(Rs1, Rs2).
or_(Rd, Rs1, Rs2) -> jit_riscv32_asm:or_(Rd, Rs1, Rs2).
xor_(Rd, Rs1, Rs2) -> jit_riscv32_asm:xor_(Rd, Rs1, Rs2).
sll(Rd, Rs1, Rs2) -> jit_riscv32_asm:sll(Rd, Rs1, Rs2).
srl(Rd, Rs1, Rs2) -> jit_riscv32_asm:srl(Rd, Rs1, Rs2).
sra(Rd, Rs1, Rs2) -> jit_riscv32_asm:sra(Rd, Rs1, Rs2).
slt(Rd, Rs1, Rs2) -> jit_riscv32_asm:slt(Rd, Rs1, Rs2).
sltu(Rd, Rs1, Rs2) -> jit_riscv32_asm:sltu(Rd, Rs1, Rs2).

%% I-type (non-shift)
addi(Rd, Rs1, Imm) -> jit_riscv32_asm:addi(Rd, Rs1, Imm).
-spec addiw(riscv_register(), riscv_register(), integer()) -> binary().
addiw(Rd, Rs1, Imm) when Imm >= -2048, Imm =< 2047 ->
    encode_i_type(16#1B, Rd, 16#0, Rs1, Imm);
addiw(_Rd, _Rs1, Imm) ->
    error({immediate_out_of_range, Imm, -2048, 2047}).
andi(Rd, Rs1, Imm) -> jit_riscv32_asm:andi(Rd, Rs1, Imm).
ori(Rd, Rs1, Imm) -> jit_riscv32_asm:ori(Rd, Rs1, Imm).
xori(Rd, Rs1, Imm) -> jit_riscv32_asm:xori(Rd, Rs1, Imm).
slti(Rd, Rs1, Imm) -> jit_riscv32_asm:slti(Rd, Rs1, Imm).
sltiu(Rd, Rs1, Imm) -> jit_riscv32_asm:sltiu(Rd, Rs1, Imm).

%% Load instructions (word and smaller, identical on RV32/RV64)
lw(Rd, Rs1) -> jit_riscv32_asm:lw(Rd, Rs1).
lw(Rd, Rs1, Offset) -> jit_riscv32_asm:lw(Rd, Rs1, Offset).
lh(Rd, Rs1) -> jit_riscv32_asm:lh(Rd, Rs1).
lh(Rd, Rs1, Offset) -> jit_riscv32_asm:lh(Rd, Rs1, Offset).
lhu(Rd, Rs1) -> jit_riscv32_asm:lhu(Rd, Rs1).
lhu(Rd, Rs1, Offset) -> jit_riscv32_asm:lhu(Rd, Rs1, Offset).
lb(Rd, Rs1) -> jit_riscv32_asm:lb(Rd, Rs1).
lb(Rd, Rs1, Offset) -> jit_riscv32_asm:lb(Rd, Rs1, Offset).
lbu(Rd, Rs1) -> jit_riscv32_asm:lbu(Rd, Rs1).
lbu(Rd, Rs1, Offset) -> jit_riscv32_asm:lbu(Rd, Rs1, Offset).

%% Store instructions (word and smaller, identical on RV32/RV64)
sw(Rs1, Rs2) -> jit_riscv32_asm:sw(Rs1, Rs2).
sw(Rs1, Rs2, Offset) -> jit_riscv32_asm:sw(Rs1, Rs2, Offset).
sh(Rs1, Rs2) -> jit_riscv32_asm:sh(Rs1, Rs2).
sh(Rs1, Rs2, Offset) -> jit_riscv32_asm:sh(Rs1, Rs2, Offset).
sb(Rs1, Rs2) -> jit_riscv32_asm:sb(Rs1, Rs2).
sb(Rs1, Rs2, Offset) -> jit_riscv32_asm:sb(Rs1, Rs2, Offset).

%% Branch instructions
beq(Rs1, Rs2, Offset) -> jit_riscv32_asm:beq(Rs1, Rs2, Offset).
bne(Rs1, Rs2, Offset) -> jit_riscv32_asm:bne(Rs1, Rs2, Offset).
blt(Rs1, Rs2, Offset) -> jit_riscv32_asm:blt(Rs1, Rs2, Offset).
bge(Rs1, Rs2, Offset) -> jit_riscv32_asm:bge(Rs1, Rs2, Offset).
bltu(Rs1, Rs2, Offset) -> jit_riscv32_asm:bltu(Rs1, Rs2, Offset).
bgeu(Rs1, Rs2, Offset) -> jit_riscv32_asm:bgeu(Rs1, Rs2, Offset).

%% JALR (identical encoding)
jalr(Rd, Rs1, Offset) -> jit_riscv32_asm:jalr(Rd, Rs1, Offset).
jalr(Rd, Rs1) -> jit_riscv32_asm:jalr(Rd, Rs1).

%% Upper immediate
lui(Rd, Imm) -> jit_riscv32_asm:lui(Rd, Imm).
auipc(Rd, Imm) -> jit_riscv32_asm:auipc(Rd, Imm).

%% Pseudo-instructions (except li and jal which differ)
nop() -> jit_riscv32_asm:nop().
mv(Rd, Rs1) -> jit_riscv32_asm:mv(Rd, Rs1).
not_(Rd, Rs1) -> jit_riscv32_asm:not_(Rd, Rs1).
neg(Rd, Rs1) -> jit_riscv32_asm:neg(Rd, Rs1).
jr(Rs1) -> jit_riscv32_asm:jr(Rs1).
ret() -> jit_riscv32_asm:ret().
call(Rd, Offset) -> jit_riscv32_asm:call(Rd, Offset).

%% M-extension
mul(Rd, Rs1, Rs2) -> jit_riscv32_asm:mul(Rd, Rs1, Rs2).
div_(Rd, Rs1, Rs2) -> jit_riscv32_asm:div_(Rd, Rs1, Rs2).
rem_(Rd, Rs1, Rs2) -> jit_riscv32_asm:rem_(Rd, Rs1, Rs2).

%% C Extension - Register-register (identical encoding)
c_add(Rd, Rs2) -> jit_riscv32_asm:c_add(Rd, Rs2).
c_sub(Rd, Rs2) -> jit_riscv32_asm:c_sub(Rd, Rs2).
c_and(Rd, Rs2) -> jit_riscv32_asm:c_and(Rd, Rs2).
c_or(Rd, Rs2) -> jit_riscv32_asm:c_or(Rd, Rs2).
c_xor(Rd, Rs2) -> jit_riscv32_asm:c_xor(Rd, Rs2).
c_mv(Rd, Rs2) -> jit_riscv32_asm:c_mv(Rd, Rs2).

%% C Extension - Immediate (identical encoding)
c_addi(Rd, Imm) -> jit_riscv32_asm:c_addi(Rd, Imm).
c_andi(Rd, Imm) -> jit_riscv32_asm:c_andi(Rd, Imm).
c_li(Rd, Imm) -> jit_riscv32_asm:c_li(Rd, Imm).
c_lui(Rd, Imm) -> jit_riscv32_asm:c_lui(Rd, Imm).
c_addi16sp(Imm) -> jit_riscv32_asm:c_addi16sp(Imm).
c_addi4spn(Rd, Imm) -> jit_riscv32_asm:c_addi4spn(Rd, Imm).

%% C Extension - Shifts (same encoding, supports 6-bit shamt on RV64)
c_slli(Rd, Shamt) -> jit_riscv32_asm:c_slli(Rd, Shamt).
c_srli(Rd, Shamt) -> jit_riscv32_asm:c_srli(Rd, Shamt).
c_srai(Rd, Shamt) -> jit_riscv32_asm:c_srai(Rd, Shamt).

%% C Extension - Word loads/stores (still valid on RV64)
c_lw(Rd, Rs1) -> jit_riscv32_asm:c_lw(Rd, Rs1).
c_sw(Rs1, Rs2) -> jit_riscv32_asm:c_sw(Rs1, Rs2).
c_lwsp(Rd, Offset) -> jit_riscv32_asm:c_lwsp(Rd, Offset).
c_swsp(Rs2, Offset) -> jit_riscv32_asm:c_swsp(Rs2, Offset).

%% C Extension - Branch and Jump
c_beqz(Rs1, Offset) -> jit_riscv32_asm:c_beqz(Rs1, Offset).
c_bnez(Rs1, Offset) -> jit_riscv32_asm:c_bnez(Rs1, Offset).
c_j(Offset) -> jit_riscv32_asm:c_j(Offset).
c_jr(Rs1) -> jit_riscv32_asm:c_jr(Rs1).
c_jalr(Rs1) -> jit_riscv32_asm:c_jalr(Rs1).

%% C Extension - System
c_ebreak() -> jit_riscv32_asm:c_ebreak().
c_nop() -> jit_riscv32_asm:c_nop().

%%=============================================================================
%% RV64-specific instructions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% Shift instructions with 6-bit shift amounts (RV64 supports 0-63)
%%-----------------------------------------------------------------------------

%% SLLI - Shift Left Logical Immediate (RV64: 6-bit shamt)
-spec slli(riscv_register(), riscv_register(), 0..63) -> binary().
slli(Rd, Rs1, Shamt) when Rd =:= Rs1, Rd =/= zero, Shamt >= 1, Shamt =< 63 ->
    c_slli(Rd, Shamt);
slli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 63 ->
    encode_i_type(16#13, Rd, 16#1, Rs1, Shamt).

%% SRLI - Shift Right Logical Immediate (RV64: 6-bit shamt)
-spec srli(riscv_register(), riscv_register(), 0..63) -> binary().
srli(Rd, Rs1, Shamt) when Rd =:= Rs1, Shamt >= 1, Shamt =< 63 ->
    case is_compressed_reg(Rd) of
        true -> c_srli(Rd, Shamt);
        false -> encode_i_type(16#13, Rd, 16#5, Rs1, Shamt)
    end;
srli(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 63 ->
    encode_i_type(16#13, Rd, 16#5, Rs1, Shamt).

%% SRAI - Shift Right Arithmetic Immediate (RV64: 6-bit shamt)
-spec srai(riscv_register(), riscv_register(), 0..63) -> binary().
srai(Rd, Rs1, Shamt) when Rd =:= Rs1, Shamt >= 1, Shamt =< 63 ->
    case is_compressed_reg(Rd) of
        true ->
            c_srai(Rd, Shamt);
        false ->
            ImmWithBit30 = Shamt bor (1 bsl 10),
            encode_i_type(16#13, Rd, 16#5, Rs1, ImmWithBit30)
    end;
srai(Rd, Rs1, Shamt) when Shamt >= 0, Shamt =< 63 ->
    ImmWithBit30 = Shamt bor (1 bsl 10),
    encode_i_type(16#13, Rd, 16#5, Rs1, ImmWithBit30).

%%-----------------------------------------------------------------------------
%% JAL - Jump and Link (RV64: no c_jal, that opcode is c_addiw on RV64)
%%-----------------------------------------------------------------------------
-spec jal(riscv_register(), integer()) -> binary().
jal(zero, Offset) when Offset >= -2048, Offset =< 2046, (Offset rem 2) =:= 0 ->
    c_j(Offset);
%% On RV64, c_jal does NOT exist (that opcode is c_addiw)
%% So jal(ra, Offset) always uses the full J-type encoding
jal(Rd, Offset) when Offset >= -1048576, Offset =< 1048574, (Offset rem 2) =:= 0 ->
    encode_j_type(16#6F, Rd, Offset).

%%-----------------------------------------------------------------------------
%% J pseudo-instruction (uses jal, but our jal never calls c_jal)
%%-----------------------------------------------------------------------------
-spec j(integer()) -> binary().
j(Offset) -> jal(zero, Offset).

%%-----------------------------------------------------------------------------
%% LI - Load Immediate (RV64: supports 64-bit immediates)
%%-----------------------------------------------------------------------------
-spec li(riscv_register(), integer()) -> binary().
li(Rd, Imm) when Rd =/= zero, Imm >= -32, Imm =< 31 ->
    c_li(Rd, Imm);
li(Rd, Imm) when Imm >= -2048, Imm =< 2047 ->
    addi(Rd, zero, Imm);
li(Rd, Imm) when Imm >= -16#80000000, Imm =< 16#7FFFF7FF ->
    li_32bit(Rd, Imm);
li(Rd, Imm) when Imm >= 0, Imm =< 16#FFFFFFFF ->
    %% Unsigned 32-bit that doesn't fit in signed 32-bit range.
    %% lui sign-extends on RV64, so values >= 0x80000000 need special handling.
    %% Use lui+addi for upper, then shift and add lower, or use slli to clear upper.
    Lower12 = Imm band 16#FFF,
    Upper20Raw = Imm bsr 12,
    Upper20Masked =
        if
            Lower12 >= 16#800 -> (Upper20Raw + 1) band 16#FFFFF;
            true -> Upper20Raw band 16#FFFFF
        end,
    %% Sign-extend from 20 bits for lui
    Upper20 =
        if
            Upper20Masked >= 16#80000 -> Upper20Masked - 16#100000;
            true -> Upper20Masked
        end,
    LowerSigned =
        if
            Lower12 >= 16#800 -> Lower12 - 16#1000;
            true -> Lower12
        end,
    %% lui sign-extends to 64 bits. Since bit 31 of Imm is set (>= 0x80000000),
    %% lui result will have upper 32 bits set. Clear with slli+srli.
    LuiInstr = lui(Rd, Upper20),
    case LowerSigned of
        0 ->
            I2 = slli(Rd, Rd, 32),
            I3 = srli(Rd, Rd, 32),
            <<LuiInstr/binary, I2/binary, I3/binary>>;
        _ ->
            I2 = addi(Rd, Rd, LowerSigned),
            I3 = slli(Rd, Rd, 32),
            I4 = srli(Rd, Rd, 32),
            <<LuiInstr/binary, I2/binary, I3/binary, I4/binary>>
    end;
li(Rd, Imm) when Imm >= -16#8000000000000000, Imm < 16#10000000000000000 ->
    %% Full 64-bit immediate
    %% Strategy: load upper 32 bits with li_32bit, then incrementally shift
    %% and add lower 32 bits in chunks using slli+addi (12+12+8 = 32 bits total).
    %% We use addi (not ori) because ori sign-extends its immediate, which would
    %% corrupt upper bits when the immediate has bit 11 set.
    Upper32 = (Imm bsr 32) band 16#FFFFFFFF,
    Lower32 = Imm band 16#FFFFFFFF,
    %% Sign-extend upper 32 bits for li_32bit
    UpperSigned =
        if
            Upper32 >= 16#80000000 -> Upper32 - 16#100000000;
            true -> Upper32
        end,
    case Lower32 of
        0 ->
            UpperInstr = li_32bit(Rd, UpperSigned),
            ShiftInstr = slli(Rd, Rd, 32),
            <<UpperInstr/binary, ShiftInstr/binary>>;
        _ ->
            %% Split lower 32 into 3 chunks: [31:20], [19:8], [7:0]
            Chunk1 = (Lower32 bsr 20) band 16#FFF,
            Chunk2 = (Lower32 bsr 8) band 16#FFF,
            Chunk3 = Lower32 band 16#FF,
            %% Compensate for sign extension of addi:
            %% Chunk3 <= 255, always fits in signed 12-bit, no compensation needed.
            %% Chunk2: if >= 0x800, addi sign-extends, compensate in Chunk1.
            AdjChunk1 =
                if
                    Chunk2 >= 16#800 -> Chunk1 + 1;
                    true -> Chunk1
                end,
            C2 =
                if
                    Chunk2 >= 16#800 -> Chunk2 - 16#1000;
                    true -> Chunk2
                end,
            %% AdjChunk1: if >= 0x800, addi sign-extends, compensate in upper.
            AdjUpperSigned =
                if
                    AdjChunk1 >= 16#800 -> UpperSigned + 1;
                    true -> UpperSigned
                end,
            C1Masked = AdjChunk1 band 16#FFF,
            C1 =
                if
                    C1Masked >= 16#800 -> C1Masked - 16#1000;
                    true -> C1Masked
                end,
            %% Handle overflow of AdjUpperSigned beyond signed 32-bit range
            AdjUpperForLi =
                if
                    AdjUpperSigned > 16#7FFFFFFF ->
                        AdjUpperSigned - 16#100000000;
                    true ->
                        AdjUpperSigned
                end,
            %% Generate instructions
            UpperInstr = li_32bit(Rd, AdjUpperForLi),
            ChunkInstrs = li_emit_chunks(Rd, [{12, C1}, {12, C2}, {8, Chunk3}]),
            <<UpperInstr/binary, ChunkInstrs/binary>>
    end.

%% Helper: load a signed 32-bit value using lui+addiw (canonical RV64 li for 32-bit range)
-spec li_32bit(riscv_register(), integer()) -> binary().
li_32bit(Rd, Imm) when Imm >= -2048, Imm =< 2047 ->
    addiw(Rd, zero, Imm);
li_32bit(Rd, Imm) ->
    Lower12 = Imm band 16#FFF,
    Upper20Raw = Imm bsr 12,
    Upper20Masked =
        if
            Lower12 >= 16#800 -> (Upper20Raw + 1) band 16#FFFFF;
            true -> Upper20Raw band 16#FFFFF
        end,
    %% Sign-extend from 20 bits: Erlang band on negative integers produces
    %% unsigned values, but lui expects a signed 20-bit immediate.
    Upper20 =
        if
            Upper20Masked >= 16#80000 -> Upper20Masked - 16#100000;
            true -> Upper20Masked
        end,
    LowerSigned =
        if
            Lower12 >= 16#800 -> Lower12 - 16#1000;
            true -> Lower12
        end,
    LuiInstr = lui(Rd, Upper20),
    case LowerSigned of
        0 ->
            LuiInstr;
        _ ->
            AddiInstr = addiw(Rd, Rd, LowerSigned),
            <<LuiInstr/binary, AddiInstr/binary>>
    end.

%% Helper: emit shift+addi instruction pairs for lower 32-bit chunks.
%% Merges consecutive shifts when the addi value is 0.
-spec li_emit_chunks(riscv_register(), [{pos_integer(), integer()}]) -> binary().
li_emit_chunks(Rd, Chunks) ->
    Merged = li_merge_chunks(Chunks),
    li_emit_merged_chunks(Rd, Merged).

-spec li_merge_chunks([{pos_integer(), integer()}]) -> [{pos_integer(), integer()}].
li_merge_chunks([{S1, 0}, {S2, A2} | Rest]) ->
    li_merge_chunks([{S1 + S2, A2} | Rest]);
li_merge_chunks([H | T]) ->
    [H | li_merge_chunks(T)];
li_merge_chunks([]) ->
    [].

-spec li_emit_merged_chunks(riscv_register(), [{pos_integer(), integer()}]) -> binary().
li_emit_merged_chunks(_Rd, []) ->
    <<>>;
li_emit_merged_chunks(Rd, [{Shift, 0}]) ->
    slli(Rd, Rd, Shift);
li_emit_merged_chunks(Rd, [{Shift, Imm} | Rest]) ->
    I1 = slli(Rd, Rd, Shift),
    I2 = addi(Rd, Rd, Imm),
    RestInstr = li_emit_merged_chunks(Rd, Rest),
    <<I1/binary, I2/binary, RestInstr/binary>>.

%%-----------------------------------------------------------------------------
%% LD - Load Doubleword (RV64I)
%% rd = mem[rs1 + offset] (64-bit load)
%% Format: I-type, opcode=0x03, funct3=0x3
%%-----------------------------------------------------------------------------
-spec ld({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
ld(Rd, {Rs1, Offset}) ->
    ld(Rd, Rs1, Offset);
ld(Rd, Rs1) when is_atom(Rs1) ->
    ld(Rd, Rs1, 0).

-spec ld(riscv_register(), riscv_register(), integer()) -> binary().
ld(Rd, sp, Offset) when
    Offset >= 0, Offset =< 504, (Offset rem 8) =:= 0, Rd =/= zero
->
    c_ldsp(Rd, Offset);
ld(Rd, Rs1, Offset) when Offset >= 0, Offset =< 248, (Offset rem 8) =:= 0 ->
    case is_compressed_reg(Rd) andalso is_compressed_reg(Rs1) of
        true -> c_ld(Rd, {Rs1, Offset});
        false -> encode_i_type(16#03, Rd, 16#3, Rs1, Offset)
    end;
ld(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_i_type(16#03, Rd, 16#3, Rs1, Offset);
ld(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% SD - Store Doubleword (RV64I)
%% mem[rs1 + offset] = rs2 (64-bit store)
%% Format: S-type, opcode=0x23, funct3=0x3
%%-----------------------------------------------------------------------------
-spec sd({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
sd(Rs2, {Rs1, Offset}) ->
    sd(Rs1, Rs2, Offset);
sd(Rs2, Rs1) when is_atom(Rs1) ->
    sd(Rs1, Rs2, 0).

-spec sd(riscv_register(), riscv_register(), integer()) -> binary().
sd(sp, Rs2, Offset) when
    Offset >= 0, Offset =< 504, (Offset rem 8) =:= 0
->
    c_sdsp(Rs2, Offset);
sd(Rs1, Rs2, Offset) when Offset >= 0, Offset =< 248, (Offset rem 8) =:= 0 ->
    case is_compressed_reg(Rs1) andalso is_compressed_reg(Rs2) of
        true -> c_sd(Rs2, {Rs1, Offset});
        false -> encode_s_type(16#23, 16#3, Rs1, Rs2, Offset)
    end;
sd(Rs1, Rs2, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_s_type(16#23, 16#3, Rs1, Rs2, Offset);
sd(_Rs1, _Rs2, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% LWU - Load Word Unsigned (RV64I)
%% rd = zero_extend(mem[rs1 + offset]) (32-bit load, zero-extended to 64 bits)
%% Format: I-type, opcode=0x03, funct3=0x6
%%-----------------------------------------------------------------------------
-spec lwu({riscv_register(), integer()} | riscv_register(), riscv_register() | integer()) ->
    binary().
lwu(Rd, {Rs1, Offset}) ->
    lwu(Rd, Rs1, Offset);
lwu(Rd, Rs1) when is_atom(Rs1) ->
    lwu(Rd, Rs1, 0).

-spec lwu(riscv_register(), riscv_register(), integer()) -> binary().
lwu(Rd, Rs1, Offset) when Offset >= -2048, Offset =< 2047 ->
    encode_i_type(16#03, Rd, 16#6, Rs1, Offset);
lwu(_Rd, _Rs1, Offset) ->
    error({offset_out_of_range, Offset, -2048, 2047}).

%%-----------------------------------------------------------------------------
%% C.LD - Compressed Load Doubleword (RV64C)
%% rd' = mem[rs1' + offset] (offset is 8-byte aligned)
%% Format: CL-type, funct3=011, op=00
%% Offset encoding: imm[5:3] at bits 12:10, imm[7:6] at bits 6:5
%%-----------------------------------------------------------------------------
-spec c_ld(riscv_register(), {riscv_register(), integer()}) -> binary().
c_ld(Rd, {Rs1, Offset}) when
    Offset >= 0, Offset =< 248, (Offset rem 8) =:= 0
->
    Offset5_3 = (Offset bsr 3) band 7,
    Offset7_6 = (Offset bsr 6) band 3,
    RdC = reg_to_c_num(Rd),
    Rs1C = reg_to_c_num(Rs1),
    %% CL format: funct3[15:13] | imm[5:3][12:10] | rs1'[9:7] | imm[7:6][6:5] | rd'[4:2] | op[1:0]
    Instr =
        (16#3 bsl 13) bor (Offset5_3 bsl 10) bor (Rs1C bsl 7) bor (Offset7_6 bsl 5) bor (RdC bsl 2) bor
            16#0,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% C.SD - Compressed Store Doubleword (RV64C)
%% mem[rs1' + offset] = rs2' (offset is 8-byte aligned)
%% Format: CS-type, funct3=111, op=00
%% Offset encoding: imm[5:3] at bits 12:10, imm[7:6] at bits 6:5
%%-----------------------------------------------------------------------------
-spec c_sd(riscv_register(), {riscv_register(), integer()}) -> binary().
c_sd(Rs2, {Rs1, Offset}) when
    Offset >= 0, Offset =< 248, (Offset rem 8) =:= 0
->
    Offset5_3 = (Offset bsr 3) band 7,
    Offset7_6 = (Offset bsr 6) band 3,
    Rs1C = reg_to_c_num(Rs1),
    Rs2C = reg_to_c_num(Rs2),
    %% CS format: funct3[15:13] | imm[5:3][12:10] | rs1'[9:7] | imm[7:6][6:5] | rs2'[4:2] | op[1:0]
    Instr =
        (16#7 bsl 13) bor (Offset5_3 bsl 10) bor (Rs1C bsl 7) bor (Offset7_6 bsl 5) bor (Rs2C bsl 2) bor
            16#0,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% C.LDSP - Compressed Load Doubleword from Stack Pointer (RV64C)
%% rd = mem[sp + offset] (offset is 8-byte aligned, up to 504)
%% Format: CI-type, funct3=011, op=10
%% Offset encoding: offset[5|4:3|8:6] -> bits [12|6:5|4:2]
%%-----------------------------------------------------------------------------
-spec c_ldsp(riscv_register(), integer()) -> binary().
c_ldsp(Rd, Offset) when
    Offset >= 0, Offset =< 504, (Offset rem 8) =:= 0, Rd =/= zero
->
    Offset5 = (Offset bsr 5) band 1,
    Offset4_3 = (Offset bsr 3) band 3,
    Offset8_6 = (Offset bsr 6) band 7,
    ImmBits = (Offset5 bsl 5) bor (Offset4_3 bsl 3) bor Offset8_6,
    RdNum = reg_to_num(Rd),
    %% CI format: funct3[15:13] | imm[5][12] | rd[11:7] | imm[4:3|8:6][6:2] | op[1:0]
    Instr =
        (16#3 bsl 13) bor ((ImmBits bsr 5) bsl 12) bor (RdNum bsl 7) bor
            ((ImmBits band 16#1F) bsl 2) bor 16#2,
    <<Instr:16/little>>.

%%-----------------------------------------------------------------------------
%% C.SDSP - Compressed Store Doubleword to Stack Pointer (RV64C)
%% mem[sp + offset] = rs2 (offset is 8-byte aligned, up to 504)
%% Format: CSS-type, funct3=111, op=10
%% Offset encoding: offset[5:3|8:6] -> bits [12:10|9:7]
%%-----------------------------------------------------------------------------
-spec c_sdsp(riscv_register(), integer()) -> binary().
c_sdsp(Rs2, Offset) when
    Offset >= 0, Offset =< 504, (Offset rem 8) =:= 0
->
    Offset5_3 = (Offset bsr 3) band 7,
    Offset8_6 = (Offset bsr 6) band 7,
    ImmBits = (Offset5_3 bsl 3) bor Offset8_6,
    Rs2Num = reg_to_num(Rs2),
    %% CSS format: funct3[15:13] | imm[5:3|8:6][12:7] | rs2[6:2] | op[1:0]
    Instr = (16#7 bsl 13) bor (ImmBits bsl 7) bor (Rs2Num bsl 2) bor 16#2,
    <<Instr:16/little>>.
