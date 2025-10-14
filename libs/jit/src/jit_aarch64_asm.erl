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

-module(jit_aarch64_asm).

-export([
    add/3,
    add/4,
    sub/3,
    sub/4,
    mul/3,
    madd/4,
    b/1,
    bcc/2,
    blr/1,
    br/1,
    brk/1,
    cbnz/2,
    cbnz_w/2,
    tbz/3,
    tbnz/3,
    cmp/2,
    cmp_w/2,
    and_/3,
    ldr/2,
    ldr_w/2,
    ldr/3,
    lsl/3,
    lsr/3,
    mov/2,
    movk/3,
    movz/3,
    orr/3,
    ret/0,
    nop/0,
    str/2,
    str_w/2,
    str/3,
    tst/2,
    tst_w/2,
    stp/4,
    ldp/4,
    subs/3,
    adr/2
]).

-export_type([
    cc/0
]).

-type aarch64_gpr_register() ::
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
    | r16
    | r17
    | r18
    | r19
    | r20
    | r21
    | r22
    | r23
    | r24
    | r25
    | r26
    | r27
    | r28
    | r29
    | r30
    | sp
    | xzr.

-type cc() :: eq | ne | cs | cc | mi | pl | vs | vc | hi | ls | ge | lt | gt | le | al | nv.

%% Emit an ADD instruction (AArch64 encoding)
%% ADD Rd, Rn, #imm - adds immediate value to register
-spec add(aarch64_gpr_register(), aarch64_gpr_register(), integer()) -> binary().
add(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 4095 ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    %% AArch64 ADD (immediate) encoding: 1001000100iiiiiiiiiiiinnnnndddddd
    %% 0x91000000 | Imm << 10 | Rn << 5 | Rd
    <<(16#91000000 bor ((Imm band 16#FFF) bsl 10) bor (RnNum bsl 5) bor RdNum):32/little>>;
add(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm) ->
    error({unencodable_immediate, Imm});
add(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    add(Rd, Rn, Rm, {lsl, 0}).

%% ADD (shifted register)
%% ADD Rd, Rn, Rm, {lsl, #amount}
-spec add(aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register(), {lsl, 0..63}) ->
    binary().
add(Rd, Rn, Rm, {lsl, Amount}) when
    is_atom(Rd), is_atom(Rn), is_atom(Rm), is_integer(Amount), Amount >= 0, Amount =< 63
->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 ADD (shifted register) encoding: 10001011000mmmmmiiiiiinnnnndddddd
    %% 0x8B000000 | Rm << 16 | Amount << 10 | Rn << 5 | Rd
    <<
        (16#8B000000 bor (RmNum bsl 16) bor ((Amount band 16#3F) bsl 10) bor (RnNum bsl 5) bor
            RdNum):32/little
    >>.

%% Emit an unconditional branch (B) to a 32-bit relative offset (AArch64 encoding)
%% offset is in bytes, relative to the next instruction
-spec b(integer()) -> binary().
b(Offset) when is_integer(Offset) ->
    %% AArch64 B encoding: 0b000101 | imm26 | 00000
    %% imm26 is (Offset / 4) signed, fits in 26 bits
    Offset26 = Offset div 4,
    <<(16#14000000 bor (Offset26 band 16#3FFFFFF)):32/little>>.

%% Emit a breakpoint (BRK) instruction with immediate (AArch64 encoding)
%% imm is a 16-bit immediate value (usually 0 for debuggers)
-spec brk(integer()) -> binary().
brk(Imm) when is_integer(Imm), Imm >= 0, Imm =< 16#FFFF ->
    %% AArch64 BRK encoding: 11010100 00100000 00000000 iiiiiiii iiiiiiii
    %% 0xd4200000 | Imm << 5
    <<(16#D4200000 bor ((Imm band 16#FFFF) bsl 5)):32/little>>.

%% Emit a branch with link register (BLR) instruction (AArch64 encoding)
%% Register is the register atom (r0-r15)
-spec blr(aarch64_gpr_register()) -> binary().
blr(Reg) when is_atom(Reg) ->
    RegNum = reg_to_num(Reg),
    %% AArch64 BLR encoding: 1101011000111111000000rrrrr00000
    %% 0xd63f0000 | (Reg << 5)
    <<(16#D63F0000 bor (RegNum bsl 5)):32/little>>.

%% Emit a branch register (BR) instruction (AArch64 encoding)
%% Register is the register atom (r0-r15)
-spec br(aarch64_gpr_register()) -> binary().
br(Reg) when is_atom(Reg) ->
    RegNum = reg_to_num(Reg),
    %% AArch64 BR encoding: 1101011000011111000000rrrrr00000
    %% 0xd61f0000 | (Reg << 5)
    <<(16#D61F0000 bor (RegNum bsl 5)):32/little>>.

%% Emit a load register (LDR) instruction for 64-bit load from memory (AArch64 encoding)
%% Dst is destination register atom, Src is {BaseReg, Offset} tuple
-spec ldr(aarch64_gpr_register(), {aarch64_gpr_register(), integer()}) -> binary().
ldr(Dst, {BaseReg, Offset}) when
    is_atom(Dst),
    is_atom(BaseReg),
    is_integer(Offset),
    Offset >= 0,
    Offset =< 32760,
    (Offset rem 8) =:= 0
->
    DstNum = reg_to_num(Dst),
    BaseRegNum = reg_to_num(BaseReg),
    %% AArch64 LDR (immediate) encoding for 64-bit: 11111001010iiiiiiiiiiibbbbbttttt
    %% 0xf9400000 | (Offset div 8) << 10 | BaseReg << 5 | Dst
    <<
        (16#F9400000 bor ((Offset div 8) bsl 10) bor (BaseRegNum bsl 5) bor DstNum):32/little
    >>;
ldr(Xt, {Xn, Xm}) when
    is_atom(Xt),
    is_atom(Xn),
    is_atom(Xm)
->
    ldr(Xt, {Xn, Xm, lsl, 0});
ldr(Xt, {Xn, Xm, lsl, Amount}) when
    is_atom(Xt),
    is_atom(Xn),
    is_atom(Xm),
    Amount =:= 0 orelse Amount =:= 3
->
    XtNum = reg_to_num(Xt),
    XnNum = reg_to_num(Xn),
    XmNum = reg_to_num(Xm),
    S = Amount div 3,
    <<
        (16#F8606800 bor (XmNum bsl 16) bor (S bsl 12) bor (XnNum bsl 5) bor XtNum):32/little
    >>.

%% Emit a load register (LDR) instruction for 32-bit load from memory (AArch64 encoding)
%% Dst is destination register atom, Src is {BaseReg, Offset} tuple
-spec ldr_w(aarch64_gpr_register(), {aarch64_gpr_register(), integer()}) -> binary().
ldr_w(Dst, {BaseReg, Offset}) when
    is_atom(Dst),
    is_atom(BaseReg),
    is_integer(Offset),
    Offset >= 0,
    Offset =< 16380,
    (Offset rem 4) =:= 0
->
    DstNum = reg_to_num(Dst),
    BaseRegNum = reg_to_num(BaseReg),
    <<
        (16#B9400000 bor ((Offset div 4) bsl 10) bor (BaseRegNum bsl 5) bor DstNum):32/little
    >>.

%% Emit a move immediate (MOV) instruction for various immediate sizes (AArch64 encoding)
%% Dst is destination register atom, Imm is immediate value
%% Returns a binary that may contain multiple instructions for complex immediates
-spec mov(aarch64_gpr_register(), integer() | aarch64_gpr_register()) -> binary().
mov(Dst, Imm) when is_atom(Dst), is_integer(Imm) ->
    mov_immediate(Dst, Imm);
mov(Rd, Rm) when is_atom(Rd), is_atom(Rm) ->
    orr(Rd, xzr, Rm).

%% Helper function to encode immediate values using optimal instruction sequence
-spec mov_immediate(aarch64_gpr_register(), integer()) -> binary().
mov_immediate(Dst, Imm) when Imm >= 0, Imm =< 16#FFFF ->
    %% Simple 16-bit positive immediate
    movz(Dst, Imm, 0);
mov_immediate(Dst, Imm) when Imm < 0, (-Imm - 1) =< 16#FFFF ->
    %% Simple 16-bit negative immediate using MOVN
    %% MOVN encodes ~immediate, so we can use it when ~Imm fits in 16 bits
    DstNum = reg_to_num(Dst),
    <<(16#92800000 bor (((-Imm - 1) band 16#FFFF) bsl 5) bor DstNum):32/little>>;
mov_immediate(Dst, Imm) when Imm >= 0 ->
    %% Complex positive immediate - build with MOVZ + MOVK sequence
    build_positive_immediate(Dst, <<Imm:64>>);
mov_immediate(Dst, Imm) when Imm < 0 ->
    %% Complex negative immediate - try MOVN approach first
    build_negative_immediate(Dst, <<Imm:64>>).

%% Build positive immediate using MOVZ + MOVK sequence
-spec build_positive_immediate(aarch64_gpr_register(), binary()) -> binary().
build_positive_immediate(Dst, <<Imm4:16, Imm3:16, Imm2:16, Imm1:16>> = ImmB) ->
    %% First try simple MOVZ/MOVK sequence for values with few non-zero chunks
    Chunks = [
        Imm1,
        Imm2,
        Imm3,
        Imm4
    ],
    NonZeroChunks = length([C || C <- Chunks, C =/= 0]),

    if
        NonZeroChunks =< 2 ->
            %% Use simple MOVZ/MOVK sequence for 1-2 chunks
            build_immediate_sequence(Dst, Chunks);
        true ->
            %% For complex values, try bitmask immediate first
            case encode_bitmask_immediate(ImmB) of
                {ok, N, Immr, Imms} ->
                    %% Use ORR immediate (MOV Rd, #imm is ORR Rd, XZR, #imm)
                    orr_immediate(Dst, N, Immr, Imms);
                error ->
                    %% Fallback to multi-instruction sequence
                    build_immediate_sequence(Dst, Chunks)
            end
    end.

%% Build negative immediate using MOVN or fallback to positive approach
-spec build_negative_immediate(aarch64_gpr_register(), binary()) -> binary().
build_negative_immediate(Dst, ImmB) ->
    %% First try to encode as bitmask immediate with ORR
    case encode_bitmask_immediate(ImmB) of
        {ok, N, Immr, Imms} ->
            %% Use ORR immediate (MOV Rd, #imm is ORR Rd, XZR, #imm)
            orr_immediate(Dst, N, Immr, Imms);
        error ->
            %% Fallback to multi-instruction sequence
            build_positive_immediate(Dst, ImmB)
    end.

%% Build instruction sequence from chunks
-spec build_immediate_sequence(aarch64_gpr_register(), [integer()]) -> binary().
build_immediate_sequence(Dst, [C0, C1, C2, C3]) ->
    %% Find the first non-zero chunk to start with MOVZ
    {Index, Value} = find_first_nonzero_chunk([C0, C1, C2, C3]),
    First = movz(Dst, Value, Index * 16),
    Rest = build_movk_sequence(Dst, [C0, C1, C2, C3], Index),
    <<First/binary, Rest/binary>>.

%% Find the first non-zero chunk
-spec find_first_nonzero_chunk([integer()]) -> {integer(), integer()} | none.
find_first_nonzero_chunk(Chunks) ->
    find_first_nonzero_chunk(Chunks, 0).

find_first_nonzero_chunk([0 | Rest], Index) -> find_first_nonzero_chunk(Rest, Index + 1);
find_first_nonzero_chunk([Chunk | _], Index) -> {Index, Chunk}.

%% Build MOVK sequence for remaining non-zero chunks
-spec build_movk_sequence(aarch64_gpr_register(), [integer()], integer()) -> binary().
build_movk_sequence(Dst, Chunks, SkipIndex) ->
    build_movk_sequence(Dst, Chunks, SkipIndex, 0, <<>>).

build_movk_sequence(_, [], _, _, Acc) ->
    Acc;
build_movk_sequence(Dst, [Chunk | Rest], SkipIndex, CurrentIndex, Acc) ->
    NewAcc =
        if
            CurrentIndex =:= SkipIndex orelse Chunk =:= 0 ->
                Acc;
            true ->
                MovkInstr = movk(Dst, Chunk, CurrentIndex * 16),
                <<Acc/binary, MovkInstr/binary>>
        end,
    build_movk_sequence(Dst, Rest, SkipIndex, CurrentIndex + 1, NewAcc).

%% Emit a MOVZ instruction (move with zero)
-spec movz(aarch64_gpr_register(), integer(), integer()) -> binary().
movz(Dst, Imm, Shift) when
    is_atom(Dst),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 16#FFFF,
    Shift rem 16 =:= 0,
    Shift >= 0,
    Shift =< 48
->
    DstNum = reg_to_num(Dst),
    Hw = Shift div 16,
    %% AArch64 MOVZ encoding: 1101001000hwiiiiiiiiiiiiiiiiibbbbb
    <<(16#D2800000 bor (Hw bsl 21) bor ((Imm band 16#FFFF) bsl 5) bor DstNum):32/little>>.

%% Emit a MOVK instruction (move with keep)
-spec movk(aarch64_gpr_register(), integer(), integer()) -> binary().
movk(Dst, Imm, Shift) when
    is_atom(Dst),
    is_integer(Imm),
    Imm >= 0,
    Imm =< 16#FFFF,
    Shift rem 16 =:= 0,
    Shift >= 0,
    Shift =< 48
->
    DstNum = reg_to_num(Dst),
    Hw = Shift div 16,
    %% AArch64 MOVK encoding: 1111001000hwiiiiiiiiiiiiiiiiibbbbb
    <<(16#F2800000 bor (Hw bsl 21) bor ((Imm band 16#FFFF) bsl 5) bor DstNum):32/little>>.

%% Emit an ORR immediate instruction (used for MOV with bitmask immediates)
-spec orr_immediate(aarch64_gpr_register(), integer(), integer(), integer()) -> binary().
orr_immediate(Dst, N, Immr, Imms) when
    is_atom(Dst),
    N >= 0,
    N =< 1,
    Immr >= 0,
    Immr =< 63,
    Imms >= 0,
    Imms =< 63
->
    DstNum = reg_to_num(Dst),
    %% AArch64 ORR (immediate) encoding: sf 01 100100 N immr imms Rn Rd
    %% For MOV Rd, #imm: ORR Rd, XZR, #imm (Rn = 31)

    % 64-bit operation
    Sf = 1,
    <<
        ((Sf bsl 31) bor (16#B2000000) bor (N bsl 22) bor (Immr bsl 16) bor (Imms bsl 10) bor
            (31 bsl 5) bor DstNum):32/little
    >>.

%% Encode a value as AArch64 bitmask immediate
%% Returns {ok, N, Immr, Imms} if encodable, error otherwise
-spec encode_bitmask_immediate(binary()) -> {ok, 0..1, integer(), integer()} | error.
encode_bitmask_immediate(Value) when byte_size(Value) =:= 8 ->
    %% Try different pattern sizes (64, 32, 16, 8, 4, 2)
    PatternSizes = [64, 32, 16, 8, 4, 2],
    try_pattern_sizes(Value, PatternSizes).

%% Encode a value as AArch64 bitmask immediate for 32 bits values
%% Returns {ok, Immr, Imms} if encodable, error otherwise
-spec encode_bitmask_immediate_w(binary()) -> {ok, integer(), integer()} | error.
encode_bitmask_immediate_w(Value) when byte_size(Value) =:= 4 ->
    %% Try different pattern sizes (32, 16, 8, 4, 2)
    PatternSizes = [32, 16, 8, 4, 2],
    case try_pattern_sizes(Value, PatternSizes) of
        {ok, 0, Immr, Imms} -> {ok, Immr, Imms};
        error -> error
    end.

%% Try encoding with different pattern sizes
-spec try_pattern_sizes(binary(), [integer()]) -> {ok, integer(), integer(), integer()} | error.
try_pattern_sizes(_, []) ->
    error;
try_pattern_sizes(Value, [Size | Rest]) ->
    case try_encode_pattern_size(Value, Size) of
        {ok, N, Immr, Imms} -> {ok, N, Immr, Imms};
        error -> try_pattern_sizes(Value, Rest)
    end.

%% Try to encode value with a specific pattern size
-spec try_encode_pattern_size(binary(), integer()) ->
    {ok, integer(), integer(), integer()} | error.
try_encode_pattern_size(Value, Size) ->
    <<Rest:(byte_size(Value) * 8 - Size), Pattern:Size>> = Value,
    if
        Value =:= <<Pattern:Size, Rest:(byte_size(Value) * 8 - Size)>> ->
            try_encode_single_pattern(Pattern, Size);
        true ->
            error
    end.

%% Try to encode a single pattern as bitmask immediate
-spec try_encode_single_pattern(integer(), integer()) ->
    {ok, integer(), integer(), integer()} | error.
try_encode_single_pattern(Pattern, Size) ->
    %% Find runs of consecutive 1s and 0s
    case find_single_run_of_ones(Pattern, Size) of
        {ok, OnesCount, StartPos} ->
            %% AArch64 cannot encode all-ones patterns as bitmask immediates
            %% (all-zeros are already filtered by find_single_run_of_ones)
            if
                OnesCount =:= Size ->
                    error;
                true ->
                    %% Calculate N, Immr, Imms
                    N =
                        case Size of
                            64 -> 1;
                            32 -> 0;
                            16 -> 0;
                            8 -> 0;
                            4 -> 0;
                            2 -> 0
                        end,

                    %% For N=0 patterns, we need to encode the size in imms
                    Imms =
                        case Size of
                            64 -> OnesCount - 1;
                            32 -> OnesCount - 1;
                            16 -> 2#100000 bor (OnesCount - 1);
                            8 -> 2#110000 bor (OnesCount - 1);
                            4 -> 2#111000 bor (OnesCount - 1);
                            2 -> 2#111100 bor (OnesCount - 1)
                        end,
                    %% immr is the rotation amount (negate of start position)
                    Immr = (-StartPos) band (Size - 1),

                    {ok, N, Immr, Imms}
            end;
        error ->
            error
    end.

%% Find a single run of consecutive 1s in the pattern
-spec find_single_run_of_ones(integer(), integer()) -> {ok, integer(), integer()} | error.
find_single_run_of_ones(Pattern, Size) ->
    %% Convert to binary string for easier analysis
    Bits = [(Pattern bsr I) band 1 || I <- lists:seq(0, Size - 1)],
    find_ones_run(Bits, 0, 0, 0, none).

find_ones_run([], _, OnesCount, StartPos, in_ones) when OnesCount > 0 ->
    %% Reached end while in ones run
    {ok, OnesCount, StartPos};
find_ones_run([], _, _, _, none) ->
    %% Reached end without finding any ones (all zeros pattern)
    error;
find_ones_run([1 | Rest], Pos, 0, _, none) ->
    %% Start of ones run
    find_ones_run(Rest, Pos + 1, 1, Pos, in_ones);
find_ones_run([1 | Rest], Pos, OnesCount, StartPos, in_ones) ->
    %% Continue ones run
    find_ones_run(Rest, Pos + 1, OnesCount + 1, StartPos, in_ones);
find_ones_run([0 | Rest], _Pos, OnesCount, StartPos, in_ones) ->
    %% End of ones run - make sure rest are zeros (single run only)
    case lists:all(fun(X) -> X =:= 0 end, Rest) of
        true -> {ok, OnesCount, StartPos};
        %% Multiple runs not supported in simple encoding
        false -> error
    end;
find_ones_run([0 | Rest], Pos, OnesCount, StartPos, none) ->
    %% Still looking for start of ones run
    find_ones_run(Rest, Pos + 1, OnesCount, StartPos, none).

%% Emit an ORR instruction (AArch64 encoding)
%% ORR Rd, Rn, Rm - performs bitwise OR of Rn and Rm, storing result in Rd
%% Special cases: ORR Rd, XZR, Rm is equivalent to MOV Rd, Rm
-spec orr(aarch64_gpr_register(), aarch64_gpr_register() | xzr, aarch64_gpr_register()) -> binary().
orr(DstReg, xzr, SrcReg) when is_atom(DstReg), is_atom(SrcReg) ->
    %% ORR Rd, XZR, Rm - equivalent to MOV Rd, Rm
    SrcNum = reg_to_num(SrcReg),
    DstNum = reg_to_num(DstReg),
    %% AArch64 ORR (shifted register) encoding: Rd = Rm (with XZR as Rn)
    %% 10101010000mmmmm000000nnnnndddddd (64-bit)
    %% 0xaa000000 | Rm << 16 | Rn << 5 | Rd (where Rn = 31 for XZR)
    <<(16#AA0003E0 bor (SrcNum bsl 16) bor DstNum):32/little>>;
orr(DstReg, Rn, Rm) when is_atom(DstReg), is_atom(Rn), is_atom(Rm) ->
    %% General ORR Rd, Rn, Rm
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    DstNum = reg_to_num(DstReg),
    %% AArch64 ORR (shifted register) encoding:
    %% 10101010000mmmmm000000nnnnndddddd (64-bit)
    <<
        (16#AA000000 bor (RmNum bsl 16) bor (RnNum bsl 5) bor DstNum):32/little
    >>;
orr(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm) ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    case encode_bitmask_immediate(<<Imm:64>>) of
        {ok, N, Immr, Imms} ->
            % OR immediate encoding: sf=1(64b) 01(op) 100100 N immr imms Rn Rd
            Opcode = 16#B2000000,
            Instr =
                Opcode bor (N bsl 22) bor (Immr bsl 16) bor (Imms bsl 10) bor (RnNum bsl 5) bor
                    RdNum,
            <<Instr:32/little>>;
        error ->
            error({unencodable_immediate, Imm})
    end.

%% Emit a store register (STR) instruction for 64-bit store to memory
-spec str(aarch64_gpr_register(), {aarch64_gpr_register(), integer()}) -> binary().
str(SrcReg, {BaseReg, Offset}) when
    is_atom(SrcReg),
    is_atom(BaseReg),
    is_integer(Offset),
    Offset >= 0,
    Offset =< 32760,
    (Offset rem 8) =:= 0
->
    SrcNum = reg_to_num(SrcReg),
    BaseNum = reg_to_num(BaseReg),
    %% AArch64 STR (immediate) encoding for 64-bit: 11111001000iiiiiiiiiiibbbbbttttt
    %% 0xf9000000 | (Offset div 8) << 10 | BaseReg << 5 | SrcReg
    <<
        (16#F9000000 bor ((Offset div 8) bsl 10) bor (BaseNum bsl 5) bor SrcNum):32/little
    >>;
str(Xt, {Xn, Xm, lsl, Amount}) when
    is_atom(Xt),
    is_atom(Xn),
    is_atom(Xm),
    Amount =:= 0 orelse Amount =:= 3
->
    XtNum = reg_to_num(Xt),
    XnNum = reg_to_num(Xn),
    XmNum = reg_to_num(Xm),
    S = Amount div 3,
    <<
        (16#F8206800 bor (XmNum bsl 16) bor (S bsl 12) bor (XnNum bsl 5) bor XtNum):32/little
    >>.

%% Emit a store register (STR) instruction for 64-bit store to memory, with store-update (writeback)
-spec str
    (aarch64_gpr_register(), {aarch64_gpr_register(), integer()}, '!') -> binary();
    (aarch64_gpr_register(), {aarch64_gpr_register()}, integer()) -> binary().
str(Reg, {Base, Imm}, '!') when
    is_atom(Reg), is_atom(Base), is_integer(Imm), Imm >= -256, Imm < 256, (Imm rem 8) =:= 0
->
    RegNum = reg_to_num(Reg),
    BaseNum = reg_to_num(Base),
    <<(16#F8000C00 bor ((Imm band 16#1FF) bsl 12) bor (BaseNum bsl 5) bor RegNum):32/little>>;
str(Reg, {Base}, Imm) when
    is_atom(Reg), is_atom(Base), is_integer(Imm), Imm >= -256, Imm < 256, (Imm rem 8) =:= 0
->
    RegNum = reg_to_num(Reg),
    BaseNum = reg_to_num(Base),
    <<(16#F8000400 bor ((Imm band 16#1FF) bsl 12) bor (BaseNum bsl 5) bor RegNum):32/little>>.

%% Emit a store register (STR) instruction for 32-bit store to memory
-spec str_w(aarch64_gpr_register(), {aarch64_gpr_register(), integer()}) -> binary().
str_w(Src, {BaseReg, Offset}) when
    is_atom(Src),
    is_atom(BaseReg),
    is_integer(Offset),
    Offset >= 0,
    Offset =< 16380,
    (Offset rem 4) =:= 0
->
    SrcNum = reg_to_num(Src),
    BaseRegNum = reg_to_num(BaseReg),
    <<
        (16#B9000000 bor ((Offset div 4) bsl 10) bor (BaseRegNum bsl 5) bor SrcNum):32/little
    >>.

%% Emit a load register (LDR) instruction for 64-bit store to memory, with store-update (writeback)
-spec ldr
    (aarch64_gpr_register(), {aarch64_gpr_register(), integer()}, '!') -> binary();
    (aarch64_gpr_register(), {aarch64_gpr_register()}, integer()) -> binary().
ldr(Reg, {Base, Imm}, '!') when
    is_atom(Reg), is_atom(Base), is_integer(Imm), Imm >= -256, Imm < 256, (Imm rem 8) =:= 0
->
    RegNum = reg_to_num(Reg),
    BaseNum = reg_to_num(Base),
    <<(16#F8400C00 bor ((Imm band 16#1FF) bsl 12) bor (BaseNum bsl 5) bor RegNum):32/little>>;
ldr(Reg, {Base}, Imm) when
    is_atom(Reg), is_atom(Base), is_integer(Imm), Imm >= -256, Imm < 256, (Imm rem 8) =:= 0
->
    RegNum = reg_to_num(Reg),
    BaseNum = reg_to_num(Base),
    <<(16#F8400400 bor ((Imm band 16#1FF) bsl 12) bor (BaseNum bsl 5) bor RegNum):32/little>>.

%% Emit a store pair (STP) instruction for 64-bit registers
%% stp(Rn, Rm, {Base}, Imm) -> binary()
%% stp(Rn, Rm, {Base, Imm}, '!') -> binary() (store-update)
-spec stp(
    aarch64_gpr_register(),
    aarch64_gpr_register(),
    {aarch64_gpr_register()} | {aarch64_gpr_register(), integer()},
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
-spec ldp(aarch64_gpr_register(), aarch64_gpr_register(), {aarch64_gpr_register()}, integer()) ->
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
reg_to_num(r16) -> 16;
reg_to_num(r17) -> 17;
reg_to_num(r18) -> 18;
reg_to_num(r19) -> 19;
reg_to_num(r20) -> 20;
reg_to_num(r21) -> 21;
reg_to_num(r22) -> 22;
reg_to_num(r23) -> 23;
reg_to_num(r24) -> 24;
reg_to_num(r25) -> 25;
reg_to_num(r26) -> 26;
reg_to_num(r27) -> 27;
reg_to_num(r28) -> 28;
reg_to_num(r29) -> 29;
reg_to_num(r30) -> 30;
%% Stack pointer (SP) is r31
reg_to_num(sp) -> 31;
%% Zero register (XZR) is also r31
reg_to_num(xzr) -> 31.

%% Emit a conditional branch instruction
-spec bcc(cc(), integer()) -> binary().
bcc(Cond, Offset) when is_atom(Cond), is_integer(Offset) ->
    CondNum =
        case Cond of
            % Equal (Z set)
            eq -> 0;
            % Not equal (Z clear)
            ne -> 1;
            % Carry set
            cs -> 2;
            % Carry clear
            cc -> 3;
            % Minus (N set)
            mi -> 4;
            % Plus (N clear)
            pl -> 5;
            % Overflow set
            vs -> 6;
            % Overflow clear
            vc -> 7;
            % Higher (unsigned)
            hi -> 8;
            % Lower or same (unsigned)
            ls -> 9;
            % Greater than or equal (signed)
            ge -> 10;
            % Less than (signed)
            lt -> 11;
            % Greater than (signed)
            gt -> 12;
            % Less than or equal (signed)
            le -> 13;
            % Always
            al -> 14;
            % Never
            nv -> 15
        end,
    Offset19 = Offset div 4,
    <<(16#54000000 bor ((Offset19 band 16#7FFFF) bsl 5) bor CondNum):32/little>>.

%% Emit a compare and branch on non-zero
-spec cbnz(aarch64_gpr_register(), integer()) -> binary().
cbnz(Rt, Offset) when is_integer(Offset) ->
    RtNum = reg_to_num(Rt),
    Offset19 = Offset div 4,
    <<(16#B5000000 bor ((Offset19 band 16#7FFFF) bsl 5) bor RtNum):32/little>>.

-spec cbnz_w(aarch64_gpr_register(), integer()) -> binary().
cbnz_w(Rt, Offset) when is_integer(Offset) ->
    RtNum = reg_to_num(Rt),
    Offset19 = Offset div 4,
    <<(16#35000000 bor ((Offset19 band 16#7FFFF) bsl 5) bor RtNum):32/little>>.

%% Emit a test bit and branch if zero
-spec tbz(aarch64_gpr_register(), 0..63, integer()) -> binary().
tbz(Rt, Bit, Offset) when Offset >= -32768 andalso Offset < 32768 ->
    RtNum = reg_to_num(Rt),
    Offset14 = Offset div 4,
    <<
        ((Bit band 32 bsl 26) bor 16#36000000 bor (Bit band 31 bsl 19) bor
            ((Offset14 band 16#3FFF) bsl 5) bor RtNum):32/little
    >>.

%% Emit a test bit and branch if not zero
-spec tbnz(aarch64_gpr_register(), 0..63, integer()) -> binary().
tbnz(Rt, Bit, Offset) when Offset >= -32768 andalso Offset < 32768 ->
    RtNum = reg_to_num(Rt),
    Offset14 = Offset div 4,
    <<
        ((Bit band 32 bsl 26) bor 16#37000000 bor (Bit band 31 bsl 19) bor
            ((Offset14 band 16#3FFF) bsl 5) bor RtNum):32/little
    >>.

%% Emit a compare instruction
-spec cmp(aarch64_gpr_register(), aarch64_gpr_register() | integer()) -> binary().
cmp(Rn, Rm) when is_atom(Rn), is_atom(Rm) ->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 CMP (shifted register) encoding: CMP Rn, Rm
    %% This is SUBS XZR, Rn, Rm: 11101011000mmmmm000000nnnnn11111
    <<(16#EB00001F bor (RmNum bsl 16) bor (RnNum bsl 5)):32/little>>;
cmp(Rn, Imm) when is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 4095 ->
    RnNum = reg_to_num(Rn),
    %% AArch64 CMP (immediate) encoding: CMP Rn, #imm
    %% This is SUBS XZR, Rn, #imm: 1111000100iiiiiiiiiiiinnnnn11111
    <<(16#F100001F bor ((Imm band 16#FFF) bsl 10) bor (RnNum bsl 5)):32/little>>;
cmp(Rn, Imm) when is_atom(Rn), is_integer(Imm) ->
    %% For large immediates, load into a temporary register and compare
    %% Use r16 as temporary register (caller-saved)
    TempReg = r16,
    LoadInstr = mov_immediate(TempReg, Imm),
    CmpInstr = cmp(Rn, TempReg),
    <<LoadInstr/binary, CmpInstr/binary>>.

%% Emit a 32-bit compare instruction
-spec cmp_w(aarch64_gpr_register(), aarch64_gpr_register() | integer()) -> binary().
cmp_w(Rn, Imm) when is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 4095 ->
    RnNum = reg_to_num(Rn),
    %% AArch64 CMP (32-bit immediate) encoding: CMP Wn, #imm
    %% This is SUBS WZR, Wn, #imm: 0111000100iiiiiiiiiiiinnnnn11111
    <<(16#7100001F bor ((Imm band 16#FFF) bsl 10) bor (RnNum bsl 5)):32/little>>;
cmp_w(Rn, Imm) when is_atom(Rn), is_integer(Imm), Imm < 0, Imm >= -4095 ->
    RnNum = reg_to_num(Rn),
    %% For negative immediates, use ADD form: CMP Wn, #(-imm) becomes ADDS WZR, Wn, #(-imm)
    %% AArch64 ADDS (32-bit immediate) encoding: 0011000100iiiiiiiiiiiinnnnn11111
    PosImm = -Imm,
    <<(16#3100001F bor ((PosImm band 16#FFF) bsl 10) bor (RnNum bsl 5)):32/little>>.

%% Emit an AND instruction (bitwise AND)
-spec and_(aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register() | integer()) ->
    binary().
and_(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 AND (shifted register) encoding: AND Rd, Rn, Rm
    %% 10001010000mmmmm000000nnnnndddddd (64-bit)
    <<
        (16#8A000000 bor (RmNum bsl 16) bor (RnNum bsl 5) bor RdNum):32/little
    >>;
and_(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm) ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    case encode_bitmask_immediate(<<Imm:64>>) of
        {ok, N, Immr, Imms} ->
            % AND immediate encoding: sf=1(64b) 00(op) 100100 N immr imms Rn Rd
            Opcode = 16#92000000,
            Instr =
                Opcode bor (N bsl 22) bor (Immr bsl 16) bor (Imms bsl 10) bor (RnNum bsl 5) bor
                    RdNum,
            <<Instr:32/little>>;
        error ->
            error({unencodable_immediate, Imm})
    end.

%% Emit a logical shift left instruction
-spec lsl(aarch64_gpr_register(), aarch64_gpr_register(), integer()) -> binary().
lsl(Rd, Rn, Shift) when is_atom(Rd), is_atom(Rn), is_integer(Shift), Shift >= 0, Shift =< 63 ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    %% AArch64 LSL (immediate) encoding: LSL Rd, Rn, #shift
    %% This is UBFM Rd, Rn, #(-shift MOD 64), #(63-shift): 1101001101ssssssrrrrrrnnnnndddddd
    NegShift = (-Shift) band 63,
    Width = 63 - Shift,
    <<
        (16#D3400000 bor ((NegShift band 16#3F) bsl 16) bor ((Width band 16#3F) bsl 10) bor
            (RnNum bsl 5) bor RdNum):32/little
    >>.

%% Emit a logical shift right instruction
-spec lsr(aarch64_gpr_register(), aarch64_gpr_register(), integer()) -> binary().
lsr(Rd, Rn, Shift) when is_atom(Rd), is_atom(Rn), is_integer(Shift), Shift >= 0, Shift =< 63 ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    %% AArch64 LSR (immediate) encoding: LSR Rd, Rn, #shift
    %% This is UBFM Rd, Rn, #shift, #63: 1101001101ssssss111111nnnnndddddd
    <<
        (16#D340FC00 bor ((Shift band 16#3F) bsl 16) bor (RnNum bsl 5) bor RdNum):32/little
    >>.

%% Emit a return instruction
-spec ret() -> binary().
ret() ->
    %% AArch64 RET encoding: RET (defaults to X30/LR)
    %% 11010110010111110000001111000000
    <<16#D65F03C0:32/little>>.

%% Emit a NOP instruction
-spec nop() -> binary().
nop() ->
    %% 11010101000000110010000000011111
    <<16#d503201f:32/little>>.

%% Emit a test instruction (bitwise AND, discarding result)
-spec tst(aarch64_gpr_register(), aarch64_gpr_register() | integer()) -> binary().
tst(Rn, Rm) when is_atom(Rn), is_atom(Rm) ->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 TST (shifted register) encoding: TST Rn, Rm
    %% This is ANDS XZR, Rn, Rm: 11101010000mmmmm000000nnnnn11111
    <<(16#EA00001F bor (RmNum bsl 16) bor (RnNum bsl 5)):32/little>>;
tst(Rn, Imm) when is_atom(Rn), is_integer(Imm) ->
    RnNum = reg_to_num(Rn),
    case encode_bitmask_immediate(<<Imm:64>>) of
        {ok, N, Immr, Imms} ->
            <<
                (16#F200001F bor (N bsl 22) bor (Immr bsl 16) bor (Imms bsl 10) bor (RnNum bsl 5)):32/little
            >>;
        _ ->
            error({unencodable_immediate, Imm})
    end.

%% Emit a 32-bit test instruction (bitwise AND, discarding result)
-spec tst_w(aarch64_gpr_register(), aarch64_gpr_register() | integer()) -> binary().
tst_w(Rn, Rm) when is_atom(Rn), is_atom(Rm) ->
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 TST (32-bit shifted register) encoding: TST Wn, Wm
    %% This is ANDS WZR, Wn, Wm: 01101010000mmmmm000000nnnnn11111
    <<(16#6A00001F bor (RmNum bsl 16) bor (RnNum bsl 5)):32/little>>;
tst_w(Rn, Imm) when is_atom(Rn), is_integer(Imm) ->
    RnNum = reg_to_num(Rn),
    case encode_bitmask_immediate_w(<<Imm:32>>) of
        {ok, Immr, Imms} ->
            <<(16#7200001F bor (Immr bsl 16) bor (Imms bsl 10) bor (RnNum bsl 5)):32/little>>;
        _ ->
            error({unencodable_immediate, Imm})
    end.

%% Emit a subtract and set flags (SUBS) instruction (AArch64 encoding)
%% SUBS Rd, Rn, Rm/imm - subtracts and sets condition flags
-spec subs(aarch64_gpr_register(), aarch64_gpr_register(), integer() | aarch64_gpr_register()) ->
    binary().
subs(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 4095 ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    %% AArch64 SUBS (immediate): 1111000101iiiiiiiiiiiinnnnndddddd
    <<(16#F1000000 bor ((Imm band 16#FFF) bsl 10) bor (RnNum bsl 5) bor RdNum):32/little>>;
subs(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    %% AArch64 SUBS (register): 11101011000mmmmm000000nnnnndddddd
    <<(16#EB000000 bor (RmNum bsl 16) bor (RnNum bsl 5) bor RdNum):32/little>>.

-spec sub(aarch64_gpr_register(), aarch64_gpr_register(), integer() | aarch64_gpr_register()) ->
    binary().
sub(Rd, Rn, Imm) when is_atom(Rd), is_atom(Rn), is_integer(Imm), Imm >= 0, Imm =< 4095 ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    <<(16#D1000000 bor ((Imm band 16#FFF) bsl 10) bor (RnNum bsl 5) bor RdNum):32/little>>;
sub(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    sub(Rd, Rn, Rm, {lsl, 0}).

-spec sub(aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register(), {lsl, 0..63}) ->
    binary().
sub(Rd, Rn, Rm, {lsl, Amount}) when
    is_atom(Rd), is_atom(Rn), is_atom(Rm), is_integer(Amount), Amount >= 0, Amount =< 63
->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    <<
        (16#CB000000 bor (RmNum bsl 16) bor ((Amount band 16#3F) bsl 10) bor (RnNum bsl 5) bor
            RdNum):32/little
    >>.

%% Emit an ADR (PC-relative address) instruction (AArch64 encoding)
%% Dst is destination register atom, Offset is signed immediate (in bytes, -1MB..+1MB)
-spec adr(aarch64_gpr_register(), integer()) -> binary().
adr(Dst, Imm) when is_atom(Dst), is_integer(Imm), Imm >= -1048576, Imm =< 1048572 ->
    DstNum = reg_to_num(Dst),
    ImmLo = Imm band 3,
    ImmHi = Imm bsr 2,
    Word = (16#10000000) bor (ImmLo bsl 29) bor ((ImmHi band 16#7FFFF) bsl 5) bor DstNum,
    <<Word:32/little>>.

-spec mul(aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register()) -> binary().
mul(Rd, Rn, Rm) when is_atom(Rd), is_atom(Rn), is_atom(Rm) ->
    madd(Rd, Rn, Rm, xzr).

-spec madd(
    aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register(), aarch64_gpr_register()
) -> binary().
madd(Rd, Rn, Rm, Ra) when is_atom(Rd), is_atom(Rn), is_atom(Rm), is_atom(Ra) ->
    RdNum = reg_to_num(Rd),
    RnNum = reg_to_num(Rn),
    RmNum = reg_to_num(Rm),
    RaNum = reg_to_num(Ra),
    <<
        (16#9B000000 bor (RmNum bsl 16) bor (RaNum bsl 10) bor (RnNum bsl 5) bor
            RdNum):32/little
    >>.
