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

%% Shared implementation for RISC-V JIT backends (riscv32 and riscv64).
%%
%% Before including this file, the backend module must define:
%%   ?ASM - assembler module (jit_riscv32_asm or jit_riscv64_asm)
%%   ?WORD_SIZE_BYTES - 4 or 8
%%   ?BOOL_SHIFT - 31 or 63
%%   ?Y_OFFSET_LIMIT - 124 or 248
%%   ?PRIMITIVE_DIRECT_LOAD_LIMIT - 124 or -1 (direct offset limit in load_primitive_ptr)
%%   ?FLOAT_DATA_OFFSET - 4 or 8
%%   ?IS_SIGNED_OR_UNSIGNED_WORD(X) - integer range guard
%%   ?LOAD_WORD(Dst, Base, Off) - word-sized load (lw or ld)
%%   ?STORE_WORD(Base, Src, Off) - word-sized store (sw or sd)
%%   ?DWARF_CTX_REG - DWARF register constant
%%   ?ARRAY_OFFSET_FOLD_GUARD(Offset) - guard for move_to_array_element/5 integer fold
%%   ?ARRAY_OFFSET_FOLD(IndexReg, Offset) - fold offset into index for move_to_array_element/5
%%
%% The backend module must also define these callback functions:
%%   handle_avm_int64_t/7 - handles {avm_int64_t, Value} in set_registers_args0
%%   parameter_regs0_avm_int64_t/3 - handles {avm_int64_t, _} in parameter_regs0

%%-----------------------------------------------------------------------------
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        labels = [],
        variant = Variant,
        regs = jit_regs:new()
    }.

%%-----------------------------------------------------------------------------
%% @doc Access the stream object.
%% @end
%% @param State current backend state
%% @return The stream object
%%-----------------------------------------------------------------------------
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @doc Get the current offset in the stream
%% @end
%% @param State current backend state
%% @return The current offset
%%-----------------------------------------------------------------------------
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @doc Flush the stream.
%% @end
%% @param State current backend state
%% @return The new state
%%-----------------------------------------------------------------------------
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger or breakpoint instruction. This is used for debugging
%% and not in production.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, ?ASM:c_ebreak()),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
used_regs(#state{used_regs = Used}) -> mask_to_list(Used).

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
available_regs(#state{available_regs = Available}) -> mask_to_list(Available).

%%-----------------------------------------------------------------------------
%% @doc Free native registers. The passed list of registers can contain
%% registers, pointer to registers or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Regs list of registers or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

free_native_register(
    #state{available_regs = Available0, used_regs = Used0} = State,
    Reg
) when is_atom(Reg) ->
    Bit = reg_bit(Reg),
    State#state{
        available_regs = Available0 bor Bit, used_regs = Used0 band (bnot Bit)
    };
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Assert that all native scratch registers are available. This is used
%% for debugging and not in production.
%% @end
%% @param State current backend state
%% @return ok
%%-----------------------------------------------------------------------------
assert_all_native_free(State) ->
    0 = State#state.used_regs,
    ?AVAILABLE_REGS_MASK = State#state.available_regs,
    ok.

%%-----------------------------------------------------------------------------
%% @doc Emit the jump table at the beginning of the module. Branches will be
%% updated afterwards with update_branches/2. Emit branches for labels from
%% 0 (special entry for lines and labels information) to LabelsCount included
%% (special entry for OP_INT_CALL_END).
%%
%% On this platform, each jump table entry is 8 bytes (AUIPC + JALR).
%% The entries are patched by add_label when label offsets are known.
%%
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    JumpTableStart = StreamModule:offset(Stream0),
    jump_table0(State#state{jump_table_start = JumpTableStart}, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    % Create jump table entry: AUIPC + JALR (8 bytes total)
    % This will be patched in add_label when the label offset is known
    JumpEntry = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    Stream1 = StreamModule:append(Stream0, JumpEntry),
    jump_table0(State#state{stream = Stream1}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Patch a single branch in the stream
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param Offset offset of the branch to patch
%% @param Type type of the branch
%% @param LabelOffset target label offset
%% @return Updated stream
%%-----------------------------------------------------------------------------
patch_branch(StreamModule, Stream, Offset, Type, LabelOffset) ->
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            {adr, Reg} when Rel rem 4 =:= 0 ->
                % Generate pc_relative_address and pad to 8 bytes with NOP
                I = pc_relative_address(Reg, Rel),
                case byte_size(I) of
                    4 -> <<I/binary, (?ASM:nop())/binary>>;
                    6 -> <<I/binary, (?ASM:c_nop())/binary>>;
                    8 -> I
                end;
            {adr, Reg} when Rel rem 4 =:= 2; Rel rem 4 =:= -2 ->
                % Handle 2-byte aligned offsets and pad to 8 bytes
                % Handle both positive and negative offsets (Erlang rem can be negative)
                I = pc_relative_address(Reg, Rel),
                case byte_size(I) of
                    4 -> <<I/binary, (?ASM:nop())/binary>>;
                    6 -> <<I/binary, (?ASM:c_nop())/binary>>;
                    8 -> I
                end;
            {far_branch, TempReg} ->
                % Check if branch can now be optimized to near branch
                if
                    Rel >= -1048576 andalso Rel =< 1048574 andalso (Rel rem 2) =:= 0 ->
                        % RISC-V jal has ±1MB range
                        % Optimize to near branch: jal + nops to fill original size
                        DirectBranch = ?ASM:jal(zero, Rel),
                        case byte_size(DirectBranch) of
                            2 ->
                                <<DirectBranch/binary, (?ASM:c_nop())/binary,
                                    (?ASM:nop())/binary>>;
                            4 ->
                                <<DirectBranch/binary, (?ASM:nop())/binary>>
                        end;
                    true ->
                        % Keep far branch sequence: auipc + jalr (PC-relative, 8 bytes)
                        % Split the relative offset into upper 20 bits and lower 12 bits
                        Hi20 = (Rel + 16#800) bsr 12,
                        Lo12 = Rel - (Hi20 bsl 12),
                        I1 = ?ASM:auipc(TempReg, Hi20),
                        I2 = ?ASM:jalr(zero, TempReg, Lo12),
                        Entry = <<I1/binary, I2/binary>>,
                        case byte_size(Entry) of
                            6 -> <<Entry/binary, (?ASM:c_nop())/binary>>;
                            8 -> Entry
                        end
                end
        end,
    StreamModule:replace(Stream, Offset, NewInstr).

%%-----------------------------------------------------------------------------
%% @doc Patch all branches targeting a specific label and return remaining branches
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param TargetLabel label to patch branches for
%% @param LabelOffset offset of the target label
%% @param Branches list of pending branches
%% @return {UpdatedStream, RemainingBranches}
%%-----------------------------------------------------------------------------
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches) ->
    patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches, []).

patch_branches_for_label(_StreamModule, Stream, _TargetLabel, _LabelOffset, [], Acc) ->
    {Stream, lists:reverse(Acc)};
patch_branches_for_label(
    StreamModule,
    Stream0,
    TargetLabel,
    LabelOffset,
    [{Label, Offset, Type} | Rest],
    Acc
) when Label =:= TargetLabel ->
    Stream1 = patch_branch(StreamModule, Stream0, Offset, Type, LabelOffset),
    patch_branches_for_label(StreamModule, Stream1, TargetLabel, LabelOffset, Rest, Acc);
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, [Branch | Rest], Acc) ->
    patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Rest, [Branch | Acc]).

%%-----------------------------------------------------------------------------
%% @doc Rewrite stream to update all branches for labels.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
update_branches(#state{branches = []} = State) ->
    State;
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = [{Label, Offset, Type} | BranchesT],
        labels = Labels
    } = State
) ->
    {Label, LabelOffset} = lists:keyfind(Label, 1, Labels),
    Stream1 = patch_branch(StreamModule, Stream0, Offset, Type, LabelOffset),
    update_branches(State#state{stream = Stream1, branches = BranchesT}).

%%-----------------------------------------------------------------------------
%% @doc Generate code to load a primitive function pointer into a register
%% @param Primitive index to the primitive to call
%% @param TargetReg register to load the function pointer into
%% @return Binary instruction sequence
%%-----------------------------------------------------------------------------
load_primitive_ptr(Primitive, TargetReg) ->
    case Primitive of
        0 ->
            ?LOAD_WORD(TargetReg, ?NATIVE_INTERFACE_REG, 0);
        N when N * ?WORD_SIZE_BYTES =< ?PRIMITIVE_DIRECT_LOAD_LIMIT ->
            ?LOAD_WORD(TargetReg, ?NATIVE_INTERFACE_REG, N * ?WORD_SIZE_BYTES);
        N when N * ?WORD_SIZE_BYTES < 256 ->
            % Can encode N * WORD_SIZE_BYTES directly in li instruction
            I1 = ?ASM:li(TargetReg, N * ?WORD_SIZE_BYTES),
            I2 = ?ASM:add(TargetReg, TargetReg, ?NATIVE_INTERFACE_REG),
            I3 = ?LOAD_WORD(TargetReg, TargetReg, 0),
            <<I1/binary, I2/binary, I3/binary>>;
        N ->
            % For very large primitive numbers, load N and shift left (multiply by word_size)
            I1 = ?ASM:li(TargetReg, N),
            I2 = ?ASM:slli(TargetReg, TargetReg, (?WORD_SIZE_BYTES bsr 2) + 1),
            I3 = ?ASM:add(TargetReg, TargetReg, ?NATIVE_INTERFACE_REG),
            I4 = ?LOAD_WORD(TargetReg, TargetReg, 0),
            <<I1/binary, I2/binary, I3/binary, I4/binary>>
    end.

%%-----------------------------------------------------------------------------
%% @doc Emit a call (call with return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention. It also saves scratch registers we need to preserve.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        used_regs = Used
    } = State,
    Primitive,
    Args
) when Available =/= 0 ->
    TempReg = first_avail(Available),
    TempBit = reg_bit(TempReg),
    % Load primitive function pointer
    PrepCall = load_primitive_ptr(Primitive, TempReg),
    Stream1 = StreamModule:append(Stream0, PrepCall),
    StateCall = State#state{
        stream = Stream1,
        available_regs = Available band (bnot TempBit),
        used_regs = Used bor TempBit,
        regs = jit_regs:invalidate_reg(State#state.regs, TempReg)
    },
    call_func_ptr(StateCall, {free, TempReg}, Args);
call_primitive(
    #state{available_regs = 0} = State,
    Primitive,
    Args
) ->
    call_func_ptr(State, {primitive, Primitive}, Args).

%%-----------------------------------------------------------------------------
%% @doc Emit a jump (call without return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
call_primitive_last(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    Primitive,
    Args
) ->
    % We need a register for the function pointer that should not be used as a parameter
    % Since we're not returning, we can use all scratch registers except
    % registers used for parameters
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = args_regs(Args),
    ArgsRegsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ScratchMask = ?AVAILABLE_REGS_MASK band (bnot (ArgsRegsMask bor ParamMask)),
    Temp = first_avail(ScratchMask),
    TempBit = reg_bit(Temp),
    AvailableRegs1 = ScratchMask band (bnot TempBit),
    UsedMask = ?AVAILABLE_REGS_MASK band (bnot AvailableRegs1),
    PrepCall = load_primitive_ptr(Primitive, Temp),
    Stream1 = StreamModule:append(Stream0, PrepCall),

    State1 = State0#state{
        stream = Stream1,
        available_regs = AvailableRegs1,
        used_regs = UsedMask,
        regs = jit_regs:invalidate_reg(State0#state.regs, Temp)
    },

    % Preprocess offset special arg
    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),

    % In RISC-V, all up to 8 arguments fit in registers (a0-a7)
    % Always use tail call when calling primitives in tail position
    State4 =
        case Args1 of
            [FirstArg, jit_state | ArgsT] ->
                % Use tail call
                ArgsForTailCall = [FirstArg, jit_state_tail_call | ArgsT],
                State2 = set_registers_args(State1, ArgsForTailCall, 0),
                tail_call_with_jit_state_registers_only(State2, Temp)
        end,
    State4#state{
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State4#state.regs)
    }.

%%-----------------------------------------------------------------------------
%% @doc Tail call to address in register.
%% RA is preserved across regular calls (call_func_ptr saves/restores it),
%% so when the called C primitive returns, it returns to opcodesswitch.h.
%% @end
%% @param State current backend state
%% @param Reg register containing the target address
%% @return Updated backend state
%%-----------------------------------------------------------------------------
tail_call_with_jit_state_registers_only(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    Reg
) ->
    % Jump to address in register (tail call)
    I1 = ?ASM:jr(Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a return of a value if it's not equal to ctx.
%% This logic is used to break out to the scheduler, typically after signal
%% messages have been processed.
%% @end
%% @param State current backend state
%% @param Reg register to compare to (should be {free, Reg} as it's always freed)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg}
) ->
    % RISC-V doesn't have a separate cmp instruction, use beq directly
    I2 =
        case Reg of
            % Return value is already in a0
            a0 -> <<>>;
            % Move to a0 (return register)
            _ -> ?ASM:mv(a0, Reg)
        end,
    I3 = ?ASM:ret(),
    % Branch if equal (skip the return)
    % Offset must account for the beq instruction itself (4 bytes) plus I2 and I3
    I1 = ?ASM:beq(Reg, ?CTX_REG, 4 + byte_size(I2) + byte_size(I3)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    RegBit = reg_bit(Reg),
    Regs1 = jit_regs:invalidate_reg(State#state.regs, Reg),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs0 bor RegBit,
        used_regs = UsedRegs0 band (bnot RegBit),
        regs = Regs1
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump to a label. The offset of the relocation is saved and will
%% be updated with `update_branches/2`.
%% @end
%% @param State current backend state
%% @param Label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, labels = Labels} = State0, Label
) ->
    LabelLookupResult = lists:keyfind(Label, 1, Labels),
    Offset = StreamModule:offset(Stream0),
    {State1, CodeBlock} = branch_to_label_code(State0, Offset, Label, LabelLookupResult),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    %% After unconditional jump, register tracking is dead until next label
    State1#state{stream = Stream1, regs = jit_regs:invalidate_all(State1#state.regs)}.

jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    CodeBlock = branch_to_offset_code(State, Offset, TargetOffset),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State#state{stream = Stream1, regs = jit_regs:invalidate_all(State#state.regs)}.

%%-----------------------------------------------------------------------------
%% @doc Jump to address in continuation pointer register
%% Calculate absolute address and jump to it.
%% @end
%% @param State current backend state
%% @param {free, OffsetReg} register containing the offset value
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available,
        offset = BaseOffset
    } = State0,
    {free, OffsetReg}
) ->
    Temp = first_avail(Available),
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    CurrentStreamOffset = StreamModule:offset(Stream0),
    NetOffset = BaseOffset - CurrentStreamOffset,

    % Get native code base address into temporary register
    I1 = pc_relative_address(Temp, NetOffset),
    % Add target offset to get final absolute address
    I2 = ?ASM:add(Temp, Temp, OffsetReg),
    % Indirect branch to the calculated absolute address
    I3 = ?ASM:jr(Temp),

    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State0#state{stream = Stream1, available_regs = ?AVAILABLE_REGS_MASK, used_regs = 0}.

branch_to_offset_code(_State, Offset, TargetOffset) when
    TargetOffset - Offset =< 2050, TargetOffset - Offset >= -2044
->
    % Near branch: use direct J instruction
    Rel = TargetOffset - Offset,
    ?ASM:j(Rel);
branch_to_offset_code(
    #state{available_regs = Available}, Offset, TargetOffset
) when Available =/= 0 ->
    TempReg = first_avail(Available),
    % Far branch: use auipc + jalr sequence for PC-relative addressing
    % This computes: PC + Immediate and jumps to it

    Rel = TargetOffset - Offset,
    % Split the relative offset into upper 20 bits and lower 12 bits
    % RISC-V PC-relative addressing: target = PC + (imm20 << 12) + sign_extend(imm12)
    % Since jalr's imm12 is sign-extended, if bit 11 of Rel is set,
    % we need to add 0x800 before splitting to compensate
    Hi20 = (Rel + 16#800) bsr 12,
    Lo12Unsigned = Rel band 16#FFF,
    % Convert to signed 12-bit value: if bit 11 is set, subtract 4096
    Lo12 =
        if
            Lo12Unsigned >= 16#800 -> Lo12Unsigned - 16#1000;
            true -> Lo12Unsigned
        end,

    % TempReg = PC + (Hi20 << 12)
    I1 = ?ASM:auipc(TempReg, Hi20),
    % Jump to TempReg + sign_extend(Lo12)
    I2 = ?ASM:jalr(zero, TempReg, Lo12),
    <<I1/binary, I2/binary>>.

branch_to_label_code(State, Offset, Label, {Label, LabelOffset}) ->
    CodeBlock = branch_to_offset_code(State, Offset, LabelOffset),
    {State, CodeBlock};
branch_to_label_code(
    #state{available_regs = Available, branches = Branches} = State0, Offset, Label, false
) when Available =/= 0 ->
    TempReg = first_avail(Available),
    % RISC-V: Far branch sequence using PC-relative auipc + jalr (8 bytes)

    % Placeholder: auipc TempReg, 0
    % Placeholder: jalr zero, TempReg, 0
    CodeBlock = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    % Add relocation entry
    Reloc = {Label, Offset, {far_branch, TempReg}},
    State1 = State0#state{branches = [Reloc | Branches]},
    {State1, CodeBlock};
branch_to_label_code(
    #state{available_regs = 0, branches = Branches} = State0, Offset, Label, false
) ->
    % RISC-V: Use t6 as scratch (caller-saved, safe to clobber)
    % Far branch sequence using PC-relative auipc + jalr (8 bytes)

    % Placeholder: auipc t6, 0
    % Placeholder: jalr zero, t6, 0
    CodeBlock = <<16#FFFFFFFF:32, 16#FFFFFFFF:32>>,
    % Add relocation entry
    Reloc = {Label, Offset, {far_branch, t6}},
    State1 = State0#state{branches = [Reloc | Branches]},
    {State1, CodeBlock};
branch_to_label_code(#state{available_regs = 0}, _Offset, _Label, LabelLookup) ->
    error({no_available_registers, LabelLookup}).

%%-----------------------------------------------------------------------------
%% @doc Emit an if block, i.e. emit a test of a condition and conditionally
%% execute a block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockFn function to emit the block that may be executed
%% @return Updated backend state
%%-----------------------------------------------------------------------------
if_block(
    #state{stream_module = StreamModule} = State0,
    {'and', CondList},
    BlockFn
) ->
    {Replacements, State1} = lists:foldl(
        fun(Cond, {AccReplacements, AccState}) ->
            Offset = StreamModule:offset(AccState#state.stream),
            {NewAccState, BranchInfo, ReplaceDelta} = if_block_cond(AccState, Cond),
            {[{Offset + ReplaceDelta, BranchInfo} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, {BranchFunc, Reg, Operand}}, AccStream) ->
            BranchOffset = OffsetAfter - ReplacementOffset,
            NewBranchInstr = apply(?ASM, BranchFunc, [Reg, Operand, BranchOffset]),
            StreamModule:replace(AccStream, ReplacementOffset, NewBranchInstr)
        end,
        Stream2,
        Replacements
    ),
    State3 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs};
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, {BranchFunc, Reg, Operand}, BranchInstrDelta} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchInstrOffset = Offset + BranchInstrDelta,
    BranchOffset = OffsetAfter - BranchInstrOffset,
    NewBranchInstr = apply(?ASM, BranchFunc, [Reg, Operand, BranchOffset]),
    Stream3 = StreamModule:replace(Stream2, BranchInstrOffset, NewBranchInstr),
    State3 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs}.

%%-----------------------------------------------------------------------------
%% @doc Emit an if else block, i.e. emit a test of a condition and
%% conditionally execute a block or another block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockTrueFn function to emit the block that is executed if condition is true
%% @param BlockFalseFn function to emit the block that is executed if condition is false
%% @return Updated backend state
%%-----------------------------------------------------------------------------
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockTrueFn,
    BlockFalseFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, {BranchFunc, Reg, Operand}, BranchInstrDelta} = if_block_cond(State0, Cond),
    BranchInstrOffset = Offset + BranchInstrDelta,
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional branch to skip the else block (will be replaced)
    ElseJumpOffset = StreamModule:offset(Stream2),
    %% Use all-1s placeholder for flash compatibility (can only flip 1->0)
    ElseJumpInstr = <<16#FFFF:16>>,
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - BranchInstrOffset,
    NewBranchInstr = apply(?ASM, BranchFunc, [Reg, Operand, ElseBranchOffset]),
    Stream4 = StreamModule:replace(Stream3, BranchInstrOffset, NewBranchInstr),
    %% Build the else block
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs,
        regs = State1#state.regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = ?ASM:j(FinalJumpOffset),
    %% Assert that replacement is 2 bytes (c.j range: -2048..2046)
    %% If this fails, the if/else blocks are too large
    2 = byte_size(NewElseJumpInstr),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    State4 = merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs),
    MergedRegs = jit_regs:merge(State2#state.regs, State3#state.regs),
    State4#state{regs = MergedRegs}.

if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '<', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: bge Reg, zero, offset (branch if Reg >= 0, i.e., NOT negative/NOT less than 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bge, Reg, zero}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Reg, Val, offset (branch if Reg >= Val, i.e., NOT less than)
    % Load immediate into a temp register for comparison
    Temp = first_avail(State0#state.available_regs),
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bge, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val) ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Reg, Temp, offset (branch if Reg >= Temp, i.e., NOT less than)
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bge, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Temp, Reg, offset (branch if Val >= Reg, i.e., NOT Val < Reg)
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bge, Temp, Reg}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val) ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Temp, Reg, offset (branch if Val >= Reg, i.e., NOT Val < Reg)
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bge, Temp, Reg}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % RISC-V: bge Reg, RegB, offset (branch if Reg >= RegB, i.e., NOT less than)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bge, Reg, RegB}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: bne Reg, zero, offset (branch if Reg != 0, i.e., NOT equal to 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bne, Reg, zero}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: bne Reg, RegB, offset (branch if Reg != RegB, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {bne, Reg, RegB}, 0};
%% Delegate (int) forms to regular forms for integer-width comparison
if_block_cond(State, {'(int)', RegOrTuple, '==', 0}) ->
    if_block_cond(State, {RegOrTuple, '==', 0});
if_block_cond(State, {'(int)', RegOrTuple, '==', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '==', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Load immediate into temp, then beq Reg, Temp, offset
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {beq, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: beq Reg, Val, offset (branch if Reg == Val, i.e., NOT not-equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {beq, Reg, Val}, 0};
if_block_cond(State, {'(int)', RegOrTuple, '!=', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '!=', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Load immediate into temp, then bne Reg, Temp, offset
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bne, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {{free, RegA}, '==', {free, RegB}}
) ->
    %% RISC-V: bne RegA, RegB, offset (branch if RegA != RegB, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    State1 = State0#state{stream = Stream1},
    State2 = if_block_free_reg({free, RegA}, State1),
    State3 = if_block_free_reg({free, RegB}, State2),
    {State3, {bne, RegA, RegB}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% RISC-V: bne Reg, Temp, offset (branch if Reg != Temp, i.e., NOT equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {bne, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% RISC-V: beq Reg, Temp, offset (branch if Reg == Temp, i.e., NOT not-equal)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, {beq, Reg, Temp}, BranchDelta};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Test bit 0 by shifting to MSB, then branch if negative (bit was 1, NOT false)
    I1 = ?ASM:slli(Temp, Reg, ?BOOL_SHIFT),
    Stream1 = StreamModule:append(Stream0, I1),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream2, regs = Regs1},
    {State2, {blt, Temp, zero}, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Test bit 0 by shifting to MSB, then branch if non-negative (bit was 0, NOT true)
    I1 = ?ASM:slli(Temp, Reg, ?BOOL_SHIFT),
    Stream1 = StreamModule:append(Stream0, I1),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream2, regs = Regs1},
    {State2, {bge, Temp, zero}, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) ->
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% RISC-V: Test bits using ANDI or li+and
    TestCode =
        if
            Val >= -2048 andalso Val =< 2047 ->
                %% Can use ANDI instruction directly
                ?ASM:andi(Temp, Reg, Val);
            true ->
                %% Need to load immediate into temp register first
                TestCode0 = ?ASM:li(Temp, Val),
                TestCode1 = ?ASM:and_(Temp, Reg, Temp),
                <<TestCode0/binary, TestCode1/binary>>
        end,
    OffsetBefore = StreamModule:offset(Stream0),
    Stream1 = StreamModule:append(Stream0, TestCode),
    BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
    %% Branch if result is zero (no bits set, NOT != 0)
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    State1 = if_block_free_reg(RegOrTuple, State0),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream2, regs = Regs1},
    {State2, {beq, Temp, zero}, BranchDelta};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {Reg, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    Temp = first_avail(Avail),
    %% RISC-V: Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = ?ASM:not_(Temp, Reg),
    I2 = ?ASM:slli(Temp, Temp, ?WORD_SIZE_BYTES * 8 - 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Temp),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    {State1, {beq, Temp, zero}, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    %% RISC-V: Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = ?ASM:not_(Reg, Reg),
    I2 = ?ASM:slli(Reg, Reg, ?WORD_SIZE_BYTES * 8 - 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    BranchInstr = <<16#FFFFFFFF:32/little>>,
    Stream2 = StreamModule:append(Stream1, BranchInstr),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Reg),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = if_block_free_reg(RegTuple, State1),
    {State2, {beq, Reg, zero}, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    %% RISC-V: AND with mask, then compare with value
    OffsetBefore = StreamModule:offset(Stream0),
    I1 = ?ASM:mv(Temp, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State1 = State0#state{stream = Stream1},
    {State2, Temp} = and_(State1#state{available_regs = AT}, {free, Temp}, Mask),
    Stream2 = State2#state.stream,
    %% Compare Temp with Val and branch if equal (NOT != Val)
    case Val of
        0 ->
            %% Optimize comparison with zero
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            State3 = State2#state{
                stream = Stream3, available_regs = State2#state.available_regs bor reg_bit(Temp)
            },
            {State3, {beq, Temp, zero}, BranchDelta};
        _ when ?IS_GPR(Val) ->
            %% Val is a register
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            State3 = State2#state{
                stream = Stream3, available_regs = State2#state.available_regs bor reg_bit(Temp)
            },
            {State3, {beq, Temp, Val}, BranchDelta};
        _ ->
            %% Val is an immediate - need second temp register
            %% Reuse the mask register for the comparison value
            MaskReg = first_avail(AT),
            AT2 = AT band (bnot reg_bit(MaskReg)),
            State3 = mov_immediate(State2#state{available_regs = AT2}, MaskReg, Val),
            Stream3 = State3#state.stream,
            BranchDelta = StreamModule:offset(Stream3) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream4 = StreamModule:append(Stream3, BranchInstr),
            Regs4 = jit_regs:invalidate_reg(State3#state.regs, MaskReg),
            State4 = State3#state{
                stream = Stream4,
                available_regs = State3#state.available_regs bor reg_bit(Temp) bor reg_bit(MaskReg),
                regs = Regs4
            },
            {State4, {beq, Temp, MaskReg}, BranchDelta}
    end;
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailRegs
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    %% RISC-V: AND with mask, then compare with value
    OffsetBefore = StreamModule:offset(Stream0),
    {State1, Reg} = and_(State0, RegTuple, Mask),
    Stream1 = State1#state.stream,
    %% Compare Reg with Val and branch if equal (NOT != Val)
    case Val of
        0 ->
            %% Optimize comparison with zero
            BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream2 = StreamModule:append(Stream1, BranchInstr),
            State2 = State1#state{stream = Stream2},
            State3 = if_block_free_reg(RegTuple, State2),
            {State3, {beq, Reg, zero}, BranchDelta};
        _ when ?IS_GPR(Val) ->
            %% Val is a register
            BranchDelta = StreamModule:offset(Stream1) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream2 = StreamModule:append(Stream1, BranchInstr),
            State2 = State1#state{stream = Stream2},
            State3 = if_block_free_reg(RegTuple, State2),
            {State3, {beq, Reg, Val}, BranchDelta};
        _ ->
            %% Val is an immediate - need temp register
            %% Reuse the mask register for the comparison value
            MaskReg = first_avail(State1#state.available_regs),
            AT = State1#state.available_regs band (bnot reg_bit(MaskReg)),
            State2 = mov_immediate(State1#state{available_regs = AT}, MaskReg, Val),
            Stream2 = State2#state.stream,
            BranchDelta = StreamModule:offset(Stream2) - OffsetBefore,
            BranchInstr = <<16#FFFFFFFF:32/little>>,
            Stream3 = StreamModule:append(Stream2, BranchInstr),
            Regs3 = jit_regs:invalidate_reg(State2#state.regs, MaskReg),
            State3 = State2#state{stream = Stream3, available_regs = AvailRegs, regs = Regs3},
            State4 = if_block_free_reg(RegTuple, State3),
            {State4, {beq, Reg, MaskReg}, BranchDelta}
    end.

if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, used_regs = UR0} = State0,
    Bit = reg_bit(Reg),
    AvR1 = AvR0 bor Bit,
    UR1 = UR0 band (bnot Bit),
    State0#state{
        available_regs = AvR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

merge_used_regs(#state{used_regs = UR} = State, OtherUR) ->
    MergedUR = UR bor OtherUR,
    MergedAvail = ?AVAILABLE_REGS_MASK band (bnot MergedUR),
    State#state{used_regs = MergedUR, available_regs = MergedAvail}.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = ?ASM:srli(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UR,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I = ?ASM:srli(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register left by a fixed number of bits, effectively
%% multiplying it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_left(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Shift
) when
    is_atom(Reg)
->
    I = ?ASM:slli(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = ?ASM:srai(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right_arith(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UR,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I = ?ASM:srai(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a function pointer with arguments. This function converts
%% arguments and passes them following the backend ABI convention.
%% @end
%% @param State current backend state
%% @param FuncPtrTuple either {free, Reg} or {primitive, PrimitiveIndex}
%% @param Args arguments to pass to the function
%% @return Updated backend state and return register
%%-----------------------------------------------------------------------------
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0Mask,
        used_regs = UsedRegs0Mask
    } = State0,
    FuncPtrTuple,
    Args
) ->
    FreeRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    FreeMask = regs_to_mask(FreeRegs),
    UsedRegs1Mask = UsedRegs0Mask band (bnot FreeMask),
    % Save RA so it's preserved across jalr calls
    SavedRegs = [
        ?RA_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | mask_to_list(UsedRegs1Mask)
    ],

    % Calculate available registers
    FreeGPMask = FreeMask band ?AVAILABLE_REGS_MASK,
    AvailableRegs1Mask = FreeGPMask bor AvailableRegs0Mask,

    % Calculate stack space: round up to 16-byte boundary for RISC-V ABI
    NumRegs = length(SavedRegs),
    StackBytes = NumRegs * ?WORD_SIZE_BYTES,
    AlignedStackBytes = ((StackBytes + 15) div 16) * 16,

    Stream1 = push_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0),

    % Set up arguments following RISC-V calling convention
    % Arguments are passed in a0-a7 (up to 8 register arguments)
    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),

    RegArgs0 = Args1,
    RegArgsRegs = lists:flatmap(fun arg_to_reg_list/1, RegArgs0),
    RegArgsRegsMask = regs_to_mask(RegArgsRegs),

    % We pushed registers to stack, so we can use these registers we saved
    % and the currently available registers
    SetArgsMask = (UsedRegs1Mask band (bnot RegArgsRegsMask)) bor AvailableRegs0Mask,
    State1 = State0#state{
        available_regs = SetArgsMask,
        used_regs = ?AVAILABLE_REGS_MASK band (bnot SetArgsMask),
        stream = Stream1
    },

    ParameterRegs = parameter_regs(RegArgs0),
    ParamMask = regs_to_mask(ParameterRegs),
    {Stream3, SetArgsAvailMask, FuncPtrReg, RegArgs} =
        case FuncPtrTuple of
            {free, FuncPtrReg0} ->
                FuncPtrReg0Bit = reg_bit(FuncPtrReg0),
                % If FuncPtrReg is in parameter regs, we must swap it with a free reg.
                case ParamMask band FuncPtrReg0Bit =/= 0 of
                    true ->
                        AvailNotParam = SetArgsMask band (bnot ParamMask),
                        case AvailNotParam of
                            0 ->
                                % Swap with a reg used in RegArgs0
                                % that is not in ParameterRegs
                                NewArgReg = first_avail(SetArgsMask),
                                FuncPtrReg1 = first_avail(RegArgsRegsMask band (bnot ParamMask)),
                                FuncPtrReg1Bit = reg_bit(FuncPtrReg1),
                                MovInstr1 = ?ASM:mv(NewArgReg, FuncPtrReg1),
                                MovInstr2 = ?ASM:mv(FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailMask1 =
                                    (SetArgsMask band (bnot FuncPtrReg1Bit)) bor FuncPtrReg0Bit,
                                RegArgs1 = replace_reg(RegArgs0, FuncPtrReg1, NewArgReg),
                                {
                                    StreamModule:append(
                                        State1#state.stream, <<MovInstr1/binary, MovInstr2/binary>>
                                    ),
                                    SetArgsAvailMask1,
                                    FuncPtrReg1,
                                    RegArgs1
                                };
                            _ ->
                                FuncPtrReg1 = first_avail(AvailNotParam),
                                FuncPtrReg1Bit = reg_bit(FuncPtrReg1),
                                MovInstr = ?ASM:mv(FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailMask1 =
                                    (SetArgsMask band (bnot FuncPtrReg1Bit)) bor FuncPtrReg0Bit,
                                {
                                    StreamModule:append(State1#state.stream, MovInstr),
                                    SetArgsAvailMask1,
                                    FuncPtrReg1,
                                    RegArgs0
                                }
                        end;
                    false ->
                        SetArgsAvailMask1 = SetArgsMask band (bnot FuncPtrReg0Bit),
                        {State1#state.stream, SetArgsAvailMask1, FuncPtrReg0, RegArgs0}
                end;
            {primitive, Primitive} ->
                FuncPtrReg0 = first_avail(SetArgsMask band (bnot ParamMask)),
                FuncPtrReg0Bit = reg_bit(FuncPtrReg0),
                SetArgsAvailMask1 = SetArgsMask band (bnot FuncPtrReg0Bit),
                PrepCall = load_primitive_ptr(Primitive, FuncPtrReg0),
                Stream2 = StreamModule:append(State1#state.stream, PrepCall),
                {Stream2, SetArgsAvailMask1, FuncPtrReg0, RegArgs0}
        end,

    State3 = State1#state{
        available_regs = SetArgsAvailMask,
        used_regs = ?AVAILABLE_REGS_MASK band (bnot SetArgsAvailMask),
        stream = Stream3
    },

    StackOffset = AlignedStackBytes,
    State4 = set_registers_args(State3, RegArgs, ParameterRegs, StackOffset),
    Stream4 = State4#state.stream,

    % Call the function pointer (using JALR for call with return)
    Call = ?ASM:jalr(ra, FuncPtrReg, 0),
    Stream5 = StreamModule:append(Stream4, Call),

    % For result, we need a free register (including FuncPtrReg).
    % If none are available (all registers were pushed to the stack),
    % we write the result to the stack position of FuncPtrReg
    {Stream6, UsedRegs2Mask, ResultReg} =
        case {length(SavedRegs), FuncPtrTuple} of
            {N, {free, ResultFPReg0}} when N >= 7 ->
                % Registers exhausted: use FuncPtrReg which is free after the call
                RegIndex = index_of(ResultFPReg0, SavedRegs),
                case RegIndex >= 0 of
                    true ->
                        StoreResultStackOffset = RegIndex * ?WORD_SIZE_BYTES,
                        StoreResult = ?STORE_WORD(sp, a0, StoreResultStackOffset),
                        {
                            StreamModule:append(Stream5, StoreResult),
                            UsedRegs1Mask bor reg_bit(ResultFPReg0),
                            ResultFPReg0
                        };
                    false ->
                        MoveResult = ?ASM:mv(ResultFPReg0, a0),
                        {
                            StreamModule:append(Stream5, MoveResult),
                            UsedRegs1Mask bor reg_bit(ResultFPReg0),
                            ResultFPReg0
                        }
                end;
            {_, {free, ResultFPReg1}} ->
                % FuncPtrReg is free after the call, use it for result
                MoveResult = ?ASM:mv(ResultFPReg1, a0),
                {
                    StreamModule:append(Stream5, MoveResult),
                    UsedRegs1Mask bor reg_bit(ResultFPReg1),
                    ResultFPReg1
                };
            _ ->
                ResultReg0 = first_avail(AvailableRegs1Mask),
                MoveResult = ?ASM:mv(ResultReg0, a0),
                {
                    StreamModule:append(Stream5, MoveResult),
                    UsedRegs1Mask bor reg_bit(ResultReg0),
                    ResultReg0
                }
        end,

    Stream8 = pop_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream6),

    ResultRegBit = reg_bit(ResultReg),
    AvailableRegs3Mask = (AvailableRegs1Mask band (bnot ResultRegBit)) band ?AVAILABLE_REGS_MASK,
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    {
        State4#state{
            stream = Stream8,
            available_regs = AvailableRegs3Mask,
            used_regs = UsedRegs2Mask,
            regs = Regs1
        },
        ResultReg
    }.

arg_to_reg_list({free, {ptr, Reg}}) -> [Reg];
arg_to_reg_list({free, Reg}) when is_atom(Reg) -> [Reg];
arg_to_reg_list(Reg) when is_atom(Reg) -> [Reg];
arg_to_reg_list(_) -> [].

index_of(Item, List) -> index_of(Item, List, 0).

index_of(_, [], _) -> -1;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Rest], Index) -> index_of(Item, Rest, Index + 1).

push_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    % RISC-V: addi sp, sp, -AlignedStackBytes then store reg for each reg
    StackAdjust = ?ASM:addi(sp, sp, -AlignedStackBytes),
    Stream1 = StreamModule:append(Stream0, StackAdjust),
    {Stream2, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Store = ?STORE_WORD(sp, Reg, Offset),
            {StreamModule:append(StreamAcc, Store), Offset + ?WORD_SIZE_BYTES}
        end,
        {Stream1, 0},
        SavedRegs
    ),
    Stream2;
push_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

pop_registers(SavedRegs, AlignedStackBytes, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    % RISC-V: load reg, offset(sp) for each reg then addi sp, sp, AlignedStackBytes
    {Stream1, _} = lists:foldl(
        fun(Reg, {StreamAcc, Offset}) ->
            Load = ?LOAD_WORD(Reg, sp, Offset),
            {StreamModule:append(StreamAcc, Load), Offset + ?WORD_SIZE_BYTES}
        end,
        {Stream0, 0},
        SavedRegs
    ),
    StackAdjust = ?ASM:addi(sp, sp, AlignedStackBytes),
    StreamModule:append(Stream1, StackAdjust);
pop_registers([], _AlignedStackBytes, _StreamModule, Stream0) ->
    Stream0.

set_registers_args(State0, Args, StackOffset) ->
    ParamRegs = parameter_regs(Args),
    set_registers_args(State0, Args, ParamRegs, StackOffset).

set_registers_args(
    #state{used_regs = UsedRegsMask} = State0,
    Args,
    ParamRegs,
    StackOffset
) ->
    ArgsRegs = args_regs(Args),
    ParamMask = regs_to_mask(ParamRegs),
    ArgsMask = regs_to_mask(ArgsRegs),
    AvailableScratchMask =
        ?AVAILABLE_REGS_MASK band (bnot (ParamMask bor ArgsMask bor UsedRegsMask)),
    AvailableScratchGP = mask_to_list(AvailableScratchMask),
    State1 = set_registers_args0(
        State0, Args, ArgsRegs, ParamRegs, AvailableScratchGP, StackOffset
    ),
    Stream1 = State1#state.stream,
    NewUsedMask = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> AccUsed band (bnot reg_bit(Reg));
            ({free, Reg}, AccUsed) when is_atom(Reg) -> AccUsed band (bnot reg_bit(Reg));
            (_, AccUsed) -> AccUsed
        end,
        UsedRegsMask,
        Args
    ),
    State1#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS_MASK band (bnot (ParamMask bor NewUsedMask)),
        used_regs = ParamMask bor NewUsedMask
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([{avm_int64_t, _} | T], Regs, Acc) ->
    parameter_regs0_avm_int64_t(T, Regs, Acc);
parameter_regs0([_Other | T], [Reg | Rest], Acc) ->
    parameter_regs0(T, Rest, [Reg | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_registers_args0(State, [], [], [], _AvailGP, _StackOffset) ->
    State;
set_registers_args0(State, [{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, StackOffset) ->
    set_registers_args0(State, [FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, StackOffset);
set_registers_args0(
    State, [ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, StackOffset
) ->
    set_registers_args0(State, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset);
% Handle 64-bit arguments via backend-specific callback
set_registers_args0(
    State,
    [{avm_int64_t, Value} | ArgsT],
    ArgsRegs,
    ParamRegs,
    AvailGP,
    StackOffset
) when is_integer(Value) ->
    handle_avm_int64_t(State, Value, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset);
% ctx is special as we need it to access x_reg/y_reg/fp_reg and we don't
% want to replace it
set_registers_args0(
    State, [Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, StackOffset
) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    State1 = set_registers_args1(State, Arg, ?CTX_REG, StackOffset),
    set_registers_args0(State1, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset);
set_registers_args0(
    #state{stream_module = StreamModule} = State0,
    [Arg | ArgsT],
    [_ArgReg | ArgsRegsT],
    [ParamReg | ParamRegsT],
    AvailGP,
    StackOffset
) ->
    case lists:member(ParamReg, ArgsRegsT) of
        false ->
            State1 = set_registers_args1(State0, Arg, ParamReg, StackOffset),
            set_registers_args0(State1, ArgsT, ArgsRegsT, ParamRegsT, AvailGP, StackOffset);
        true ->
            [Avail | AvailGPT] = AvailGP,
            I = ?ASM:mv(Avail, ParamReg),
            Stream1 = StreamModule:append(State0#state.stream, I),
            State1 = set_registers_args1(
                State0#state{stream = Stream1}, Arg, ParamReg, StackOffset
            ),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_registers_args0(
                State1, NewArgsT, ArgsRegsT, ParamRegsT, AvailGPT, StackOffset
            )
    end.

set_registers_args1(State, Reg, Reg, _Offset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    jit_state,
    ParamReg,
    _StackOffset
) ->
    % jit_state is always in a1, so we only need to move it if the param reg is different
    case ParamReg of
        a1 ->
            State;
        _ ->
            I = ?ASM:mv(ParamReg, a1),
            Stream1 = StreamModule:append(Stream0, I),
            State#state{stream = Stream1}
    end;
% For tail calls, jit_state is already in a1
set_registers_args1(State, jit_state_tail_call, a1, _StackOffset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {x_reg, extra},
    Reg,
    _StackOffset
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I = ?LOAD_WORD(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, Reg, _StackOffset
) ->
    {XReg, X_REGOffset} = ?X_REG(X),
    I = ?LOAD_WORD(Reg, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Source}, Reg, _StackOffset
) ->
    I = ?LOAD_WORD(Reg, Source, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AvailRegs} = State,
    {y_reg, X},
    Reg,
    _StackOffset
) ->
    Code = ldr_y_reg(Reg, X, AvailRegs),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, ArgReg, Reg, _StackOffset
) when
    ?IS_GPR(ArgReg)
->
    I = ?ASM:mv(Reg, ArgReg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(State, Value, Reg, _StackOffset) when ?IS_SIGNED_OR_UNSIGNED_WORD(Value) ->
    mov_immediate(State, Reg, Value).

%%-----------------------------------------------------------------------------
%% @doc Emit a move to a vm register (x_reg, y_reg, fpreg or a pointer on x_reg)
%% from an immediate, a native register or another vm register.
%% @end
%% @param State current backend state
%% @param Src value to move to vm register
%% @param Dest vm register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
move_to_vm_register(#state{regs = Regs0} = State, Src, Dest) ->
    VmLoc = jit_regs:vm_dest_to_contents(Dest, ?MAX_REG),
    Regs1 =
        case VmLoc of
            unknown -> Regs0;
            _ -> jit_regs:invalidate_vm_loc(Regs0, VmLoc)
        end,
    State1 = move_to_vm_register_emit(State#state{regs = Regs1}, Src, Dest),
    case {Src, VmLoc} of
        {Reg, Contents} when is_atom(Reg), Contents =/= unknown ->
            #state{regs = Regs2} = State1,
            State1#state{regs = jit_regs:set_contents(Regs2, Reg, Contents)};
        _ ->
            State1
    end.

% Native register to VM register
move_to_vm_register_emit(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = ?STORE_WORD(BaseReg, Src, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {x_reg, X}) when is_atom(Src) ->
    {BaseReg, Off} = ?X_REG(X),
    I1 = ?STORE_WORD(BaseReg, Src, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = ?STORE_WORD(Reg, Src, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(
    #state{available_regs = Avail, regs = Regs0} = State0, Src, {y_reg, Y}
) when
    is_atom(Src)
->
    Temp1 = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp1)),
    Code = str_y_reg(Src, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    % str_y_reg clobbers Temp1, and for large offsets also clobbers first_avail(AT)
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp1),
    Regs2 =
        case AT of
            0 -> Regs1;
            _ -> jit_regs:invalidate_reg(Regs1, first_avail(AT))
        end,
    State0#state{stream = Stream1, regs = Regs2};
% Source is an integer to y_reg (optimized: ldr first, then movs)
move_to_vm_register_emit(
    #state{available_regs = Avail, regs = Regs0} = State0, N, {y_reg, Y}
) when
    is_integer(N), N >= 0, N =< 255
->
    Temp1 = first_avail(Avail),
    Avail2 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail2),
    AT = Avail2 band (bnot reg_bit(Temp2)),
    I1 = ?ASM:li(Temp2, N),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, YCode/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp1), Temp2),
    % str_y_reg may clobber first_avail(AT) for large offsets
    Regs2 =
        case AT of
            0 -> Regs1;
            _ -> jit_regs:invalidate_reg(Regs1, first_avail(AT))
        end,
    State0#state{stream = Stream1, regs = Regs2};
% Source is an integer (0-255 for movs, negative values need different handling)
move_to_vm_register_emit(#state{available_regs = AR0} = State0, N, Dest) when
    is_integer(N), N >= 0, N =< 255
->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = ?ASM:li(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
%% Handle large values using simple literal pool (branch-over pattern)
move_to_vm_register_emit(#state{available_regs = AR0} = State0, N, Dest) when
    is_integer(N)
->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, N),
    State2 = move_to_vm_register(State1, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State2#state{available_regs = AR0, regs = Regs1};
% Source is a VM register
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, extra}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = ?LOAD_WORD(Temp, BaseReg, Off),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, X}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = ?LOAD_WORD(Temp, XReg, X_REGOffset),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {ptr, Reg}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = ?LOAD_WORD(Temp, Reg, 0),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {y_reg, Y}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    Code = ldr_y_reg(Temp, Y, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, Code),
    % ldr_y_reg clobbers first_avail(AT) as a hidden temp for loading Y_REGS pointer
    Regs0a =
        case AT of
            0 -> State0#state.regs;
            _ -> jit_regs:invalidate_reg(State0#state.regs, first_avail(AT))
        end,
    State0a = State0#state{
        stream = Stream1,
        available_regs = AT,
        regs = Regs0a
    },
    State1 = move_to_vm_register(State0a, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
% term_to_float
move_to_vm_register_emit(
    #state{
        stream_module = StreamModule,
        available_regs = Avail,
        stream = Stream0,
        variant = Variant
    } =
        State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    Temp1 = first_avail(Avail),
    Temp2 = first_avail(Avail band (bnot reg_bit(Temp1))),
    {BaseReg, Off} = ?FP_REGS,
    I1 = ?LOAD_WORD(Temp1, BaseReg, Off),
    I2 = ?ASM:lw(Temp2, Reg, ?FLOAT_DATA_OFFSET),
    case Variant band ?JIT_VARIANT_FLOAT32 of
        0 ->
            % Double precision: write both 32-bit parts
            I3 = ?ASM:sw(Temp1, Temp2, F * 8),
            I4 = ?ASM:lw(Temp2, Reg, ?FLOAT_DATA_OFFSET + 4),
            I5 = ?ASM:sw(Temp1, Temp2, F * 8 + 4),
            Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>;
        _ ->
            % Single precision: write only first 32-bit part
            I3 = ?ASM:sw(Temp1, Temp2, F * 4),
            Code = <<I1/binary, I2/binary, I3/binary>>
    end,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = free_native_register(State0, Reg),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State1#state.regs, Temp1), Temp2),
    State1#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a vm or a native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Dest vm or native register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    I1 = ?LOAD_WORD(Temp, Reg, Index * ?WORD_SIZE_BYTES),
    {BaseReg, Off} = ?X_REG(X),
    I2 = ?STORE_WORD(BaseReg, Temp, Off),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    I1 = ?LOAD_WORD(Temp, Reg, Index * ?WORD_SIZE_BYTES),
    I2 = ?STORE_WORD(Dest, Temp, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp1 = first_avail(Avail),
    Avail2 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail2),
    AT = Avail2 band (bnot reg_bit(Temp2)),
    I1 = ?LOAD_WORD(Temp2, Reg, Index * ?WORD_SIZE_BYTES),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp1),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp2),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    I1 = ?LOAD_WORD(Reg, Reg, Index * ?WORD_SIZE_BYTES),
    YCode = str_y_reg(Reg, Y, Temp, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Reg),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Reg,
    Index,
    Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = ?LOAD_WORD(Dest, Reg, Index * ?WORD_SIZE_BYTES),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Dest),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = ?ASM:slli(IndexReg, IndexReg, (?WORD_SIZE_BYTES bsr 2) + 1),
    I2 = ?ASM:add(IndexReg, Reg, IndexReg),
    I3 = ?LOAD_WORD(IndexReg, IndexReg, 0),
    {BaseReg, Off} = ?X_REG(X),
    I4 = ?STORE_WORD(BaseReg, IndexReg, Off),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs2
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {ptr, PtrReg}
) when is_atom(IndexReg) ->
    I1 = ?ASM:slli(IndexReg, IndexReg, (?WORD_SIZE_BYTES bsr 2) + 1),
    I2 = ?ASM:add(IndexReg, Reg, IndexReg),
    I3 = ?LOAD_WORD(IndexReg, IndexReg, 0),
    I4 = ?STORE_WORD(PtrReg, IndexReg, 0),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when is_atom(IndexReg) ->
    Temp = first_avail(AvailableRegs0),
    AT = AvailableRegs0 band (bnot reg_bit(Temp)),
    I1 = ?ASM:slli(IndexReg, IndexReg, (?WORD_SIZE_BYTES bsr 2) + 1),
    I2 = ?ASM:add(IndexReg, Reg, IndexReg),
    I3 = ?LOAD_WORD(IndexReg, IndexReg, 0),
    Code = str_y_reg(IndexReg, Y, Temp, AT),
    I4 = Code,
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    Regs3 = jit_regs:invalidate_reg(Regs2, IndexReg),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs3
    }.

%% @doc move reg[x] to a vm or native register
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = ?LOAD_WORD(Reg, Reg, Index * ?WORD_SIZE_BYTES),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    Reg,
    Index
) ->
    ElemReg = first_avail(Avail),
    ElemBit = reg_bit(ElemReg),
    I1 = ?LOAD_WORD(ElemReg, Reg, Index * ?WORD_SIZE_BYTES),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ElemReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot ElemBit),
            used_regs = UsedRegs0 bor ElemBit,
            regs = Regs1
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = ?STORE_WORD(Reg, ValueReg, Index * ?WORD_SIZE_BYTES),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:mv(Temp, IndexReg),
    I2 = ?ASM:slli(Temp, Temp, (?WORD_SIZE_BYTES bsr 2) + 1),
    I3 = ?ASM:add(Temp, Reg, Temp),
    I4 = ?STORE_WORD(Temp, ValueReg, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    State0,
    Value,
    Reg,
    Index
) ->
    {State1, Temp} = copy_to_native_register(State0, Value),
    State2 = move_to_array_element(State1, Temp, Reg, Index),
    free_native_register(State2, Temp).

move_to_array_element(
    State,
    Value,
    BaseReg,
    IndexReg,
    Offset
) when is_integer(IndexReg) andalso is_integer(Offset) andalso ?ARRAY_OFFSET_FOLD_GUARD(Offset) ->
    move_to_array_element(State, Value, BaseReg, ?ARRAY_OFFSET_FOLD(IndexReg, Offset));
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:addi(Temp, IndexReg, Offset),
    I2 = ?ASM:slli(Temp, Temp, (?WORD_SIZE_BYTES bsr 2) + 1),
    I3 = ?ASM:add(Temp, BaseReg, Temp),
    I4 = ?STORE_WORD(Temp, ValueReg, 0),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    State0,
    Value,
    BaseReg,
    IndexReg,
    Offset
) ->
    {State1, ValueReg} = copy_to_native_register(State0, Value),
    Temp = first_avail(State1#state.available_regs),
    I1 = ?ASM:addi(Temp, IndexReg, Offset),
    I2 = ?ASM:slli(Temp, Temp, (?WORD_SIZE_BYTES bsr 2) + 1),
    I3 = ?ASM:add(Temp, BaseReg, Temp),
    I4 = ?STORE_WORD(Temp, ValueReg, 0),
    Stream1 = (State1#state.stream_module):append(
        State1#state.stream, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
    free_native_register(State2, ValueReg).

move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = jit_regs:value_to_contents(Value, ?MAX_REG),
    case Contents =/= unknown andalso jit_regs:find_reg_with_contents(Regs, Contents) of
        {ok, CachedReg} ->
            Bit = reg_bit(CachedReg),
            case State#state.used_regs band Bit of
                0 ->
                    case State#state.available_regs band Bit of
                        0 ->
                            move_to_native_register_emit(State, Value, Contents);
                        _ ->
                            {
                                State#state{
                                    used_regs = State#state.used_regs bor Bit,
                                    available_regs = State#state.available_regs band (bnot Bit)
                                },
                                CachedReg
                            }
                    end;
                _ ->
                    {State, CachedReg}
            end;
        _ ->
            move_to_native_register_emit(State, Value, Contents)
    end.

move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    cp,
    Contents
) ->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    {BaseReg, Off} = ?CP,
    I1 = ?LOAD_WORD(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {ptr, Reg},
    _Contents
) when is_atom(Reg) ->
    I1 = ?LOAD_WORD(Reg, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
move_to_native_register_emit(
    #state{
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State0,
    Imm,
    Contents
) when
    is_integer(Imm)
->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    State1 = State0#state{
        used_regs = Used bor RegBit,
        available_regs = Avail band (bnot RegBit),
        regs = Regs1
    },
    {move_to_native_register(State1, Imm, Reg), Reg};
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, extra},
    Contents
) ->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = ?LOAD_WORD(Reg, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    {BaseReg, Offset} = ?X_REG(X),
    I1 = ?LOAD_WORD(Reg, BaseReg, Offset),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor RegBit,
            available_regs = Avail band (bnot RegBit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {y_reg, Y},
    Contents
) ->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    AvailT = Avail band (bnot RegBit),
    Code = ldr_y_reg(Reg, Y, AvailT),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    % ldr_y_reg clobbers first_avail(AvailT) as a hidden temp for loading Y_REGS pointer
    Regs2 =
        case AvailT of
            0 -> Regs1;
            _ -> jit_regs:invalidate_reg(Regs1, first_avail(AvailT))
        end,
    {
        State#state{
            stream = Stream1,
            available_regs = AvailT,
            used_regs = Used bor RegBit,
            regs = Regs2
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used
    } = State,
    {fp_reg, F},
    _Contents
) ->
    RegA = first_avail(Avail),
    RegABit = reg_bit(RegA),
    Avail2 = Avail band (bnot RegABit),
    RegB = first_avail(Avail2),
    RegBBit = reg_bit(RegB),
    AvailT = Avail2 band (bnot RegBBit),
    {BaseReg, Off} = ?FP_REGS,
    I1 = ?LOAD_WORD(RegB, BaseReg, Off),
    I2 = ?ASM:lw(RegA, RegB, F * 8),
    I3 = ?ASM:lw(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{
            stream = Stream1, available_regs = AvailT, used_regs = Used bor RegABit bor RegBBit
        },
        {fp, RegA, RegB}
    }.

move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = ?ASM:mv(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    SrcContents = jit_regs:get_contents(Regs0, RegSrc),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, SrcContents),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(State, ValSrc, RegDst) when is_integer(ValSrc) ->
    State1 = mov_immediate(State, RegDst, ValSrc),
    #state{regs = Regs0} = State1,
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {imm, ValSrc}),
    State1#state{regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = ?LOAD_WORD(RegDst, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, extra}, RegDst
) ->
    {BaseReg, Off} = ?X_REG(?MAX_REG),
    I1 = ?LOAD_WORD(RegDst, BaseReg, Off),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    {XReg, X_REGOffset} = ?X_REG(X),
    I1 = ?LOAD_WORD(RegDst, XReg, X_REGOffset),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = AT, regs = Regs0} =
        State,
    {y_reg, Y},
    RegDst
) ->
    Code = ldr_y_reg(RegDst, Y, AT),
    Stream1 = StreamModule:append(Stream0, Code),
    % ldr_y_reg clobbers first_avail(AT) as a hidden temp for loading Y_REGS pointer
    Regs1 =
        case AT of
            0 -> Regs0;
            _ -> jit_regs:invalidate_reg(Regs0, first_avail(AT))
        end,
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    {fp_reg, F},
    {fp, RegA, RegB}
) ->
    {BaseReg, Off} = ?FP_REGS,
    I1 = ?LOAD_WORD(RegB, BaseReg, Off),
    I2 = ?ASM:lw(RegA, RegB, F * 8),
    I3 = ?ASM:lw(RegB, RegB, F * 8 + 4),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    Reg
) when is_atom(Reg) ->
    SaveReg = first_avail(Avail),
    SaveBit = reg_bit(SaveReg),
    I1 = ?ASM:mv(SaveReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    SrcContents = jit_regs:get_contents(Regs0, Reg),
    Regs1 = jit_regs:set_contents(Regs0, SaveReg, SrcContents),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot SaveBit),
            used_regs = Used bor SaveBit,
            regs = Regs1
        },
        SaveReg
    };
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = Used,
        regs = Regs0
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    SaveReg = first_avail(Avail),
    SaveBit = reg_bit(SaveReg),
    I1 = ?LOAD_WORD(SaveReg, Reg, 0),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, SaveReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot SaveBit),
            used_regs = Used bor SaveBit,
            regs = Regs1
        },
        SaveReg
    };
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {y_reg, Y}
) ->
    Reg = first_avail(Avail),
    AvailT = Avail band (bnot reg_bit(Reg)),
    I1 = ldr_y_reg(Reg, Y, AvailT),
    {BaseReg, Off} = ?CP,
    I2 = ?STORE_WORD(BaseReg, Reg, Off),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % ldr_y_reg clobbers first_avail(AvailT) as a hidden temp for loading Y_REGS pointer
    Regs1a = jit_regs:invalidate_reg(Regs0, Reg),
    Regs1 =
        case AvailT of
            0 -> Regs1a;
            _ -> jit_regs:invalidate_reg(Regs1a, first_avail(AvailT))
        end,
    State#state{stream = Stream1, regs = Regs1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Offset
) ->
    Reg = first_avail(Avail),
    {BaseReg1, Off1} = ?Y_REGS,
    I1 = ?LOAD_WORD(Reg, BaseReg1, Off1),
    I2 = ?ASM:addi(Reg, Reg, Offset * ?WORD_SIZE_BYTES),
    {BaseReg2, Off2} = ?Y_REGS,
    I3 = ?STORE_WORD(BaseReg2, Reg, Off2),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = State,
    Label
) ->
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    Offset = StreamModule:offset(Stream0),
    case lists:keyfind(Label, 1, Labels) of
        {Label, LabelOffset} ->
            % Label is already known, emit direct pc-relative address without relocation
            Rel = LabelOffset - Offset,
            I1 = pc_relative_address(Temp, Rel),
            I2 = ?STORE_WORD(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, regs = Regs1};
        false ->
            % Label not yet known, emit placeholder and add relocation
            % Reserve 8 bytes (2 x 32-bit instructions) with all-1s placeholder for flash programming
            % The relocation will replace these with the correct offset
            I1 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
            Reloc = {Label, Offset, {adr, Temp}},
            I2 = ?STORE_WORD(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, branches = [Reloc | Branches], regs = Regs1}
    end.

%% @doc Set the continuation to a given offset.
%% Return a reference so the offset will be updated with update_branches.
%% This is only used with OP_WAIT_TIMEOUT and the offset is after the current
%% code and not too far, so we can use a pc-relative address.
set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        branches = Branches,
        regs = Regs0
    } = State
) ->
    Temp = first_avail(Avail),
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    % Reserve 8 bytes with all-1s placeholder for flash programming
    I1 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
    Reloc = {OffsetRef, Offset, {adr, Temp}},
    % Store continuation (jit_state is in a1)
    I2 = ?STORE_WORD(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    {State#state{stream = Stream1, branches = [Reloc | Branches], regs = Regs1}, OffsetRef}.

%% @doc Implement a continuation entry point.
continuation_entry_point(State) ->
    State.

get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail,
        used_regs = UsedRegs0,
        regs = Regs0
    } = State
) ->
    Reg = first_avail(Avail),
    RegBit = reg_bit(Reg),
    % Load module pointer from jit_state (which is in a1)
    I1 = ?LOAD_WORD(Reg, ?JITSTATE_REG, ?JITSTATE_MODULE_OFFSET),
    I2 = ?ASM:lw(Reg, Reg, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot RegBit),
            used_regs = UsedRegs0 bor RegBit,
            regs = Regs1
        },
        Reg
    }.

%% @doc Perform an AND of a register with an immediate.
%% JIT currently calls this with two values: ?TERM_PRIMARY_CLEAR_MASK (-4) to
%% clear bits and ?TERM_BOXED_TAG_MASK (0x3F). We can avoid any literal pool
%% by using andi (which accepts sign-extended 12-bit immediates).
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    SrcReg
) when
    is_atom(SrcReg)
->
    I = ?ASM:and_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    Val
) when Val =:= (1 bsl (?BOOL_SHIFT + 1 - 8)) - 1 ->
    %% Optimization: clear top 8 bits by shift left then shift right
    I1 = ?ASM:slli(Reg, Reg, 8),
    I2 = ?ASM:srli(Reg, Reg, 8),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, {free, Reg}, Val
) when
    Val >= -2048 andalso Val =< 2047
->
    I = ?ASM:andi(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, bnot (Val)),
    Stream1 = State1#state.stream,
    % RISC-V doesn't have bics, use not + and
    I1 = ?ASM:not_(Temp, Temp),
    I2 = ?ASM:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    {free, Reg},
    Val
) when Avail =/= 0 ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:and_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    {State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, used_regs = UR, regs = Regs0} =
        State0,
    Reg,
    ?TERM_PRIMARY_CLEAR_MASK
) ->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I = ?ASM:andi(ResultReg, Reg, -4),
    Stream1 = StreamModule:append(State0#state.stream, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State0#state{
            stream = Stream1,
            available_regs = Avail band (bnot ResultBit),
            used_regs = UR bor ResultBit,
            regs = Regs1
        },
        ResultReg
    }.

or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I = ?ASM:or_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    Val >= -2048 andalso Val =< 2047
->
    I = ?ASM:ori(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
or_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    Reg,
    Val
) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:or_(Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

xor_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, SrcReg
) when
    is_atom(SrcReg)
->
    I = ?ASM:xor_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
xor_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    Val >= -2048 andalso Val =< 2047
->
    I = ?ASM:xori(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
xor_(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    Reg,
    Val
) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:xor_(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    Val >= 0 andalso Val =< 255
->
    I = ?ASM:addi(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    is_atom(Val)
->
    I = ?ASM:add(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0, Reg, Val) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:add(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) when
    Val >= -16#800, Val =< 16#7FF
->
    % RISC-V li can handle 12-bit signed immediates in a single instruction (addi)
    I = ?ASM:li(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    % For values outside 12-bit range, li generates a multi-instruction sequence
    % which is efficient enough, no need for literal pool
    I = ?ASM:li(Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    Val >= 0 andalso Val =< 255
->
    I1 = ?ASM:addi(Reg, Reg, -Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    is_atom(Val)
->
    I = ?ASM:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0, Reg, Val) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:sub(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 3) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:slli(Temp, Reg, 1),
    I2 = ?ASM:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 5) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:slli(Temp, Reg, 2),
    I2 = ?ASM:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 7) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:slli(Temp, Reg, 3),
    I2 = ?ASM:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 9) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:slli(Temp, Reg, 3),
    I2 = ?ASM:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 15) ->
    Temp = first_avail(Avail),
    I1 = ?ASM:slli(Temp, Reg, 4),
    I2 = ?ASM:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(
    #state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0,
    Reg,
    Val
) when is_integer(Val) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = ?ASM:mul(Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State1#state{
        stream = Stream2,
        available_regs = State1#state.available_regs bor reg_bit(Temp),
        regs = Regs1
    };
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I = ?ASM:mul(DestReg, DestReg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

%%
%% RISC-V implementation (no prolog/epilog needed due to 32 registers):
%%   Decrement reductions, store back.
%%   Branch if reductions != 0 to continuation.
%%   Otherwise, set continuation and schedule next process.
%%   continuation: [actual function body]
%%
%% Key insight: With 32 registers, RISC-V doesn't need prolog/epilog like ARM Thumb.
%% When reductions != 0, we branch directly to continue execution.
%% When reductions == 0, we schedule the next process, and resume at the continuation point.
%%
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0
) ->
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    % Load reduction count
    I1 = ?ASM:lw(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    % Decrement reduction count
    I2 = ?ASM:addi(Temp, Temp, -1),
    % Store back the decremented value
    I3 = ?ASM:sw(?JITSTATE_REG, Temp, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    I4 = <<16#FFFFFFFF:32/little>>,
    % Set continuation to the next instruction
    ADROffset = BNEOffset + byte_size(I4),
    % Use 8-byte placeholder (2 words of 0xFFFFFFFF) for pc_relative_address
    % This ensures we can always rewrite with either auipc alone (4 bytes) or auipc+addi (8 bytes)
    I5 = <<16#FFFFFFFF:32/little, 16#FFFFFFFF:32/little>>,
    I6 = ?STORE_WORD(?JITSTATE_REG, Temp, ?JITSTATE_CONTINUATION_OFFSET),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = ?ASM:bne(Temp, zero, NewOffset - BNEOffset),
    NewI5Offset = NewOffset - ADROffset,
    % Generate the new pc_relative_address instruction, padding with NOP if needed
    NewI5 =
        case pc_relative_address(Temp, NewI5Offset) of
            I when byte_size(I) =:= 4 ->
                % Only auipc, pad with NOP (4 bytes)
                <<I/binary, (?ASM:nop())/binary>>;
            I when byte_size(I) =:= 6 ->
                % auipc + c.addi, pad with c.nop (2 bytes)
                <<I/binary, (?ASM:c_nop())/binary>>;
            I when byte_size(I) =:= 8 ->
                % auipc + addi, no padding needed
                I
        end,
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    StreamN = Stream4,
    State3 = merge_used_regs(State2#state{stream = StreamN}, State1#state.used_regs),
    %% The schedule_next path is a tail call (dead end), so the register tracking
    %% from the non-taken path (State1) is what matters at the continuation.
    State3#state{regs = State1#state.regs}.

call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Avail
    } = State0,
    Label
) ->
    Temp = first_avail(Avail),
    % Load reduction count (jit_state is in a1)
    I1 = ?ASM:lw(Temp, ?JITSTATE_REG, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    % Decrement reduction count
    I2 = ?ASM:addi(Temp, Temp, -1),
    % Store back the decremented value
    I3 = ?ASM:sw(?JITSTATE_REG, Temp, ?JITSTATE_REDUCTIONCOUNT_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    % Use trampoline technique: branch if zero (eq) to skip over the long branch
    % If not zero, we want to continue execution at Label
    % If zero, we want to fall through to scheduling code

    % Look up label once to avoid duplicate lookup in helper
    LabelLookupResult = lists:keyfind(Label, 1, State0#state.labels),

    BccOffset = StreamModule:offset(Stream1),

    State4 =
        case LabelLookupResult of
            {Label, LabelOffset} ->
                % Label is known, check if we can optimize the conditional branch
                % After branch instruction
                Rel = LabelOffset - BccOffset,

                if
                    Rel >= -4096 andalso Rel =< 4094 andalso (Rel rem 2) =:= 0 ->
                        % Near branch: use direct conditional branch (RISC-V has ±4KB range)

                        % Branch if NOT zero (temp != 0)
                        I4 = ?ASM:bne(Temp, zero, Rel),
                        Stream2 = StreamModule:append(Stream1, I4),
                        State0#state{stream = Stream2};
                    true ->
                        % Far branch: use trampoline with helper
                        % Get the code block size for the far branch sequence that will follow

                        % RISC-V branch is 4 bytes
                        FarSeqOffset = BccOffset + 4,
                        {State1, FarCodeBlock} = branch_to_label_code(
                            State0, FarSeqOffset, Label, LabelLookupResult
                        ),
                        FarSeqSize = byte_size(FarCodeBlock),
                        % Skip over the far branch sequence if zero (temp == 0)
                        I4 = ?ASM:beq(Temp, zero, FarSeqSize + 4),
                        Stream2 = StreamModule:append(Stream1, I4),
                        Stream3 = StreamModule:append(Stream2, FarCodeBlock),
                        State1#state{stream = Stream3}
                end;
            false ->
                % Label not known, get the far branch size for the skip

                % RISC-V branch is 4 bytes
                FarSeqOffset = BccOffset + 4,
                {State1, FarCodeBlock} = branch_to_label_code(State0, FarSeqOffset, Label, false),
                FarSeqSize = byte_size(FarCodeBlock),
                I4 = ?ASM:beq(Temp, zero, FarSeqSize + 4),
                Stream2 = StreamModule:append(Stream1, I4),
                Stream3 = StreamModule:append(Stream2, FarCodeBlock),
                State1#state{stream = Stream3}
        end,
    State5 = set_continuation_to_label(State4, Label),
    call_primitive_last(State5, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

set_cp(#state{available_regs = Avail, used_regs = UsedRegs} = State0) ->
    TempReg = first_avail(Avail),
    TempBit = reg_bit(TempReg),
    % Reserve a temporary register for the offset BEFORE calling get_module_index
    % to avoid running out of available registers
    State0b = State0#state{
        available_regs = Avail band (bnot TempBit), used_regs = UsedRegs bor TempBit
    },
    % get module index (dynamically)
    {
        #state{stream_module = StreamModule, stream = Stream0} = State1,
        Reg
    } = get_module_index(
        State0b
    ),

    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = ?ASM:slli(Reg, Reg, 24),
    % Reserve space for offset load instruction
    % li can generate 1 instruction (4 bytes) for small immediates (< 2048)
    % or 2 instructions (8 bytes) for large immediates
    % Since we don't know the final CP value yet (it depends on code size),
    % we must always reserve 2 instructions (8 bytes) to be safe
    % The final CP value is (final_offset << 2), and final_offset is unknown
    % Use 0xFFFFFFFF placeholders for flash compatibility (can only flip 1->0)
    I2 = <<16#FFFFFFFF:32/little>>,
    I3 = <<16#FFFFFFFF:32/little>>,
    MOVOffset = Offset + byte_size(I1),
    % OR the module index with the offset (loaded in temp register)
    I4 = ?ASM:or_(Reg, TempReg),
    {BaseReg, Off} = ?CP,
    I5 = ?STORE_WORD(BaseReg, Reg, Off),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    State4 = free_native_register(State3, TempReg),
    {State4, MOVOffset, TempReg}.

rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    TempReg
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    CPValue = NewOffset bsl 2,
    NewMoveInstr = ?ASM:li(TempReg, CPValue),
    % We reserved 8 bytes (2 instructions) for the CP value
    % Pad with NOP if needed to maintain alignment
    PaddedInstr =
        case byte_size(NewMoveInstr) of
            2 ->
                <<NewMoveInstr/binary, (?ASM:nop())/binary,
                    (?ASM:c_nop())/binary>>;
            4 ->
                <<NewMoveInstr/binary, (?ASM:nop())/binary>>;
            6 ->
                <<NewMoveInstr/binary, (?ASM:c_nop())/binary>>;
            8 ->
                NewMoveInstr
        end,
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, PaddedInstr),
    State0#state{stream = Stream1}.

set_bs(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0,
    TermReg
) ->
    Temp = first_avail(Avail),
    {BaseReg1, Off1} = ?BS,
    I1 = ?STORE_WORD(BaseReg1, TermReg, Off1),
    I2 = ?ASM:li(Temp, 0),
    {BaseReg2, Off2} = ?BS_OFFSET,
    I3 = ?STORE_WORD(BaseReg2, Temp, Off2),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @param State current state
%% @param SortedLines line information, sorted by offset
%% @doc Build labels and line tables and encode a function that returns it.
%% In this case, the function returns the effective address of what immediately
%% follows.
%% @end
%% @return New state
%%-----------------------------------------------------------------------------
return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        labels = Labels
    } = State,
    SortedLines
) ->
    SortedLabels = lists:keysort(2, [
        {Label, LabelOffset}
     || {Label, LabelOffset} <- Labels, is_integer(Label)
    ]),

    I2 = ?ASM:ret(),
    % Assume total size is 10 bytes (8-byte I1 + 2-byte c.ret)
    % If actual is 8 bytes (6-byte I1 + 2-byte c.ret), we'll pad with 2 bytes
    I1 = pc_relative_address(a0, 10),
    Prologue = <<I1/binary, I2/binary>>,
    ProloguePadded =
        case byte_size(Prologue) of
            10 -> Prologue;
            % 2-byte padding
            8 -> <<Prologue/binary, 16#FFFF:16>>
        end,
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<ProloguePadded/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

%% @doc Generate PC-relative address calculation using AUIPC + ADDI
%% This replaces the ARM-style 'adr' pseudo-instruction with native RISC-V instructions
pc_relative_address(Rd, 0) ->
    % Simple case: just get current PC
    ?ASM:auipc(Rd, 0);
pc_relative_address(Rd, Offset) ->
    % PC-relative address calculation
    % Split offset into upper 20 bits and lower 12 bits
    % AUIPC can represent offsets in range: (-524288 << 12) to (524287 << 12)
    % Combined with ADDI: (-524288 << 12) - 2048 to (524287 << 12) + 2047
    Lower = Offset band 16#FFF,
    % Sign extend lower 12 bits
    LowerSigned =
        if
            Lower >= 16#800 -> Lower - 16#1000;
            true -> Lower
        end,
    % Compute upper 20 bits, adjusting if lower is negative
    % Use arithmetic right shift (bsr) which preserves sign in Erlang
    Upper =
        if
            LowerSigned < 0 ->
                (Offset bsr 12) + 1;
            true ->
                Offset bsr 12
        end,
    % Validate that Upper is in valid range for AUIPC
    if
        Upper < -16#80000; Upper > 16#7FFFF ->
            error({offset_out_of_range, Offset, Upper, -16#80000, 16#7FFFF});
        true ->
            ok
    end,
    case {Upper, LowerSigned} of
        {0, 0} ->
            % Zero offset
            ?ASM:auipc(Rd, 0);
        {0, _} ->
            % Only lower bits needed: auipc + addi
            AuipcInstr = ?ASM:auipc(Rd, 0),
            AddiInstr = ?ASM:addi(Rd, Rd, LowerSigned),
            <<AuipcInstr/binary, AddiInstr/binary>>;
        {_, 0} ->
            % Only upper bits needed
            ?ASM:auipc(Rd, Upper);
        {_, _} ->
            % Both upper and lower bits
            AuipcInstr = ?ASM:auipc(Rd, Upper),
            AddiInstr = ?ASM:addi(Rd, Rd, LowerSigned),
            <<AuipcInstr/binary, AddiInstr/binary>>
    end.

%% Helper function to generate str instruction with y_reg offset, handling large offsets
str_y_reg(SrcReg, Y, TempReg, _AvailableMask) when Y * ?WORD_SIZE_BYTES =< ?Y_OFFSET_LIMIT ->
    % Small offset - use immediate addressing
    {BaseReg, Off} = ?Y_REGS,
    I1 = ?LOAD_WORD(TempReg, BaseReg, Off),
    I2 = ?STORE_WORD(TempReg, SrcReg, Y * ?WORD_SIZE_BYTES),
    <<I1/binary, I2/binary>>;
str_y_reg(SrcReg, Y, TempReg1, AvailableMask) when AvailableMask =/= 0 ->
    % Large offset - use register arithmetic with second available register
    TempReg2 = first_avail(AvailableMask),
    Offset = Y * ?WORD_SIZE_BYTES,
    {BaseReg, Off} = ?Y_REGS,
    I1 = ?LOAD_WORD(TempReg1, BaseReg, Off),
    I2 = ?ASM:li(TempReg2, Offset),
    I3 = ?ASM:add(TempReg2, TempReg2, TempReg1),
    I4 = ?STORE_WORD(TempReg2, SrcReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>.

%% Helper function to generate ldr instruction with y_reg offset, handling large offsets
ldr_y_reg(DstReg, Y, AvailableMask) when AvailableMask =/= 0 andalso Y * ?WORD_SIZE_BYTES =< ?Y_OFFSET_LIMIT ->
    % Small offset - use immediate addressing
    TempReg = first_avail(AvailableMask),
    {BaseReg, Off} = ?Y_REGS,
    I1 = ?LOAD_WORD(TempReg, BaseReg, Off),
    I2 = ?LOAD_WORD(DstReg, TempReg, Y * ?WORD_SIZE_BYTES),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, AvailableMask) when AvailableMask =/= 0 ->
    % Large offset - use DstReg as second temp register for arithmetic
    TempReg = first_avail(AvailableMask),
    Offset = Y * ?WORD_SIZE_BYTES,
    {BaseReg, Off} = ?Y_REGS,
    I1 = ?LOAD_WORD(TempReg, BaseReg, Off),
    I2 = ?ASM:li(DstReg, Offset),
    I3 = ?ASM:add(DstReg, DstReg, TempReg),
    I4 = ?LOAD_WORD(DstReg, DstReg, 0),
    <<I1/binary, I2/binary, I3/binary, I4/binary>>;
ldr_y_reg(DstReg, Y, 0) when Y * ?WORD_SIZE_BYTES =< ?Y_OFFSET_LIMIT ->
    % Small offset, no registers available - use DstReg as temp
    {BaseReg, Off} = ?Y_REGS,
    I1 = ?LOAD_WORD(DstReg, BaseReg, Off),
    I2 = ?LOAD_WORD(DstReg, DstReg, Y * ?WORD_SIZE_BYTES),
    <<I1/binary, I2/binary>>.

reg_bit(a0) -> ?REG_BIT_A0;
reg_bit(a1) -> ?REG_BIT_A1;
reg_bit(a2) -> ?REG_BIT_A2;
reg_bit(a3) -> ?REG_BIT_A3;
reg_bit(a4) -> ?REG_BIT_A4;
reg_bit(a5) -> ?REG_BIT_A5;
reg_bit(a6) -> ?REG_BIT_A6;
reg_bit(a7) -> ?REG_BIT_A7;
reg_bit(t0) -> ?REG_BIT_T0;
reg_bit(t1) -> ?REG_BIT_T1;
reg_bit(t2) -> ?REG_BIT_T2;
reg_bit(t3) -> ?REG_BIT_T3;
reg_bit(t4) -> ?REG_BIT_T4;
reg_bit(t5) -> ?REG_BIT_T5;
reg_bit(t6) -> ?REG_BIT_T6.

regs_to_mask([]) -> 0;
regs_to_mask([ctx | T]) -> regs_to_mask(T);
regs_to_mask([imm | T]) -> regs_to_mask(T);
regs_to_mask([jit_state | T]) -> regs_to_mask(T);
regs_to_mask([offset | T]) -> regs_to_mask(T);
regs_to_mask([stack | T]) -> regs_to_mask(T);
regs_to_mask([Reg | T]) -> reg_bit(Reg) bor regs_to_mask(T).

%% first_avail returns the first available register from a bitmask.
%% Order matches AVAILABLE_REGS = [t6, t5, t4, t3, t2, t1, t0]
first_avail(Mask) when Mask band ?REG_BIT_T6 =/= 0 -> t6;
first_avail(Mask) when Mask band ?REG_BIT_T5 =/= 0 -> t5;
first_avail(Mask) when Mask band ?REG_BIT_T4 =/= 0 -> t4;
first_avail(Mask) when Mask band ?REG_BIT_T3 =/= 0 -> t3;
first_avail(Mask) when Mask band ?REG_BIT_T2 =/= 0 -> t2;
first_avail(Mask) when Mask band ?REG_BIT_T1 =/= 0 -> t1;
first_avail(Mask) when Mask band ?REG_BIT_T0 =/= 0 -> t0.

%% Convert bitmask to list, covering all register bits.
mask_to_list(0) -> [];
mask_to_list(Mask) -> mask_to_list_t6(Mask).

mask_to_list_t6(Mask) when Mask band ?REG_BIT_T6 =/= 0 -> [t6 | mask_to_list_t5(Mask)];
mask_to_list_t6(Mask) -> mask_to_list_t5(Mask).
mask_to_list_t5(Mask) when Mask band ?REG_BIT_T5 =/= 0 -> [t5 | mask_to_list_t4(Mask)];
mask_to_list_t5(Mask) -> mask_to_list_t4(Mask).
mask_to_list_t4(Mask) when Mask band ?REG_BIT_T4 =/= 0 -> [t4 | mask_to_list_t3(Mask)];
mask_to_list_t4(Mask) -> mask_to_list_t3(Mask).
mask_to_list_t3(Mask) when Mask band ?REG_BIT_T3 =/= 0 -> [t3 | mask_to_list_t2(Mask)];
mask_to_list_t3(Mask) -> mask_to_list_t2(Mask).
mask_to_list_t2(Mask) when Mask band ?REG_BIT_T2 =/= 0 -> [t2 | mask_to_list_t1(Mask)];
mask_to_list_t2(Mask) -> mask_to_list_t1(Mask).
mask_to_list_t1(Mask) when Mask band ?REG_BIT_T1 =/= 0 -> [t1 | mask_to_list_t0(Mask)];
mask_to_list_t1(Mask) -> mask_to_list_t0(Mask).
mask_to_list_t0(Mask) when Mask band ?REG_BIT_T0 =/= 0 -> [t0 | mask_to_list_a7(Mask)];
mask_to_list_t0(Mask) -> mask_to_list_a7(Mask).
mask_to_list_a7(Mask) when Mask band ?REG_BIT_A7 =/= 0 -> [a7 | mask_to_list_a6(Mask)];
mask_to_list_a7(Mask) -> mask_to_list_a6(Mask).
mask_to_list_a6(Mask) when Mask band ?REG_BIT_A6 =/= 0 -> [a6 | mask_to_list_a5(Mask)];
mask_to_list_a6(Mask) -> mask_to_list_a5(Mask).
mask_to_list_a5(Mask) when Mask band ?REG_BIT_A5 =/= 0 -> [a5 | mask_to_list_a4(Mask)];
mask_to_list_a5(Mask) -> mask_to_list_a4(Mask).
mask_to_list_a4(Mask) when Mask band ?REG_BIT_A4 =/= 0 -> [a4 | mask_to_list_a3(Mask)];
mask_to_list_a4(Mask) -> mask_to_list_a3(Mask).
mask_to_list_a3(Mask) when Mask band ?REG_BIT_A3 =/= 0 -> [a3 | mask_to_list_a2(Mask)];
mask_to_list_a3(Mask) -> mask_to_list_a2(Mask).
mask_to_list_a2(Mask) when Mask band ?REG_BIT_A2 =/= 0 -> [a2 | mask_to_list_a1(Mask)];
mask_to_list_a2(Mask) -> mask_to_list_a1(Mask).
mask_to_list_a1(Mask) when Mask band ?REG_BIT_A1 =/= 0 -> [a1 | mask_to_list_a0(Mask)];
mask_to_list_a1(Mask) -> mask_to_list_a0(Mask).
mask_to_list_a0(Mask) when Mask band ?REG_BIT_A0 =/= 0 -> [a0];
mask_to_list_a0(_Mask) -> [].

args_regs(Args) ->
    lists:map(
        fun
            ({free, {ptr, Reg}}) -> Reg;
            ({free, Reg}) when is_atom(Reg) -> Reg;
            ({free, Imm}) when is_integer(Imm) -> imm;
            (offset) -> imm;
            (ctx) -> ?CTX_REG;
            (jit_state) -> jit_state;
            (jit_state_tail_call) -> jit_state;
            (stack) -> stack;
            (Reg) when is_atom(Reg) -> Reg;
            (Imm) when is_integer(Imm) -> imm;
            ({ptr, Reg}) -> Reg;
            ({x_reg, _}) -> ?CTX_REG;
            ({y_reg, _}) -> ?CTX_REG;
            ({fp_reg, _}) -> ?CTX_REG;
            ({free, {x_reg, _}}) -> ?CTX_REG;
            ({free, {y_reg, _}}) -> ?CTX_REG;
            ({free, {fp_reg, _}}) -> ?CTX_REG;
            ({avm_int64_t, _}) -> imm
        end,
        Args
    ).

%%-----------------------------------------------------------------------------
%% @doc Add a label at the current offset.
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
add_label(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Label) ->
    Offset0 = StreamModule:offset(Stream0),
    Regs1 = jit_regs:invalidate_all(Regs0),
    add_label(State0#state{regs = Regs1}, Label, Offset0).

%%-----------------------------------------------------------------------------
%% @doc Add a label at a specific offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @param Offset the explicit offset for this label
%% @return Updated backend state
%%-----------------------------------------------------------------------------
add_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        jump_table_start = JumpTableStart,
        branches = Branches,
        labels = Labels
    } = State,
    Label,
    LabelOffset
) when is_integer(Label) ->
    % Patch the jump table entry immediately
    % Each jump table entry is AUIPC + JALR (8 bytes)
    JumpTableEntryOffset = JumpTableStart + Label * 8,

    % Calculate PC-relative offset from AUIPC instruction to target
    PCRelOffset = LabelOffset - JumpTableEntryOffset,

    % Split into upper 20 bits and lower 12 bits
    % RISC-V encodes: target = PC + (upper20 << 12) + sign_ext(lower12)
    % If lower12 >= 0x800, it's negative when sign-extended, so add 1 to upper
    Upper20 = (PCRelOffset + 16#800) bsr 12,
    Lower12 = PCRelOffset band 16#FFF,
    % Sign-extend lower 12 bits for JALR immediate
    Lower12Signed =
        if
            Lower12 >= 16#800 -> Lower12 - 16#1000;
            true -> Lower12
        end,

    % Encode AUIPC and JALR with computed offsets
    I1 = ?ASM:auipc(a3, Upper20),
    I2 = ?ASM:jalr(zero, a3, Lower12Signed),
    % Create 8-byte jump table entry
    JumpTableEntry = <<I1/binary, I2/binary>>,
    PaddedEntry =
        case byte_size(JumpTableEntry) of
            6 -> <<JumpTableEntry/binary, (?ASM:c_nop())/binary>>;
            8 -> JumpTableEntry
        end,

    Stream1 = StreamModule:replace(Stream0, JumpTableEntryOffset, PaddedEntry),

    % Eagerly patch any branches targeting this label
    {Stream2, RemainingBranches} = patch_branches_for_label(
        StreamModule,
        Stream1,
        Label,
        LabelOffset,
        Branches
    ),

    State#state{
        stream = Stream2, branches = RemainingBranches, labels = [{Label, LabelOffset} | Labels]
    };
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = [{Label, Offset} | Labels]}.

