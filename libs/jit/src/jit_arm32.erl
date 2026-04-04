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

-module(jit_arm32).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    flush/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
    jump_to_continuation/2,
    jump_to_offset/2,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_right_arith/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3,
    xor_/3
]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_ctx_register/0
]).
-endif.

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

%-define(ASSERT(Expr), true = Expr).
-define(ASSERT(Expr), ok).

%% ARM32 (AArch32 ARM mode) AAPCS32 ABI:
%%   - r0-r3: argument passing and return value (r0-r1 for 64-bit returns)
%%   - r4-r11: callee-saved (must preserve)
%%   - r12 (IP): intra-procedure-call scratch
%%   - r13 (SP), r14 (LR), r15 (PC): special purpose
%%
%% All ARM32 instructions are 32-bit (4 bytes). All registers are
%% directly accessible (no low-register restrictions like Thumb).
%% Branch instructions have +/-32MB range.

-type arm32_register() ::
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
    | r15.

-define(IS_GPR(Reg),
    (Reg =:= r0 orelse Reg =:= r1 orelse Reg =:= r2 orelse Reg =:= r3 orelse Reg =:= r4 orelse
        Reg =:= r5 orelse Reg =:= r6 orelse Reg =:= r7 orelse Reg =:= r8 orelse Reg =:= r9 orelse
        Reg =:= r10 orelse Reg =:= r11 orelse Reg =:= r12 orelse Reg =:= r13 orelse Reg =:= r14 orelse
        Reg =:= r15)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    jump_table_start :: non_neg_integer(),
    available_regs :: non_neg_integer(),
    used_regs :: non_neg_integer(),
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    literal_pool :: [{non_neg_integer(), arm32_register(), non_neg_integer()}],
    %% Register value tracking for optimization
    regs :: jit_regs:regs()
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, arm32_register()}.
-type value() :: immediate() | vm_register() | arm32_register() | {ptr, arm32_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_arm32_register() ::
    {free, arm32_register()} | arm32_register().

-type condition() ::
    {arm32_register(), '<', integer()}
    | {maybe_free_arm32_register(), '<', arm32_register()}
    | {integer(), '<', maybe_free_arm32_register()}
    | {maybe_free_arm32_register(), '==', integer()}
    | {maybe_free_arm32_register(), '!=', arm32_register() | integer()}
    | {'(int)', maybe_free_arm32_register(), '==', integer()}
    | {'(int)', maybe_free_arm32_register(), '!=', arm32_register() | integer()}
    | {'(bool)', maybe_free_arm32_register(), '==', false}
    | {'(bool)', maybe_free_arm32_register(), '!=', false}
    | {maybe_free_arm32_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, arm32_register()}, '==', {free, arm32_register()}}.

% ctx->e is 0x14
% ctx->x is 0x18
-define(CTX_REG, r0).
-define(NATIVE_INTERFACE_REG, r2).
-define(Y_REGS, {?CTX_REG, 16#14}).
-define(X_REG(N), {?CTX_REG, 16#18 + (N * 4)}).
-define(CP, {?CTX_REG, 16#5C}).
-define(FP_REGS, {?CTX_REG, 16#60}).
-define(BS, {?CTX_REG, 16#64}).
-define(BS_OFFSET, {?CTX_REG, 16#68}).
% JITSTATE is on stack, accessed via stack offset
% These macros now expect a register that contains the jit_state pointer
-define(JITSTATE_MODULE(Reg), {Reg, 0}).
-define(JITSTATE_CONTINUATION(Reg), {Reg, 16#4}).
-define(JITSTATE_REDUCTIONCOUNT(Reg), {Reg, 16#8}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 4}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

-define(JUMP_TABLE_ENTRY_SIZE, 8).

%% ARM32 register mappings

%% IP can be used as an additional scratch register
-define(IP_REG, r12).

%% Stack offset for function prolog: push {r1,r4,r5,r6,r7,r8,r9,r10,r11,lr}
%% r1 (JITSTATE_REG) is at SP+0 after push (10 registers = 40 bytes)
-define(STACK_OFFSET_JITSTATE, 0).

%% Offset of LR on the stack (last of 10 pushed registers)
-define(STACK_OFFSET_LR, 36).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).
-define(IS_SIGNED_OR_UNSIGNED_INT32_T(X),
    is_integer(X) andalso X >= -16#80000000 andalso X < 16#100000000
).

%% ARM32 (AArch32 ARM mode) register allocation:
%% - r0: context pointer (reserved)
%% - r1, r3: available (r1 saved/restored, r3 can be parameter)
%% - r2: parameter register (not available for scratch)
%% - r4-r11: callee-saved (saved/restored on entry/exit), all accessible
%% - r12: intra-procedure call scratch
%% - r13 (SP), r14 (LR), r15 (PC): special purpose
%% High registers (r8-r11) are fully accessible in ARM mode
-define(AVAILABLE_REGS, [r11, r10, r9, r8, r7, r6, r5, r4, r3, r1]).
-define(PARAMETER_REGS, [r0, r1, r2, r3]).
-define(SCRATCH_REGS, [r11, r10, r9, r8, r7, r6, r5, r4, r3, r2, r1, r0, r12]).

-define(REG_BIT_R0, (1 bsl 0)).
-define(REG_BIT_R1, (1 bsl 1)).
-define(REG_BIT_R2, (1 bsl 2)).
-define(REG_BIT_R3, (1 bsl 3)).
-define(REG_BIT_R4, (1 bsl 4)).
-define(REG_BIT_R5, (1 bsl 5)).
-define(REG_BIT_R6, (1 bsl 6)).
-define(REG_BIT_R7, (1 bsl 7)).
-define(REG_BIT_R8, (1 bsl 8)).
-define(REG_BIT_R9, (1 bsl 9)).
-define(REG_BIT_R10, (1 bsl 10)).
-define(REG_BIT_R11, (1 bsl 11)).
-define(REG_BIT_R12, (1 bsl 12)).
-define(REG_BIT_R13, (1 bsl 13)).
-define(REG_BIT_R14, (1 bsl 14)).
-define(REG_BIT_R15, (1 bsl 15)).

-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_R11 bor ?REG_BIT_R10 bor ?REG_BIT_R9 bor ?REG_BIT_R8 bor
        ?REG_BIT_R7 bor ?REG_BIT_R6 bor ?REG_BIT_R5 bor ?REG_BIT_R4 bor
        ?REG_BIT_R3 bor ?REG_BIT_R1)
).
-define(SCRATCH_REGS_MASK,
    (?REG_BIT_R11 bor ?REG_BIT_R10 bor ?REG_BIT_R9 bor ?REG_BIT_R8 bor
        ?REG_BIT_R7 bor ?REG_BIT_R6 bor ?REG_BIT_R5 bor ?REG_BIT_R4 bor
        ?REG_BIT_R3 bor ?REG_BIT_R2 bor ?REG_BIT_R1 bor ?REG_BIT_R0 bor ?REG_BIT_R12)
).

-include("jit_backend_dwarf_impl.hrl").

%%-----------------------------------------------------------------------------
%% @doc Return the word size in bytes, i.e. the sizeof(term) i.e.
%% sizeof(uintptr_t)
%%
%% C code equivalent is:
%% #if UINTPTR_MAX == UINT32_MAX
%%    #define TERM_BYTES 4
%% #elif UINTPTR_MAX == UINT64_MAX
%%    #define TERM_BYTES 8
%% #else
%%    #error "Term size must be either 32 bit or 64 bit."
%% #endif
%%
%% @end
%% @return Word size in bytes
%%-----------------------------------------------------------------------------
-spec word_size() -> 4 | 8.
word_size() -> 4.

%%-----------------------------------------------------------------------------
%% @doc Create a new backend state for provided variant, module and stream.
%% @end
%% @param Variant JIT variant to use (currently ?JIT_VARIANT_PIC)
%% @param StreamModule module to stream instructions
%% @param Stream stream state
%% @return New backend state
%%-----------------------------------------------------------------------------
-spec new(any(), module(), stream()) -> state().
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
        literal_pool = [],
        regs = jit_regs:new()
    }.

%%-----------------------------------------------------------------------------
%% @doc Access the stream object.
%% @end
%% @param State current backend state
%% @return The stream object
%%-----------------------------------------------------------------------------
-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @doc Get the current offset in the stream
%% @end
%% @param State current backend state
%% @return The current offset
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @doc Flush the current state, e.g. literal pools
%% @end
%% @param State current backend state
%% @return The flushed state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> state().
flush(#state{} = State) ->
    flush_literal_pool(State).

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger of breakpoint instruction. This is used for debugging
%% and not in production.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, jit_arm32_asm:bkpt(0)),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [arm32_register()].
used_regs(#state{used_regs = Used}) -> mask_to_list(Used).

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [arm32_register()].
available_regs(#state{available_regs = Available}) -> mask_to_list(Available).

%%-----------------------------------------------------------------------------
%% @doc Free native registers. The passed list of registers can contain
%% registers, pointer to registers or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Regs list of registers or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, used_regs = Used0} = State,
    Reg
) when
    is_atom(Reg)
->
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
-spec assert_all_native_free(state()) -> ok.
assert_all_native_free(State) ->
    0 = State#state.used_regs,
    ?AVAILABLE_REGS_MASK = State#state.available_regs,
    ok.

%%-----------------------------------------------------------------------------
%% @doc Emit the jump table at the beginning of the module. Branches will be
%% updated afterwards with update_branches/1. Emit branches for labels from
%% 0 (special entry for lines and labels information) to LabelsCount included
%% (special entry for OP_INT_CALL_END).
%%
%% On this platform, each jump table entry is 8 bytes.
%% ```
%% push {r1, r4, r5, r6, r7, r8, r9, r10, r11, lr}
%% b <label>
%% ```
%%
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_table(state(), pos_integer()) -> state().
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
    % Create jump table entry: push prolog registers + branch placeholder
    I1 = jit_arm32_asm:push([r1, r4, r5, r6, r7, r8, r9, r10, r11, lr]),
    I2 = <<16#FFFFFFFF:32>>,

    JumpEntry = <<I1/binary, I2/binary>>,
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
-spec patch_branch(module(), stream(), non_neg_integer(), any(), non_neg_integer()) -> stream().
patch_branch(StreamModule, Stream, Offset, Type, LabelOffset) ->
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            branch ->
                % ARM32 b has +/-32MB range, always fits
                jit_arm32_asm:b(al, Rel);
            {cond_branch, CC} ->
                % ARM32 conditional branch also has +/-32MB range
                jit_arm32_asm:b(CC, Rel);
            {add_pc, Reg} ->
                % add(al, Reg, pc, offset) where pc = Offset + 8
                jit_arm32_asm:add(al, Reg, pc, Rel - 8)
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
-spec patch_branches_for_label(
    module(),
    stream(),
    integer(),
    non_neg_integer(),
    [{integer(), non_neg_integer(), any()}]
) -> {stream(), [{integer(), non_neg_integer(), any()}]}.
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
-spec update_branches(state()) -> state().
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
-spec load_primitive_ptr(non_neg_integer(), arm32_register()) -> binary().
load_primitive_ptr(Primitive, TargetReg) ->
    case Primitive of
        0 ->
            jit_arm32_asm:ldr(al, TargetReg, {?NATIVE_INTERFACE_REG, 0});
        N when N * 4 =< 4095 ->
            jit_arm32_asm:ldr(al, TargetReg, {?NATIVE_INTERFACE_REG, N * 4});
        N ->
            % For very large primitive numbers, try encoding N*4 directly or fall back to N+shift
            NTimes4 = N * 4,
            case jit_arm32_asm:encode_imm(NTimes4) of
                {_, _} ->
                    I1 = jit_arm32_asm:mov(al, TargetReg, NTimes4),
                    I2 = jit_arm32_asm:ldr(al, TargetReg, {?NATIVE_INTERFACE_REG, TargetReg}),
                    <<I1/binary, I2/binary>>;
                false ->
                    case jit_arm32_asm:encode_imm(N) of
                        {_, _} ->
                            I1 = jit_arm32_asm:mov(al, TargetReg, N),
                            I2 = jit_arm32_asm:lsl(al, TargetReg, TargetReg, 2),
                            I3 = jit_arm32_asm:ldr(
                                al, TargetReg, {?NATIVE_INTERFACE_REG, TargetReg}
                            ),
                            <<I1/binary, I2/binary, I3/binary>>;
                        false ->
                            error({primitive_index_too_large, N})
                    end
            end
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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), arm32_register()}.
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
    % Use an available register for loading the function pointer
    TempReg = first_avail(Available),
    TempBit = reg_bit(TempReg),
    PrepCall = load_primitive_ptr(Primitive, TempReg),
    Stream1 = StreamModule:append(Stream0, PrepCall),
    Regs0 = jit_regs:invalidate_reg(State#state.regs, TempReg),
    StateCall = State#state{
        stream = Stream1,
        available_regs = Available band (bnot TempBit),
        used_regs = Used bor TempBit,
        regs = Regs0
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
-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
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
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    ScratchMask = ?AVAILABLE_REGS_MASK band (bnot (ArgsMask bor ParamMask)),
    Temp = first_avail(ScratchMask),
    TempBit = reg_bit(Temp),
    AvailableRegs1 = ScratchMask band (bnot TempBit),
    UsedRegs = ?AVAILABLE_REGS_MASK band (bnot AvailableRegs1),
    PrepCall = load_primitive_ptr(Primitive, Temp),
    Stream1 = StreamModule:append(Stream0, PrepCall),

    % Invalidate register cache for Temp since it now holds the function pointer,
    % not whatever value the cache may have recorded for it.
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Temp),
    State1 = State0#state{
        stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs, regs = Regs1
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

    % Handle arguments differently for 5+ arguments - use direct call without register preservation
    State4 =
        case Args1 of
            [Arg1, Arg2, Arg3, Arg4, Arg5 | Arg6L] ->
                State2 =
                    case Arg6L of
                        [Arg6] ->
                            set_stack_args(State1, Arg5, Arg6);
                        [] ->
                            set_stack_args(State1, Arg5, undefined)
                    end,
                State3 = set_registers_args(State2, [Arg1, Arg2, Arg3, Arg4], 8),
                #state{stream = Stream2} = State3,
                % Call the function pointer directly
                Call = jit_arm32_asm:blx(al, Temp),
                Stream3 = StreamModule:append(Stream2, Call),
                % Deallocate stack space that was allocated for 5+ arguments
                DeallocateArgs = jit_arm32_asm:add(al, sp, sp, 8),
                Stream4 = StreamModule:append(Stream3, DeallocateArgs),
                % Return: pop prolog registers and return
                PopCode = jit_arm32_asm:pop([r1, r4, r5, r6, r7, r8, r9, r10, r11, pc]),
                Stream5 = StreamModule:append(Stream4, PopCode),
                State3#state{stream = Stream5};
            [FirstArg, jit_state | ArgsT] ->
                % For 4 or fewer args, use tail call
                ArgsForTailCall = [FirstArg, jit_state_tail_call | ArgsT],
                State2 = set_registers_args(State1, ArgsForTailCall, 0),
                tail_call_with_jit_state_registers_only(State2, Temp)
        end,
    State5 = State4#state{
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State4#state.regs)
    },
    flush_literal_pool(State5).

%%-----------------------------------------------------------------------------
%% @doc Tail call to address in register, restoring prolog registers including
%% jit_state in r1. Only use when target function expects jit_state as second parameter.
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
    % Standard tail call for 4 or fewer arguments
    % First restore LR from stack (so target function can return properly)
    % Choose temp register to avoid conflict with Reg
    TempReg =
        case Reg of
            r7 -> r6;
            _ -> r7
        end,
    % Load saved LR to temp
    RestoreLRToTemp = jit_arm32_asm:ldr(al, TempReg, {sp, ?STACK_OFFSET_LR}),
    % Store function pointer (pipeline friendly)
    OverwriteLR = jit_arm32_asm:str(al, Reg, {sp, ?STACK_OFFSET_LR}),
    % Move saved LR to LR register
    RestoreLR = jit_arm32_asm:mov(al, lr, TempReg),
    % Pop prolog registers: {r1,r4-r11,lr} where lr is now target address
    % This restores jit_state in r1 and branches to target via pc
    PopCode = jit_arm32_asm:pop([r1, r4, r5, r6, r7, r8, r9, r10, r11, pc]),

    Code = <<RestoreLRToTemp/binary, OverwriteLR/binary, RestoreLR/binary, PopCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
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
        used_regs = UsedRegs0,
        regs = Regs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_arm32_asm:cmp(al, Reg, ?CTX_REG),
    I3 =
        case Reg of
            % Return value is already in r0
            r0 -> <<>>;
            % Move to r0 (return register)
            _ -> jit_arm32_asm:mov(al, r0, Reg)
        end,
    I4 = jit_arm32_asm:pop([r1, r4, r5, r6, r7, r8, r9, r10, r11, pc]),
    I2 = jit_arm32_asm:b(eq, 4 + byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    RegBit = reg_bit(Reg),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs0 bor RegBit,
        used_regs = UsedRegs0 band (bnot RegBit),
        regs = Regs1
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump to a label. The offset of the relocation is saved and will
%% be updated with `update_branches/1`.
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
    State2 = State1#state{stream = Stream1, regs = jit_regs:invalidate_all(State1#state.regs)},
    flush_literal_pool(State2).

%% @doc Emit an unconditional branch to an absolute offset.
-spec jump_to_offset(state(), non_neg_integer()) -> state().
jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    CodeBlock = branch_to_offset_code(State, Offset, TargetOffset),
    Stream1 = StreamModule:append(Stream0, CodeBlock),
    State2 = State#state{stream = Stream1, regs = jit_regs:invalidate_all(State#state.regs)},
    flush_literal_pool(State2).

%%-----------------------------------------------------------------------------
%% @doc Jump to address in continuation pointer register
%% The continuation points to a function prologue, so we need to compute
%% the target address using PIC and use function epilogue to jump.
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
    % ARM32 PIC implementation:
    % 1. Use mov(al, Temp, pc) to read PC (gives current instruction address + 8)
    % 2. Add PC to OffsetReg to get intermediate value
    % 3. Load base offset immediate into temp
    % 4. Add base offset to get final target address
    % 5. Use function epilogue pattern to jump

    MovPCOffset = StreamModule:offset(Stream0),
    % mov Temp, pc gives MovPCOffset + 8
    I1 = jit_arm32_asm:mov(al, Temp, pc),

    % Add PC to OffsetReg: OffsetReg = OffsetReg + Temp
    I2 = jit_arm32_asm:add(al, OffsetReg, OffsetReg, Temp),

    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),

    % Calculate what we need to add: BaseOffset - (MovPCOffset + 8)
    ImmediateValue = BaseOffset - MovPCOffset - 8,

    % Generate mov_immediate to load the calculated base offset into Temp
    State1 = mov_immediate(State0#state{stream = Stream1}, Temp, ImmediateValue),

    % Add base offset to get final target address: OffsetReg = OffsetReg + Temp
    I3 = jit_arm32_asm:add(al, OffsetReg, OffsetReg, Temp),

    % Function epilogue pattern:
    % Load saved LR to temp register (LR is at sp+?STACK_OFFSET_LR)
    I4 = jit_arm32_asm:ldr(al, Temp, {sp, ?STACK_OFFSET_LR}),
    % Store target address to LR position on stack
    I5 = jit_arm32_asm:str(al, OffsetReg, {sp, ?STACK_OFFSET_LR}),
    % Move saved LR to LR register
    I6 = jit_arm32_asm:mov(al, lr, Temp),
    % Pop prolog registers: {r1,r4-r11,lr} where lr is now target address
    % This restores jit_state in r1 and branches to target via pc
    I7 = jit_arm32_asm:pop([r1, r4, r5, r6, r7, r8, r9, r10, r11, pc]),

    Code = <<I3/binary, I4/binary, I5/binary, I6/binary, I7/binary>>,
    Stream2 = StreamModule:append(State1#state.stream, Code),
    % Free all registers as this is a terminal instruction
    State2 = State1#state{stream = Stream2, available_regs = ?AVAILABLE_REGS_MASK, used_regs = 0},
    flush_literal_pool(State2).

branch_to_offset_code(_State, Offset, TargetOffset) ->
    % ARM32 b has +/-32MB range, always use direct branch
    Rel = TargetOffset - Offset,
    jit_arm32_asm:b(al, Rel).

branch_to_label_code(State, Offset, Label, {Label, LabelOffset}) ->
    CodeBlock = branch_to_offset_code(State, Offset, LabelOffset),
    {State, CodeBlock};
branch_to_label_code(
    #state{branches = Branches} = State0, Offset, Label, false
) ->
    % ARM32 b has +/-32MB range, emit a placeholder and add relocation
    CodeBlock = <<16#FFFFFFFF:32>>,
    Reloc = {Label, Offset, branch},
    State1 = State0#state{branches = [Reloc | Branches]},
    {State1, CodeBlock}.

%%-----------------------------------------------------------------------------
%% @doc Emit an if block, i.e. emit a test of a condition and conditionnally
%% execute a block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockFn function to emit the block that may be executed
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_block(state(), condition() | {'and', [condition()]}, fun((state()) -> state())) -> state().
if_block(
    #state{stream_module = StreamModule} = State0,
    {'and', CondList},
    BlockFn
) ->
    {Replacements, State1} = lists:foldl(
        fun(Cond, {AccReplacements, AccState}) ->
            Offset = StreamModule:offset(AccState#state.stream),
            {NewAccState, CC, ReplaceDelta} = if_block_cond(AccState, Cond),
            {[{Offset + ReplaceDelta, CC} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, CC}, AccStream) ->
            BranchOffset = OffsetAfter - ReplacementOffset,
            NewBranchInstr = jit_arm32_asm:b(CC, BranchOffset),
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
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = jit_arm32_asm:b(CC, BranchOffset),
    Stream3 = StreamModule:replace(Stream2, Offset + BranchInstrOffset, NewBranchInstr),
    State3 = merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs}.

%%-----------------------------------------------------------------------------
%% @doc Emit an if else block, i.e. emit a test of a condition and
%% conditionnally execute a block or another block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockTrueFn function to emit the block that is executed if condition is true
%% @param BlockFalseFn function to emit the block that is executed if condition is false
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockTrueFn,
    BlockFalseFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional branch to skip the else block (will be replaced)
    ElseJumpOffset = StreamModule:offset(Stream2),
    ?ASSERT(byte_size(jit_arm32_asm:b(al, 0)) =:= 4),
    ElseJumpInstr = <<16#FFFFFFFF:32>>,
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = jit_arm32_asm:b(CC, ElseBranchOffset),
    Stream4 = StreamModule:replace(Stream3, Offset + BranchInstrOffset, NewBranchInstr),
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
    NewElseJumpInstr = jit_arm32_asm:b(al, FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    State4 = merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs),
    MergedRegs = jit_regs:merge(State2#state.regs, State3#state.regs),
    State4#state{regs = MergedRegs}.

-spec if_block_cond(state(), condition()) ->
    {state(), jit_arm32_asm:cc(), non_neg_integer()}.
%% Handle {Val, '<', Reg} which means "Val < Reg" or "Reg > Val"
%% For immediate value 0-255
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_arm32_asm:cmp(al, Reg, Val),
    %% Branch if less than or equal (to skip the block when Val >= Reg)
    I2 = jit_arm32_asm:b(le, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, le, byte_size(I1)};
%% Handle {Val, '<', Reg} for values > 255, need to load into temp register
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val), Available =/= 0 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    Offset0 = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    Offset1 = StreamModule:offset(Stream1),
    I1 = jit_arm32_asm:cmp(al, Reg, Temp),
    %% Branch if less than or equal (to skip the block when Val >= Reg)
    I2 = jit_arm32_asm:b(le, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream2 = StreamModule:append(Stream1, Code),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, le, Offset1 - Offset0 + byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '<', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Compare register with 0
    I1 = jit_arm32_asm:cmp(al, Reg, 0),
    %% Branch if positive (N flag clear)
    CC = pl,
    ?ASSERT(byte_size(jit_arm32_asm:b(pl, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Val >= 0, Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_arm32_asm:cmp(al, Reg, Val),
    % ge = greater than or equal
    CC = ge,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Available =/= 0 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    Offset0 = StreamModule:offset(Stream0),
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    Offset1 = StreamModule:offset(Stream1),
    I1 = jit_arm32_asm:cmp(al, Reg, Temp),
    % ge = greater than or equal
    CC = ge,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, 16#FFFFFFFF:32>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, CC, Offset1 - Offset0 + byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, available_regs = Available} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val), Available =/= 0 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    State1 = mov_immediate(State0, Temp, Val),
    Stream0 = State1#state.stream,
    I1 = jit_arm32_asm:cmp(al, Reg, Temp),
    % le = less than or equal (branch when Val >= Reg, i.e., NOT Val < Reg)
    CC = le,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream1, regs = Regs2},
    {State3, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_arm32_asm:cmp(al, Reg, RegB),
    % ge = greater than or equal
    CC = ge,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    %% Compare register with 0
    I1 = jit_arm32_asm:cmp(al, Reg, 0),
    %% Branch if not equal
    CC = ne,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
%% Delegate (int) forms to regular forms since we only have 32-bit words
if_block_cond(State, {'(int)', RegOrTuple, '==', 0}) ->
    if_block_cond(State, {RegOrTuple, '==', 0});
if_block_cond(State, {'(int)', RegOrTuple, '==', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '==', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when (is_integer(Val) andalso Val >= 0 andalso Val =< 255) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_arm32_asm:cmp(al, Reg, Val),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(State, {'(int)', RegOrTuple, '!=', Val}) when is_integer(Val) ->
    if_block_cond(State, {RegOrTuple, '!=', Val});
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) andalso Val >= 0 andalso Val =< 255 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_arm32_asm:cmp(al, Reg, Val),
    CC = ne,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {{free, RegA}, '==', {free, RegB}}
) ->
    % Compare two free registers: cmp RegA, RegB; beq <target>
    I1 = jit_arm32_asm:cmp(al, RegA, RegB),
    CC = ne,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = State0#state{stream = Stream1},
    State2 = if_block_free_reg({free, RegA}, State1),
    State3 = if_block_free_reg({free, RegB}, State2),
    {State3, CC, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val), Available =/= 0 ->
    Temp = first_avail(Available),
    Offset0 = StreamModule:offset(Stream0),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    Offset1 = StreamModule:offset(Stream1),
    I1 = jit_arm32_asm:cmp(al, Reg, Temp),
    CC = ne,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, 16#FFFFFFFF:32>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, CC, Offset1 - Offset0 + byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Available} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val), Available =/= 0 ->
    Temp = first_avail(Available),
    Offset0 = StreamModule:offset(Stream0),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    State1 = mov_immediate(State0, Temp, Val),
    Stream1 = State1#state.stream,
    Offset1 = StreamModule:offset(Stream1),
    I1 = jit_arm32_asm:cmp(al, Reg, Temp),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, 16#FFFFFFFF:32>>),
    State2 = if_block_free_reg(RegOrTuple, State1),
    Regs2 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{stream = Stream2, regs = Regs2},
    {State3, CC, Offset1 - Offset0 + byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = _Available
    } = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bit 0 using TST instruction
    I1 = jit_arm32_asm:tst(al, Reg, 1),
    % branch if not zero (bit was 1/true) - skip block when bool == false means skip when true
    CC = ne,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bit 0 using TST instruction
    I1 = jit_arm32_asm:tst(al, Reg, 1),
    % branch if zero (bit was 0/false) - skip block when bool != false means skip when false
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, 16#FFFFFFFF:32>>),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, CC, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) when Available =/= 0 ->
    Temp = first_avail(Available),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bits using TST instruction
    {TestCode, BranchCond} =
        case jit_arm32_asm:encode_imm(Val) of
            false ->
                % Value not encodable as ARM immediate, use mov+tst
                TestCode0 = jit_arm32_asm:mov(al, Temp, Val),
                TestCode1 = jit_arm32_asm:tst(al, Reg, Temp),
                {<<TestCode0/binary, TestCode1/binary>>, eq};
            _ ->
                % Value encodable as ARM immediate, use tst directly
                TestCode0 = jit_arm32_asm:tst(al, Reg, Val),
                {TestCode0, eq}
        end,
    ?ASSERT(byte_size(jit_arm32_asm:b(BranchCond, 0)) =:= 4),
    Code = <<TestCode/binary, 16#FFFFFFFF:32>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
    {State2, BranchCond, byte_size(TestCode)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available
    } = State0,
    {Reg, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg), Available =/= 0 ->
    Temp = first_avail(Available),
    % Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_arm32_asm:and_(al, Temp, Reg, 16#F),
    I2 = jit_arm32_asm:cmp(al, Temp, 16#F),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, 16#FFFFFFFF:32>>),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Temp),
    State1 = State0#state{stream = Stream1, regs = Regs1},
    {State1, CC, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', 16#F, '!=', 16#F}
) when ?IS_GPR(Reg) ->
    % Special case Reg & ?TERM_IMMED_TAG_MASK != ?TERM_INTEGER_TAG
    I1 = jit_arm32_asm:and_(al, Reg, Reg, 16#F),
    I2 = jit_arm32_asm:cmp(al, Reg, 16#F),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, 16#FFFFFFFF:32>>),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Reg),
    State1 = State0#state{stream = Stream1, regs = Regs1},
    State2 = if_block_free_reg(RegTuple, State1),
    {State2, CC, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = Available
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg), Available =/= 0 ->
    Temp = first_avail(Available),
    TempBit = reg_bit(Temp),
    AT = Available band (bnot TempBit),
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    I1 = jit_arm32_asm:mov(al, Temp, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State1 = State0#state{stream = Stream1},
    {State2, Temp} = and_(State1#state{available_regs = AT}, {free, Temp}, Mask),
    Stream2 = State2#state.stream,
    % Compare with value
    I2 = jit_arm32_asm:cmp(al, Temp, Val),
    Stream3 = StreamModule:append(Stream2, I2),
    OffsetAfter = StreamModule:offset(Stream3),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream4 = StreamModule:append(Stream3, <<16#FFFFFFFF:32>>),
    Regs3 = jit_regs:invalidate_reg(State2#state.regs, Temp),
    State3 = State2#state{
        stream = Stream4, available_regs = State2#state.available_regs bor TempBit, regs = Regs3
    },
    {State3, CC, OffsetAfter - OffsetBefore};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    {State1, Reg} = and_(State0, RegTuple, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_arm32_asm:cmp(al, Reg, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    CC = eq,
    ?ASSERT(byte_size(jit_arm32_asm:b(CC, 0)) =:= 4),
    Stream3 = StreamModule:append(Stream2, <<16#FFFFFFFF:32>>),
    State3 = State1#state{stream = Stream3},
    State4 = if_block_free_reg(RegTuple, State3),
    {State4, CC, OffsetAfter - OffsetBefore}.

-spec if_block_free_reg(arm32_register() | {free, arm32_register()}, state()) -> state().
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

-spec merge_used_regs(state(), non_neg_integer()) -> state().
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
-spec shift_right(#state{}, maybe_free_arm32_register(), non_neg_integer()) ->
    {#state{}, arm32_register()}.
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_arm32_asm:lsr(al, Reg, Reg, Shift),
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
    Bit = reg_bit(ResultReg),
    I = jit_arm32_asm:lsr(al, ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot Bit),
            used_regs = UR bor Bit,
            regs = Regs1
        },
        ResultReg
    }.

-spec shift_right_arith(#state{}, maybe_free_arm32_register(), non_neg_integer()) ->
    {#state{}, arm32_register()}.
shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_arm32_asm:asr(al, Reg, Reg, Shift),
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
    Bit = reg_bit(ResultReg),
    I = jit_arm32_asm:asr(al, ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail band (bnot Bit),
            used_regs = UR bor Bit,
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
    I = jit_arm32_asm:lsl(al, Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a function pointer with arguments. This function converts
%% arguments and passes them following the backend ABI convention.
%% @end
%% @param State current backend state
%% @param FuncPtrTuple either {free, Reg} or {primitive, PrimitiveIndex}
%% @param Args arguments to pass to the function
%% @return Updated backend state and return register
%%-----------------------------------------------------------------------------
-spec call_func_ptr(state(), {free, arm32_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), arm32_register()}.
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
    AvailableRegs0 = mask_to_list(AvailableRegs0Mask),
    UsedRegs0 = mask_to_list(UsedRegs0Mask),
    FreeRegs = lists:flatmap(
        fun
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegsBase = [?CTX_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],

    % Calculate available registers for potential padding
    FreeGPRegs = FreeRegs -- (FreeRegs -- ?AVAILABLE_REGS),
    AvailableRegs1 = FreeGPRegs ++ AvailableRegs0,

    % Add padding register if odd number to maintain 8-byte stack alignment per ARM AAPCS
    SavedRegs =
        case (length(SavedRegsBase) rem 2) =:= 1 of
            true when AvailableRegs1 /= [] ->
                [PaddingReg | _] = AvailableRegs1,
                SavedRegsBase ++ [PaddingReg];
            _ ->
                PaddingReg = undefined,
                SavedRegsBase
        end,

    Stream1 = push_registers(SavedRegs, StreamModule, Stream0),

    % Set up arguments following ARM AAPCS calling convention
    % First four args are passed in r0-r3, 5th and 6th on the stack.
    Args1 = lists:map(
        fun(Arg) ->
            case Arg of
                offset -> StreamModule:offset(Stream1);
                _ -> Arg
            end
        end,
        Args
    ),
    {RegArgs0, StackArgs} =
        case Args1 of
            [Arg1, Arg2, Arg3, Arg4 | StackArgs0] -> {[Arg1, Arg2, Arg3, Arg4], StackArgs0};
            _ -> {Args, []}
        end,
    RegArgsRegs = lists:flatmap(fun arg_to_reg_list/1, RegArgs0),
    StackArgsRegs = lists:flatmap(fun arg_to_reg_list/1, StackArgs),

    % We pushed registers to stack, so we can use these registers we saved
    % and the currently available registers to push values to the stack.
    SetArgsPushStackAvailableArgs = (UsedRegs1 -- (RegArgsRegs ++ StackArgsRegs)) ++ AvailableRegs0,
    State1 = State0#state{
        available_regs = jit_regs:regs_to_mask(SetArgsPushStackAvailableArgs, fun reg_bit/1),
        used_regs = jit_regs:regs_to_mask(
            ?AVAILABLE_REGS -- SetArgsPushStackAvailableArgs, fun reg_bit/1
        ),
        stream = Stream1
    },
    State2 =
        case StackArgs of
            [] -> State1;
            [Arg5] -> set_stack_args(State1, Arg5, undefined);
            [Arg5, Args6] -> set_stack_args(State1, Arg5, Args6)
        end,

    SetArgsRegsOnlyAvailableArgs = mask_to_list(State2#state.available_regs),
    ParameterRegs = parameter_regs(RegArgs0),
    {Stream3, SetArgsAvailableRegs, FuncPtrReg, RegArgs} =
        case FuncPtrTuple of
            {free, FuncPtrReg0} ->
                % If FuncPtrReg is in parameter regs, we must swap it with a free reg.
                case lists:member(FuncPtrReg0, ParameterRegs) of
                    true ->
                        case SetArgsRegsOnlyAvailableArgs -- ParameterRegs of
                            [] ->
                                % Swap SetArgsRegsOnlyAvailableArgs with a reg used in RegArgs0
                                % that is not in ParameterRegs
                                [NewArgReg | _] = SetArgsRegsOnlyAvailableArgs,
                                [FuncPtrReg1 | _] = RegArgsRegs -- ParameterRegs,
                                MovInstr1 = jit_arm32_asm:mov(al, NewArgReg, FuncPtrReg1),
                                MovInstr2 = jit_arm32_asm:mov(al, FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailableArgs1 =
                                    (SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg1]) ++
                                        [FuncPtrReg0],
                                RegArgs1 = replace_reg(RegArgs0, FuncPtrReg1, NewArgReg),
                                {
                                    StreamModule:append(
                                        State2#state.stream, <<MovInstr1/binary, MovInstr2/binary>>
                                    ),
                                    SetArgsAvailableArgs1,
                                    FuncPtrReg1,
                                    RegArgs1
                                };
                            [FuncPtrReg1 | _] ->
                                MovInstr = jit_arm32_asm:mov(al, FuncPtrReg1, FuncPtrReg0),
                                SetArgsAvailableArgs1 =
                                    (SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg1]) ++
                                        [FuncPtrReg0],
                                {
                                    StreamModule:append(State2#state.stream, MovInstr),
                                    SetArgsAvailableArgs1,
                                    FuncPtrReg1,
                                    RegArgs0
                                }
                        end;
                    false ->
                        SetArgsAvailableArgs1 = SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg0],
                        {State2#state.stream, SetArgsAvailableArgs1, FuncPtrReg0, RegArgs0}
                end;
            {primitive, Primitive} ->
                [FuncPtrReg0 | _] = SetArgsRegsOnlyAvailableArgs -- ParameterRegs,
                SetArgsAvailableRegs1 = SetArgsRegsOnlyAvailableArgs -- [FuncPtrReg0],
                PrepCall = load_primitive_ptr(Primitive, FuncPtrReg0),
                Stream2 = StreamModule:append(State2#state.stream, PrepCall),
                {Stream2, SetArgsAvailableRegs1, FuncPtrReg0, RegArgs0}
        end,

    State3 = State2#state{
        available_regs = jit_regs:regs_to_mask(SetArgsAvailableRegs, fun reg_bit/1),
        used_regs = jit_regs:regs_to_mask(?AVAILABLE_REGS -- SetArgsAvailableRegs, fun reg_bit/1),
        stream = Stream3
    },

    StackOffset =
        case StackArgs of
            [] -> length(SavedRegs) * 4;
            _ -> length(SavedRegs) * 4 + 8
        end,
    State4 = set_registers_args(State3, RegArgs, ParameterRegs, StackOffset),
    Stream4 = State4#state.stream,

    % Call the function pointer (using BLX for call with return)
    Call = jit_arm32_asm:blx(al, FuncPtrReg),
    Stream5 = StreamModule:append(Stream4, Call),

    % For result, we need a free register (including FuncPtrReg) but ideally
    % not the one used for padding. If none are available (all 8 registers
    % were pushed to the stack), we write the result to the stack position
    % of FuncPtrReg
    % When 5+ args are used, stack args space (8 bytes) is between SP and the
    % pushed registers, so we need to add 8 to the offset.
    StackArgsExtra =
        case StackArgs of
            [] -> 0;
            _ -> 8
        end,
    {Stream6, UsedRegs2} =
        case length(SavedRegs) of
            8 when element(1, FuncPtrTuple) =:= free ->
                % We use original FuncPtrReg then as we know it's available.
                % Calculate stack offset: ARM push stores registers sorted by
                % register number, so offset is the index in the sorted list.
                ResultReg = element(2, FuncPtrTuple),
                StoreResultStackOffset =
                    pushed_reg_stack_offset(ResultReg, SavedRegs) + StackArgsExtra,
                StoreResult = jit_arm32_asm:str(al, r0, {sp, StoreResultStackOffset}),
                {StreamModule:append(Stream5, StoreResult), [ResultReg | UsedRegs1]};
            8 when PaddingReg =/= undefined ->
                % We use PaddingReg then as we know it's available.
                % Calculate stack offset: ARM push stores registers sorted by
                % register number, so offset is the index in the sorted list.
                ResultReg = PaddingReg,
                StoreResultStackOffset =
                    pushed_reg_stack_offset(ResultReg, SavedRegs) + StackArgsExtra,
                StoreResult = jit_arm32_asm:str(al, r0, {sp, StoreResultStackOffset}),
                {StreamModule:append(Stream5, StoreResult), [PaddingReg | UsedRegs1]};
            _ ->
                % Use any free that is not in SavedRegs
                [ResultReg | _] = AvailableRegs1 -- SavedRegs,
                MoveResult = jit_arm32_asm:mov(al, ResultReg, r0),
                {StreamModule:append(Stream5, MoveResult), [ResultReg | UsedRegs1]}
        end,

    % Deallocate stack space if we allocated it for 5+ arguments
    Stream7 =
        case length(Args) >= 5 of
            true ->
                DeallocateArgs = jit_arm32_asm:add(al, sp, sp, 8),
                StreamModule:append(Stream6, DeallocateArgs);
            false ->
                Stream6
        end,

    Stream8 = pop_registers(lists:reverse(SavedRegs), StreamModule, Stream7),

    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    {
        State4#state{
            stream = Stream8,
            available_regs = jit_regs:regs_to_mask(AvailableRegs3, fun reg_bit/1),
            used_regs = jit_regs:regs_to_mask(UsedRegs2, fun reg_bit/1),
            regs = Regs1
        },
        ResultReg
    }.

arg_to_reg_list({free, {ptr, Reg}}) -> [Reg];
arg_to_reg_list({free, Reg}) when is_atom(Reg) -> [Reg];
arg_to_reg_list(Reg) when is_atom(Reg) -> [Reg];
arg_to_reg_list(_) -> [].

push_registers(SavedRegs, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    StreamModule:append(Stream0, jit_arm32_asm:push(SavedRegs));
push_registers([], _StreamModule, Stream0) ->
    Stream0.

pop_registers(SavedRegs, StreamModule, Stream0) when length(SavedRegs) > 0 ->
    StreamModule:append(Stream0, jit_arm32_asm:pop(SavedRegs));
pop_registers([], _StreamModule, Stream0) ->
    Stream0.

%% @doc Calculate the stack offset of a register within a push frame.
%% ARM push (STMDB) stores registers sorted by register number regardless of
%% list order, so the offset is the index of the register in the sorted list.
pushed_reg_stack_offset(Reg, SavedRegs) ->
    RegNum = jit_arm32_asm:reg_to_num(Reg),
    Index = length([R || R <- SavedRegs, jit_arm32_asm:reg_to_num(R) < RegNum]),
    Index * 4.

%% @doc Handle 5th and optionally 6th arguments on stack.
%% For 5 args: push 5th arg at sp+0 with 4-byte padding at sp+4 for 8-byte alignment
%% For 6 args: push 5th arg at sp+0, 6th arg at sp+4 (2×4 bytes = 8-byte aligned, no padding)
set_stack_args(
    #state{stream_module = StreamModule, stream = Stream0} = State0, Arg5, Arg6
) ->
    % Decrement stack pointer by 8 bytes once
    I1 = jit_arm32_asm:sub(al, sp, sp, 8),
    Stream1 = StreamModule:append(Stream0, I1),

    % Handle Arg6 if present (goes at sp+4)
    State1 =
        case Arg6 of
            undefined ->
                % 5 arguments: no 6th arg to handle
                State0#state{stream = Stream1};
            {free, Reg6} when is_atom(Reg6) ->
                % 6 arguments: Arg6 is already in native register, store directly and free
                I2 = jit_arm32_asm:str(al, Reg6, {sp, 4}),
                StreamB = StreamModule:append(Stream1, I2),
                free_native_register(State0#state{stream = StreamB}, Reg6);
            _ ->
                % 6 arguments: store Arg6 at sp+4
                % Handle {free, NonNativeReg} by unwrapping
                ActualArg6 =
                    case Arg6 of
                        {free, InnerArg6} -> InnerArg6;
                        Other6 -> Other6
                    end,
                {StateA, Reg6} = move_to_native_register(
                    State0#state{stream = Stream1}, ActualArg6
                ),
                StreamA = StateA#state.stream,
                I2 = jit_arm32_asm:str(al, Reg6, {sp, 4}),
                StreamB = StreamModule:append(StreamA, I2),
                free_native_register(StateA#state{stream = StreamB}, Reg6)
        end,

    % Handle Arg5 (always present, always goes at sp+0)
    State2 =
        case Arg5 of
            {free, Reg5} when is_atom(Reg5) ->
                % Arg5 is already in native register, store directly and free
                I3 = jit_arm32_asm:str(al, Reg5, {sp, 0}),
                Stream3 = StreamModule:append(State1#state.stream, I3),
                free_native_register(State1#state{stream = Stream3}, Reg5);
            _ ->
                % Move Arg5 to register, store, and free
                % Handle {free, NonNativeReg} by unwrapping
                ActualArg5 =
                    case Arg5 of
                        {free, InnerArg5} -> InnerArg5;
                        Other5 -> Other5
                    end,
                {StateTemp, Reg5} = move_to_native_register(State1, ActualArg5),
                StreamTemp = StateTemp#state.stream,
                I3 = jit_arm32_asm:str(al, Reg5, {sp, 0}),
                Stream3 = StreamModule:append(StreamTemp, I3),
                free_native_register(StateTemp#state{stream = Stream3}, Reg5)
        end,
    State2.

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
    UsedRegsList = mask_to_list(UsedRegsMask),
    AvailableScratchGP = ((?SCRATCH_REGS -- ParamRegs) -- ArgsRegs) -- UsedRegsList,
    State1 = set_registers_args0(
        State0, Args, ArgsRegs, ParamRegs, AvailableScratchGP, StackOffset
    ),
    Stream1 = State1#state.stream,
    NewUsedRegsList = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> lists:delete(Reg, AccUsed);
            ({free, Reg}, AccUsed) -> lists:delete(Reg, AccUsed);
            (_, AccUsed) -> AccUsed
        end,
        UsedRegsList,
        Args
    ),
    FinalAvail = ?AVAILABLE_REGS -- ParamRegs -- NewUsedRegsList,
    FinalUsed = ParamRegs ++ (NewUsedRegsList -- ParamRegs),
    State1#state{
        stream = Stream1,
        available_regs = jit_regs:regs_to_mask(FinalAvail, fun reg_bit/1),
        used_regs = jit_regs:regs_to_mask(FinalUsed, fun reg_bit/1)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

% AAPCS32: 64-bit arguments require double-word alignment (even register number)
parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([{avm_int64_t, _} | T], [r0, r1 | Rest], Acc) ->
    parameter_regs0(T, Rest, [r1, r0 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [r1, r2, r3 | Rest], Acc) ->
    parameter_regs0(T, Rest, [r3, r2 | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [r2, r3 | Rest], Acc) ->
    parameter_regs0(T, Rest, [r3, r2 | Acc]);
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
% Handle 64-bit arguments that need two registers according to AAPCS32
set_registers_args0(
    State,
    [{avm_int64_t, Value} | ArgsT],
    ArgsRegs,
    ParamRegs,
    AvailGP,
    StackOffset
) when is_integer(Value) ->
    LowPart = Value band 16#FFFFFFFF,
    HighPart = (Value bsr 32) band 16#FFFFFFFF,
    set_registers_args0(
        State, [LowPart, HighPart | ArgsT], [imm | ArgsRegs], ParamRegs, AvailGP, StackOffset
    );
% ctx is special as we need it to access x_reg/y_reg/fp_reg and we don't
% want to replace it
set_registers_args0(
    State, [Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, StackOffset
) ->
    ?ASSERT(not lists:member(?CTX_REG, ArgsRegs)),
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
            I = jit_arm32_asm:mov(al, Avail, ParamReg),
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
    #state{stream_module = StreamModule, stream = Stream0} = State, jit_state, ParamReg, StackOffset
) ->
    JitStateOffset = ?STACK_OFFSET_JITSTATE + StackOffset,
    I = jit_arm32_asm:ldr(al, ParamReg, {sp, JitStateOffset}),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
% For tail calls, jit_state will be restored by pop - skip generating load instruction
set_registers_args1(State, jit_state_tail_call, r1, _StackOffset) ->
    State;
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {x_reg, extra},
    Reg,
    _StackOffset
) ->
    I = jit_arm32_asm:ldr(al, Reg, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, Reg, _StackOffset
) ->
    I = jit_arm32_asm:ldr(al, Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Source}, Reg, _StackOffset
) ->
    I = jit_arm32_asm:ldr(al, Reg, {Source, 0}),
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
    I = jit_arm32_asm:mov(al, Reg, ArgReg),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
set_registers_args1(State, Value, Reg, _StackOffset) when ?IS_SIGNED_OR_UNSIGNED_INT32_T(Value) ->
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
-spec move_to_vm_register(state(), Src :: value() | vm_register(), Dest :: vm_register()) ->
    state().
move_to_vm_register(#state{regs = Regs0} = State, Src, Dest) ->
    VmLoc = vm_dest_to_contents(Dest),
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
    I1 = jit_arm32_asm:str(al, Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_arm32_asm:str(al, Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_arm32_asm:str(al, Src, {Reg, 0}),
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
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp1),
    % str_y_reg may use first_avail(AT) as hidden temp for large offsets
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
    Avail1 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail1),
    AT = Avail1 band (bnot reg_bit(Temp2)),
    I1 = jit_arm32_asm:mov(al, Temp2, N),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, YCode/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp1), Temp2),
    % str_y_reg may use first_avail(AT) as hidden temp for large offsets
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
    I1 = jit_arm32_asm:mov(al, Temp, N),
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
    I1 = jit_arm32_asm:ldr(al, Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {x_reg, X}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = jit_arm32_asm:ldr(al, Temp, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{available_regs = AR0, regs = Regs1};
move_to_vm_register_emit(#state{available_regs = AR0} = State0, {ptr, Reg}, Dest) ->
    Temp = first_avail(AR0),
    AT = AR0 band (bnot reg_bit(Temp)),
    I1 = jit_arm32_asm:ldr(al, Temp, {Reg, 0}),
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
    I1 = jit_arm32_asm:ldr(al, Temp1, ?FP_REGS),
    I2 = jit_arm32_asm:ldr(al, Temp2, {Reg, 4}),
    case Variant band ?JIT_VARIANT_FLOAT32 of
        0 ->
            % Double precision: write both 32-bit parts
            I3 = jit_arm32_asm:str(al, Temp2, {Temp1, F * 8}),
            I4 = jit_arm32_asm:ldr(al, Temp2, {Reg, 8}),
            I5 = jit_arm32_asm:str(al, Temp2, {Temp1, F * 8 + 4}),
            Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>;
        _ ->
            % Single precision: write only first 32-bit part
            I3 = jit_arm32_asm:str(al, Temp2, {Temp1, F * 4}),
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
-spec move_array_element(
    state(),
    arm32_register(),
    non_neg_integer() | arm32_register(),
    vm_register() | arm32_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) andalso Index * 4 =< 4095 ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:ldr(al, Temp, {Reg, Index * 4}),
    I2 = jit_arm32_asm:str(al, Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, available_regs = Avail} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    Temp1 = first_avail(Avail),
    Temp2 = first_avail(Avail band (bnot reg_bit(Temp1))),
    % For large offsets, use max offset (4092) in ldr + remainder in temp register
    Offset = Index * 4,
    LdrOffset = 4092,
    Remainder = Offset - LdrOffset,
    % Load offset remainder into temp register and add to base
    State1 = mov_immediate(State, Temp1, Remainder),
    Stream1 = State1#state.stream,
    % add Temp1, Reg (Temp1 = Temp1 + Reg)
    I1 = jit_arm32_asm:add(al, Temp1, Temp1, Reg),
    % ldr Temp2, [Temp1, #4092]
    I2 = jit_arm32_asm:ldr(al, Temp2, {Temp1, LdrOffset}),
    % str Temp2, [r0, #X*4]
    I3 = jit_arm32_asm:str(al, Temp2, ?X_REG(X)),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(State1#state.regs, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs1, Temp1), Temp2),
    State1#state{stream = Stream2, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) andalso Index * 4 =< 4095 ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:ldr(al, Temp, {Reg, Index * 4}),
    I2 = jit_arm32_asm:str(al, Temp, {Dest, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, available_regs = Avail} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    % For large offsets, use max offset (4092) in ldr + remainder in temp register
    Offset = Index * 4,
    LdrOffset = 4092,
    Remainder = Offset - LdrOffset,
    % Load offset remainder into temp register and add to base
    State1 = mov_immediate(State, Temp, Remainder),
    Stream1 = State1#state.stream,
    I1 = jit_arm32_asm:add(al, Temp, Temp, Reg),
    I2 = jit_arm32_asm:ldr(al, Temp, {Temp, LdrOffset}),
    I3 = jit_arm32_asm:str(al, Temp, {Dest, 0}),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{stream = Stream2, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) andalso Index * 4 =< 4095 ->
    Temp1 = first_avail(Avail),
    Avail1 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail1),
    AT = Avail1 band (bnot reg_bit(Temp2)),
    I1 = jit_arm32_asm:ldr(al, Temp2, {Reg, Index * 4}),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp1), Temp2),
    Regs2 = jit_regs:invalidate_vm_loc(Regs1, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{
        stream_module = StreamModule, available_regs = Avail
    } =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    Temp1 = first_avail(Avail),
    Avail1 = Avail band (bnot reg_bit(Temp1)),
    Temp2 = first_avail(Avail1),
    AT = Avail1 band (bnot reg_bit(Temp2)),
    % For large offsets, use max offset (4092) in ldr + remainder in temp register
    Offset = Index * 4,
    LdrOffset = 4092,
    Remainder = Offset - LdrOffset,
    State1 = mov_immediate(State, Temp2, Remainder),
    Stream1 = State1#state.stream,
    I1 = jit_arm32_asm:add(al, Temp2, Temp2, Reg),
    I2 = jit_arm32_asm:ldr(al, Temp2, {Temp2, LdrOffset}),
    YCode = str_y_reg(Temp2, Y, Temp1, AT),
    Code = <<I1/binary, I2/binary, YCode/binary>>,
    Stream2 = StreamModule:append(Stream1, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State1#state.regs, Temp1), Temp2),
    Regs2 = jit_regs:invalidate_vm_loc(Regs1, {y_reg, Y}),
    State1#state{stream = Stream2, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    I1 = jit_arm32_asm:ldr(al, Reg, {Reg, Index * 4}),
    YCode = str_y_reg(Reg, Y, Temp, AT),
    Code = <<I1/binary, YCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    Regs2 = jit_regs:invalidate_vm_loc(Regs1, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_arm32_asm:ldr(al, Dest, {Reg, Index * 4}),
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
    I1 = jit_arm32_asm:lsl(al, IndexReg, IndexReg, 2),
    I2 = jit_arm32_asm:ldr(al, IndexReg, {Reg, IndexReg}),
    I3 = jit_arm32_asm:str(al, IndexReg, ?X_REG(X)),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
    I1 = jit_arm32_asm:lsl(al, IndexReg, IndexReg, 2),
    I2 = jit_arm32_asm:ldr(al, IndexReg, {Reg, IndexReg}),
    I3 = jit_arm32_asm:str(al, IndexReg, {PtrReg, 0}),
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
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
    I1 = jit_arm32_asm:lsl(al, IndexReg, IndexReg, 2),
    I2 = jit_arm32_asm:ldr(al, IndexReg, {Reg, IndexReg}),
    Temp = first_avail(AvailableRegs0),
    TempBit = reg_bit(Temp),
    AT = mask_to_list(AvailableRegs0 band (bnot TempBit)),
    Code = str_y_reg(IndexReg, Y, Temp, AT),
    I3 = Code,
    Bit = reg_bit(IndexReg),
    AvailableRegs1 = AvailableRegs0 bor Bit,
    UsedRegs1 = UsedRegs0 band (bnot Bit),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary>>
    ),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), IndexReg),
    Regs2 = jit_regs:invalidate_vm_loc(Regs1, {y_reg, Y}),
    State#state{
        available_regs = AvailableRegs1,
        used_regs = UsedRegs1,
        stream = Stream1,
        regs = Regs2
    }.

%% @doc move reg[x] to a vm or native register
-spec get_array_element(state(), arm32_register() | {free, arm32_register()}, non_neg_integer()) ->
    {state(), arm32_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg},
    Index
) when Index * 4 =< 4095 ->
    I1 = jit_arm32_asm:ldr(al, Reg, {Reg, Index * 4}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        available_regs = Avail
    } = State,
    {free, Reg},
    Index
) ->
    Temp = first_avail(Avail),
    % For large offsets, split into ldr immediate (max 4092) + remainder in temp register
    Offset = Index * 4,
    Remainder = Offset - 4092,
    State1 = mov_immediate(State, Temp, Remainder),
    Stream1 = State1#state.stream,
    I1 = jit_arm32_asm:add(al, Reg, Reg, Temp),
    I2 = jit_arm32_asm:ldr(al, Reg, {Reg, 4092}),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Reg),
    {State1#state{stream = Stream2, regs = Regs1}, Reg};
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
) when Index * 4 =< 4095 ->
    ElemReg = first_avail(Avail),
    ElemBit = reg_bit(ElemReg),
    I1 = jit_arm32_asm:ldr(al, ElemReg, {Reg, Index * 4}),
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
    };
get_array_element(
    #state{
        stream_module = StreamModule,
        available_regs = Avail,
        used_regs = UsedRegs0
    } = State,
    Reg,
    Index
) ->
    ElemReg = first_avail(Avail),
    ElemBit = reg_bit(ElemReg),
    Avail1 = Avail band (bnot ElemBit),
    Temp = first_avail(Avail1),
    % For large offsets, split into ldr immediate (max 4092) + remainder in temp register
    Offset = Index * 4,
    Remainder = Offset - 4092,
    % Load offset remainder into temp register
    State1 = mov_immediate(State, Temp, Remainder),
    Stream1 = State1#state.stream,
    I1 = jit_arm32_asm:add(al, Temp, Temp, Reg),
    I2 = jit_arm32_asm:ldr(al, ElemReg, {Temp, 4092}),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, ElemReg),
    {
        State1#state{
            stream = Stream2,
            available_regs = Avail1,
            used_regs = UsedRegs0 bor ElemBit,
            regs = Regs1
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | arm32_register(), arm32_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) andalso Index < 32 ->
    I1 = jit_arm32_asm:str(al, ValueReg, {Reg, Index * 4}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, available_regs = Avail} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    Temp = first_avail(Avail),
    % For large offsets, split into str immediate (max 4092) + remainder in temp register
    Offset = Index * 4,
    Remainder = Offset - 4092,
    % Load offset remainder into temp register
    State1 = mov_immediate(State0, Temp, Remainder),
    Stream1 = State1#state.stream,
    I1 = jit_arm32_asm:add(al, Temp, Temp, Reg),
    I2 = jit_arm32_asm:str(al, ValueReg, {Temp, 4092}),
    Stream2 = StreamModule:append(Stream1, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{stream = Stream2, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:mov(al, Temp, IndexReg),
    I2 = jit_arm32_asm:lsl(al, Temp, Temp, 2),
    I3 = jit_arm32_asm:str(al, ValueReg, {Reg, Temp}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    State0,
    Value,
    Reg,
    Index
) when not ?IS_GPR(Value) andalso ?IS_GPR(Reg) ->
    {State1, Temp} = copy_to_native_register(State0, Value),
    State2 = move_to_array_element(State1, Temp, Reg, Index),
    free_native_register(State2, Temp).

move_to_array_element(
    State,
    Value,
    BaseReg,
    IndexReg,
    Offset
) when is_integer(IndexReg) andalso is_integer(Offset) ->
    move_to_array_element(State, Value, BaseReg, IndexReg + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail} = State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:add(al, Temp, IndexReg, Offset),
    I2 = jit_arm32_asm:lsl(al, Temp, Temp, 2),
    I3 = jit_arm32_asm:str(al, ValueReg, {BaseReg, Temp}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    State0,
    Value,
    BaseReg,
    IndexReg,
    Offset
) ->
    {State1, ValueReg} = copy_to_native_register(State0, Value),
    Temp = first_avail(State1#state.available_regs),
    I1 = jit_arm32_asm:add(al, Temp, IndexReg, Offset),
    I2 = jit_arm32_asm:lsl(al, Temp, Temp, 2),
    I3 = jit_arm32_asm:str(al, ValueReg, {BaseReg, Temp}),
    Stream1 = (State1#state.stream_module):append(
        State1#state.stream, <<I1/binary, I2/binary, I3/binary>>
    ),
    State2 = State1#state{stream = Stream1},
    free_native_register(State2, ValueReg).

-spec move_to_native_register(state(), value() | cp) -> {state(), arm32_register()}.
move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = value_to_contents(Value),
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
    Bit = reg_bit(Reg),
    I1 = jit_arm32_asm:ldr(al, Reg, ?CP),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Avail band (bnot Bit),
            regs = Regs1
        },
        Reg
    };
move_to_native_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {ptr, Reg},
    _Contents
) when is_atom(Reg) ->
    I1 = jit_arm32_asm:ldr(al, Reg, {Reg, 0}),
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
    Bit = reg_bit(Reg),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    State1 = State0#state{
        used_regs = Used bor Bit,
        available_regs = Avail band (bnot Bit),
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
    Bit = reg_bit(Reg),
    I1 = jit_arm32_asm:ldr(al, Reg, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Avail band (bnot Bit),
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
    Bit = reg_bit(Reg),
    I1 = jit_arm32_asm:ldr(al, Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            used_regs = Used bor Bit,
            available_regs = Avail band (bnot Bit),
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
    Bit = reg_bit(Reg),
    AvailT = Avail band (bnot Bit),
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
            used_regs = Used bor Bit,
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
    BitA = reg_bit(RegA),
    Avail1 = Avail band (bnot BitA),
    RegB = first_avail(Avail1),
    BitB = reg_bit(RegB),
    AvailT = Avail1 band (bnot BitB),
    I1 = jit_arm32_asm:ldr(al, RegB, ?FP_REGS),
    I2 = jit_arm32_asm:ldr(al, RegA, {RegB, F * 8}),
    I3 = jit_arm32_asm:ldr(al, RegB, {RegB, F * 8 + 4}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailT, used_regs = Used bor BitA bor BitB},
        {fp, RegA, RegB}
    }.

-spec move_to_native_register(state(), value(), arm32_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_arm32_asm:mov(al, RegDst, RegSrc),
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
    I1 = jit_arm32_asm:ldr(al, RegDst, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, extra}, RegDst
) ->
    I1 = jit_arm32_asm:ldr(al, RegDst, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_arm32_asm:ldr(al, RegDst, ?X_REG(X)),
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
    I1 = jit_arm32_asm:ldr(al, RegB, ?FP_REGS),
    I2 = jit_arm32_asm:ldr(al, RegA, {RegB, F * 8}),
    I3 = jit_arm32_asm:ldr(al, RegB, {RegB, F * 8 + 4}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), arm32_register()}.
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
    I1 = jit_arm32_asm:mov(al, SaveReg, Reg),
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
    I1 = jit_arm32_asm:ldr(al, SaveReg, {Reg, 0}),
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
    I2 = jit_arm32_asm:str(al, Reg, ?CP),
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
    I1 = jit_arm32_asm:ldr(al, Reg, ?Y_REGS),
    I2 = jit_arm32_asm:add(al, Reg, Reg, Offset * 4),
    I3 = jit_arm32_asm:str(al, Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        offset = JumpTableOffset,
        available_regs = Avail
    } = State,
    Label
) ->
    Temp1 = first_avail(Avail),
    Temp2 = first_avail(Avail band (bnot reg_bit(Temp1))),
    % Calculate jump table entry offset
    JumpTableEntryOffset = (Label * ?JUMP_TABLE_ENTRY_SIZE) + JumpTableOffset,

    MovPCOffset = StreamModule:offset(Stream0),
    % mov Temp1, pc gives MovPCOffset + 8 in ARM32
    I1 = jit_arm32_asm:mov(al, Temp1, pc),
    Stream1 = StreamModule:append(Stream0, I1),

    % Calculate what we need to load: JumpTableEntryOffset - (MovPCOffset + 8)
    ImmediateValue = JumpTableEntryOffset - MovPCOffset - 8,

    % Generate mov_immediate to load the calculated offset
    State1 = mov_immediate(State#state{stream = Stream1}, Temp2, ImmediateValue),

    % Add PC + offset, load jit_state, and store continuation
    I2 = jit_arm32_asm:add(al, Temp2, Temp2, Temp1),
    I3 = jit_arm32_asm:ldr(al, Temp1, {sp, ?STACK_OFFSET_JITSTATE}),
    I4 = jit_arm32_asm:str(al, Temp2, ?JITSTATE_CONTINUATION(Temp1)),

    Code = <<I2/binary, I3/binary, I4/binary>>,
    Stream2 = StreamModule:append(State1#state.stream, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State#state.regs, Temp1), Temp2),
    State1#state{stream = Stream2, regs = Regs1}.

%% @doc Set the continuation to a given offset
%% Return a reference so the offset will be updated with update_branches
%% This is only used with OP_WAIT_TIMEOUT and the offset is after the current
%% code and not too far, so we can use add(Rd, pc, #imm) instruction.
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
    TempJitState = first_avail(Avail band (bnot reg_bit(Temp))),
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    % In ARM32, use add(al, Temp, pc, #imm) as placeholder - will be patched
    % pc reads as Offset + 8, so add(al, Temp, pc, 0) gives Offset + 8
    % We need to patch this to add(al, Temp, pc, TargetOffset - Offset - 8)
    ?ASSERT(byte_size(jit_arm32_asm:add(al, Temp, pc, 0)) =:= 4),
    I1 = <<16#FFFFFFFF:32>>,
    Reloc = {OffsetRef, Offset, {add_pc, Temp}},
    % Load jit_state pointer from stack, then store continuation
    I2 = jit_arm32_asm:ldr(al, TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    I3 = jit_arm32_asm:str(al, Temp, ?JITSTATE_CONTINUATION(TempJitState)),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), TempJitState),
    {State#state{stream = Stream1, branches = [Reloc | Branches], regs = Regs1}, OffsetRef}.

%% @doc Implement a continuation entry point.
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State
) ->
    % ARM32: all instructions are 4-byte aligned, no alignment needed
    Prolog = jit_arm32_asm:push([r1, r4, r5, r6, r7, r8, r9, r10, r11, lr]),
    Stream1 = StreamModule:append(Stream0, Prolog),
    State#state{stream = Stream1}.

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
    Avail1 = Avail band (bnot RegBit),
    TempJitState = first_avail(Avail1),
    % Load jit_state pointer from stack, then load module
    I1a = jit_arm32_asm:ldr(al, TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    I1b = jit_arm32_asm:ldr(al, Reg, ?JITSTATE_MODULE(TempJitState)),
    I2 = jit_arm32_asm:ldr(al, Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1a/binary, I1b/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempJitState), Reg),
    {
        State#state{
            stream = Stream1,
            available_regs = Avail1,
            used_regs = UsedRegs0 bor RegBit,
            regs = Regs1
        },
        Reg
    }.

%% @doc Perform an AND of a register with an immediate.
%% JIT currently calls this with two values: ?TERM_PRIMARY_CLEAR_MASK (-4) to
%% clear bits and ?TERM_BOXED_TAG_MASK (0x3F). We can avoid any literal pool
%% by using BIC for -4.
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    SrcReg
) when
    is_atom(SrcReg)
->
    I = jit_arm32_asm:and_(al, Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    {free, Reg},
    16#FFFFFF
) ->
    I1 = jit_arm32_asm:lsl(al, Reg, Reg, 8),
    I2 = jit_arm32_asm:lsr(al, Reg, Reg, 8),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0,
    {free, Reg},
    Val
) when Avail =/= 0 andalso Val < 0 andalso Val >= -256 ->
    Temp = first_avail(Avail),
    TempBit = reg_bit(Temp),
    AT = Avail band (bnot TempBit),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, bnot (Val)),
    Stream1 = State1#state.stream,
    I = jit_arm32_asm:bic(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State1#state.regs, Reg), Temp),
    {State1#state{available_regs = AT bor TempBit, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail} = State0,
    {free, Reg},
    Val
) when Avail =/= 0 ->
    Temp = first_avail(Avail),
    TempBit = reg_bit(Temp),
    AT = Avail band (bnot TempBit),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_arm32_asm:and_(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(State1#state.regs, Reg), Temp),
    {State1#state{available_regs = AT bor TempBit, stream = Stream2, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = 0} = State0,
    {free, Reg},
    Val
) when Val < 0 andalso Val >= -256 ->
    % No available registers, use r0 as temp and save it to r12
    Stream0 = State0#state.stream,
    % Save r0 to r12
    Save = jit_arm32_asm:mov(al, ?IP_REG, r0),
    Stream1 = StreamModule:append(Stream0, Save),
    % Load immediate value into r0
    State1 = mov_immediate(State0#state{stream = Stream1}, r0, bnot (Val)),
    Stream2 = State1#state.stream,
    % Perform BIC operation
    I = jit_arm32_asm:bic(al, Reg, Reg, r0),
    Stream3 = StreamModule:append(Stream2, I),
    % Restore r0 from r12
    Restore = jit_arm32_asm:mov(al, r0, ?IP_REG),
    Stream4 = StreamModule:append(Stream3, Restore),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Reg),
    {State0#state{stream = Stream4, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = 0, regs = Regs0} = State0,
    {free, Reg},
    Val
) ->
    % No available registers, use r0 as temp and save it to r12
    Stream0 = State0#state.stream,
    % Save r0 to r12
    Save = jit_arm32_asm:mov(al, ?IP_REG, r0),
    Stream1 = StreamModule:append(Stream0, Save),
    % Load immediate value into r0
    State1 = mov_immediate(State0#state{stream = Stream1}, r0, Val),
    Stream2 = State1#state.stream,
    % Perform AND operation
    I = jit_arm32_asm:and_(al, Reg, Reg, r0),
    Stream3 = StreamModule:append(Stream2, I),
    % Restore r0 from r12
    Restore = jit_arm32_asm:mov(al, r0, ?IP_REG),
    Stream4 = StreamModule:append(Stream3, Restore),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream4, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, available_regs = Avail, used_regs = UR, regs = Regs0} =
        State0,
    Reg,
    ?TERM_PRIMARY_CLEAR_MASK
) ->
    ResultReg = first_avail(Avail),
    ResultBit = reg_bit(ResultReg),
    I1 = jit_arm32_asm:lsr(al, ResultReg, Reg, 2),
    I2 = jit_arm32_asm:lsl(al, ResultReg, ResultReg, 2),
    Stream1 = StreamModule:append(State0#state.stream, <<I1/binary, I2/binary>>),
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
    I = jit_arm32_asm:orr(al, Reg, Reg, SrcReg),
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
    I = jit_arm32_asm:orr(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

xor_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, SrcReg
) when
    is_atom(SrcReg)
->
    I = jit_arm32_asm:eor(al, Reg, Reg, SrcReg),
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
    I = jit_arm32_asm:eor(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Temp),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Reg, Val) when
    (Val >= 0 andalso Val =< 255) orelse is_atom(Val)
->
    I = jit_arm32_asm:add(al, Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State0#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0, Reg, Val) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_arm32_asm:add(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

mov_immediate(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when
    is_integer(Val)
->
    % Invalidate register cache since we're overwriting Reg with a constant
    State1 = State#state{regs = jit_regs:invalidate_reg(Regs0, Reg)},
    % ARM32: try encode_imm for mov(al, Reg, Val)
    UVal = Val band 16#FFFFFFFF,
    case jit_arm32_asm:encode_imm(UVal) of
        false ->
            % Try MVN with inverted value
            InvVal = bnot Val band 16#FFFFFFFF,
            case jit_arm32_asm:encode_imm(InvVal) of
                false ->
                    % Try shifted byte
                    case Val > 0 andalso Val =< 16#7FFFFFFF of
                        true ->
                            case find_shifted_byte(Val) of
                                {ok, Byte, Shift} ->
                                    I1 = jit_arm32_asm:mov(al, Reg, Byte),
                                    I2 = jit_arm32_asm:lsl(al, Reg, Reg, Shift),
                                    Stream1 = StreamModule:append(
                                        Stream0, <<I1/binary, I2/binary>>
                                    ),
                                    State1#state{stream = Stream1};
                                not_encodable ->
                                    mov_immediate_literal_pool(State1, Reg, Val)
                            end;
                        false ->
                            mov_immediate_literal_pool(State1, Reg, Val)
                    end;
                _ ->
                    % MVN with inverted value
                    I = jit_arm32_asm:mvn(al, Reg, InvVal),
                    Stream1 = StreamModule:append(Stream0, I),
                    State1#state{stream = Stream1}
            end;
        _ ->
            % Direct mov with encodable immediate
            I = jit_arm32_asm:mov(al, Reg, UVal),
            Stream1 = StreamModule:append(Stream0, I),
            State1#state{stream = Stream1}
    end.

%% Helper to add value to literal pool
mov_immediate_literal_pool(
    #state{stream_module = StreamModule, stream = Stream0, literal_pool = LP} = State0, Reg, Val
) ->
    LdrInstructionAddr = StreamModule:offset(Stream0),
    ?ASSERT(byte_size(jit_arm32_asm:ldr(al, Reg, {pc, 0})) =:= 4),
    Stream1 = StreamModule:append(Stream0, <<16#FFFFFFFF:32>>),
    State1 = State0#state{stream = Stream1, literal_pool = [{LdrInstructionAddr, Reg, Val} | LP]},
    maybe_flush_literal_pool(State1).

%% Find if a value can be encoded as (0-255) << N
find_shifted_byte(Val) when Val > 255, Val =< 16#7FFFFFFF ->
    find_shifted_byte(Val, 1, 31).

find_shifted_byte(_Val, Shift, MaxShift) when Shift > MaxShift ->
    not_encodable;
find_shifted_byte(Val, Shift, MaxShift) ->
    case Val band ((1 bsl Shift) - 1) of
        0 ->
            % Lower bits are zero, check if upper bits fit in a byte
            ShiftedDown = Val bsr Shift,
            if
                ShiftedDown >= 0 andalso ShiftedDown =< 255 ->
                    {ok, ShiftedDown, Shift};
                true ->
                    find_shifted_byte(Val, Shift + 1, MaxShift)
            end;
        _ ->
            find_shifted_byte(Val, Shift + 1, MaxShift)
    end.

maybe_flush_literal_pool(#state{literal_pool = []} = State) ->
    State;
maybe_flush_literal_pool(
    #state{stream_module = StreamModule, stream = Stream0, literal_pool = LP} = State
) ->
    % Determine the offset of the last item.
    Offset = StreamModule:offset(Stream0),
    {Addr, _, _} = lists:last(LP),
    % Heuristically set the threshold at 2048 (half the 4095 range of ARM32 ldr).
    if
        Offset - Addr > 2048 ->
            NbLiterals = length(LP),
            % ARM32: all instructions are 4-byte, no alignment needed
            Continuation = NbLiterals * 4 + 4,
            Stream1 = StreamModule:append(Stream0, jit_arm32_asm:b(al, Continuation)),
            flush_literal_pool(State#state{stream = Stream1});
        true ->
            State
    end.

flush_literal_pool(#state{literal_pool = []} = State) ->
    State;
flush_literal_pool(
    #state{stream_module = StreamModule, stream = Stream0, literal_pool = LP} = State
) ->
    % ARM32: no alignment needed, all instructions are 4 bytes
    Stream1 = Stream0,
    % Lay all values and update ldr instructions
    Stream2 = lists:foldl(
        fun({LdrInstructionAddr, Reg, Val}, AccStream) ->
            LiteralPosition = StreamModule:offset(AccStream),
            % ARM32: PC = instruction address + 8
            LdrPC = LdrInstructionAddr + 8,
            LiteralOffset = LiteralPosition - LdrPC,
            LdrInstruction = jit_arm32_asm:ldr(al, Reg, {pc, LiteralOffset}),
            AccStream1 = StreamModule:append(AccStream, <<Val:32/little>>),
            StreamModule:replace(
                AccStream1, LdrInstructionAddr, LdrInstruction
            )
        end,
        Stream1,
        lists:reverse(LP)
    ),
    State#state{stream = Stream2, literal_pool = []}.

sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    (Val >= 0 andalso Val =< 255) orelse is_atom(Val)
->
    I1 = jit_arm32_asm:sub(al, Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, available_regs = Avail, regs = Regs0} = State0, Reg, Val) ->
    Temp = first_avail(Avail),
    AT = Avail band (bnot reg_bit(Temp)),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_arm32_asm:sub(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State1#state{available_regs = Avail, stream = Stream2, regs = Regs1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 3) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:lsl(al, Temp, Reg, 1),
    I2 = jit_arm32_asm:add(al, Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 5) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:lsl(al, Temp, Reg, 2),
    I2 = jit_arm32_asm:add(al, Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 7) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:lsl(al, Temp, Reg, 3),
    I2 = jit_arm32_asm:sub(al, Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 9) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:lsl(al, Temp, Reg, 3),
    I2 = jit_arm32_asm:add(al, Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{available_regs = Avail, regs = Regs0} = State, Reg, 15) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:lsl(al, Temp, Reg, 4),
    I2 = jit_arm32_asm:sub(al, Reg, Temp, Reg),
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
    TempBit = reg_bit(Temp),
    AT = Avail band (bnot TempBit),
    State1 = mov_immediate(State0#state{available_regs = AT}, Temp, Val),
    Stream1 = State1#state.stream,
    I = jit_arm32_asm:mul(al, Reg, Reg, Temp),
    Stream2 = StreamModule:append(Stream1, I),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State1#state{
        stream = Stream2, available_regs = State1#state.available_regs bor TempBit, regs = Regs1
    };
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I = jit_arm32_asm:mul(al, DestReg, DestReg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

%% @doc Decrement reductions and schedule next process if zero.
%% When reductions != 0, branch past the prolog directly to the function body.
%% When reductions == 0, schedule next process; on resume, execute the prolog
%% then continue to the function body.
-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{
        stream_module = StreamModule, stream = Stream0, available_regs = Avail
    } = State0
) ->
    Temp = first_avail(Avail),
    TempJitState = first_avail(Avail band (bnot reg_bit(Temp))),
    % Load jit_state pointer from stack
    I0 = jit_arm32_asm:ldr(al, TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    % Load reduction count
    I1 = jit_arm32_asm:ldr(al, Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    % Decrement reduction count
    I2 = jit_arm32_asm:subs(al, Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_arm32_asm:str(al, Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    Stream1 = StreamModule:append(Stream0, <<I0/binary, I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    ?ASSERT(byte_size(jit_arm32_asm:b(ne, 0)) =:= 4),
    I4 = <<16#FFFFFFFF:32>>,
    % Set continuation to the next instruction
    AddPCOffset = BNEOffset + byte_size(I4),
    ?ASSERT(byte_size(jit_arm32_asm:add(al, Temp, pc, 0)) =:= 4),
    I5 = <<16#FFFFFFFF:32>>,
    I6 = jit_arm32_asm:str(al, Temp, ?JITSTATE_CONTINUATION(TempJitState)),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Add the prolog at the continuation point (where scheduled execution resumes)
    #state{stream = Stream3} = State2,
    ContinuationOffset = StreamModule:offset(Stream3),
    Prolog = jit_arm32_asm:push([r1, r4, r5, r6, r7, r8, r9, r10, r11, lr]),
    Stream4 = StreamModule:append(Stream3, Prolog),
    % Calculate offsets for rewriting
    ContinuationAfterPrologOffset = StreamModule:offset(Stream4),
    % Rewrite the branch to skip over the prolog (branch to continuation_after_prolog)
    NewI4 = jit_arm32_asm:b(ne, ContinuationAfterPrologOffset - BNEOffset),
    % Rewrite the add pc to point to the continuation point (prolog location)
    % ARM32: PC reads as instruction_address + 8
    AddPCImmediate = ContinuationOffset - AddPCOffset - 8,
    NewI5 = jit_arm32_asm:add(al, Temp, pc, AddPCImmediate),
    Stream5 = StreamModule:replace(
        Stream4, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    State3 = merge_used_regs(State2#state{stream = Stream5}, State1#state.used_regs),
    MergedRegs = jit_regs:merge(State1#state.regs, State2#state.regs),
    State3#state{regs = MergedRegs}.

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
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
    TempJitState = first_avail(Avail band (bnot reg_bit(Temp))),
    % Load jit_state pointer from stack
    I0 = jit_arm32_asm:ldr(al, TempJitState, {sp, ?STACK_OFFSET_JITSTATE}),
    % Load reduction count
    I1 = jit_arm32_asm:ldr(al, Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    % Decrement reduction count
    I2 = jit_arm32_asm:subs(al, Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_arm32_asm:str(al, Temp, ?JITSTATE_REDUCTIONCOUNT(TempJitState)),
    Stream1 = StreamModule:append(Stream0, <<I0/binary, I1/binary, I2/binary, I3/binary>>),
    % Use trampoline technique: branch if zero (eq) to skip over the long branch
    % If not zero, we want to continue execution at Label
    % If zero, we want to fall through to scheduling code

    % Look up label once to avoid duplicate lookup in helper
    LabelLookupResult = lists:keyfind(Label, 1, State0#state.labels),

    BccOffset = StreamModule:offset(Stream1),

    State4 =
        case LabelLookupResult of
            {Label, LabelOffset} ->
                % Label is known, use direct conditional branch (ARM32 has +/-32MB range)
                Rel = LabelOffset - BccOffset,
                I4 = jit_arm32_asm:b(ne, Rel),
                Stream2 = StreamModule:append(Stream1, I4),
                State0#state{stream = Stream2};
            false ->
                % Label not known, emit placeholder and add relocation
                I4 = <<16#FFFFFFFF:32>>,
                Stream2 = StreamModule:append(Stream1, I4),
                Reloc = {Label, BccOffset, {cond_branch, ne}},
                State0#state{stream = Stream2, branches = [Reloc | State0#state.branches]}
        end,
    State5 = set_continuation_to_label(State4, Label),
    call_primitive_last(State5, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

%% @doc Emit a call to a primitive with continuation point. Sets CP before
%% calling, then rewrites the CP offset after the call returns.
-spec call_primitive_with_cp(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, TempReg} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, TempReg).

-spec set_cp(state()) -> {state(), non_neg_integer(), arm32_register()}.
set_cp(State0) ->
    % get module index (dynamically)
    {
        #state{stream_module = StreamModule, stream = Stream0, available_regs = AvailRegs} = State1,
        Reg
    } = get_module_index(
        State0
    ),
    % Get a temporary register from available registers
    TempReg = first_avail(AvailRegs),

    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_arm32_asm:lsl(al, Reg, Reg, 24),
    % Placeholder for offset load instruction
    I2 = <<16#FFFFFFFF:32>>,
    MOVOffset = Offset + byte_size(I1),
    % OR the module index with the offset (loaded in temp register)
    I3 = jit_arm32_asm:orr(al, Reg, Reg, TempReg),
    I4 = jit_arm32_asm:str(al, Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, MOVOffset, TempReg}.

-spec rewrite_cp_offset(state(), non_neg_integer(), arm32_register()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    TempReg
) ->
    CurrentOffset = StreamModule:offset(Stream0),

    Delta0 = CurrentOffset - CodeOffset,
    OffsetImm0 = Delta0 bsl 2,

    % Check if offset fits in ARM32 rotated immediate
    case jit_arm32_asm:encode_imm(OffsetImm0 band 16#FFFFFFFF) of
        false ->
            % Need to emit literal pool (4 bytes for the literal)
            OffsetImm1 = (Delta0 + 4) bsl 2,
            % Emit the 32-bit literal right after current position
            StreamWithLiteral = StreamModule:append(
                Stream0, <<OffsetImm1:32/little>>
            ),
            % Compute PC-relative offset for ldr instruction
            % ARM32: PC = instruction_address + 8
            PCRelOffset = CurrentOffset - (RewriteOffset + 8),
            LdrInstr = jit_arm32_asm:ldr(al, TempReg, {pc, PCRelOffset}),
            Stream1 = StreamModule:replace(StreamWithLiteral, RewriteOffset, LdrInstr),
            Prolog = jit_arm32_asm:push([r1, r4, r5, r6, r7, r8, r9, r10, r11, lr]),
            Stream2 = StreamModule:append(Stream1, Prolog),
            State0#state{stream = Stream2};
        _ ->
            MovInstr = jit_arm32_asm:mov(al, TempReg, OffsetImm0),
            Stream1 = StreamModule:replace(Stream0, RewriteOffset, MovInstr),
            Prolog = jit_arm32_asm:push([r1, r4, r5, r6, r7, r8, r9, r10, r11, lr]),
            Stream2 = StreamModule:append(Stream1, Prolog),
            State0#state{stream = Stream2}
    end.

set_bs(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = Avail, regs = Regs0} =
        State0,
    TermReg
) ->
    Temp = first_avail(Avail),
    I1 = jit_arm32_asm:str(al, TermReg, ?BS),
    I2 = jit_arm32_asm:mov(al, Temp, 0),
    I3 = jit_arm32_asm:str(al, Temp, ?BS_OFFSET),
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

    % add r0, pc, #0 sets r0 to instruction_address + 8 = address of data after pop
    % We have: add (4 bytes) + pop (4 bytes) = 8 bytes before data
    % PC reads as instruction_address + 8, so offset 0 points right after the pop
    I1 = jit_arm32_asm:add(al, r0, pc, 0),
    I2 = jit_arm32_asm:pop([r1, r4, r5, r6, r7, r8, r9, r10, r11, pc]),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

%% Helper function to generate str instruction with y_reg offset, handling large offsets
str_y_reg(SrcReg, Y, TempReg, _) when Y * 4 =< 4095 ->
    % Small offset - use immediate addressing
    I1 = jit_arm32_asm:ldr(al, TempReg, ?Y_REGS),
    I2 = jit_arm32_asm:str(al, SrcReg, {TempReg, Y * 4}),
    <<I1/binary, I2/binary>>;
str_y_reg(SrcReg, Y, TempReg1, _AvailMask) ->
    % Large offset (Y * 4 > 4095) - split into base + 4080 + remainder
    % 4080 (0xFF0) is the largest ARM-encodable immediate close to the 4095 ldr/str limit
    Offset = Y * 4,
    BaseOffset = 4080,
    Remainder = Offset - BaseOffset,
    I1 = jit_arm32_asm:ldr(al, TempReg1, ?Y_REGS),
    I2 = jit_arm32_asm:add(al, TempReg1, TempReg1, BaseOffset),
    I3 = jit_arm32_asm:str(al, SrcReg, {TempReg1, Remainder}),
    <<I1/binary, I2/binary, I3/binary>>.

%% Helper function to generate ldr instruction with y_reg offset, handling large offsets
ldr_y_reg(DstReg, Y, AvailMask) when AvailMask =/= 0, Y * 4 =< 4095 ->
    % Small offset - use immediate addressing
    TempReg = first_avail(AvailMask),
    I1 = jit_arm32_asm:ldr(al, TempReg, ?Y_REGS),
    I2 = jit_arm32_asm:ldr(al, DstReg, {TempReg, Y * 4}),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, AvailMask) when AvailMask =/= 0 ->
    % Large offset (Y * 4 > 4095) - split into base + 4080 + remainder
    % 4080 (0xFF0) is the largest ARM-encodable immediate close to the 4095 ldr/str limit
    TempReg = first_avail(AvailMask),
    Offset = Y * 4,
    BaseOffset = 4080,
    Remainder = Offset - BaseOffset,
    I1 = jit_arm32_asm:ldr(al, TempReg, ?Y_REGS),
    I2 = jit_arm32_asm:add(al, TempReg, TempReg, BaseOffset),
    I3 = jit_arm32_asm:ldr(al, DstReg, {TempReg, Remainder}),
    <<I1/binary, I2/binary, I3/binary>>;
ldr_y_reg(DstReg, Y, 0) when Y * 4 =< 4095 ->
    % Small offset, no registers available - use DstReg as temp
    I1 = jit_arm32_asm:ldr(al, DstReg, ?Y_REGS),
    I2 = jit_arm32_asm:ldr(al, DstReg, {DstReg, Y * 4}),
    <<I1/binary, I2/binary>>;
ldr_y_reg(DstReg, Y, 0) ->
    % Large offset (Y * 4 > 4095), no registers available
    % Use DstReg as temp: load Y_REGS base, add 4080, ldr with remainder
    % 4080 (0xFF0) is the largest ARM-encodable immediate close to the 4095 ldr/str limit
    Offset = Y * 4,
    BaseOffset = 4080,
    Remainder = Offset - BaseOffset,
    I1 = jit_arm32_asm:ldr(al, DstReg, ?Y_REGS),
    I2 = jit_arm32_asm:add(al, DstReg, DstReg, BaseOffset),
    I3 = jit_arm32_asm:ldr(al, DstReg, {DstReg, Remainder}),
    <<I1/binary, I2/binary, I3/binary>>.

reg_bit(r0) -> ?REG_BIT_R0;
reg_bit(r1) -> ?REG_BIT_R1;
reg_bit(r2) -> ?REG_BIT_R2;
reg_bit(r3) -> ?REG_BIT_R3;
reg_bit(r4) -> ?REG_BIT_R4;
reg_bit(r5) -> ?REG_BIT_R5;
reg_bit(r6) -> ?REG_BIT_R6;
reg_bit(r7) -> ?REG_BIT_R7;
reg_bit(r8) -> ?REG_BIT_R8;
reg_bit(r9) -> ?REG_BIT_R9;
reg_bit(r10) -> ?REG_BIT_R10;
reg_bit(r11) -> ?REG_BIT_R11;
reg_bit(r12) -> ?REG_BIT_R12;
reg_bit(r13) -> ?REG_BIT_R13;
reg_bit(r14) -> ?REG_BIT_R14;
reg_bit(r15) -> ?REG_BIT_R15.

%% first_avail returns the first available register from a bitmask.
%% Order matches ?AVAILABLE_REGS = [r11, r10, r9, r8, r7, r6, r5, r4, r3, r1]
first_avail(Mask) when Mask band ?REG_BIT_R11 =/= 0 -> r11;
first_avail(Mask) when Mask band ?REG_BIT_R10 =/= 0 -> r10;
first_avail(Mask) when Mask band ?REG_BIT_R9 =/= 0 -> r9;
first_avail(Mask) when Mask band ?REG_BIT_R8 =/= 0 -> r8;
first_avail(Mask) when Mask band ?REG_BIT_R7 =/= 0 -> r7;
first_avail(Mask) when Mask band ?REG_BIT_R6 =/= 0 -> r6;
first_avail(Mask) when Mask band ?REG_BIT_R5 =/= 0 -> r5;
first_avail(Mask) when Mask band ?REG_BIT_R4 =/= 0 -> r4;
first_avail(Mask) when Mask band ?REG_BIT_R3 =/= 0 -> r3;
first_avail(Mask) when Mask band ?REG_BIT_R1 =/= 0 -> r1.

%% Convert bitmask to list, matching the order of ?AVAILABLE_REGS.
mask_to_list(0) -> [];
mask_to_list(Mask) -> mask_to_list_r11(Mask).

mask_to_list_r11(Mask) when Mask band ?REG_BIT_R11 =/= 0 -> [r11 | mask_to_list_r10(Mask)];
mask_to_list_r11(Mask) -> mask_to_list_r10(Mask).
mask_to_list_r10(Mask) when Mask band ?REG_BIT_R10 =/= 0 -> [r10 | mask_to_list_r9(Mask)];
mask_to_list_r10(Mask) -> mask_to_list_r9(Mask).
mask_to_list_r9(Mask) when Mask band ?REG_BIT_R9 =/= 0 -> [r9 | mask_to_list_r8(Mask)];
mask_to_list_r9(Mask) -> mask_to_list_r8(Mask).
mask_to_list_r8(Mask) when Mask band ?REG_BIT_R8 =/= 0 -> [r8 | mask_to_list_r7(Mask)];
mask_to_list_r8(Mask) -> mask_to_list_r7(Mask).
mask_to_list_r7(Mask) when Mask band ?REG_BIT_R7 =/= 0 -> [r7 | mask_to_list_r6(Mask)];
mask_to_list_r7(Mask) -> mask_to_list_r6(Mask).
mask_to_list_r6(Mask) when Mask band ?REG_BIT_R6 =/= 0 -> [r6 | mask_to_list_r5(Mask)];
mask_to_list_r6(Mask) -> mask_to_list_r5(Mask).
mask_to_list_r5(Mask) when Mask band ?REG_BIT_R5 =/= 0 -> [r5 | mask_to_list_r4(Mask)];
mask_to_list_r5(Mask) -> mask_to_list_r4(Mask).
mask_to_list_r4(Mask) when Mask band ?REG_BIT_R4 =/= 0 -> [r4 | mask_to_list_r3(Mask)];
mask_to_list_r4(Mask) -> mask_to_list_r3(Mask).
mask_to_list_r3(Mask) when Mask band ?REG_BIT_R3 =/= 0 -> [r3 | mask_to_list_r1(Mask)];
mask_to_list_r3(Mask) -> mask_to_list_r1(Mask).
mask_to_list_r1(Mask) when Mask band ?REG_BIT_R1 =/= 0 -> [r1];
mask_to_list_r1(_Mask) -> [].

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
%% @doc Add a label at the current offset. Eventually align it with a nop.
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference()) -> state().
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
-spec add_label(state(), integer() | reference(), integer()) -> state().
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
    % Each ARM32 jump table entry is 8 bytes:
    % - push {r1, r4-r11, lr} (4 bytes) at offset 0
    % - b <label> (4 bytes) at offset 4
    JumpTableEntryStart = JumpTableStart + Label * ?JUMP_TABLE_ENTRY_SIZE,
    BranchOffset = JumpTableEntryStart + 4,

    % Calculate relative offset from the branch instruction to target label
    Rel = LabelOffset - BranchOffset,
    BranchInstr = jit_arm32_asm:b(al, Rel),

    Stream1 = StreamModule:replace(Stream0, BranchOffset, BranchInstr),

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

%% Convert a value to a contents descriptor for tracking.
value_to_contents(Value) -> jit_regs:value_to_contents(Value, ?MAX_REG).

%% Convert a VM register destination to a contents descriptor.
vm_dest_to_contents(Dest) -> jit_regs:vm_dest_to_contents(Dest, ?MAX_REG).

-ifdef(JIT_DWARF).
%%-----------------------------------------------------------------------------
%% @doc Return the DWARF register number for the ctx parameter
%% @return The DWARF register number where ctx is passed (r0 in ARM)
%% @end
%%-----------------------------------------------------------------------------
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_R0_REG_ARM32.

-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(r0) -> 0;
dwarf_register_number(r1) -> 1;
dwarf_register_number(r2) -> 2;
dwarf_register_number(r3) -> 3;
dwarf_register_number(r4) -> 4;
dwarf_register_number(r5) -> 5;
dwarf_register_number(r6) -> 6;
dwarf_register_number(r7) -> 7;
dwarf_register_number(r8) -> 8;
dwarf_register_number(r9) -> 9;
dwarf_register_number(r10) -> 10;
dwarf_register_number(r11) -> 11;
dwarf_register_number(r12) -> 12;
dwarf_register_number(r13) -> 13;
dwarf_register_number(r14) -> 14;
dwarf_register_number(r15) -> 15.
-endif.
